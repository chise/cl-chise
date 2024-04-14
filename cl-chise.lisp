(in-package :cl-user)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require 'cl-redis))

(defpackage :chise
  (:use :cl)
  (:export
   :open-ds
   :decode-char
   :get-char-attribute :put-char-attribute
   :define-char
   :metadata-feature-name-p
   :ccs-feature-name-p
   :sequence-list-p))

(in-package :chise)

(defun sequence-list-p (object)
  (cond ((null object))
	((consp object)
	 (sequence-list-p (cdr object)))))

(defun open-ds ()
  (handler-case (redis:connect) (redis:redis-error (c) (format t "~a" c)))
  (red:select 1))

(defun metadata-feature-name-p (feature-name)
  (if (symbolp feature-name)
      (setq feature-name (symbol-name feature-name)))
  (search "*" feature-name))

(defun ccs-feature-name-p (feature-name)
  (and (not (metadata-feature-name-p feature-name))
       (progn
	 (if (symbolp feature-name)
	     (setq feature-name (format nil "~a" feature-name)))
	 (eql (elt feature-name 0) #\=)
	 (not (let ((pos (search "decomposition" feature-name)))
		(and pos
		     (or (= pos 1)
			 (and (= pos 2)
			      (eql (elt feature-name 1) #\>)))))))))

(defun generate-object-id ()
  (let ((ret (red:get "character:sys:next-id"))
	status)
    (setq ret (if ret
		  (read-from-string ret)
		  #xF0000))
    (setq status (red:set "character:sys:next-id" (1+ ret)))
    (if (string= status "OK")
	ret)))

(defun define-char (char-spec)
  (let ((id (cdr (assoc '=ucs char-spec)))
	ret obj)
    (cond (id
	   (setq obj (code-char id))
	   )
	  ((find-if (lambda (feature-pair)
		      (and (ccs-feature-name-p (car feature-pair))
			   (setq ret (decode-char (car feature-pair)
						  (cdr feature-pair)))))
		    char-spec)
	   (setq obj ret)
	   )
	  ((setq id (generate-object-id))
	   (setq obj (code-char id))
	   ))
    (when obj
      (dolist (feature-pair char-spec)
	(put-char-attribute obj (car feature-pair)(cdr feature-pair))))
    obj))

(defun put-char-attribute (character attribute value)
  (let ((key (format nil "character:obj:~a;~a"
		     (char-code character)
		     attribute))
	index ret ret2)
    (cond ((ccs-feature-name-p attribute)
	   (setq index (format nil "character:idx:~a;~a"
			       attribute value))
	   (when (string= (setq ret (red:set key value)) "OK")
	     (setq ret2 (red:set index character))
	     (values value ret ret2))
	   )
	  ((and (consp value)
		(sequence-list-p value))
	   (red:del key)
	   (when (integerp (setq ret (apply #'red:rpush key value)))
	     (values value ret))
	   )
	  (t
	   (when (string= (setq ret (red:set key value)) "OK")
	     (values value ret))
	   ))))

(defun get-char-attribute (character attribute)
  (let ((key (format nil "character:obj:~a;~a"
		     (char-code character)
		     attribute))
	ret)
    (cond ((string= (red:type key) "list")
	   (mapcar #'read-from-string (red:lrange key 0 -1))
	   )
	  (t
	   (when (setq ret (red:get key))
	     (read-from-string ret))
	   ))))

(defun decode-char (ccs code-point)
  (let ((index (format nil "character:idx:~a;~a"
		       ccs code-point))
	ret)
    (when (setq ret (red:get index))
      (read-from-string ret))))
