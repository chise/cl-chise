(in-package :cl-user)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require 'cl-concord))

(defpackage :chise
  (:use :cl)
  (:import-from
   :concord
   :some-in-feature
   :metadata-feature-name-p
   :decomposition-feature-name-p
   :structure-feature-name-p
   :relation-feature-name-p
   :make-reversed-relation-feature-name
   :sequence-list-p :association-list-p
   :while
   :=ucs)
  (:export
   :decode-char :encode-char
   :get-char-attribute :put-char-attribute
   :char-feature
   :define-char :find-char :char-spec :char-ccs-spec
   :normalize-as-char
   :char-ucs :char-ucs-chars
   :ids-parse-element
   :ids-parse-string
   :ids-read-file
   :ids-update-index
   :ideograph-find-products :ideographic-structure-find-chars))

(in-package :chise)

(defun char-spec (character)
  (if (characterp character)
      (setq character (concord:object :character (char-code character))))
  (concord:object-spec character))

(defun char-ccs-spec (character)
  (let ((spec (char-spec character))
	dest ret)
    (cond
      ((setq ret (assoc '=ucs spec))
       (setq dest (list ret))
       (cond ((setq ret (assoc 'name spec))
	      (setq dest (cons ret dest))
	      )
	     ((setq ret (assoc 'name* spec))
	      (setq dest (cons ret dest))
	      ))
       )
      ((setq ret (or (assoc '=mj spec)
		     (assoc '=adobe-japan1-0 spec)
		     (assoc '=adobe-japan1-1 spec)
		     (assoc '=adobe-japan1-2 spec)
		     (assoc '=adobe-japan1-3 spec)
		     (assoc '=adobe-japan1-4 spec)
		     (assoc '=adobe-japan1-5 spec)
		     (assoc '=adobe-japan1-6 spec)
		     (assoc '==mj spec)
		     (assoc '==adobe-japan1-0 spec)
		     (assoc '==adobe-japan1-1 spec)
		     (assoc '==adobe-japan1-2 spec)
		     (assoc '==adobe-japan1-3 spec)
		     (assoc '==adobe-japan1-4 spec)
		     (assoc '==adobe-japan1-5 spec)
		     (assoc '==adobe-japan1-6 spec)))
       (setq dest (list ret))
       (dolist (cell spec)
	 (when (and (not (metadata-feature-name-p (car cell)))
		    (search "=ucs@" (format nil "~a" (car cell))))
	   (setq dest (cons cell dest))))
       )
      (t
       (dolist (cell spec)
	 (cond ((concord:id-feature-name-p (car cell))
		(setq dest (cons cell dest))
		)
	       ((member (car cell) '(name name*))
		(setq dest (cons cell dest))
		)))
       ))
    dest))

(defun find-char (char-spec)
  (normalize-as-char (concord:find-object :character char-spec)))

(defun define-char (char-spec)
  (let ((id (cdr (assoc '=ucs char-spec))))
    (concord:define-object :character char-spec :id id)))

(defun put-char-attribute (character attribute value)
  (if (characterp character)
      (setq character (concord:object :character (char-code character))))
  (concord:object-put character attribute value))

(defun get-char-attribute (character attribute &optional default-value)
  (if (characterp character)
      (setq character (concord:object :character (char-code character))))
  (concord:object-get character attribute default-value))

(defun char-feature (character feature &optional default-value)
  (if (characterp character)
      (setq character (concord:object :character (char-code character))))
  (concord:object-get character feature default-value :recursive t))

(defun adjoin-char-attribute (character attribute item)
  (if (characterp character)
      (setq character (concord:object :character (char-code character))))
  (concord:object-adjoin character attribute item))

(defun encode-char (character ccs)
  (get-char-attribute character ccs))

(defun decode-char (ccs code-point)
  (let (ret)
    (cond ((and (or (eql ccs '=ucs)
		    (equal ccs "=ucs"))
		(< code-point #xF0000))
	   (code-char code-point)
	   )
	  ((setq ret (concord:decode-object ccs code-point :genre 'character))
	   (if (< (concord:object-id ret) #xF0000)
	       (code-char (concord:object-id ret))
	       ret)
	   ))))

(defun normalize-as-char (object)
  (let (id)
    (if (and (concord:object-p object)
	     (eq (concord:genre-name (concord:object-genre object)) 'character)
	     (< (setq id (concord:object-id object)) #xF0000))
	(code-char id)
	object)))

(defun char-ref-p (object)
  (and (consp object)
       (keywordp (car object))))

(defun char-ucs (char)
  (or (encode-char char "=ucs")
      (char-feature char "=ucs")
      (char-feature char "=>ucs")))

(defun char-ucs-chars (character)
  (let (ucs dest)
    (if (and (setq ucs (encode-char character '=ucs))
	     (not (and (<= #x2E80 ucs)(<= ucs #x2EF3)))
	     (null (get-char-attribute character '=>ucs*)))
	(setq dest (list (normalize-as-char character))))
    (dolist (c (mapcan #'char-ucs-chars
		       (get-char-attribute character '->subsumptive)))
      (setq dest (adjoin (normalize-as-char c) dest)))
    (dolist (c (mapcan #'char-ucs-chars
		       (get-char-attribute character '->denotational)))
      (setq dest (adjoin (normalize-as-char c) dest)))
    (dolist (c (mapcan #'char-ucs-chars
		       (get-char-attribute character '->denotational@component)))
      (setq dest (adjoin (normalize-as-char c) dest)))
    dest))
