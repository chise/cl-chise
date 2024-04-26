(in-package :cl-user)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require 'cl-concord))

(defpackage :chise
  (:use :cl)
  (:export
   :decode-char
   :get-char-attribute :put-char-attribute
   :define-char
   :metadata-feature-name-p
   :ccs-feature-name-p
   :sequence-list-p
   :normalize-as-char))

(in-package :chise)

(defun char-spec (character)
  (if (characterp character)
      (setq character (concord:object :character (char-code character))))
  (concord:object-spec character))

(defun define-char (char-spec)
  (let ((id (cdr (assoc '=ucs char-spec))))
    (concord:define-object :character char-spec :id id)))

(defun put-char-attribute (character attribute value)
  (if (characterp character)
      (setq character (concord:object :character (char-code character))))
  (concord:object-put character attribute value))

(defun get-char-attribute (character attribute)
  (if (characterp character)
      (setq character (concord:object :character (char-code character))))
  (concord:object-get character attribute))

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
