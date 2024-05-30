(in-package :chise)

(require 'split-sequence)

(defun ids-parse-terminal (previous in)
  (let ((chr (or previous
		 (read-entity-reference in)
		 (read-char in nil)))
	ucs big5)
    (cond
      ((stringp chr)
       chr)
      (chr
       (setq ucs (encode-char chr '=ucs))
       (or (unless (or (and ucs (<= #x2FF0 ucs)(<= ucs #x2FFF))
		       (member (encode-char chr '=ucs-var-001)
			       '(#x2FF0))
		       (member (encode-char chr '=ucs-itaiji-001)
			       '(#x2FF1 #x2FF9 #x2FF6 #x2FFB))
		       (member (encode-char chr '=ucs-itaiji-002)
			       '(#x2FF1)))
	     (if (and ucs (<= #xE000 ucs)(<= ucs #xF8FF)
		      (setq big5 (encode-char chr '=big5)))
		 (setq chr (decode-char '=big5-cdp big5)))
	     chr)
	   (if (characterp chr)
	       (unread-char chr in)
	       (values nil chr)))
       ))))

(defun ids-parse-op-2 (previous in)
  (let ((chr (or previous
		 (read-entity-reference in)
		 (read-char in nil)))
	ucs)
    (cond
      ((stringp chr)
       (values nil chr)
       )
      (chr
       (setq ucs (encode-char chr '=ucs))
       (cond
	 ((or (and ucs
		   (or (eql ucs #x2FF0)
		       (eql ucs #x2FF1)
		       (and (<= #x2FF4 ucs)(<= ucs #x2FFB))))
	      (member (encode-char chr '=ucs-var-001)
		      '(#x2FF0))
	      (member (encode-char chr '=ucs-itaiji-001)
		      '(#x2FF1 #x2FF9 #x2FF6 #x2FFB))
	      (member (encode-char chr '=ucs-itaiji-002)
		      '(#x2FF1)))
	  chr)
	 ((characterp chr)
	  (unread-char chr in)
	  )
	 (t
	  (values nil chr)
	  ))))))

(defun ids-parse-op-3 (previous in)
  (let ((chr (or previous
		 (read-entity-reference in)
		 (read-char in nil))))
    (cond
      ((member chr '(#\u2FF2 #\u2FF3))
       chr)
      ((characterp chr)
       (unread-char chr in)
       )
      (t
       (values nil chr)
       ))))

(defun ids-parse-element (previous in)
  (let (ret op arg1 arg2 arg3)
    (cond ((multiple-value-bind (parsed rest)
	       (ids-parse-terminal previous in)
	     (setq previous rest
		   ret parsed))
	   (values ret previous)
	   )
	  ((multiple-value-bind (parsed rest)
	       (ids-parse-op-2 previous in)
	     (setq previous rest
		   op parsed))
	   (when (multiple-value-bind (parsed rest)
		     (ids-parse-element previous in)
		   (setq previous rest
			 ret parsed))
	     (setq arg1 ret)
	     (when (multiple-value-bind (parsed rest)
		       (ids-parse-element previous in)
		     (setq previous rest
			   ret parsed))
	       (setq arg2 ret)
	       (values (list (list 'ideographic-structure op arg1 arg2))
		       previous)))
	   )
	  ((multiple-value-bind (parsed rest)
	       (ids-parse-op-3 previous in)
	     (setq previous rest
		   op parsed))
	   (when (multiple-value-bind (parsed rest)
		     (ids-parse-element previous in)
		   (setq previous rest
			 ret parsed))
	     (setq arg1 ret)
	     (when (multiple-value-bind (parsed rest)
		       (ids-parse-element previous in)
		     (setq previous rest
			   ret parsed))
	       (setq arg2 ret)
	       (when (multiple-value-bind (parsed rest)
			 (ids-parse-element previous in)
		       (setq previous rest
			     ret parsed))
		 (setq arg3 ret)
		 (values (list (list 'ideographic-structure op arg1 arg2 arg3))
			 previous))))
	   ))))

(defun ids-parse-string (ids-string)
  "Parse IDS-STRING and return the result."
  (let (parsed rest)
    (when (multiple-value-setq (parsed rest)
	    (with-input-from-string (in ids-string)
	      (ids-parse-element nil in)))
      (unless rest
	(when (consp parsed)
	  parsed)))))

(defun ids-read-file (file &key override prompt)
  (when prompt
    (princ "Loading ")
    (princ file)
    (princ "..."))
  (with-open-file (in file :direction :input)
    (loop (let ((line (read-line in nil))
		fields crep cpos ids opt
		char ret)
	    (if (not line) (return))
	    (setq fields (split-sequence:split-sequence #\tab line))
	    (setq crep (pop fields))
	    (pop fields)
	    (setq ids (pop fields))
	    (setq opt (pop fields))
	    (when (and (eql (search "U+" crep) 0)
		       (setq cpos (parse-integer crep
						 :start 2 :radix 16
						 :junk-allowed t))
		       (setq char (or (decode-char '=ucs cpos)
				      (code-char cpos))))
	      (when (and (or override
			     (null (get-char-attribute
				    char 'ideographic-structure)))
			 (setq ret (ids-parse-string ids))
			 (setq ret (cdr (assoc 'ideographic-structure ret))))
		(put-char-attribute char 'ideographic-structure ret))
	      (when (and opt
			 (or override
			     (null (get-char-attribute
				    char 'ideographic-structure@apparent)))
			 (eql (search "@apparent=" opt) 0)
			 (setq ret (ids-parse-string (subseq opt 10)))
			 (setq ret (cdr (assoc 'ideographic-structure ret))))
		(put-char-attribute
		 char 'ideographic-structure@apparent ret)
		))))
    (when prompt
      (format t "done.~%"))))

(defun ids-index-store-char (product component)
  (let (ret)
    (adjoin-char-attribute component "ideographic-products" product)
    (when (setq ret (get-char-attribute component "ideographic-structure"))
      (ids-index-store-structure product ret))
    (when (setq ret (get-char-attribute component "ideographic-structure@apparent"))
      (ids-index-store-structure product ret))
    (when (setq ret (get-char-attribute component "ideographic-structure@apparent/leftmost"))
      (ids-index-store-structure product ret))
    (when (setq ret (get-char-attribute component "ideographic-structure@apparent/rightmost"))
      (ids-index-store-structure product ret))))

(defun ids-index-store-structure (product structure)
  (setq product (normalize-as-char product))
  (let (ret)
    (dolist (cell (cdr structure))
      (if (char-ref-p cell)
	  (setq cell (getf cell :char)))
      (cond ((or (characterp cell)
		 (concord:object-p cell))
	     (ids-index-store-char product cell))
	    ((setq ret (assoc 'ideographic-structure cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((setq ret (assoc 'ideographic-structure@apparent cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((setq ret (assoc 'ideographic-structure@apparent/leftmost cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((setq ret (assoc 'ideographic-structure@apparent/rightmost cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((and (association-list-p cell)
		  (setq ret (define-char cell)))
	     (ids-index-store-char product ret))
	    ))))

(defun ids-update-index (&optional (s t))
  (format s "Updating index for `ideographic-structure'...")
  (concord:some-in-feature
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   "ideographic-structure" :genre 'character)
  (format s "done.~%")
  (format s "Updating index for `ideographic-structure@apparent'...")
  (concord:some-in-feature
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   "ideographic-structure@apparent" :genre 'character)
  (format s "done.~%")
  (format s "Updating index for `ideographic-structure@apparent/leftmost'...")
  (concord:some-in-feature
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   "ideographic-structure@apparent/leftmost" :genre 'character)
  (format s "done.~%")
  (format s "Updating index for `ideographic-structure@apparent/rightmost'...")
  (concord:some-in-feature
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   "ideographic-structure@apparent/rightmost" :genre 'character)
  (format s "done.~%")
  (let (products ucs)
    (concord:some-in-feature
     (lambda (c v)
       (setq products (get-char-attribute c 'ideographic-products))
       (dolist (comp (remove c (char-ucs-chars c)))
	 (dolist (p_c (get-char-attribute comp 'ideographic-products))
	   (unless (encode-char p_c '=ucs)
	     (if (setq ucs (char-ucs p_c))
		 (setq p_c (decode-char '=ucs ucs))))
	   (setq products (adjoin p_c products))))
       (put-char-attribute c 'ideographic-products products)
       nil)
     '=>iwds-1)
    (concord:some-in-feature
     (lambda (c v)
       (setq products (get-char-attribute c 'ideographic-products))
       (dolist (comp (remove c (char-ucs-chars c)))
	 (dolist (p_c (get-char-attribute comp 'ideographic-products))
	   (unless (encode-char p_c '=ucs)
	     (if (setq ucs (char-ucs p_c))
		 (setq p_c (decode-char '=ucs ucs))))
	   (setq products (adjoin p_c products))))
       (put-char-attribute c 'ideographic-products products)
       nil)
     '=>ucs@iwds-1)
    (concord:some-in-feature
     (lambda (c v)
       (setq products (get-char-attribute c 'ideographic-products))
       (dolist (comp (delq c (char-ucs-chars c)))
	 (put-char-attribute
	  comp 'ideographic-products
	  (union products
		 (get-char-attribute comp 'ideographic-products))))
       )
     '=>iwds-1)
    (concord:some-in-feature
     (lambda (c v)
       (setq products (get-char-attribute c 'ideographic-products))
       (dolist (comp (delq c (char-ucs-chars c)))
	 (put-char-attribute
	  comp 'ideographic-products
	  (union products
		 (get-char-attribute comp 'ideographic-products))))
       )
     '=>ucs@iwds-1)
    )
  )

(defun ideograph-find-products (components &optional ignored-chars)
  (if (stringp components)
      (setq components (concatenate 'list components)))
  (let (dest products)
    (setq dest (get-char-attribute (car components) "ideographic-products"))
    (loop
      (unless (and dest
		   (setq components (cdr components)))
	(return))
      (setq products (get-char-attribute (car components) "ideographic-products"))
      (setq dest (intersection dest products)))
    dest))
