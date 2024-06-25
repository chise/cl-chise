(in-package :cl-user)
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require 'cl-concord))

(defpackage :chise
  (:use :cl)
  (:import-from
   :concord
   :some-in-feature
   :store-union-in-feature
   :metadata-feature-name-p
   :decomposition-feature-name-p
   :structure-feature-name-p
   :relation-feature-name-p
   :make-reversed-relation-feature-name
   :expand-feature-name
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
   :ideograph-find-products :ideographic-structure-find-chars
   :ideographic-structure
   :some-in-character-feature
   :some-char-in-family
   :store-union-in-feature))

(in-package :chise)

(defvar char-db-feature-domains
  '(ucs ucs/compat daikanwa cns gt jis jis/a jis/b
	jis-x0212 jis-x0213 cdp shinjigen mj
	r001 r007 r030 r053 r055 r074 r130 r140 r159 misc unknown))

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

(defun character-object-p (object)
  (or (characterp object)
      (and (concord:object-p object)
	   (eq (concord:genre-name (concord:object-genre object))
	       'character))))

(defun char-ref-p (object)
  (and (consp object)
       (keywordp (car object))))

(defun char-ucs (char)
  (or (encode-char char "=ucs")
      (char-feature char "=ucs")
      (char-feature char "=>ucs")))

(defun char-id (char)
  (if (characterp char)
      (char-code char)
      (concord:object-id char)))

(defun char-ucs-chars (character)
  (let (ucs dest)
    (if (and (setq ucs (encode-char character '=ucs))
	     ;; (not (and (<= #x2E80 ucs)(<= ucs #x2EF3)))
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

(defun some-in-character-feature (func feature-name)
  (some-in-feature (lambda (c v)
		     (funcall func
			      (normalize-as-char c)
			      v))
		   feature-name :genre 'character))

(defun some-char-in-family (function char &optional ignore-sisters)
  (let ((rest (list char))
	ret checked)
    (while rest
      (unless (member (car rest) checked)
	(if (setq ret (funcall function (car rest)))
	    (return ret))
	(setq checked (cons (car rest) checked)
	      rest (append rest
			   (get-char-attribute (car rest) '->subsumptive)
			   (get-char-attribute (car rest) '->denotational)
			   (get-char-attribute (car rest) '->identical)))
	(unless ignore-sisters
	  (setq rest (append rest
			     (get-char-attribute (car rest) '<-subsumptive)
			     (get-char-attribute (car rest) '<-denotational)))))
      (setq rest (cdr rest)))))

(defmethod store-union-in-feature (feature-name (dest-obj character) &rest objects)
  (let ((ds (concord:genre-ds (concord:genre 'character))))
    (apply #'concord::ds-store-union
	   ds
 	   (format nil "character:obj:~a;~a"
		   (char-id dest-obj)
		   feature-name)
	   (mapcar (lambda (obj)
		     (format nil "character:obj:~a;~a"
			     (char-id obj)
			     feature-name))
		   objects))))

(defun get-char-feature-from-domains (char feature domains
					   &optional tester arg
					   ignore-sisters)
  (some-char-in-family
   (lambda (ch)
     (let (ret)
       (dolist (domain domains)
	 (if (and (or (null tester)
		      (equal (or (char-feature
				  ch (expand-feature-name tester domain))
				 (char-feature ch tester))
			     arg))
		  (setq ret (or (char-feature
				 ch (expand-feature-name feature domain))
				(char-feature ch feature))))
	     (return ret)))))
   char ignore-sisters))

(defun car-safe (object)
  (if (consp object)
      (car object)))

(defun cdr-safe (object)
  (if (consp object)
      (cdr object)))

(defun int-list< (a b)
  (if (numberp (car a))
      (if (numberp (car b))
	  (if (= (car a) (car b))
	      (int-list< (cdr a)(cdr b))
	    (< (car a) (car b)))
	(if (= (car a) 0)
	    nil
	  (< (car a) 0)))
    (if (numberp (car b))
	(if (= (car b) 0)
	    t
	  (< 0 (car b)))
      )))

(defun morohashi-daikanwa< (a b)
  (if (integerp a)
      (setq a (list a)))
  (if (integerp b)
      (setq b (list b)))
  (cond ((eq (car-safe a) 'ho)
	 (if (eq (car-safe b) 'ho)
	     (int-list< (cdr-safe a)(cdr-safe b))
	   nil))
	((or (integerp a)
	     (integerp (car a)))
	 (if (eq (car b) 'ho)
	     t
	   (int-list< a b)))
	(t
	 (if (eq (car-safe b) 'ho)
	     t
	   (int-list< a b)))))

(defun char-ideographic-radical (char &optional radical ignore-sisters)
  (let (ret)
    (or (if radical
	    (get-char-feature-from-domains
	     char 'ideographic-radical (cons nil char-db-feature-domains)
	     'ideographic-radical radical ignore-sisters)
	  (get-char-feature-from-domains
	   char 'ideographic-radical (cons nil char-db-feature-domains)
	   ignore-sisters))
	(dolist (cell (get-char-attribute char 'ideographic-))
	  (if (and (setq ret (getf cell :radical))
		   (or (eq ret radical)
		       (null radical)))
	      (return ret)))
	(get-char-feature-from-domains
	 char 'ideographic-radical (cons nil char-db-feature-domains))
        (progn
	  (setq ret
		(or (get-char-attribute char 'daikanwa-radical)
		    (get-char-attribute char 'kangxi-radical)
		    (get-char-attribute char 'japanese-radical)
		    (get-char-attribute char 'korean-radical)))
	  (when ret
	    (put-char-attribute char 'ideographic-radical ret)
	    ret)))))

(defvar ideographic-radical nil)

(defun char-representative-of-daikanwa (char &optional radical
					     ignore-default checked)
  (unless radical
    (setq radical ideographic-radical))
  (if (or (null radical)
          (eq (or (get-char-attribute char 'ideographic-radical)
                  (char-ideographic-radical char radical t))
              radical))
      (let ((ret (or (encode-char char '=daikanwa)
		     (encode-char char '=daikanwa@rev2)
		     (encode-char char '=daikanwa/+p)
                     (encode-char char '=daikanwa/+2p)
                     (encode-char char '=daikanwa/ho)
                     )))
	(or (and ret char)
	    (if (setq ret (get-char-attribute char 'morohashi-daikanwa))
		(let ((m-m (car ret))
		      (m-s (nth 1 ret))
		      pat)
		  (if (= m-s 0)
		      (or (decode-char '=daikanwa@rev2 m-m)
			  (decode-char '=daikanwa m-m))
		    (or (cond ((eq m-m 'ho)
			       (decode-char '=daikanwa/ho m-s))
			      ((eq m-s 1)
			       (decode-char '=daikanwa/+p m-m))
			      ((eq m-s 2)
			       (decode-char '=daikanwa/+2p m-m)))
			(progn
			  (setq pat (list m-m m-s))
			  (some-in-feature (lambda (c v)
					     (if (equal pat v)
						 c))
					   'morohashi-daikanwa
					   :genre 'character))))))
            (and (setq ret (get-char-attribute char '=>daikanwa))
		 (if (numberp ret)
		     (or (decode-char '=daikanwa@rev2 ret)
			 (decode-char '=daikanwa ret))
		   (some-in-feature (lambda (c v)
				      (if (equal ret v)
					  char))
				    'morohashi-daikanwa
				    :genre 'character)))
	    (unless (member char checked)
	      (setq checked (cons char checked))
	      (or (dolist (sc (get-char-attribute char '->subsumptive))
		    (if (setq ret (char-representative-of-daikanwa
				   sc radical t checked))
			(return ret))
		    (setq checked (cons sc checked)))
		  (dolist (sc (get-char-attribute char '->denotational))
		    (if (setq ret (char-representative-of-daikanwa
				   sc radical t checked))
			(return ret))
		    (setq checked (cons sc checked)))
		  (dolist (sc (get-char-attribute char '<-subsumptive))
		    (when (setq ret (char-representative-of-daikanwa
				     sc radical t checked))
		      (return ret))
		    (setq checked (cons sc checked)))
		  (dolist (sc (get-char-attribute char '<-denotational))
		    (when (setq ret (char-representative-of-daikanwa
				     sc radical t checked))
		      (return ret))
		    (setq checked (cons sc checked)))))
	    (unless ignore-default
	      char)))))

(defun char-attributes-poly< (c1 c2 accessors testers defaulters)
  (let (a1 a2 accessor tester dm)
    (while (and accessors testers)
      (setq accessor (car accessors)
	    tester (car testers)
	    dm (car defaulters))
      (when (and accessor tester)
	(setq a1 (funcall accessor c1)
	      a2 (funcall accessor c2))
	(cond ((null a1)
	       (if a2
		   (cond ((eq dm '<)
			  (return t))
			 ((eq dm '>)
			  (return nil)))))
	      ((null a2)
	       (cond ((eq dm '<)
		      (return nil))
		     ((eq dm '>)
		      (return t))))
	      (t
	       (cond ((funcall tester a1 a2)
		      (return t))
		     ((funcall tester a2 a1)
		      (return nil))))))
      (setq accessors (cdr accessors)
	    testers (cdr testers)
	    defaulters (cdr defaulters)))))

(defun char-daikanwa-radical (char &optional radical ignore-sisters)
  (or (and (or (encode-char char '=daikanwa@rev2)
	       (encode-char char '=daikanwa))
	   (or (get-char-attribute char 'ideographic-radical@daikanwa)
	       (get-char-attribute char 'ideographic-radical)))
      (char-ideographic-radical char radical ignore-sisters)))

(defun char-daikanwa-strokes (char &optional radical)
  (unless radical
    (setq radical ideographic-radical))
  (let ((drc (char-representative-of-daikanwa char radical))
	(r (char-ideographic-radical char radical)))
    (if (and drc
	     (or (null r)
		 (= (char-ideographic-radical drc radical) r)))
	(setq char drc)))
  (char-ideographic-strokes char radical '(daikanwa)))

(defun char-daikanwa (char &optional radical checked depth)
  (unless radical
    (setq radical ideographic-radical))
  (if (or (null radical)
          (eq (or (get-char-attribute char 'ideographic-radical)
                  (char-daikanwa-radical char radical t))
              radical))
      (let ((ret (or (encode-char char '=daikanwa@rev2)
                     (encode-char char '=daikanwa)
                     (get-char-attribute char 'morohashi-daikanwa))))
	(unless ret
	  (cond
	   ((setq ret (encode-char char '=daikanwa/+p))
	    (setq ret (list ret 1)))
	   ((setq ret (encode-char char '=daikanwa/+2p))
	    (setq ret (list ret 2)))
	   ((setq ret (encode-char char '=daikanwa/ho))
	    (setq ret (list 'ho ret)))))
        (or (if ret
		(if depth
		    (if (integerp ret)
			(list ret depth)
		      (append ret (list depth)))
		  ret))
	    (and (setq ret (get-char-attribute char '=>daikanwa))
		 (if (numberp ret)
		     (list ret -10)
		   (append ret '(-10))))
	    (unless (member char checked)
	      (unless depth
		(setq depth 0))
	      (let ((i 0)
		    lnum)
		(setq checked (cons char checked))
		  (or (dolist (sc (get-char-attribute char '->subsumptive))
			(if (setq ret (char-daikanwa sc radical checked
						     (1- depth)))
			    (return ret))
			(setq checked (cons sc checked)
			      i (1+ i)))
		      (dolist (sc (get-char-attribute char '->denotational))
			(if (setq ret (char-daikanwa sc radical checked
						     (1- depth)))
			    (return ret))
			(setq checked (cons sc checked)
			      i (1+ i)))
		      (dolist (sc (get-char-attribute char '->denotational@component))
			(if (setq ret (char-daikanwa sc radical checked
						     (1- depth)))
			    (return ret))
			(setq checked (cons sc checked)
			      i (1+ i)))
		      (dolist (sc (get-char-attribute char '<-subsumptive))
			(when (setq ret (char-daikanwa sc radical checked depth))
			  (return
			    (if (numberp ret)
				(list ret 0 i)
				(if (>= (setq lnum (car (last ret))) 0)
				    (append ret (list i))
				    (nconc (butlast ret)
					   (list 0 (- lnum) i))))))
			(setq checked (cons sc checked)))
		      (dolist (sc (get-char-attribute char '<-denotational))
			(when (setq ret (char-daikanwa sc radical checked depth))
			  (return
			    (if (numberp ret)
				(list ret 0 i)
				(if (>= (setq lnum (car (last ret))) 0)
				    (append ret (list i))
				    (nconc (butlast ret)
					   (list 0 (- lnum) i))))))
			(setq checked (cons sc checked)))
		      (dolist (sc (get-char-attribute char '<-denotational@component))
			(when (setq ret (char-daikanwa sc radical checked depth))
			  (return
			    (if (numberp ret)
				(list ret 0 i)
				(if (>= (setq lnum (car (last ret))) 0)
				    (append ret (list i))
				    (nconc (butlast ret)
					   (list 0 (- lnum) i))))))
			(setq checked (cons sc checked)))
		      )))))))

(defun char-ideographic-strokes-diff (char &optional radical)
  (if (or (get-char-attribute char '<-subsumptive)
	  (get-char-attribute char '<-denotational))
      (let (s ds)
	(when (and (setq s (char-ideographic-strokes char radical))
		   (setq ds (char-daikanwa-strokes char radical)))
	  (abs (- s ds))))
    0))

(defun ideograph-char< (a b &optional radical)
  (let ((ideographic-radical (or radical
				 ideographic-radical)))
    (char-attributes-poly<
     a b
     '(char-daikanwa-strokes char-daikanwa char-ucs
			     char-ideographic-strokes-diff char-id)
     '(< morohashi-daikanwa< < < <)
     '(> > > > >))))

(defun char-ideographic-strokes-from-domains (char domains &optional radical)
  (if radical
      (get-char-feature-from-domains char 'ideographic-strokes domains
				     'ideographic-radical radical)
    (get-char-feature-from-domains char 'ideographic-strokes domains)))

(defun char-ideographic-strokes (char &optional radical preferred-domains)
   (char-ideographic-strokes-from-domains
    char (append preferred-domains
		 (cons nil
		       char-db-feature-domains))
    radical))

(defun char-total-strokes-from-domains (char domains)
  (let (ret)
    (dolist (domain domains)
      (if (setq ret (char-feature
		     char
		     (format nil "total-strokes@~a"
			     domain)))
	  (return ret)))))

(defun char-total-strokes (char &optional preferred-domains)
  (or (char-total-strokes-from-domains char preferred-domains)
      (char-feature char 'total-strokes)
      (char-total-strokes-from-domains char char-db-feature-domains)))
