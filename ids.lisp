(in-package :chise)

(require 'split-sequence)

(defvar *ideographic-structure-to-characters-table*
  (make-hash-table :test 'equal))

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
	    (when (and (or (eql (search "U+" crep) 0)
			   (eql (search "U-" crep) 0))
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
      (cond ((character-object-p cell)
	     (ids-index-store-char product cell))
	    ((setq ret (assoc 'ideographic-structure cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((setq ret (assoc 'ideographic-structure@apparent cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((setq ret (assoc 'ideographic-structure@apparent/leftmost cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((setq ret (assoc 'ideographic-structure@apparent/rightmost cell))
	     (ids-index-store-structure product (cdr ret)))
	    ((setq ret (find-char cell))
	     (ids-index-store-char product ret))
	    ))))

(defun ids-update-index (&optional (s t))
  (format s "Updating index for `ideographic-structure'...")
  (some-in-character-feature
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   "ideographic-structure")
  (format s "done.~%")
  (format s "Updating index for `ideographic-structure@apparent'...")
  (some-in-character-feature
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   "ideographic-structure@apparent")
  (format s "done.~%")
  (format s "Updating index for `ideographic-structure@apparent/leftmost'...")
  (some-in-character-feature
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   "ideographic-structure@apparent/leftmost")
  (format s "done.~%")
  (format s "Updating index for `ideographic-structure@apparent/rightmost'...")
  (some-in-character-feature
   (lambda (c v)
     (ids-index-store-structure c v)
     nil)
   "ideographic-structure@apparent/rightmost")
  (format s "done.~%")
  (format s "Copying UCS products into =>iwds-1 abstract characters...")
  (some-in-character-feature
   (lambda (c v)
     (unless (eq v 307)
       (dolist (comp (remove c (char-ucs-chars c)))
	 (store-union-in-feature "ideographic-products" c comp c)))
     nil)
   '=>iwds-1)
  (format s "done.~%")
  (format s "Copying UCS products into =>ucs@iwds-1 abstract characters...")
  (some-in-character-feature
   (lambda (c v)
     (dolist (comp (remove c (char-ucs-chars c)))
       (store-union-in-feature "ideographic-products" c comp c))
     nil)
   '=>ucs@iwds-1)
  (format s "done.~%")
  (format s "Copying products of =>iwds-1 abstract characters into UCS...")
  (some-in-character-feature
   (lambda (c v)
     (unless (eq v 307)
       (dolist (comp (remove c (char-ucs-chars c)))
	 (store-union-in-feature "ideographic-products" comp c comp)))
     nil)
   '=>iwds-1)
  (format s "done.~%")
  (format s "Copying products of =>ucs@iwds-1 abstract characters into UCS...")
  (some-in-character-feature
   (lambda (c v)
     (dolist (comp (remove c (char-ucs-chars c)))
       (store-union-in-feature "ideographic-products" comp c comp))
     nil)
   '=>ucs@iwds-1)
  (format s "done.~%")
  )

(defun ideograph-find-products (components)
  (if (stringp components)
      (setq components (concatenate 'list components)))
  (apply #'concord:intersection-in-feature
	 "ideographic-products"
	 (mapcar (lambda (c)
		   (concord:object :character (char-id c)))
		 components))
  )
  
(defun ideographic-structure-merge-components-alist (ca1 ca2)
  (let ((dest-alist ca1)
	ret)
    (dolist (cell ca2)
      (if (setq ret (assoc (car cell) dest-alist))
	  (setf (cdr ret) (+ (cdr ret)(cdr cell)))
	  (setq dest-alist (cons cell dest-alist))))
    dest-alist))

(defun ideographic-structure-to-components-alist (structure)
  (apply #'ideographic-structure-to-components-alist* structure))

(defun ideographic-structure-to-components-alist* (operator component1 component2
						   &optional component3)
  (let (dest-alist ret)
    (setq dest-alist
	  (cond ((character-object-p component1)
		 (unless (encode-char component1 "ascii")
		   (list (cons component1 1)))
		 )
		((setq ret (assoc 'ideographic-structure component1))
		 (ideographic-structure-to-components-alist (cdr ret))
		 )
		((setq ret (find-char component1))
		 (list (cons ret 1))
		 )))
    (setq dest-alist
	  (ideographic-structure-merge-components-alist
	   dest-alist
	   (cond ((character-object-p component2)
		  (unless (encode-char component2 "ascii")
		    (list (cons component2 1)))
		  )
		 ((setq ret (assoc 'ideographic-structure component2))
		  (ideographic-structure-to-components-alist (cdr ret))
		  )
		 ((setq ret (find-char component2))
		  (list (cons ret 1))
		  ))))
    (if (member operator '(#\u2FF2 #\u2FF3))
	(ideographic-structure-merge-components-alist
	 dest-alist
	 (cond ((character-object-p component3)
		(unless (encode-char component3 "ascii")
		  (list (cons component3 1)))
		)
	       ((setq ret (assoc 'ideographic-structure component3))
		(ideographic-structure-to-components-alist (cdr ret))
		)
	       ((setq ret (find-char component3))
		(list (cons ret 1))
		)))
      dest-alist)))

(defun ids-find-merge-variables (ve1 ve2)
  (cond ((eq ve1 t)
	 ve2)
	((eq ve2 t)
	 ve1)
	(t
	 (let ((dest-alist ve1)
	       (rest ve2)
	       cell ret)
	   (while (and rest
		       (setq cell (car rest))
		       (if (setq ret (assoc (car cell) ve1))
			   (eq (cdr ret)(cdr cell))
			 (setq dest-alist (cons cell dest-alist))))
	     (setq rest (cdr rest)))
	   (if rest
	       nil
	     dest-alist)))))

(defun ideographic-structure-equal (structure1 structure2)
  (let (dest-alist ret)
    (and (setq dest-alist (ideographic-structure-character=
			   (car structure1)(car structure2)))
	 (setq ret (ideographic-structure-character=
		    (nth 1 structure1)(nth 1 structure2)))
	 (setq dest-alist (ids-find-merge-variables dest-alist ret))
	 (setq ret (ideographic-structure-character=
		    (nth 2 structure1)(nth 2 structure2)))
	 (setq dest-alist (ids-find-merge-variables dest-alist ret))
	 (if (member (car structure1) '(#\u2FF2 #\u2FF3))
	     (and (setq ret (ideographic-structure-character=
			     (nth 3 structure1)(nth 3 structure2)))
		  (setq dest-alist (ids-find-merge-variables dest-alist ret)))
	   dest-alist))))

(defun ideographic-structure-char-equal (c1 c2)
  (or (eq c1 c2)
      (if (member c2 (char-ucs-chars c1))
	  t)
      (if (member c1 (char-ucs-chars c2))
	  t)
      ))

(defun ideographic-structure-character= (c1 c2)
  (let (ret ret2)
    (cond ((character-object-p c1)
	   (cond ((encode-char c1 "ascii")
		  (list (cons c1 c2))
		  )
		 ((character-object-p c2)
		  (if (encode-char c2 "ascii")
		      (list (cons c2 c1))
		      (ideographic-structure-char-equal c1 c2))
		  )
		 ((setq ret2 (find-char c2))
		  (ideographic-structure-char-equal c1 ret2)
		  )
		 ((setq ret2 (assoc 'ideographic-structure c2))
		  (and (setq ret (get-char-attribute c1 'ideographic-structure))
		       (ideographic-structure-equal ret (cdr ret2)))
		  ))
	   )
	  ((setq ret (assoc 'ideographic-structure c1))
	   (cond ((character-object-p c2)
		  (if (encode-char c2 "ascii")
		      (list (cons c2 c1))
		    (and (setq ret2 (get-char-attribute c2 'ideographic-structure))
			 (ideographic-structure-equal (cdr ret) ret2)))
		  )
		 ((setq ret2 (find-char c2))
		  (and (setq ret2 (get-char-attribute ret2 'ideographic-structure))
		       (ideographic-structure-equal (cdr ret) ret2))
		  )
		 ((setq ret2 (assoc 'ideographic-structure c2))
		  (ideographic-structure-equal (cdr ret)(cdr ret2))
		  ))
	   )
	  ((setq ret (find-char c1))
	   (cond ((character-object-p c2)
		  (if (encode-char c2 "ascii")
		      (list (cons c2 c1))
		      (ideographic-structure-char-equal ret c2))
		  )
		 ((setq ret2 (find-char c2))
		  (ideographic-structure-char-equal ret ret2)
		  )
		 ((setq ret2 (assoc 'ideographic-structure c2))
		  (and (setq ret (get-char-attribute ret 'ideographic-structure))
		       (ideographic-structure-equal ret (cdr ret2))
		       )))))))

(defun ideographic-structure-some-chars (func structure &key require-component)
  (let ((comp-alist (ideographic-structure-to-components-alist structure))
	str)
    (cond
      (comp-alist
       (dolist (pc (ideograph-find-products (mapcar #'car comp-alist)))
	 (when (or (and
		    (setq str
			  (get-char-attribute
			   pc 'ideographic-structure))
		    (ideographic-structure-equal str structure))
		   (and
		    (setq str
			  (get-char-attribute
			   pc 'ideographic-structure@apparent))
		    (ideographic-structure-equal str structure))
		   (and
		    (setq str
			  (get-char-attribute
			   pc 'ideographic-structure@apparent/leftmost))
		    (ideographic-structure-equal str structure))
		   (and
		    (setq str
			  (get-char-attribute
			   pc 'ideographic-structure@apparent/rightmost))
		    (ideographic-structure-equal str structure)))
	   (funcall func (normalize-as-char pc))
	   ))
       )
      ((not require-component)
       (dolist (feat '("ideographic-structure"
		       "ideographic-structure@apparent"
		       "ideographic-structure@apparent/leftmost"
		       "ideographic-structure@apparent/rightmost"))
	 (some-in-character-feature
	  (lambda (pc str)
	    (when (ideographic-structure-equal str structure)
	      (funcall func (normalize-as-char pc))))
	  feat))
       ))))

(defun ideographic-structure-find-chars (structure)
  (let ((pl (gethash structure *ideographic-structure-to-characters-table*)))
    (cond
      (pl)
      (t
       (setq pl nil)
       (ideographic-structure-some-chars
	(lambda (pc)
	  (setq pl (cons pc pl))
	  nil)
	structure :require-component t)
       (setf (gethash structure *ideographic-structure-to-characters-table*)
	     pl)
       pl))))

(defun ideographic-char-count-components (char component)
  (let ((dest 0)
	structure)
    (cond ((eq char component)
	   1)
	  ((setq structure (get-char-attribute char 'ideographic-structure))
	   (dolist (cell (ideographic-structure-to-components-alist structure))
	     (setq dest
		   (+ dest
		      (if (eq (car cell) char)
			  (cdr cell)
			(* (ideographic-char-count-components (car cell) component)
			   (cdr cell))))))
	   dest)
	  (t
	   0))))

(defun ideographic-character-get-structure (character)
  "Return ideographic-structure of CHARACTER.
CHARACTER can be a character or char-spec."
  (mapcar (lambda (cell)
	    (or (and (listp cell)
		     (find-char cell))
		cell))
	  (let (ret)
	    (cond ((character-object-p character)
		   (get-char-attribute character 'ideographic-structure)
		   )
		  ((setq ret (assoc 'ideographic-structure character))
		   (cdr ret)
		   )
		  ((setq ret (find-char character))
		   (get-char-attribute ret 'ideographic-structure)
		   )))))

(defun ideographic-char-match-component (char component)
  "Return non-nil if character CHAR has COMPONENT in ideographic-structure.
COMPONENT can be a character or char-spec."
  (or (ideographic-structure-character= char component)
      (let ((str (ideographic-character-get-structure char)))
	(and str
	     (or (ideographic-char-match-component (nth 1 str) component)
		 (ideographic-char-match-component (nth 2 str) component)
		 (if (member (car str) '(#\u2FF2 #\u2FF3))
		     (ideographic-char-match-component (nth 3 str) component)))))))

(defun ideographic-structure-char< (a b)
  (let ((sa (get-char-attribute a 'ideographic-structure))
	(sb (get-char-attribute b 'ideographic-structure))
	tsa tsb)
    (cond (sa
	   (cond (sb
		  (setq tsa (char-total-strokes a)
			tsb (char-total-strokes b))
		  (if tsa
		      (if tsb
			  (or (< tsa tsb)
			      (and (= tsa tsb)
				   (ideograph-char< a b)))
			t)
		    (if tsb
			nil
		      (ideograph-char< a b))))
		 (t
		  nil))
	   )
	  (t
	   (cond (sb
		  t)
		 (t
		  (setq tsa (char-total-strokes a)
			tsb (char-total-strokes b))
		  (if tsa
		      (if tsb
			  (or (< tsa tsb)
			      (and (= tsa tsb)
				   (ideograph-char< a b)))
			t)
		    (if tsb
			nil
		      (ideograph-char< a b)))
		  ))
	   ))
    ))

(defun ideo-comp-tree-adjoin (tree char)
  (let ((rest tree)
	included ; other
	dest cell finished)
    (while (and (not finished)
		rest)
      (setq cell (pop rest))
      (cond ((ideographic-structure-character= char (car cell))
	     (setq finished t
		   dest tree
		   rest nil)
	     )
	    ((ideographic-char-match-component char (car cell))
	     (setq dest
		   (cons (cons (car cell)
			       (ideo-comp-tree-adjoin (cdr cell) char))
			 dest))
	     (setq finished t)
	     )
	    ((ideographic-char-match-component (car cell) char)
	     (setq included (cons cell included))
	     )
            ;; (included
            ;;  (setq other (cons cell other))
            ;;  )
	    (t
	     (setq dest (cons cell dest))
	     )))
    (cond (finished
	   (nconc dest rest)
	   )
	  (included
	   (cons (cons char included)
		 (nconc dest rest))
	   )
	  (t
	   (cons (list char) tree)
	   ))))

(defun ideographic-chars-to-is-a-tree* (chars)
  (let (tree)
    (dolist (char (sort chars #'ideographic-structure-char<))
      (setq tree (ideo-comp-tree-adjoin tree char)))
    tree))

(defun ideographic-chars-to-is-a-tree (chars)
  (ideographic-chars-to-is-a-tree* (copy-list chars)))

(defun ids-find-chars-including-ids (structure)
  (let (comp-alist comp-spec ret str rest)
    (cond
     ((character-object-p structure)
      (setq rest (get-char-attribute structure 'ideographic-products))
      )
     ((setq ret (ideographic-structure-find-chars structure))
      ;; (dolist (pc ret)
      ;; 	(setq rest
      ;; 	      (union
      ;; 	       rest
      ;; 	       (copy-list (get-char-attribute pc 'ideographic-products)))))
      (setq rest (apply #'union-in-character-feature "ideographic-products" ret))
      )
     (t
      (setq comp-alist (ideographic-structure-to-components-alist structure)
	    comp-spec (list (cons 'ideographic-structure structure)))
      (dolist (pc (ideograph-find-products (mapcar #'car comp-alist))
		  ;; (caar
		  ;;  (sort (mapcar (lambda (cell)
		  ;; 		   (if (setq ret (get-char-attribute
		  ;; 				  (car cell) 'ideographic-products))
		  ;; 		       (cons ret (length ret))
		  ;; 		     (cons nil 0)))
		  ;; 		 comp-alist)
		  ;; 	 (lambda (a b)
		  ;; 	   (< (cdr a)(cdr b)))))
		  )
	(when (and (every (lambda (cell)
			    (>= (ideographic-char-count-components pc (car cell))
				(cdr cell)))
			  comp-alist)
		   (or (ideographic-char-match-component pc comp-spec)
		       ;; (and (setq str (get-char-attribute pc 'ideographic-structure))
		       ;; 	    (ideographic-char-match-component
		       ;; 	     (list
		       ;; 	      (cons
		       ;; 	       'ideographic-structure
		       ;; 	       (functional-ideographic-structure-to-apparent-structure
		       ;; 		str)))
		       ;; 	     comp-spec))
		       (and (setq str
				  (get-char-attribute
				   pc 'ideographic-structure@apparent))
			    (ideographic-char-match-component
		       	     (list (cons 'ideographic-structure str))
			     comp-spec))
		       (and (setq str
				  (get-char-attribute
				   pc 'ideographic-structure@apparent/leftmost))
			    (ideographic-char-match-component
		       	     (list (cons 'ideographic-structure str))
			     comp-spec))
		       (and (setq str
				  (get-char-attribute
				   pc 'ideographic-structure@apparent/rightmost))
			    (ideographic-char-match-component
		       	     (list (cons 'ideographic-structure str))
			     comp-spec))
		       ))
	  (push pc rest)))
      ))
    rest))

(defun ids-find-chars-including-ids-as-is-a-tree (structure)
  (ideographic-chars-to-is-a-tree*
   (ids-find-chars-including-ids structure)))

(defun ideographic-structure-compact (structure)
  (let ((rest structure)
	cell
	ret dest sub)
    (while rest
      (setq cell (pop rest))
      (if (and (consp cell)
	       (setq ret (find-char cell)))
	  (setq cell ret))
      (cond
       ((and (consp cell)
	     (cond ((setq ret (assoc 'ideographic-structure cell))
		    (setq sub (cdr ret))
		    )
		   ((atom (car cell))
		    (setq sub cell)
		    )))
	(setq cell
	      (cond ((setq ret (ideographic-structure-find-chars sub))
		     (car ret)
		     )
		    ((setq ret (ideographic-structure-compact sub))
		     (list (cons 'ideographic-structure ret))
		     )
		    (t
		     (list (cons 'ideographic-structure sub))))
	      )
	))
      (setq dest (cons cell dest)))
    (nreverse dest)))

(defun functional-ideographic-structure-to-apparent-structure (structure)
  (ideographic-structure-compare-functional-and-apparent
   structure :conversion-only t))

(defun ideographic-structure-compare-functional-and-apparent (structure
							      &key conversion-only)
  (let (enc enc-str enc2-str enc3-str new-str new-str-c
	    f-res a-res ret code)
    (cond
     ((eq (car structure) #\⿸)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((character-object-p enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assoc 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) #\⿰)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿱
			      (nth 2 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿰ (nth 1 enc-str) new-str-c)
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿰ (nth 1 enc-str) new-str-c)
		       111)
		 ))
	  )
	 ((and (eq (car enc-str) #\⿲)
	       (member (char-ucs (nth 1 enc-str)) '(#x4EBB #x2E85))
	       (eq (nth 2 enc-str) #\丨))
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿱
			      (nth 3 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿰ (decode-char '=big5-cdp #x8B7A) new-str-c)
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿰ (decode-char '=big5-cdp #x8B7A) new-str-c)
		       112)
		 ))
	  )
	 ((eq (car enc-str) #\⿱)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str
		(list
		 (cond
		  ((character-object-p (nth 2 enc-str))
		   (if (or (member (encode-char (nth 2 enc-str) '=>ucs@component)
				 '(#x20087 #x5382 #x4E06))
			   (eq (encode-char (nth 2 enc-str) '=>ucs@iwds-1)
			       #x4E06)
			   (eq (encode-char (nth 2 enc-str) '=ucs-itaiji-001)
			       #x2E282)
			   (eq (encode-char (nth 2 enc-str) '=big5-cdp)
			       #x89CE)
			   (eq (encode-char (nth 2 enc-str) '=>big5-cdp)
			       #x88E2)
			   (eq (encode-char (nth 2 enc-str) '=big5-cdp)
			       #x88AD)
			   (eq (or (encode-char (nth 2 enc-str) '=>big5-cdp)
				   (encode-char (nth 2 enc-str) '=big5-cdp-itaiji-001))
			       #x8766)
			   (eq (car (get-char-attribute (nth 2 enc-str)
							'ideographic-structure))
			       #\⿸))
		       #\⿸
		     #\⿰))
		  ((eq (car (cdr (assoc 'ideographic-structure (nth 2 enc-str))))
		       #\⿸)
		   #\⿸)
		  (t
		   #\⿰))
		 (nth 2 enc-str)
		 (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿱ (nth 1 enc-str) new-str-c)
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿱ (nth 1 enc-str) new-str-c)
		       (if (eq (car new-str) #\⿸)
			   121
			   122))
		 ))
	  )
	 ((eq (car enc-str) #\⿸)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list (cond
			       ((character-object-p (nth 2 enc-str))
				(if (member (char-ucs (nth 2 enc-str))
					  '(#x5F73))
				    #\⿰
				  #\⿱)
				)
			       (t
				#\⿱))
			      (nth 2 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿸ (nth 1 enc-str) new-str-c)
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿸ (nth 1 enc-str) new-str-c)
		       (if (eq (car new-str) #\⿰)
			   131
			   132))
		 ))
	  )))
      )
     ((eq (car structure) #\⿹)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((character-object-p enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assoc 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) #\⿰)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿱
			      (nth 1 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿰ new-str-c (nth 2 enc-str))
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿰ new-str-c (nth 2 enc-str))
		       210)
		 ))
	  )
	 ((eq (car enc-str) #\⿱)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿰
			      (nth 2 structure)
			      (nth 2 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿱ (nth 1 enc-str) new-str-c)
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿱ (nth 1 enc-str) new-str-c)
		       220)
		 ))
	  )
	 ))
      )
     ((eq (get-char-attribute (car structure) '=ucs-itaiji-001) #x2FF6)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((character-object-p enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assoc 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) #\⿺)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿱
			      (nth 2 structure)
			      (nth 1 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿺ new-str-c (nth 2 enc-str))
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿺ new-str-c (nth 2 enc-str))
		       310)
		 ))
	  )
	 ((eq (car enc-str) #\⿱)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿰
			      (nth 2 structure)
			      (nth 1 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿱ new-str-c (nth 2 enc-str))
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿱ new-str-c (nth 2 enc-str))
		       320)
		 ))
	  )
	 ((eq (car enc-str) #\⿰)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿱
			      (nth 2 structure)
			      (nth 1 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿰ new-str-c (nth 2 enc-str))
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿰ new-str-c (nth 2 enc-str))
		       330)
		 ))
	  ))
	)
      )
     ((eq (car structure) #\⿴)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((character-object-p enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assoc 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) #\⿱)
	  (cond
	   ((and (character-object-p (nth 2 enc-str))
		 (or (member (char-ucs (nth 2 enc-str)) '(#x56D7 #x5F51 #x897F))
		     (eq (char-feature (nth 2 enc-str) '=>big5-cdp)
			 #x87A5)))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿴
				(nth 2 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ (nth 1 enc-str) new-str-c)
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ (nth 1 enc-str) new-str-c)
			 411)
		   ))
	    )
	   ((and (character-object-p (nth 2 enc-str))
		 (eq (char-ucs (nth 2 enc-str)) #x51F5))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿶
				(nth 2 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ (nth 1 enc-str) new-str-c)
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ (nth 1 enc-str) new-str-c)
			 412)
		   ))
	    )	    
	   ((and (character-object-p (nth 1 enc-str))
		 (eq (char-feature (nth 1 enc-str) '=>ucs@component)
		     #x300E6))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿵
				(nth 1 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ new-str-c (nth 2 enc-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ new-str-c (nth 2 enc-str))
			 413)
		   ))
	    )
	   (t
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿱ (nth 2 structure) (nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ (nth 1 enc-str) new-str-c)
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ (nth 1 enc-str) new-str-c)
			 414)
		   ))
	    ))
	  )
	 ((eq (car enc-str) #\⿳)
	  (cond
	   ((and (character-object-p (nth 2 enc-str))
		 (eq (char-ucs (nth 2 enc-str)) #x56D7))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿴ (nth 2 enc-str) (nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (setq new-str (list #\⿱ (nth 1 enc-str) new-str-c))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱  new-str-c (nth 3 enc-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱  new-str-c (nth 3 enc-str))
			 415)
		   ))
	    )
	   ((and (character-object-p (nth 2 enc-str))
		 (eq (char-ucs (nth 2 enc-str)) #x5196))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿱ (nth 1 enc-str) (nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (setq new-str (list #\⿱ new-str-c (nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ new-str-c (nth 3 enc-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ new-str-c (nth 3 enc-str))
			 416)
		   ))
	    )
	   ((and (character-object-p (nth 2 enc-str))
		 (or (eq (encode-char (nth 2 enc-str) '=>big5-cdp)
			 #x89A6)
		     (eq (encode-char (nth 2 enc-str) '=>gt-k)
			 146)
		     (eq (char-ucs (nth 2 enc-str)) #x2008A)))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿱ (nth 2 structure) (nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (setq new-str (list #\⿸ new-str-c (nth 3 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ (nth 1 enc-str) new-str-c)
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ (nth 1 enc-str) new-str-c)
			 417)
		   ))
	    )
	   (t
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿻ (nth 2 enc-str) (nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (setq new-str (list #\⿱ (nth 1 enc-str) new-str-c))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱  new-str-c (nth 3 enc-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱  new-str-c (nth 3 enc-str))
			 419)
		   ))
	    ))
	  )
	 ((eq (car enc-str) #\⿰)
	  (cond
	   ((equal (nth 1 enc-str)(nth 2 enc-str))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿲
				(nth 1 enc-str)
				(nth 2 structure)
				(nth 2 enc-str)))
	    (setq new-str-c
		  (list (cons 'ideographic-structure new-str)))
	    (cond (conversion-only
		   new-str)
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 new-str
			 421)
		   ))
	    )
	   (t
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿰
				(nth 2 structure)
				(nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿰ (nth 1 enc-str) new-str-c)
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿰ (nth 1 enc-str) new-str-c)
			 422)
		   ))
	    ))
	  ))
	)
      )
     ((eq (car structure) #\⿶)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((character-object-p enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assoc 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) #\⿱)
	  (setq enc2-str (ideographic-character-get-structure (nth 1 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) #\⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿲
				(nth 1 enc2-str)
				(nth 2 structure)
				(nth 2 enc2-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ new-str-c (nth 2 enc-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ new-str-c (nth 2 enc-str))
			 511)
		   ))
	    )
	  )
	 ((eq (car enc-str) #\⿳)
	  (setq enc2-str (ideographic-character-get-structure (nth 1 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) #\⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿲
				(nth 1 enc2-str)
				(nth 2 structure)
				(nth 2 enc2-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿳ new-str-c (nth 2 enc-str) (nth 3 enc-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿳ new-str-c (nth 2 enc-str) (nth 3 enc-str))
			 512)
		   ))
	    )
	  )
	 ((eq (car enc-str) #\⿲)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿱
			      (nth 2 structure)
			      (nth 2 enc-str)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿲ (nth 1 enc-str) new-str-c (nth 3 enc-str))
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿲ (nth 1 enc-str) new-str-c (nth 3 enc-str))
		       520)
		 ))
	  )
	 ((eq (car enc-str) #\⿴)
	  (setq enc2-str (ideographic-character-get-structure (nth 1 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) #\⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿱
				(nth 2 structure)
				(nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿲ (nth 1 enc2-str) new-str-c (nth 2 enc2-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿲ (nth 1 enc2-str) new-str-c (nth 2 enc2-str))
			 530)
		   ))
	    )
	  )))
      )
     ((eq (car structure) #\⿵)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((character-object-p enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assoc 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) #\⿱)	  
	  (cond
	   ((and (character-object-p (nth 2 enc-str))
		 (member (char-ucs (nth 2 enc-str))
		       '(#x9580 #x9B25)))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿵
				(nth 2 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ (nth 1 enc-str) new-str-c)
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ (nth 1 enc-str) new-str-c)
			 601)
		   ))
	    )
	   ((and (setq enc2-str (ideographic-character-get-structure (nth 2 enc-str)))
		 (cond
		  ((eq (car enc2-str) #\⿰)
		   (setq code 611)
		   )
		  ((eq (car enc2-str) #\⿲)
		   (setq code 614)
		   )
		  ((and (eq (car enc2-str) #\⿱)
			(setq enc3-str
			      (ideographic-character-get-structure (nth 2 enc2-str)))
			(eq (car enc3-str) #\⿰))
		   (setq code 613)
		   )))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str
		  (cond ((eq code 611)
			 (list #\⿲
			       (nth 1 enc2-str)
			       (nth 2 structure)
			       (nth 2 enc2-str))
			 )
			((eq code 613)
			 (list #\⿲
			       (nth 1 enc3-str)
			       (nth 2 structure)
			       (nth 2 enc3-str))
			 )
			((eq code 614)
			 (list #\⿲
			       (nth 1 enc2-str)
			       (list (list 'ideographic-structure
					   #\⿱
					   (nth 2 enc2-str)
					   (nth 2 structure)))
			       (nth 3 enc2-str))
			 )))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure
				(ideographic-structure-compact new-str)))))
	    (cond (conversion-only
		   (cond ((or (eq code 611)
			      (eq code 614))
			  (list #\⿱ (nth 1 enc-str) new-str-c)
			  )
			 ((eq code 613)
			  (list #\⿳ (nth 1 enc-str)(nth 1 enc2-str) new-str-c)
			  ))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (cond ((or (eq code 611)
				    (eq code 614))
				(list #\⿱ (nth 1 enc-str) new-str-c)
				)
			       ((eq code 613)
				(list #\⿳ (nth 1 enc-str)(nth 1 enc2-str) new-str-c)
				))
			 code)
		   ))
	    ))
	  )
	 ((eq (car enc-str) #\⿳)
	  (setq enc2-str (ideographic-character-get-structure (nth 3 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) #\⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿲
				(nth 1 enc2-str)
				(nth 2 structure)
				(nth 2 enc2-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿳ (nth 1 enc-str) (nth 2 enc-str) new-str-c)
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿳ (nth 1 enc-str) (nth 2 enc-str) new-str-c)
			 612)
		   ))
	    )
	  )
	 ((eq (car enc-str) #\⿲)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿱
			      (nth 2 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿲ (nth 1 enc-str) new-str-c (nth 3 enc-str))
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿲ (nth 1 enc-str) new-str-c (nth 3 enc-str))
		       620)
		 ))
	  )
	 ((eq (car enc-str) #\⿴)
	  (setq enc2-str (ideographic-character-get-structure (nth 1 enc-str)))
	  (when (and enc2-str
		     (eq (car enc2-str) #\⿰))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿱
				(nth 2 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿲ (nth 1 enc2-str) new-str-c (nth 2 enc2-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿲ (nth 1 enc2-str) new-str-c (nth 2 enc2-str))
			 630))
		  ))
	  )
	 ((eq (car enc-str) #\⿵)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿱
			      (nth 2 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿵ (nth 1 enc-str) new-str-c)
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿵ (nth 1 enc-str) new-str-c)
		       640)
		 ))
	  )
	 ))
      )
     ((eq (car structure) #\⿷)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((character-object-p enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assoc 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) #\⿺)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (setq new-str (list #\⿱
			      (nth 2 enc-str)
			      (nth 2 structure)))
	  (setq new-str-c
		(if (setq ret (ideographic-structure-find-chars new-str))
		    (car ret)
		  (list (cons 'ideographic-structure new-str))))
	  (cond (conversion-only
		 (list #\⿺ (nth 1 enc-str) new-str-c)
		 )
		(t
		 (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		 (list enc
		       f-res
		       new-str-c
		       a-res
		       (list #\⿺ (nth 1 enc-str) new-str-c)
		       710)
		 ))
	  )
	 ((eq (car enc-str) #\⿸)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (cond
	   ((and (character-object-p (nth 2 enc-str))
		 (or (member (char-ucs (nth 2 enc-str))
			   '(#x4EBA #x5165 #x513F #x51E0))
		     (member (or (encode-char (nth 2 enc-str) '=>ucs@iwds-1)
			       (encode-char (nth 2 enc-str) '=>ucs@component))
			   '(#x4EBA #x513F))))
	    (setq new-str (list #\⿺
				(nth 2 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿸ (nth 1 enc-str) new-str-c)
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿸ (nth 1 enc-str) new-str-c)
			 721)
		   ))
	    )
	   (t
	    (setq new-str (list #\⿱
				(nth 2 structure)
				(nth 2 enc-str)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿸ (nth 1 enc-str) new-str-c)
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿸ (nth 1 enc-str) new-str-c)
			 722)
		   ))
	    ))
	  )
	 ))
      )
     ((eq (car structure) #\⿺)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((character-object-p enc)
			 (or (get-char-attribute enc 'ideographic-structure)
			     (get-char-attribute enc 'ideographic-structure@apparent)
			     (get-char-attribute enc 'ideographic-structure@apparent/leftmost)
			     (get-char-attribute enc 'ideographic-structure@apparent/rightmost))
			 )
			((consp enc)
			 (or (cdr (assoc 'ideographic-structure enc))
			     (cdr (assoc 'ideographic-structure@apparent enc))
			     (cdr (assoc 'ideographic-structure@apparent/leftmost enc))
			     (cdr (assoc 'ideographic-structure@apparent/rightmost enc)))
			 )))
        ;; (setq enc-str
        ;;       (mapcar (lambda (cell)
        ;;                 (or (and (listp cell)
        ;;                          (find-char cell))
        ;;                     cell))
        ;;               enc-str))
	(cond
	 ((eq (car enc-str) #\⿱)
	  (cond
	   ((and (character-object-p (nth 1 enc-str))
		 (or (and (eq (char-ucs (nth 1 enc-str)) #x200CA)
			  (setq code 811))
		     (and (eq (char-feature (nth 1 enc-str) '=>iwds-1) 233)
			  (character-object-p (nth 2 structure))
			  (eq (char-ucs (nth 2 structure)) #x4E36)
			  (setq code 812))))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿺
				(nth 1 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ new-str-c (nth 2 enc-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ new-str-c (nth 2 enc-str))
			 code)
		   ))
	    )
	   ((and (character-object-p (nth 2 enc-str))
		 (or (member (char-ucs (nth 2 enc-str))
			   '(#x4E00
			     #x706C
			     #x65E5 #x66F0 #x5FC3
			     #x2123C #x58EC #x738B #x7389))
		     (member (encode-char (nth 2 enc-str) '=>ucs@component)
			   '(#x2123C #x58EC))
		     (eq (encode-char (nth 2 enc-str) '=>ucs@iwds-1)
			 #x7389)
		     (eq (encode-char (nth 2 enc-str) '=>big5-cdp)
			 #x8D71)))
	    (unless conversion-only
	      (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	    (setq new-str (list #\⿰
				(nth 1 enc-str)
				(nth 2 structure)))
	    (setq new-str-c
		  (if (setq ret (ideographic-structure-find-chars new-str))
		      (car ret)
		    (list (cons 'ideographic-structure new-str))))
	    (cond (conversion-only
		   (list #\⿱ new-str-c (nth 2 enc-str))
		   )
		  (t
		   (setq a-res (ids-find-chars-including-ids-as-is-a-tree new-str))
		   (list enc
			 f-res
			 new-str-c
			 a-res
			 (list #\⿱ new-str-c (nth 2 enc-str))
			 813)
		   ))
	    )
	   ))))
      )
     ((eq (car structure) #\⿻)
      (setq enc (nth 1 structure))
      (when (setq enc-str
		  (cond ((character-object-p enc)
			 (get-char-attribute enc 'ideographic-structure)
			 )
			((consp enc)
			 (cdr (assoc 'ideographic-structure enc))
			 )))
	(cond
	 ((eq (car enc-str) #\⿱)
	  (unless conversion-only
	    (setq f-res (ids-find-chars-including-ids-as-is-a-tree enc-str)))
	  (if conversion-only
	      (list #\⿳ (nth 1 enc-str) (nth 2 structure) (nth 2 enc-str))
	    (list enc
		  f-res
		  new-str
		  nil
		  (list #\⿳
			(nth 1 enc-str)
			(nth 2 structure)
			(nth 2 enc-str))
		  911))
	  )))
      ))
    ))
