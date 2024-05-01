(defsystem :cl-chise
  :description "CHISE implementation based on Common Lisp"
  :version "0.3"
  :author "Tomohiko Morioka"
  :licence "LGPL"
  :depends-on (:cl-concord)
  :serial t
  :components ((:file "cl-chise")
	       (:file "entity-ref")
	       (:file "loadup-char-defs" :depends-on ("cl-chise" "entity-ref"))))
