(defsystem :cl-chise
  :description "CHISE implementation based on Common Lisp"
  :version "1.2"
  :author "Tomohiko Morioka"
  :licence "LGPL"
  :depends-on (:cl-concord :split-sequence :cl-json)
  :serial t
  :components ((:file "cl-chise")
	       (:file "entity-ref")
	       (:file "ids" :depends-on ("cl-chise" "entity-ref"))
	       (:file "chise-metadata")
	       (:file "loadup-char-defs" :depends-on ("cl-chise"
						      "entity-ref"
						      "ids"
						      "chise-metadata"))
	       (:file "chise-json")))
