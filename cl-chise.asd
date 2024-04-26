(defsystem :cl-chise
  :description "CHISE implementation based on Common Lisp"
  :version "0.2"
  :author "Tomohiko Morioka"
  :licence "LGPL"
  :depends-on (:cl-concord)
  :serial t
  :components ((:file "cl-chise")
	       (:file "loadup-char-defs" :depends-on ("cl-chise"))))
