(defsystem :cl-chise
  :description "CHISE implementation based on Common Lisp"
  :version "0.0"
  :author "Tomohiko Morioka"
  :licence "LGPL"
  :depends-on (:cl-redis)
  :serial t
  :components ((:file "cl-chise")))
