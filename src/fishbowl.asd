
(asdf:defsystem #:fishbowl
  :description "An Enhanced Interactive Shell for Common Lisp (based on the Ipython protocol)."
  :version "0.3"
  :author "Frederic Peschanski (format nil \"<frederic~Apeschanski~Awork~Agmail~Acom>\" \".\" \".\" \"@\" \".\")" 
  :license "BSD 2-Clause. See LICENSE."
  :depends-on (:pzmq
               :bordeaux-threads
               :uuid
	       ;; :ironclad  (for signed messages)
	       :cl-base64)
  :serial t
  :components ((:file "packages")
               (:file "utils")
	       (:file "myjson")
	       (:file "config")
               (:file "message")
               (:file "shell")
	       (:file "iopub")
	       (:file "display")
	       (:file "evaluator")
               (:file "user")
               (:file "kernel")))
