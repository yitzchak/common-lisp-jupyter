
(asdf:defsystem #:uncommonshell
  :description "A Common Lisp kernel for the Ipython protocol."
  :version "0.2"
  :author "Frederic Peschanski (format nil \"<frederic~Apeschanski~Awork~Agmail~Acom>\" \".\" \".\" \"@\" \".\")" 
  :license "BSD 2-Clause. See LICENSE."
  :depends-on (:pzmq
               :bordeaux-threads
               :uuid
               :ironclad)
  :serial t
  :components ((:file "packages")
               (:file "utils")
	       (:file "myjson")
               (:file "message")
               (:file "shell")
	       (:file "iopub")
	       (:file "evaluator")
               (:file "kernel")))
