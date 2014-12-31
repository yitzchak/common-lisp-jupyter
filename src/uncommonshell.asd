
(asdf:defsystem #:uncommonshell
  :description "A Common Lisp kernel for the Ipython protocol."
  :version "0.1"
  :author "Frederic Peschanski (format nil \"<frederic~Apeschanski~Awork~Agmail~Acom>\" \".\" \".\" \"@\" \".\")" 
  :license "BSD 2-Clause. See LICENSE."
  :depends-on (:cl-json
               :pzmq
               :bordeaux-threads
               :uuid
               :ironclad)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "message")
               (:file "shell")
	       (:file "evaluator")
               (:file "kernel")))



  

