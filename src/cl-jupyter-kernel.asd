(asdf:defsystem #:cl-jupyter-kernel
  :description "An Enhanced Interactive Shell for Common Lisp (based on the Jupyter protocol)."
  :version "0.6"
  :author "Frederic Peschanski (format nil \"<frederic~Apeschanski~Awork~Agmail~Acom>\" \".\" \".\" \"@\" \".\")"
  :license "BSD 2-Clause. See LICENSE."
  :depends-on (:pzmq
               :bordeaux-threads
               :uuid
               :babel
               :ironclad
               :iterate
               :cl-base64
               :cl-containers
               :jsown
               :trivial-gray-streams
               :trivial-mimes)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "config")
               (:file "channel")
               (:file "heartbeat")
               (:file "message")
               (:file "shell")
               (:file "stdin")
               (:file "iopub")
               (:file "results")
               (:file "kernel")
               (:file "evaluator")
               (:file "cl-kernel")))
