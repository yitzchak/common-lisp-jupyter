(asdf:defsystem #:common-lisp-jupyter
  :description "An Enhanced Interactive Shell for Common Lisp (based on the Jupyter protocol)."
  :version "0.7"
  :author "Tarn W. Burton"
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
  :components ((:module src
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
                             (:file "cl-kernel")))))
