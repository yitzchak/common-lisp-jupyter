(asdf:defsystem #:jupyter-lab-extension
  :components ((:module src
                :pathname "src/lab-extension/"
                :serial t
                :components ((:file "packages")
                             (:file "asdf")))))
