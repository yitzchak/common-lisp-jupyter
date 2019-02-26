(asdf:defsystem #:common-lisp-jupyter
  :description "A Common Lisp kernel for Jupyter along with a library for building Jupyter kernels."
  :version "0.1"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (
    :alexandria
    :babel
    :bordeaux-threads
    :cl-base64
    :cl-containers
    :closer-mop
    :ironclad
    :iterate
    :jsown
    :pzmq
    :trivial-documentation
    :trivial-gray-streams
    :trivial-mimes
    :uuid)
  :components
    ((:module src
      :serial t
      :components
        ((:file "packages")
         (:file "utils")
         (:file "config")
         (:file "channel")
         (:file "heartbeat")
         (:file "message")
         (:file "shell")
         (:file "stdin")
         (:file "iopub")
         (:file "results")
         (:file "comm")
         (:module widgets
          :serial t
          :components
            ((:file "traits")
             (:file "widget")
             (:file "trait-types")
             (:file "shared-slots")
             (:file "dom-widget")
             (:file "style")
             (:file "checkbox")
             (:file "box")
             (:file "button")
             (:file "color-picker")
             (:file "controller")
             (:file "date-picker")
             (:file "progress")
             (:file "radio-buttons")
             (:file "select")
             (:file "slider")
             (:file "text")
             (:file "toggle-button")))
         (:file "kernel")
         (:file "cl-kernel")))))
