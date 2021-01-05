(asdf:defsystem #:common-lisp-jupyter
  :description "A Common Lisp kernel for Jupyter along with a library for building Jupyter kernels."
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:alexandria
     :babel
     :bordeaux-threads
     :cl-base64
     :cl-indentify
     :closer-mop
     :dissect
     ; This should be `(:feature (:not :clasp) :ironclad)` but some distributions still don't have
     ; ASDF 3
     #-clasp :eclector
     #-clasp :ironclad
     :iterate
     :jsown
     :multilang-documentation
     :pzmq
     :puri
     :static-vectors
     :trivial-do
     :trivial-garbage
     :trivial-gray-streams
     :trivial-mimes)
  :components
    ((:module res
      :components
        ((:module ccl
          :components
            ((:static-file "logo-64x64.png")))
         (:module cl
          :components
            ((:static-file "logo-64x64.png")))
         (:module clisp
          :components
            ((:static-file "logo-64x64.png")))
         (:module ecl
          :components
            ((:static-file "logo-64x64.png")))
         (:module sbcl
          :components
            ((:static-file "logo-64x64.png")))))
     (:module src
      :serial t
      :components
        ((:file "packages")
         (:file "utils")
         (:file "queue")
         (:file "config")
         (:file "log")
         (:file "mac")
         (:file "channel")
         (:file "heartbeat")
         (:file "message")
         (:file "shell")
         (:file "stdin")
         (:file "control")
         (:file "iopub")
         (:file "results")
         (:file "comm")
         (:module formatters
          :serial t
          :components
           ((:file "markdown")))
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
             (:file "combobox")
             (:file "box")
             (:file "button")
             (:file "color-picker")
             (:file "controller")
             (:file "date-picker")
             (:file "file-upload")
             (:file "label")
             (:file "link")
             (:file "media")
             (:file "output")
             (:file "progress")
             (:file "select")
             (:file "slider")
             (:file "text")
             (:file "toggle-button")
             (:file "valid")
             (:file "interactive")))
         (:file "history")
         (:file "matches")
         (:file "kernel")
         (:file "installer")
         (:module cl-jupyter
          :serial t
          :components
            ((:file "utils")
             (:file "parser")
             (:file "kernel")
             (:file "complete")
             (:file "inspect")
             (:file "installer")
             (:file "overrides")
             (:file "ros-install")))
         (:file "convert"))))
  . #+asdf3
      (:version "0.1"
       :homepage "https://yitzchak.github.io/common-lisp-jupyter/"
       :bug-tracker "https://github.com/yitzchak/common-lisp-jupyter/issues")
    #-asdf3 ())


#+asdf3.1
  (asdf:register-system-packages "common-lisp-jupyter"
                                 '(:jupyter :common-lisp-jupyter :jupyter-convert
                                   :jupyter/markdown-formatter :jupyter-widgets))


