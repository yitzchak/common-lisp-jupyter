(in-package #:jupyter/lab-extension)

(defun extension-pathname (name)
  (merge-pathnames (make-pathname :directory (list :relative
                                                   #+darwin "Jupyter"
                                                   #-darwin "jupyter"
                                                   "labextensions"
                                                   name))
                   #+darwin (merge-pathnames (make-pathname :directory '(:relative "Library"))
                                             (uiop:getenv-pathname "HOME" :ensure-directory t))
                   #+windows (uiop:get-folder-path :appdata)
                   #-(or darwin windows) (uiop:xdg-data-home)))

(defclass jupyter-lab-extension (asdf:file-component)
  ()
  (:default-initargs :type "lab-extension"))

(defmethod asdf:input-files ((op asdf:compile-op) (c jupyter-lab-extension))
  (remove-if (lambda (x)
               (not (uiop:file-pathname-p x)))
             (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
                                                        :name :wild :type :wild :version :wild)
                                         (asdf:component-pathname c)))))

(defmethod asdf:output-files ((op asdf:compile-op) (c jupyter-lab-extension))
  (values (loop with source = (asdf:component-pathname c)
                with destination = (extension-pathname (asdf:component-name c))

                for x in (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
                                                                    :name :wild :type :wild :version :wild)
                                                     source))
                when (uiop:file-pathname-p x)
                  collect (merge-pathnames (uiop:subpathp x source) destination))
          t))

(defmethod asdf:perform ((op asdf:compile-op) (c jupyter-lab-extension))
  (loop for input in (asdf:input-files op c)
        for output in (asdf:output-files op c)
        do (ensure-directories-exist output)
           (uiop:copy-file input output)))

(defmethod asdf:perform ((op asdf:load-op) (c jupyter-lab-extension)))

(setf (find-class 'asdf::jupyter-lab-extension) (find-class 'jupyter-lab-extension))
