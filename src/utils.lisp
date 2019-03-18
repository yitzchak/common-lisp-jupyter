(in-package #:jupyter)

#|

# CommonTypes: Utilities #

|#

; (defvar maxima::$kernel_info nil)

(defparameter v:*process-locally* t)

(defun fatal-error (format-control &rest format-arguments)
  (v:fatal :kernel 'simple-error :format-control format-control
                                 :format-arguments format-arguments)
  (apply #'error format-control format-arguments))

(defun make-uuid ()
  (string-downcase (remove #\- (format nil "~W" (uuid:make-v4-uuid)))))

(defun install-kernel (&key argv class display-name kernel-name language resources)
  "Install a kernel spec file given a kernel name and a language name."
  (let* ((kernel-directory
           (merge-pathnames
             (make-pathname :directory (list :relative
                                             ; Just in case HFS+ is case-sensitive
                                             (if (uiop:os-macosx-p) "Jupyter" "jupyter")
                                             "kernels"
                                             kernel-name))
                       (cond
                         ((uiop:os-macosx-p)
                           (merge-pathnames
                             (make-pathname :directory '(:relative "Library"))
                             (uiop:getenv-pathname "HOME" :ensure-directory t)))
                         ((uiop:os-windows-p)
                           (uiop:get-folder-path :appdata))
                         (t
                           (uiop:xdg-data-home)))))
            (kernel-path (merge-pathnames (make-pathname :name "kernel" :type "json") kernel-directory))
            (image-path (merge-pathnames (make-pathname :name "image" :type (when (uiop:os-windows-p) "exe")) kernel-directory)))
  (format t "Installing kernel spec file ~A~%" kernel-path)
  (ensure-directories-exist kernel-path)
  (with-open-file (stream kernel-path :direction :output :if-exists :supersede)
    (write-string
      (jsown:to-json
        (jsown:new-js
          ("argv" (or argv (list (namestring image-path) "{connection_file}")))
          ("display_name" display-name)
          ("language" language)))
      stream))
  (iter
    (for src in resources)
    (for dest next (merge-pathnames kernel-directory src))
    (format t "Installing kernel resource ~A~%" dest)
    (copy-file src dest))
  (unless argv
    (setq uiop:*image-entry-point*
      `(lambda ()
        (run-kernel
          (find-symbol ,(symbol-name class) ,(symbol-package class))
          (first (uiop:command-line-arguments)))))
    (format t "Creating kernel image ~A~%" image-path)
    (uiop:dump-image image-path :executable t))
  t))

(defun json-getf (object indicator &optional default)
  "Safe accessor for the internal JSON format that behaves like getf"
  (iter
    (for (key . value) in (cdr object))
    (when (string= indicator key)
      (leave value))
    (finally
      (return default))))
