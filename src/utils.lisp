(in-package #:jupyter)

#|

# CommonTypes: Utilities #

|#

; (defvar maxima::$kernel_info nil)

(defun make-uuid ()
  (remove #\- (format nil "~(~A~)" (uuid:make-v4-uuid))))

(defun kernel-home (&key kernel-name system)
  (merge-pathnames
    (make-pathname
      :directory (list :relative
                       ; Just in case HFS+ is case-sensitive
                       (if (uiop:os-macosx-p) "Jupyter" "jupyter")
                         "kernels"
                         kernel-name))
    (cond
      ; system is true and is a string or pathname so use it as a prefix
      ((or (stringp system) (pathnamep system))
        (merge-pathnames (make-pathname :directory '(:relative "share"))
                         (uiop:ensure-directory-pathname system)))
      ; system is true so use %PROGAMDATA% on Windows
      ((and system (uiop:os-windows-p))
        (uiop:getenv-absolute-directory "PROGRAMDATA"))
      ; system is true so assume the root is /usr/local/share/
      (system
        (make-pathname :directory '(:absolute "usr" "local" "share")))
      ; Use ~/Library/ on MacOS
      ((uiop:os-macosx-p)
        (merge-pathnames (make-pathname :directory '(:relative "Library"))
                         (uiop:getenv-pathname "HOME" :ensure-directory t)))
      ; Use %APPDATA% on Windows
      ((uiop:os-windows-p)
        (uiop:get-folder-path :appdata))
      ; Use XDG_DATA_HOME on all other platforms
      (t
        (uiop:xdg-data-home)))))

(defun install-kernel (&key argv class display-name kernel-name language resources system)
  "Install a kernel spec file given a kernel name and a language name."
  (let* ((kernel-home (kernel-home :kernel-name kernel-name :system system))
         (kernel-path (merge-pathnames (make-pathname :name "kernel" :type "json")
                                       kernel-home))
         (image-path (merge-pathnames (make-pathname :name "image" :type (when (uiop:os-windows-p) "exe"))
                                      kernel-home)))
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
    (for dest next (merge-pathnames kernel-home src))
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
