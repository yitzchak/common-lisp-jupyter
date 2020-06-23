(in-package #:jupyter-convert)

(defclass cell ()
  ((markdown
     :initarg :markdown
     :initform nil
     :accessor cell-markdown)
   (source
     :initarg :source
     :initform ""
     :accessor cell-source)))

; (defmethod shasht:write-json ((c cell) &optional output-stream)
;   (with-slots (markdown source) c
;     (if markdown
;       (shasht:with-object output-stream
;         ("cell_type" "markdown")
;         ("source" (list source))
;         ("metadata" (jupyter:json-empty-obj)))
;       (shasht:with-object output-stream
;         ("cell_type" "code")
;         ("source" (list source))
;         ("execution_count" :null)
;         ("outputs" nil)
;         ("metadata" (jupyter:json-new-obj ("collapsed" t)))))))

(defun my-read (value)
  (handler-case
    (read-from-string value nil :no-forms)
    (end-of-file () :end-of-file)))

(defun to-notebook (src dest)
  "Convert Lisp source to Jupyter notebook"
  (with-open-file (dest-stream dest :direction :output :if-exists :supersede)
    (shasht:write-json
      (list :object
        (cons "nbformat" 4)
        (cons "nbformat_minor" 2)
        (cons "metadata"
          (list :object
            (cons "kernelspec"
              (list :object
                (cons "display_name" "Common Lisp")
                (cons "language" "common-lisp")
                (cons "name" "common-lisp")))
            (cons "language_info"
              (list :object
                (cons "codemirror_mode" "text/x-common-lisp")
                (cons "file_extension" ".lisp")
                (cons "mimetype" "text/x-common-lisp")
                (cons "name" "common-lisp")
                (cons "pygments_lexer" "common-lisp")
                (cons "version" "1.4.8")))))
        (cons "cells"
          (iter
            (with cell = nil)
            (for line in-file src using #'read-line)
            (when (and (not cell) (zerop (length (string-trim '(#\Space #\Tab #\Newline) line))))
              (next-iteration))
            (if cell
              (setf (cell-source cell)
                (concatenate 'string
                  (cell-source cell)
                  (coerce '(#\Newline) 'string)
                  line))
              (setq cell (make-instance 'cell :source line)))
            (case (my-read (cell-source cell))
              (:end-of-file)
              (:no-forms
                (with-slots (markdown source) cell
                  (setq markdown t)
                  (let ((trimmed-source (string-trim '(#\Space #\Tab #\Newline) source)))
                    (setq source
                      (format nil "```~%~A~%```"
                        (string-trim '(#\Space #\Tab #\Newline)
                          (if (starts-with-subseq "#|" trimmed-source)
                            (subseq trimmed-source 2 (- (length trimmed-source) 4))
                            (string-left-trim '(#\;) trimmed-source)))))))
                (collect cell into cells)
                (setq cell nil))
              (otherwise
                (collect cell into cells)
                (setq cell nil)))
            (finally
              (return
                (if cell
                  (nconc cells (list cell))
                  cells))))))
      dest-stream))
  t)
