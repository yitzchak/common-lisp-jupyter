(in-package #:jupyter/convert)

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


(defclass my-client (eclector.parse-result:parse-result-client)
  ())


(defmethod eclector.parse-result:make-expression-result ((client my-client) result children source)
  (declare (ignore result children))
  source)


(defmethod eclector.parse-result:make-skipped-input-result ((client my-client) stream reason children source)
  (declare (ignore stream reason children))
  source)


(defun to-notebook (src dest)
  "Convert Lisp source to Jupyter notebook"
  (with-open-file (dest-stream dest :direction :output :if-exists :supersede)
    (shasht:write-json
      (list :object-alist
        (cons "nbformat" 4)
        (cons "nbformat_minor" 2)
        (cons "metadata"
          (list :object-alist
            (cons "kernelspec"
              (list :object-alist
                (cons "display_name" "Common Lisp")
                (cons "language" "common-lisp")
                (cons "name" "common-lisp")))
            (cons "language_info"
              (list :object-alist
                (cons "codemirror_mode" "text/x-common-lisp")
                (cons "file_extension" ".lisp")
                (cons "mimetype" "text/x-common-lisp")
                (cons "name" "common-lisp")
                (cons "pygments_lexer" "common-lisp")
                (cons "version" "1.4.8")))))
        (cons "cells"
          (prog* (cells
                  (client (make-instance 'my-client))
                  (contents (alexandria:read-file-into-string src))
                  (stream (make-string-input-stream contents)))
           next
            (multiple-value-bind (result skipped)
                                 (eclector.parse-result:read client stream nil :eof)
              (unless (and (eq :eof result)
                           (null skipped))
                (push `(:object-plist
                         "cell_type" "code"
                         "execution_count" :null
                         "metadata" :empty-object
                         "outputs" :empty-array
                         "source" (,(let ((source (reduce (lambda (previous current)
                                                            (cons (min (car previous) (car current))
                                                                  (max (cdr previous) (cdr current))))
                                                          (cons result skipped))))
                                      (subseq contents (car source) (cdr source)))))
                      cells))
              (when (eq :eof result)
                (return (nreverse cells)))
              (go next)))))
      dest-stream))
  (values))
