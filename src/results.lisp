(in-package #:jupyter)

#|

Standard MIME types

|#

(defvar *gif-mime-type* "image/gif")
(defvar *html-mime-type* "text/html")
(defvar *javascript-mime-type* "application/javascript")
(defvar *jpeg-mime-type* "image/jpeg")
(defvar *json-mime-type* "application/json")
(defvar *latex-mime-type* "text/latex")
(defvar *lisp-mime-type* "text/x-common-lisp")
(defvar *markdown-mime-type* "text/markdown")
(defvar *pdf-mime-type* "application/pdf")
(defvar *plain-text-mime-type* "text/plain")
(defvar *png-mime-type* "image/png")
(defvar *ps-mime-type* "application/postscript")
(defvar *svg-mime-type* "image/svg+xml")


(defun sexpr-to-text (value)
  (string-trim '(#\Newline)
    (with-output-to-string (s)
      (pprint value s))))

(defgeneric render (result)
  (:documentation "Render evaluation result as a mime bundle for execute_result
  or display_data."))

(defmethod render (res))

(defclass result ()
  ((display-data :initarg :display-data
                 :initform nil
                 :accessor result-display-data
                 :documentation "Show result as display_data in client."))
  (:documentation "Base class for encapsulation of evaluation result."))

(defclass sexpr-result (result)
  ((value :initarg :value
          :reader sexpr-result-value)))

(defmethod render ((res sexpr-result))
  (list :object
        (cons *plain-text-mime-type* (sexpr-to-text (sexpr-result-value res)))))

(defclass inline-result (result)
  ((value :initarg :value
          :reader inline-result-value)
   (mime-type :initarg :mime-type
              :reader inline-result-mime-type)))

(defun make-inline-result (value &key (mime-type *plain-text-mime-type*) (display-data nil) (handle nil))
  "Make a result based on an inline value. The handle argument is used by the
  convenience functions to instantly process the result."
  (let ((result (make-instance 'inline-result :value value
                                              :mime-type mime-type
                                              :display-data display-data)))
    (if (and handle display-data)
      (progn
        (send-result result)
        t)
      result)))

(defmethod render ((res inline-result))
  (let ((value (inline-result-value res))
        (mime-type (inline-result-mime-type res)))
    (cond
      ((equal mime-type *plain-text-mime-type*)
        (list :object
              (cons mime-type value)))
      ((equal mime-type *markdown-mime-type*)
        (list :object
              (cons *plain-text-mime-type* value)
              (cons mime-type value)))
      (t
        (list :object
              (cons *plain-text-mime-type* "inline-value")
              (cons mime-type (if (or (stringp value) (ends-with-subseq "json" mime-type))
                                value
                                (cl-base64:usb8-array-to-base64-string value))))))))

(defclass file-result (result)
  ((path :initarg :path
         :reader file-result-path)
   (mime-type :initarg :mime-type
              :initform nil
              :reader file-result-mime-type)))

(defun make-file-result (path &key (mime-type nil) (display-data nil) (handle nil))
  "Make a result based on a file. The handle argument is used by the convenience
  functions to instantly process the result."
  (let ((result (make-instance 'file-result :path path
                                            :mime-type mime-type
                                            :display-data display-data)))
    (if (and handle display-data)
      (progn
        (send-result result)
        t)
      result)))

(defmethod render ((res file-result))
  (let* ((path (file-result-path res))
         (mime-type (or (file-result-mime-type res) (trivial-mimes:mime path))))
    (if (equal mime-type *plain-text-mime-type*)
      (list :object
            (cons mime-type (read-file-into-string path)))
      (list :object
            (cons *plain-text-mime-type* path)
            (cons mime-type
                  (if (or (equal mime-type *svg-mime-type*)
                          (starts-with-subseq "text/" mime-type))
                    (read-file-into-string path)
                    (cl-base64:usb8-array-to-base64-string
                      (read-file-into-byte-vector path))))))))

(defclass error-result (result)
  ((ename :initarg :ename
          :reader error-result-ename)
   (evalue :initarg :evalue
           :reader error-result-evalue)
   (quit :initarg :quit
         :initform nil
         :reader error-result-quit)
   (traceback :initarg :traceback
              :initform nil
              :reader error-result-traceback)))

(defun make-error-result (ename evalue &key (quit nil) (traceback nil))
  "Make a result based on an error. The quit the parameter indicates that the
  kernel should exit. The handle argument is used by the convenience functions
  to instantly process the result."
  (make-instance 'error-result :ename ename
                               :evalue evalue
                               :quit quit
                               :traceback traceback))

(defun make-lisp-result (value &key (display-data nil))
  "Make a lisp result based on an inline value."
  (cond ((typep value 'result)
         value)
        ((not (eq :no-output value))
         (make-instance 'sexpr-result :value value :display-data display-data))))

(defun eval-error-p (result)
  "Predicate to determine if result is an error result."
  (typep result 'error-result))

(defun quit-eval-error-p (result)
  "Predicate to determine if result is an quit result."
  (and (typep result 'error-result) (error-result-quit result)))

; Convienence functions

(defun file (path &optional (display-data nil))
  "Create a result based on a file path. The mime type with automatically be
  determined from the file extension."
  (make-file-result path :display-data display-data :handle t))

(defun gif-file (path &optional (display-data nil))
  "Create a GIF image result based on a file path."
  (make-file-result path
                    :display-data display-data :handle t
                    :mime-type *gif-mime-type*))

(defun jpeg-file (path &optional (display-data nil))
  "Create a JPEG image result based on a file path."
  (make-file-result path
                    :display-data display-data :handle t
                    :mime-type *jpeg-mime-type*))

(defun pdf-file (path &optional (display-data nil))
  "Create a PDF result based on a file path."
  (make-file-result path
                    :display-data display-data :handle t
                    :mime-type *pdf-mime-type*))

(defun png-file (path &optional (display-data nil))
  "Create a PNG image result based on a file path."
  (make-file-result path
                    :display-data display-data :handle t
                    :mime-type *png-mime-type*))

(defun ps-file (path &optional (display-data nil))
  "Create a PostScript result based on a file path."
  (make-file-result path
                    :display-data display-data :handle t
                    :mime-type *ps-mime-type*))

(defun svg-file (path &optional (display-data nil))
  "Create a SVG result based on a file path."
  (make-file-result path
                    :display-data display-data :handle t
                    :mime-type *svg-mime-type*))

(defun inline-result (value mime-type &optional (display-data nil))
  "Create a result based on an inline value."
  (make-inline-result value
                      :mime-type mime-type
                      :display-data display-data
                      :handle t))

(defun text (value &optional (display-data nil))
  "Create a plain text result based on an inline value."
  (make-inline-result value
                      :display-data display-data
                      :handle t))

(defun html (value &optional (display-data nil))
  "Create a HTML result based on an inline value."
  (make-inline-result value
                      :mime-type *html-mime-type*
                      :display-data display-data
                      :handle t))

(defun javascript (value &optional (display-data nil))
  "Create a JavaScript text result based on an inline value."
  (make-inline-result value
                      :mime-type *javascript-mime-type*
                      :display-data display-data
                      :handle t))

(defun jpeg (value &optional (display-data nil))
  "Create a JPEG image result based on an inline value."
  (make-inline-result value
                      :mime-type *jpeg-mime-type*
                      :display-data display-data
                      :handle t))

(defun latex (value &optional (display-data nil))
  "Create a LaTeX result based on an inline value."
  (make-inline-result value
                      :mime-type *latex-mime-type*
                      :display-data display-data
                      :handle t))

(defun markdown (value &optional (display-data nil))
  "Create a Markdown result based on an inline value."
  (make-inline-result value
                      :mime-type *markdown-mime-type*
                      :display-data display-data
                      :handle t))

(defun png (value &optional (display-data nil))
  "Create a PNG image result based on an inline value."
  (make-inline-result value
                      :mime-type *png-mime-type*
                      :display-data display-data
                      :handle t))

(defun svg (value &optional (display-data nil))
  "Create a SVG result based on an inline value."
  (make-inline-result value
                      :mime-type *svg-mime-type*
                      :display-data display-data
                      :handle t))

#|

Jupyter clients generally don't know about the myriad of mime types associated
with TeX/LaTeX and assume that the proper mime type is always text/latex. The
following function will make sure that trivial-mimes database reflects this.

|#

(defun check-mime-db ()
  (dolist (ext '("tex" "latex" "tikz"))
    (setf (gethash ext trivial-mimes:*mime-db*) *latex-mime-type*)))

(check-mime-db)
