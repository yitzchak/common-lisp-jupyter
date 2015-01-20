
(in-package #:fishbowl-user)

(defclass fishbowl-quit-obj ()
  ()
  (:documentation "A quit object for identifying a request for kernel shutdown."))

(defun quit ()
  (make-instance 'fishbowl-quit-obj))
  

#|

  ## Basic Markdown support ##

|#

;; remark: this is not supported in IPython 2.x (will it be in 3 ?)
(defclass markdown-text ()
  ((text :initarg :text :reader markdown-text)))

(defmethod render-markdown ((doc markdown-text))
  (markdown-text doc))

(defun markdown (text)
  (display-markdown (make-instance 'markdown-text :text text)))


#|

  ## Basic Latex support ##

|#

(defclass latex-text ()
  ((text :initarg :text :reader latex-text)))

(defmethod render-latex ((doc latex-text))
  (with-output-to-string (str)
    (format str "~A" (latex-text doc))))

(defun latex (text)
  (display-latex (make-instance 'latex-text :text text)))

(example (fishbowl::display-object-data (latex "$\\frac{1}{2}$"))
         => '(("text/plain" . "#<LATEX-TEXT #x302001F9E74D>")
              ("text/latex" . "$\\frac{1}{2}$")) :warn-only t)



#|

  ## Basic PNG support ## 

|#

(defclass png-bytes ()
  ((bytes :initarg :bytes :reader png-bytes)))

(defmethod render-jpg ((img png-bytes))
  (cl-base64:usb8-array-to-base64-string (png-bytes img)))

(defun png-from-file (filename)
  (let ((bytes (read-binary-file filename)))
    (display-png (make-instance 'png-bytes :bytes bytes))))

#|

 ## Basic SVG support ##

|#


(defclass svg-str ()
  ((str :initarg :str :reader svg-str)))

(defmethod render-svg ((img svg-str))
  (svg-str img))

(defun svg-from-file (filename)
  (let ((str (read-string-file filename)))
    (display-svg (make-instance 'svg-str :str str))))


