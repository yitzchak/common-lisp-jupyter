
(in-package #:fishbowl-user)

(defclass fishbowl-quit-obj ()
  ()
  (:documentation "A quit object for identifying a request for kernel shutdown."))

(defun quit ()
  (make-instance 'fishbowl-quit-obj))
  
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


(defclass svg-str ()
  ((str :initarg :str :reader svg-str)))

(defmethod render-svg ((img svg-str))
  (svg-str img))

(defun svg-from-file (filename)
  (let ((str (read-string-file filename)))
    (display-svg (make-instance 'svg-str :str str))))


