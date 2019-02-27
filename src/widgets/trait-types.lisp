(in-package #:jupyter-widgets)

(defparameter +widget-prefix+ "IPY_MODEL_")

; Bool

(defmethod serialize-trait (object name (type (eql :bool)) (value (eql nil)))
  :false)

; Date

(defmethod serialize-trait (object name (type (eql :date)) value)
  (if value
    (jsown:new-js
      ("year" (parse-integer value :start 0 :end 4))
      ("month" (1- (parse-integer value :start 5 :end 7)))
      ("date" (parse-integer value :start 8 :end 10)))
    :null))

(defmethod deserialize-trait (object name (type (eql :date)) value)
  (when (cdr value)
    (format nil "~4,'0D-~2,'0D-~2,'0D"
                (jsown:val value "year")
                (1+ (jsown:val value "month"))
                (jsown:val value "date"))))

; Integer

(defmethod serialize-trait (object name (type (eql :int)) (value (eql nil)))
  :null)

; Float

(defmethod deserialize-trait (object name (type (eql :float)) value)
  (coerce value 'double-float))

; Float List

(defmethod deserialize-trait (object name (type (eql :float-list)) value)
  (mapcar (lambda (x) (coerce x 'double-float)) value))

; Link

(defmethod serialize-trait (object name (type (eql :link)) value)
  (when value
    (list (serialize-trait object name :widget (first value))
          (serialize-trait object name :unicode (second value)))))

(defmethod deserialize-trait (object name (type (eql :link)) value)
  (when value
    (list (deserialize-trait object name :widget (first value))
          (deserialize-trait object name :unicode (second value)))))

; Unicode

(defmethod serialize-trait (object name (type (eql :unicode)) (value (eql nil)))
  :null)

; Widget

(defmethod serialize-trait (object name (type (eql :widget)) (value (eql nil)))
  :null)

(defmethod serialize-trait (object name type (value widget))
  (concatenate 'string +widget-prefix+ (jupyter:comm-id value)))

(defmethod deserialize-trait (object name (type (eql :widget)) value)
  (jupyter:get-comm (subseq value (length +widget-prefix+))))

; Widget List

(defmethod serialize-trait (object name (type (eql :widget-list)) value)
  (mapcar (lambda (v) (serialize-trait object name :widget v)) value))

(defmethod deserialize-trait (object name (type (eql :widget-list)) value)
  (mapcar (lambda (v) (deserialize-trait object name :widget v)) value))
