(in-package #:jupyter-widgets)

(defparameter +widget-prefix+ "IPY_MODEL_")

; Bool

(defmethod serialize-trait (object name (type (eql :boolean)) (value (eql nil)))
  :false)

; Integer

(defmethod serialize-trait (object name (type (eql :integer)) (value (eql nil)))
  :null)

; Float

(defmethod deserialize-trait (object name (type (eql :float)) value)
  (coerce value 'double-float))

; Float List

(defmethod deserialize-trait (object name (type (eql :float-list)) value)
  (mapcar (lambda (x) (coerce x 'double-float)) value))

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
