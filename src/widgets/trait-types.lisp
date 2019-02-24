(in-package #:jupyter-widgets)

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

(defmethod serialize-trait (object type name (value widget))
  (format nil "IPY_MODEL_~A" (jupyter:comm-id value)))
