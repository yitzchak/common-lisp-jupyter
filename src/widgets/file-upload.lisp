(in-package #:jupyter-widgets)


(defclass file-upload (description-widget button-style-slot disabled-slot icon-slot)
  ((accept
     :initarg :accept
     :initform ""
     :accessor widget-accept
     :documentation "If set, ensure value is in options. Implies continuous_update=False.	File types to accept, empty string for all."
     :trait :unicode)
   (data
     :initarg :data
     :initform nil
     :accessor widget-data
     :documentation "List of file content (bytes)"
     :trait :byte-list)
   (error
     :initarg :error
     :initform ""
     :accessor widget-error
     :documentation "Error message"
     :trait :unicode)
   (metadata
     :initarg :metadata
     :initform nil
     :accessor widget-metadata
     :documentation "List of file metadata"
     :trait :object-list)
   (multiple
     :initarg :multiple
     :initform nil
     :accessor widget-multiple
     :documentation "If True, allow for multiple files upload"
     :trait :bool))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "FileUploadModel"
    :%view-name "FileUploadView"))

(register-widget file-upload)

