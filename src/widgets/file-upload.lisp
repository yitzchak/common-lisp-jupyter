(in-package #:jupyter/widgets)


(defwidget file-upload (description-widget button-style-slot disabled-slot icon-slot)
  ((accept :initarg :accept
	   :initform ""
	   :accessor widget-accept
	   :documentation "If set, ensure value is in options. Implies continuous_update=False.	File types to accept, empty string for all."
	   :trait :string)
   (value :initarg :value
	  :initform nil
	  :accessor widget-value
	  :documentation "The file upload value"
	  :trait :buffer-list)
   (error :initarg :error
	  :initform ""
	  :accessor widget-error
	  :documentation "Error message"
	  :trait :string)
   (multiple :initarg :multiple
	     :initform nil
	     :accessor widget-multiple
	     :documentation "If True, allow for multiple files upload"
	     :trait :bool))
  (:default-initargs :%model-name "FileUploadModel"
		     :%view-name "FileUploadView"))

