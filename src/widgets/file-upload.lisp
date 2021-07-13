(in-package #:jupyter/widgets)


(defwidget file-upload (description-widget button-style-slot disabled-slot icon-slot)
  ((accept
     :initarg :accept
     :initform ""
     :accessor widget-accept
     :documentation "If set, ensure value is in options. Implies continuous_update=False.	File types to accept, empty string for all."
     :trait :string)
   (data
     :initarg :data
     :initform nil
     :accessor widget-data
     :documentation "List of file content (bytes)"
     :trait :buffer-list)
   (error
     :initarg :error
     :initform ""
     :accessor widget-error
     :documentation "Error message"
     :trait :string)
   (metadata
     :initarg :metadata
     :initform nil
     :accessor widget-metadata
     :documentation "List of file metadata"
     :trait :json)
   (multiple
     :initarg :multiple
     :initform nil
     :accessor widget-multiple
     :documentation "If True, allow for multiple files upload"
     :trait :bool))
  (:default-initargs
    :%model-name "FileUploadModel"
    :%view-name "FileUploadView"))




(defmethod widget-value ((instance file-upload))
  (map 'vector
       (lambda (content metadata)
         (let ((table (alexandria:copy-hash-table metadata)))
           (setf (gethash "content" table) content)
           table))
       (widget-data instance) (widget-metadata instance)))


(defun file-upload-value-notify (instance)
  (when (and (slot-boundp instance 'data)
             (slot-boundp instance 'metadata)
             (= (length (widget-data instance)) (length (widget-metadata instance)))
             (every (lambda (content metadata)
                      (= (length content) (gethash "size" metadata)))
                    (widget-data instance) (widget-metadata instance)))
    (jupyter::enqueue *trait-notifications*
                      (list instance :any :value nil (widget-value instance) nil))))


(defmethod on-trait-change :after ((instance file-upload) type (name (eql :data)) old-value new-value source)
  (declare (ignore type name old-value new-value source))
  (file-upload-value-notify instance))


(defmethod on-trait-change :after ((instance file-upload) type (name (eql :metadata)) old-value new-value source)
  (declare (ignore type name old-value new-value source))
  (file-upload-value-notify instance))

