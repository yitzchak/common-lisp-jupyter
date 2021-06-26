(in-package #:jupyter-widgets)


(defwidget base-select (description-widget %options-labels-slot disabled-slot)
  ((rows
     :initarg :rows
     :initform 5
     :accessor widget-rows
     :documentation "The number of rows to display."
     :trait :int)))


(defwidget select (base-select index-slot)
  ((options
    :accessor widget-options
    :initarg :options
    :documentation "The option values that correspond to the labels"))
  (:default-initargs
    :%model-name "SelectModel"
    :%view-name "SelectView")
  (:documentation
    "Listbox that only allows one item to be selected at any given time."))



; Simulate value property below

(defun select-value (instance index)
  (when index
    (nth index
         (if (slot-boundp instance 'options)
           (widget-options instance)
           (widget-%options-labels instance)))))

(defun select-values (instance indices)
  (mapcar (lambda (index)
            (select-value instance index))
          indices))

(defmethod widget-value ((instance select))
  (select-value instance (widget-index instance)))

(defmethod (setf widget-value) (new-value (instance select))
  (setf (widget-index instance)
        (position new-value
                  (if (slot-boundp instance 'options)
                    (widget-options instance)
                    (widget-%options-labels instance))
                  :test #'equal)))

(defmethod on-trait-change :after ((instance select) type (name (eql :index)) old-value new-value source)
  (jupyter::enqueue *trait-notifications*
    (list instance :any :value (select-value instance old-value) (select-value instance new-value) source)))


(defmethod initialize-instance :after ((instance select) &rest initargs &key &allow-other-keys)
  (let ((value (getf initargs :value)))
    (when value
      (setf (widget-value instance) value))))


(defwidget select-multiple (base-select)
  ((index
     :initarg :index
     :initform nil
     :accessor widget-index
     :documentation "Selected indicies"
     :trait :int-list)
   (options
    :accessor widget-options
    :initarg :options
    :documentation "The option values that correspond to the labels"))
  (:default-initargs
    :%model-name "SelectMultipleModel"
    :%view-name "SelectMultipleView")
  (:documentation
    "Listbox that allows many items to be selected at any given time."))



(defmethod widget-value ((instance select-multiple))
  (select-values instance (widget-index instance)))

(defmethod (setf widget-value) (new-value (instance select-multiple))
  (setf (widget-index instance)
        (mapcar (lambda (value)
                  (position value
                            (if (slot-boundp instance 'options)
                              (widget-options instance)
                              (widget-%options-labels instance))
                            :test #'equal))
                new-value)))

(defmethod on-trait-change :after ((instance select-multiple) type (name (eql :index)) old-value new-value source)
  (jupyter::enqueue *trait-notifications*
    (list instance :any :value (select-values instance old-value) (select-values instance new-value) source)))


(defmethod initialize-instance :after ((instance select-multiple) &rest initargs &key &allow-other-keys)
  (let ((value (getf initargs :value)))
    (when value
      (setf (widget-value instance) value))))


(defwidget radio-buttons (select)
  ()
  (:default-initargs
    :%model-name "RadioButtonsModel"
    :%view-name "RadioButtonsView")
  (:documentation
"Group of radio buttons that represent an enumeration. Only one radio button can
be toggled at any point in time."))




(defwidget dropdown (select)
  ()
  (:default-initargs
    :%model-name "DropdownModel"
    :%view-name "DropdownView")
  (:documentation "Allows you to select a single item from a dropdown."))



