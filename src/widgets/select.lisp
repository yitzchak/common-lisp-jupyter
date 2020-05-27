(in-package #:jupyter-widgets)


(defclass base-select (description-widget %options-labels-slot disabled-slot)
  ((rows
     :initarg :rows
     :initform 5
     :accessor widget-rows
     :documentation "The number of rows to display."
     :trait :int))
  (:metaclass trait-metaclass))


(defclass select (base-select index-slot)
  ((options
    :accessor widget-options
    :initarg :options
    :documentation "The option values that correspond to the labels"))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SelectModel"
    :%view-name "SelectView")
  (:documentation
    "Listbox that only allows one item to be selected at any given time."))

(register-widget select)

; Simulate value property below

(defun select-value (instance index)
  (when index
    (nth index
         (if (slot-boundp instance 'options)
           (widget-options instance)
           (widget-%options-labels instance)))))

(defmethod widget-value ((instance select))
  (select-value instance (widget-index instance)))

(defmethod (setf widget-value) (new-value (instance select))
  (setf (widget-index)
        (position new-value
                  (if (slot-boundp instance 'options)
                    (widget-options instance)
                    (widget-%options-labels instance))
                  :test #'equal)))

(defmethod on-trait-change :after ((instance select) type (name (eql :index)) old-value new-value source)
  (jupyter::enqueue *trait-notifications*
    (list instance :any :value (select-value instance old-value) (select-value instance new-value) source)))


(defclass select-multiple (base-select)
  ((index
     :initarg :index
     :initform nil
     :accessor widget-index
     :documentation "Selected indicies"
     :trait :int-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SelectMultipleModel"
    :%view-name "SelectMultipleView")
  (:documentation
    "Listbox that allows many items to be selected at any given time."))

(register-widget select-multiple)


(defclass radio-buttons (select)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "RadioButtonsModel"
    :%view-name "RadioButtonsView")
  (:documentation
"Group of radio buttons that represent an enumeration. Only one radio button can
be toggled at any point in time."))

(register-widget radio-buttons)


(defclass dropdown (select)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "DropdownModel"
    :%view-name "DropdownView")
  (:documentation "Allows you to select a single item from a dropdown."))

(register-widget dropdown)

