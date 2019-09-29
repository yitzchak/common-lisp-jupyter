(in-package #:jupyter-widgets)

(defclass box (dom-widget)
  ((box-style
     :initarg :box-style
     :initform ""
     :accessor widget-box-style
     :documentation "Use a predefined styling for the box."
     :trait :unicode)
   (children
     :initarg :children
     :accessor widget-children
     :documentation "List of widget children."
     :trait :widget-list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "BoxModel"
    :%view-name "BoxView")
  (:documentation
"Displays multiple widgets in a group. The widgets are laid out horizontally.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar title-widget (make-widget 'html :value \"<em>Box Example</em>\"))
(defvar slider (make-widget 'int-slider))
(make-widget 'box :children (list title-widget slider))
```"))

(register-widget box)


(defclass accordion (box)
  ((%titles
     :initarg :%titles
     :initform ""
     :accessor widget-%titles
     :documentation "Titles of the pages."
     :trait :unicode-list)
   (selected-index
     :initarg :selected-index
     :accessor widget-selected-index
     :documentation "The index of the selected page. This is either an integer selecting a particular sub-widget, or nil to have no widgets selected."
     :trait :int))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "AccordionModel"
    :%view-name "AccordionView")
  (:documentation "Displays children each on a separate accordion page."))

(register-widget accordion)

(defmethod validate-trait ((w accordion) (type (eql :int)) name value)
  (if (and (integerp value)
           (equal name 'selected-index)
           (outside-left-closed-interval value
             0
             (if (slot-boundp w 'children)
               (length (widget-children w))
               0)))
    (error 'trait-error :format-control "Invalid selection: selected-index out of bounds")
    (call-next-method)))


(defclass grid-box (box)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "GridBoxModel"
    :%view-name "GridBoxView"))

(register-widget grid-box)


(defclass h-box (box)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "HBoxModel"
    :%view-name "HBoxView")
  (:documentation
"Displays multiple widgets horizontally using the flexible box model.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar title-widget (make-widget 'html :value \"<em>Box Example</em>\"))
(defvar slider (make-widget 'int-slider))
(make-widget 'h-box :children (list title-widget slider))
```"))

(register-widget h-box)


(defclass tab (accordion)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TabModel"
    :%view-name "TabView")
  (:documentation "Displays children each on a separate accordion tab."))

(register-widget tab)


(defclass v-box (box)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "VBoxModel"
    :%view-name "VBoxView")
  (:documentation
"Displays multiple widgets vertically using the flexible box model.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar title-widget (make-widget 'html :value \"<em>Box Example</em>\"))
(defvar slider (make-widget 'int-slider))
(make-widget 'v-box :children (list title-widget slider))
```"))

(register-widget v-box)
