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
    :%view-name "BoxView"))

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
    :%view-name "AccordionView"))

(register-widget accordion)


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
    :%view-name "HBoxView"))

(register-widget h-box)


(defclass tab (accordion)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "TabModel"
    :%view-name "TabView"))

(register-widget tab)


(defclass v-box (box)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "VBoxModel"
    :%view-name "VBoxView"))

(register-widget v-box)
