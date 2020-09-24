(in-package #:jupyter-widgets)


; bounded int - int-slider, bounded-int-text
; bounded float - float-slider, bounded-float-text
; bounded log float - float-log-slider
; int interval - int-range-slider
; float interval - float-range-slider
; int - int-text
; float - float-text
; boolean - toggle-button, checkbox
; option - dropdown, radio-buttons, select, selection-slider
; option interval - selection-range-slider
; option set - toggle-buttons, select-multiple
; text
; text-area
; date date-picker
; color color-picker
; file file-upload

; storage
; - class
; - plist with separate desc
; - alist with separate desc
; - hashtable with separate desc
; - function


(defun make-interactive-widget (schema)
  (ecase (getf schema :type)
    (:bool
      (make-instance (ecase (getf schema :style :check)
                       (:toggle
                         'toggle-button)
                       (:check
                         'checkbox))
                     :description (getf schema :description "")
                     :layout (make-instance 'layout
                                            :align-item "center"
                                            :display "block"
                                            :justify-item "start")
                     :value (getf schema :default nil)))
    (:float
      (make-instance (ecase (getf schema :style :text)
                       (:log-slider
                         'float-log-slider)
                       (:slider
                         'float-slider)
                       (:bounded-text
                         'bounded-float-text)
                       (:text
                         'float-text))
                     :description (getf schema :description "")
                     :step (getf schema :step 0.1d0)
                     :base (getf schema :base 10d0)
                     :min (getf schema :min 0d0)
                     :max (getf schema :max 100d0)
                     :value (getf schema :default 0d0)))
    (:int
      (make-instance (ecase (getf schema :style :text)
                       (:slider
                         'int-slider)
                       (:bounded-text
                         'bounded-int-text)
                       (:text
                         'int-text))
                     :description (getf schema :description "")
                     :step (getf schema :step 1)
                     :min (getf schema :min 0)
                     :max (getf schema :max 100)
                     :value (getf schema :default 0)))
    (:string
      (make-instance (ecase (getf schema :style :text)
                       (:area
                         'text-area)
                       (:combo
                         'combobox)
                       (:text
                         'text))
                     :description (getf schema :description "")
                     :options (getf schema :options)
                     :value (getf schema :default "")))))


(defmacro make-observer (place indicator)
  (let ((indicator-var (gensym)))
           `(let ((,indicator-var ,indicator))
            (lambda (instance type name old-value new-value source)
     (declare (ignore instance type name old-value source))
     (setf (getf ,place ,indicator-var) new-value)))))


(defmacro make-interactive-widgets (place schemas)
  (let ((schema-var (gensym))
        (widget-var (gensym))
        (value-var (gensym))
        (indicator-var (gensym))
        (indicator-found-var (gensym))
        (tail-var (gensym)))
    `(make-instance 'grid-box
                    :layout (make-instance 'layout
                                           :grid-template-columns "min-content 1fr"
                                           :grid-auto-rows "min-content"
                                           :grid-gap ".25em")
                    :children (loop for ,schema-var in ,schemas
                                    for ,widget-var = (make-interactive-widget ,schema-var)
                                    for ,indicator-var = (getf ,schema-var :indicator)
                                    for (,indicator-found-var ,value-var ,tail-var) =
                                        (multiple-value-list (get-properties ,place (list ,indicator-var)))
                                    when ,indicator-found-var
                                      do (setf (widget-value ,widget-var) ,value-var)
                                    do (observe ,widget-var :value
                                         (make-observer ,place ,indicator-var))
                                    collect (make-instance 'label :value (getf ,schema-var :label))
                                    collect ,widget-var))))

