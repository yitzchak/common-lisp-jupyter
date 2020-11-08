(in-package #:jupyter-widgets)


; bounded int - int-slider, bounded-int-text
; bounded float - float-slider, bounded-float-text
; bounded log float - float-log-slider
; int range - int-range-slider
; float interval - float-range-slider
; int - int-text
; float - float-text
; boolean - toggle-button, checkbox
; option - dropdown, radio-buttons, select, selection-slider, toggle-buttons
; option range - selection-range-slider
; option set - select-multiple
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


(defun make-interactive-widget (schema value observer)
  (let ((control (ecase (getf schema :type)
                   (:bool
                     (make-instance (ecase (getf schema :style :check)
                                      (:toggle
                                        'toggle-button)
                                      (:check
                                        'checkbox))
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :layout (make-instance 'layout
                                                           :align-item "center"
                                                           :display "block"
                                                           :justify-item "start")
                                    :value (or value
                                               (getf schema :default nil))))
                   (:color
                     (make-instance 'color-picker
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :options (getf schema :options)
                                    :value (or value
                                               (getf schema :default "black"))))
                   (:date
                     (make-instance 'date-picker
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :options (getf schema :options)
                                    :value (or value
                                               (getf schema :default))))
                   (:file
                     (make-instance 'file-upload
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :accept (getf schema :accept)))
                   (:file-multiple
                     (make-instance 'file-upload
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :multiple t
                                    :accept (getf schema :accept)))
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
                                    :on-trait-change (list (cons :value observer))
                                    :step (getf schema :step 0.1d0)
                                    :base (getf schema :base 10d0)
                                    :min (getf schema :min 0d0)
                                    :max (getf schema :max 100d0)
                                    :value (or value
                                               (getf schema :default 0d0))))
                   (:float-range
                     (make-instance 'float-range-slider
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :step (getf schema :step 0.1d0)
                                    :min (getf schema :min 0d0)
                                    :max (getf schema :max 100d0)
                                    :value (or value
                                               (getf schema :default (list 0d0 1d0)))))
                   (:int
                     (make-instance (ecase (getf schema :style :text)
                                      (:slider
                                        'int-slider)
                                      (:bounded-text
                                        'bounded-int-text)
                                      (:text
                                        'int-text))
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :step (getf schema :step 1)
                                    :min (getf schema :min 0)
                                    :max (getf schema :max 100)
                                    :value (or value
                                               (getf schema :default 0))))
                   (:int-range
                     (make-instance 'int-range-slider
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :step (getf schema :step 1)
                                    :min (getf schema :min 0)
                                    :max (getf schema :max 100)
                                    :value (or value
                                               (getf schema :default (list 0 10)))))
                   (:option
                     (make-instance (ecase (getf schema :style :select)
                                      (:dropdown
                                        'dropdown)
                                      (:radio
                                        'radio-buttons)
                                      (:slider
                                        'selection-slider)
                                      (:select
                                        'select)
                                      (:toggle
                                        'toggle-buttons))
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :%options-labels (or (getf schema :labels)
                                                         (getf schema :options))
                                    :options (getf schema :options)
                                    :value (or value
                                               (getf schema :default nil))))
                   (:option-range
                     (make-instance 'selection-range-slider
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :%options-labels (or (getf schema :labels)
                                                         (getf schema :options))
                                    :options (getf schema :options)
                                    :value (or value
                                               (getf schema :default nil))))
                   (:option-multiple
                     (make-instance 'select-multiple
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :%options-labels (or (getf schema :labels)
                                                         (getf schema :options))
                                    :options (getf schema :options)
                                    :value (or value
                                               (getf schema :default nil))))
                   (:string
                     (make-instance (ecase (getf schema :style :text)
                                      (:area
                                        'text-area)
                                      (:combo
                                        'combobox)
                                      (:text
                                        'text))
                                    :description (getf schema :description "")
                                    :on-trait-change (list (cons :value observer))
                                    :options (getf schema :options)
                                    :value (or value
                                               (getf schema :default "")))))))
    (list (lambda (value)
            (setf (widget-value control) value))
          (make-instance 'label :value (getf schema :label))
          control)))


(defun make-interactive-widgets (schemas values observers)
  (let ((w (mapcar #'make-interactive-widget schemas values observers)))
    (values (make-instance 'grid-box
                           :layout (make-instance 'layout
                                                  :grid-template-columns "min-content 1fr"
                                                  :grid-auto-rows "min-content"
                                                  :grid-gap ".25em")
                           :children (mapcan #'cdr w))
            (mapcar #'car w))))


(defmacro make-plist-observer (place indicator)
  (let ((indicator-var (gensym)))
    `(let ((,indicator-var ,indicator))
       (lambda (instance type name old-value new-value source)
         (declare (ignore instance type name old-value source))
         (setf (getf ,place ,indicator-var) new-value)))))


(defmacro make-interactive-plist (schemas place &key owner name)
  (let ((schemas-var (gensym))
        (indicators-var (gensym))
        (owner-var (gensym))
        (controls-var (gensym))
        (setters-var (gensym))
        (name-var (gensym)))
    `(let* ((,schemas-var ,schemas)
            (,owner-var ,owner)
            (,name-var ,name)
            (,indicators-var (mapcar (lambda (schema)
                                       (getf schema :indicator))
                                     ,schemas-var)))
       (multiple-value-bind (,controls-var ,setters-var)
                            (make-interactive-widgets ,schemas-var
                                                      (mapcar (lambda (indicator)
                                                                (getf ,place indicator))
                                                              ,indicators-var)
                                                      (mapcar (lambda (indicator)
                                                                (make-plist-observer ,place indicator))
                                                              ,indicators-var))
          (when ,owner-var
            (observe ,owner-var ,name-var
              (lambda (owner type name old-value new-value source)
                (declare (ignore owner type name old-value source))
                (map nil (lambda (schema setter &aux (indicator (getf schema :indicator)))
                           (funcall setter (getf new-value indicator)))
                         ,schemas-var
                         ,setters-var))))
          ,controls-var))))


(defmacro make-alist-observer (place indicator key test)
  (let ((indicator-var (gensym)))
    `(let ((,indicator-var ,indicator))
       (lambda (instance type name old-value new-value source)
         (declare (ignore instance type name old-value source))
         (let ((pair (assoc ,indicator-var ,place :key ,key :test ,test)))
           (if pair
             (setf (cdr pair) new-value)
             (setf ,place (acons ,indicator-var new-value ,place))))))))


(defmacro make-interactive-alist (schemas place &key key test owner name)
  (let ((schemas-var (gensym))
        (indicators-var (gensym))
        (key-var (gensym))
        (owner-var (gensym))
        (controls-var (gensym))
        (setters-var (gensym))
        (name-var (gensym))
        (test-var (gensym)))
    `(let* ((,schemas-var ,schemas)
            (,owner-var ,owner)
            (,name-var ,name)
            (,key-var (or ,key #'identity))
            (,test-var (or ,test #'eql))
            (,indicators-var (mapcar (lambda (schema)
                                       (getf schema :indicator))
                                     ,schemas-var)))
       (multiple-value-bind (,controls-var ,setters-var)
                            (make-interactive-widgets ,schemas-var
                                                      (mapcar (lambda (indicator)
                                                                (cdr (assoc indicator ,place :key ,key-var :test ,test-var)))
                                                              ,indicators-var)
                                                      (mapcar (lambda (indicator)
                                                                (make-alist-observer ,place indicator ,key-var ,test-var))
                                                              ,indicators-var))
          (when ,owner-var
            (observe ,owner-var ,name-var
              (lambda (owner type name old-value new-value source)
                (declare (ignore owner type name old-value source))
                (map nil (lambda (schema setter &aux (indicator (getf schema :indicator)))
                           (funcall setter (cdr (assoc indicator new-value :key ,key-var :test ,test-var))))
                         ,schemas-var
                         ,setters-var))))
          ,controls-var))))


(defmacro make-hash-table-observer (place indicator)
  (let ((indicator-var (gensym)))
    `(let ((,indicator-var ,indicator))
       (lambda (instance type name old-value new-value source)
         (declare (ignore instance type name old-value source))
         (setf (gethash ,indicator-var ,place) new-value)))))


(defmacro make-interactive-hash-table (schemas place)
  (let ((schemas-var (gensym))
        (indicators-var (gensym))
        (owner-var (gensym))
        (controls-var (gensym))
        (setters-var (gensym))
        (name-var (gensym)))
    `(let* ((,schemas-var ,schemas)
            (,owner-var ,owner)
            (,name-var ,name)
            (,indicators-var (mapcar (lambda (schema)
                                       (getf schema :indicator))
                                     ,schemas-var)))
       (multiple-value-bind (,controls-var ,setters-var)
                            (make-interactive-widgets ,schemas-var
                                                      (mapcar (lambda (indicator)
                                                                (gethash indicator ,place))
                                                              ,indicators-var)
                                                      (mapcar (lambda (indicator)
                                                                (make-hash-table-observer ,place indicator))
                                                              ,indicators-var)))
          (when ,owner-var
            (observe ,owner-var ,name-var
              (lambda (owner type name old-value new-value source)
                (declare (ignore owner type name old-value source))
                (map nil (lambda (schema setter &aux (indicator (getf schema :indicator)))
                           (funcall setter (gethash indicator new-value)))
                         ,schemas-var
                         ,setters-var))))
          ,controls-var)))

