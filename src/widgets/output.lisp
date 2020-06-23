(in-package #:jupyter-widgets)

(defclass output (dom-widget)
  ((msg-id
     :initarg :msg-id
     :initform ""
     :accessor widget-msg-id
     :documentation "Parent message id of messages to capture"
     :trait :unicode)
   (outputs
     :initarg :outputs
     :initform nil
     :accessor widget-outputs
     :documentation "The output messages synced from the frontend."
     :trait :list))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "OutputModel"
    :%model-module +output-module+
    :%model-module-version +output-module-version+
    :%view-name "OutputView"
    :%view-module +output-module+
    :%view-module-version +output-module-version+)
  (:documentation
"Widget used as a context manager to display output.

This widget can capture and display stdout, stderr, and rich output.  To use it,
create an instance of it and display it.

You can then use the widget as a context manager: any output produced while in
the context will be captured and displayed in the widget instead of the standard
output area.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar out (make-instance 'output))
(with-output out
  (print \"prints to output area\")
```"))

(register-widget output)


; We should clean up after ourselves, but the messages are processed outside of this lexigraphic context.
(defmacro with-output (output &body body)
  "Evaluate body with all output sent to the output widget."
  `(with-slots (msg-id) ,output
     (unwind-protect 
       (progn
         (finish-output) 
         (finish-output *error-output*) 
         (setf msg-id (gethash "msg_id" (jupyter::message-header jupyter::*message*)))
         ,@body)
       (finish-output) 
       (finish-output *error-output*) 
       (setf msg-id "")))) 


(defclass sidecar (output)
  ((title
     :accessor widget-title
     :initarg :title
     :initform nil
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "SidecarModel"
    :%model-module +sidecar-module+
    :%model-module-version +sidecar-module-version+
    :%view-name "SidecarView"
    :%view-module +sidecar-module+
    :%view-module-version +sidecar-module-version+)
  (:documentation
"Widget used as a context manager to display output.

This widget can capture and display stdout, stderr, and rich output.  To use it,
create an instance of it and display it.

You can then use the widget as a context manager: any output produced while in
the context will be captured and displayed in the widget instead of the standard
output area.

### Example

```common-lisp
(use-package :jupyter-widgets)
(defvar out (make-instance 'output))
(with-output out
  (print \"prints to output area\")
```"))
  
