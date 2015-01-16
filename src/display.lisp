
(in-package #:fishbowl)

#|

# Rich display #

The IPython notebook implements a simple yet powerfull
rich display system that allows to interpret computation results
 not only as text for classical, textual display but also :

  - as MARKDOWN for programmatically-generated styled text

  - as LATEX for programmatically-generated math formulas

  - as PNG or JPEG for bitmap image display

  - as SVG or PDF for vectorial drawings

  - as HTML for arbitrary displays (e.g. whole www pages as inner frames,
     interactive displays through embedded javascripts, etc.)

  - as JAVASCRIPT for whatever we'd like to do dynamically in the browser.

|#

#|

  ## Display objects ##

A `diplay-object` instance associates a Lisp `value` to some
 `data` corresponding to its representation.

|#

(defclass display-object ()
  ((value :initarg :value :reader display-object-value)
   (data :initarg :data :reader display-object-data))
  (:documentation "The class of DISPLAY-OBJECTs, i.e. objets supposed
to be displayed by the Fishbowl/IPython frontend."))

#|

  ## Renderers ##

A renderer is a set of generic functions whose purpose is to
build display representations for Lisp values.  The only default
 representation is the plain text representation produced by the
Lisp printer.  The other renderers must be specialized for rich
display of user objects.

|#

(defgeneric render-plain (value)
  (:documentation "RENDER a value as plain text (default rendering)."))

#|

By default values are repsesented as plain text as produced by the
Lisp printer. In most cases this is enough but specializations are
 of course possible.

|#

(defmethod render-plain ((value t))
  (format nil "~A" value))

(example (render-plain '(1 2 3))
			  => "(1 2 3)")



(defun make-display-object (value data)
  (make-instance 'display-object
		 :value value
		 :data (or data
			   (render-plain value))))

(defgeneric render-html (value)
  (:documentation "RENDER a value as HTML."))

(defmethod render-html ((value t))
  nil) ; by default no rendering

(defgeneric render-markdown (value)
  (:documentation "RENDER a value as MARDOWN."))

(defmethod render-markdown ((value t))
  nil) ; by default no rendering


(defun combine-render (pairs)
  (loop 
   for pair in pairs 
     when (not (null (cdr pair)))
   collect pair))

(example (combine-render  `(("hello" . "world")
			    ("bonjour" . nil)
			    ("griacias" . (1 2 3))))
	 => '(("hello" . "world") 
	      ("griacias" . (1 2 3))))


(defun display (value)
  "Display VALUE in all supported representations."
  (if (typep value 'display-object)
      value ; already displayed
      ;; otherwise needs to display
      (let ((data (combine-render 
		   `(("text/plain" . ,(render-plain value))
		     ("text/html" . ,(render-html value))
		     ("text/markdown" . ,(render-markdown value))
		     ("text/latex" . ,(render-latex value))
		     ("image/png" . ,(render-png value))
		     ("image/jpeg" . ,(render-jpeg value))
		     ("image/svg+xml" . ,(render-svg value))
		     ("application/json" . ,(render-json value))
		     ("application/javascript" . ,(render-javascript value))))))
	(make-instance 'display-object :value value :data data))))

(defun display-plain (value)
  "Display VALUE in plain text."
  (if (typep value 'display-object)
      value ; already displayed
      ;; otherwise needs to display
      (let ((data `(("text/plain" . ,(render-text value)))))
	(make-instance 'display-object :value value :data data))))

(defun display-png (value)
  "Display VALUE as a base64 string encoded PNG image."
  (if (typep value 'display-object)
      value ; already displayed
      ;; otherwise needs to display
      (let ((data `(("image/png" . ,(render-png value)))))
	(make-instance 'display-object :value value :data data))))
