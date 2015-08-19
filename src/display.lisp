
(in-package #:cl-jupyter)

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


#|

By default values are repsesented as plain text as produced by the
Lisp printer. In most cases this is enough but specializations are
 of course possible.

|#

(defgeneric render-plain (value)
  (:documentation "Render the VALUE as plain text (default rendering)."))


(defmethod render-plain ((value t))
  ;; Lisp printer by default
  (format nil "~S" value))

(example (render-plain '(1 2 3))
			  => "(1 2 3)")


(defgeneric render-html (value)
  (:documentation "Render the VALUE as an HTML document (represented as a sting)."))

(defmethod render-html ((value t))
  ;; no rendering by default
  nil)

(defgeneric render-markdown (value)
  (:documentation "Render the VALUE as MARDOWN text."))

(defmethod render-markdown ((value t))
  ;; no rendering by default
  nil)


(defgeneric render-latex (value)
  (:documentation "Render the VALUE as a LATEX document."))

(defmethod render-latex ((value t))
  ;; no rendering by default
  nil)

(defgeneric render-png (value)
  (:documentation "Render the VALUE as a PNG image. The expected
 encoding is a Base64-encoded string."))

(defmethod render-png ((value t))
  ;; no rendering by default
  nil)

(defgeneric render-jpeg (value)
  (:documentation "Render the VALUE as a JPEG image. The expected
 encoding is a Base64-encoded string."))

(defmethod render-jpeg ((value t))
  ;; no rendering by default
  nil)

(defgeneric render-svg (value)
  (:documentation "Render the VALUE as a SVG image (XML format represented as a string)."))

(defmethod render-svg ((value t))
  ;; no rendering by default
  nil)

(defgeneric render-json (value)
  (:documentation "Render the VALUE as a JSON document. This uses the MYJSON encoding
 (alist with string keys)"))

(defmethod render-json ((value t))
  ;; no rendering by default
  nil)

(defgeneric render-javascript (value)
  (:documentation "Render the VALUE as a JAVASCRIPT source (represented as a string)."))

(defmethod render-javascript ((value t))
  ;; no rendering by default
  nil)


#|

 ## Display methods ##

|#

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

(defun display-dispatch (value render-alist)
  (if (typep value 'display-object)
      value ; already displayed
      ;; otherwise needs to display
      (let ((data (combine-render (cons `("text/plain" . ,(render-plain value)) ; at least text/plain encoding is required
					render-alist))))
	(make-instance 'display-object :value value :data data))))


(defun display (value)
  "Display VALUE in all supported representations."
  (display-dispatch value  `(("text/html" . ,(render-html value))
			     ("text/markdown" . ,(render-markdown value))
			     ("text/latex" . ,(render-latex value))
			     ("image/png" . ,(render-png value))
			     ("image/jpeg" . ,(render-jpeg value))
			     ("image/svg+xml" . ,(render-svg value))
			     ("application/json" . ,(render-json value))
			     ("application/javascript" . ,(render-javascript value)))))

(defun display-html (value)
  "Display VALUE as HTML."
  (display-dispatch value `(("text/html" . ,(render-html value)))))

(defun display-markdown (value)
  "Display VALUE as MARDOWN text."
  (display-dispatch value `(("text/markdown" . ,(render-markdown value)))))

(defun display-latex (value)
  "Display VALUE as a LATEX document."
  (display-dispatch value `(("text/latex" . ,(render-latex value)))))

(defun display-png (value)
  "Display VALUE as a PNG image."
  (display-dispatch value `(("image/png" . ,(render-png value)))))

(defun display-jpeg (value)
  "Display VALUE as a JPEG image."
  (display-dispatch value `(("image/jpeg" . ,(render-jpeg value)))))

(defun display-svg (value)
  "Display VALUE as a SVG image."
  (display-dispatch value `(("image/svg+xml" . ,(render-svg value)))))

(defun display-json (value)
  "Display VALUE as a JSON document."
  (display-dispatch value `(("application/json" . ,(render-json value)))))

(defun display-javascript (value)
  "Display VALUE as embedded JAVASCRIPT."
  (display-dispatch value `(("application/javascript" . ,(render-javascript value)))))


