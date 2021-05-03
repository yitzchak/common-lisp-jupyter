(in-package #:jupyter)

#|

Standard MIME types

|#

(defvar +gif-mime-type+ "image/gif")
(defvar +html-mime-type+ "text/html")
(defvar +javascript-mime-type+ "application/javascript")
(defvar +jpeg-mime-type+ "image/jpeg")
(defvar +json-mime-type+ "application/json")
(defvar +latex-mime-type+ "text/latex")
(defvar +lisp-mime-type+ "text/x-common-lisp")
(defvar +markdown-mime-type+ "text/markdown")
(defvar +pdf-mime-type+ "application/pdf")
(defvar +plain-text-mime-type+ "text/plain")
(defvar +png-mime-type+ "image/png")
(defvar +ps-mime-type+ "application/postscript")
(defvar +svg-mime-type+ "image/svg+xml")
(defvar +vega-mime-type+ "application/vnd.vega.v5+json")
(defvar +vega-lite-mime-type+ "application/vnd.vegalite.v4+json")


(defgeneric mime-bundle-data (value)
  (:documentation "Return a JSON object with keys that mime types and the values are a rendering
of `value` in that mime type.")
  (:method (value)
    (list :object-plist
          +plain-text-mime-type+
          (string-trim '(#\Newline)
                       (with-output-to-string (s)
                         (pprint value s))))))


(defgeneric mime-bundle-metadata (value)
  (:documentation "Return metadata specific to `value`.")
  (:method (value)
    (declare (ignore value))
    :empty-object))


(defclass mime-bundle ()
  ((data
     :reader mime-bundle-data
     :initarg :data
     :initform :empty-object)
   (metadata
     :reader mime-bundle-metadata
     :initarg :metadata
     :initform :empty-object)))


(defun make-mime-bundle (data &optional metadata)
  (make-instance 'mime-bundle
                 :data (or data :empty-object)
                 :metadata (or metadata :empty-object)))


(defun execute-result (result)
  "Send a result as mime bundle execution result. `result` must implement the `mime-bundle-data`
method and optionally `mime-bundle-metadata`."
  (send-execute-result (kernel-iopub *kernel*) *message* (kernel-execution-count *kernel*)
                       (mime-bundle-data result) (mime-bundle-metadata result)))


(defun display (result &key id update)
  "Send a result as mime bundle display data. `result` must implement the `mime-bundle-data`
method and optionally `mime-bundle-metadata`. If an `id` is specified then future calls with the
same `id` and `update` is `t`."
  (send-display-data (kernel-iopub *kernel*) *message*
                     (mime-bundle-data result) (mime-bundle-metadata result)
                     (if id
                       (list :object-plist "display_id" id)
                       :empty-object)
                     update))


(defun make-file-mime-bundle (path mime-type metadata display-data update id)
  (let* ((mime-type (or mime-type (trivial-mimes:mime path)))
         (bundle (make-instance 'mime-bundle
                                :metadata (or metadata :empty-object)
                                :data (if (equal mime-type +plain-text-mime-type+)
                                        (list :object-plist
                                              mime-type (alexandria:read-file-into-string path))
                                        (list :object-plist
                                              +plain-text-mime-type+ path
                                              mime-type (if (or (equal mime-type +svg-mime-type+)
                                                                (alexandria:starts-with-subseq "text/" mime-type))
                                                          (alexandria:read-file-into-string path)
                                                          (cl-base64:usb8-array-to-base64-string
                                                            (alexandria:read-file-into-byte-vector path))))))))
    (cond
      (display-data
        (display bundle :update update :id id)
        (values))
      (t
        bundle))))


(defun make-inline-mime-bundle (value mime-type metadata display-data update id)
  (let ((bundle (make-instance 'mime-bundle :data (list :object-plist
                                                        mime-type value)
                                            :metadata (or metadata :empty-object))))
    (cond
      (display-data
        (display bundle :update update :id id)
        (values))
      (t
        bundle))))


(defun file (path &key display update id)
  "Create a result based on a file path. The mime type with automatically be
  determined from the file extension."
  (make-file-mime-bundle path nil nil display update id))


(defun gif-file (path &key display update id)
  "Create a GIF image result based on a file path."
  (make-file-mime-bundle path +gif-mime-type+ nil display update id))


(defun jpeg-file (path &key display update id)
  "Create a JPEG image result based on a file path."
  (make-file-mime-bundle path +jpeg-mime-type+ nil display update id))


(defun json-file (path &key display update id expanded)
  "Create a JSON result based on a file path."
  (make-file-mime-bundle path +json-mime-type+
                             (list :object-plist
                               +json-mime-type+
                               (list :object-plist
                                     "expanded" (if expanded :true :false)))
                        display update id))


(defun pdf-file (path &key display update id)
  "Create a PDF result based on a file path."
  (make-file-mime-bundle path +pdf-mime-type+ nil display update id))


(defun png-file (path &key display update id)
  "Create a PNG image result based on a file path."
  (make-file-mime-bundle path +png-mime-type+ nil display update id))


(defun ps-file (path &key display update id)
  "Create a PostScript result based on a file path."
  (make-file-mime-bundle path +ps-mime-type+ nil display update id))


(defun svg-file (path &key display update id)
  "Create a SVG result based on a file path."
  (make-file-mime-bundle path +gif-mime-type+ nil display update id))


(defun vega-file (path &key display update id)
  "Create a Vega graph based on a file path."
  (make-file-mime-bundle path +vega-mime-type+ nil display update id))


(defun vega-lite-file (path &key display update id)
  "Create a VegaLite graph based on a file path."
  (make-file-mime-bundle path +vega-lite-mime-type+ nil display update id))


(defun inline-result (value mime-type &key display update id)
  "Create a result based on an inline value."
  (make-inline-mime-bundle value mime-type nil display update id))


(defun text (value &key display update id)
  "Create a plain text result based on an inline value."
  (make-inline-mime-bundle value +plain-text-mime-type+ nil display update id))


(defun html (value &key display update id)
  "Create a HTML result based on an inline value."
  (make-inline-mime-bundle value +html-mime-type+ nil display update id))


(defun javascript (value &key display update id)
  "Create a JavaScript text result based on an inline value."
  (make-inline-mime-bundle value +javascript-mime-type+ nil display update id))


(defun jpeg (value &key display update id)
  "Create a JPEG image result based on an inline value."
  (make-inline-mime-bundle value +jpeg-mime-type+ nil display update id))


(defun latex (value &key display update id)
  "Create a LaTeX result based on an inline value."
  (make-inline-mime-bundle value +latex-mime-type+ nil display update id))


(defun json (value &key display update id expanded)
  "Create a plain text result based on an inline value."
  (make-inline-mime-bundle value +json-mime-type+
                           (list :object-plist
                                 +json-mime-type+
                                 (list :object-plist
                                       "expanded" (if expanded :true :false)))
                           display update id))


(defun markdown (value &key display update id)
  "Create a Markdown result based on an inline value."
  (make-inline-mime-bundle value +markdown-mime-type+ nil display update id))


(defun png (value &key display update id)
  "Create a PNG image result based on an inline value."
  (make-inline-mime-bundle value +png-mime-type+ nil display update id))


(defun svg (value &key display update id)
  "Create a SVG result based on an inline value."
  (make-inline-mime-bundle value +svg-mime-type+ nil display update id))


(defun vega (value &key display update id)
  "Create a Vega result based on an inline value."
  (make-inline-mime-bundle value +vega-mime-type+ nil display update id))


(defun vega-lite (value &key display update id)
  "Create a VegaLite result based on an inline value."
  (make-inline-mime-bundle value +vega-lite-mime-type+ nil display update id))


(defclass display-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((mime-type :initarg :mime-type
              :reader display-stream-mime-type)
   (value :initarg :value
          :initform (make-array *iopub-stream-size*
                                :fill-pointer 0
                                :adjustable t
                                :element-type 'character)
          :reader display-stream-value)))


(defun make-display-stream (mime-type)
  (make-instance 'display-stream :mime-type mime-type))


(defmethod trivial-gray-streams:stream-write-char ((stream display-stream) char)
  (vector-push-extend char (display-stream-value stream)))


(defmethod trivial-gray-streams:stream-finish-output ((stream display-stream))
  (with-slots (channel mime-type value) stream
    (unless (zerop (length value))
      (inline-result value mime-type :display t)
      (adjust-array value (array-total-size value)
                    :fill-pointer 0))))


#|

Jupyter clients generally don't know about the myriad of mime types associated
with TeX/LaTeX and assume that the proper mime type is always text/latex. The
following function will make sure that trivial-mimes database reflects this.

|#

(defun check-mime-db ()
  (dolist (ext '("tex" "latex" "tikz"))
    (setf (gethash ext trivial-mimes:*mime-db*) +latex-mime-type+)))

(check-mime-db)
