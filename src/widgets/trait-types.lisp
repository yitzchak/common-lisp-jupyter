(in-package #:jupyter-widgets)

(defparameter +widget-prefix+ "IPY_MODEL_")

; vector

(defmethod serialize-trait (object type name (value vector))
  (declare (ignore object type name))
  (if (binary-value-p value)
    (values :null (list nil) (list value))
    (values value nil nil)))

; Buffer

(defmethod serialize-trait (object (type (eql :buffer)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :null nil nil))

(defmethod serialize-trait (object (type (eql :buffer)) name value)
  (declare (ignore object type name))
  (values :null (list nil) (list value)))

(defmethod deserialize-trait (object (type (eql :buffer)) name (value vector))
  (declare (ignore type name))
  (if (binary-value-p value)
    (let ((result (static-vectors:make-static-vector (length value)
                                                     :element-type '(unsigned-byte 8))))
      (trivial-garbage:finalize object (lambda () (static-vectors:free-static-vector result)))
      (static-vectors:replace-foreign-memory (static-vectors:static-vector-pointer result)
                                             (static-vectors:static-vector-pointer value)
                                             (length value))
      result)
    value))

; Buffer List

(defmethod serialize-trait (object (type (eql :buffer-list)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :empty-array nil nil))

(defmethod serialize-trait (object (type (eql :buffer-list)) name value)
  (declare (ignore type name))
  (declare (ignore type name))
  (let (arr buffer-paths buffers)
    (trivial-do:dolist* (index v value (values (nreverse arr) buffer-paths buffers))
      (multiple-value-bind (sv sub-buffer-paths sub-buffers)
                           (serialize-trait object :buffer nil v)
        (setf buffer-paths (nconc buffer-paths
                                  (mapcar (lambda (sp) (cons index sp)) sub-buffer-paths))
              buffers (nconc buffers sub-buffers))
        (push sv arr)))))

; Bool

(defmethod serialize-trait (object (type (eql :bool)) name value)
  (declare (ignore object type name))
  (cond
    ((eql :null value)
      :null)
    (value
      :true)
    (t
      :false)))

(defmethod deserialize-trait (object (type (eql :bool)) name (value (eql :false)))
  (declare (ignore object type name value))
  nil)

(defmethod deserialize-trait (object (type (eql :bool)) name (value (eql :true)))
  (declare (ignore object type name value))
  t)

; color

(defparameter *color-names*
  '("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure" "beige" "bisque"
    "black" "blanchedalmond" "blue" "blueviolet" "brown" "burlywood" "cadetblue"
    "chartreuse" "chocolate" "coral" "cornflowerblue" "cornsilk" "crimson"
    "cyan" "darkblue" "darkcyan" "darkgoldenrod" "darkgray" "darkgreen"
    "darkkhaki" "darkmagenta" "darkolivegreen" "darkorange" "darkorchid"
    "darkred" "darksalmon" "darkseagreen" "darkslateblue" "darkslategray"
    "darkturquoise" "darkviolet" "deeppink" "deepskyblue" "dimgray" "dodgerblue"
    "firebrick" "floralwhite" "forestgreen" "fuchsia" "gainsboro" "ghostwhite"
    "gold" "goldenrod" "gray" "green" "greenyellow" "honeydew" "hotpink"
    "indianred " "indigo " "ivory" "khaki" "lavender" "lavenderblush"
    "lawngreen" "lemonchiffon" "lightblue" "lightcoral" "lightcyan"
    "lightgoldenrodyellow" "lightgray" "lightgreen" "lightpink" "lightsalmon"
    "lightseagreen" "lightskyblue" "lightslategray" "lightsteelblue"
    "lightyellow" "lime" "limegreen" "linen" "magenta" "maroon"
    "mediumaquamarine" "mediumblue" "mediumorchid" "mediumpurple"
    "mediumseagreen" "mediumslateblue" "mediumspringgreen" "mediumturquoise"
    "mediumvioletred" "midnightblue" "mintcream" "mistyrose" "moccasin"
    "navajowhite" "navy" "oldlace" "olive" "olivedrab" "orange" "orangered"
    "orchid" "palegoldenrod" "palegreen" "paleturquoise" "palevioletred"
    "papayawhip" "peachpuff" "peru" "pink" "plum" "powderblue" "purple"
    "rebeccapurple" "red" "rosybrown" "royalblue" "saddlebrown" "salmon"
    "sandybrown" "seagreen" "seashell" "sienna" "silver" "skyblue" "slateblue"
    "slategray" "snow" "springgreen" "steelblue" "tan" "teal" "thistle" "tomato"
    "transparent" "turquoise" "violet" "wheat" "white" "whitesmoke" "yellow"
    "yellowgreen"))

(defparameter *hex-digits*
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\A #\b #\B #\c #\c #\d #\D #\e
    #\E #\f #\F))

(defmethod validate-trait (object (type (eql :color)) name value)
  (if (and (stringp value)
           (or (position value *color-names* :test #'equal)
               (and (or (equal (length value) 4) (equal (length value) 7))
                    (char-equal #\# (char value 0))
                    (every (lambda (ch) (position ch *hex-digits*))
                      (subseq value 1)))))
    (call-next-method)
    (error 'trait-error :format-control "Invalid color of ~A assigned to ~A"
                        :format-arguments (list value name))))

; Date

(defmethod serialize-trait (object (type (eql :date)) name value)
  (declare (ignore object type name))
  (values (if value
            `(:object-alist
               ("year" . ,(parse-integer value :start 0 :end 4))
               ("month" . ,(1- (parse-integer value :start 5 :end 7)))
               ("date" . ,(parse-integer value :start 8 :end 10)))
            :null)
          nil nil))

(defmethod deserialize-trait (object (type (eql :date)) name value)
  (declare (ignore object type name))
  (format nil "~4,'0D-~2,'0D-~2,'0D"
              (gethash "year" value 2000)
              (1+ (gethash "month" value 0))
              (gethash "date" value 1)))

; alist

(defmethod serialize-trait (object (type (eql :alist)) name (value (eql :null)))
  (declare (ignore object type name value))
  (values :null nil nil))

(defmethod deserialize-trait (object (type (eql :alist)) name (value (eql :null)))
  (declare (ignore object type name value))
  :null)

(defmethod serialize-trait (object (type (eql :alist)) name (value list))
  (declare (ignore name type))
  (let (obj buffer-paths buffers)
    (trivial-do:doalist (k v value (values (cons :object-alist obj) buffer-paths buffers))
      (multiple-value-bind (sv sub-buffer-paths sub-buffers)
                           (serialize-trait object :json nil v)
        (setf buffer-paths (nconc buffer-paths
                                  (mapcar (lambda (sp) (cons k sp)) sub-buffer-paths))
              buffers (nconc buffers sub-buffers))
        (setf obj (acons k sv obj))))))

(defmethod deserialize-trait (object (type (eql :alist)) name (value hash-table))
  (alexandria:hash-table-alist value))

; alist list

(defmethod serialize-trait (object (type (eql :alist-list)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :empty-array nil nil))

(defmethod deserialize-trait (object (type (eql :alist-list)) name (value (eql :empty-array)))
  (declare (ignore object type name value))
  nil)

(defmethod serialize-trait (object (type (eql :alist-list)) name (value list))
  (declare (ignore type name))
  (let (arr buffer-paths buffers)
    (trivial-do:dolist* (index v value (values (nreverse arr) buffer-paths buffers))
      (multiple-value-bind (sv sub-buffer-paths sub-buffers)
                           (serialize-trait object :alist nil v)
        (setf buffer-paths (nconc buffer-paths
                                  (mapcar (lambda (sp) (cons index sp)) sub-buffer-paths))
              buffers (nconc buffers sub-buffers))
        (push sv arr)))))

(defmethod deserialize-trait (object (type (eql :alist-list)) name (value list))
  (declare (ignore object type name))
  (mapcar #'cdr value))


; Integer

(defmethod serialize-trait (object (type (eql :int)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :null nil nil))

; Float

(defmethod serialize-trait (object (type (eql :float)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :null nil nil))

(defmethod deserialize-trait (object (type (eql :float)) name value)
  (declare (ignore object type name))
  (coerce value 'double-float))

; Float List

(defmethod deserialize-trait (object (type (eql :float-list)) name value)
  (declare (ignore object type name))
  (mapcar (lambda (x) (coerce x 'double-float)) value))

; Single Float Buffer

(defmethod serialize-trait (object (type (eql :single-float-buffer)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :null nil nil))

(defmethod serialize-trait (object (type (eql :single-float-buffer)) name value)
  (declare (ignore object type name))
  (values :null (list nil) (list value)))

(defmethod deserialize-trait (object (type (eql :single-float-buffer)) name (value vector))
  (declare (ignore type name))
  (if (binary-value-p value)
    (let ((result (static-vectors:make-static-vector (/ (length value) 4)
                                                     :element-type 'single-float)))
      (trivial-garbage:finalize object (lambda () (static-vectors:free-static-vector result)))
      (static-vectors:replace-foreign-memory (static-vectors:static-vector-pointer result)
                                             (static-vectors:static-vector-pointer value)
                                             (length value))
      result)
    value))

; Single Float Buffer List

(defmethod serialize-trait (object (type (eql :single-float-buffer-list)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :empty-array nil nil))

(defmethod serialize-trait (object (type (eql :single-float-buffer-list)) name value)
  (declare (ignore type name))
  (let (arr buffer-paths buffers)
    (trivial-do:dolist* (index v value (values (nreverse arr) buffer-paths buffers))
      (multiple-value-bind (sv sub-buffer-paths sub-buffers)
                           (serialize-trait object :single-float-buffer nil v)
        (setf buffer-paths (nconc buffer-paths
                                  (mapcar (lambda (sp) (cons index sp)) sub-buffer-paths))
              buffers (nconc buffers sub-buffers))
        (push sv arr)))))

; Link

(defmethod serialize-trait (object (type (eql :link)) name value)
  (declare (ignore type name))
  (values (when value
            (list (serialize-trait object :widget nil (first value))
                  (serialize-trait object :trait-name nil (second value))))
          nil nil))

(defmethod deserialize-trait (object (type (eql :link)) name value)
  (declare (ignore type name))
  (when value
    (list (deserialize-trait object :widget nil (first value))
          (deserialize-trait object :trait-name nil (second value)))))

; list

(defmethod serialize-trait (object (type (eql :list)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :empty-array nil nil))

; plist snake case

(defmethod serialize-trait (object (type (eql :plist-snake-case)) name value)
  (declare (ignore name type))
  (let (obj buffer-paths buffers)
    (trivial-do:doplist (k v value (values (cons :object-alist obj) buffer-paths buffers))
      (let ((path (symbol-to-snake-case k)))
        (multiple-value-bind (sv sub-buffer-paths sub-buffers)
                             (serialize-trait object :json nil v)
          (setf buffer-paths (nconc buffer-paths
                                    (mapcar (lambda (sp) (cons path sp)) sub-buffer-paths))
                buffers (nconc buffers sub-buffers))
          (setf obj (acons path sv obj)))))))

(defmethod deserialize-trait (object (type (eql :plist-snake-case)) name value)
  (declare (ignore object type name))
  (mapcan (lambda (pair)
            (list (snake-case-to-symbol (car pair))
                  (cdr pair)))
          (cdr value)))

; plist camel case

(defmethod serialize-trait (object (type (eql :plist-camel-case)) name value)
  (declare (ignore name type))
  (let (obj buffer-paths buffers)
    (trivial-do:doplist (k v value (values (cons :object-alist obj) buffer-paths buffers))
      (let ((path (symbol-to-camel-case k)))
        (multiple-value-bind (sv sub-buffer-paths sub-buffers)
                             (serialize-trait object :json nil v)
          (setf buffer-paths (nconc buffer-paths
                                    (mapcar (lambda (sp) (cons path sp)) sub-buffer-paths))
                buffers (nconc buffers sub-buffers))
          (setf obj (acons path sv obj)))))))

(defmethod deserialize-trait (object (type (eql :plist-camel-case)) name value)
  (declare (ignore object type name))
  (mapcan (lambda (pair)
            (list (camel-case-to-symbol (car pair))
                  (cdr pair)))
          (cdr value)))

; plist list snake case

(defmethod serialize-trait (object (type (eql :plist-list-snake-case)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :empty-array nil nil))

(defmethod deserialize-trait (object (type (eql :plist-list-snake-case)) name (value (eql :empty-array)))
  (declare (ignore object type name value))
  nil)

(defmethod serialize-trait (object (type (eql :plist-list-snake-case)) name (value list))
  (declare (ignore type name))
  (let (arr buffer-paths buffers)
    (trivial-do:dolist* (index v value (values (nreverse arr) buffer-paths buffers))
      (multiple-value-bind (sv sub-buffer-paths sub-buffers)
                           (serialize-trait object :plist-snake-case nil v)
        (setf buffer-paths (nconc buffer-paths
                                  (mapcar (lambda (sp) (cons index sp)) sub-buffer-paths))
              buffers (nconc buffers sub-buffers))
        (push sv arr)))))

(defmethod deserialize-trait (object (type (eql :plist-list-snake-case)) name (value list))
  (declare (ignore type name))
  (mapcar (lambda (v)
            (deserialize-trait object :plist-snake-case nil v))
          value))

; plist list camel case

(defmethod serialize-trait (object (type (eql :plist-list-camel-case)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :empty-array nil nil))

(defmethod deserialize-trait (object (type (eql :plist-list-camel-case)) name (value (eql :empty-array)))
  (declare (ignore object type name value))
  nil)

(defmethod serialize-trait (object (type (eql :plist-list-camel-case)) name (value list))
  (declare (ignore type name))
  (let (arr buffer-paths buffers)
    (trivial-do:dolist* (index v value (values (nreverse arr) buffer-paths buffers))
      (multiple-value-bind (sv sub-buffer-paths sub-buffers)
                           (serialize-trait object :plist-camel-case nil v)
        (setf buffer-paths (nconc buffer-paths
                                  (mapcar (lambda (sp) (cons index sp)) sub-buffer-paths))
              buffers (nconc buffers sub-buffers))
        (push sv arr)))))

(defmethod deserialize-trait (object (type (eql :plist-list-camel-case)) name (value list))
  (declare (ignore type name))
  (mapcar (lambda (v)
            (deserialize-trait object :plist-camel-case nil v))
          value))

; Trait Name

(defmethod serialize-trait (object (type (eql :trait-name)) name value)
  (declare (ignore object type name))
  (values (when value
            (symbol-to-snake-case value))
          nil nil))

(defmethod deserialize-trait (object (type (eql :trait-name)) name value)
  (declare (ignore object type name))
  (when value
    (snake-case-to-symbol value)))

; Unicode

(defmethod serialize-trait (object (type (eql :unicode)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :null nil nil))

(defmethod serialize-trait (object (type (eql :string)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :null nil nil))

; Unicode array

(defmethod serialize-trait (object (type (eql :unicode-list)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :empty-array nil nil))

(defmethod serialize-trait (object (type (eql :string-list)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :empty-array nil nil))

; Widget

(defmethod serialize-trait (object (type (eql :widget)) name (value (eql nil)))
  (declare (ignore object type name value))
  (values :null nil nil))

(defmethod serialize-trait (object type name (value widget))
  (declare (ignore object type name))
  (values (concatenate 'string +widget-prefix+ (jupyter:comm-id value))
          nil nil))

(defmethod deserialize-trait (object (type (eql :widget)) name value)
  (declare (ignore object type name))
  (jupyter:get-comm (subseq value (length +widget-prefix+))))

; Widget List

(defmethod serialize-trait (object (type (eql :widget-list)) name value)
  (declare (ignore type name))
  (values (mapcar (lambda (v)
                    (serialize-trait object :widget nil v)) value)
          nil nil))

(defmethod deserialize-trait (object (type (eql :widget-list)) name value)
  (declare (ignore type name))
  (mapcar (lambda (v) (deserialize-trait object :widget nil v)) value))

(defmethod serialize-trait (object (type (eql :widget-list)) name (value (eql nil)))
  :empty-array)

; Widget Vector

(defmethod deserialize-trait (object (type (eql :widget-vector)) name value)
  (map 'vector (lambda (v) (deserialize-trait object :widget name v)) value))

(defmethod serialize-trait (object (type (eql :widget-vector)) name (value (eql nil)))
  :empty-array)

(defmethod serialize-trait (object (type (eql :widget-vector)) name (value list))
  (map 'vector (lambda (v) (serialize-trait object :widget name v)) value))

