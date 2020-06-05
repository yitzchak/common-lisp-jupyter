(in-package #:jupyter-widgets)

(defparameter +widget-prefix+ "IPY_MODEL_")

; Bool

(defmethod serialize-trait (object (type (eql :bool)) name (value (eql nil)))
  :false)

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
  (if value
    (jupyter:json-new-obj
      ("year" (parse-integer value :start 0 :end 4))
      ("month" (1- (parse-integer value :start 5 :end 7)))
      ("date" (parse-integer value :start 8 :end 10)))
    :null))

(defmethod deserialize-trait (object (type (eql :date)) name value)
  (when (cdr value)
    (format nil "~4,'0D-~2,'0D-~2,'0D"
                (jupyter:json-getf value "year")
                (1+ (jupyter:json-getf value "month"))
                (jupyter:json-getf value "date"))))

; Dict

(defmethod serialize-trait (object (type (eql :dict)) name value)
  (cons :obj value))

(defmethod deserialize-trait (object (type (eql :dict)) name value)
  (cdr value))

; Integer

(defmethod serialize-trait (object (type (eql :int)) name (value (eql nil)))
  :null)

; Float

(defmethod deserialize-trait (object (type (eql :float)) name value)
  (coerce value 'double-float))

; Float List

(defmethod deserialize-trait (object (type (eql :float-list)) name value)
  (mapcar (lambda (x) (coerce x 'double-float)) value))

; Link

(defmethod serialize-trait (object (type (eql :link)) name value)
  (when value
    (list (serialize-trait object :widget name (first value))
          (serialize-trait object :trait-name name (second value)))))

(defmethod deserialize-trait (object (type (eql :link)) name value)
  (when value
    (list (deserialize-trait object :widget name (first value))
          (deserialize-trait object :trait-name name (second value)))))

; plist snake case

(defmethod serialize-trait (object (type (eql :plist-snake-case)) name value)
  (cons :obj
        (mapcar (lambda (pair)
                  (cons (symbol-to-snake-case (car pair)) (cdr pair)))
                (alexandria:plist-alist value))))

(defmethod deserialize-trait (object (type (eql :plist-snake-case)) name value)
  (mapcan (lambda (pair)
            (list (snake-case-to-symbol (car pair))
                  (cdr pair)))
          (cdr value)))

; plist camel case

(defmethod serialize-trait (object (type (eql :plist-camel-case)) name value)
  (cons :obj
    (mapcar (lambda (pair)
              (cons (symbol-to-camel-case (car pair)) (cdr pair)))
            (alexandria:plist-alist value))))

(defmethod deserialize-trait (object (type (eql :plist-camel-case)) name value)
  (mapcan (lambda (pair)
            (list (camel-case-to-symbol (car pair))
                  (cdr pair)))
          (cdr value)))

; Trait Name

(defmethod serialize-trait (object (type (eql :trait-name)) name value)
  (when value
    (symbol-to-snake-case value)))

(defmethod deserialize-trait (object (type (eql :trait-name)) name value)
  (when value
    (snake-case-to-symbol value)))

; Unicode

(defmethod serialize-trait (object (type (eql :unicode)) name (value (eql nil)))
  :null)

; Widget

(defmethod serialize-trait (object (type (eql :widget)) name (value (eql nil)))
  :null)

(defmethod serialize-trait (object type name (value widget))
  (concatenate 'string +widget-prefix+ (jupyter:comm-id value)))

(defmethod deserialize-trait (object (type (eql :widget)) name value)
  (jupyter:get-comm (subseq value (length +widget-prefix+))))

; Widget List

(defmethod serialize-trait (object (type (eql :widget-list)) name value)
  (mapcar (lambda (v) (serialize-trait object :widget name v)) value))

(defmethod deserialize-trait (object (type (eql :widget-list)) name value)
  (mapcar (lambda (v) (deserialize-trait object :widget name v)) value))
