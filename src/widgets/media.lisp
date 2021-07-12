(in-package #:jupyter-widgets)

(defwidget audio (dom-widget format-slot byte-value-slot)
  ((autoplay
     :initarg :autoplay
     :initform t
     :accessor widget-autoplay
     :documentation "When true, the audio starts when it's displayed."
     :trait :bool)
   (controls
     :initarg :controls
     :initform t
     :accessor widget-controls
     :documentation "Specifies that media controls should be displayed (such as a play/pause button etc)"
     :trait :bool)
   (loop
     :initarg :loop
     :initform t
     :accessor widget-loop
     :documentation "When true, the audio will start from the beginning after finishing"
     :trait :bool))
  (:default-initargs
    :%model-name "AudioModel"
    :%view-name "AudioView"
    :format "mp3")
  (:documentation "Displays an audio clip as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
audio data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to \"mp3\").

If you pass `\"url\"` to the `\"format\"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8."))




(defwidget video (audio height-slot width-slot)
  ()
  (:default-initargs
    :%model-name "VideoModel"
    :%view-name "VideoView"
    :format "mp4")
  (:documentation "Displays a video as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
video data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to \"mp4\").

If you pass `\"url\"` to the `\"format\"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8."))




(defwidget image (dom-widget format-slot height-slot width-slot byte-value-slot)
  ()
  (:default-initargs
    :%model-name "ImageModel"
    :%view-name "ImageView"
    :format "png")
  (:documentation "Displays an image as a widget.

The `value` of this widget accepts a byte string.  The byte string is the raw
image data that you want the browser to display.  You can explicitly define the
format of the byte string using the `format` trait (which defaults to \"png\").

If you pass `\"url\"` to the `\"format\"` trait, `value` will be interpreted as
a URL as bytes encoded in UTF-8."))




(defwidget play (description-widget disabled-slot int-min-max-slots int-step-slot
                 int-value-slot)
  ((%playing
    :initarg :%playing
    :initform nil
    :accessor widget-%playing
    :documentation "Whether the control is currently playing."
    :trait :bool)
   (%repeat
    :initarg :%repeat
    :initform nil
    :accessor widget-%repeat
    :documentation "Whether the control will repeat in a continous loop."
    :trait :bool)
   (interval
    :initarg :interval
    :initform 100
    :accessor widget-interval
    :documentation "The maximum value for the play control."
    :trait :int)
   (show-repeat
    :initarg :show-repeat
    :initform t
    :accessor widget-show-repeat
    :documentation "Show the repeat toggle button in the widget."
    :trait :bool))
  (:default-initargs
    :%model-name "PlayModel"
    :%view-name "PlayView")
  (:documentation
"Play/repeat buttons to step through values automatically, and optionally loop."))


