(in-package #:jupyter-widgets)

(defclass audio (dom-widget)
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
   (format
    :initarg :format
    :initform "mp3"
    :accessor widget-format
    :documentation "The format of the media."
    :trait :bool)
   (loop
    :initarg :loop
    :initform t
    :accessor widget-loop
    :documentation "When true, the audio will start from the beginning after finishing"
    :trait :bool)
   (value
    :initarg :value
    :initform nil
    :accessor widget-value
    :documentation "The media data as a byte string."
    :trait :byte))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "AudioModel"
    :%view-name "AudioView"))

(register-widget audio)


(defclass video (audio)
  ((height
    :initarg :height
    :initform ""
    :accessor widget-height
    :documentation "Height of the video in pixels."
    :trait :string)
   (width
    :initarg :width
    :initform ""
    :accessor widget-width
    :documentation "Width of the video in pixels."
    :trait :string))
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "VideoModel"
    :%view-name "VideoView"
    :format "mp4"))

(register-widget video)
