(in-package #:jupyter-widgets)

(defclass audio (dom-widget format-slot byte-value-slot)
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
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "AudioModel"
    :%view-name "AudioView"
    :format "mp3"))

(register-widget audio)


(defclass video (audio height-slot width-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "VideoModel"
    :%view-name "VideoView"
    :format "mp4"))

(register-widget video)


(defclass image (dom-widget format-slot height-slot width-slot byte-value-slot)
  ()
  (:metaclass trait-metaclass)
  (:default-initargs
    :%model-name "ImageModel"
    :%view-name "ImageView"
    :format "png"))

(register-widget image)
