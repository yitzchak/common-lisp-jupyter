(in-package #:common-lisp-jupyter)

#+ros.installing
(eval-when (:compile-toplevel)
  (defparameter roswell.install::*build-hook* #'install-roswell))
