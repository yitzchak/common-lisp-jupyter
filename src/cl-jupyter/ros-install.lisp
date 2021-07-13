(in-package #:jupyter/common-lisp)

#+ros.installing
(eval-when (:compile-toplevel)
  (defparameter roswell.install::*build-hook* #'install-roswell))
