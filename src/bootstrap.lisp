(in-package :maxima)

(defmfun $bootstrap (source-path)
  (push source-path asdf:*central-registry*)
  (ql:quickload "maxima-jupyter")
  ($load "stringproc"))
