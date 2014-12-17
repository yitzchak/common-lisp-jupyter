
;; add the source directory to the ASDF registry
(push (truename "./src")  asdf:*central-registry*)

;; activate debugging
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

(ql:quickload "uncommonshell")

(in-package #:uncommon-shell)

;; TODO : start main loop

