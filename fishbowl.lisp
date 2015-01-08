
;; add the source directory to the ASDF registry
(push (truename "./src")  asdf:*central-registry*)

;; activate debugging
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

;; in production (?)
;;(declaim (optimize (speed 3) (space 0) (debug 0) (safety 2)))

(ql:quickload "fishbowl")

(in-package #:fishbowl-user)

;; start main loop
(fishbowl:kernel-start)

