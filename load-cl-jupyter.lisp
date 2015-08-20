(require :asdf)

(push #p"/home/robert/playpen/github/fishbowl-repl/src/" asdf:*central-registry*)
;; ?? !! (push #p"/usr/lib/sbcl/contrib/" asdf:*central-registry*)

;; for debugging
;;(push (truename "./src") asdf:*central-registry*)

;; activate debugging
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

;; in production (?)
;;(declaim (optimize (speed 3) (space 0) (debug 0) (safety 2)))

(ql:quickload "cl-jupyter")

;; (in-package #:fishbowl-user)

;; start main loop
;; (fishbowl:kernel-start)
