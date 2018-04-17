(in-package #:maxima)

#|

init-cl.lisp Overrides

to_lisp and to-maxima are overridden since we do not have true Maxima REPL. We
set a flag and throw out of the current evaluation.

|#

(defun $to_lisp ()
  (format t "~&Type (to-maxima) to restart, ($quit) to quit Maxima.~%")
  ; REPLACE the following with a call to to-lisp.
  ; (let ((old-debugger-hook *debugger-hook*))
  ;   (catch 'to-maxima
  ;     (unwind-protect
	;    (maxima-read-eval-print-loop)
	; (setf *debugger-hook* old-debugger-hook)
	; (format t "Returning to Maxima~%")))))
  ; BEGIN
  (maxima-jupyter::to-lisp))
  ; END

(defun to-maxima ()
  ; REPLACE the following with a call to to-maxima along with a message.
  ; (throw 'to-maxima t))
  ; BEGIN
  (format t "Returning to Maxima~%")
  (maxima-jupyter::to-maxima))
  ; END


#|

mload.lisp Overrides

batch is overridden to catch the demo flag and use the set_next_input payload
to send the code to the client.

|#

(let ((old-$batch #'$batch))
  (defun $batch (filename &optional (demo :batch))
    (if (eql demo '$demo)
      (with-open-file (in-stream filename)
        (format t (intl:gettext "~%read and interpret file: ~A~%")
          (truename in-stream))
        (loop
          for expr = (dbm-read in-stream nil) then (dbm-read in-stream nil)
          while expr
          do
          (maxima-jupyter::enqueue-input
            (with-output-to-string (f)
              (mgrind (third expr) f)
              (write-char (if (eql (caar expr) 'displayinput) #\; #\$) f)))))
      (apply old-$batch (list filename demo)))))

#|

nparse.lisp Overrides

mread-synerr is overridden so that a condition is created instead of a throw.
The original is not included since we call it directly.

|#

(let ((old-mread-synerr #'mread-synerr))
  (defun mread-synerr (&rest args)
    (error (make-condition 'maxima-jupyter::maxima-syntax-error :message
      (with-output-to-string (*standard-output*)
        (catch 'macsyma-quit
          (apply old-mread-synerr args)))))))

#|

suprv1.lisp Overrides

$quit is overridden send a condition versus signally bye.

|#


(defmfun $quit ()
  ;; REPLACE the following with a condition.
  ;; (princ *maxima-epilog*)
  ;; (bye)
  ;; (mtell (intl:gettext "quit: No known quit function for this Lisp.~%")))
  ;; BEGIN
  (error (make-condition 'maxima-jupyter::quit)))
  ;; END
