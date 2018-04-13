(in-package #:maxima)

#|

macdes.lisp Overrides

|#


#|

$example is overridden so that input is sent as display_data and the output
produced by meval will handled by displayed make-maxima-result.

|#

(defmspec $example (l)
  (declare (special *need-prompt*))
  (let ((example (second l)))
    (when (symbolp example)
      ;; Coerce a symbol to be a string.
      ;; Remove the first character if it is a dollar sign.
      (setq example (coerce (exploden (stripdollar example)) 'string)))
    (unless (stringp example)
      (merror
        (intl:gettext "example: argument must be a symbol or a string; found: ~M") example))
    ;; Downcase the string. $example is not case sensitive.
    (setq example (string-downcase example))
    (with-open-file (st ($file_search1 $manual_demo '((mlist) $file_search_demo)))
      (prog (tem all c-tag d-tag)
       again
       (setq tem (read-char st nil))
       (unless tem (go notfound))
       (unless (eql tem #\&) (go again))
       (setq tem (read-char st nil))
       (unless (eql tem #\&) (go again))
       ;; so we are just after having read &&

       (setq tem (read st nil nil))
       (unless tem (go notfound))
       ;; Coerce the topic in tem to be a string.
       (setq tem (coerce (exploden tem) 'string))
       (cond ((string= tem example)
	      (go doit))
	     (t (push tem all)
		(go again)))
       ;; at this stage we read maxima forms and print and eval
       ;; until a peek sees '&' as the first character of next expression,
       ;; but at first skip over whitespaces.
       doit
       (when (member (setq tem (peek-char nil st nil))
                     '(#\tab #\space #\newline #\linefeed #\return #\page))
         ;; Found whitespace. Read char and look for next char.
         ;; The && label can be positioned anywhere before the next topic.
         (setq tem (read-char st nil))
         (go doit))
       (cond ((or (null tem) (eql tem #\&))
	      (setf *need-prompt* t)
	      (return '$done)))
       (setq tem (dbm-read st nil nil))
       (incf $linenum)
       (setq c-tag (makelabel $inchar))
       (unless $nolabels (setf (symbol-value c-tag) (nth 2 tem)))
   ;; REPLACE the following that evaluates and displays results.
   ;;     (let ($display2d)
   ;; (displa `((mlabel) ,c-tag ,(nth 2 tem))))
   ;;     (setq $% (meval* (nth 2 tem)))
   ;;     (setq d-tag (makelabel $outchar))
   ;;     (unless $nolabels (setf (symbol-value d-tag) $%))
   ;;     (when (eq (caar tem) 'displayinput)
   ;; (displa `((mlabel) ,d-tag ,$%)))
   ;; BEGIN
       (maxima-jupyter::display-and-eval tem)
   ;; END
       (go doit)

       notfound
       (setf *need-prompt* t)
       (if (= (length l) 1)
         (return `((mlist) ,@(nreverse all)))
         (progn
           (mtell (intl:gettext "example: ~M not found. 'example();' returns the list of known examples.~%") example)
           (return '$done)))))))


#|

mload.lisp Overrides

|#

#|

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
