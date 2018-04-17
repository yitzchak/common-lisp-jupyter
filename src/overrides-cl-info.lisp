(in-package :cl-info)

#|

cl-info.lisp Overrides

|#

#|

display-items is overridden to make use of the pager for output.

|#

(defun display-items (items)
  (let*
    ((items-list (rearrange-matches items))
     (nitems (length items-list))
     wanted)

    (loop for i from 0 for item in items-list do
      (when (> nitems 1)
        (let
          ((heading-title (nth 4 (second item)))
           (item-name (first (second item))))
          (format t "~% ~d: ~a~@[  (~a)~]" i item-name heading-title))))

    ;; ADD the following to improve spacing in clients.
    (format t "~%~%")
    ;; END

    (setq wanted
          (if (> nitems 1)
            (prog1
              (loop
                for prompt-count from 0
                thereis (progn
                          (finish-output *debug-io*)
                          (print-prompt prompt-count)
                          (finish-output)
                          (clear-input)
                          (select-info-items
                            (parse-user-choice nitems) items-list)))
              (clear-input))
            items-list))
    (finish-output *debug-io*)
    (when (consp wanted)
      (format t "~%")
      (loop for item in wanted
	    do (let ((doc (read-info-text (first item) (second item))))
		 (if doc
		     ;; REPLACE the following with a write to *page-output*
		     ;; (format t "~A~%~%" doc)
		     ;; BEGIN
		     (format maxima-jupyter::*page-output* "~A~%~%" doc)
		     ;; END
		     (format t "Unable to find documentation for `~A'.~%~
                                Possible bug maxima-index.lisp or build_index.pl?~%"
			     (first (second item)))))))))
