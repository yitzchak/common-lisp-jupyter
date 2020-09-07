(ql:quickload :drakma)


(defparameter +clhs-map-root+ "http://www.lispworks.com/documentation/HyperSpec/Data/")
(defparameter +clhs-map-name+ "Map_Sym.txt")


(defun load-clhs-map ()
  (handler-case
      (let ((stream (drakma:http-request (puri:merge-uris +clhs-map-name+ +clhs-map-root+) :want-stream t)))
        (unwind-protect
            (do* ((name (read-line stream nil) (read-line stream nil))
                  (link (read-line stream nil) (read-line stream nil))
                  results)
                 ((or (null name) (null link)) (reverse results))
              (push (cons name (subseq link 8)) results))
          (close stream)))
    (drakma:drakma-error ())))

