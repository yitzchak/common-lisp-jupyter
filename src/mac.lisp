(in-package #:jupyter)

(defclass mac (source)
  ((signature-scheme :initarg :signature-scheme
                     :reader mac-signature-scheme
                     :type string
                     :documentation "Signature scheme from connection file.")
   (key :initarg :key
        :reader mac-key
        :documentation "Signing key from connection file.")
   (args :initform nil
         :accessor mac-args)))

(defmethod start ((m mac))
  (inform :info m "Starting message authentification")
  (with-slots (key signature-scheme args) m
    (when key
      (if (starts-with-subseq "hmac-" signature-scheme)
        (let* ((digest-name (subseq signature-scheme 5))
               (digest-type (find-symbol (string-upcase digest-name) 'ironclad)))
          (if (and digest-type (ironclad:digest-supported-p digest-type))
            (progn
              (inform :info m "Using hmac with ~A digest for message signing." digest-name)
              (setf args (list 'ironclad:hmac key digest-type)))
            (inform :error m "Unknown digest ~A" digest-name)))
        (inform :error m "Unknown signature scheme ~A" signature-scheme)))))

(defmethod stop ((m mac))
  (inform :info m "Stopping message authentification"))

(defun octets-to-hex-string (bytes)
  (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list)))

(defun compute-signature (m parts)
  (with-slots (args) m
    (if m
      (let ((mac (apply #'ironclad:make-mac args)))
        ;; updates
        (iter
          (for part in parts)
          (ironclad:update-mac mac (babel:string-to-octets part)))
        ;; digest
        (octets-to-hex-string (ironclad:produce-mac mac)))
      "")))
