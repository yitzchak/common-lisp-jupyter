(in-package #:jupyter)

(defclass mac (source)
  ((signature-scheme
     :initarg :signature-scheme
     :reader mac-signature-scheme
     :type string
     :documentation "Signature scheme from connection file.")
   (key
     :initarg :key
     :reader mac-key
     :documentation "Signing key from connection file.")
   (args
     :initform nil
     :accessor mac-args)))

(defmethod start ((m mac))
  (inform :info m "Starting message authentification")
  (with-slots (key signature-scheme args) m
    (when key
      (if #-clasp (starts-with-subseq "hmac-" signature-scheme)
          #+clasp (string= "hmac-sha256" signature-scheme)
        #-clasp
        (let* ((digest-name (subseq signature-scheme 5))
               (digest-type (find-symbol (string-upcase digest-name) 'ironclad)))
          (if (and digest-type (ironclad:digest-supported-p digest-type))
            (progn
              (inform :info m "Using hmac with ~A digest for message signing." digest-name)
              (setf args (list 'ironclad:hmac key digest-type)))
            (inform :error m "Unknown digest ~A" digest-name)))
        #+clasp
        (inform :info m "Using signature scheme ~A for message signing." signature-scheme)
        (inform :error m "Unknown signature scheme ~A" signature-scheme)))))

(defmethod stop ((m mac))
  (inform :info m "Stopping message authentification"))

(defun compute-signature (m parts)
  (if m
    #-clasp
    (let ((mac (apply #'ironclad:make-mac (mac-args m))))
      (mapc (lambda (part)
              (ironclad:update-mac mac (babel:string-to-octets part)))
            parts)
      (octets-to-hex-string (ironclad:produce-mac mac)))
    #+clasp
    (core:hmac-sha256
      (apply #'concatenate '(vector (unsigned-byte 8)) (mapcar #'babel:string-to-octets parts))
      (mac-key m))
    ""))
