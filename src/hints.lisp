(in-package #:sdl2)

(defvar *hints*
  (let ((hints (make-hash-table :test #'eq)))
    (do-external-symbols (x :sdl2-ffi)
      (let* ((name (symbol-name x))
             (prefix (subseq name 1 (min (1- (length name)) 10))))
        (when (string= prefix "SDL-HINT-")
          (let* ((hint (subseq name 10 (1- (length name))))
                 (keyword (alexandria:make-keyword hint)))
            (setf (gethash keyword hints) (symbol-value x))))))
    hints))

(defun set-hint (hint value)
  (alexandria:if-let ((hint-name (gethash hint *hints*)))
    (sdl2-ffi.functions:sdl-set-hint
     hint-name
     (etypecase value
       (null "0")
       (integer (format nil "~d" value))
       (symbol (string-downcase value))
       (string value)))
    (error "Unknown SDL hint: ~s." hint)))

(defun get-hint (hint)
  (alexandria:if-let ((hint-name (gethash hint *hints*)))
    (sdl2-ffi.functions:sdl-get-hint hint-name)
    (error "Unknown SDL hint: ~s." hint)))
