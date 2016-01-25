(in-package #:sdl2)

(autowrap:define-bitmask-from-enum (keymod sdl2-ffi::sdl-keymod))

(defmacro c-keysym ((ks) &body body)
  `(c-let ((,ks sdl2-ffi:sdl-keysym :from ,ks))
     ,@body))

(defun key-down-p (state)
  (= state sdl2-ffi:+sdl-pressed+))

(defun key-up-p (state)
  (= state sdl2-ffi:+sdl-released+))

(defun scancode-value (keysym)
  (c-keysym (keysym) (keysym :scancode)))

(defun scancode (keysym)
  (autowrap:enum-key 'sdl2-ffi:sdl-scancode
                     (scancode-value keysym)))

(defun scancode-symbol (scancode)
  (autowrap:enum-key 'sdl2-ffi:sdl-scancode scancode))

(defun mod-value (keysym)
  (c-keysym (keysym) (keysym :mod)))

(defun sym-value (keysym)
  (c-keysym (keysym) (keysym :sym)))

(defgeneric scancode= (value scancode-key))

(defmethod scancode= ((scancode integer) scancode-key)
  (= scancode (autowrap:enum-value 'sdl2-ffi:sdl-scancode scancode-key)))

(defmethod scancode= ((keysym sdl2-ffi:sdl-keysym) scancode-key)
  (= (scancode-value keysym)
     (autowrap:enum-value 'sdl2-ffi:sdl-scancode scancode-key)))

(defun mod-keywords (value)
  (autowrap:mask-keywords 'keymod value))

(defun mod-value-p (value &rest keywords)
  (let ((mask (autowrap:mask-apply 'keymod keywords)))
    (/= 0 (logand mask value))))

(defun keyboard-state-p (scancode)
  "Whether the key corresponding to the given scancode is currently pressed."
  (let ((state (nth-value 1 (autowrap:inhibit-string-conversion
                              (sdl2-ffi.functions:sdl-get-keyboard-state nil))))
        (scancode-num (autowrap:enum-value 'sdl2-ffi:sdl-scancode scancode)))
    (c-let ((state :unsigned-char :ptr state))
      (= (state scancode-num) 1))))

(defun get-key-from-scancode (scancode)
  (sdl-get-key-from-scancode scancode))

(defun get-key-name (key)
  (sdl-get-key-name key))

(defun scancode-name (scancode)
  (get-key-name (get-key-from-scancode scancode)))
