(in-package #:sdl2)

(autowrap:define-bitmask-from-enum (keymod sdl2-ffi::sdl-keymod))

(defun key-down-p (state)
  (= state sdl2-ffi:+sdl-pressed+))

(defun key-up-p (state)
  (= state sdl2-ffi:+sdl-released+))

(defun scancode-value (keysym)
  (sdl-keysym.scancode keysym))

(defun scancode (keysym)
  (autowrap:enum-key 'sdl2-ffi:sdl-scancode
                     (scancode-value keysym)))

(defun mod-value (keysym)
  (sdl-keysym.mod keysym))

(defun sym-value (keysym)
  (sdl-keysym.sym keysym))

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


