(in-package #:sdl2)

(defun make-rgb-color (r g b a)
  "Return an SDL_Color filled in with the arguments. It will be garbage collected as needed."
  (c-let ((color sdl2-ffi:sdl-color))
    (setf (color :r) r
	  (color :g) g
	  (color :b) b
	  (color :a) a)
    color))

(define-struct-accessors (color sdl2-ffi:sdl-color)
  :r :g :b :a)
