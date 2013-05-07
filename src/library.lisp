(in-package :sdl2)

(defctype sdl2-ffi::size-t :unsigned-int)

(define-foreign-library libsdl2
  (t (:default "libSDL2")))

#+-(use-foreign-library libsdl2)
