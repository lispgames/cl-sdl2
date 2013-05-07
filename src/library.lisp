(in-package :sdl2)

(defctype sdl2-ffi::size-t :unsigned-int)

(define-foreign-library libsdl2
  (:unix (:or "libSDL2-2.0.so.0" "libSDL2"))
  (t (:default "libSDL2")))

(use-foreign-library libsdl2)
