(in-package :sdl2)

(cffi:define-foreign-library libsdl2
  (:darwin (:or (:framework "SDL2") (:default "libSDL2")))
  (:unix (:or "libSDL2-2.0.so.0" "libSDL2.so.0.2" "libSDL2"))
  (:windows "SDL2.dll")
  (t (:default "libSDL2")))

(cffi:use-foreign-library libsdl2)
