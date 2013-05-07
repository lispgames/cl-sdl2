;;;; package.lisp

(defpackage #:sdl2
  (:use #:cl #:cffi)
  (:export :sys-video
           :sys-audio
           :sys-timer
           :sys-joystick
           :sys-gamecontroller ; implies joystick
           :noparachute
           :everything
           :init
           :quit))

(defpackage #:sdl2-ffi)
