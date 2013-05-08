;;;; package.lisp

(defpackage #:sdl2
  (:use #:cl #:alexandria #:cffi)
  (:export :sys-video
           :sys-audio
           :sys-timer
           :sys-joystick
           :sys-gamecontroller ; implies joystick
           :noparachute
           :everything
           :init
           :quit
           :create-window
           :destroy-window))

(defpackage #:sdl2-ffi)
