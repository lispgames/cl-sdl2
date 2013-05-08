;;;; package.lisp

(defpackage #:sdl2
  (:use #:cl #:alexandria #:cffi)
  (:export ;; API
           #:init
           #:quit
           #:create-window
           #:destroy-window

           ;; Conditions
           #:sdl-error))

(defpackage #:sdl2-ffi)
