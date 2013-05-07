
;;;; package.lisp

(defpackage #:sdl2
  (:use #:cl #:cffi))

(in-package #:sdl2)

(define-foreign-library libsdl2
  (t (:default "libSDL2")))

(use-foreign-library libsdl2)

(load "sdl2-cffi.lisp")

