;;;; package.lisp

(defpackage #:sdl2
  (:use #:cl #:alexandria #:cffi)
  (:export ;; API
           #:init
           #:quit
           #:create-window
           #:destroy-window
           #:enable-screensaver
           #:disable-screensaver
           #:gl-create-context
           #:gl-delete-context
           #:gl-extension-supported-p
           #:gl-make-current
           #:gl-get-swap-interval
           #:gl-set-swap-interval
           #:gl-swap-window
           #:gl-get-attr
           #:gl-get-attrs
           #:gl-set-attr
           #:gl-set-attrs

           ;; Utility
           #:sdl-ptr

           ;; Conditions
           #:sdl-error))

(defpackage #:sdl2-ffi)
