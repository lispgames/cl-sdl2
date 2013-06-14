;;;; package.lisp

(defpackage #:sdl2
  (:use #:cl #:alexandria #:cffi)
  (:export ;; API
           #:init
           #:quit
           #:with-init
           #:create-window
           #:destroy-window
           #:with-window
           #:hide-window
           #:show-window
           #:maximize-window
           #:minimize-window
           #:raise-window
           #:restore-window
           #:update-window
           #:set-window-title
           #:get-window-title
           #:set-window-fullscreen
           #:set-window-size
           #:get-window-size
           #:set-window-position
           #:get-window-position
           #:get-window-flags
           #:enable-screensaver
           #:disable-screensaver
           #:screensaver-enabled-p
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
           #:new-event
           #:free-event
           #:with-sdl-event
           #:pump-events
           #:push-event
           #:next-event
           #:with-event-loop
           #:keysym-slot-value
           #:scancode-value
           #:sym-value
           #:mod-value
           #:mod-keywords
           #:mod-value-p
           #:joystick-count
           #:joystick-open
           #:joystick-close
           #:joystick-name-for-index
           #:joystick-name
           #:joystick-hat-count
           #:joystick-axis-count
           #:joystick-ball-count
           #:joystick-button-count

           ;; Utility
           #:sdl-ptr

           ;; Conditions
           #:sdl-error))

(defpackage #:sdl2-ffi)

(defpackage #:sdl2-examples
  (:use #:cl #:alexandria #:cffi)
  (:export #:basic-test))
