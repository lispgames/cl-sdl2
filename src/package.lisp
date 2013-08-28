;;;; package.lisp

(defpackage #:sdl2-ffi)
(defpackage #:sdl2-ffi.accessors)
(defpackage #:sdl2-ffi.functions)

(defpackage #:sdl2
  (:use #:cl #:alexandria #:autowrap.minimal
        #:sdl2-ffi.accessors #:sdl2-ffi.functions)
  (:import-from :cffi
                #:mem-ref #:with-foreign-objects #:with-foreign-object
                #:foreign-alloc #:foreign-free #:null-pointer-p)
  (:shadow #:sdl-error)
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
           #:warp-mouse-in-window
           #:hide-cursor
           #:show-cursor
           #:toggle-cursor
           #:relative-mouse-mode
           #:absolute-mouse-mode
           #:toggle-relative-mouse-mode
           #:joystick-count
           #:joystick-open
           #:joystick-close
           #:joystick-name-for-index
           #:joystick-name
           #:joystick-hat-count
           #:joystick-axis-count
           #:joystick-ball-count
           #:joystick-button-count
           #:game-controller-p
           #:game-controller-name-for-index
           #:game-controller-open
           #:game-controller-close
           #:game-controller-attached-p
           #:game-controller-add-mapping
           #:key-down-p
           #:key-up-p
           #:scancode-value
           #:mod-value
           #:sym-value
           #:scancode=
           #:make-point
           #:copy-point
           #:copy-into-point
           #:free-point
           #:with-points
           #:make-rect
           #:copy-rect
           #:copy-into-rect
           #:free-rect
           #:with-rects
           #:rect-empty
           #:rect-equals
           #:has-intersect
           #:intersect-rect
           #:union-rect

           ;; Utility
           #:sdl-ptr

           ;; Conditions
           #:sdl-error))

(defpackage #:sdl2-examples
  (:use #:cl #:alexandria #:cffi)
  (:export #:basic-test))
