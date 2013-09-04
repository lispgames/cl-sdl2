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

           ;; video.lisp
           #:windowpos-underfined
           #:windowpos-centered
           #:windowpos-from-coord
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
           #:set-window-fullscreen
           #:set-window-size
           #:set-window-position
           #:get-window-title
           #:get-window-size
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

           ;; events.lisp
           #:new-event
           #:free-event
           #:with-sdl-event
           #:pump-events
           #:push-event
           #:push-quit-event
           #:next-event
           #:with-event-loop

           ;; keyboard.lisp
           #:keysym-slot-value
           #:key-down-p
           #:key-up-p
           #:scancode-value
           #:scancode
           #:mod-value
           #:sym-value
           #:scancode=
           #:mod-keywords
           #:mod-value-p

           ;; mouse.lisp
           #:warp-mouse-in-window
           #:hide-cursor
           #:show-cursor
           #:set-relative-mouse-mode
           #:relative-mouse-mode-p
           #:toggle-relative-mouse-mode

           ;; joystick.lisp
           #:joystick-update
           #:joystick-count
           #:joystick-opened-p
           #:joystick-open
           #:joystick-close
           #:joystick-name-for-index
           #:joystick-name
           #:joystick-hat-count
           #:joystick-axis-count
           #:joystick-ball-count
           #:joystick-button-count

           ;; gamecontroller.lisp
           #:game-controller-p
           #:game-controller-name-for-index
           #:game-controller-open
           #:game-controller-close
           #:game-controller-attached-p
           #:game-controller-add-mapping
           #:game-controller-get-joystick

           ;; rect.lisp
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

           ;; haptic.lisp
           #:joystick-is-haptic-p
           #:mouse-is-haptic-p
           #:haptic-open
           #:haptic-open-from-joystick
           #:haptic-open-from-mouse
           #:haptic-close
           #:haptic-index
           #:haptic-opened-p
           #:rumble-supported-p
           #:rumble-init
           #:rumble-play
           #:rumble-stop

           ;; timer.lisp
           #:delay
           #:get-ticks
           #:get-performance-counter
           #:get-performance-frequency
           #:add-timer
           #:remove-timer

           ;; Utility
           #:sdl-ptr

           ;; Conditions
           #:sdl-error))

(defpackage #:sdl2-examples
  (:use #:cl #:alexandria #:cffi)
  (:export #:basic-test))
