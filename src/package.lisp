;;;; package.lisp

(push :sdl2 *features*)

(defpackage #:sdl2-ffi)
(defpackage #:sdl2-ffi.accessors)
(defpackage #:sdl2-ffi.functions
  (:export #:sdl-quit))

(defpackage #:sdl2
  (:use #:cl #:alexandria #:autowrap.minimal #:plus-c
        #:sdl2-ffi.accessors #:sdl2-ffi.functions
        #:trivial-channels)
  (:import-from :cffi
                #:mem-ref #:with-foreign-objects #:with-foreign-object
                #:foreign-alloc #:foreign-free #:null-pointer-p)
  (:shadow #:sdl-error)
  (:export ;; API
           #:init
           #:quit
           #:with-init
           #:in-main-thread
           #:version
           #:version-wrapped
           #:make-this-thread-main

           ;; video.lisp
           #:get-num-video-drivers
           #:get-video-driver
           #:get-current-video-driver
           #:get-num-video-displays
           #:get-display-name
           #:get-num-display-modes
           #:get-current-display-mode
           #:get-display-mode
           #:windowpos-undefined
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
           #:get-window-surface
           #:get-window-position
           #:get-window-flags
           #:get-window-id
           #:enable-screensaver
           #:disable-screensaver
           #:screensaver-enabled-p
           #:gl-create-context
           #:gl-delete-context
           #:with-gl-context
           #:gl-extension-supported-p
           #:gl-make-current
           #:gl-get-swap-interval
           #:gl-set-swap-interval
           #:gl-swap-window
           #:gl-get-attr
           #:gl-get-attrs
           #:gl-set-attr
           #:gl-set-attrs
           #:gl-get-proc-address
           #:with-everything

           ;; events.lisp
           #:new-event
           #:free-event
           #:register-user-event-type
           #:with-sdl-event
           #:get-event-type
           #:pump-events
           #:push-event
           #:push-user-event
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
           #:keyboard-state-p
           #:get-key-from-scancode
           #:get-key-name
           #:scancode-name

           ;; mouse.lisp
           #:warp-mouse-in-window
           #:hide-cursor
           #:show-cursor
           #:set-relative-mouse-mode
           #:relative-mouse-mode-p
           #:toggle-relative-mouse-mode
           #:mouse-state
           #:mouse-state-p

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
           #:joystick-instance-id

           ;; gamecontroller.lisp
           #:game-controller-p
           #:game-controller-name-for-index
           #:game-controller-open
           #:game-controller-close
           #:game-controller-attached-p
           #:game-controller-add-mapping
           #:game-controller-get-joystick
           #:game-controller-instance-id

           ;; rect.lisp
           #:make-point
           #:copy-point
           #:copy-into-point
           #:free-point
           #:with-points
           #:points*
           #:make-rect
           #:copy-rect
           #:copy-into-rect
           #:free-rect
           #:with-rects
           #:rects*
           #:rect-empty
           #:rect-equals
           #:has-intersect
           #:intersect-rect
           #:union-rect

           ;; render.lisp
           #:render-clear
           #:render-draw-line
           #:render-draw-lines
           #:render-draw-points
           #:render-draw-rect
           #:render-draw-rects
           #:render-fill-rect
           #:render-fill-rects
           #:set-render-draw-color
           #:set-texture-blend-mode
           #:set-render-draw-blend-mode
           #:render-copy
           #:render-copy-ex
           #:render-present
           #:update-texture
           #:get-num-render-drivers
           #:get-render-driver-info
           #:create-window-and-renderer
           #:create-renderer
           #:create-software-renderer
           #:destroy-renderer
           #:get-renderer
           #:get-renderer-info
           ;#:get-renderer-output-size
           #:create-texture
	   #:create-texture-from-surface
	   #:query-texture
	   #:texture-width
	   #:texture-height
           #:destroy-texture
           #:lock-texture
           #:unlock-texture
           #:gl-bind-texture
           #:gl-unbind-texture
           #:with-renderer

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

           ;; pixels.lisp
           #:map-rgb
           #:get-pixel-format-name

           ;; surface.lisp
           #:surface-width
           #:surface-height
           #:surface-pixels
           #:surface-pitch
           #:surface-format
           #:surface-format-format
           #:create-rgb-surface
           #:create-rgb-surface-from
           #:free-surface
           #:load-bmp
           #:convert-surface
           #:convert-surface-format
           #:blit-surface
           #:blit-scaled
           #:fill-rect

           ;; timer.lisp
           #:delay
           #:get-ticks
           #:get-performance-counter
           #:get-performance-frequency
           #:add-timer
           #:remove-timer

           ;; platform.lisp
           #:platform
           #:cpu-cache-line-size
           #:cpu-count
           #:mmx-p #:alti-vec-p #:rdtsc-p
           #:sse-p #:sse2-p #:sse3-p #:sse41-p #:sse42-p
           #:power-info

           ;;rwops.lisp
           :rw-from-file
           :rw-close

           ;; Utility
           #:sdl-ptr

           ;; Conditions
           #:sdl-continue
           #:sdl-quit))

(defpackage #:sdl2-examples
  (:use #:cl #:alexandria #:cffi)
  (:export #:basic-test #:renderer-test))
