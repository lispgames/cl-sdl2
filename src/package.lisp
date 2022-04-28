;;;; package.lisp

(push :sdl2 *features*)

(uiop:define-package #:sdl2-ffi (:use))
(uiop:define-package #:sdl2-ffi.accessors (:use))
(uiop:define-package #:sdl2-ffi.functions
  (:use)
  (:export #:sdl-quit))

(defpackage #:sdl2
  (:use #:cl
        #:alexandria
        #:autowrap.minimal
        #:plus-c
        #:sdl2-ffi.accessors
        #:sdl2-ffi.functions
        #:trivial-channels)
  (:import-from
   #:cffi
   #:mem-ref
   #:with-foreign-objects
   #:with-foreign-object
   #:foreign-alloc
   #:foreign-free
   #:null-pointer-p)
  (:shadow #:sdl-error)
  (:export
   #:init
   #:init*
   #:quit
   #:quit*
   #:was-init
   #:with-init
   #:in-main-thread
   #:version
   #:version-wrapped
   #:make-this-thread-main

   ;; hints.lisp
   #:get-hint
   #:set-hint

   ;; video.lisp
   #:get-num-video-drivers
   #:get-video-driver
   #:get-current-video-driver
   #:get-num-video-displays
   #:get-display-name
   #:get-num-display-modes
   #:get-current-display-mode
   #:get-display-mode
   #:get-display-bounds
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
   #:get-window-aspect-ratio
   #:get-window-surface
   #:get-window-position
   #:get-window-flags
   #:get-window-pixel-format
   #:get-window-id
   #:get-window-display-index
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
   #:expand-handler

   ;; keyboard.lisp
   #:keysym-slot-value
   #:key-down-p
   #:key-up-p
   #:scancode-value
   #:scancode
   #:scancode-symbol
   #:scancode-key-to-value
   #:get-mod-state
   #:mod-value
   #:sym-value
   #:scancode=
   #:mod-keywords
   #:mod-value-p
   #:mod-key-state-p
   #:keyboard-state-p
   #:get-key-from-scancode
   #:get-key-name
   #:scancode-name
   #:scancode-key-name
   #:start-text-input
   #:stop-text-input

   ;; mouse.lisp
   #:warp-mouse-in-window
   #:hide-cursor
   #:show-cursor
   #:set-relative-mouse-mode
   #:relative-mouse-mode-p
   #:toggle-relative-mouse-mode
   #:mouse-state
   #:mouse-state-p
   #:get-global-mouse-state
   #:global-mouse-state-p

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
   #:game-controller-from-instance-id
   #:game-controller-add-mappings-from-file
   #:game-controller-name

   ;; rect.lisp
   #:copy-f-rect
   #:copy-into-f-rect
   #:copy-into-point
   #:copy-into-rect
   #:copy-point
   #:copy-rect
   #:f-rect-empty
   #:f-rect-equals
   #:f-rect-height
   #:f-rect-width
   #:f-rect-x
   #:f-rect-y
   #:f-rects*
   #:free-f-rect
   #:free-point
   #:free-rect
   #:has-intersect
   #:intersect-rect
   #:make-f-rect
   #:make-point
   #:make-rect
   #:point-x
   #:point-y
   #:points*
   #:rect-empty
   #:rect-equals
   #:rect-height
   #:rect-width
   #:rect-x
   #:rect-y
   #:rects*
   #:union-rect
   #:with-f-rects
   #:with-points
   #:with-rects

   ;; render.lisp
   #:render-set-viewport
   #:render-get-viewport
   #:render-clear
   #:render-draw-line
   #:render-draw-lines
   #:render-draw-point
   #:render-draw-points
   #:render-draw-rect
   #:render-draw-rects
   #:render-fill-rect
   #:render-fill-rect-f
   #:render-fill-rects
   #:render-fill-rects-f
   #:set-render-draw-color
   #:get-render-draw-color
   #:set-texture-blend-mode
   #:set-render-draw-blend-mode
   #:set-render-target
   #:get-render-target
   #:render-copy
   #:render-copy-ex
   #:render-copy-ex-f
   #:render-copy-f
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
   #:get-renderer-max-texture-size
   #:get-renderer-output-size
   #:create-texture
   #:create-texture-from-surface
   #:set-texture-color-mod
   #:get-texture-color-mod
   #:set-texture-alpha-mod
   #:get-texture-alpha-mod
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
   #:create-rgb-surface-with-format-from
   #:free-surface
   #:load-bmp
   #:convert-surface
   #:convert-surface-format
   #:blit-surface
   #:blit-scaled
   #:fill-rect
   #:set-color-key
   #:get-color-key
   #:set-alpha-mod
   #:get-alpha-mod
   #:set-color-mod
   #:get-color-mod

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

   ;; syswm.lisp
   #:get-window-wm-info

   ;;rwops.lisp
   :rw-from-file
   :rw-close

   ;; Utility
   #:sdl-ptr

   ;; Conditions
   #:sdl-continue
   #:sdl-quit

   ;; constants
   #:+pixelformat-unknown+
   #:+pixelformat-index1lsb+
   #:+pixelformat-index1msb+
   #:+pixelformat-index14lsb+
   #:+pixelformat-index14msb+
   #:+pixelformat-index8+
   #:+pixelformat-rgb332+
   #:+pixelformat-rgb444+
   #:+pixelformat-rgb555+
   #:+pixelformat-bgr555+
   #:+pixelformat-argb4444+
   #:+pixelformat-rgba4444+
   #:+pixelformat-abgr4444+
   #:+pixelformat-argb4444+
   #:+pixelformat-abgr1555+
   #:+pixelformat-bgra5551+
   #:+pixelformat-rgb565+
   #:+pixelformat-bgr565+
   #:+pixelformat-rgb24+
   #:+pixelformat-bgr24+
   #:+pixelformat-rgb888+
   #:+pixelformat-rgbx8888+
   #:+pixelformat-bgr888+
   #:+pixelformat-bgrx8888+
   #:+pixelformat-argb8888+
   #:+pixelformat-rgba8888+
   #:+pixelformat-bgr8888+
   #:+pixelformat-bgra8888+
   #:+pixelformat-rgb2101010+
   #:+pixelformat-rgba32+
   #:+pixelformat-argb32+
   #:+pixelformat-bgra32+
   #:+pixelformat-abgr32+
   #:+pixelformat-yv12+
   #:+pixelformat-iyuv+
   #:+pixelformat-yuy2+
   #:+pixelformat-uyvy+
   #:+pixelformat-yvyu+
   #:+pixelformat-nv12+
   #:+pixelformat-nv21+
   #:+pixeltype-unknown+
   #:+pixeltype-index1+
   #:+pixeltype-index4+
   #:+pixeltype-index8+
   #:+pixeltype-packed8+
   #:+pixeltype-packed16+
   #:+pixeltype-packed32+
   #:+pixeltype-arrayu8+
   #:+pixeltype-arrayu16+
   #:+pixeltype-arrayu32+
   #:+pixeltype-arrayf16+
   #:+pixeltype-arrayf32+
   #:+bitmaporder-none+
   #:+bitmaporder-4321+
   #:+bitmaporder-1234+
   #:+packedorder-none+
   #:+packedorder-xrgb+
   #:+packedorder-rgbx+
   #:+packedorder-argb+
   #:+packedorder-rgba+
   #:+packedorder-xbgr+
   #:+packedorder-bgrx+
   #:+packedorder-abgr+
   #:+packedorder-bgra+
   #:+arrayorder-none+
   #:+arrayorder-rgb+
   #:+arrayorder-rgba+
   #:+arrayorder-argb+
   #:+arrayorder-bgr+
   #:+arrayorder-bgra+
   #:+arrayorder-abgr+
   #:+packedlayout-none+
   #:+packedlayout-332+
   #:+packedlayout-4444+
   #:+packedlayout-1555+
   #:+packedlayout-5551+
   #:+packedlayout-565+
   #:+packedlayout-8888+
   #:+packedlayout-2101010+
   #:+packedlayout-1010102+))

(defpackage #:sdl2-examples
  (:use #:cl
        #:alexandria
        #:cffi)
  (:export #:basic-test
           #:renderer-test))
