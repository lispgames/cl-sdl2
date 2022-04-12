(in-package #:sdl2)

(alexandria:define-constant +accelerometer-as-joystick+
  sdl2-ffi:+sdl-hint-accelerometer-as-joystick+
  :test #'string=)

(alexandria:define-constant +allow-alt-tab-while-grabbed+
  sdl2-ffi:+sdl-hint-allow-alt-tab-while-grabbed+
  :test #'string=)

(alexandria:define-constant +allow-top-most+
  sdl2-ffi:+sdl-hint-allow-topmost+
  :test #'string=)

(alexandria:define-constant +android-apk-expansion-main-file-version+
  sdl2-ffi:+sdl-hint-android-apk-expansion-main-file-version+
  :test #'string=)

(alexandria:define-constant +android-apk-expansion-patch-file-version+
  sdl2-ffi:+sdl-hint-android-apk-expansion-patch-file-version+
  :test #'string=)

(alexandria:define-constant +android-block-on-pause+
  sdl2-ffi:+sdl-hint-android-block-on-pause+
  :test #'string=)

(alexandria:define-constant +android-block-on-pause-pause-audio+
  sdl2-ffi:+sdl-hint-android-block-on-pause-pauseaudio+
  :test #'string=)

(alexandria:define-constant +android-trap-back-button+
  sdl2-ffi:+sdl-hint-android-trap-back-button+
  :test #'string=)

(alexandria:define-constant +app-name+ sdl2-ffi:+sdl-hint-app-name+ :test #'string=)

(alexandria:define-constant +apple-tv-controller-ui-events+
  sdl2-ffi:+sdl-hint-apple-tv-controller-ui-events+
  :test #'string=)

(alexandria:define-constant +apple-tv-remote-allow-rotation+
  sdl2-ffi:+sdl-hint-apple-tv-remote-allow-rotation+
  :test #'string=)

(alexandria:define-constant +audio-category+ sdl2-ffi:+sdl-hint-audio-category+ :test #'string=)

(alexandria:define-constant +audio-device-app-name+
  sdl2-ffi:+sdl-hint-audio-device-app-name+
  :test #'string=)

(alexandria:define-constant +audio-device-stream-name+
  sdl2-ffi:+sdl-hint-audio-device-stream-name+
  :test #'string=)

(alexandria:define-constant +audio-device-stream-role+
  sdl2-ffi:+sdl-hint-audio-device-stream-role+
  :test #'string=)

(alexandria:define-constant +audio-include-monitors+
  sdl2-ffi:+sdl-hint-audio-include-monitors+
  :test #'string=)

(alexandria:define-constant +audio-resampling-mode+
  sdl2-ffi:+sdl-hint-audio-resampling-mode+
  :test #'string=)

(alexandria:define-constant +auto-update-joysticks+
  sdl2-ffi:+sdl-hint-auto-update-joysticks+
  :test #'string=)

(alexandria:define-constant +auto-update-sensors+
  sdl2-ffi:+sdl-hint-auto-update-sensors+
  :test #'string=)

(alexandria:define-constant +bmp-save-legacy-format+
  sdl2-ffi:+sdl-hint-bmp-save-legacy-format+
  :test #'string=)

(alexandria:define-constant +display-usable-bounds+
  sdl2-ffi:+sdl-hint-display-usable-bounds+
  :test #'string=)

(alexandria:define-constant +emscripten-asyncify+
  sdl2-ffi:+sdl-hint-emscripten-asyncify+
  :test #'string=)

(alexandria:define-constant +emscripten-keyboard-element+
  sdl2-ffi:+sdl-hint-emscripten-keyboard-element+
  :test #'string=)

(alexandria:define-constant +enable-steam-controllers+
  sdl2-ffi:+sdl-hint-enable-steam-controllers+
  :test #'string=)

(alexandria:define-constant +event-logging+ sdl2-ffi:+sdl-hint-event-logging+ :test #'string=)

(alexandria:define-constant +framebuffer-acceleration+
  sdl2-ffi:+sdl-hint-framebuffer-acceleration+
  :test #'string=)

(alexandria:define-constant +gamecontroller-config+
  sdl2-ffi:+sdl-hint-gamecontrollerconfig+
  :test #'string=)

(alexandria:define-constant +gamecontroller-config-file+
  sdl2-ffi:+sdl-hint-gamecontrollerconfig-file+
  :test #'string=)

(alexandria:define-constant +gamecontroller-type+
  sdl2-ffi:+sdl-hint-gamecontrollertype+
  :test #'string=)

(alexandria:define-constant +gamecontroller-ignore-devices+
  sdl2-ffi:+sdl-hint-gamecontroller-ignore-devices+
  :test #'string=)

(alexandria:define-constant +gamecontroller-ignore-devices-except+
  sdl2-ffi:+sdl-hint-gamecontroller-ignore-devices-except+
  :test #'string=)

(alexandria:define-constant +gamecontroller-use-button-labels+
  sdl2-ffi:+sdl-hint-gamecontroller-use-button-labels+
  :test #'string=)

(alexandria:define-constant +grab-keyboard+ sdl2-ffi:+sdl-hint-grab-keyboard+ :test #'string=)

(alexandria:define-constant +idle-timer-disabled+
  sdl2-ffi:+sdl-hint-idle-timer-disabled+
  :test #'string=)

(alexandria:define-constant +ime-internal-editing+
  sdl2-ffi:+sdl-hint-ime-internal-editing+
  :test #'string=)

(alexandria:define-constant +ime-show-ui+ sdl2-ffi:+sdl-hint-ime-show-ui+ :test #'string=)

(alexandria:define-constant +ios-hide-home-indicator+
  sdl2-ffi:+sdl-hint-ios-hide-home-indicator+
  :test #'string=)

(alexandria:define-constant +joystick-allow-background-events+
  sdl2-ffi:+sdl-hint-joystick-allow-background-events+
  :test #'string=)

(alexandria:define-constant +joystick-device+ sdl2-ffi:+sdl-hint-joystick-device+ :test #'string=)

(alexandria:define-constant +joystick-hidapi+ sdl2-ffi:+sdl-hint-joystick-hidapi+ :test #'string=)

(alexandria:define-constant +joystick-hidapi-gamecube+
  sdl2-ffi:+sdl-hint-joystick-hidapi-gamecube+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-joy-cons+
  sdl2-ffi:+sdl-hint-joystick-hidapi-joy-cons+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-luna+
  sdl2-ffi:+sdl-hint-joystick-hidapi-luna+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-ps4+
  sdl2-ffi:+sdl-hint-joystick-hidapi-ps4+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-ps4-rumble+
  sdl2-ffi:+sdl-hint-joystick-hidapi-ps4-rumble+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-ps5+
  sdl2-ffi:+sdl-hint-joystick-hidapi-ps5+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-ps5-rumble+
  sdl2-ffi:+sdl-hint-joystick-hidapi-ps5-rumble+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-stadia+
  sdl2-ffi:+sdl-hint-joystick-hidapi-stadia+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-steam+
  sdl2-ffi:+sdl-hint-joystick-hidapi-steam+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-switch+
  sdl2-ffi:+sdl-hint-joystick-hidapi-switch+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-switch-home-led+
  sdl2-ffi:+sdl-hint-joystick-hidapi-switch-home-led+
  :test #'string=)

(alexandria:define-constant +joystick-hidapi-xbox+
  sdl2-ffi:+sdl-hint-joystick-hidapi-xbox+
  :test #'string=)

(alexandria:define-constant +joystick-raw-input+
  sdl2-ffi:+sdl-hint-joystick-rawinput+
  :test #'string=)

(alexandria:define-constant +joystick-raw-input-correlate-xinput+
  sdl2-ffi:+sdl-hint-joystick-rawinput-correlate-xinput+
  :test #'string=)

(alexandria:define-constant +joystick-thread+ sdl2-ffi:+sdl-hint-joystick-thread+ :test #'string=)

(alexandria:define-constant +kmsdrm-require-drm-master+
  sdl2-ffi:+sdl-hint-kmsdrm-require-drm-master+
  :test #'string=)

(alexandria:define-constant +linux-joystick-classic+
  sdl2-ffi:+sdl-hint-linux-joystick-classic+
  :test #'string=)

(alexandria:define-constant +linux-joystick-deadzones+
  sdl2-ffi:+sdl-hint-linux-joystick-deadzones+
  :test #'string=)

(alexandria:define-constant +mac-background-app+
  sdl2-ffi:+sdl-hint-mac-background-app+
  :test #'string=)

(alexandria:define-constant +mac-ctrl-click-emulate-right-click+
  sdl2-ffi:+sdl-hint-mac-ctrl-click-emulate-right-click+
  :test #'string=)

(alexandria:define-constant +mouse-double-click-radius+
  sdl2-ffi:+sdl-hint-mouse-double-click-radius+
  :test #'string=)

(alexandria:define-constant +mouse-double-click-time+
  sdl2-ffi:+sdl-hint-mouse-double-click-time+
  :test #'string=)

(alexandria:define-constant +mouse-focus-click-through+
  sdl2-ffi:+sdl-hint-mouse-focus-clickthrough+
  :test #'string=)

(alexandria:define-constant +mouse-normal-speed-scale+
  sdl2-ffi:+sdl-hint-mouse-normal-speed-scale+
  :test #'string=)

(alexandria:define-constant +mouse-relative-mode-warp+
  sdl2-ffi:+sdl-hint-mouse-relative-mode-warp+
  :test #'string=)

(alexandria:define-constant +mouse-relative-scaling+
  sdl2-ffi:+sdl-hint-mouse-relative-scaling+
  :test #'string=)

(alexandria:define-constant +mouse-relative-speed-scale+
  sdl2-ffi:+sdl-hint-mouse-relative-speed-scale+
  :test #'string=)

(alexandria:define-constant +mouse-touch-events+
  sdl2-ffi:+sdl-hint-mouse-touch-events+
  :test #'string=)

(alexandria:define-constant +no-signal-handlers+
  sdl2-ffi:+sdl-hint-no-signal-handlers+
  :test #'string=)

(alexandria:define-constant +opengl-es-driver+
  sdl2-ffi:+sdl-hint-opengl-es-driver+
  :test #'string=)

(alexandria:define-constant +orientations+ sdl2-ffi:+sdl-hint-orientations+ :test #'string=)

(alexandria:define-constant +poll-sentinel+ sdl2-ffi:+sdl-hint-poll-sentinel+ :test #'string=)

(alexandria:define-constant +qtwayland-context-orientation+
  sdl2-ffi:+sdl-hint-qtwayland-content-orientation+
  :test #'string=)

(alexandria:define-constant +qtwayland-window-flags+
  sdl2-ffi:+sdl-hint-qtwayland-window-flags+
  :test #'string=)

(alexandria:define-constant +render-batching+ sdl2-ffi:+sdl-hint-render-batching+ :test #'string=)

(alexandria:define-constant +render-line-method+
  sdl2-ffi:+sdl-hint-render-line-method+
  :test #'string=)

(alexandria:define-constant +render-direct3d11-debug+
  sdl2-ffi:+sdl-hint-render-direct3d11-debug+
  :test #'string=)

(alexandria:define-constant +render-direct3d-threadsafe+
  sdl2-ffi:+sdl-hint-render-direct3d-threadsafe+
  :test #'string=)

(alexandria:define-constant +render-driver+ sdl2-ffi:+sdl-hint-render-driver+ :test #'string=)

(alexandria:define-constant +render-logical-size-mode+
  sdl2-ffi:+sdl-hint-render-logical-size-mode+
  :test #'string=)

(alexandria:define-constant +render-opengl-shaders+
  sdl2-ffi:+sdl-hint-render-opengl-shaders+
  :test #'string=)

(alexandria:define-constant +render-scale-quality+
  sdl2-ffi:+sdl-hint-render-scale-quality+
  :test #'string=)

(alexandria:define-constant +render-vsync+ sdl2-ffi:+sdl-hint-render-vsync+ :test #'string=)

(alexandria:define-constant +return-key-hides-ime+
  sdl2-ffi:+sdl-hint-return-key-hides-ime+
  :test #'string=)

(alexandria:define-constant +rpi-video-layer+ sdl2-ffi:+sdl-hint-rpi-video-layer+ :test #'string=)

(alexandria:define-constant +screensaver-inhibit-activity-name+
  sdl2-ffi:+sdl-hint-screensaver-inhibit-activity-name+
  :test #'string=)

(alexandria:define-constant +thread-force-realtime-time-critical+
  sdl2-ffi:+sdl-hint-thread-force-realtime-time-critical+
  :test #'string=)

(alexandria:define-constant +thread-priority-policy+
  sdl2-ffi:+sdl-hint-thread-priority-policy+
  :test #'string=)

(alexandria:define-constant +thread-stack-size+
  sdl2-ffi:+sdl-hint-thread-stack-size+
  :test #'string=)

(alexandria:define-constant +timer-resolution+
  sdl2-ffi:+sdl-hint-timer-resolution+
  :test #'string=)

(alexandria:define-constant +touch-mouse-events+
  sdl2-ffi:+sdl-hint-touch-mouse-events+
  :test #'string=)

(alexandria:define-constant +tv-remote-as-joystick+
  sdl2-ffi:+sdl-hint-tv-remote-as-joystick+
  :test #'string=)

(alexandria:define-constant +video-allow-screensaver+
  sdl2-ffi:+sdl-hint-video-allow-screensaver+
  :test #'string=)

(alexandria:define-constant +video-double-buffer+
  sdl2-ffi:+sdl-hint-video-double-buffer+
  :test #'string=)

(alexandria:define-constant +video-egl-allow-transparency+
  sdl2-ffi:+sdl-hint-video-egl-allow-transparency+
  :test #'string=)

(alexandria:define-constant +video-external-context+
  sdl2-ffi:+sdl-hint-video-external-context+
  :test #'string=)

(alexandria:define-constant +video-highdpi-disabled+
  sdl2-ffi:+sdl-hint-video-highdpi-disabled+
  :test #'string=)

(alexandria:define-constant +video-mac-fullscreen-spaces+
  sdl2-ffi:+sdl-hint-video-mac-fullscreen-spaces+
  :test #'string=)

(alexandria:define-constant +video-minimize-on-focus-loss+
  sdl2-ffi:+sdl-hint-video-minimize-on-focus-loss+
  :test #'string=)

(alexandria:define-constant +video-wayland-allow-libdecor+
  sdl2-ffi:+sdl-hint-video-wayland-allow-libdecor+
  :test #'string=)

(alexandria:define-constant +video-window-share-pixel-format+
  sdl2-ffi:+sdl-hint-video-window-share-pixel-format+
  :test #'string=)

(alexandria:define-constant +video-win-d3dcompiler+
  sdl2-ffi:+sdl-hint-video-win-d3dcompiler+
  :test #'string=)

(alexandria:define-constant +video-x11-net-wm-bypass-compositor+
  sdl2-ffi:+sdl-hint-video-x11-net-wm-bypass-compositor+
  :test #'string=)

(alexandria:define-constant +video-x11-net-wm-png+
  sdl2-ffi:+sdl-hint-video-x11-net-wm-ping+
  :test #'string=)

(alexandria:define-constant +video-x11-window-visualid+
  sdl2-ffi:+sdl-hint-video-x11-window-visualid+
  :test #'string=)

(alexandria:define-constant +video-x11-xinerama+
  sdl2-ffi:+sdl-hint-video-x11-xinerama+
  :test #'string=)

(alexandria:define-constant +video-x11-xrandr+
  sdl2-ffi:+sdl-hint-video-x11-xrandr+
  :test #'string=)

(alexandria:define-constant +wave-fact-chunk+ sdl2-ffi:+sdl-hint-wave-fact-chunk+ :test #'string=)

(alexandria:define-constant +wave-riff-chunk-size+
  sdl2-ffi:+sdl-hint-wave-riff-chunk-size+
  :test #'string=)

(alexandria:define-constant +wave-truncation+ sdl2-ffi:+sdl-hint-wave-truncation+ :test #'string=)

(alexandria:define-constant +windows-disable-thread-naming+
  sdl2-ffi:+sdl-hint-windows-disable-thread-naming+
  :test #'string=)

(alexandria:define-constant +windows-force-mutex-critical-sections+
  sdl2-ffi:+sdl-hint-windows-force-mutex-critical-sections+
  :test #'string=)

(alexandria:define-constant +windows-force-semaphore-kernel+
  sdl2-ffi:+sdl-hint-windows-force-semaphore-kernel+
  :test #'string=)

(alexandria:define-constant +windows-int-resource-icon+
  sdl2-ffi:+sdl-hint-windows-intresource-icon+
  :test #'string=)

(alexandria:define-constant +windows-int-resource-icon-small+
  sdl2-ffi:+sdl-hint-windows-intresource-icon-small+
  :test #'string=)

(alexandria:define-constant +windows-no-close-on-alt-f4+
  sdl2-ffi:+sdl-hint-windows-no-close-on-alt-f4+
  :test #'string=)

(alexandria:define-constant +windows-use-d3d9ex+
  sdl2-ffi:+sdl-hint-windows-use-d3d9ex+
  :test #'string=)

(alexandria:define-constant +window-frame-usable-while-cursor-hidden+
  sdl2-ffi:+sdl-hint-window-frame-usable-while-cursor-hidden+
  :test #'string=)

(alexandria:define-constant +window-no-activation-when-shown+
  sdl2-ffi:+sdl-hint-window-no-activation-when-shown+
  :test #'string=)

(alexandria:define-constant +winrt-handle-back-button+
  sdl2-ffi:+sdl-hint-winrt-handle-back-button+
  :test #'string=)

(alexandria:define-constant +winrt-privacy-policy-label+
  sdl2-ffi:+sdl-hint-winrt-privacy-policy-label+
  :test #'string=)

(alexandria:define-constant +winrt-privacy-policy-url+
  sdl2-ffi:+sdl-hint-winrt-privacy-policy-url+
  :test #'string=)

(alexandria:define-constant +x11-force-override-redirect+
  sdl2-ffi:+sdl-hint-x11-force-override-redirect+
  :test #'string=)

(alexandria:define-constant +xinput-enabled+
  sdl2-ffi:+sdl-hint-xinput-enabled+
  :test #'string=)

(alexandria:define-constant +xinput-use-old-joystick-mapping+
  sdl2-ffi:+sdl-hint-xinput-use-old-joystick-mapping+
  :test #'string=)

(defun set-hint (hint value)
  (sdl2-ffi.functions:sdl-set-hint
   hint
   (typecase value
     (null "0")
     (symbol (string-downcase (symbol-name value)))
     (string value)
     (t "1"))))

(defun get-hint (hint)
  (sdl2-ffi.functions:sdl-get-hint hint))
