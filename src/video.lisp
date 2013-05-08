(in-package #:sdl2)

(defbitfield-from-cenum (sdl-window-flags
                         sdl2-ffi:sdl-windowflags
                         "SDL-WINDOW-")
  (:centered #x0))

(defbitfield-from-cenum (sdl-gl-attr
                         sdl2-ffi:sdl-glattr
                         "SDL-GL-"))

(defun windowpos-undefined (&optional (display 0))
  (logior sdl2-ffi:+sdl-windowpos-undefined-mask+
          display))

(defun windowpos-centered (&optional (display 0))
  (logior sdl2-ffi:+sdl-windowpos-centered-mask+
          display))

(defun windowpos-from-coord (n)
  (case n
    (:undefined (windowpos-undefined))
    (:centered (windowpos-centered))
    (t n)))

(defun create-window (&key
                        (title "SDL2 Window")
                        (x :centered) (y :centered)
                        (w 800) (h 600) flags)
  (let ((window-flags (foreign-bitfield-value 'sdl-window-flags flags))
        (x (windowpos-from-coord x))
        (y (windowpos-from-coord y)))
    (check-null (sdl2-ffi:sdl-createwindow title x y w h window-flags))))

(defun destroy-window (win)
  (sdl2-ffi:sdl-destroywindow win))

(defun enable-screensaver ()
  (sdl2-ffi:sdl-enablescreensaver))

(defun disable-screensaver ()
  (sdl2-ffi:sdl-disablescreensaver))

(defun gl-create-context (win)
  (check-null (sdl2-ffi:sdl-gl-createcontext win)))

(defun gl-delete-context (gl-context)
  (sdl2-ffi:sdl-gl-deletecontext gl-context))

(defun gl-extension-supported-p (extension)
  (sdl2-ffi:sdl-gl-extensionsupported extension))

(defun gl-make-current (win gl-context)
  (check-rc (sdl2-ffi:sdl-gl-makecurrent win gl-context)))

(defun gl-get-swap-interval ()
  (let ((rc (sdl2-ffi:sdl-gl-getswapinterval)))
    ;; Doing this funky thing because you want to get the value back from
    ;; the call, and still want to have the sdl-error condition if it's
    ;; not supported.
    (check-rc rc)
    rc))

(defun gl-set-swapinterval (interval)
  "0 for immediate updates, 1 for updates synchronized with the vertical retrace"
  (check-rc sdl2-ffi:sdl-gl-setswapinterval interval))

(defun gl-swap-window (win)
  (sdl2-ffi:sdl-gl-swapwindow win))

(defun gl-get-attr (attr)
  (with-foreign-object (value :int)
    (check-rc (sdl2-ffi:sdl-gl-getattribute
               (foreign-bitfield-value 'sdl-gl-attr (list attr))
               value))
    (mem-ref value :int)))

(defun gl-get-attrs (&rest attrs)
  (mapcan #'list attrs (mapcar #'gl-get-attr attrs)))

(defun gl-set-attr (attr value)
  (check-rc (sdl2-ffi:sdl-gl-setattribute
             (foreign-bitfield-value 'sdl-gl-attr (list attr))
             value)))

(defun gl-set-attrs (&rest attr-plist)
  (loop for (attr value) on attr-plist by #'cddr
     do (gl-set-attr attr value)))
