(in-package #:sdl2)

(defbitfield-from-cenum (sdl-window-flags
                         sdl2-ffi:sdl-windowflags
                         "SDL-WINDOW-")
  (:centered #x0))

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

