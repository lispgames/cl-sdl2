(in-package #:sdl2)


(defun warp-mouse-in-window (win x y)
  (sdl2-ffi::sdl-warpmouseinwindow (sdl-ptr win) x y))

(defun hide-cursor ()
  (check-rc (sdl2-ffi::sdl-showcursor 0)))

(defun show-cursor ()
  (check-rc (sdl2-ffi::sdl-showcursor 1)))

(defun toggle-cursor ()
  (let ((cursor-state (check-rc (sdl2-ffi::sdl-showcursor -1))))
    (sdl2-ffi::sdl-showcursor (logxor 1 cursor-state))))

(defun relative-mouse-mode ()
  (check-rc (sdl2-ffi::sdl-setrelativemousemode 1)))

(defun absolute-mouse-mode ()
  (check-rc (sdl2-ffi::sdl-setrelativemousemode 0)))

(defun toggle-relative-mouse-mode ()
  (let ((relative-state (check-rc (sdl2-ffi::sdl-getrelativemousemode))))
    (sdl2-ffi::sdl-setrelativemousemode (logxor 1 relative-state))))
