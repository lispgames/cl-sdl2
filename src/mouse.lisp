(in-package #:sdl2)


(defmacro warp-mouse-in-window (win x y)
  "Use this function to move the mouse to the given position within the window."
  `(sdl-warp-mouse-in-window win x y))

(defun hide-cursor ()
  (check-rc (sdl-show-cursor nil)))

(defun show-cursor ()
  (check-rc (sdl-show-cursor t)))

(defun set-relative-mouse-mode (enabled)
  (check-rc (sdl-set-relative-mouse-mode enabled)))

(defun relative-mouse-mode-p ()
  (sdl-true-p (sdl-get-relative-mouse-mode)))

(defun toggle-relative-mouse-mode ()
  (set-relative-mouse-mode (not (relative-mouse-mode-p))))
