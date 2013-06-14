(in-package #:sdl2)

(defwrapper sdl-gamecontroller ())

(defun game-controller-p (device-index)
  (= 1 (sdl2-ffi::sdl-isgamecontroller device-index)))

(defun game-controller-name-for-index (device-index)
  (sdl2-ffi::sdl-gamecontrollernameforindex device-index))

(defun game-controller-open (device-index)
  (sdl-collect
   (%make-sdl-gamecontroller
    :ptr (check-null (sdl2-ffi::sdl-gamecontrolleropen device-index)))
   #'sdl2-ffi::sdl-gamecontrollerclose))

(defun game-controller-close (gamecontroller)
  (sdl2-ffi::sdl-gamecontrollerclose (sdl-ptr gamecontroller))
  (sdl-cancel-collect gamecontroller))

(defun game-controller-attached-p (gamecontroller)
  (= 1 (sdl2-ffi::sdl-gamecontrollergetattached (sdl-ptr gamecontroller))))

(defun game-controller-add-mapping (mapping-string)
  (sdl2-ffi::sdl-gamecontrolleraddmapping mapping-string))

;; TODO add support to build mapping string from lisp data
