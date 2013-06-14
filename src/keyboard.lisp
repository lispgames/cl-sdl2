(in-package #:sdl2)

(defbitfield-from-cenum (keymod sdl2-ffi::sdl-keymod "SDL-"))

(defmacro keysym-slot-value (sdl-keysym-ptr slot-name)
  `(foreign-slot-value ,sdl-keysym-ptr
                       ;; TODO why do I need this? I get an error
                       ;; if I use 'sdl2-ffi::sdl-keysym even though
                       ;; that's what this function returns
                       (foreign-slot-type 'sdl2-ffi::sdl-keyboardevent
                                          'sdl2-ffi::keysym)
                       ,slot-name))

(defun scancode-value (sdl-keysym-ptr)
  (keysym-slot-value sdl-keysym-ptr 'sdl2-ffi::scancode))

(defun sym-value (sdl-keysym-ptr)
  (keysym-slot-value sdl-keysym-ptr 'sdl2-ffi::sym))

(defun mod-value (sdl-keysym-ptr)
  (keysym-slot-value sdl-keysym-ptr 'sdl2-ffi::mod))

(defun mod-keywords (value)
  ;; using rest here because :kmod-none is always first
  ;; TODO is it possible that I'm using it incorrectly?
  (rest (foreign-bitfield-symbols 'keymod value)))

(defun mod-value-p (value &rest keywords)
  (let ((bitfield-value (foreign-bitfield-value 'keymod keywords)))
    (not (= 0 (logand bitfield-value value)))))

;; TODO need to be able to unpack a keysym struct in one go
;;(defmacro with-keysym-slot-values ())

