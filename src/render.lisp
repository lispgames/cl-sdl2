(in-package :sdl2)

;;;; TODO
;;;; SDL_CreateWindowAndRenderer
;;;; SDL_GetRendererOutputSize

(defun make-renderer-info ()
  "Return an uninitialized SDL_RendererInfo structure."
  (sdl-collect (autowrap:alloc 'sdl2-ffi:sdl-renderer-info)))

(defmethod print-object ((rinfo sdl2-ffi:sdl-renderer-info) stream)
  (print-unreadable-object (rinfo stream :type t :identity t)
    (format stream "name ~S flags ~A num-texture-formats ~A texture-formats TBD max-texture-width ~A max-texture-height ~A"
            (cffi:foreign-string-to-lisp (sdl-renderer-info.name rinfo))
            (sdl-renderer-info.flags rinfo)
            (sdl-renderer-info.num-texture-formats rinfo)
            (sdl-renderer-info.max-texture-width rinfo)
            (sdl-renderer-info.max-texture-height rinfo))))

(defun free-render-info (rinfo)
  "Specifically free the SDL_RendererInfo structure which will do the right
thing with respect to the garbage collector. This is not required, but
may make garbage collection performance better if used in tight
SDL_RendererInfo allocating loops."
  (foreign-free (ptr rinfo))
  (sdl-cancel-collect rinfo)
  (autowrap:invalidate rinfo))

;;;; And now the wrapping of the SDL2 calls

;; Create the keywords for the SDL_RendererFLags enum.
(autowrap:define-bitmask-from-enum
    (sdl-renderer-flags sdl2-ffi:sdl-renderer-flags))

;; Create the keywords for the SDL_TextureModulate enum.
(autowrap:define-bitmask-from-enum
    (sdl-texture-modulate sdl2-ffi:sdl-texture-modulate))

;; Create the keywords for the SDL_RendererFlip enum.
(autowrap:define-bitmask-from-enum
    (sdl-renderer-flip sdl2-ffi:sdl-renderer-flip))

;; (autowrap:mask-apply 'sdl-renderer-flags '(:software :accelerated))

;; export
(defun get-num-render-drivers ()
  "Return the number of 2D rendering drivers available for the current
display."
  (sdl-get-num-render-drivers))

;; export
(defun get-render-driver-info (index)
  "Allocate and return a new SDL_RendererInfo structure and fill it
with information relating to the specific 2D rendering driver
specified in the index."
  (let ((rinfo (make-renderer-info)))
    (check-rc (sdl-get-render-driver-info index rinfo))
    rinfo))

;; export
;; TODO SDL_CreateWindowAndRenderer
;; Do the same trick as in create-render once I figure this out.
(defun create-window-and-renderer (width height flags)
  (niy "SDL_CreateWindowAndRenderer()"))

;; export
(defun create-renderer (window index &optional (flags 0))
  "Create a 2D rendering context for a window."
  (sdl-collect
   (check-null (sdl-create-renderer window index flags))
   (lambda (r) (sdl-destroy-renderer r))))

;; export
(defun create-software-renderer (surface)
  "Create and return a 2D software rendering context for the surface."
  (check-null (sdl-create-softeware-renderer surface)))

;; export
(defun get-renderer (window)
  "Return NIL if there is no renderer associated with the window, or otherwise
the SDL_Renderer structure."
  (let ((renderer (sdl-get-renderer window)))
    (if (null-pointer-p (autowrap:ptr renderer))
        nil
        renderer)))

;; export
(defun get-renderer-info (renderer)
  "Allocate a new SDL_RendererInfo structure, fill it in with information
about the specified renderer, and return it."
  (let ((rinfo (make-renderer-info)))
    (check-rc (sdl-get-renderer-info renderer rinfo))
    rinfo))

;; export
;; TODO SDL_GetRendererOutputSize
(defun get-renderer-output-size (renderer)
  (niy "SDL_GetRendererOutputSize()"))

;; export
(defun create-texture (renderer texture-format access width height)
  (let ((texture (sdl-create-texture renderer texture-format access w h)))
    (if (null-pointer-p (autowrap:ptr texture))
        nil
        texture)))




