;;;
;;; Demonstrate opening a window with SDL, establishing a GL context,
;;; drawing a texture with cairo, and rendering it to a GL texture.
;;;
;;; Note this unfortunately does not use the SDL_Texture stuff yet.
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :sdl2)
  (asdf:load-system :cl-opengl)
  (asdf:load-system :cl-cairo2))

(defparameter *vertex-shader* "
void main() {
    gl_TexCoord[0] = gl_MultiTexCoord0;
    gl_Position = ftransform();
}
")

(defparameter *fragment-shader* "
#version 120

uniform sampler2D tex;

void main() {
    gl_FragColor = texture2D(tex, gl_TexCoord[0].st).bgra;
}
")

(defparameter *texture-size* 512)

(defun gl-ortho-setup (&key (width 500) (height 500))
  "Set up 1:1 pixel ortho matrix"
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:ortho 0 width height 0 -1 1))

(defun compile-and-check-shader (shader source)
  (gl:shader-source shader source)
  (gl:compile-shader shader)
  (unless (gl:get-shader shader :compile-status)
    (gl:get-shader-info-log shader)))

(defun gl-init-shaders ()
  (let ((v-shader (gl:create-shader :vertex-shader))
        (f-shader (gl:create-shader :fragment-shader)))
    (compile-and-check-shader v-shader *vertex-shader*)
    (compile-and-check-shader f-shader *fragment-shader*)
    (let ((program (gl:create-program)))
      (if (= 0 program)
          (error "Error creating program")
          (progn
            (gl:attach-shader program v-shader)
            (gl:attach-shader program f-shader)
            (gl:link-program program)
            (format t "~A~%" (gl:get-program-info-log program))
            (gl:use-program program)))
      program)))

(defun render (window width height margin tex)
  (let* ((surf (cairo:create-image-surface-for-data
                tex :argb32
                *texture-size* *texture-size* (* 4 *texture-size*)))
         (ctx (cairo:create-context surf)))
    (progn
      (cairo:with-context (ctx)
        (cairo:set-source-rgb 1 1 1)
        (cairo:paint)
        (cairo:set-source-rgb 0 0 1)
        (cairo:set-line-width 3)
        (cairo:move-to 0 0)
        (cairo:line-to *texture-size* *texture-size*)
        (cairo:stroke)

        (cairo:set-source-rgb 1 0 0)
        (cairo:set-line-width 1)
        (cairo:arc 0 0 30 0 (* 2 pi))
        (cairo:stroke)
        (cairo:arc *texture-size* *texture-size* 30 0 (* 2 pi))
        (cairo:stroke))
      (cairo:destroy ctx)
      (cairo:destroy surf)))
  (gl:tex-image-2d :texture-2d 0 :rgba
                   *texture-size* *texture-size*
                   0 :rgba :unsigned-byte tex)
  (gl:begin :quads)
  (gl:color 1.0 0.0 0.0 1.0)
  (%gl:tex-coord-2f 0.0 0.0)
  (gl:vertex margin margin)
  (%gl:tex-coord-2f 0.0 1.0)
  (gl:vertex margin (- height margin))
  (%gl:tex-coord-2f 1.0 1.0)
  (gl:vertex (- width margin) (- height margin))
  (%gl:tex-coord-2f 1.0 0.0)
  (gl:vertex (- width margin) margin)
  (gl:end)
  (gl:flush)
  (sdl2:gl-swap-window window))

(defun cairo-test (&key (width *texture-size*) (height *texture-size*) (margin 0))
  (sdl2:with-init (:everything)
    (multiple-value-bind (window renderer)
        (sdl2:create-window-and-renderer width height '(:shown :opengl))
      (sdl2:with-gl-context (gl window)
        (sdl2:gl-make-current window gl)
        (gl:enable :texture-2d)
        (gl-ortho-setup :width width :height height)

        (autowrap:with-alloc (tex :unsigned-char (* *texture-size* *texture-size* 4))
          (let* ((program (gl-init-shaders))
                 (texid (gl:get-uniform-location program "tex")))
            (%gl:active-texture :texture0)
            (gl:uniformi texid 0)

            (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
            (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
            (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
            (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)

            (gl:matrix-mode :modelview)
            (gl:load-identity)
            (gl:clear-color 1.0 1.0 1.0 1.0)
            (gl:clear :color-buffer)

            (sdl2:with-event-loop (:method :poll)
              (:keyup () (sdl2:push-event :quit))
              (:mousebuttondown () (sdl2:push-event :quit))
              (:idle ()
                (render window width height margin tex)
                (sleep 0.1))
              (:quit () t))

            (sdl2:destroy-renderer renderer)
            (sdl2:destroy-window window)))))))

#+-(cairo-test)
