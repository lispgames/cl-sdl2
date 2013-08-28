(in-package #:sdl2)

;;;;
;;;; TODO: This is missing these interfaces out of SDL_rect.h
;;;; SDL_EnclosePoints()
;;;; SDL_IntersectRectAndLine()
;;;;

(defun make-point (x y)
  "Return an SDL_Point filled in with the arguments. It will be garbage
collected as needed."
  (let* ((point (sdl-collect (autowrap:alloc 'sdl2-ffi:sdl-point))))
    (setf (sdl-point.x point) x
          (sdl-point.y point) y)
    point))

(defmethod print-object ((point sdl2-ffi:sdl-point) stream)
  (print-unreadable-object (point stream :type t :identity t)
    (format stream "x ~A y ~A"
            (sdl-point.x point) (sdl-point.y point))))

(defun copy-point (point)
  "Allocate and return a new SDL_Point and make its slots be equal to
the passed in SDL_Point."
  (make-point (sdl-point.x point) (sdl-point.y point)))

(defun copy-into-point (dest-point src-point)
  "Copy the information from the SDL_Point src-point into the SDL_Point
dest-point. Return the dest-point."
  (setf
   (sdl-point.x dest-point) (sdl-point.x src-point)
   (sdl-point.y dest-point) (sdl-point.x src-point))
  dest-point)

(defun free-point (point)
  "Specifically free the SDL_Point structure which will do the right
thing with respect to the garbage collector. This is not required, but
may make garbage collection performance better if used in tight
SDL_Point allocating loops."
  (foreign-free (ptr point))
  (sdl-cancel-collect point)
  (autowrap:invalidate point))

;; used as a helper for with-points
(defmacro %with-point ((binding) &body body)
  (cond
    ((symbolp binding)
     `(let ((,binding (make-point 0 0)))
        ,@body))
    ((= (length binding) 3)
     `(let ((,(first binding) (make-point ,@(cdr binding))))
        ,@body))
    (t
     (error "with-point: Must have a binding of either a symbol or a symbol and 2 forms which are x y of a point"))))

(defmacro with-points (bindings &body body)
  "A LET-like convenient bindings facility for SDL_point
structures. Raw symbols are bound to (make-point 0 0).

  Example:

  (let ((a 1) (b 2))
    (with-points (foo
                  (qux 5 10)
                  (bar (1+ a) b)
       (list foo qux bar))))

  -> (#<SDL-FFI:SDL-POINT x 0 y 0>
      #<SDL-FFI:SDL-POINT x 5 y 10>
      #<SDL-FFI:SDL-POINT x 2 y 2>)"
  (if (null bindings)
      `(progn ,@body)
      `(%with-point (,(car bindings))
         (with-points ,(cdr bindings) ,@body))))


(defun make-rect (x y w h)
  "Allocate and return a new SDL_Rect filled in with the arguments. It
will be garbage collected as needed."
  (let* ((rect (sdl-collect (alloc 'sdl2-ffi:sdl-rect))))
    (setf (sdl-rect.x rect) x
          (sdl-rect.y rect) y
          (sdl-rect.w rect) w
          (sdl-rect.h rect) h)
    rect))

(defmethod print-object ((rect sdl2-ffi:sdl-rect) stream)
  (print-unreadable-object (rect stream :type t :identity t)
    (format stream "x ~A y ~A w ~A h ~A"
            (sdl-rect.x rect) (sdl-rect.y rect)
            (sdl-rect.w rect) (sdl-rect.h rect))))

(defun copy-rect (rect)
  "Allocate and return a new SDL_Rect and make its slots be equal to the
passed in SDL_Rect."
  (make-rect (sdl-rect.x rect)
             (sdl-rect.y rect)
             (sdl-rect.w rect)
             (sdl-rect.h rect)))

(defun copy-into-rect (dest-rect src-rect)
  "Copy the information from the SDL_Rect src-rect into the SDL_Rect
dest-rect. Return the dest-rect."
  (setf
   (sdl-rect.x dest-rect) (sdl-rect.x src-rect)
   (sdl-rect.y dest-rect) (sdl-rect.y src-rect)
   (sdl-rect.w dest-rect) (sdl-rect.w src-rect)
   (sdl-rect.h dest-rect) (sdl-rect.h src-rect))
  dest-rect)

(defun free-rect (rect)
  "Specifically free the SDL_Rect structure which will do the right
thing with respect to the garbage collector. This is not required, but
may make garbage collection performance better if used in tight
SDL_Rect allocating loops."
  (foreign-free (ptr rect))
  (sdl-cancel-collect rect)
  (autowrap:invalidate rect))

;; I hope trivial-garbage deals with these things correctly...

;; used as a helper for with-rects
(defmacro %with-rect ((binding) &body body)
  (cond
    ((symbolp binding)
     `(let ((,binding (make-rect 0 0 0 0)))
        ,@body))
    ((= (length binding) 5)
     `(let ((,(first binding) (make-rect ,@(cdr binding))))
        ,@body))
    (t
     (error "with-rect: Must have a binding of either a symbol or a symbol and 4 forms which are x y w h of a rectangle"))))

(defmacro with-rects (bindings &body body)
  "A LET-like convenient bindings facility for SDL_Rect
structures. Raw symbols are bound to (make-rect 0 0 0 0).

  Example:

  (let ((a 1) (b 2) (c 3) (d 4))
    (with-rects (foo
                 (qux 5 10 15 20)
                 (bar (1+ a) b c (* d 10)))
       (list foo qux bar)))

  -> (#<SDL-FFI:SDL-RECT x 0 y 0 w 0 z 0>
      #<SDL-FFI:SDL-RECT x 5 y 10 w 15 h 20>
      #<SDL-FFI:SDL-RECT x 2 y 2 w 3 d 40>)"
  (if (null bindings)
      `(progn ,@body)
      `(%with-rect (,(car bindings))
         (with-rects ,(cdr bindings) ,@body))))

;;; The implementation of the SDL_rect.h methods.

(defun rect-empty (&rest rects)
  "Return T if the rectangle has no width or height."
  (every (lambda (rect)
           (and (not (null-pointer-p (ptr rect)))
                (or (<= (sdl-rect.w rect) 0)
                    (<= (sdl-rect.h rect) 0))))
         rects))

(defun %rect-equal (a b)
  "Return T if the two rectanges are valid and the slots are equal"
  (and (= (sdl-rect.x a) (sdl-rect.x b))
       (= (sdl-rect.y a) (sdl-rect.y b))
       (= (sdl-rect.w a) (sdl-rect.w b))
       (= (sdl-rect.h a) (sdl-rect.h b))))

(defun rect-equals (first-rect &rest rects)
  "Return T if the passed in SDL_Rect structures are valid and all
slots are equal to each other."
  (dolist (rect rects)
    (when (not (%rect-equal first-rect rect))
      (return-from rect-equals nil)))
  t)

(defun has-intersect (first-rect &rest rects)
  "Return T if every SDL_Rect structure intersects every other
SDL_Rect structure."
  (loop for (a b) in (unique-pairs `(,first-rect ,@rects))
     do
       (unless (sdl-true-p (sdl-has-intersection a b))
         (return-from has-intersect nil)))
  t)

(defun intersect-rect (first-rect &rest rects)
  "Return two values. The first one is T if the intersection of ALL
rectangles results in a non-empty intersection. The second value is
the SDL_Rect of the intersection rectangle. If an empty intersection
is discovered, then NIL and an empty rectangle at the origin is
returned. The second value is always a newly allocated SDL_Rect
structure."
  (let ((empty (make-rect 0 0 0 0))
        (intersect (copy-rect first-rect)))
    (dolist (rect rects)
      (unless (sdl-true-p (sdl-intersect-rect rect intersect intersect))
        (return-from intersect-rect (values nil empty))))
    (values t intersect)))

(defun union-rect (first-rect &rest rects)
  "Calculate and return the union of all rectangles passed in. The
result will be one large rectangle as a newly allocated SDL_rect in
which all others fit perfectly."
  (let ((union-rect (copy-rect first-rect)))
    (dolist (rect rects)
      (sdl-union-rect rect union-rect union-rect))
    union-rect))
