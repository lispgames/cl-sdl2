(in-package #:sdl2)

;;;; TODO: This is missing these interfaces out of SDL_rect.h
;;;; SDL_EnclosePoints()
;;;; SDL_IntersectRectAndLine()
;;;;
;;;; TODO: Maybe put some restarts in here for invalid SDL_point/rect
;;;; structures.

;; export
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

;; export
(defun copy-point (point)
  "Allocate and return a new SDL_Point and make its slots be equal to
the passed in SDL_Point."
  (make-point (sdl-point.x point) (sdl-point.y point)))

;; export
(defun copy-into-point (dest-point src-point)
  "Copy the information from the SDL_Point src-point into the SDL_Point
dest-point. Return the dest-point."
  (setf
   (sdl-point.x dest-point) (sdl-point.x src-point)
   (sdl-point.y dest-point) (sdl-point.x src-point))
  dest-point)

;; export
(defun free-point (point)
  "Specifically free the SDL_Point structure which will do the right
thing with respect to the garbage collector. This is not required, but
may make garbage collection performance better if used in tight
SDL_Point allocating loops."
  (foreign-free (ptr point))
  (sdl-cancel-collect point)
  (autowrap:invalidate point))

;; export
(defun make-rect (x y w h)
  "Allocate and return a new SDL_Rect filled in with the arguments. It
will be garbage collected as needed."
  (let* ((rect (sdl-collect (alloc 'sdl2-ffi:sdl-rect))))
    (setf (sdl-rect.x rect) x
          (sdl-rect.y rect) y
          (sdl-rect.w rect) w
          (sdl-rect.h rect) h)
    rect))

;; no export
(defmethod print-object ((rect sdl2-ffi:sdl-rect) stream)
  (print-unreadable-object (rect stream :type t :identity t)
    (format stream "x ~A y ~A w ~A h ~A"
            (sdl-rect.x rect) (sdl-rect.y rect)
            (sdl-rect.w rect) (sdl-rect.h rect))))

;; export
(defun copy-rect (rect)
  "Allocate and return a new SDL_Rect and make its slots be equal to the
passed in SDL_Rect."
  (make-rect (sdl-rect.x rect)
             (sdl-rect.y rect)
             (sdl-rect.w rect)
             (sdl-rect.h rect)))

;; export
(defun copy-into-rect (dest-rect src-rect)
  "Copy the information from the SDL_Rect src-rect into the SDL_Rect
dest-rect. Return the dest-rect."
  (setf
   (sdl-rect.x dest-rect) (sdl-rect.x src-rect)
   (sdl-rect.y dest-rect) (sdl-rect.y src-rect)
   (sdl-rect.w dest-rect) (sdl-rect.w src-rect)
   (sdl-rect.h dest-rect) (sdl-rect.h src-rect))
  dest-rect)

;; export
(defun free-rect (rect)
  "Specifically free the SDL_Rect structure which will do the right
thing with respect to the garbage collector. This is not required, but
may make garbage collection performance better if used in tight
SDL_Rect allocating loops."
  (foreign-free (ptr rect))
  (sdl-cancel-collect rect)
  (autowrap:invalidate rect))

;; I hope trivial-garbage deals with these things correctly...

;; no export
;; used as a helper for with-rects
(defmacro with-rect ((binding) &body body)
  (cond
    ((symbolp binding)
     `(let ((,binding (make-rect 0 0 0 0)))
        ,@body))
    ((consp binding)
     `(let (,binding)
        ,@body))
    (t
     (error "with-rect: Must have a binding of either a symbol or a symbol and form"))))

;; export
(defmacro with-rects (bindings &body body)
  "With similar syntax to LET, allow specific bindings of SDL_Rect
structures or create empty rectangles with no height, width, and at
the origin for all unbound symbols in the bindings form. Example:

 (with-rects (foo
              (bar (make-rect 5 5 10 10))
              (qux 42))

  (list foo bar qux))

 -> (#<SDL-FFI.SDL-RECT x 0 y 0 w 0 h 0>
     #<SDL-FFI.SDL-RECT x 5 y 5 w 10 h 10>
     42)"
  (if (null bindings)
      `(progn ,@body)
      `(with-rect (,(car bindings))
         (with-rects ,(cdr bindings) ,@body))))

;;; The implementation of the methods.

;; export
(defun rect-empty (&rest rects)
  "Return T if the rectangle has no width or height."
  (every (lambda (rect)
           (and (not (null-pointer-p (ptr rect)))
                (or (<= (sdl-rect.w rect) 0)
                    (<= (sdl-rect.h rect) 0))))
         rects))

;; no export
(defun rect-equal (a b)
  "Return T if the two rectanges are valid and the slots are equal"
  (and (not (null-pointer-p (ptr a)))
       (not (null-pointer-p (ptr b)))
       (= (sdl-rect.x a) (sdl-rect.x b))
       (= (sdl-rect.y a) (sdl-rect.y b))
       (= (sdl-rect.w a) (sdl-rect.w b))
       (= (sdl-rect.h a) (sdl-rect.h b))))

;; export
(defun rect-equals (&rest rects)
  "Return T if the passed in SDL_Rect structures are valid and all
slots are equal to each other."
  (cond
    ((null rects)
     ;; Nothing is not equal to anything.
     nil)
    ((null (cdr rects))
     ;; One is equal to itself
     (not (null-pointer-p (ptr (car rects)))))
    (t
     ;; Many must be all equal to each other
     (destructuring-bind (head &rest tail) rects
       (unless (null-pointer-p (ptr head))
         (dolist (rect tail)
           (when (or (null-pointer-p (ptr rect)) (not (rect-equal head rect)))
             (return-from rect-equals nil)))
         t)))))

;; export
(defun has-intersect (&rest rects)
  "Return T if every SDL_Rect structure intersects every other
SDL_Rect structure."
  (if (null rects)
      nil
      (progn
        (loop :for (a b) :in (unique-pairs rects) :do
           (unless (sdl-true-p (sdl-has-intersection a b))
             (return-from has-intersect nil)))
        t)))


;; export
(defun intersect-rect (&rest rects)
  "Return two values. The first one is T if the intersection of ALL
rectangles results in a non-empty intersection. The second value is
the SDL_Rect of the intersection rectangle. If an empty intersection
is discovered, then NIL and an empty rectangle at the origin is
returned. The second value is always a newly allocated SDL_Rect
structure."
  (let ((empty (make-rect 0 0 0 0)))
    (cond
      ((null rects)
       ;; No intersection with no rectangle.
       (values nil empty))
      ((null (cdr rects))
       ;; One is its own intersection
       (values t (copy-rect (car rects))))
      (t
       ;; Compute the intersection of all of them in such a way as to
       ;; not cause excessive memory allocation.
       (destructuring-bind (head &rest tail) rects
         (let ((intersect (copy-rect head))
               (tmp-rect (make-rect 0 0 0 0)))
           (dolist (rect tail)
             (copy-into-rect tmp-rect intersect)
             (unless (sdl-true-p (sdl-intersect-rect rect tmp-rect intersect))
               (return-from intersect-rect (values nil empty))))
           (values t intersect)))))))

;; export
;; Calculate the union of all rectangles passed in. The result will be
;; one large rectangle in which all others fit perfectly.
(defun union-rect (&rest rects)
  "Return either NIL if no union is possible (empty argument list), or
the rectangle which is the union of all passed in rectangles. The
result is a newly allocated SDL_Rect."
  (cond
    ((null rects)
     ;; No union with no rectangle.
     nil)
    ((null (cdr rects))
     ;; One is its own union
     (copy-rect (car rects)))
    (t
     ;; Compute the union of all of them in such a way as to
     ;; not cause excessive memory allocation.
     (destructuring-bind (head &rest tail) rects
       (let ((union-rect (copy-rect head))
             (tmp-rect (make-rect 0 0 0 0)))
         (dolist (rect tail)
           (copy-into-rect tmp-rect union-rect)
           (sdl-union-rect rect tmp-rect union-rect))
         union-rect)))))
