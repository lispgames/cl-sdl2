(in-package :sdl2)

;; no export
(defun unique-pairs (list)
  "Return all unique pair combinations of the list."
  (mapcon (lambda (x)
            (mapcar (lambda (y)
                      (list (car x) y))
                    (cdr x)))
          list))

(defmacro define-struct-accessors ((prefix foreign-struct) &body fields)
  `(progn
     ,@(loop for field in fields
             as name = (symbolicate prefix "-" field)
             collect
             `(defun ,name (,prefix) (c-ref ,prefix ,foreign-struct ,field))
             collect
             `(defun (setf ,name) (v ,prefix)
                (setf (c-ref ,prefix ,foreign-struct ,field) v)))))
