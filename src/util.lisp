(in-package :sdl2)

;; no export
(defun unique-pairs (list)
  "Return all unique pair combinations of the list."
  (mapcon (lambda (x)
            (mapcar (lambda (y)
                      (list (car x) y))
                    (cdr x)))
          list))
