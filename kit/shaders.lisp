(in-package :sdl2.kit)

;; (defvar *shader-dict*
;;   '((:solid
;;      (:uniforms ((x "x") (y "y")))
;;      (:shaders
;;       :vertex-shader "..."
;;       :fragment-shader "..."))))

(defun compile-and-check-shader (shader source)
  (gl:shader-source shader source)
  (gl:compile-shader shader)
  (unless (gl:get-shader shader :compile-status)
    (gl:get-shader-info-log shader)))

(defun compile-and-link-program (&rest shaders)
  "(compile-and-link-program :vertex-shader STRING :fragment-shader STRING ...)"
  (let (compiled-shaders)
    (loop for type in shaders by #'cddr
          for text in (cdr shaders) by #'cddr
          do (let ((shader (gl:create-shader type)))
               (compile-and-check-shader shader text)
               (push shader compiled-shaders)))
    (let ((program (gl:create-program)))
      (if (= 0 program)
          (progn
            (loop for shader in compiled-shaders
                  do (gl:delete-shader shader))
            (error "Error creating program"))
          (progn
            (loop for shader in compiled-shaders
                  do (gl:attach-shader program shader))
            (gl:link-program program)
            (let ((log (gl:get-program-info-log program)))
              (unless (string= "" log)
                (format *error-output* "~A~%" log)))
            (loop for shader in compiled-shaders
                  do (gl:detach-shader program shader)
                     (gl:delete-shader shader))))
      program)))

(defclass program ()
  ((name :initform nil :initarg :name)
   (id :initform nil)
   (uniforms :initform (make-hash-table :test 'equal))))

(defclass shader-dictionary ()
  ((programs :initform (make-hash-table))
   (active-program :initform nil)))

(defgeneric preprocess-program-entry (type entry program))
(defgeneric postprocess-program-entry (type entry program))

(defmethod preprocess-program-entry (type entry program))
(defmethod postprocess-program-entry (type entry program))

(defmethod preprocess-program-entry ((type (eql :shaders)) entry program)
  (let ((p (apply #'compile-and-link-program entry)))
    (gl:use-program p)
    (with-slots (id) program
      (setf id p))))

(defun symbol-to-uniform (symbol)
  (substitute #\_ #\- (string-downcase (symbol-name symbol))))

(defmethod postprocess-program-entry ((type (eql :uniforms)) entry program)
  (with-slots (id uniforms) program
    (loop for uniform in entry
          as symbol = (if (symbolp uniform) uniform (car uniform))
          as name = (if (or (symbolp uniform)
                            (not (cadr uniform)))
                        (symbol-to-uniform uniform)
                        (cadr uniform))
          as loc = (gl:get-uniform-location id name)
          do (setf (gethash symbol uniforms) loc))))

(defun find-program (dictionary name)
  (with-slots (programs) dictionary
    (gethash name programs)))

(defun find-uniform (dictionary program name)
  (with-slots (uniforms) (find-program dictionary program)
    (gethash name uniforms)))

(defun compile-shader-dictionary (dictionary)
  "Input is a well-formatted list of shaders.  Returns a new
SHADER-DICTIONARY object.  This must be called with a valid, active
GL-CONTEXT.  The result is only valid while that GL-CONTEXT is
valid."
  (let ((sd (make-instance 'shader-dictionary)))
    (with-slots (programs) sd
      (loop for program-spec in dictionary
            as name = (car program-spec)
            as program = (make-instance 'program :name name)
            do (setf (gethash name programs) program)
               (loop for entry in (cdr program-spec)
                     do (preprocess-program-entry
                         (car entry) (cdr entry) program))
               (loop for entry in (cdr program-spec)
                     do (postprocess-program-entry
                         (car entry) (cdr entry) program))))
    sd))

(defun use-program (dict program)
  "Set program named `PROGRAM` in `DICT` as the active program."
  (with-slots (active-program) dict
    (let ((p (find-program dict program)))
      (with-slots (id) p
        (setf active-program p)
        (gl:use-program id)))))

(defmacro with-uniform-location ((var name) dict &body body)
  `(with-slots (active-program) ,dict
     (with-slots (uniforms) active-program
       (let ((,var (gethash ,name uniforms)))
         ,@body))))

(declaim (notinline uniformi uniformf uniformfv uniform-matrix))
(defun uniformi (dict name x &optional y z w)
  "Set the value for uniform with name `NAME` in the
active program (set by sdk2.kit:use-program)."
  (with-uniform-location (u name) dict
    (cond
      (w (%gl:uniform-4i u x y z w))
      (z (%gl:uniform-3i u x y z))
      (y (%gl:uniform-2i u x y))
      (x (%gl:uniform-1i u x)))))

(defun uniformf (dict name x &optional y z w)
  "Set the value for uniform with name `NAME` in the
active program (set by sdk2.kit:use-program)."
  (with-uniform-location (u name) dict
    (cond
      (w (%gl:uniform-4f u x y z w))
      (z (%gl:uniform-3f u x y z))
      (y (%gl:uniform-2f u x y))
      (x (%gl:uniform-1f u x)))))

(defun uniformfv (dict name a)
  (with-uniform-location (u name) dict
    (gl:uniformfv u a)))

(defun uniform-matrix (dict name dim matrices &optional (transpose t))
  (with-uniform-location (u name) dict
    (gl:uniform-matrix u dim matrices transpose)))
