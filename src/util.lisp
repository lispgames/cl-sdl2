(in-package :sdl2)

(defmacro defbitfield* (name-and-options &body fields)
  "A bit of a hack to evaluate CONSTANTP values because DEFBITFIELD
does not itself."
  `(cffi:defbitfield ,name-and-options
     ,@(mapcar (lambda (f)
                 (if (listp f)
                     `(,(car f) ,(if (constantp (cadr f))
                                     (eval (cadr f))
                                     (cadr f)))
                     f))
               fields)))

(defmacro defbitfield-from-cenum ((bitfield-name enum-name
                                   &optional regexp (replace ""))
                                  &body additional-values)
  "Note this relies on some CFFI internals, but CFFI is otherwise
broken on typedef'd enums"
  (let* ((parsed-type (cffi::parse-type enum-name))
         (enum-type (cffi::name
                     (if (typep parsed-type 'cffi::foreign-enum)
                         parsed-type
                         (cffi::actual-type parsed-type))))
         (matcher (when regexp (ppcre:create-scanner regexp)))
         (keywords (foreign-enum-keyword-list enum-type)))
    `(defbitfield* ,bitfield-name
       ,@(loop for kw in keywords collect
               `(,(make-keyword (ppcre:regex-replace-all matcher (string kw)
                                                         replace))
                 ,(foreign-enum-value enum-type kw)))
       ,@additional-values)))

(defun struct-slots (struct-name)
  (let ((slots (cffi::slots (cffi::follow-typedefs (cffi::parse-type struct-name)))))
    (loop for slot being each hash-key in slots
          collect slot)))

(defmacro make-struct-accessors (struct-name)
  (let ((slots (struct-slots struct-name)))
    `(progn
       ,@(loop for slot in slots
               as struct-var = (symbolicate struct-name)
               as slot-accessor = (symbolicate struct-name "-" slot)
               collect
               `(progn
                  (declaim (inline ,slot-accessor (setf ,slot-accessor)))
                  (defun ,slot-accessor (,struct-var)
                    (cffi:foreign-slot-value ,struct-var
                                             ',struct-name
                                             ',slot))
                  (defun (setf ,slot-accessor) (val ,struct-var)
                    (setf (cffi:foreign-slot-value ,struct-var
                                                   ',struct-name
                                                   ',slot)
                          val)))))))
