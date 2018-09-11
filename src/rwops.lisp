;; SDL2 offers an abstract interface for I/O streams. This file defines bindings operations on RWops
;; including creating RWops structures from files and closing them
(in-package :sdl2)

(defun %sdl-rw-close (sdl-rwops-ptr)
  (cffi:foreign-funcall-pointer
   (plus-c:c-ref sdl-rwops-ptr sdl2-ffi:sdl-rwops :close)
   ()
   :pointer sdl-rwops-ptr
   :int))

(defun rw-close (sdl-rwops-struct)
  "Flush the file represented by the sdl-rwops object and free the memory associated with it.
Returns 0 if the file is successfully flushed and -1 otherwise. Even if the file fails to flush the
memory is freed and pointer is invalid"
  (%sdl-rw-close (autowrap:ptr sdl-rwops-struct))
  (autowrap:invalidate sdl-rwops-struct))

(defun rw-from-file (file-name mode)
  "Create an RWops structure from a given file name in a given mode."
  (check-nullptr (sdl-rw-from-file file-name mode)))
