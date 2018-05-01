(in-package #:sdl2)

(defmacro delay (ms)
  "This function waits a specified number of milliseconds before returning. It waits at least the
specified time, but possibly longer due to OS scheduling"
  `(sdl-delay ,ms))

(defmacro get-ticks ()
  "Returns an unsigned 32-bit value representing the number of milliseconds since the SDL library
initialized."
  `(sdl-get-ticks))

(defmacro get-performance-counter ()
  "This function is typically used for profiling. The counter values are only meaningful relative to
each other. Differences between values can be converted to times by using
'get-performance-frequency'."
  `(sdl-get-performance-counter))

(defmacro get-performance-frequency ()
  "Returns a platform-specific count per second."
  `(sdl-get-performance-frequency))

(defun add-timer (interval fn args)
  "Use this function to set up a callback function to be run on a separate thread after the
specified number of milliseconds has elapsed.

The callback function is passed the current timer interval and the user supplied parameter from the
'add-timer' call and returns the next timer interval. If the returned value from the callback is 0,
the timer is canceled."
  (check-zero (sdl-add-timer interval fn args)))

(defun remove-timer (timer)
  "Use this function to remove a timer created with 'add-timer'."
  (check-false (sdl-remove-timer timer)))
