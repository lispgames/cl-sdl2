(in-package #:sdl2)

(defun platform ()
  "Returns the name of the platform. If the correct platform name is not available, returns a string
beginning with the text \"Unknown\""
  (multiple-value-bind (p _) (sdl-get-platform)
    (declare (ignore _))
    p))

(defmacro cpu-cache-line-size ()
  "Returns the L1 cache line size of the CPU.
This is useful for determining multi-threaded structure padding or SIMD prefetch sizes."
  `(sdl-get-cpu-cache-line-size))

(defmacro cpu-count ()
  "Returns the total number of logical CPU cores. On CPUs that include technologies such as
hyperthreading, the number of logical cores may be more than the number of physical cores."
  `(sdl-get-cpu-count))

(defmacro mmx-p ()
  "Returns t if CPU has MMX features, nil if not."
  `(sdl-true-p (sdl-has-mmx)))

(defmacro alti-vec-p ()
  "Returns t if CPU has MMX features, nil if not."
  `(sdl-true-p (sdl-has-alti-vec)))

(defmacro rdtsc-p ()
  "Returns t if CPU has the RDTSC instruction, nil if not."
  `(sdl-true-p (sdl-has-rdtsc)))

(defmacro sse-p ()
  "Returns t if CPU has SSE features, nil if not."
  `(sdl-true-p (sdl-has-sse)))

(defmacro sse2-p ()
  "Returns t if CPU has SSE 2 features, nil if not."
  `(sdl-true-p (sdl-has-sse2)))

(defmacro sse3-p ()
  "Returns t if CPU has SSE 3 features, nil if not."
  `(sdl-true-p (sdl-has-sse3)))

(defmacro sse41-p ()
  "Returns t if CPU has SSE 4.1 features, nil if not."
  `(sdl-true-p (sdl-has-sse41)))

(defmacro sse42-p ()
  "Returns t if CPU has SSE 4.2 features, nil if not."
  `(sdl-true-p (sdl-has-sse42)))

(defun power-info ()
  "Use this function to get the current power supply details.

Returns the current power state, seconds remaining, and percent remaining.
- Power state will be one of: :unknown, :on-battery, :no-battery, :charging, :charged
- Seconds will be -1 if a value can't be determined or you're not running on battery.
- Percent remaining will be a value between 0 and 100, or -1 if a value can't be determined or
  you're not running on battery.

You should never take a battery status as absolute truth. Batteries (especially failing batteries)
are delicate hardware, and the values reported here are best estimates based on what that hardware
reports. It's not uncommon for older batteries to lose stored power much faster than it reports, or
completely drain when reporting it has 20 percent left, etc.

Battery status can change at any time; if you are concerned with power state, you should call this
function frequently, and perhaps ignore changes until they seem to be stable for a few seconds."
  (with-foreign-objects ((seconds :int)
                         (percent :int))
    (let ((state (sdl-get-power-info seconds percent)))
      (values (autowrap:enum-key 'sdl2-ffi::sdl-power-state state)
              (mem-ref seconds :int)
              (mem-ref percent :int)))))
