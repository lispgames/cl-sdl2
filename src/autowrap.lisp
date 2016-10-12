(cl:in-package :sdl2-ffi)

(autowrap:c-include
 '(sdl2 autowrap-spec "SDL2.h")
  :accessor-package :sdl2-ffi.accessors
  :function-package :sdl2-ffi.functions
  :spec-path '(sdl2 autowrap-spec)
  :exclude-sources ("/usr/local/lib/clang/([^/]*)/include/(?!stddef.h)"
                    "/usr/include/"
                    "/usr/include/arm-linux-gnueabihf"
                    "/usr/include/X11/")
  :include-sources ("stdint.h"
                    "bits/types.h"
                    "machine/_types.h"
                    "sys/types.h"
                    "SDL2")
  :exclude-definitions ("SDL_LogMessageV"
                        "SDL_vsnprintf"
                        "_inline$"
                        "^_mm_")
  :include-definitions ("^XID$" "^Window$" "^Display$" "^_XDisplay$")
  :symbol-exceptions (("SDL_Log" . "SDL-LOGGER")
                      ("SDL_log" . "SDL-LOGN")
                      ("SDL_RWops" . "SDL-RWOPS")
                      ("SDL_GLContext" . "SDL-GLCONTEXT")
                      ("SDL_GLattr" . "SDL-GLATTR")
                      ("SDL_GLprofile" . "SDL-GLPROFILE")
                      ("SDL_GLcontextFlag" . "SDL-GLCONTEXT-FLAG")
                      ("SDL_SysWMinfo" . "SDL-SYSWM-INFO")
                      ("SDL_SysWMmsg" . "SDL-SYSWM-MSG")
                      ("SDL_TRUE" . "TRUE")
                      ("SDL_FALSE" . "FALSE"))
  :no-accessors cl:t
  :release-p cl:t)
