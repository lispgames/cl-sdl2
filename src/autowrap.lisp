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
 :include-sources #.(cl:append
                      #+darwin (cl:list "MacOSX.sdk/usr/include/sys/_types"
                                        "MacOSX.sdk/usr/include/_types"
                                        "MacOSX.sdk/usr/include/arm/_types")
                      (cl:list "stdint.h"
                               "bits/types.h"
                               "sys/types.h"
                               "bits/stdint"
                               "machine/_types.h"
                               "SDL2"))
 :sysincludes `,(cl:append
                  #+openbsd (cl:list "/usr/X11R6/include")
                  #+(and unix (not darwin))
                  (cl:list "/usr/lib/clang/13.0.1/include/")
                  #+darwin (cl:list "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/"))
 :exclude-definitions ("SDL_main"
                       "SDL_LogMessageV"
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
