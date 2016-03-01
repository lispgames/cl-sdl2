#include <SDL2/SDL.h>

#define SDL_VIDEO_DRIVER_X11 1

/* From:
   https://msdn.microsoft.com/en-us/library/windows/desktop/aa383751(v=vs.85).aspx

   This is a somewhat minimal set of stuff that makes the SDL_syswm.h
   windows-specific structs load, without requiring a full windows
   header set.
*/
#define SDL_VIDEO_DRIVER_WINDOWS 1
typedef void* HANDLE;
typedef HANDLE HWND;
typedef HANDLE HDC;

#if defined(_WIN64)
typedef unsigned __int64 UINT_PTR;
#else
typedef unsigned int UINT_PTR;
#endif

#if defined(_WIN64)
typedef __int64 LONG_PTR; 
#else
typedef long LONG_PTR;
#endif

typedef UINT_PTR WPARAM;
typedef LONG_PTR LPARAM;
typedef unsigned int UINT;
#include <SDL2/SDL_syswm.h>
