#ifdef __PPC64__
/*
  Force uint8_t, uint16_t, uint32_t and uint64_t to be defined.  On
  ppc64, by default sys/types.h gets included by SDL.h, and it
  includes bits/stdint-intn.h, but not bits/stdint-uintn.h.  As a
  result, int8_t, int16_t, int32_t and int64_t are defined but not
  uint8_t, uint16_t, uint32_t and uint64_t.  u_int8_t, u_int16_t,
  u_int32_t and u_int64_t are defined, but autowrap does not use them.
  Include bits/stdint-uintn.h directly to eliminate missing field
  errors for SDL Uint8, Uint16, Uint32 and Uint64 fields, e.g.: ;
  Unknown field :TYPE for foreign-record type:
  #<AUTOWRAP:FOREIGN-RECORD SDL-EVENT {100534A9DC}>
 */
# include <bits/stdint-uintn.h>
#endif
#include <SDL2/SDL.h>
#include <SDL2/SDL_syswm.h>
