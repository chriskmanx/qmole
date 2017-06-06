/*
  ImageMagick Memory Allocation Methods.
*/
#ifndef _MEMORY_H
#define _MEMORY_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#if defined(WITH_DMALLOC)
#include <dmalloc.h>
#endif
/*
  Memory declarations.
*/
extern Export void
  *AllocateMemory(const size_t),
  FreeMemory(void *),
  *ReallocateMemory(void *,const size_t);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
