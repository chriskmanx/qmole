#ifndef __COMMON
#define __COMMON 1

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <config.h>
#include <string.h>
#include <math.h>
#ifdef WITH_DMALLOC
# include <dmalloc.h>
#endif
#ifdef __EMX__
#include <sys/types.h>
#endif

#if defined(__GNUC__) && (__GNUC__ >= 4)
#define __hidden __attribute__((visibility("hidden")))
#else
#define __hidden
#endif

#define DATABIG unsigned long long
#define DATA64  unsigned long long
#define DATA32  unsigned int
#define DATA16  unsigned short
#define DATA8   unsigned char

#ifdef DO_MMX_ASM
__hidden int __imlib_get_cpuid(void);
#define CPUID_MMX (1 << 23)
#define CPUID_XMM (1 << 25)
#endif

#define CLIP(x, y, w, h, xx, yy, ww, hh) \
if (x < (xx)) {w += (x - (xx)); x = (xx);} \
if (y < (yy)) {h += (y - (yy)); y = (yy);} \
if ((x + w) > ((xx) + (ww))) {w = (ww) - (x - xx);} \
if ((y + h) > ((yy) + (hh))) {h = (hh) - (y - yy);}
#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

#define round(x) ((x)>=0?(int)((x)+0.5):(int)((x)-0.5))

#ifdef __EMX__
extern char *__XOS2RedirRoot(const char *);
#endif

#endif
