/*
  ImageMagick Application Programming Interface declarations.
*/
#ifndef _MAGICK_H
#define _MAGICK_H

#if defined(sun)
#define __EXTENSIONS__  1
#endif

#if defined(__hpux)
#define _HPUX_SOURCE  1
#endif

#if defined(vms)
#define _POSIX_C_SOURCE  1
#endif

/*
  System include declarations.
*/
#if defined(__cplusplus) || defined(c_plusplus)
#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cstring>
#else
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#endif
#if defined(_VISUALC_)
#include <direct.h>
#else
#include <unistd.h>
#endif
#include <ctype.h>
#include <signal.h>
#include <locale.h>
#include <errno.h>
#include <math.h>
#include <assert.h>
#if !defined(__OPENNT)
#include <time.h>
#else
#include <sys/time.h>
#endif
#if !defined(__MWERKS__)
#include <sys/types.h>
#include <sys/stat.h>
#else
#include <SIOUX.h>
#include <console.h>
#include <unix.h>
#include <types.h>
#include <stat.h>
#endif

/*
  ImageMagick API headers
*/
#if !defined(vms) && !defined(macintosh) && !defined(WIN32)
#include <magick/api.h>
#else
#include "api.h"
#endif

#undef index

#if defined(macintosh)
#define HasJPEG
#define HasLZW
#define HasPNG
#define HasTIFF
#define HasTTF
#define HasZLIB
#endif

#if defined(WIN32)
#define HasJBIG
#define HasJPEG
#define HasLZW
#define HasPNG
#define HasTIFF
#define HasTTF
#define HasX11
#define HasZLIB
#endif

#if defined(VMS)
#define HasJPEG
#define HasLZW
#define HasPNG
#define HasTIFF
#define HasTTF
#define HasX11
#define HasZLIB
#endif

#endif
