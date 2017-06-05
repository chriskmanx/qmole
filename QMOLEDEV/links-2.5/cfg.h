/* cfg.h
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#ifndef CFG_H
#define CFG_H
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_CONFIG2_H
#include "config2.h"
#endif

/* no one will probably ever port svgalib on atheos or beos or port atheos
   interface to beos, but anyway: make sure they don't clash */

#if defined(__BEOS__) || defined(__HAIKU__)
#ifdef GRDRV_SVGALIB
#undef GRDRV_SVGALIB
#endif
#ifdef GRDRV_ATHEOS
#undef GRDRV_ATHEOS
#endif
#endif

#ifdef GRDRV_ATHEOS
#ifdef GRDRV_SVGALIB
#undef GRDRV_SVGALIB
#endif
#endif

typedef int cfg_h_no_empty_unit;

#endif
