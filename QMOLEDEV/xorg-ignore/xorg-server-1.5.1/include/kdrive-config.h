/* include/kdrive-config.h.  Generated from kdrive-config.h.in by configure.  */
/* kdrive-config.h.in: not at all generated.                      -*- c -*-
 */

#ifndef _KDRIVE_CONFIG_H_
#define _KDRIVE_CONFIG_H_

#include <dix-config.h>
#include <xkb-config.h>

/* Building kdrive server. */
/* #undef KDRIVESERVER */

/* Include framebuffer support in X servers */
/* #undef KDRIVEFBDEV */

/* Include vesa support in X servers */
/* #undef KDRIVEVESA */

/* Enable touchscreen support */
/* #undef TOUCHSCREEN */

/* Support tslib touchscreen abstraction library */
/* #undef TSLIB */

/* Verbose debugging output hilarity */
/* #undef DEBUG */

/* Have the backtrace() function. */
#define HAVE_BACKTRACE 1

/* Have execinfo.h for backtrace(). */
#define HAVE_EXECINFO_H 1

#endif /* _KDRIVE_CONFIG_H_ */
