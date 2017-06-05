/* DECODE.C, UNARJ, R JUNG, 06/05/02
 * Decode ARJ archive
 * Copyright (c) 1991-2002 by ARJ Software, Inc.  All rights reserved.
 *
 *   This code may be freely used in programs that are NOT ARJ archivers
 *   (both compress and extract ARJ archives).
 *
 *   If you wish to distribute a modified version of this program, you
 *   MUST indicate that it is a modified version both in the program and
 *   source code.
 *
 *   If you modify this program, we would appreciate a copy of the new
 *   source code.  We are holding the copyright on the source code, so
 *   please do not delete our name from the program files or from the
 *   documentation.
 *
 * Modification history:
 * Date      Programmer  Description of modification.
 * 04/05/91  R. Jung     Rewrote code.
 * 04/23/91  M. Adler    Portabilized.
 * 04/29/91  R. Jung     Made GETBIT independent of short size.
 * 05/04/91  R. Jung     Simplified use of start[len].
 * 08/28/91  R. Jung     Added KEEP_WINDOW for systems with low memory.
 * 02/17/93  R. Jung     Added extra test for bad data to make_table().
 *                       Added PTABLESIZE defines.
 * 01/22/94  R. Jung     Changed copyright message.
 * 06/05/02  R. Jung     Changed sizeof() to actual values in calls to
 *                       make_table() per suggestion Grzegorz Malicki.
 *
 */

#include "unarj.h"

#ifdef MODERN
#include <stdlib.h>
#else /* !MODERN */
extern void free();
#endif /* ?MODERN */


/* Local functions */


/* Local variables */


/* end DECODE.C */
