/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _ETERM_DEBUG_H
# define _ETERM_DEBUG_H

/* Debugging macros/defines which set the debugging levels for each output type.
   To change the debugging level at which something appears, change the number in
   both the DEBUG_ definition and the D_ macro (if there is one). -- mej */

#  define DEBUG_SCREEN          1
#  define D_SCREEN(x)           DPRINTF1(x)
#  define DEBUG_CMD             1
#  define D_CMD(x)              DPRINTF1(x)
#  define DEBUG_TTY             1
#  define D_TTY(x)              DPRINTF1(x)
#  define DEBUG_SELECTION       1
#  define D_SELECT(x)           DPRINTF1(x)
#  define DEBUG_UTMP            1
#  define D_UTMP(x)             DPRINTF1(x)
#  define DEBUG_IMLIB           1
#  define D_IMLIB(x)            DPRINTF1(x)
#  define DEBUG_PIXMAP          1
#  define D_PIXMAP(x)           DPRINTF1(x)
#  define DEBUG_EVENTS          1
#  define D_EVENTS(x)           DPRINTF1(x)
 
#  define DEBUG_X11             2
#  define D_X11(x)              DPRINTF2(x)
#  define DEBUG_ENL             2
#  define D_ENL(x)              DPRINTF2(x)
#  define DEBUG_SCROLLBAR       2
#  define D_SCROLLBAR(x)        DPRINTF2(x)
#  define DEBUG_BBAR            2
#  define D_BBAR(x)             DPRINTF2(x)
#  define DEBUG_TIMER           2
#  define D_TIMER(x)            DPRINTF2(x)
#  define DEBUG_SCRIPT          2
#  define D_SCRIPT(x)           DPRINTF2(x)
 
#  define DEBUG_MENU            3
#  define D_MENU(x)             DPRINTF3(x)
#  define DEBUG_FONT            3
#  define D_FONT(x)             DPRINTF3(x)
#  define DEBUG_TTYMODE         3
#  define D_TTYMODE(x)          DPRINTF3(x)
#  define DEBUG_COLORS          3
#  define D_COLORS(x)           DPRINTF3(x)
 
#  define DEBUG_ACTIONS         4
#  define D_ACTIONS(x)          DPRINTF4(x)
#  define DEBUG_ESCREEN         4
#  define D_ESCREEN(x)          DPRINTF4(x)

#  define DEBUG_PROFILE         5
#  define D_PROFILE(x)          DPRINTF5(x)

#  define DEBUG_VT              6
#  define D_VT(x)               DPRINTF6(x)

#  define DEBUG_ALL_SCREEN      7
#  define D_ALL_SCREEN(x)       DPRINTF7(x)

#  define DEBUG_X               9

#endif /* _ETERM_DEBUG_H */
