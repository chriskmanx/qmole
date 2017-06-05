/*
 * Copyright (C) 1989-95 GROUPE BULL
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * GROUPE BULL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of GROUPE BULL shall not be
 * used in advertising or otherwise to promote the sale, use or other dealings
 * in this Software without prior written authorization from GROUPE BULL.
 */

/*****************************************************************************\
*  WrFFrData.c:                                                               *
*                                                                             *
*  XPM library                                                                *
*  Parse an Xpm array and write a file that corresponds to it.                *
*                                                                             *
*  Developed by Dan Greening dgreen@cs.ucla.edu / dgreen@sti.com              *
\*****************************************************************************/

#include <LTconfig.h>

/* amai: we can't include DebugUtil.h before system headers.
   So we assume that system headers are idempotent, #include
   all of those used below here and then try with our
   DebugUtil.h! */
#include <stdio.h>
#include <string.h>

#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if defined(FOR_MSW) || defined(WIN32)
#include <io.h>
#endif

#include <X11/Intrinsic.h> /* Avoid re-definition of Pixel-type */

#include <Xm/XpmP.h>
#include <XmI/XpmI.h>

/* #if !defined(WITH_DBMALLOC) && !defined(WITH_DMALLOC) */
#include <XmI/DebugUtil.h>

#ifdef HAVE_STRLCAT
# define STRLCAT(dst, src, dstsize) { \
 	if (strlcat(dst, src, dstsize) >= (dstsize)) \
	    return (XpmFileInvalid); }
# define STRLCPY(dst, src, dstsize) { \
  	if (strlcpy(dst, src, dstsize) >= (dstsize)) \
	    return (XpmFileInvalid); }
#else
# define STRLCAT(dst, src, dstsize) { \
	if ((strlen(dst) + strlen(src)) < (dstsize)) \
 	    strcat(dst, src); \
	else return (XpmFileInvalid); }
# define STRLCPY(dst, src, dstsize) { \
	if (strlen(src) < (dstsize)) \
 	    strcpy(dst, src); \
	else return (XpmFileInvalid); }
#endif

int
XpmWriteFileFromData(filename, data)
    char *filename;
    char **data;
{
    XpmImage image;
    XpmInfo info;
    int ErrorStatus;

    info.valuemask = XpmReturnComments | XpmReturnExtensions;

    ErrorStatus = XpmCreateXpmImageFromData(data, &image, &info);

    if (ErrorStatus != XpmSuccess)
	return (ErrorStatus);

    ErrorStatus = XpmWriteFileFromXpmImage(filename, &image, &info);

    XpmFreeXpmImage(&image);
    XpmFreeXpmInfo(&info);

    return (ErrorStatus);
}
