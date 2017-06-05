/*
 * Copyright (C) 1997-2004, Michael Jennings
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

/**
 * @file file.c
 * File manipulation routines.
 *
 * This file contains file-related functions.
 *
 * @author Michael Jennings <mej@eterm.org>
 * $Revision: 1.11 $
 * $Date: 2004/07/23 21:38:39 $
 */

static const char __attribute__((unused)) cvs_ident[] = "$Id: file.c,v 1.11 2004/07/23 21:38:39 mej Exp $";

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <libast_internal.h>

/**
 * Create and open a temporary file.
 *
 * This function creates a temporary file and returns a standard UNIX
 * file descriptor for the new file.  The file is created in $TMPDIR,
 * or $TMP, or /tmp if all else fails.  The libc function @c mkstemp()
 * does the actual file creation/opening.  The umask is set to 0077
 * just before the call, and the permissions are forceably changed to
 * 0600 immediately after.  If the @a len parameter is non-zero, up to
 * @a len characters of the path and filename of the newly-created
 * file are copied into the buffer pointed to by @a ftemplate.  The
 * file descriptor is then returned to the caller.
 *
 * @param ftemplate The initial portion of the filename.  If you want
 *                  the path info returned, make sure this buffer is
 *                  sufficiently large.
 * @param len       The maximum number of bytes @a ftemplate can
 *                  hold.
 * @return          The file descriptor for the new temp file.
 */
int
spiftool_temp_file(spif_charptr_t ftemplate, size_t len)
{
    spif_char_t buff[256];
    int fd;
    mode_t m;

    ASSERT_RVAL(!SPIF_PTR_ISNULL(ftemplate), SPIF_CAST_C(int) -1);
    ASSERT_RVAL(len > 0, SPIF_CAST_C(int) -1);
    if (getenv("TMPDIR")) {
        snprintf(SPIF_CAST_C(char *) buff, sizeof(buff),
                 "%s/%sXXXXXX", getenv("TMPDIR"), ftemplate);
    } else if (getenv("TMP")) {
        snprintf(SPIF_CAST_C(char *) buff, sizeof(buff),
                 "%s/%sXXXXXX", getenv("TMP"), ftemplate);
    } else {
        snprintf(SPIF_CAST_C(char *) buff, sizeof(buff),
                 "/tmp/%sXXXXXX", ftemplate);
    }
    m = umask(0077);
    fd = mkstemp(SPIF_CAST_C(char *) buff);
    umask(m);
    if ((fd < 0) || fchmod(fd, (S_IRUSR | S_IWUSR))) {
        return (-1);
    }

    if (len) {
        spiftool_safe_strncpy(ftemplate, buff, len);
    }
    return (fd);
}
