/* Copyright (c) 2007 Mark Nevill
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

/** @file basedir_fs.h
  * Filesystem functions related to the XDG Base Directory specification. */

#ifndef XDG_BASEDIR_FS_H
#define XDG_BASEDIR_FS_H

#include <basedir.h>
#include <stdio.h>
#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

/** @name Filesystem-related XDG Base Directory Queries */
/*@{*/

/** Find all existing data files corresponding to relativePath.
  * Consider as performing @code fopen(filename, "r") @endcode on every possible @c filename
  * 	and returning the successful <tt>filename</tt>s.
  * @param relativePath Path to scan for.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return A sequence of null-terminated strings terminated by a double-null (empty string)
  * 	and allocated using malloc(), e.g.: @code "/etc/share\0/home/jdoe/.local\0" @endcode
  */
char * xdgDataFind(const char* relativePath, xdgHandle *handle);

/** Find all existing config files corresponding to relativePath.
  * Consider as performing @code fopen(filename, "r") @endcode on every possible @c filename
  * 	and returning the successful <tt>filename</tt>s.
  * @param relativePath Path to scan for.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return A sequence of null-terminated strings terminated by a double-null (empty string)
  * 	and allocated using malloc(), e.g.: @code "/etc/xdg\0/home/jdoe/.config\0" @endcode
  */
char * xdgConfigFind(const char* relativePath, xdgHandle *handle);

/** Open first possible data file corresponding to relativePath.
  * Consider as performing @code fopen(filename, mode) @endcode on every possible @c filename
  * 	and returning the first successful @c filename or @c NULL.
  * @param relativePath Path to scan for.
  * @param mode Mode with which to attempt to open files (see fopen modes).
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return File pointer if successful else @c NULL. Client must use @c fclose to close file.
  */
FILE * xdgDataOpen(const char* relativePath, const char* mode, xdgHandle *handle);

/** Open first possible config file corresponding to relativePath.
  * Consider as performing @code fopen(filename, mode) @endcode on every possible @c filename
  * 	and returning the first successful @c filename or @c NULL.
  * @param relativePath Path to scan for.
  * @param mode Mode with which to attempt to open files (see fopen modes).
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return File pointer if successful else @c NULL. Client must use @c fclose to close file.
  */
FILE * xdgConfigOpen(const char* relativePath, const char* mode, xdgHandle *handle);

/** Create path by recursively creating directories.
  * This utility function is not part of the XDG specification, but
  * nevertheless useful in context of directory manipulation.
  * @param path The path to be created.
  * @param mode The permissions to use for created directories. This parameter
  * 	is modified by the process's umask. For details, see mkdir(2)'s mode
  * 	parameter.
  * @return Zero on success, -1 if an error occured (in which case errno will
  * 	be set appropriately)
  */
int xdgMakePath(const char * path, mode_t mode);

/*@}*/

#ifdef __cplusplus
} // extern "C"
#endif

#endif /*XDG_BASEDIR_FS_H*/
