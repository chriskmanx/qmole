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

/** @mainpage
 *
 * <div style="margin: 2em 5em 2em 5em; border: 1px solid #CCC; padding: 1em; background: #E8EEF2;">
 * Various specifications specify files and file formats. The <a
 * href="http://standards.freedesktop.org/basedir-spec/basedir-spec-0.6.html">
 * XDG Base Directory specification</a> defines where these files should
 * be looked for by defining one or more base directories relative to
 * which files should be located.
 * </div>
 *
 * This library implements functions to list the directories according
 * to the specification and provides a few higher-level functions for
 * use with the specification.
 */

/** @file basedir.h
  * Functions for using the XDG Base Directory specification. */ 

#ifndef XDG_BASEDIR_H
#define XDG_BASEDIR_H

#ifdef __cplusplus
extern "C" {
#endif

/** Version of XDG Base Directory specification implemented in this library. */
#define XDG_BASEDIR_SPEC 0.7

/** @name XDG data cache management */
/*@{*/

/** Handle to XDG data cache.
  * Handles are initialized with xdgInitHandle() and
  * freed with xdgWipeHandle(). */
typedef struct /*_xdgHandle*/ {
	/** Reserved for internal use, do not modify. */
	void *reserved;
} xdgHandle;

/** Initialize a handle to an XDG data cache and initialize the cache.
  * Use xdgWipeHandle() to free the handle.
  * @return a pointer to the handle if initialization was successful, else 0 */
xdgHandle * xdgInitHandle(xdgHandle *handle);

/** Wipe handle of XDG data cache.
  * Wipe handle initialized using xdgInitHandle(). */
void xdgWipeHandle(xdgHandle *handle);

/** Update the data cache.
  * This should not be done frequently as it reallocates the cache.
  * Even if updating the cache fails the handle remains valid and can
  * be used to access XDG data as it was before xdgUpdateData() was called.
  * @return 0 if update failed, non-0 if successful.*/
int xdgUpdateData(xdgHandle *handle);

/*@}*/
/** @name Basic XDG Base Directory Queries */
/*@{*/

/** Base directory for user specific data files.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return a path as described by the standards. */
const char * xdgDataHome(xdgHandle *handle);

/** Base directory for user specific configuration files.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return a path as described by the standards. */
const char * xdgConfigHome(xdgHandle *handle);

/** Preference-ordered set of base directories to search for data files
  * in addition to the $XDG_DATA_HOME base directory.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return A null-terminated list of directory strings. */
const char * const * xdgDataDirectories(xdgHandle *handle);

/** Preference-ordered set of base directories to search for data files
  * with $XDG_DATA_HOME prepended.
  * The base directory defined by $XDG_DATA_HOME is considered more
  * important than any of the base directories defined by $XDG_DATA_DIRS.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return A null-terminated list of directory strings. */
const char * const * xdgSearchableDataDirectories(xdgHandle *handle);

/** Preference-ordered set of base directories to search for configuration
  * files in addition to the $XDG_CONFIG_HOME base directory.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return A null-terminated list of directory strings. */
const char * const * xdgConfigDirectories(xdgHandle *handle);

/** Preference-ordered set of base directories to search for configuration
  * files with $XDG_CONFIG_HOME prepended.
  * The base directory defined by $XDG_CONFIG_HOME is considered more
  * important than any of the base directories defined by $XDG_CONFIG_DIRS.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return A null-terminated list of directory strings. */
const char * const * xdgSearchableConfigDirectories(xdgHandle *handle);

/** Base directory for user specific non-essential data files.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return a path as described by the standards. */
const char * xdgCacheHome(xdgHandle *handle);

/** Base directory for user specific non-essential runtime files such as
  * sockets and named pipes.
  * @param handle Handle to data cache, initialized with xdgInitHandle().
  * @return a path as described by the standards, or NULL if no path has been
  * set. */
const char * xdgRuntimeDirectory(xdgHandle *handle);

/*@}*/

#ifdef __cplusplus
} // extern "C"
#endif

#endif /*XDG_BASEDIR_H*/
