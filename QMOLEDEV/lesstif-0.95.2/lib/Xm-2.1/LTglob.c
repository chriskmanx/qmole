/**
 * $Id: LTglob.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $
 *
 * derived from GNU libc glob.c
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Id: LTglob.c,v 1.1 2004/08/28 19:22:44 dannybackx Exp $";

#ifdef WIN32
#include <windows.h>
#endif

#include <LTconfig.h>

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stddef.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#ifndef POSIX
#ifdef	_POSIX_VERSION
#define	POSIX
#endif
#endif
#endif

#include <X11/Intrinsic.h>  /* for Xt*alloc() */

#ifdef WIN32


/* Thank you Microsoft! - None of the dirent stuff is defined on Windows 
 * and a lot of what is defined has underscores in front of the names.
 **/
#include <sys/stat.h>
#define stat _stat
#define S_IFMT	_S_IFMT
#define S_IFDIR	_S_IFDIR

#else

#if	defined (POSIX) || defined (HAVE_DIRENT_H) || defined (__GNU_LIBRARY__)
#include <dirent.h>
#ifndef	__GNU_LIBRARY__
#define D_NAMLEN(d) strlen((d)->d_name)
#else /* GNU C library.  */
#define D_NAMLEN(d) ((d)->d_namlen)
#endif /* Not GNU C library.  */
#else /* Not POSIX or HAVE_DIRENT_H.  */
#define direct dirent
#define D_NAMLEN(d) ((d)->d_namlen)
#ifdef	HAVE_SYS_NDIR_H
#include <sys/ndir.h>
#endif /* HAVE_SYS_NDIR_H */
#ifdef	HAVE_SYS_DIR_H
#include <sys/dir.h>
#endif /* HAVE_SYS_DIR_H */
#ifdef HAVE_NDIR_H
#include <ndir.h>
#endif /* HAVE_NDIR_H */
#endif /* POSIX or HAVE_DIRENT_H or __GNU_LIBRARY__.  */
#endif /* !WIN32 */

#if defined (POSIX) && !defined (__GNU_LIBRARY__)
/* Posix does not require that the d_ino field be present, and some
   systems do not provide it. */
#define REAL_DIR_ENTRY(dp) 1
#else
#define REAL_DIR_ENTRY(dp) (dp->d_ino != 0)
#endif /* POSIX */

#include <XmI/LTglob.h>

#include <XmI/DebugUtil.h>


/*
   rws 5 Nov 1998
   IMO a link to a directory is still a directory.
   We shouldn't care whether it is a link or not, therefore I think stat is
   the correct call here, whether we have lstat or not.  If I'm wrong, write
   a test and prove it by doing a FileSelectionSearch on a directory with
   links to directories and comparing the results.

#  define LTstat lstat
*/

#define LTstat stat

#ifdef STAT_MACROS_BROKEN
#  undef S_ISDIR
#endif
#ifndef S_ISDIR
#  define S_ISDIR(mode) (((mode) & S_IFMT) == S_IFDIR)
#endif


#if 1
/* amai: We don't use external hooks. If you want to do so -
   also change <LTglob.h> */
static void *(*__glob_opendir_hook) (const char *directory)=NULL;
static const char *(*__glob_readdir_hook) (void * stream)=NULL;
static void (*__glob_closedir_hook) (void * stream)=NULL;
#else
void *(*__glob_opendir_hook) (const char *directory);
const char *(*__glob_readdir_hook) (void * stream);
void (*__glob_closedir_hook) (void * stream);
#endif /* */

/* just for convenience: */
typedef int (* errfunc_t)(const char *, int);

static int glob_pattern_p (const char *pattern, int quote);
static int glob_in_dir (const char *pattern, const char *directory,
                        int flags, errfunc_t errfunc, glob_t *pglob);
static int prefix_array (const char *prefix, char **array, size_t n);
static int collated_compare (const void *, const void *);




/* Match STRING against the filename pattern PATTERN, returning zero if
   it matches, nonzero if not.
   Derived from GNU libc fnmatch.c  */
static int
_Lesstif_fnmatch(const char *pattern,
		 const char *string,
		 int flags)
{
    register const char *p = pattern, *n = string;
    register char c;

/* Note that this evalutes C many times.  */
#define FOLD(c)	((flags & FNM_CASEFOLD) && isupper (c) ? tolower (c) : (c))

    while ((c = *p++) != '\0')
    {
	c = FOLD(c);

	switch (c)
	{
	case '?':
	    if (*n == '\0')
		return FNM_NOMATCH;
	    else if ((flags & FNM_FILE_NAME) && *n == '/')
		return FNM_NOMATCH;
	    else if ((flags & FNM_PERIOD) && *n == '.' &&
		     (n == string || ((flags & FNM_FILE_NAME) && n[-1] == '/')))
		return FNM_NOMATCH;
	    break;

	case '\\':
	    if (!(flags & FNM_NOESCAPE))
	    {
		c = *p++;
		c = FOLD(c);
	    }
	    if (FOLD(*n) != c)
		return FNM_NOMATCH;
	    break;

	case '*':
	    if ((flags & FNM_PERIOD) && *n == '.' &&
		(n == string || ((flags & FNM_FILE_NAME) && n[-1] == '/')))
		return FNM_NOMATCH;

	    for (c = *p++; c == '?' || c == '*'; c = *p++, ++n)
		if (((flags & FNM_FILE_NAME) && *n == '/') ||
		    (c == '?' && *n == '\0'))
		    return FNM_NOMATCH;

	    if (c == '\0')
		return 0;

	    {
		char c1 = (!(flags & FNM_NOESCAPE) && c == '\\') ? *p : c;
		c1 = FOLD(c1);
		for (--p; *n != '\0'; ++n)
		    if ((c == '[' || FOLD(*n) == c1) &&
			_Lesstif_fnmatch(p, n, flags & ~FNM_PERIOD) == 0)
			return 0;
		return FNM_NOMATCH;
	    }

	case '[':
	    {
		/* Nonzero if the sense of the character class is inverted.  */
		register int c_not;

		if (*n == '\0')
		    return FNM_NOMATCH;

		if ((flags & FNM_PERIOD) && *n == '.' &&
		    (n == string || ((flags & FNM_FILE_NAME) && n[-1] == '/')))
		    return FNM_NOMATCH;

		c_not = (*p == '!' || *p == '^');
		if (c_not)
		    ++p;

		c = *p++;
		for (;;)
		{
		    register char cstart = c, cend = c;

		    if (!(flags & FNM_NOESCAPE) && c == '\\')
			cstart = cend = *p++;

		    cstart = cend = FOLD(cstart);

		    if (c == '\0')
			/* [ (unterminated) loses.  */
			return FNM_NOMATCH;

		    c = *p++;
		    c = FOLD(c);

		    if ((flags & FNM_FILE_NAME) && c == '/')
			/* [/] can never match.  */
			return FNM_NOMATCH;

		    if (c == '-' && *p != ']')
		    {
			cend = *p++;
			if (!(flags & FNM_NOESCAPE) && cend == '\\')
			    cend = *p++;
			if (cend == '\0')
			    return FNM_NOMATCH;
			cend = FOLD(cend);

			c = *p++;
		    }

		    if (FOLD(*n) >= cstart && FOLD(*n) <= cend)
			goto matched;

		    if (c == ']')
			break;
		}
		if (!c_not)
		    return FNM_NOMATCH;
		break;

	      matched:;
		/* Skip the rest of the [...] that already matched.  */
		while (c != ']')
		{
		    if (c == '\0')
			/* [... (unterminated) loses.  */
			return FNM_NOMATCH;

		    c = *p++;
		    if (!(flags & FNM_NOESCAPE) && c == '\\')
			/* XXX 1003.2d11 is unclear if this is right.  */
			++p;
		}
		if (c_not)
		    return FNM_NOMATCH;
	    }
	    break;

	default:
	    if (c != FOLD(*n))
		return FNM_NOMATCH;
	}

	++n;
    }

    if (*n == '\0')
	return 0;

    if ((flags & FNM_LEADING_DIR) && *n == '/')
	/* The FNM_LEADING_DIR flag says that "foo*" matches "foobar/frobozz".  */
	return 0;

    return FNM_NOMATCH;
} /* _Lesstif_fnmatch() */



/* Do glob searching for PATTERN, placing results in PGLOB.
   The bits defined above may be set in FLAGS.
   If a directory cannot be opened or read and ERRFUNC is not nil,
   it is called with the pathname that caused the error, and the
   `errno' value from the failing call; if it returns non-zero
   `glob' returns GLOB_ABEND; if it returns zero, the error is ignored.
   If memory cannot be allocated for PGLOB, GLOB_NOSPACE is returned.
   Otherwise, `glob' returns zero.
 */
int
_Lesstif_glob(const char *pattern, int flags,
	      errfunc_t errfunc, glob_t *pglob)
{
    char *dirname;
    const char *filename;
    size_t dirlen;
    int status;
    int oldcount;

    if (pattern == NULL || pglob == NULL || (flags & ~__GLOB_FLAGS) != 0)
    {
	errno = EINVAL;
	return -1;
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_Lesstif_glob(%s, %i)\n", pattern, flags));

    /* Find the filename.  */
    filename = strrchr(pattern, '/');
    if (filename == NULL)
    {
        dirname=XtMalloc(2);
	filename = pattern;
	strcpy(dirname, ".");
	dirlen = 0;
    }
    else if (filename == pattern)
    {
	/* "/pattern".  */
        dirname=XtMalloc(2);
	strcpy(dirname, "/");
	dirlen = 1;
	++filename;
    }
    else
    {
	dirlen = filename - pattern;
	dirname = XtMalloc(dirlen + 1);
	memcpy(dirname, pattern, dirlen);
	dirname[dirlen] = '\0';
	++filename;
    }

    if (filename[0] == '\0' && dirlen > 1)
	/* "pattern/".  Expand "pattern", appending slashes.  */
    {
	int val = _Lesstif_glob(dirname, flags | GLOB_MARK, errfunc, pglob);
	if (val == 0)
	    pglob->gl_flags = (pglob->gl_flags & ~GLOB_MARK) |
		(flags & GLOB_MARK);
	XtFree(dirname);
	return val;
    }

    if (!(flags & GLOB_APPEND))
    {
	pglob->gl_pathc = 0;
	pglob->gl_pathv = NULL;
    }

    oldcount = pglob->gl_pathc;

    if (glob_pattern_p(dirname, !(flags & GLOB_NOESCAPE)))
    {
	/* The directory name contains metacharacters, so we
	   have to glob for the directory, and then glob for
	   the pattern in each directory found.  */
	glob_t dirs;
	register int i;

        DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "glob_pattern(%s, !(flags & GLOB_NOESCAPE))!=NULL\n", dirname));
	status = _Lesstif_glob(dirname,
			 ((flags & (GLOB_ERR | GLOB_NOCHECK | GLOB_NOESCAPE)) |
			  GLOB_NOSORT),
			  errfunc, &dirs);
	if (status != 0) {
     	    XtFree(dirname);
	    return status;
        }

	/* We have successfully globbed the preceding directory name.
	   For each name we found, call glob_in_dir on it and FILENAME,
	   appending the results to PGLOB.  */
	for (i = 0; i < dirs.gl_pathc; ++i)
	{
	    int oldcount;

	    oldcount = pglob->gl_pathc;
	    status = glob_in_dir(filename, dirs.gl_pathv[i],
				 (flags | GLOB_APPEND) & ~GLOB_NOCHECK,
				 errfunc, pglob);
	    if (status == GLOB_NOMATCH)
		/* No matches in this directory.  Try the next.  */
		continue;

	    if (status != 0)
	    {
		_Lesstif_globfree(&dirs);
		_Lesstif_globfree(pglob);
		XtFree(dirname);
		return status;
	    }

	    /* Stick the directory on the front of each name.  */
	    if (prefix_array(dirs.gl_pathv[i],
			     &pglob->gl_pathv[oldcount],
			     pglob->gl_pathc - oldcount))
	    {
		_Lesstif_globfree(&dirs);
		_Lesstif_globfree(pglob);
		XtFree(dirname);
		return GLOB_NOSPACE;
	    }
	}

	flags |= GLOB_MAGCHAR;

	if (pglob->gl_pathc == oldcount)
	{
	    /* No matches.  */
	    if (flags & GLOB_NOCHECK)
	    {
		size_t len = strlen(pattern) + 1;
		char *patcopy = (char *)XtMalloc(len);
		if (patcopy == NULL) {
		    XtFree(dirname);
		    return GLOB_NOSPACE;
		}
		memcpy(patcopy, pattern, len);

		pglob->gl_pathv =
		    (char **)XtRealloc((char *)pglob->gl_pathv,
				       (pglob->gl_pathc +
					((flags & GLOB_DOOFFS) ?
					 pglob->gl_offs : 0) +
					1 + 1) *
				       sizeof(char *));
		if (pglob->gl_pathv == NULL)
		{
		    XtFree(patcopy);
		    XtFree(dirname);
		    return GLOB_NOSPACE;
		}

		if (flags & GLOB_DOOFFS)
		    while (pglob->gl_pathc < pglob->gl_offs)
			pglob->gl_pathv[pglob->gl_pathc++] = NULL;

		pglob->gl_pathv[pglob->gl_pathc++] = patcopy;
		pglob->gl_pathv[pglob->gl_pathc] = NULL;
		pglob->gl_flags = flags;
	    }
	    else {
	        XtFree(dirname);
		return GLOB_NOMATCH;
	    }
	}
    } /* if (glob_pattern_p(dirname, !(flags & GLOB_NOESCAPE))) */
    else
    {
        DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "glob_pattern(%s, !(flags & GLOB_NOESCAPE))=NULL\n", dirname));

	status = glob_in_dir(filename, dirname, flags, errfunc, pglob);
	if (status != 0) {
 	    XtFree(dirname);
	    return status;
	}

	if (dirlen > 0)
	{
	    /* Stick the directory on the front of each name.  */
	    if (prefix_array(dirname,
			     &pglob->gl_pathv[oldcount],
			     pglob->gl_pathc - oldcount))
	    {
		_Lesstif_globfree(pglob);
		XtFree(dirname);
		return GLOB_NOSPACE;
	    }
	}
    } /* else */

    if (!(flags & GLOB_NOSORT))
	/* Sort the vector.  */
	qsort((void *) & pglob->gl_pathv[oldcount],
	      pglob->gl_pathc - oldcount,
	      sizeof(char *), collated_compare);

    if (flags & GLOB_MARK)
    {
	/* Append slashes to directory names.  glob_in_dir has already
	   allocated the extra character for us.

	   This must be done after the sorting to avoid screwing up the
	   order (`"./" > "../"' but `"." < ".."'). */
	int i;
	struct stat st;

        DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_Lesstif_glob(): GLOB_NOSORT\n"));
	for (i = oldcount; i < pglob->gl_pathc; ++i)
	    if (LTstat(pglob->gl_pathv[i], &st) == 0 &&
		S_ISDIR(st.st_mode))
		strcat(pglob->gl_pathv[i], "/");
    }
    XtFree(dirname);
    return 0;
} /* _Lesstif_glob() */


/* Free storage allocated in PGLOB by a previous `glob' call.  */
void
_Lesstif_globfree(register glob_t *pglob)
{
    if (pglob->gl_pathv != NULL)
    {
	register int i;

	for (i = 0; i < pglob->gl_pathc; ++i)
	    if (pglob->gl_pathv[i] != NULL)
		XtFree((char *) pglob->gl_pathv[i]);
	XtFree((char *) pglob->gl_pathv);
    }
}


/* Do a collated comparison of A and B.  */
static int
collated_compare(const void * a, const void * b)
{
    const char *const s1 = *(const char *const *const)a;
    const char *const s2 = *(const char *const *const)b;

    if (s1 == s2)
	return 0;
    if (s1 == NULL)
	return 1;
    if (s2 == NULL)
	return -1;
    return strcoll(s1, s2);
}


/* Prepend DIRNAME to each of N members of ARRAY, replacing ARRAY's
   elements in place.  Return nonzero if out of memory, zero if successful.
   A slash is inserted between DIRNAME and each elt of ARRAY,
   unless DIRNAME is just "/".  Each old element of ARRAY is freed.
 */
static int
prefix_array(const char *dirname, char **array, size_t n)
{
    register size_t i;
    size_t dirlen = strlen(dirname);

    if (dirlen == 1 && dirname[0] == '/')
	/* DIRNAME is just "/", so normal prepending would get us "//foo".
	   We want "/foo" instead, so don't prepend any chars from DIRNAME.  */
	dirlen = 0;

    for (i = 0; i < n; ++i)
    {
	size_t eltlen = strlen(array[i]) + 1;
	/*
	 * MLM This is buggy.  This doesn't retain the extra byte allocated
	 * by glob_in_dir if the element is a directory.  We make it so that
	 * all elements get the extra byte.
	 */
	char *new_w = (char *)XtMalloc(dirlen + 2 + eltlen);
	if (new_w == NULL)
	{
	    while (i > 0)
		XtFree((char *) array[--i]);
	    return 1;
	}

	memcpy(new_w, dirname, dirlen);
	new_w[dirlen] = '/';
	memcpy(&new_w[dirlen + 1], array[i], eltlen);
	XtFree((char *) array[i]);
	array[i] = new_w;
    }

    return 0;
}


/* Return nonzero if PATTERN contains any metacharacters.
   Metacharacters can be quoted with backslashes if QUOTE is nonzero.  */
static int
glob_pattern_p(const char *pattern, int quote)
{
    register const char *p;
    int open = 0;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "glob_pattern_p(%s, %i)\n", pattern, quote));
    for (p = pattern; *p != '\0'; ++p)
	switch (*p)
	{
	case '?':
	case '*':
	    return 1;

	case '\\':
	    if (quote)
		++p;
	    break;

	case '[':
	    open = 1;
	    break;

	case ']':
	    if (open)
		return 1;
	    break;
	}

    return 0;
} /* glob_pattern_p() */


/* Like `glob', but PATTERN is a final pathname component,
   and matches are searched for in DIRECTORY.
   The GLOB_NOSORT bit in FLAGS is ignored.  No sorting is ever done.
   The GLOB_APPEND flag is assumed to be set (always appends).
 */
static int
glob_in_dir(const char *pattern, const char *directory, int flags,
	    errfunc_t errfunc, glob_t *pglob)
{
#ifdef WIN32
   HANDLE stream;
   WIN32_FIND_DATA findinfo;
   int first = 1;
#else
    void *stream;
#endif
    struct globlink
    {
	struct globlink *next;
	char *name;
    };
    struct globlink *names = NULL;
    size_t nfound = 0;


    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "glob_in_dir(%s, %s, %i,)\n",
		      pattern, directory, flags));    
    if (!glob_pattern_p(pattern, !(flags & GLOB_NOESCAPE)))
    {
	stream = NULL;
	flags |= GLOB_NOCHECK;
    }
    else
    {
	flags |= GLOB_MAGCHAR;

#ifdef WIN32
	stream = FindFirstFile(directory, &findinfo);
	if (stream == INVALID_HANDLE_VALUE) stream = NULL;
#else
	stream = (__glob_opendir_hook ? (*__glob_opendir_hook) (directory)
		  : (void *) opendir(directory));
#endif
	if (stream == NULL)
	{
	    if ((errfunc != NULL && (*errfunc) (directory, errno)) ||
		(flags & GLOB_ERR))
		return GLOB_ABEND;
	}
	else
	    while (1)
	    {
		const char *name;
		size_t len;

#ifdef WIN32
		if (first) 
		{
		   /* The FindFirstFile call actaully reads the first one */
		   first = 0;
	   	}
		else 
		{
		   if (!FindNextFile(stream, &findinfo)) break;
		}
		name = findinfo.cFileName;
		len = 0;

#else
		if (__glob_readdir_hook)
		{
		    name = (*__glob_readdir_hook) (stream);
		    if (name == NULL)
			break;
		    len = 0;
		}
		else
		{
		    struct dirent *d = readdir((DIR *) stream);
		    if (d == NULL)
			break;

		    DEBUGOUT(_LtDebug0(__FILE__, NULL, "glob_in_dir(%s)\n", d->d_name));

		    if (!REAL_DIR_ENTRY(d)) {
			DEBUGOUT(_LtDebug0(__FILE__, NULL, "glob_in_dir() continue\n"));
			continue;
		    }
		    name = d->d_name;
#ifdef	HAVE_D_NAMLEN
		    len = d->d_namlen;
#else
		    len = 0;
#endif
		}
#endif

		if (_Lesstif_fnmatch(pattern, name,
				     (!(flags & GLOB_PERIOD) ? FNM_PERIOD : 0) |
			    ((flags & GLOB_NOESCAPE) ? FNM_NOESCAPE : 0)) == 0)
		{
		    struct globlink *new_w
		    = (struct globlink *)XtMalloc(sizeof(struct globlink));

		    if (len == 0)
			len = strlen(name);
		    new_w->name
			= (char *)XtMalloc(len + ((flags & GLOB_MARK) ? 1 : 0) + 1);
		    if (new_w->name == NULL)
			goto memory_error;
		    memcpy((void *) new_w->name, name, len);
		    new_w->name[len] = '\0';
		    new_w->next = names;
		    names = new_w;
		    ++nfound;
		} else {
		    DEBUGOUT(_LtDebug0(__FILE__, NULL, "glob_in_dir() failed fnmatch\n"));
		}
	    }
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "glob_in_dir() found %d\n", nfound));

    if (nfound == 0 && (flags & GLOB_NOCHECK))
    {
	size_t len = strlen(pattern);
	nfound = 1;
	names = (struct globlink *)XtMalloc(sizeof(struct globlink));
	names->next = NULL;
	names->name = (char *)XtMalloc(len + 1);
	if (names->name == NULL)
	    goto memory_error;
	memcpy(names->name, pattern, len);
	names->name[len] = '\0';
    }

    pglob->gl_pathv
	= (char **)XtRealloc((char *)pglob->gl_pathv,
			     (pglob->gl_pathc +
			      ((flags & GLOB_DOOFFS) ? pglob->gl_offs : 0) +
			      nfound + 1) *
			     sizeof(char *));
    if (pglob->gl_pathv == NULL)
	goto memory_error;

    if (flags & GLOB_DOOFFS)
	while (pglob->gl_pathc < pglob->gl_offs)
	    pglob->gl_pathv[pglob->gl_pathc++] = NULL;

    while (names != NULL) {
	struct globlink *next = names->next;
	pglob->gl_pathv[pglob->gl_pathc++] = names->name;
	XtFree((char *)names);
	names = next;
    }
    pglob->gl_pathv[pglob->gl_pathc] = NULL;

    pglob->gl_flags = flags;

    if (stream != NULL)
    {
	int save = errno;

#ifdef WIN32
	FindClose(stream);
#else
	if (__glob_closedir_hook)
	    (*__glob_closedir_hook) (stream);
	else
	    closedir((DIR *) stream);
#endif
	errno = save;
    }

    return nfound == 0 ? GLOB_NOMATCH : 0;

  memory_error:
    {
	int save = errno;

#ifdef WIN32
	FindClose(stream);
#else
	if (__glob_closedir_hook)
	    (*__glob_closedir_hook) (stream);
	else
	    closedir((DIR *) stream);
#endif
	errno = save;
    }
    while (names != NULL)
    {
	if (names->name != NULL)
	    XtFree((char *) names->name);
	names = names->next;
    }
    if (names) XtFree((char *)names);
    return GLOB_NOSPACE;
} /* glob_in_dir() */
