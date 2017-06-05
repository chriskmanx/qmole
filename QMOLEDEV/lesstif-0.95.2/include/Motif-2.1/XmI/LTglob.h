/*
 * $Id: LTglob.h,v 1.1 2004/08/28 19:23:30 dannybackx Exp $ 
 *
 * Copyright (C) 1991, 1992 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
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


#ifndef	_LTGLOB_H
#define	_LTGLOB_H


/* Begin of "<LTfnmatch.h>" */
/* We #undef these before defining them because some losing systems
   (HP-UX A.08.07 for example) define these in <unistd.h>.  */
#undef	FNM_PATHNAME
#undef	FNM_NOESCAPE
#undef	FNM_PERIOD

/* Bits set in the FLAGS argument to `fnmatch'.  */
#define	FNM_PATHNAME	(1 << 0) /* No wildcard can ever match `/'.  */
#define	FNM_NOESCAPE	(1 << 1) /* Backslashes don't quote special chars.  */
#define	FNM_PERIOD	(1 << 2) /* Leading `.' is matched only explicitly.  */

#if !defined (_POSIX_C_SOURCE) || _POSIX_C_SOURCE < 2 || defined (_GNU_SOURCE)
#define	FNM_FILE_NAME	FNM_PATHNAME /* Preferred GNU name.  */
#define	FNM_LEADING_DIR	(1 << 3) /* Ignore `/...' after a match.  */
#define	FNM_CASEFOLD	(1 << 4) /* Compare without regard to case.  */
#endif

/* The 9 lines under this are to make us work on RedHat 5.0 which doesn't
 * seem to trigger the stuff above.
 * Remove these lines if they bother you; please send me mail about it
 * if this happens. <lesstif@lesstif.org>.
 * Danny 4/2/1998
 */
#ifndef	FNM_FILE_NAME
#define	FNM_FILE_NAME	FNM_PATHNAME /* Preferred GNU name.  */
#endif
#ifndef	FNM_LEADING_DIR
#define	FNM_LEADING_DIR	(1 << 3) /* Ignore `/...' after a match.  */
#endif
#ifndef	FNM_CASEFOLD
#define	FNM_CASEFOLD	(1 << 4) /* Compare without regard to case.  */
#endif

/* Value returned by `fnmatch' if STRING does not match PATTERN.  */
#define	FNM_NOMATCH	1

/* End of "<LTfnmatch.h>" */


/* Some system header files erroneously define these.
   We want our own definitions to take precedence.  */
#undef	GLOB_ERR
#undef	GLOB_MARK
#undef	GLOB_NOSORT
#undef	GLOB_DOOFFS
#undef	GLOB_NOCHECK
#undef	GLOB_APPEND
#undef	GLOB_NOESCAPE
#undef	GLOB_PERIOD

/* Bits set in the FLAGS argument to `glob'.  */
#define	GLOB_ERR	(1 << 0)/* Return on read errors.  */
#define	GLOB_MARK	(1 << 1)/* Append a slash to each name.  */
#define	GLOB_NOSORT	(1 << 2)/* Don't sort the names.  */
#define	GLOB_DOOFFS	(1 << 3)/* Insert PGLOB->gl_offs NULLs.  */
#define	GLOB_NOCHECK	(1 << 4)/* If nothing matches, return the pattern.  */
#define	GLOB_APPEND	(1 << 5)/* Append to results of a previous call.  */
#define	GLOB_NOESCAPE	(1 << 6)/* Backslashes don't quote metacharacters.  */
#define	GLOB_PERIOD	(1 << 7)/* Leading `.' can be matched by metachars.  */
#define	__GLOB_FLAGS	(GLOB_ERR|GLOB_MARK|GLOB_NOSORT|GLOB_DOOFFS| \
			 GLOB_NOESCAPE|GLOB_NOCHECK|GLOB_APPEND|GLOB_PERIOD)

/* This is not POSIX, so don't try to be too clever ... */
#ifndef GLOB_MAGCHAR
#define	GLOB_MAGCHAR    (1 << 8)   /* Set in gl_flags if any metachars seen. */
#endif

/* Error returns from `glob'.  */
#define	GLOB_NOSPACE	1	/* Ran out of memory.  */
#define	GLOB_ABEND	2	/* Read error.  */
#define	GLOB_NOMATCH	3	/* No matches found.  */

/* Structure describing a globbing run.  */
typedef struct
  {
    int gl_pathc;		/* Count of paths matched by the pattern.  */
    char **gl_pathv;		/* List of matched pathnames.  */
    int gl_offs;		/* Slots to reserve in `gl_pathv'.  */
    int gl_flags;		/* Set to FLAGS, maybe | GLOB_MAGCHAR.  */
  } glob_t;

/* Do glob searching for PATTERN, placing results in PGLOB.
   The bits defined above may be set in FLAGS.
   If a directory cannot be opened or read and ERRFUNC is not nil,
   it is called with the pathname that caused the error, and the
   `errno' value from the failing call; if it returns non-zero
   `glob' returns GLOB_ABEND; if it returns zero, the error is ignored.
   If memory cannot be allocated for PGLOB, GLOB_NOSPACE is returned.
   Otherwise, `glob' returns zero.  */
int _Lesstif_glob (const char *__pattern, int __flags,
                   int (*__errfunc) (const char *, int),
		   glob_t *__pglob);

/* Free storage allocated in PGLOB by a previous `glob' call.  */
void _Lesstif_globfree (glob_t *__pglob);


#if 0
/* We don't use these hooks ... */
/* If they are non-NULL our `glob()' uses these functions
   to read directories.  */
extern void * (*__glob_opendir_hook) (const char *__directory);
extern const char *(*__glob_readdir_hook) (void * __stream);
extern void (*__glob_closedir_hook) (void * __stream);
#endif

#endif /* _LTGLOB_H  */
