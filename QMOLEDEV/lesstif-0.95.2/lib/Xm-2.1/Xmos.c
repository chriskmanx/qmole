/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Xmos.c,v 1.2 2004/10/20 19:32:11 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Id: Xmos.c,v 1.2 2004/10/20 19:32:11 dannybackx Exp $";

#ifdef WIN32
#include <windows.h>
#endif

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#ifdef TIME_WITH_SYS_TIME
#include <time.h>
#endif
#endif
#if !defined(HAVE_SLEEP) || !defined(HAVE_USLEEP)
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/XmosP.h>
#include <XmI/LTglob.h>
#include <XmI/LTmisc.h>


#ifdef __EMX__   /* -> OS/2 drive letters support */
#define  INCL_DOS
#define  INCL_DOSMISC
#define  INCL_ERRORS
#include <os2.h>
static const char *GetDriveMap(void);
#endif

#include <XmI/DebugUtil.h>


/*
 * this is OS dependent, but this should catch most OS's.
 */
#define MAX_PATH_LEN   2048


/* some default settings */
char _XmSDEFAULT_FONT[] = "fixed";
char _XmSDEFAULT_BACKGROUND[] = "Blue";


/*
 * There are actually two functions, one for seconds and one
 * for microseconds.  The semantics are identical to the well-known
 * sleep() and usleep().  Neither continues after a signal.  - Jamie
 *
 * amai: Funny, but the often problematic select() call 
 *       is meanwhile the fallback ...
 */

extern void
_XmSleep(unsigned int secs)
{

#ifdef HAVE_SLEEP
    sleep(secs);
#else
    struct timeval tv;

    tv.tv_sec  = secs;
    tv.tv_usec = 0;
    select(0, NULL, NULL, NULL, &tv);
#endif
    return;
}


extern int
_XmMicroSleep(long usecs)
{

#ifdef HAVE_USLEEP
    usleep(usecs);
    return 0;
#else
    struct timeval tv;

    tv.tv_sec  = 0;
    tv.tv_usec = usecs;
    return select(0, NULL, NULL, NULL, &tv);
#endif
}


extern XmString
_XmOSGetLocalizedString(char *reserved, Widget w, String resourceName, String value)
{
    return XmStringCreateLocalized(value);
}


extern XmString
XmeGetLocalizedString(char *reserved, Widget widget, char *resource, String string)
{
    return _XmOSGetLocalizedString(reserved, widget, resource, string);
}


/*
 * find the pattern part in a fileSpec
 * Rules I've been able to get out of M*tif
 * 1) If a spec ends in '/', return the remaining null string.
 * 2) Ordinarily, return either a) the first path component without
 * an escaped wildcard, or b) the last component that doesn't end in '/'.
 * 3) Escaped wildcards ('\') will be ignored.
 */
extern String
_XmOSFindPatternPart(String fileSpec)
{
    static const char wildcards[] = "*?[]+/";
    String ret, tmp;

    /*
     * Wendell Duncan <duncan@mercury.chem.utah.edu> says stripping off the
     * directory information helps : now directories with some of the
     * traditional wildcard characters in their names will behave as expected.
     */
    if (strlen(fileSpec)) {
	if ((ret = strrchr(fileSpec, '/')))
		fileSpec = ret + 1;
    }

    for (;;)
    {
	tmp = fileSpec;
retry:
	if (strlen(tmp) == 0)
	{
	    return fileSpec;
	}
	if ((ret = strpbrk(tmp, wildcards)) == NULL)
	{
	    return fileSpec;
	}
	else
	{
	    if (ret > tmp && ret[-1] == '\\')
	    {
		tmp = ret + 1;
		goto retry;
	    }
	    else if (*ret == '/')
	    {
		fileSpec = ret + 1;
		continue;
	    }

	    while ((ret > fileSpec) && (*ret != '/'))
	    {
		ret--;
	    }

	    if (*ret == '/')
	    {
		ret++;
	    }

	    return ret;
	}
    }

}


extern String
_XmOSGetHomeDirName(void)
{
    static char *home=NULL;

    /* Only actually check for the directory the first time.
     * We just keep it around for future calls.
     */

    if (!home)
    {
	char *estr;

	/* First, try the environment.
	 * If that fails, look in the passwd file.
	 * If nothing there, return NULL - the caller must handle this.
	 */

	estr = getenv("HOME");
	if (estr)
	   home = XtNewString(estr);
#if defined(HAVE_GETPWUID) && defined(HAVE_GETUID)
	else
	{
           struct passwd *pw;

           if ((estr = getenv("LOGNAME")) && (pw = getpwuid(getuid())))
              home = XtNewString(estr);
           else if ((estr = getenv("USER")) && (pw = getpwuid(getuid())))
              home = XtNewString(estr);
           else if ((pw = getpwuid(getuid())))
              home = XtNewString(pw->pw_dir);
        }
#endif

        if (!home)
           home = XtNewString("");
    }

    return home;
}


String
XmeGetHomeDirName(void)
{
    return _XmOSGetHomeDirName();
}

static Boolean
startsWithTwiddle(String dir)
{
    char *ptr;

    for (ptr = dir; *ptr && isspace(*ptr); ptr++)
    {
    }

    return *ptr == '~';
}


static String
convertTwiddle(char *dir)
{
    String ptr, ptr1;
    String newdir, home;

    for (ptr = dir; *ptr && isspace(*ptr); ptr++)
    {
    }
    ptr++;			/* to get by twiddle */

    ptr1 = strchr(ptr, '/');
    if (ptr == ptr1)
    {
	home = XtNewString(_XmOSGetHomeDirName());
    }
    else
    {
	String name, ptr2;
#if defined(HAVE_GETPWNAM)
	struct passwd *pw;
#endif

    	name = XtNewString(ptr);
      ptr2=strchr(name, '/');
      if (ptr2)
         *ptr2='\0';
#if defined(HAVE_GETPWNAM)
    	pw = getpwnam(name);
    	if (pw)
    	{
	    home = XtNewString(pw->pw_dir);
    	}
    	else
#endif
    	{
	    home = XtNewString("/");
    	}
    	XtFree(name);
    }
    newdir = XtMalloc(strlen(home) + strlen(ptr1) + 1);
    strcpy(newdir, home);
    strcat(newdir, ptr1);
    
    XtFree(home);

    return newdir;
}

/*
 * qualify a file/dir spec
 * Rules I've learned from Motif:
 * 1) If dirSpec has wildcards, this routine won't try to remove them.  So
 * wildcards had better not get here.
 * 2) If the dirSpec is all dots, or combinations of dots, the routine will
 * resolve the dots relative to the current directory and return that.
 */
extern void
_XmOSQualifyFileSpec(String dirSpec,
                     String filterSpec,
                     String *pQualifiedDir,
                     String *pQualifiedPattern)
{
    String dir, filt;
    char *tmp, *tmp2;

    if (!dirSpec) 
       dirSpec = "";
    /* no dir string? get cwd */
    if (strlen(dirSpec) == 0 ||
	strncmp(dirSpec, ".", 1) == 0 || strncmp(dirSpec, "..", 2) == 0)
    {
#if HAVE_GETCWD
      tmp=XtMalloc(MAX_PATH_LEN);
      /* this is not proper POSIX coding:
         MAX_PATH_LEN is by now means an upper limit, so we may
         actually miss to resolve the CWD */
	if (getcwd(tmp, MAX_PATH_LEN) != NULL) 
	{
      }
      else
      {
	    XtFree(tmp);
#else
	{
#endif
	    tmp = XtNewString(_XmOSGetHomeDirName());
      }

	dir = (String)XtMalloc(strlen(tmp) + 1);
	strcpy(dir, tmp);
	XtFree(tmp);

	while (strncmp(dirSpec, "..", 2) == 0)
	{
	    tmp = dir + strlen(dir);
	    while (tmp > dir && *tmp != '/')
	    {
		tmp--;
	    }
	    if (*tmp == '/')
	    {
		*tmp = 0;
	    }
	    dirSpec += 2;
	    if (*dirSpec == '/')
	    {
		dirSpec++;
	    }
	}
	while (strncmp(dirSpec, ".", 2) == 0)
	{
	    dirSpec++;
	    if (*dirSpec == '/')
	    {
		dirSpec++;
	    }
	}
    }
    /* otherwise, we have a dir */
    else
    {
	dir = (String)XtMalloc(strlen(dirSpec) + 1);
	strcpy(dir, dirSpec);
    }

    /* if the dir doesn't terminate in a '/', add one */
    if (dir[strlen(dir) - 1] != '/')
    {
	dir = (String)XtRealloc(dir, strlen(dir) + 2);
	strcat(dir, "/");
    }
    if (startsWithTwiddle(dir))
    {
	char *newdir;

	newdir = convertTwiddle(dir);
	XtFree(dir);
	dir = newdir;
    }

    /* if the dir doesn't start in a '/', add the cwd */
    if (dir[0] != '/')
    {
	String tmp, tmp1;

	if ((tmp = (char *)getcwd(NULL, MAX_PATH_LEN)) == NULL)
	    tmp = XtNewString(_XmOSGetHomeDirName());

	tmp1 = XtMalloc(strlen(tmp) + strlen(dir) + 2);
	strcpy(tmp1, tmp);
	strcat(tmp1,"/");
	strcat(tmp1,dir);
	XtFree(dir);
	dir = tmp1;
    }

    if (!filterSpec) filterSpec = "";
    /* if the filter starts with an absolute pathname, use it */
    if (*filterSpec == '/')
    {
	XtFree(dir);
	dir = XtNewString(filterSpec);
    }
    else if (startsWithTwiddle(dir))
    {
	char *newdir;

	newdir = convertTwiddle(dir);
	XtFree(dir);
	dir = newdir;
	dir = XtRealloc(dir, strlen(dir) + strlen(filterSpec) + 1);
	strcat(dir, filterSpec);
    }
    /* if the filter is empty, make it all files */
    else if (strlen(filterSpec) == 0)
    {
	dir = (String)XtRealloc(dir, strlen(dir) + 2);
	strcat(dir, "*");
    }
    else
    {
	/* now add the filter spec */
	dir = XtRealloc(dir, strlen(dir) + strlen(filterSpec) + 1);
	strcat(dir, filterSpec);
    }

    /* eat any "..", ".", or "//" left in the path */
    for (;;)
    {
	if ((tmp = strstr(dir, "/./")) != NULL)
	{
	    if (tmp == dir || (tmp > dir && tmp[-1] != '\\'))
	    {
		*tmp = 0;
		tmp2 = XtNewString(tmp + 2);

		strcat(dir, tmp2);

		XtFree(tmp2);

		continue;
	    }
	}
	else if ((tmp = strstr(dir, "/../")) != NULL)
	{
	    if (tmp == dir || (tmp > dir && tmp[-1] != '\\'))
	    {
		*tmp = 0;
		if ((tmp2 = strrchr(dir, '/')) != NULL)
		{
		    *tmp2 = 0;
		    tmp2 = XtNewString(tmp + 3);

		    strcat(dir, tmp2);

		    XtFree(tmp2);

		    continue;
		}
		else
		{
		    strcat(dir, tmp + 3);
		}
	    }
	}
	else if ((tmp = strstr(dir, "//")) != NULL)
	{
	    if (tmp == dir || (tmp > dir && tmp[-1] != '\\'))
	    {
		*tmp = 0;
		tmp2 = XtNewString(tmp + 1);

		strcat(dir, tmp2);

		XtFree(tmp2);

		continue;
	    }
	}
	else
	{
	    break;
	}
    }

    /* extract the last component for the filter spec */
    if ((tmp = strrchr(dir, '/')) != NULL)
    {
	if (strlen(tmp) != 0)
	{
	    tmp++;
	    filt = XtNewString(tmp);
	    *tmp = 0;
	}
	else
	    filt = XtNewString("*");
    }
    else
    {
	filt = dir;
	dir = XtNewString("/");
    }

    *pQualifiedDir = dir;
    *pQualifiedPattern = filt;
}


/*
 * Takes a string and converts /this/dir/. into /this/dir/
 * also converts /this/dir/.. into /this/
 */
static void
_XmOSGetDotDot(String s)
{
    int i, j;

    if (s == 0)
    {
	return;
    }

    for (i = 0; s[i]; i++)
    {
    }

    if (s[i - 1] == '.' && s[i - 2] == '.' && s[i - 3] == '/')
    {				/* parent dir */

	for (j = i - 3; j > 0 && s[j] != '/'; j--)
	{
	}

	for (j--; j > 0 && s[j] != '/'; j--)
	{
	}

	if (j >= 0 && s[j] == '/')
	{
	    s[j + 1] = '\0';
	}

    }
    else if (s[i - 1] == '.' && s[i - 2] == '/')
    {				/* this dir */

	for (j = i - 2; j > 0 && s[j] != '/'; j--)
	{
	}

	if (j >= 0 && s[j] == '/')
	{
	    s[j + 1] = '\0';
	}

    }
    /* else no action */
}


/*
 * does this operate on one dir, or many ? (later) One.
 * Motif apparently does it this way.  If someone can prove otherwise,
 * PLEASE send the offending output to me (miers@packet.net), and a
 * description of the problem.
 * FIX ME -- This is a hack.  It should understand locale (for * mapping).
 */
extern void
_XmOSGetDirEntries(String qualifiedDir,
		   String matchPattern,
		   unsigned char fileType,
		   Boolean matchDotsLiterally,
		   Boolean listWithFullPath,
		   String **pEntries,
		   unsigned int *pNumEntries,
		   unsigned int *pNumAlloc)
{
/* OS/2 status, pre 20010511:
    - no driver letters
    - no ".." in / dir list 
    (.. in file list in "/" !)
    */
 
    glob_t result;
    char buf[2048];
    int i, cnt, max;
    String slash, tmp, *ret = NULL;
    int flags = GLOB_MARK | (matchDotsLiterally ? 0 : GLOB_PERIOD);

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		   "_XmOSGetDirEntries(%s,%s)\n", qualifiedDir, matchPattern));

    _XmOSGetDotDot(qualifiedDir);
    _XmOSGetDotDot(matchPattern);

    if (strlen(matchPattern) == 0)
    {
	qualifiedDir = "*";
    }
    else if ((slash = strstr(matchPattern, "/")) != NULL)
    {
	if (slash > matchPattern && slash[-1] != '\\')
	{
	    tmp = XtMalloc(slash - qualifiedDir + 1);
	    memcpy(tmp, qualifiedDir, slash - qualifiedDir);
	    tmp[slash - qualifiedDir] = 0;
	    matchPattern = tmp;
	}
    }

    if (matchPattern[0] == '/')
    {
	strcpy(buf, matchPattern);
    }
    else
    {
	strcpy(buf, qualifiedDir);

	for (i = 0; buf[i]; i++)
	{
	}

	i--;

	if (buf[i] != '/')
	{
	    strcat(buf, "/");
	}

	strcat(buf, matchPattern);
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmOSGetDirEntries -> work on '%s'\n", buf));

    memset((void *)&result, 0, sizeof(result));

    i = _Lesstif_glob(buf, flags, NULL, &result);
    if (i)
    {
	return;
    }

    max = *pNumAlloc;
    if (!max)
    {
	max = 64;
	ret = (String *)XtCalloc(sizeof(String *), max);
    }
    for (i = 0, cnt = *pNumEntries; i < result.gl_pathc; i++)
    {

	if (cnt == max)
	{
	    max *= 2;
	    ret = (String *)XtRealloc((char *)ret, max * sizeof(String *));
	}

	if (fileType == XmFILE_ANY_TYPE)
	{

	    if (result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] == '/')
	    {
		result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] = 0;
	    }

	    if (listWithFullPath)
	    {
		if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
		{
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
		}
	    }
	    else
	    {
		if ((tmp = strrchr(result.gl_pathv[i], '/')) == NULL)
		{
		    _XmError(NULL, "No '/' in path!\n");
		}
		if ((ret[cnt] = XtNewString(tmp + 1)) == NULL)
		{
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
		}
	    }

	    cnt++;
	}
	else if (fileType == XmFILE_REGULAR &&
		 result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] != '/')
	{

	    if (listWithFullPath)
	    {
		if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
		{
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
		}
	    }
	    else
	    {
		if ((tmp = strrchr(result.gl_pathv[i], '/')) == NULL)
		{
		    _XmError(NULL, "No '/' in path!\n");
		}
		if ((ret[cnt] = XtNewString(tmp + 1)) == NULL)
		{
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
		}
	    }
	    cnt++;
	}
	else if (fileType == XmFILE_DIRECTORY &&
		 result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] == '/')
	{

	    if (result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] == '/')
	    {
		result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] = 0;
	    }

	    if (listWithFullPath)
	    {
		if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
		{
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
		}
	    }
	    else
	    {
		if ((tmp = strrchr(result.gl_pathv[i], '/')) == NULL)
		{
		    _XmError(NULL, "No '/' in path!\n");
		}
		if ((ret[cnt] = XtNewString(tmp + 1)) == NULL)
		{
		    _XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
		}
	    }

	    cnt++;
	}
    }

    _Lesstif_globfree(&result);
    *pNumAlloc = max;

    if (cnt == 0)
    {
	XtFree((char *)ret);
	*pEntries = NULL;
	*pNumEntries = 0;
    }
    else
    {
	*pNumEntries = cnt;
	*pEntries = ret;
    }

    if (_LtDebugInDebug(__FILE__, NULL))
    {
	int i;

	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "_XmOSGetDirEntries: %d results\n", cnt));
	for (i = 0; i < cnt; i++)
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL, "\t[%d] - %s\n", i, ret[i]));
	}
    }
}


extern void
_XmOSBuildFileList(String dirPath,
		   String pattern,
		   unsigned char typeMask,
		   String **pEntries,
		   unsigned int *pNumEntries,
		   unsigned int *pNumAlloc)
{
/* OS/2 status, pre 20010511:
    - .. in file list in "/" !
*/
    glob_t result;
    char buf[2048];
    int i, cnt, max;
    String *ret = NULL;
    int flags = GLOB_MARK | GLOB_PERIOD | GLOB_NOSORT;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "_XmOSBuildFileList(%s,%s)\n", dirPath, pattern));

    *pEntries = NULL;
    *pNumEntries = 0;
    *pNumAlloc = 0;

    _XmOSGetDotDot(dirPath);
    _XmOSGetDotDot(pattern);

    if (strlen(dirPath) == 0)
	dirPath = "*";

    if (pattern[0] == '/')
	strcpy(buf, pattern);
    else
    {
	strcpy(buf, dirPath);
	for (i = 0; buf[i]; i++);
	i--;
	if (buf[i] != '/')
	    strcat(buf, "/");
	strcat(buf, pattern);
    }

    i = _Lesstif_glob(buf, flags, NULL, &result);
    if (i) {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "WARNING: _XmOSBuildFileList() return after glob\n"));
	return;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL,
	"WARNING: _XmOSBuildFileList: Resetting initial list (glob count %d)\n",
	result.gl_pathc));

    *pNumAlloc = *pNumEntries = 0;
    *pEntries = NULL;

    max = *pNumAlloc;
    if (!max)
    {
	max = 64;
	ret = (String *)XtCalloc(sizeof(String *), max);
    }
    else
    {
    	ret = *pEntries;
    }

    for (i = 0, cnt = *pNumEntries; i < result.gl_pathc; i++)
    {

	if (cnt == max)
	{
	    max *= 2;
	    ret = (String *)XtRealloc((char *)ret, max * sizeof(String *));
	}

	if ((typeMask & XmFILE_REGULAR) &&
	    (result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] != '/'))
	{
	    if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
	    {
		_XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    }
	    cnt++;
	}

	if ((typeMask & XmFILE_DIRECTORY) &&
	    (result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] == '/'))
	{
	    if (result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] == '/')
	    {
		result.gl_pathv[i][strlen(result.gl_pathv[i]) - 1] = 0;
	    }
	    if ((ret[cnt] = XtNewString(result.gl_pathv[i])) == NULL)
	    {
		_XmError(NULL, "Out of memory in _XmOSGetDirEntries.");
	    }
	    cnt++;
	}
    }

    _Lesstif_globfree(&result);

    *pNumAlloc = max;

    if (cnt == 0)
    {
	XtFree((char *)ret);
	*pEntries = NULL;
	*pNumEntries = 0;
    }
    else
    {
	*pNumEntries = cnt;
	*pEntries = ret;
	/*
	qsort((void *)ret, cnt, sizeof(String), vstrcmp);
	*/
    }

    if (_LtDebugInDebug(__FILE__, NULL))
    {
	int i;

	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "_XmOSBuildFileList: %d results\n", cnt));
	for (i = 0; i < cnt; i++)
	{
	    DEBUGOUT(_LtDebug(__FILE__, NULL, "\t[%d] - %s\n", i, ret[i]));
	}
    }
}

/*
 * a sort function, perhaps?
 */
/* rws 3 Jul 1998
   Mozilla shows us what this is.  It is the comparison function used in
   qsort.
 */

extern int
_XmOSFileCompare(XmConst void *sp1, XmConst void *sp2)
{
    return strcmp(*((char **)sp1), *((char **)sp2));
}


/*
 * This one exists in Motif 1.2 (and above?)
 */
extern String
_XmOSInitPath(String file_name, String env_pathname, Boolean *user_path)
{
   _XmWarning(NULL, "_XmOSInitPath() is not yet implemented!");
   return (String)NULL;
}

extern int 
_XmOSPutenv(String env_string)
/* amai: "obviously" just a wrapper around a libc's putenv(3) call?! */
{
#ifdef HAVE_PUTENV
   return putenv(env_string);
#else
   /* amai: I have no idea how to fake a putenv() call on this
            level (outside libc or so ...) */
   _XmWarning(NULL, "_XmOSPutenv() is not implemented on this system!");
   return -1;
#endif
}


extern String 
_XmOSBuildFileName(String a, String b)
{
  _XmWarning(NULL, "_XmOSBuildFileName() is not yet implemented!");
  return (String)NULL;
}


#ifdef __EMX__
/*
   On OS/2 we may need to deal with drive letters.
   Question is whether this "manual" implementation or
   libcExt/POSIX2 will be faster in providing this feature ...
*/

static const char 
*GetDriveMap(void)
{
  /* returns a string containing a list of all drives: e.g. "C: D: E:" */

#define MAXDRIVELETTERS 26
  static char drivelist[3*MAXDRIVELETTERS +1] = "";
  static char tmpstr[4] = " : ";
  ULONG  CurDrive;
  ULONG  DriveMap;
  int    dnum;
  int    start = 3;  /* this should correspond to a hard-coded limit
                        to 'C', i.e. no check for floppy disks "A: B" ?! */
  static const char offset = 'A' -1;
  char *retstr;

  /* DosError(0); */

  DosQueryCurrentDisk(&CurDrive, &DriveMap);
  DriveMap>>=start-1;
  for (dnum = start; dnum <= MAXDRIVELETTERS; dnum++) {
     if ((DriveMap & (ULONG)1)) {
        tmpstr[0] = (char)dnum + offset;
        strcat(drivelist, tmpstr);
     }
      DriveMap>>=1; 
  } /* for dnum */

  /* cosmetics: strip trailing blank: */ 
  if (drivelist[strlen(drivelist)-1]== ' ')
    drivelist[strlen(drivelist)-1] = '\0';

  /* DosError(1); */

  retstr=(char *)malloc(strlen(drivelist)+1);
  strcpy(retstr, drivelist);
  return retstr;
}
#endif /* __EMX__ */
