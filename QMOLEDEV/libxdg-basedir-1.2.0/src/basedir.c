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

/** @file basedir.c
  * @brief Implementation of the XDG Base Directory specification. */

#if defined(HAVE_CONFIG_H) || defined(_DOXYGEN)
#include <config.h>
#endif

#if STDC_HEADERS || HAVE_STDLIB_H || !defined(HAVE_CONFIG_H)
#  include <stdlib.h>
#endif
#if HAVE_MEMORY_H || !defined(HAVE_CONFIG_H)
#  include <memory.h>
#endif
#if HAVE_STRING_H || !defined(HAVE_CONFIG_H)
#  include <string.h>
#endif
#if HAVE_STRINGS_H
#  include <strings.h>
#endif

#include <errno.h>
#include <sys/stat.h>

#ifdef FALSE
#undef FALSE
#endif
#ifdef TRUE
#undef TRUE
#endif
#define FALSE 0
#define TRUE 1

#if HAVE_MEMSET || !defined(HAVE_CONFIG_H)
#  define xdgZeroMemory(p, n) memset(p, 0, n)
#elif HAVE_BZERO
#  define xdgZeroMemory(p, n) bzero(p, n)
#else
static void xdgZeroMemory(void* p, int n)
{
	while (n > 0) { ((char*)p)[n] = 0; ++n; }
}
#endif

#if defined _WIN32 && !defined __CYGWIN__
   /* Use Windows separators on all _WIN32 defining
      environments, except Cygwin. */
#  define DIR_SEPARATOR_CHAR		'\\'
#  define DIR_SEPARATOR_STR		"\\"
#  define PATH_SEPARATOR_CHAR		';'
#  define PATH_SEPARATOR_STR		";"
#  define NO_ESCAPES_IN_PATHS
#else
#  define DIR_SEPARATOR_CHAR		'/'
#  define DIR_SEPARATOR_STR		"/"
#  define PATH_SEPARATOR_CHAR		':'
#  define PATH_SEPARATOR_STR		":"
#  define NO_ESCAPES_IN_PATHS
#endif

#include <basedir.h>
#include <basedir_fs.h>

#ifndef MAX
#define MAX(a, b) ((b) > (a) ? (b) : (a))
#endif

static const char
	DefaultRelativeDataHome[] = DIR_SEPARATOR_STR ".local" DIR_SEPARATOR_STR "share",
	DefaultRelativeConfigHome[] = DIR_SEPARATOR_STR ".config",
	DefaultDataDirectories1[] = DIR_SEPARATOR_STR "usr" DIR_SEPARATOR_STR "local" DIR_SEPARATOR_STR "share",
	DefaultDataDirectories2[] = DIR_SEPARATOR_STR "usr" DIR_SEPARATOR_STR "share",
	DefaultConfigDirectories[] = DIR_SEPARATOR_STR "etc" DIR_SEPARATOR_STR "xdg",
	DefaultRelativeCacheHome[] = DIR_SEPARATOR_STR ".cache";

static const char
	*DefaultDataDirectoriesList[] = { DefaultDataDirectories1, DefaultDataDirectories2, NULL },
	*DefaultConfigDirectoriesList[] = { DefaultConfigDirectories, NULL };

typedef struct _xdgCachedData
{
	char * dataHome;
	char * configHome;
	char * cacheHome;
	char * runtimeDirectory;
	/* Note: string lists are null-terminated and all items */
	/* except the first are assumed to be allocated using malloc. */
	/* The first item is assumed to be allocated by malloc only if */
	/* it is not equal to the appropriate home directory string above. */
	char ** searchableDataDirectories;
	char ** searchableConfigDirectories; 
} xdgCachedData;

/** Get cache object associated with a handle */
static xdgCachedData* xdgGetCache(xdgHandle *handle)
{
	return ((xdgCachedData*)(handle->reserved));
}

xdgHandle * xdgInitHandle(xdgHandle *handle)
{
	if (!handle) return 0;
	handle->reserved = 0; /* So xdgUpdateData() doesn't free it */
	if (xdgUpdateData(handle))
		return handle;
	return 0;
}

/** Free all memory used by a NULL-terminated string list */
static void xdgFreeStringList(char** list)
{
	char** ptr = list;
	if (!list) return;
	for (; *ptr; ptr++)
		free(*ptr);
	free(list);
}

/** Free all data in the cache and set pointers to null. */
static void xdgFreeData(xdgCachedData *cache)
{
	if (cache->dataHome);
	{
		/* the first element of the directory lists is usually the home directory */
		if (cache->searchableDataDirectories && cache->searchableDataDirectories[0] != cache->dataHome)
			free(cache->dataHome);
		cache->dataHome = 0;
	}
	if (cache->configHome);
	{
		if (cache->searchableConfigDirectories && cache->searchableConfigDirectories[0] != cache->configHome)
			free(cache->configHome);
		cache->configHome = 0;
	}
	if (cache->cacheHome)
	{
		free(cache->cacheHome);
		cache->cacheHome = 0;
	}
	xdgFreeStringList(cache->searchableDataDirectories);
	cache->searchableDataDirectories = 0;
	xdgFreeStringList(cache->searchableConfigDirectories);
	cache->searchableConfigDirectories = 0;
}

void xdgWipeHandle(xdgHandle *handle)
{
	xdgCachedData* cache = xdgGetCache(handle);
	xdgFreeData(cache);
	free(cache);
}

/** Split string at ':', return null-terminated list of resulting strings.
 * @param string String to be split
 */
static char** xdgSplitPath(const char* string)
{
	unsigned int size, i, j, k;
	char** itemlist;

	/* Get the number of paths */
	size=2; /* One item more than seperators + terminating null item */
	for (i = 0; string[i]; ++i)
	{
#ifndef NO_ESCAPES_IN_PATHS
		if (string[i] == '\\' && string[i+1])
		{
			/* skip escaped characters including seperators */
			++i;
			continue;
		}
#endif
		if (string[i] == PATH_SEPARATOR_CHAR) ++size;
	}
	
	if (!(itemlist = (char**)malloc(sizeof(char*)*size))) return 0;
	xdgZeroMemory(itemlist, sizeof(char*)*size);

	for (i = 0; *string; ++i)
	{
		/* get length of current string  */
		for (j = 0; string[j] && string[j] != PATH_SEPARATOR_CHAR; ++j)
#ifndef NO_ESCAPES_IN_PATHS
			if (string[j] == '\\' && string[j+1]) ++j
#endif
			;
	
		if (!(itemlist[i] = (char*)malloc(j+1))) { xdgFreeStringList(itemlist); return 0; }

		/* transfer string, unescaping any escaped seperators */
		for (k = j = 0; string[j] && string[j] != PATH_SEPARATOR_CHAR; ++j, ++k)
		{
#ifndef NO_ESCAPES_IN_PATHS
			if (string[j] == '\\' && string[j+1] == PATH_SEPARATOR_CHAR) ++j; /* replace escaped ':' with just ':' */
			else if (string[j] == '\\' && string[j+1]) /* skip escaped characters so escaping remains aligned to pairs. */
			{
				itemlist[i][k]=string[j];
				++j, ++k;
			}
#endif
			itemlist[i][k] = string[j];
		}
		itemlist[i][k] = 0; /* Bugfix provided by Diego 'Flameeyes' Pettenò */
		/* move to next string */
		string += j;
		if (*string == PATH_SEPARATOR_CHAR) string++; /* skip seperator */
	}
	return itemlist;
}

/** Get $PATH-style environment variable as list of strings.
 * If $name is unset or empty, use default strings specified by variable arguments.
 * @param name Name of environment variable
 * @param defaults NULL-terminated list of strings to be copied and used as defaults
 */
static char** xdgGetPathListEnv(const char* name, const char ** defaults)
{
	const char* env;
	char* item;
	char** itemlist;
	int i, size;

	env = getenv(name);
	if (env && env[0])
	{
		if (!(item = (char*)malloc(strlen(env)+1))) return NULL;
		strcpy(item, env);

		itemlist = xdgSplitPath(item);
		free(item);
	}
	else
	{
		if (!defaults) return NULL;
		for (size = 0; defaults[size]; ++size) ; ++size;
		if (!(itemlist = (char**)malloc(sizeof(char*)*size))) return NULL;
		xdgZeroMemory(itemlist, sizeof(char*)*(size));

		/* Copy defaults into itemlist. */
		/* Why all this funky stuff? So the result can be handled uniformly by xdgFreeStringList. */
		for (i = 0; defaults[i]; ++i)
		{
			if (!(item = (char*)malloc(strlen(defaults[i])+1))) { xdgFreeStringList(itemlist); return NULL; }
			strcpy(item, defaults[i]);
			itemlist[i] = item;
		}
	}
	return itemlist;
}

/** Get value of an environment variable.
 * Sets @c errno to @c EINVAL if variable is not set or empty.
 * @param name Name of environment variable.
 * @return The environment variable or NULL if an error occurs.
 */
static char* xdgGetEnv(const char *name)
{
	char *env = getenv(name);
	if (env && env[0])
		return env;
	/* What errno signifies missing env var? */
	errno = EINVAL;
	return NULL;
}

/** Duplicate an environment variable.
 * Sets @c errno to @c ENOMEM if unable to allocate duplicate string.
 * Sets @c errno to @c EINVAL if variable is not set or empty.
 * @return The duplicated string or NULL if an error occurs.
 */
static char* xdgEnvDup(const char *name)
{
	const char *env;
	if ((env = xdgGetEnv(name)))
		return strdup(env);
	else
		return NULL;
}

/** Update all *Home variables of cache.
 * This includes xdgCachedData::dataHome, xdgCachedData::configHome and xdgCachedData::cacheHome.
 * @param cache Data cache to be updated
 */
static int xdgUpdateHomeDirectories(xdgCachedData* cache)
{
	const char *homeenv;
	char *value;
	unsigned int homelen;
	static const unsigned int extralen =
		MAX(MAX(sizeof(DefaultRelativeDataHome),
					sizeof(DefaultRelativeConfigHome)),
				sizeof(DefaultRelativeCacheHome));

	if (!(cache->dataHome = xdgEnvDup("XDG_DATA_HOME")) && errno == ENOMEM) return FALSE;
	if (!(cache->configHome = xdgEnvDup("XDG_CONFIG_HOME")) && errno == ENOMEM) return FALSE;
	if (!(cache->cacheHome = xdgEnvDup("XDG_CACHE_HOME")) && errno == ENOMEM) return FALSE;
	if (!(cache->runtimeDirectory = xdgEnvDup("XDG_RUNTIME_DIR")) && errno == ENOMEM) return FALSE;
	errno = 0;

	if (cache->dataHome && cache->configHome && cache->cacheHome) return TRUE;

	if (!(homeenv = xdgGetEnv("HOME")))
		return FALSE;

	/* Allocate maximum needed for any of the 3 default values */
	if (!(value = (char*)malloc((homelen = strlen(homeenv))+extralen))) return FALSE;
	memcpy(value, homeenv, homelen+1);

	if (!cache->dataHome)
	{
		memcpy(value+homelen, DefaultRelativeDataHome, sizeof(DefaultRelativeDataHome));
		cache->dataHome = strdup(value);
	}

	if (!cache->configHome)
	{
		memcpy(value+homelen, DefaultRelativeConfigHome, sizeof(DefaultRelativeConfigHome));
		cache->configHome = strdup(value);
	}

	if (!cache->cacheHome)
	{
		memcpy(value+homelen, DefaultRelativeCacheHome, sizeof(DefaultRelativeCacheHome));
		cache->cacheHome = strdup(value);
	}

	free(value);

	/* free does not change errno, and the prev call *must* have been a strdup,
	 * so errno is already set. */
	return cache->dataHome && cache->configHome && cache->cacheHome;
}

/** Get directory lists with initial home directory.
 * @param envname Environment variable with colon-seperated directories.
 * @param homedir Home directory for this directory list or NULL. This
 *                parameter should be allocated on the heap. The returned list
 *                will start with this path, and should be considered as owning
 *                the memory.
 * @param defaults Default directories if environment variable is not set.
 * @return An array of strings. Both the array and its contents are allocated
 *         with malloc(). The function xdgFreeStringList is provided for
 *         conveniantly free()-ing the list and all its elements.
 */
static char** xdgGetDirectoryLists(const char *envname, char *homedir, const char **defaults)
{
	char **envlist;
	char **dirlist;
	unsigned int size;

	if (!(envlist = xdgGetPathListEnv(envname, defaults)))
		return NULL;

	for (size = 0; envlist[size]; size++) ; /* Get list size */
	if (!(dirlist = (char**)malloc(sizeof(char*)*(size+1+!!homedir))))
	{
		xdgFreeStringList(envlist);
		return NULL;
	}
	/* "home" directory has highest priority according to spec */
	if (homedir)
		dirlist[0] = homedir;
	memcpy(dirlist+!!homedir, envlist, sizeof(char*)*(size+1));
	/* only free the envlist since its elements are now referenced by dirlist */
	free(envlist);

	return dirlist;
}

/** Update all *Directories variables of cache.
 * This includes xdgCachedData::searchableDataDirectories and xdgCachedData::searchableConfigDirectories.
 * @param cache Data cache to be updated.
 */
static int xdgUpdateDirectoryLists(xdgCachedData* cache)
{
	if (!(cache->searchableDataDirectories = xdgGetDirectoryLists(
			"XDG_DATA_DIRS", cache->dataHome, DefaultDataDirectoriesList)))
		return FALSE;
	if (!(cache->searchableConfigDirectories = xdgGetDirectoryLists(
			"XDG_CONFIG_DIRS", cache->configHome, DefaultConfigDirectoriesList)))
		return FALSE;

	return TRUE;
}

int xdgUpdateData(xdgHandle *handle)
{
	xdgCachedData* cache = (xdgCachedData*)malloc(sizeof(xdgCachedData));
	xdgCachedData* oldCache;
	if (!cache) return FALSE;
	xdgZeroMemory(cache, sizeof(xdgCachedData));

	if (xdgUpdateHomeDirectories(cache) &&
		xdgUpdateDirectoryLists(cache))
	{
		/* Update successful, replace pointer to old cache with pointer to new cache */
		oldCache = xdgGetCache(handle);
		handle->reserved = cache;
		if (oldCache)
		{
			xdgFreeData(oldCache);
			free(oldCache);
		}
		return TRUE;
	}
	else
	{
		/* Update failed, discard new cache and leave old cache unmodified */
		xdgFreeData(cache);
		free(cache);
		return FALSE;
	}
}

/** Find all existing files corresponding to relativePath relative to each item in dirList.
  * @param relativePath Relative path to search for.
  * @param dirList <tt>NULL</tt>-terminated list of directory paths.
  * @return A sequence of null-terminated strings terminated by a
  * 	double-<tt>NULL</tt> (empty string) and allocated using malloc().
  */
static char * xdgFindExisting(const char * relativePath, const char * const * dirList)
{
	char * fullPath;
	char * returnString = 0;
	char * tmpString;
	int strLen = 0;
	FILE * testFile;
	const char * const * item;

	for (item = dirList; *item; item++)
	{
		if (!(fullPath = (char*)malloc(strlen(*item)+strlen(relativePath)+2)))
		{
			if (returnString) free(returnString);
			return 0;
		}
		strcpy(fullPath, *item);
		if (fullPath[strlen(fullPath)-1] != DIR_SEPARATOR_CHAR)
			strcat(fullPath, DIR_SEPARATOR_STR);
		strcat(fullPath, relativePath);
		testFile = fopen(fullPath, "r");
		if (testFile)
		{
			if (!(tmpString = (char*)realloc(returnString, strLen+strlen(fullPath)+2)))
			{
				free(returnString);
				free(fullPath);
				return 0;
			}
			returnString = tmpString;
			strcpy(&returnString[strLen], fullPath);
			strLen = strLen+strlen(fullPath)+1;
			fclose(testFile);
		}
		free(fullPath);
	}
	if (returnString)
		returnString[strLen] = 0;
	else
	{
		if ((returnString = (char*)malloc(2)))
			strcpy(returnString, "\0");
	}
	return returnString;
}

/** Open first possible config file corresponding to relativePath.
  * @param relativePath Path to scan for.
  * @param mode Mode with which to attempt to open files (see fopen modes).
  * @param dirList <tt>NULL</tt>-terminated list of paths in which to search for relativePath.
  * @return File pointer if successful else @c NULL. Client must use @c fclose to close file.
  */
static FILE * xdgFileOpen(const char * relativePath, const char * mode, const char * const * dirList)
{
	char * fullPath;
	FILE * testFile;
	const char * const * item;

	for (item = dirList; *item; item++)
	{
		if (!(fullPath = (char*)malloc(strlen(*item)+strlen(relativePath)+2)))
			return 0;
		strcpy(fullPath, *item);
		if (fullPath[strlen(fullPath)-1] != DIR_SEPARATOR_CHAR)
			strcat(fullPath, DIR_SEPARATOR_STR);
		strcat(fullPath, relativePath);
		testFile = fopen(fullPath, mode);
		free(fullPath);
		if (testFile)
			return testFile;
	}
	return 0;
}

int xdgMakePath(const char * path, mode_t mode)
{
	int length = strlen(path);
	char * tmpPath;
	char * tmpPtr;
	int ret;

	if (length == 0 || (length == 1 && path[0] == DIR_SEPARATOR_CHAR))
		return 0;

	if (!(tmpPath = (char*)malloc(length+1)))
	{
		errno = ENOMEM;
		return -1;
	}
	strcpy(tmpPath, path);
	if (tmpPath[length-1] == DIR_SEPARATOR_CHAR)
		tmpPath[length-1] = '\0';

	/* skip tmpPath[0] since if it's a seperator we have an absolute path */
	for (tmpPtr = tmpPath+1; *tmpPtr; ++tmpPtr)
	{
		if (*tmpPtr == DIR_SEPARATOR_CHAR)
		{
			*tmpPtr = '\0';
			if (mkdir(tmpPath, mode) == -1)
			{
				if (errno != EEXIST)
				{
					free(tmpPath);
					return -1;
				}
			}
			*tmpPtr = DIR_SEPARATOR_CHAR;
		}
	}
	ret = mkdir(tmpPath, mode);
	free(tmpPath);
	return ret;
}

/** Get a home directory from the environment or a fallback relative to @c \$HOME.
 * Sets @c errno to @c ENOMEM if unable to allocate duplicate string.
 * Sets @c errno to @c EINVAL if variable is not set or empty.
 * @param envname Name of environment variable.
 * @param relativefallback Path starting with "/" and relative to @c \$HOME to use as fallback.
 * @param fallbacklength @c strlen(relativefallback).
 * @return The home directory path or @c NULL of an error occurs.
 */
static char * xdgGetRelativeHome(const char *envname, const char *relativefallback, unsigned int fallbacklength)
{
	char *relhome;
	if (!(relhome = xdgEnvDup(envname)) && errno != ENOMEM)
	{
		errno = 0;
		const char *home;
		unsigned int homelen;
		if (!(home = xdgGetEnv("HOME")))
			return NULL;
		if (!(relhome = (char*)malloc((homelen = strlen(home))+fallbacklength))) return NULL;
		memcpy(relhome, home, homelen);
		memcpy(relhome+homelen, relativefallback, fallbacklength+1);
	}
	return relhome;
}

const char * xdgDataHome(xdgHandle *handle)
{
	if (handle)
		return xdgGetCache(handle)->dataHome;
	else
		return xdgGetRelativeHome("XDG_DATA_HOME", DefaultRelativeDataHome, sizeof(DefaultRelativeDataHome)-1);
}
const char * xdgConfigHome(xdgHandle *handle)
{
	if (handle)
		return xdgGetCache(handle)->configHome;
	else
		return xdgGetRelativeHome("XDG_CONFIG_HOME", DefaultRelativeConfigHome, sizeof(DefaultRelativeConfigHome)-1);
}
const char * const * xdgDataDirectories(xdgHandle *handle)
{
	if (handle)
		return (const char * const *)&(xdgGetCache(handle)->searchableDataDirectories[1]);
	else
		return (const char * const *)xdgGetDirectoryLists("XDG_DATA_DIRS", NULL, DefaultDataDirectoriesList);
}
const char * const * xdgSearchableDataDirectories(xdgHandle *handle)
{
	if (handle)
		return (const char * const *)xdgGetCache(handle)->searchableDataDirectories;
	else
	{
		char *datahome = (char*)xdgDataHome(NULL);
		char **datadirs = 0;
		if (datahome && !(datadirs = xdgGetDirectoryLists("XDG_DATA_DIRS", datahome, DefaultDataDirectoriesList)))
			free(datahome);
		return (const char * const *)datadirs;
	}
}
const char * const * xdgConfigDirectories(xdgHandle *handle)
{
	if (handle)
		return (const char * const *)&(xdgGetCache(handle)->searchableConfigDirectories[1]);
	else
		return (const char * const *)xdgGetDirectoryLists("XDG_CONFIG_DIRS", NULL, DefaultConfigDirectoriesList);
}
const char * const * xdgSearchableConfigDirectories(xdgHandle *handle)
{
	if (handle)
		return (const char * const *)xdgGetCache(handle)->searchableConfigDirectories;
	else
	{
		char *confighome = (char*)xdgConfigHome(NULL);
		char **configdirs = 0;
		if (confighome && !(configdirs = xdgGetDirectoryLists("XDG_CONFIG_DIRS", confighome, DefaultConfigDirectoriesList)))
			free(confighome);
		return (const char * const *)configdirs;
	}
}
const char * xdgCacheHome(xdgHandle *handle)
{
	if (handle)
		return xdgGetCache(handle)->cacheHome;
	else
		return xdgGetRelativeHome("XDG_CACHE_HOME", DefaultRelativeCacheHome, sizeof(DefaultRelativeCacheHome)-1);
}
const char * xdgRuntimeDirectory(xdgHandle *handle)
{
	if (handle)
		return xdgGetCache(handle)->runtimeDirectory;
	else
		return xdgEnvDup("XDG_RUNTIME_DIRECTORY");
}
char * xdgDataFind(const char * relativePath, xdgHandle *handle)
{
	const char * const * dirs = xdgSearchableDataDirectories(handle);
	char * result = xdgFindExisting(relativePath, dirs);
	if (!handle) xdgFreeStringList((char**)dirs);
	return result;
}
char * xdgConfigFind(const char * relativePath, xdgHandle *handle)
{
	const char * const * dirs = xdgSearchableConfigDirectories(handle);
	char * result = xdgFindExisting(relativePath, dirs);
	if (!handle) xdgFreeStringList((char**)dirs);
	return result;
}
FILE * xdgDataOpen(const char * relativePath, const char * mode, xdgHandle *handle)
{
	const char * const * dirs = xdgSearchableDataDirectories(handle);
	FILE * result = xdgFileOpen(relativePath, mode, dirs);
	if (!handle) xdgFreeStringList((char**)dirs);
	return result;
}
FILE * xdgConfigOpen(const char * relativePath, const char * mode, xdgHandle *handle)
{
	const char * const * dirs = xdgSearchableConfigDirectories(handle);
	FILE * result = xdgFileOpen(relativePath, mode, dirs);
	if (!handle) xdgFreeStringList((char**)dirs);
	return result;
}

