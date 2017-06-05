/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1990-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <string.h>
#if defined(__NetBSD__) || defined(__FreeBSD) || defined (__APPLE__)
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#include <stdlib.h>

#define DFLTPATH ".:/usr/ucb:/bin:/usr/bin"

static int executable(filename)
char *filename;
{
	struct stat statb;
	if (stat(filename, &statb) != 0) return 0;
	if ((statb.st_mode & S_IFMT) != S_IFREG) return 0;
	if (access(filename, X_OK) != 0) return 0;
	return 1;
}

char *searchPATH(base)
char *base;
{
	int baselen, pathlen, dirlen;
	char *path, *endpath, *dir, *enddir, *filename;
	char c;

	if (base == (char *)(0)) return (char *)(0);
	if (base[0] == '\0') return (char *)(0);
	if (strchr(base, '/'))
	{
		/* path contains '/', so evaluate directly */
		if (executable(base))
		{
			baselen = strlen(base) + 1;
			filename = malloc(baselen);
			memcpy(filename, base, baselen);
			return filename;
		}
		return (char *)(0);
	}
	/* evaluate relative to PATH components */
	if ((path = getenv("PATH")) == (char *)(0))
	{
		path = DFLTPATH;
	}
	pathlen = strlen(path) + 1;
	endpath = path + pathlen;
	baselen = strlen(base) + 1;
	filename = malloc(pathlen + baselen); /* this is enough */
	for (dir = enddir = path; dir != endpath; dir = ++enddir)
	{
		while (((c = *enddir) != ':') && (c != '\0')) ++enddir;
		if (dirlen = enddir - dir)
		{
			memcpy(filename, dir, enddir - dir);
			filename[dirlen] = '/';
			dirlen++;
		}
		memcpy(filename + dirlen, base, baselen);
		if (executable(filename))
		{
			return filename;
		}
	}
	free(filename);
	return (char *)(0);
}
