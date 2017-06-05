#ifndef XFEUTILS_H
#define XFEUTILS_H


#include <errno.h>
#include <unistd.h>
#include <sys/stat.h>
#include <dirent.h>

#include <fx.h>

#include "xvt/xvt.h"

// Global variables
#if defined(linux)
extern FXStringDict* mtdevices;
extern FXStringDict* updevices;
#endif


// Single click types
enum
{
	SINGLE_CLICK_NONE,
	SINGLE_CLICK_DIR,
	SINGLE_CLICK_DIR_FILE,
};


// Wait cursor states
enum
{
	BEGIN_CURSOR,
	END_CURSOR,
	QUERY_CURSOR
};


// Note : some inline functions must be declared in the header file or it won't compile!

// Convert a character to lower case
inline int toLower(int c)
{
	return ('A' <= c && c <= 'Z' ? c + 32 : c);
}


// To test if two strings are equal (strcmp replacement, thanks to Francesco Abbate)
inline int streq(const char*a, const char*b)
{
	if (a == NULL || b == NULL)
		return 0;
	return (strcmp(a, b) == 0);
}


// Convert a string to lower cases and returns the string size
inline void strlow (char* str)
{
    while (*str)
    {
        *str = toLower( *str );
        ++str;
    }
} 


// Replacement of the stat function
inline int statrep(const char* filename, struct stat* buf)
{
#if defined(linux)

	static int ret;

	// It's a mount point
	if(mtdevices->find(filename))
	{
		// Mount point is down
		if(streq(updevices->find(filename),"down"))
			return -1;
		
		// Mount point is up
		else
		{
			ret=stat(filename,buf);
			if (ret==-1)
			{
				updevices->remove(filename);
				updevices->insert(filename,"down");
			}
			return ret;
		}
	}
		
	// It's not a mount point
	else
#endif
		return stat(filename,buf);
}


// Replacement of the lstat function
inline int lstatrep(const char* filename, struct stat* buf)
{
#if defined(linux)

	static int ret;

	// It's a mount point
	if(mtdevices->find(filename))
	{
		// Mount point is down
		if(streq(updevices->find(filename),"down"))
			return -1;
		
		// Mount point is up
		else
		{
			ret=lstat(filename,buf);		
			if (ret==-1)
			{
				updevices->remove(filename);
				updevices->insert(filename,"down");
			}
			return ret;
		}
	}
		
	// It's not a mount point
	else
#endif
		return lstat(filename,buf);
}

FXHotKey _parseAccel(const FXString&);
FXbool existCommand(const FXString);
FXString getKeybinding(FXEvent*);
int mkpath(const char*, mode_t);
FXString createTrashpathname(FXString, FXString);
int createTrashinfo(FXString, FXString, FXString, FXString);
FXString mimetype(FXString);
FXString quote(FXString);
FXbool isUtf8(const char*, unsigned int);
int statrep(const char*, struct stat*);
int lstatrep(const char*, struct stat*);
#if defined(linux)
int lstatmt(const char*, struct stat*);
#endif
size_t strlcpy(char*, const char*, size_t);
size_t strlcat(char*, const char*, size_t);
FXulong dirsize(const char*);
FXulong pathsize(char*, unsigned int*, unsigned int*);
FXString hSize(char*);
FXString cleanPath(const FXString);
FXString filePath(const FXString);
FXString filePath(const FXString, const FXString);
FXString fileFromURI(FXString);
FXString fileToURI(const FXString&);
long deltime(FXString);
int isEmptyDir(const FXString);
int hasSubDirs(const FXString);
FXbool exists(const FXString&);
FXbool isDirectory(const FXString&);
FXbool isFile(const FXString&);
FXbool isGroupMember(gid_t);
FXbool isWritable(const FXString&);
FXbool isReadable(const FXString&);
FXbool isReadExecutable(const FXString&);
FXbool isLink(const FXString&);
FXbool info(const FXString&, struct stat&);
FXString permissions(unsigned int);
FXString readLink(const FXString&);
FXbool identical(const FXString& ,const FXString&);
int setWaitCursor(FXApp*, unsigned int);
int runinxvt(FXString);

#endif


