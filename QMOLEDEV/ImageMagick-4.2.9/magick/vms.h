/*
  VMS Utility Methods for ImageMagick.
*/
#ifndef _VMS_H
#define _VMS_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#include <lib$routines.h>
#include <errno.h>
#include <descrip.h>
#include <rmsdef.h>
#include <ctype.h> 
#if defined(__VMS_VER) && (__VMS_VER >= 70000000)
#include <dirent.h>
#else

/*
  Typedef declarations.
*/
struct dirent
{
  char
     d_name[255];

  int
    d_namlen;
};

typedef struct _dirdesc
{
  long
    context;

  char
    *pattern;

  struct dirent
    entry;

  struct dsc$descriptor_s
    pat;
} DIR;

/*
  VMS utilities routines.
*/
extern DIR
  *opendir(char *);

extern struct dirent
  *readdir(DIR *);

extern void
  closedir(DIR *);
#endif

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
