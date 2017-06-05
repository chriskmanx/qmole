/* $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/mkdirtree.c,v 1.7 2001/08/14 09:14:04 amai Exp $ */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include <unistd.h>

#include <Xm/FileSB.h>

#include "mkdirtree.h"


static char tmppath[MAXPATHLEN],
            path1[MAXPATHLEN], path2[MAXPATHLEN], Ltdir[MAXPATHLEN];
#define TmpDefault "/tmp"


static const char dir_list[][MAXPATHLEN]={
   "/lesstif", "/lesstif/dir1", "/lesstif/dir2"
  };
  
static const char file_list[][MAXPATHLEN]={
   "/lesstif/file1", "/lesstif/file2", 
   "/lesstif/dir1/file1", "/lesstif/dir1/file2", "/lesstif/dir1/file3",
   "/lesstif/dir2/file1", "/lesstif/dir2/file2", "/lesstif/dir2/file3",
  };

static const char linkfrom_list[][MAXPATHLEN]={
	"/lesstif/dir1",     "/lesstif/file1"
  };

static const char linkto_list[][MAXPATHLEN]={
	"/lesstif/dirlink1", "/lesstif/filelink1"
  };
	

#define CREATEDIR(x)     mkdir(x, S_IRWXU)
#define CREATEFILE(x)    close(open(x,O_CREAT,S_IRUSR))
#define CREATELINK(x,y)  symlink(x,y)
#define MKPATH1(x)       {strcpy(path1,tmppath);strcat(path1,x);}
#define MKPATH2(x)       {strcpy(path2,tmppath);strcat(path2,x);}


extern int
make_tmp_dir_tree(void)
{

  int i, rc;
	
  /* don't customize this.  The point of this file is to have a standard
     set of directories that's not going to vary from machine to machine,
     so that the sizes, etc are always the same.  For architectures
     without a /tmp, this is doomed from the start, so there's no reason
     to even try */

  strcpy(tmppath, TmpDefault);

  strcpy(Ltdir, tmppath);
  strcat(Ltdir, "/lesstif");

  for(i=0;i<(sizeof(dir_list)/MAXPATHLEN);i++) {
          MKPATH1(dir_list[i]);
          CREATEDIR(path1);
  }
    for(i=0;i<(sizeof(file_list)/MAXPATHLEN);i++) {
	  MKPATH1(file_list[i]);
	  CREATEFILE(path1);
  }
  if (sizeof(linkfrom_list)==sizeof(linkto_list)){
    for(i=0;i<(sizeof(linkfrom_list)/MAXPATHLEN);i++) {
      MKPATH1(linkfrom_list[i]);
      MKPATH2(linkto_list[i]);
      CREATELINK(path1, path2);
    }  	  
  }
  
  chdir(Ltdir);
  return 0;
}


extern void
set_path(Widget FileBox)
{
  XmString path,filter;

  path = XmStringCreateLtoR(Ltdir, XmSTRING_DEFAULT_CHARSET);
  filter = XmStringCreateLtoR("*", XmSTRING_DEFAULT_CHARSET);
  
  XtVaSetValues(FileBox,
		XmNdirectory, path,
		XmNpattern, filter,
		NULL);

  XmStringFree(path);
  XmStringFree(filter);
}


extern void
get_path(Widget FileBox)
{
  XmString directory, pattern, dirmask, dirspec;
  char *ptr;

  XtVaGetValues(FileBox,
		XmNdirectory, &directory,
		XmNpattern,   &pattern,
		XmNdirMask,   &dirmask,
		XmNdirSpec,   &dirspec,
		NULL);

  if (XmStringGetLtoR(directory, XmFONTLIST_DEFAULT_TAG, &ptr)) {
     fprintf(stdout, "directory=%s\n", ptr);
  }
  if (XmStringGetLtoR(pattern, XmFONTLIST_DEFAULT_TAG, &ptr)) {
     fprintf(stdout, "pattern=%s\n", ptr);
  }
  if (XmStringGetLtoR(dirmask, XmFONTLIST_DEFAULT_TAG, &ptr)) {
     fprintf(stdout, "dirmask=%s\n", ptr);
  }
  if (XmStringGetLtoR(dirspec, XmFONTLIST_DEFAULT_TAG, &ptr)) {
     fprintf(stdout, "dirspec=%s\n", ptr);
  }
}
