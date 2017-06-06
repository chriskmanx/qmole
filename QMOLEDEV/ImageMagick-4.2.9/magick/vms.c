/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%                            V   V  M   M  SSSSS                              %
%                            V   V  MM MM  SS                                 %
%                            V   V  M M M   SSS                               %
%                             V V   M   M     SS                              %
%                              V    M   M  SSSSS                              %
%                                                                             %
%                                                                             %
%                     VMS Utility Methods for ImageMagick                     %
%                                                                             %
%                                                                             %
%                               Software Design                               %
%                                 John Cristy                                 %
%                                October 1994                                 %
%                                                                             %
%                                                                             %
%  Copyright 1999 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission is hereby granted, free of charge, to any person obtaining a    %
%  copy of this software and associated documentation files ("ImageMagick"),  %
%  to deal in ImageMagick without restriction, including without limitation   %
%  the rights to use, copy, modify, merge, publish, distribute, sublicense,   %
%  and/or sell copies of ImageMagick, and to permit persons to whom the       %
%  ImageMagick is furnished to do so, subject to the following conditions:    %
%                                                                             %
%  The above copyright notice and this permission notice shall be included in %
%  all copies or substantial portions of ImageMagick.                         %
%                                                                             %
%  The software is provided "as is", without warranty of any kind, express or %
%  implied, including but not limited to the warranties of merchantability,   %
%  fitness for a particular purpose and noninfringement.  In no event shall   %
%  E. I. du Pont de Nemours and Company be liable for any claim, damages or   %
%  other liability, whether in an action of contract, tort or otherwise,      %
%  arising from, out of or in connection with ImageMagick or the use or other %
%  dealings in ImageMagick.                                                   %
%                                                                             %
%  Except as contained in this notice, the name of the E. I. du Pont de       %
%  Nemours and Company shall not be used in advertising or otherwise to       %
%  promote the sale, use or other dealings in ImageMagick without prior       %
%  written authorization from the E. I. du Pont de Nemours and Company.       %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  The directory methods are strongly based on similar methods written
%  by Rich Salz.
%
*/

#if defined(vms) && !defined(_AXP_)
/*
  Include declarations.
*/
#include "magick.h"
#include "defines.h"
#include "vms.h"

#if !defined(__VMS_VER) || (__VMS_VER < 70000000)
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   c l o s e d i r                                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method closedir closes the named directory stream and frees the DIR
%  structure.
%
%  The format of the closedir method is:
%
%
%  A description of each parameter follows:
%
%    o entry: Specifies a pointer to a DIR structure.
%
%
*/
void closedir(DIR *directory)
{
  assert(directory != (DIR *) NULL);
  free(directory->pattern);
  free((char *) directory);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   o p e n e d i r                                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method opendir opens the directory named by filename and associates
%  a directory stream with it.
%
%  The format of the opendir method is:
%
%      opendir(entry)
%
%  A description of each parameter follows:
%
%    o entry: Specifies a pointer to a DIR structure.
%
%
*/
DIR *opendir(char *name)
{
  DIR
    *directory;

  /*
    Allocate memory for handle and the pattern.
  */
  directory=(DIR *) AllocateMemory(sizeof(*directory));
  if (directory == (DIR *) NULL)
    {
      errno=ENOMEM;
      return((DIR *) NULL);
    }
  if (strcmp(".",name) == 0)
    name="";
  directory->pattern=AllocateMemory((unsigned int) (strlen(name)
    +sizeof("*.*")+1));
  if (directory->pattern == (char *) NULL)
    {
      FreeMemory((char *) directory);
      errno=ENOMEM;
      return(NULL);
    }
  /*
    Initialize descriptor.
  */
  (void) sprintf(directory->pattern,"%.1024s*.*",name);
  directory->context=0;
  directory->pat.dsc$a_pointer=directory->pattern;
  directory->pat.dsc$w_length=strlen(directory->pattern);
  directory->pat.dsc$b_dtype=DSC$K_DTYPE_T;
  directory->pat.dsc$b_class=DSC$K_CLASS_S;
  return(directory);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   r e a d d i r                                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Method readdir returns a pointer to a structure representing the
%  directory entry at the current position in the directory stream to
%  which entry refers.
%
%  The format of the readdir
%
%      readdir(entry)
%
%  A description of each parameter follows:
%
%    o entry: Specifies a pointer to a DIR structure.
%
%
*/
struct dirent *readdir(DIR *directory)
{
  char
    buffer[sizeof(directory->entry.d_name)];

  int
    status;

  register char
    *p;

  register int
    i;

  struct dsc$descriptor_s
    result;

  /*
    Initialize the result descriptor.
  */
  result.dsc$a_pointer=buffer;
  result.dsc$w_length=sizeof(buffer)-2;
  result.dsc$b_dtype=DSC$K_DTYPE_T;
  result.dsc$b_class=DSC$K_CLASS_S;
  status=lib$find_file(&directory->pat,&result,&directory->context);
  if ((status == RMS$_NMF) || (directory->context == 0L))
    return((struct dirent *) NULL);
  /*
    Lowercase all filenames.
  */
  buffer[sizeof(buffer)-1]='\0';
  for (p=buffer; *p; p++)
    if (isupper(*p))
      *p=tolower(*p);
  /*
    Skip any directory component and just copy the name.
  */
  p=buffer;
  while (!isspace(*p))
    p++;
  *p='\0';
  p=strchr(buffer,']');
  if (p)
    (void) strcpy(directory->entry.d_name,p+1);
  else
    (void) strcpy(directory->entry.d_name,buffer);
  directory->entry.d_namlen=strlen(directory->entry.d_name);
  return(&directory->entry);
}
#endif
#endif
