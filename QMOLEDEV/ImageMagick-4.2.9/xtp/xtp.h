/*
  Include declarations
*/

#if defined(__hpux)
#define _HPUX_SOURCE  1
#endif
#include <stdio.h>
#if defined(SOCKS)
#include <socks.h>
#endif
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <time.h>
#if !defined(macintosh)
#include <sys/types.h>
#include <sys/stat.h>
#else
#include <SIOUX.h>
#include <console.h>
#include <unix.h>
#include <types.h>
#include <stat.h>
#endif
#include <pwd.h>
#if defined(HAVE_CONFIG_H)
#include "config.h"
#endif

/*
  Define declarations for the xtp program.
*/
#if defined(__cplusplus) || defined(c_plusplus)
#define class  c_class
#endif
#define False  0
#define IsGlob(text) \
  ((strchr(text,'*') != (char *) NULL) || \
   (strchr(text,'?') != (char *) NULL) || \
   (strchr(text,'{') != (char *) NULL) || \
   (strchr(text,'}') != (char *) NULL))
#define Max(x,y)  (((x) > (y)) ? (x) : (y))
#define MaxTextLength  2048
#define True  1
#define Warning(message,qualifier)  \
{  \
  (void) fprintf(stderr,"%s: %s",client_name,message);  \
  if (qualifier != (char *) NULL)  \
    (void) fprintf(stderr," (%s)",qualifier);  \
  (void) fprintf(stderr,".\n");  \
}
#if defined(SVR4) && !defined(linux)
#define HAVE_PTMX
#endif
#if !defined(XTP_FTP)
#if defined(SOCKS)
#define XTP_FTP "rftp"
#else
#define XTP_FTP "ftp"
#endif
#endif

#ifndef lint
static char
  Version[]="@(#)ImageMagick 4.2.9 99/09/01 cristy@mystic.es.dupont.com";
#endif
