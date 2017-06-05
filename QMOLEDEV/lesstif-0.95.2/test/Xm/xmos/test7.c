/* $Header: /cvsroot/lesstif/lesstif/test/Xm/xmos/test7.c,v 1.1 2001/08/15 08:04:37 amai Exp $ */

/* Test some non-documented interfaces.
   As opposed to other tests within this xmos/ directory this
   is a non-interactive approach! */

#include <stdlib.h>
#include <stdio.h>

#include <X11/Intrinsic.h>

/* this is not documented?!: */
extern void
_XmOSQualifyFileSpec(String dirSpec,
                     String filterSpec,
                     String *pQualifiedDir,
                     String *pQualifiedPattern);


static char *paths[] = {
     "",
     "/",
     "//",
     "..",
     "~"
     };

static char *filters[] = {
    "",
    "*",
    ".*",
    "..",
    "~",
    "foo"
    };


static int
Test__XmOSQualifyFileSpec(void) {

  String dirSpec, filterSpec, pQualifiedDir, pQualifiedPattern;
  int d, f;
  
  for (d=0; d<(sizeof(paths)/sizeof(char *)); d++) {
     dirSpec=paths[d];
     for (f=0; f<(sizeof(filters)/sizeof(char *)); f++) {
         filterSpec=filters[f];

         _XmOSQualifyFileSpec(dirSpec, filterSpec,
                              &pQualifiedDir, &pQualifiedPattern);

         fprintf(stdout, "_XmOSQualifyFileSpec(\"%s\", \"%s\") = \"%s\", \"%s\"\n\n",
                         dirSpec, filterSpec, pQualifiedDir, pQualifiedPattern);
     }
  }

  return 0;
}


int
main(int argc, char *argv[]) {

  Test__XmOSQualifyFileSpec();     

  exit(0);
}
