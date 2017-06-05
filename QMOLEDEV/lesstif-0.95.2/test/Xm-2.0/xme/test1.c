/**
 * $Id: test1.c,v 1.1 2002/05/14 11:23:44 amai Exp $
 *
 * Check whether the warning convenience functions do work,
 * at least w/o segfaulting ...
 *
 **/
 
#include <stdlib.h>
#include <Xm/XmP.h>

int main() {

  char text[]="simple";
  char func[]="XmeWarning()";
  int i=1;

#if XmVERSION >= 2  
#if XmREVISION > 0  
  XmeWarning(NULL, "This is a simple test for XmeWarning\n"
             );
#else
  XmeWarning(NULL, "This is a %s test (%i) for %s\n",
             text, i, func);
#endif
#endif
  exit(0);
}
