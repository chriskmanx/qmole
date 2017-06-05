/**
 * $Id: test19.c,v 1.3 2001/05/06 22:14:52 rwscott Exp $
 *
 * Check whether the warning convenience functions do work,
 * at least w/o segfaulting ...
 *
 **/
 
#include <stdlib.h>
#include <Xm/XmP.h>

int main() {

  char text[]="simple";
  char func[]="_XmWarning()";
  int i=1;

#if XmVERSION > 1
#if XmREVISION > 0
 _XmWarning(NULL, "This is a simple test for _XmWarning (1.x)\n");
#else
  _XmWarning(NULL, "This is a %s test (%i) for %s (2.x)\n",
             text, i, func);
#endif
#else
 _XmWarning(NULL, "This is a simple test for _XmWarning (1.x)\n");
#endif
  exit(0);
}
