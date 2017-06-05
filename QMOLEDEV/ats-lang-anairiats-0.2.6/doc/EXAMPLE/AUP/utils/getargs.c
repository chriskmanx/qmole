//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: September, 2010
//
/* ****** ****** */
//
// book: AUP (2nd edition), pages 292 - 296
// section 5.4: Implementing a Shell (Version I)
//
/* ****** ****** */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ****** ****** */

#define true 1
#define false 0
typedef int bool ;

/* ****** ****** */

#define MAXLINE 256

/* ****** ****** */

int getargs (
  char *argv[], int n, bool *iseofp
) {
  static char cmd[MAXLINE] ;
  char *cmdp ;
  int i ;
  *iseofp = false ;
//
  if (!fgets(cmd, MAXLINE, stdin)) {
    if (ferror(stdin)) goto FAIL ;
    *iseofp = true ; return -1 ;
  } // end of [if]
//
  if (!strchr(cmd, '\n')) {
    while (1) {
      switch(getchar()) {
      case '\n': break ;
      case EOF: if (ferror(stdin)) goto FAIL ;
      default: continue ;
      }
      break ;
    } // end of [while]
    printf ("Line too long -- command ignored\n") ;
    return -1 ;
  } // end of [if]
//
  cmdp = &cmd[0] ;
  for (i = 0; i < n; i += 1) {
    if ((argv[i] = strtok(cmdp, " \t\n")) == (char*)0) break ;
    cmdp = NULL ; // tell [strtok] to continue
  } // end of [for]
//
  if (i >= n) {
    printf ("Line too long -- command ignored\n") ;
    return -1 ;
  } else {
    return i ;
  } // end of [if]
//
  FAIL: {
    return -1 ;
  } // end of [FAIL]
} // end of [getargs]

/* ****** ****** */

/* end of [getargs.c] */
