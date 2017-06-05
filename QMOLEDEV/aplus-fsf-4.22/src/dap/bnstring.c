/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * bnstring() is a varargs functions that allocates space for the
 * concatenation of the n strings given as its arguments. it then
 * concatenates the strings into this space and returns a pointer to the
 * memory.  It uses balloc, which exits if the memory is not available.
 * (char *)(0) is returned if the first argument is (char *)(0).  All
 * arguments before the first argument with value of (char *)(0) are
 * processed.
 */

/* header file inclusions */
#if !defined(__cfront)
#include <stdarg.h>
#endif
#include <dap/balloc.h>

/* external function definitions */
char *
bnstring(char *arg0,...)
{
  int count = 0;
  int size = 0;
  int *sp;
  char **ap;
  char *cp;
  char *cp_end;
  int *sizes;
  char **args;
  char *r;
  va_list parg;

  /* count the number of arguments */
  va_start(parg, arg0);
  cp = arg0;
  while (cp != (char *) (0)) {
    count++;
    cp = va_arg(parg, char *);
  }
  va_end(parg);

  /* allocate a sizes array and an args array */
  sp = sizes = (int *) balloc(count * sizeof(*sizes));
  ap = args = (char **) balloc(count * sizeof(*args));

  /* fill sizes and args arrays, also compute total size */
  va_start(parg, arg0);
  cp = arg0;
  while (cp != (char *) (0)) {
    *ap++ = cp;
    size += *sp++ = strlen(cp);
    cp = va_arg(parg, char *);
  }
  va_end(parg);

  /* allocate space for concatentation */
  r = (char *) balloc(size + 1);

  /* copy arguments into the concatentation area */
  sp = sizes;
  ap = args;
  for (cp_end = (cp = r) + size; cp < cp_end; cp += *sp++) {
    bcopy(*ap++, cp, *sp);
  }
  /* place a terminating NUL */
  *cp = '\0';

  /* free args and sizes */
  bfree((char *) args);
  bfree((char *) sizes);

  /* return pointer to concatenated strings */
  return r;
}
