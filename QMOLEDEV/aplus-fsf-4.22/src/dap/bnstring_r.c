/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * bnstring_r() is like bnstring except that it returns (char *)(0) and sets
 * errno to ENOMEM if it cannot do its work due to a lack of space.
 */

/* header file inclusions */
#if !defined(__cfront)
#include <stdarg.h>
#endif
#include <dap/balloc.h>

/* external function definitions */
char *
bnstring_r(char *arg0,...)
{
  int cnt = 0;
  int size = 0;
  int *sp = (int *) (0);
  char **ap = (char **) (0);
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
    cnt++;
    cp = va_arg(parg, char *);
  }
  va_end(parg);

  /* allocate a sizes array and an args array */
  if (((sp = (int *) balloc_r(cnt * sizeof(*sizes))) == (int *) (0))
      || ((ap = (char **) balloc_r(cnt * sizeof(*args))) == (char **) (0))) {
    bfree((char *) sp);
    return (char *) (0);
  }
  /* fill sizes and args arrays, also compute total size */
  va_start(parg, arg0);
  sizes = sp;
  args = ap;
  cp = arg0;
  while (cp != (char *) (0)) {
    *ap++ = cp;
    size += *sp++ = strlen(cp);
    cp = va_arg(parg, char *);
  }
  sp = sizes;
  ap = args;
  va_end(parg);

  /* allocate space for concatentation */
  if ((r = (char *) balloc(size + 1)) == (char *) (0)) {
    bfree((char *) ap);
    bfree((char *) sp);
    return (char *) (0);
  }
  /* copy arguments into the concatentation area */
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
