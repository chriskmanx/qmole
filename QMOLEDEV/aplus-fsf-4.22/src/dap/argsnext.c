/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/args.h>

/* external function definitions */
void 
argsnext(int argc, char **argv)
{
  /* skip past null arguments */
  while (args_index < argc) {
    if (argv[++args_index] != (char *) (0)) {
      break;
    }
  }

  /* zero position within argument */
  args_argpos = 0;

  return;
}
