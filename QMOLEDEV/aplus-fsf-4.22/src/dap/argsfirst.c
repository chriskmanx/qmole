/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * argsfirst initializes the variables used by other functions in processing
 * the argument list.  It sets args_value to be argv[0] and returns zero.  If
 * the argument list is empty, it returns -1 and sets args_value to be null.
 */

/* header file inclusions */
#include <dap/args.h>

/* external function definitions */
int 
argsfirst(int argc, char **argv)
{
  /* initalize argument processing variables */
  args_index = 0;
  args_argpos = 0;
  args_value = (char *) (0);

  /* check for empty list */
  if ((argv == (char **) (0))
      || (args_index >= argc)) {
    /* empty argument list */
    return -1;
  }
  /* process argv[0] */
  args_value = argv[0];
  argsnext(argc, argv);
  return 0;
}
