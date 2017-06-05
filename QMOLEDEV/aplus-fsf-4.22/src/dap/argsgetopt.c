/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * argsgetopt - this similar to getopt(3) except that it 1) never prints an
 * error message; 2) always returns the actual option letter found, relying
 * on the user to handle unknown options in the default case of a switch
 * statement; 3) treats unknown options as though they had an argument (to
 * avoid cascaded errors); 4) Sets args_value to null when no argument can be
 * found; 5) Null arguments are skipped.
 */

/* header file inclusions */
#include <a/development.h>
#include <string.h>
#include <dap/args.h>

/* external function definitions */
int 
argsgetopt(int argc, char **argv, char *opts)
{
  int c;
  char *cp;

  if (args_index == 0) {
    /*
     * args_first never called or it detected empty list, call it and if it
     * returns -1, return -1, indicating that there are no options.
     */
    if (argsfirst(argc, argv) == -1) {
      return -1;
    }
  }
  if (args_argpos == 0) {
    if (args_index >= argc) {
      return -1;
    }
    /* begin processing new argument */
    if ((argv[args_index][0] != '-')
	|| (argv[args_index][1] == '\0')) {
      /* new argument is first after options */
      return -1;
    }
    if (argv[args_index][1] == '-') {
      /* new argument is end of options marker, "--" */
      argsnext(argc, argv);
      return -1;
    }
    args_argpos = 1;
  }
  if (((c = argv[args_index][args_argpos++]) == ':')
      || ((cp = (char *) strchr((DEV_STRARG) opts, c)) == 0)
      || (*++cp == ':')) {
    /* unknown option or option with argument */
    if (argv[args_index][args_argpos] != '\0') {
      /* option argument follows immediately */
      args_value = &argv[args_index][args_argpos];
      argsnext(argc, argv);
    } else if (argsnext(argc, argv), args_index >= argc) {
      /* no option argument */
      args_value = (char *) (0);
    } else {
      /* option argument is nex* argument */
      args_value = argv[args_index];
      argsnext(argc, argv);
    }
  } else {
    /* good option without argument */
    if (argv[args_index][args_argpos] == '\0') {
      argsnext(argc, argv);
    }
    args_value = (char *) (0);
  }
  return c;
}
