/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* convert unsigned long integer to octal character string */

/* head file inclusions */
#include <dap/ulto.h>

/* internal data definitions */
static unsigned long powers[ULTOOCT_SZ] =
{
  0L, 07L, 077L, 0777L,
  07777L, 077777L, 0777777L, 07777777L,
  077777777L, 0777777777L, 07777777777L, 037777777777L
};

static char digits[8] =
{
  '0', '1', '2', '3', '4', '5', '6', '7'
};

/* external function definitions */
int 
ultooct(long unsigned int value, char *result, int length)
{
  int place;

  /* determine number of places to be used */
  if (value == 0) {
    place = 1;
  } else {
    for (place = 1; value > powers[place]; place++);
  }
  if (place < length) {
    int rc = place + 1;		/* return code */

    result[place] = '\0';
    while (place-- > 0) {
      result[place] = digits[value % 8];
      value /= 8;
    }

    return rc;
  } else {
    /* insufficent space in caller provided string */
    return -1;
  }

  /* NOTREACHED */
}

/* internal function definitions */

#ifdef UNITTEST
/* unit test header file inclusions */
#include <stdio.h>

/* unit test function declarations */
extern int main();
extern void unittest();

/* unit test function definitions */
int 
main(argc, argv)
  int argc;
  char *argv[];
{
  int length;
  int count;
  int printit;

  if (argc < 2) {
    length = ULTOOCT_SZ;
  } else {
    length = atoi(argv[1]);
  }
  if (argc < 3) {
    count = 1;
    printit = 1;
  } else {
    count = atoi(argv[2]);
    printit = 0;
  }

  while (count-- > 0) {
    unittest((unsigned long) (0), length, printit);
    unittest((unsigned long) (01), length, printit);
    unittest((unsigned long) (02), length, printit);
    unittest((unsigned long) (03), length, printit);
    unittest((unsigned long) (04), length, printit);
    unittest((unsigned long) (05), length, printit);
    unittest((unsigned long) (06), length, printit);
    unittest((unsigned long) (07), length, printit);
    unittest((unsigned long) (010), length, printit);
    unittest((unsigned long) (017), length, printit);
    unittest((unsigned long) (026), length, printit);
    unittest((unsigned long) (035), length, printit);
    unittest((unsigned long) (044), length, printit);
    unittest((unsigned long) (053), length, printit);
    unittest((unsigned long) (062), length, printit);
    unittest((unsigned long) (071), length, printit);
    unittest((unsigned long) (077), length, printit);
    unittest((unsigned long) (0100), length, printit);
    unittest((unsigned long) (0777), length, printit);
    unittest((unsigned long) (01000), length, printit);
    unittest((unsigned long) (07777), length, printit);
    unittest((unsigned long) (010000), length, printit);
    unittest((unsigned long) (077777), length, printit);
    unittest((unsigned long) (0100000), length, printit);
    unittest((unsigned long) (0777777), length, printit);
    unittest((unsigned long) (01000000), length, printit);
    unittest((unsigned long) (07777777), length, printit);
    unittest((unsigned long) (010000000), length, printit);
    unittest((unsigned long) (077777777), length, printit);
    unittest((unsigned long) (0100000000), length, printit);
    unittest((unsigned long) (0777777777), length, printit);
    unittest((unsigned long) (01000000000), length, printit);
    unittest((unsigned long) (07777777777), length, printit);
    unittest((unsigned long) (010000000000), length, printit);
    unittest((unsigned long) (037777777777), length, printit);
  }
  exit(0);
  /* NOTREACHED */
}

static void 
unittest(value, length, printit)
  unsigned long value;
  int length;
  int printit;
{
  int rc;
  static char result[ULTOOCT_SZ];

  result[0] = '\0';
  rc = ultooct(value, result, length);
  if (printit) {
    (void) fprintf(stdout,
		   "ultooct(%lo, %s, %d) = %d\n",
		   value, result, length, rc);

/* Lexa redefines fflush in stdio.h      */
/* which results in an unresolved symbol */
#if defined(__edgfe) && defined(fflush)
#undef fflush
#endif
/******************************************/

    fflush(stdout);
  }
  return;
}
#endif
