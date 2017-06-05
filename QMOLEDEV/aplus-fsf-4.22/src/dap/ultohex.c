/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* convert unsigned long integer to decimal character string */

/* head file inclusions */
#include <dap/ulto.h>

/* internal data definitions */
static unsigned long powers[ULTOHEX_SZ] =
{
  0x0L,
  0xfL, 0xffL, 0xfffL, 0xffffL,
  0xfffffL, 0xffffffL, 0xfffffffL, 0xffffffffL
};

static char digits[16] =
{
  '0', '1', '2', '3', '4', '5', '6', '7',
  '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
};

/* external function definitions */
int 
ultohex(long unsigned int value, char *result, int length)
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
      result[place] = digits[value % 16];
      value /= 16;
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
    length = ULTOHEX_SZ;
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
    unittest((unsigned long) (0x0), length, printit);
    unittest((unsigned long) (0x1), length, printit);
    unittest((unsigned long) (0x2), length, printit);
    unittest((unsigned long) (0x3), length, printit);
    unittest((unsigned long) (0x4), length, printit);
    unittest((unsigned long) (0x5), length, printit);
    unittest((unsigned long) (0x6), length, printit);
    unittest((unsigned long) (0x7), length, printit);
    unittest((unsigned long) (0x8), length, printit);
    unittest((unsigned long) (0x9), length, printit);
    unittest((unsigned long) (0xa), length, printit);
    unittest((unsigned long) (0xb), length, printit);
    unittest((unsigned long) (0xc), length, printit);
    unittest((unsigned long) (0xd), length, printit);
    unittest((unsigned long) (0xe), length, printit);
    unittest((unsigned long) (0xf), length, printit);
    unittest((unsigned long) (0x10), length, printit);
    unittest((unsigned long) (0x1f), length, printit);
    unittest((unsigned long) (0x2e), length, printit);
    unittest((unsigned long) (0x3d), length, printit);
    unittest((unsigned long) (0x4c), length, printit);
    unittest((unsigned long) (0x5b), length, printit);
    unittest((unsigned long) (0x6a), length, printit);
    unittest((unsigned long) (0x79), length, printit);
    unittest((unsigned long) (0x88), length, printit);
    unittest((unsigned long) (0x97), length, printit);
    unittest((unsigned long) (0xa6), length, printit);
    unittest((unsigned long) (0xb5), length, printit);
    unittest((unsigned long) (0xc4), length, printit);
    unittest((unsigned long) (0xd3), length, printit);
    unittest((unsigned long) (0xe2), length, printit);
    unittest((unsigned long) (0xf1), length, printit);
    unittest((unsigned long) (0xff), length, printit);
    unittest((unsigned long) (0x100), length, printit);
    unittest((unsigned long) (0xfff), length, printit);
    unittest((unsigned long) (0x1000), length, printit);
    unittest((unsigned long) (0xffff), length, printit);
    unittest((unsigned long) (0x10000), length, printit);
    unittest((unsigned long) (0xfffff), length, printit);
    unittest((unsigned long) (0x100000), length, printit);
    unittest((unsigned long) (0xffffff), length, printit);
    unittest((unsigned long) (0x1000000), length, printit);
    unittest((unsigned long) (0xfffffff), length, printit);
    unittest((unsigned long) (0x10000000), length, printit);
    unittest((unsigned long) (0xffffffff), length, printit);
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
  static char result[ULTOHEX_SZ];

  result[0] = '\0';
  rc = ultohex(value, result, length);
  if (printit) {
    (void) fprintf(stdout,
		   "ultohex(%lx, %s, %d) = %d\n",
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
