/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Keith W. Iverson */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/node.h>
#include <dap/kvp.h>

/* internal function declarations */
static int kvpcompare(struct node ** a, struct node ** b);

/* external function definitions */
void 
nodesort(struct node * hp, int (*func) ())
{
  if (hp != (struct node *) (0)) {
    int c;
    struct node *np;

    for (c = 0, np = hp->f; np != hp; c++, np = np->f);

    if (c != 0) {
      struct node **a;
      struct node **ai;
      struct node **ae;

      a = (struct node **) balloc(c * sizeof(*a));
      for (ae = (ai = a) + c; ai != ae; ai++) {
	noderemove(np = hp->f);
	*ai = np;
      }
      if (func == (int (*) ()) (0))
	func = kvpcompare;
      qsort((char *) (a), c, sizeof(*a), func);
      for (ai = a; ai != ae; ai++) {
	nodeinsert(*ai, hp);
      }
      bfree((char *) a);
    }
  }
  return;
}

/* internal function definitions */
static int 
kvpcompare(struct node ** a, struct node ** b)
{
  struct kvp *ka;
  struct kvp *kb;
  char *kak;
  char *kbk;

  if (a == (struct node **) (0)) {
    if (b == (struct node **) (0))
      return 0;
    return -1;
  }
  if (b == (struct node **) (0))
    return 1;
  if (*a == (struct node *) (0)) {
    if (*b == (struct node *) (0))
      return 0;
    return -1;
  }
  if (*b == (struct node *) (0))
    return 1;
  if ((ka = KVPAT(*a)) == (struct kvp *) (0)) {
    if (KVPAT(*b) == (struct kvp *) (0))
      return 0;
    return -1;
  }
  if ((kb = KVPAT(*b)) == (struct kvp *) (0))
    return 1;
  if ((kak = (char *) (ka->key)) == (char *) (0)) {
    if ((char *) (kb->key) == (char *) (0))
      return 0;
    return -1;
  }
  if ((kbk = (char *) (kb->key)) == (char *) (0))
    return 1;

  return strcmp(kak, kbk);
}
