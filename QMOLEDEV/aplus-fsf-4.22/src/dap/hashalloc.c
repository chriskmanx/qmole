/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/node.h>
#include <dap/kvp.h>
#include <dap/hash.h>

/* external function definitions */
struct hash *hashalloc(int tblsz, int (*hash) (), void *(*value) (), struct node * (*find) (), struct node * (*insert) (), void (*replace) (), 
		       void (*remove) ())
/* hash table size */
/* info hashing function */
/* info value function */
/* info find function */
/* info insert notifier */
/* info replace notifier */
/* info remove notifier */
{
  struct hash *p;
  struct node *hnp;
  struct node *end_hnp;

  if (tblsz <= 0)
    tblsz = HASH_TBLSZ;
  if (hash == (int (*) ()) (0))
    hash = bstrhash;
  if (value == (void *(*) ()) (0))
    value = kvpvalue;
  if (find == (struct node * (*) ()) (0))
    find = kvpfind;
  if (insert == (struct node * (*) ()) (0))
    insert = kvpinsert;
  if (replace == (void (*) ()) (0))
    replace = kvpreplace;
  if (remove == (void (*) ()) (0))
    remove = kvpremove;

  p = (struct hash *) balloc(sizeof(*p));
  p->tbl = (struct node *) balloc(tblsz * sizeof(*(p->tbl)));
  for (end_hnp = (hnp = p->tbl) + tblsz; hnp != end_hnp; hnp++) {
    hnp->f = hnp->b = hnp;
  }
  p->tblsz = tblsz;
  p->hashfunc = hash;
  p->value = value;
  p->find = find;
  p->insert = insert;
  p->replace = replace;
  p->remove = remove;

  return p;
}
