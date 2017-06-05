/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * avlalloc() will allocate a new instance of an avl tree search structure
 * and initialize it as an empty tree.
 */

/*
 * Reference: _Algorithms_&_Data_Structures_, by Niklaus Wirth,
 * Prentiss-Hall, Englewood Cliff, NJ, 1986, pp.196-227.
 */

/* header file inclusions */
#include <string.h>
#include <dap/balloc.h>
#include <dap/kvp.h>
#include <dap/avl.h>

/* internal function declarations */
static void *_key(struct kvp * p);
static void *_value(struct kvp * p);
static int _compare(char *k1, char *k2);
static void *_replace(struct kvp * p, void *val);

/* external function definitions */
struct avl *avlalloc(void *(*key) (), void *(*value) (), int (*compare) (), void *(*insert) (), void *(*replace) (), 
		     void (*remove) ())
{
  struct avl *p;

  if (key == (void *(*) ()) (0))
    key = _key;
  if (value == (void *(*) ()) (0))
    value = _value;
  if (compare == (int (*) ()) (0))
    compare = _compare;
  if (insert == (void *(*) ()) (0))
    insert = (void *(*) ()) kvpalloc;
  if (replace == (void *(*) ()) (0))
    replace = _replace;
  if (remove == (void (*) ()) (0))
    remove = kvpfree;

  p = (struct avl *) balloc(sizeof(*p));
  p->root = (struct avln *) (0);
  p->key = key;
  p->value = value;
  p->compare = compare;
  p->insert = insert;
  p->replace = replace;
  p->remove = remove;

  return p;
}

/* internal function definitions */
static void *
_key(struct kvp * p)
{
  return p->key;
}

static void *
_value(struct kvp * p)
{
  return p->d;
}

static int 
_compare(char *k1, char *k2)
{
  int rc = strcmp((DEV_STRARG) k1, (DEV_STRARG) k2);

  if (rc < 0)
    return -1;
  if (rc > 0)
    return 1;
  return 0;
}

static void *
_replace(struct kvp * p, void *val)
{
  void *rc = p->d;

  p->d = val;
  return rc;
}
