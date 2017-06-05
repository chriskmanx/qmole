/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * avlinsert() will place an element into an avl tree.  The element will be
 * placed in such a way that an inorder traversal of the nodes of the tree
 * visits the nodes in increasing key order.  The tree remains balanced in
 * accordance with the AVL balancing criteria.
 */
/*
 * avlreplace() will search the avl tree for the element with the given key
 * value and replace it with the given new value.
 */

/* header file inclusions */
#include <dap/Warn.h>
#include <dap/balloc.h>
#include <dap/avl.h>

/* internal function declarations */
static void *_insert(struct avl * p, void *key, void *value, int replace, struct avln ** app, int *hp);
static int _inslbal(struct avln ** app);
static int _insrbal(struct avln ** app);

/* external function definitions */
void *
avlinsert(struct avl * p, void *key, void *value)
{
  if ((p != (struct avl *) (0))
      && (key != (void *) (0))) {
    int h = 0;
    return _insert(p, key, value, 0, &(p->root), &h);
  }
  return (void *) (0);
}

void *
avlreplace(struct avl * p, void *key, void *value)
{
  if ((p != (struct avl *) (0))
      && (key != (void *) (0))) {
    int h = 0;
    return _insert(p, key, value, 1, &(p->root), &h);
  }
  return (void *) (0);
}

/* internal function definitions */
static void *
_insert(struct avl * p, void *key, void *value, int replace, struct avln ** app, int *hp)
{
  struct avln *ap = *app;
  int r;
  void *rc;

  if (ap == (struct avln *) (0)) {
    ap = (struct avln *) balloc(sizeof(*ap));
    ap->l = ap->r = (struct avln *) (0);
    ap->bal = 0;
    ap->d = (*(p->insert)) (key, value);
    *app = ap;
    return (void *) (0);
  }
  r = (*(p->compare)) (key, (*(p->key)) (ap->d));
  if (r < 0) {
    rc = _insert(p, key, value, replace, &(ap->l), hp);
    if (*hp)
      *hp = _inslbal(app);
  } else if (r > 0) {
    rc = _insert(p, key, value, replace, &(ap->r), hp);
    if (*hp)
      *hp = _insrbal(app);
  } else {
    if (replace)
      rc = (*(p->replace)) (ap->d, value);
    else
      rc = (*(p->value)) (ap->d);
  }
  return rc;
}

static int 
_inslbal(struct avln ** app)
{
  struct avln *ap = *app;

  switch (ap->bal) {
  case -1:
    /* rebalance */
    {
      struct avln *ap1 = ap->l;

      if (ap1->bal == -1) {
	/* single LL rotation */
	ap->l = ap1->r;
	ap1->r = ap;
	ap->bal = 0;
	ap = ap1;
      } else {
	/* double LR rotation */
	struct avln *ap2 = ap1->r;

	ap1->r = ap2->l;
	ap2->l = ap1;
	ap->l = ap2->r;
	ap2->r = ap;
	ap->bal = (ap2->bal == -1) ? 1 : 0;
	ap1->bal = (ap2->bal == 1) ? -1 : 0;
	ap = ap2;
      }
      ap->bal = 0;
      *app = ap;
    }
    return 0;
  case 0:
    ap->bal = -1;
    return 1;
  case 1:
    ap->bal = 0;
    return 0;
  default:
    Abort("%t avlinsert(): abort: bad balance factor = %d\n",
	  ap->bal);
  }
  /* NOTREACHED */
}

static int 
_insrbal(struct avln ** app)
{
  struct avln *ap = *app;

  switch (ap->bal) {
  case -1:
    ap->bal = 0;
    return 0;
  case 0:
    ap->bal = 1;
    return 1;
  case 1:
    /* rebalance */
    {
      struct avln *ap1 = ap->r;

      if (ap1->bal == 1) {
	/* single RR rotation */
	ap->r = ap1->l;
	ap1->l = ap;
	ap->bal = 0;
	ap = ap1;
      } else {
	/* double RL rotation */
	struct avln *ap2 = ap1->l;

	ap1->l = ap2->r;
	ap2->r = ap1;
	ap->r = ap2->l;
	ap2->l = ap;
	ap->bal = (ap2->bal == 1) ? -1 : 0;
	ap1->bal = (ap2->bal == -1) ? 1 : 0;
	ap = ap2;
      }
      ap->bal = 0;
      *app = ap;
    }
    return 0;
  default:
    Abort("%t avlinsert(): abort: bad balance factor = %d\n",
	  ap->bal);
  }
  /* NOTREACHED */
}
