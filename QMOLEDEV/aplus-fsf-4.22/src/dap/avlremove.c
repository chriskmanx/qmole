/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * avlremove() will search the avl tree for the element with the given key
 * value and remove it from the tree.  The tree remains balanced in
 * accordance with the AVL balancing criteria.
 */

/* header file inclusions */
#include <dap/Warn.h>
#include <dap/balloc.h>
#include <dap/avl.h>

/* internal function declarations */
static void *_remove(struct avl * p, void *key, struct avln ** app, int *hp);
static int _rem(struct avln ** aqp, struct avln ** app);
static int _remlbal(struct avln ** app);
static int _remrbal(struct avln ** app);

/* external function definitions */
void *
avlremove(struct avl * p, void *key)
{
  if ((p != (struct avl *) (0))
      && (key != (void *) (0))) {
    int h = 0;

    return _remove(p, key, &(p->root), &h);
  }
  return (void *) (0);
}

/* internal function definitions */
static void *
_remove(struct avl * p, void *key, struct avln ** app, int *hp)
{
  struct avln *aq;
  struct avln *ap = *app;
  int r;
  void *rc;

  if (ap == (struct avln *) (0)) {
    /* not in the tree */
    return (void *) (0);
  }
  r = (*(p->compare)) (key, (*(p->key)) (ap->d));
  if (r < 0) {
    rc = _remove(p, key, &(ap->l), hp);
    if (*hp)
      *hp = _remlbal(app);
  } else if (r > 0) {
    rc = _remove(p, key, &(ap->r), hp);
    if (*hp)
      *hp = _remrbal(app);
  } else {
    rc = (*(p->value)) (ap->d);
    (*(p->remove)) (ap->d);
    ap->d = (void *) (0);
    /* aq will be the node the is deallocated */
    aq = ap;
    if (ap->r == (struct avln *) (0)) {
      *app = ap->l;
      *hp = 1;
    } else if (ap->l == (struct avln *) (0)) {
      *app = ap->r;
      *hp = 1;
    } else {
      *hp = _rem(&aq, &(ap->l));
      if (*hp)
	*hp = _remlbal(app);
    }
    bfree((char *) aq);
  }
  return rc;
}

static int 
_rem(struct avln ** aqp, struct avln ** app)
{
  struct avln *ap = *app;
  int rc = 0;

  if (ap->r != (struct avln *) (0)) {
    if (rc = _rem(aqp, &(ap->r)))
      rc = _remrbal(app);
  } else {
    /*
     * transfer content of ap to aq, then pull up the left side of ap to ap's
     * position, aq then becomes ap.
     */
    (*aqp)->d = ap->d;
    *aqp = ap;
    *app = ap->l;
    rc = 1;
  }
  return rc;
}

static int 
_remlbal(struct avln ** app)
{
  struct avln *ap = *app;
  int rc = 1;

  switch (ap->bal) {
  case -1:
    ap->bal = 0;
    break;
  case 0:
    ap->bal = 1;
    rc = 0;
    break;
  case 1:
    /* rebalance */
    {
      struct avln *ap1 = ap->r;
      int bal1 = ap1->bal;

      if (bal1 >= 0) {
	/* single RR rotation */
	ap->r = ap1->l;
	ap1->l = ap;

	if (bal1 == 0) {
	  ap->bal = 1;
	  ap1->bal = -1;
	  rc = 0;
	} else {
	  ap->bal = 0;
	  ap1->bal = 0;
	}
	ap = ap1;
      } else {
	/* double RL rotation */
	struct avln *ap2 = ap1->l;
	int bal2 = ap2->bal;

	ap1->l = ap2->r;
	ap2->r = ap1;
	ap->r = ap2->l;
	ap2->l = ap;
	ap->bal = (bal2 == 1) ? -1 : 0;
	ap1->bal = (bal2 == -1) ? 1 : 0;
	ap = ap2;
	ap2->bal = 0;
      }
      *app = ap;
    }
    break;
  default:
    Abort("%t avlremove(): abort: bad balance factor = %d\n",
	  ap->bal);
  }
  return rc;
}

static int 
_remrbal(struct avln ** app)
{
  struct avln *ap = *app;
  int rc = 1;

  switch (ap->bal) {
  case -1:
    /* rebalance */
    {
      struct avln *ap1 = ap->l;
      int bal1 = ap1->bal;

      if (bal1 <= 0) {
	/* single LL rotation */
	ap->l = ap1->r;
	ap1->r = ap;

	if (bal1 == 0) {
	  ap->bal = -1;
	  ap1->bal = 1;
	  rc = 0;
	} else {
	  ap->bal = 0;
	  ap1->bal = 0;
	}
	ap = ap1;
      } else {
	/* double LR rotation */
	struct avln *ap2 = ap1->r;
	int bal2 = ap2->bal;

	ap1->r = ap2->l;
	ap2->l = ap1;
	ap->l = ap2->r;
	ap2->r = ap;
	ap->bal = (bal2 == -1) ? 1 : 0;
	ap1->bal = (bal2 == 1) ? -1 : 0;
	ap = ap2;
	ap2->bal = 0;
      }
      *app = ap;
    }
    break;
  case 0:
    ap->bal = -1;
    rc = 0;
    break;
  case 1:
    ap->bal = 0;
    break;
  default:
    Abort("%t avlremove(): abort: bad balance factor = %d\n",
	  ap->bal);
  }
  return rc;
}
