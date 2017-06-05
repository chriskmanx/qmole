/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/* header file inclusions */
#include <dap/avl.h>

/* internal function definitions */
static void *_preorder(struct avln * ap, void *(*func) (), void *result);
static void *_rev_preorder(struct avln * ap, void *(*func) (), void *result);
static void *_inorder(struct avln * ap, void *(*func) (), void *result);
static void *_rev_inorder(struct avln * ap, void *(*func) (), void *result);
static void *_postorder(struct avln * ap, void *(*func) (), void *result);
static void *_rev_postorder(struct avln * ap, void *(*func) (), void *result);
static void *_leaf(struct avln * ap, void *(*func) (), void *result);
static void *_rev_leaf(struct avln * ap, void *(*func) (), void *result);
static void *_all(struct avln * ap, void *(*func) (), void *result);
static void *_rev_all(struct avln * ap, void *(*func) (), void *result);

/* external function definitions */
void *avlwalk(struct avl * p, int type, void *(*func) (), void *result) {
  if ((p != (struct avl *) (0))
      && (func != (void *(*) ()) (0))) {
    if (type & AVLWALK_REVERSE) {
      type &= ~AVLWALK_REVERSE;
      switch (type) {
      case AVLWALK_PREORDER:
	result = _rev_preorder(p->root, func, result);
	break;
      case AVLWALK_INORDER:
	result = _rev_inorder(p->root, func, result);
	break;
      case AVLWALK_POSTORDER:
	result = _rev_postorder(p->root, func, result);
	break;
      case AVLWALK_LEAF:
	result = _rev_leaf(p->root, func, result);
	break;
      case AVLWALK_ALL:
      default:
	result = _rev_all(p->root, func, result);
	break;
      }
    } else {
      switch (type) {
      case AVLWALK_PREORDER:
	result = _preorder(p->root, func, result);
	break;
      case AVLWALK_INORDER:
	result = _inorder(p->root, func, result);
	break;
      case AVLWALK_POSTORDER:
	result = _postorder(p->root, func, result);
	break;
      case AVLWALK_LEAF:
	result = _leaf(p->root, func, result);
	break;
      case AVLWALK_ALL:
      default:
	result = _all(p->root, func, result);
	break;
      }
    }
  }
  return result;
}

/* internal function definitions */
static void *_preorder(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    result = (*func) (ap->d, result);
    result = _preorder(ap->l, func, result);
    result = _preorder(ap->r, func, result);
  }
  return result;
}

static void *_rev_preorder(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    result = (*func) (ap->d, result);
    result = _rev_preorder(ap->r, func, result);
    result = _rev_preorder(ap->l, func, result);
  }
  return result;
}

static void *_inorder(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    result = _inorder(ap->l, func, result);
    result = (*func) (ap->d, result);
    result = _inorder(ap->r, func, result);
  }
  return result;
}

static void *_rev_inorder(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    result = _rev_inorder(ap->r, func, result);
    result = (*func) (ap->d, result);
    result = _rev_inorder(ap->l, func, result);
  }
  return result;
}

static void *_postorder(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    result = _postorder(ap->l, func, result);
    result = _postorder(ap->r, func, result);
    result = (*func) (ap->d, result);
  }
  return result;
}

static void *_rev_postorder(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    result = _rev_postorder(ap->r, func, result);
    result = _rev_postorder(ap->l, func, result);
    result = (*func) (ap->d, result);
  }
  return result;
}

static void *_leaf(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    if ((ap->l == (struct avln *) (0))
	&& (ap->r == (struct avln *) (0))) {
      result = (*func) (ap->d, result);
    } else {
      result = _leaf(ap->l, func, result);
      result = _leaf(ap->r, func, result);
    }
  }
  return result;
}

static void *_rev_leaf(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    if ((ap->l == (struct avln *) (0))
	&& (ap->r == (struct avln *) (0))) {
      result = (*func) (ap->d, result);
    } else {
      result = _rev_leaf(ap->r, func, result);
      result = _rev_leaf(ap->l, func, result);
    }
  }
  return result;
}

static void *_all(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    if ((ap->l == (struct avln *) (0))
	&& (ap->r == (struct avln *) (0))) {
      result = (*func) (ap->d, result, AVLWALK_LEAF);
    } else {
      result = (*func) (ap->d, result, AVLWALK_PREORDER);
      result = _all(ap->l, func, result);
      result = (*func) (ap->d, result, AVLWALK_INORDER);
      result = _all(ap->r, func, result);
      result = (*func) (ap->d, result, AVLWALK_POSTORDER);
    }
  }
  return result;
}

static void *_rev_all(struct avln * ap, void *(*func) (), void *result) {
  if (ap != (struct avln *) (0)) {
    if ((ap->l == (struct avln *) (0))
	&& (ap->r == (struct avln *) (0))) {
      result = (*func) (ap->d, result,
			AVLWALK_REVERSE | AVLWALK_LEAF);
    } else {
      result = (*func) (ap->d, result,
			AVLWALK_REVERSE | AVLWALK_PREORDER);
      result = _all(ap->r, func, result);
      result = (*func) (ap->d, result,
			AVLWALK_REVERSE | AVLWALK_INORDER);
      result = _all(ap->l, func, result);
      result = (*func) (ap->d, result,
			AVLWALK_REVERSE | AVLWALK_POSTORDER);
    }
  }
  return result;
}
