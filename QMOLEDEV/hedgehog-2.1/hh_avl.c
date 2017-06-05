/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#include "hh_common.h"
#include "hh_error.h"
#include "hh_avl.h"


/* A simple default comparison function for symbols, integers, and
   strings.  Returns -1, 0, or 1 if `a' is considered to be less than,
   equal to, or greater than `b', respectively.

   The relative order of types is integers, symbols, and strings, in
   increasing order. */

int hh_default_cmpfun(hh_context_t *ctx,
		      hh_word_t a,
		      hh_word_t b)
{
  if (HH_WORD_IS_INT(a)) {
    hh_signed_word_t sa, sb;

    if (!HH_WORD_IS_INT(b))
      return -1;
    sa = HH_WORD_TO_SIGNED(ctx, a);
    sb = HH_WORD_TO_SIGNED(ctx, b);
    if (sa < sb)
      return -1;
    else if (sa == sb)
      return 0;
    return 1;
  }
  if (!HH_WORD_IS_PTR(b))
    return 1;
  {
    hh_word_t *pa = HH_WORD_TO_PTR(ctx, a);
    hh_word_t *pb = HH_WORD_TO_PTR(ctx, b);

    if (HH_IS_SYMBOL(pa)) {
      if (!HH_IS_SYMBOL(pb))
	return -1;
      a = HH_SYMBOL_STRING(pa);
      pa = HH_WORD_TO_PTR(ctx, a);
      b = HH_SYMBOL_STRING(pb);
      pb = HH_WORD_TO_PTR(ctx, b);
    } else
      if (HH_IS_SYMBOL(pb))
	return 1;
    HH_ASSERT(HH_IS_STRING(pa));
    HH_ASSERT(HH_IS_STRING(pb));
    return hh_strcmp(pa, pb);
  }
}


/* Make a new AVL-tree node with the given key, value and subtrees.
   If the height difference of the subtrees is two, the routine
   performs necessary rotations to bring the new node into balance.
   No rotations are made if the heights differ less, and a fatal error
   is raised if the heights differ by three or more.  It is assumed
   that `HH_AVL_MAKE_NODE_N_WORDS' words can be allocated from the
   heap. */

hh_word_t hh_avl_make_node(hh_context_t *ctx,
			   hh_word_t key,
			   hh_word_t value,
			   hh_word_t left,
			   hh_word_t right)
{

  /* The following macro performs the non-balancing AVL-tree node
     construction.  It is not wrapped inside 'do { .. } while (0)'
     because at least in some gcc version it introduced barriers for
     instruction scheduling and resulted in worse code.  Cache some
     values into local `_lc' variables to avoid excess code
     duplication. */
#define AVL_CONS(ctx, resh, res, k, v, lh, l, rh, r)			  \
  {									  \
    hh_word_t lh_lc = (lh);						  \
    hh_word_t rh_lc = (rh);						  \
    hh_word_t r_lc = (r);						  \
    hh_word_t *p_lc;							  \
									  \
    (resh) = 2;								  \
    HH_ASSERT((lh_lc) - (rh_lc) < 2 || (lh_lc) - (rh_lc) > -2);		  \
    if ((lh_lc) > 0) {							  \
      if ((rh_lc) > 0) {						  \
        (resh) = ((lh_lc) + (rh_lc) + 3) >> 1;				  \
        (p_lc) = HH_ALLOCATE(ctx, 5);					  \
        (p_lc)[4] = (r_lc);						  \
      } else {								  \
        HH_ASSERT((lh_lc) == 1);					  \
        (p_lc) = HH_ALLOCATE(ctx, 4);					  \
      }									  \
      (p_lc)[3] = (l);							  \
    } else								  \
      if ((rh_lc) > 0) {						  \
        HH_ASSERT((rh_lc) == 1);					  \
        (p_lc) = HH_ALLOCATE(ctx, 4);					  \
        (p_lc)[3] = (r_lc);						  \
      } else {								  \
        (resh) = 1;							  \
        (p_lc) = HH_ALLOCATE(ctx, 3);					  \
      }									  \
    (p_lc)[0] = ((resh) << 24) | ((lh_lc) << 16) | ((rh_lc) << 8) | 0x16; \
    (p_lc)[1] = (k);							  \
    (p_lc)[2] = (v);							  \
    (res) = HH_PTR_TO_WORD(ctx, p_lc, 0x04);				  \
  }
  /* As above, but assume lh >= 1. */
#define AVL_CONS_L(ctx, resh, res, k, v, lh, l, rh, r)			  \
  {									  \
    hh_word_t lh_lc = (lh);						  \
    hh_word_t rh_lc = (rh);						  \
    hh_word_t r_lc = (r);						  \
    hh_word_t *p_lc;							  \
									  \
    (resh) = 2;								  \
    HH_ASSERT((lh_lc) - (rh_lc) < 2 || (lh_lc) - (rh_lc) > -2);		  \
    HH_ASSERT((lh_lc) > 0);						  \
    if ((rh_lc) > 0) {							  \
      (resh) = ((lh_lc) + (rh_lc) + 3) >> 1;				  \
      (p_lc) = HH_ALLOCATE(ctx, 5);					  \
      (p_lc)[4] = (r_lc);						  \
    } else {								  \
      HH_ASSERT((lh_lc) == 1);						  \
      (p_lc) = HH_ALLOCATE(ctx, 4);					  \
    }									  \
    (p_lc)[0] = ((resh) << 24) | ((lh_lc) << 16) | ((rh_lc) << 8) | 0x16; \
    (p_lc)[1] = (k);							  \
    (p_lc)[2] = (v);							  \
    (p_lc)[3] = (l);							  \
    (res) = HH_PTR_TO_WORD(ctx, p_lc, 0x04);				  \
  }
  /* As AVL_CONS, but assume rh >= 1. */
#define AVL_CONS_R(ctx, resh, res, k, v, lh, l, rh, r)			  \
  {									  \
    hh_word_t lh_lc = (lh);						  \
    hh_word_t rh_lc = (rh);						  \
    hh_word_t r_lc = (r);						  \
    hh_word_t *p_lc;							  \
									  \
    (resh) = 2;								  \
    HH_ASSERT((lh_lc) - (rh_lc) < 2 || (lh_lc) - (rh_lc) > -2);		  \
    HH_ASSERT((rh_lc) >= 1);						  \
    if ((lh_lc) > 0) {							  \
      (resh) = ((lh_lc) + (rh_lc) + 3) >> 1;				  \
      (p_lc) = HH_ALLOCATE(ctx, 5);					  \
      (p_lc)[4] = (r_lc);						  \
      (p_lc)[3] = (l);							  \
    } else {								  \
      HH_ASSERT((rh_lc) == 1);						  \
      (p_lc) = HH_ALLOCATE(ctx, 4);					  \
      (p_lc)[3] = (r_lc);						  \
    }									  \
    (p_lc)[0] = ((resh) << 24) | ((lh_lc) << 16) | ((rh_lc) << 8) | 0x16; \
    (p_lc)[1] = (k);							  \
    (p_lc)[2] = (v);							  \
    (res) = HH_PTR_TO_WORD(ctx, p_lc, 0x04);				  \
  }
  /* As AVL_CONS, but assume rh >= 1 and lh >= 1. */
#define AVL_CONS_LR(ctx, resh, res, k, v, lh, l, rh, r)			  \
  {									  \
    hh_word_t lh_lc = (lh);						  \
    hh_word_t rh_lc = (rh);						  \
    hh_word_t r_lc = (r);						  \
    hh_word_t *p_lc;							  \
									  \
    (resh) = 2;								  \
    HH_ASSERT((lh_lc) - (rh_lc) < 2 || (lh_lc) - (rh_lc) > -2);		  \
    HH_ASSERT((lh_lc) >= 1);						  \
    HH_ASSERT((rh_lc) >= 1);						  \
    (resh) = ((lh_lc) + (rh_lc) + 3) >> 1;				  \
    (p_lc) = HH_ALLOCATE(ctx, 5);					  \
    (p_lc)[0] = ((resh) << 24) | ((lh_lc) << 16) | ((rh_lc) << 8) | 0x16; \
    (p_lc)[1] = (k);							  \
    (p_lc)[2] = (v);							  \
    (p_lc)[3] = (l);							  \
    (p_lc)[4] = (r_lc);							  \
    (res) = HH_PTR_TO_WORD(ctx, p_lc, 0x04);				  \
  }

  hh_word_t *p, n1, n2, n3;
  hh_word_t lh, rh, n1h, n2h, n3h;

  if (left == HH_NIL)
    lh = 0;
  else
    lh = HH_AVL_HEIGHT(HH_WORD_TO_PTR(ctx, left));
  if (right == HH_NIL)
    rh = 0;
  else
    rh = HH_AVL_HEIGHT(HH_WORD_TO_PTR(ctx, right));

  switch (lh - rh) {
  case 2:
    /* Left subtree too high. */
    {
      hh_word_t *l = HH_WORD_TO_PTR(ctx, left);
      int llh = HH_AVL_LEFT_HEIGHT(l);
      int lrh = HH_AVL_RIGHT_HEIGHT(l);
      
      if (llh >= lrh) {
	/* Simple rotate right. */
	AVL_CONS(ctx, n1h, n1, key, value,
		 lrh, HH_AVL_RIGHT(l), rh, right);
	AVL_CONS_R(ctx, n2h, n2, HH_AVL_KEY(l), HH_AVL_VALUE(l),
		   llh, HH_AVL_LEFT(l), n1h, n1);
	return n2;
      } else {
	/* Double rotate right. */
	hh_word_t lright = HH_AVL_RIGHT(l);
	hh_word_t *lr = HH_WORD_TO_PTR(ctx, lright);
	
	AVL_CONS(ctx, n1h, n1, HH_AVL_KEY(l), HH_AVL_VALUE(l),
		 llh, HH_AVL_LEFT(l), HH_AVL_LEFT_HEIGHT(lr), HH_AVL_LEFT(lr));
	AVL_CONS(ctx, n2h, n2, key, value,
		 HH_AVL_RIGHT_HEIGHT(lr), HH_AVL_RIGHT(lr), rh, right);
	p = lr;
	goto cons_top;
      }
    }
    break;
  case 1:
  case 0:
  case -1:
    /* The height difference of the left and right subtrees is at most
       one: no rotations needed. */
  no_rotation:
    AVL_CONS(ctx, n1h, n1, key, value,
	     lh, left, rh, right);
    return n1;
    break;
  case -2:
    /* Right subtree too high. */
    {
      hh_word_t *r = HH_WORD_TO_PTR(ctx, right);
      int rlh = HH_AVL_LEFT_HEIGHT(r);
      int rrh = HH_AVL_RIGHT_HEIGHT(r);
      
      if (rrh >= rlh) {
	/* Simple rotate left. */
	AVL_CONS(ctx, n1h, n1, key, value,
		 lh, left, rlh, HH_AVL_LEFT(r));
	AVL_CONS_L(ctx, n2h, n2, HH_AVL_KEY(r), HH_AVL_VALUE(r),
		   n1h, n1, rrh, HH_AVL_RIGHT(r));
	return n2;
      } else {
	/* Double rotate left. */
	hh_word_t rleft = HH_AVL_LEFT(r);
	hh_word_t *rl = HH_WORD_TO_PTR(ctx, rleft);
	
	AVL_CONS(ctx, n1h, n1, key, value,
		 lh, left, HH_AVL_LEFT_HEIGHT(rl), HH_AVL_LEFT(rl));
	AVL_CONS(ctx, n2h, n2, HH_AVL_KEY(r), HH_AVL_VALUE(r),
		 HH_AVL_RIGHT_HEIGHT(rl), HH_AVL_RIGHT(rl), rrh,
		 HH_AVL_RIGHT(r));
	p = rl;
      cons_top:
	AVL_CONS_LR(ctx, n3h, n3, HH_AVL_KEY(p), HH_AVL_VALUE(p),
		    n1h, n1, n2h, n2);
	return n3;
      }
    }
    break;
  default:
    /* Someone uses this function incorrectly, report an error and do
       the best that we can. */
#ifndef HH_SMALL
    HH_PRINT("avl_make_node asked to merge two trees which are of too "
	     "different height\nkey =\n");
    hh_lisp_print_interpreter(ctx, key, 3);
    HH_PRINT("value =\n");
    hh_lisp_print_interpreter(ctx, value, 3);
    HH_PRINT("left =\n");
    hh_lisp_print_interpreter(ctx, left, 3);
    HH_PRINT("right =\n");
    hh_lisp_print_interpreter(ctx, right, 3);
    HH_PRINT("I'll just merge them without any balancing, "
	     "you go get your pesticides...\n\n");
    HH_BACKTRACE(ctx);
#endif /* !HH_TESTING */
    goto no_rotation;
    break;
  }
  HH_NOTREACHED;
}


/* Perform a search in the given tree using the default comparison
   function.  Return the value stored for the given key, or
   `default_value' if not found. */

hh_word_t hh_avl_default_get(hh_context_t *ctx,
			     hh_word_t tree,
			     hh_word_t key,
			     hh_word_t default_value)
{
  hh_word_t *p;
  int cmp;

  while (1) {
    if (tree == HH_NIL)
      return default_value;
    p = HH_WORD_TO_PTR(ctx, tree);
    cmp = hh_default_cmpfun(ctx, key, HH_AVL_KEY(p));
    if (cmp < 0)
      tree = HH_AVL_LEFT(p);
    else if (cmp > 0)
      tree = HH_AVL_RIGHT(p);
    else
      return HH_AVL_VALUE(p);
  }
  HH_NOTREACHED;
}


/* Perform insertion/replacement in the given tree using the default
   comparison function.  Return the new tree, or HH_NIL if the heap
   did not contain enough memory. */

hh_word_t hh_avl_default_put(hh_context_t *ctx,
			     hh_word_t tree,
			     hh_word_t key,
			     hh_word_t value)
{
  hh_word_t *p, node_key, node_value, left, right;
  int cmp;

  if (tree != HH_NIL) {
    p = HH_WORD_TO_PTR(ctx, tree);
    node_key = HH_AVL_KEY(p);
    node_value = HH_AVL_VALUE(p);
    left = HH_AVL_LEFT(p);
    right = HH_AVL_RIGHT(p);
    cmp = hh_default_cmpfun(ctx, key, node_key);
    if (cmp < 0) {
      left = hh_avl_default_put(ctx, left, key, value);
      if (left == HH_NIL)
	return HH_NIL;
    } else if (cmp > 0) {
      right = hh_avl_default_put(ctx, right, key, value);
      if (right == HH_NIL)
	return HH_NIL;
    } else
      node_value = value;
  } else {
    left = right = HH_NIL;
    node_key = key;
    node_value = value;
  }
  if (!HH_CAN_ALLOCATE(ctx, HH_AVL_MAKE_NODE_N_WORDS))
    return HH_NIL;
  return hh_avl_make_node(ctx, node_key, node_value, left, right);
}
