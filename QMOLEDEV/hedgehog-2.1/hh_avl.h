/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#ifndef HH_INCL_AVL
#define HH_INCL_AVL  1


#include "hh_common.h"
#include "hh_data.h"
#include "hh_error.h"


#define HH_IS_AVL(ptr)		 ((*(ptr) & 0xFF) == 0x16)
#define HH_AVL_KEY(ptr)          ((ptr)[1])
#define HH_AVL_VALUE(ptr)        ((ptr)[2])
#define HH_AVL_RIGHT_HEIGHT(ptr) (((ptr)[0] >> 8) & 0xFF)
#define HH_AVL_LEFT_HEIGHT(ptr)  (((ptr)[0] >> 16) & 0xFF)
#define HH_AVL_HEIGHT(ptr)       ((ptr)[0] >> 24)
#define HH_AVL_LEFT(ptr)         (HH_AVL_LEFT_HEIGHT(ptr) ? (ptr)[3] : HH_NIL)
#define HH_AVL_RIGHT(ptr)        (HH_AVL_RIGHT_HEIGHT(ptr)		   \
                                  ? (ptr)[HH_AVL_LEFT_HEIGHT(ptr) ? 4 : 3] \
                                  : HH_NIL)

/* A simple default comparison function for symbols, integers, and
   strings.  Returns -1, 0, or 1 if `a' is considered to be less than,
   equal to, or greater than `b', respectively. */

int hh_default_cmpfun(hh_context_t *ctx,
		      hh_word_t a,
		      hh_word_t b);

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
			   hh_word_t right);

#define HH_AVL_MAKE_NODE_N_WORDS  (3 * 5)


/* Perform a search in the given tree using the default comparison
   function.  Return the value stored for the given key, or
   `default_value' if not found. */

hh_word_t hh_avl_default_get(hh_context_t *ctx,
			     hh_word_t tree,
			     hh_word_t key,
			     hh_word_t default_value);

/* Perform insertion/replacement in the given tree using the default
   comparison function.  Return the new tree, or HH_NIL if the heap
   did not contain enough memory. */

hh_word_t hh_avl_default_put(hh_context_t *ctx,
			     hh_word_t tree,
			     hh_word_t key,
			     hh_word_t value);

#endif /* HH_INCL_AVL */
