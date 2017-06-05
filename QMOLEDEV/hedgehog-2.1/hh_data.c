/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#include "hh_common.h"
#include "hh_error.h"
#include "hh_interp.h"
#include "hh_data.h"
#include "hh_avl.h"

#include <ctype.h>
#include <stdio.h>

unsigned char hh_is_large_abs_value[4] = { 0, 1, 1, 0 };


void hh_gc_start(hh_context_t *ctx)
{
  ctx->heap_free = ctx->old_heap;
}


static unsigned char hh_cell_n_words[64] = {
  0,				/* Forward pointer. */
  0,				/* Headered cell. */
  2,				/* Cons cell. */
  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, /* n-tuples. */
  1, 1,				/* Large integers. */
};


hh_word_t hh_gc_copy(hh_context_t *ctx, hh_word_t word)
{
  hh_word_t *p = HH_WORD_TO_PTR(ctx, word);
  hh_word_t w = *p, *q;
  unsigned long n_words;
  
  if ((w & 0xFF) == 0x00)
    /* The referred object has already been forwarded. */
    goto use_forward_ptr;
  /* How many words to copy? */
  n_words = hh_cell_n_words[(word & 0x7F) >> 2];
  if (n_words == 0) {
    /* The pointer did not tell the size of the referred object, look
       at the object itself. */
    if ((w & 0xFF) == 0x0A)
      /* `w' is a string header.  The upper 24 bits tell the string
	 length in bytes.  Skip them and the header. */
      n_words = ((w >> 8) + 8) >> 2;
    else if ((w & 0xFF) == 0x0E)
      /* `w' is a symbol. */
      n_words = 2;
    else if ((w & 0xFF) == 0x16) {
      /* `w' is an AVL-tree node. */
      n_words = 3;
      if (w & 0xFF00)
	/* Left subtree present. */
	n_words++;
      if (w & 0xFF0000)
	/* Right subtree present. */
	n_words++;
    } else
      /* New header types will be added here.  Currently all other
	 cases (t, nil, byte code reference) all use only one word.
	 The debug headers should never ever appear in the collected
	 heap, only in the constant pool. */
      n_words = 1;
  }
  /* Construct the forward pointer. */
  w = (ctx->heap_free - ctx->old_heap) << 8;
  /* Copy the object. */
  q = p;
  do {
    *ctx->heap_free++ = *q++;
  } while (--n_words > 0);
  /* Write the forward pointer. */
  *p = w;
 use_forward_ptr:
  /* Take the uppermost 24 bits of the forward pointer's value, but
     retain the lowermost 8 bits that tell the type of the referred
     object. */
  return w | (word & 0xFF);
}


void hh_gc_finish(hh_context_t *ctx)
{
  hh_word_t *p = ctx->old_heap, w;

  while (p != ctx->heap_free) {
    HH_ASSERT(p < ctx->heap_free);
    w = *p;
    if (HH_WORD_IS_HEAP_PTR(w))
      *p++ = hh_gc_copy(ctx, w);
    else if ((w & 0xFF) == 0x0A)
      /* `w' is a string header.  The upper 24 bits tell the string
	 length in bytes.  Skip them and the header. */
      p += ((w >> 8) + 8) >> 2;
    else
      /* Just skip anything else, be it a small integer, pointer to
	 constant data, or whatever. */
      p++;
  }

  /* Perform the semispace flip. */
  p = ctx->heap;
  ctx->heap = ctx->old_heap;
  ctx->old_heap = p;

  /* Init the allocation pointer. */
  ctx->heap_ptr = ctx->heap + ctx->heap_n_words;
}


/* Change the byte order of the program file to suit that of the
   processor currently used. */

hh_error_t hh_fix_byteorder(hh_word_t *heap, unsigned long n_words)
{
  hh_word_t *p, w, m;

  if (*heap == 0x01020304)
    /* Correct byte order, nothing to do. */
    return HH_OK;
  if (*heap != 0x04030201)
    return HH_ERROR_PROGRAM_CORRUPT;
  m = 0xFF00FFL;
  p = heap;
  while (p < heap + n_words) {
    w = *p;
    /* Swap the bytes in `w'. */
    w = ((w & m) << 8) | ((w >> 8) & m);
    w = (w << 16) | (w >> 16);
    *p = w;
    if ((w & 0xFF) == 0x0A)
      /* The bytes in the string are stored byte-wise, don't swap
	 them. */
      p += ((w >> 8) + 8) >> 2;
    else
      p++;
  }
  return HH_OK;
}


/* This function should be called only from either macro
   HH_UNSIGNED_TO_WORD or HH_SIGNED_TO_WORD.  It is called for large
   values of `w' to allocate a single word, store the uppermost 31
   bits of `w' into the heap-allocated word, and return a tagged word
   which indicates whether the lowest bit is one or zero. */

hh_word_t hh_box_integer(hh_context_t *ctx, hh_word_t w)
{
  hh_word_t *p;

  HH_ASSERT(HH_CAN_ALLOCATE(ctx, HH_BOX_N_WORDS));

  p = HH_ALLOCATE(ctx, HH_BOX_N_WORDS);
  if (w & 0x1) {
    *p = w;
    w = 0x40;
  } else {
    *p = w | 0x1;
    w = 0x44;
  }
  return HH_PTR_TO_WORD(ctx, p, w);
}


/* Allocate a string of given length and return an unboxed pointer to
   it.  The string's content is not initialized with this call, except
   for zeroing the last word. */

hh_word_t *hh_alloc_string(hh_context_t *ctx, size_t n_bytes)
{
  unsigned long n_words = HH_STRING_N_WORDS(n_bytes);
  hh_word_t *p;

  HH_ASSERT(HH_CAN_ALLOCATE(ctx, n_words));
  p = HH_ALLOCATE(ctx, n_words);
  p[n_words - 1] = 0;
  p[0] = HH_STRING_HDR(n_bytes);
  return p;
}


/* Copy the given array of bytes into the lisp heap. */

hh_word_t hh_box_string(hh_context_t *ctx, const char *string, size_t n_bytes)
{
  hh_word_t *p = hh_alloc_string(ctx, n_bytes);
  HH_MEMMOVE(HH_STRING_PTR(p), string, n_bytes);
  return HH_PTR_TO_WORD(ctx, p, 0x04);
}


/* Perform string comparison similar to C's `strcmp', except for
   not assuming null-character termination, on the two strings. */

int hh_strcmp(hh_word_t *s1, hh_word_t *s2)
{
  int len1 = HH_STRING_LEN(s1), len2 = HH_STRING_LEN(s2), r;

  if (len1 < len2) {
    r = HH_MEMCMP(HH_STRING_PTR(s1), HH_STRING_PTR(s2), len1);
    /* Don't return `r', because in theory it's return value might not
       be a "short" integer. */
    if (r <= 0)
      /* Consider the longer string `s2' lexicographically later. */
      return -1;
    else
      return 1;
  } else if (len1 > len2) {
    r = HH_MEMCMP(HH_STRING_PTR(s1), HH_STRING_PTR(s2), len2);
    if (r < 0)
      return -1;
    else
      return 1;
  } else {
    HH_ASSERT(len1 == len2);
    r = HH_MEMCMP(HH_STRING_PTR(s1), HH_STRING_PTR(s2), len1);
    if (r < 0)
      return -1;
    else if (r > 0)
      return 1;
    else
      return 0;
  }
}


/* Encode the given integer to a heap-allocated string with the given
   base.  The base must be within the range 2 to 36. */

hh_word_t hh_itoa(hh_context_t *ctx, hh_signed_word_t value, unsigned int base)
{
  static char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
  /* The size of the temporary buffer `buf' is the worst case length
     of encoding the given integer with base 2 and with a sign-bit
     included. */
  char buf[sizeof(hh_signed_word_t) * 8 + 1], *s;
  hh_word_t n;
  int is_negative;

  HH_ASSERT(base >= 2);
  HH_ASSERT(base <= 36);

  s = buf + sizeof(buf);
  if (value == 0)
    *--s = '0';
  else {
    if (value < 0) {
      is_negative = 1;
      n = -value;
    } else {
      is_negative = 0;
      n = value;
    }
    while (n != 0) {
      *--s = digits[n % base];
      n /= base;
    }
    if (is_negative)
      *--s = '-';
  }
  return hh_box_string(ctx, s, buf + sizeof(buf) - s);
}


/* Decode the signed integer in the given string cell in the given
   base.  The base must be in range 2 to 36, and sufficient memory
   must be reserved that an integer boxing is possible. */

hh_word_t hh_atoi(hh_context_t *ctx, hh_word_t *str, unsigned int base)
{
  hh_signed_word_t v = 0;
  int is_negative = 0;
  hh_word_t len = HH_STRING_LEN(str);
  char *s = HH_STRING_PTR(str);

  HH_ASSERT(base >= 2);
  HH_ASSERT(base <= 36);

  while (len > 0 && (*s == ' ' || *s == '\t' || *s == '\n')) {
    len--;
    s++;
  }
  while (len > 0) {
    if (*s == '-')
      is_negative = !is_negative;
    else if (*s >= '0' && *s < '0' + (base <= 10 ? base : 10))
      v = v * base + (*s - '0');
    else if (*s >= 'a' && *s < 'a' - 10 + base)
      v = v * base + (*s - 'a' + 10);
    else if (*s >= 'A' && *s < 'A' - 10 + base)
      v = v * base + (*s - 'A' + 10);
    else
      goto out;
    len--;
    s++;
  }

 out:
  if (is_negative)
    v = -v;
  return HH_SIGNED_TO_WORD(ctx, (hh_signed_word_t) v);
}


/* Return the number of cons cells in the list. */

hh_word_t hh_list_length(hh_context_t *ctx, hh_word_t list)
{
  hh_word_t n = 0;

  while (HH_IS_CONS(list)) {
    list = HH_CDR(HH_WORD_TO_PTR(ctx, list));
    n++;
  }
  return n;
}


#ifndef HH_COMPILER

/* Pretty printing routine.  When HH_SMALL is specified, only
   booleans, integers, strings and symbols are printed, otherwise
   almost anything is printed. */

int hh_lisp_print(hh_printf_callback_t cb, void *uncast_ctx, void *value)
{
  hh_lisp_print_ctx_t *lpx = (hh_lisp_print_ctx_t *) uncast_ctx;
  hh_context_t *ctx = lpx->ctx;
  hh_word_t word = * (hh_word_t *) value;

  if (HH_WORD_IS_INT(word)) {
    return hh_printf(cb, uncast_ctx, "%d", HH_WORD_TO_SIGNED(ctx, word));
#ifndef HH_SMALL
  } else if (HH_IS_PC(word)) {
    hh_word_t pc = HH_WORD_TO_PC(ctx, word) - (ctx->program + 12);

    if (ctx->constant[1] == HH_DEBUG_INFO_HDR_WORD) {
      /* Find the filename and line number from the debug information
	 in the constant pool.  See README for documentation of the
	 debug info format. */
      hh_word_t di_pc = 0;  /* Debug Info Program Counter. */
      unsigned char *di_p = 
	(unsigned char *) (HH_WORD_TO_PTR(ctx, ctx->constant[3]) + 1);
      unsigned int file = 0, line = 0xFFFF;
      hh_word_t *p;

      if (!HH_IS_STRING(HH_WORD_TO_PTR(ctx, ctx->constant[3])))
	/* Broken debug info. */
	goto default_pc_print;
      /* Find the three bytes covering this instruction. */
      do {
	if (di_p[0] & 0x80) {
	  di_pc += di_p[0] & 0x7F;
	  file = di_p[1];
	  line = (((hh_word_t) di_p[2]) << 8) | di_p[3];
	  di_p += 4;
	} else {
	  di_pc += di_p[0];
	  line += ((signed char *) di_p)[1];
	  di_p += 2;
	}
      } while (di_pc <= pc && *di_p != 0x7F);
      /* Dig out the line number and file index. */
      if (line == 0xFFFF || *di_p == 0x7F)
	return hh_printf(cb, uncast_ctx, "PC#%d at end of program", pc);
      /* Find the file name based on the file index. */
      p = HH_WORD_TO_PTR(ctx, ctx->constant[2]);
      while (file != 0) {
	if (HH_CDR(p) == HH_NIL)
	  /* Broken debug info. */
	  goto default_pc_print;
	p = HH_WORD_TO_PTR(ctx, HH_CDR(p));
	file--;
      }
      p = HH_WORD_TO_PTR(ctx, HH_CAR(p));
      return hh_printf(cb, uncast_ctx, "PC#%d file \"%.*s\" line %d", 
		       pc, HH_STRING_LEN(p), HH_STRING_PTR(p), line);
    }
  default_pc_print:
    return hh_printf(cb, uncast_ctx, "PC#%d", pc);
#endif
  } else if (word == HH_FALSE)
    return hh_printf(cb, uncast_ctx, "NIL");
  else if (word == HH_TRUE)
    return hh_printf(cb, uncast_ctx, "T");
  else if (HH_WORD_IS_PTR(word)) {
    hh_word_t *p = HH_WORD_TO_PTR(ctx, word);

    if (HH_IS_SYMBOL(p)) {
      p = HH_WORD_TO_PTR(ctx, HH_SYMBOL_STRING(p));
      goto print_string;
    } else if (HH_IS_STRING(p)) {
#ifdef HH_SMALL
    print_string:
      return hh_printf(cb, uncast_ctx, "%.*s", 
		       HH_STRING_LEN(p), HH_STRING_PTR(p));
#else
      if (lpx->depth == 0) {
      print_string:
	return hh_printf(cb, uncast_ctx, "%.*s", 
		       HH_STRING_LEN(p), HH_STRING_PTR(p));
      } else
	return hh_printf(cb, uncast_ctx, "\"%.*S\"",
			 HH_STRING_LEN(p), HH_STRING_PTR(p));
    } else if (lpx->max_depth >= 0 && lpx->depth >= lpx->max_depth)
      return hh_printf(cb, uncast_ctx, "...");
    else {
      int n, total = 0, lpx_depth_backup = lpx->depth;

#define HH_PR(call)				\
      do {					\
	n = call;				\
	if (n < 0)				\
	  goto out;				\
	total += n;				\
      } while (0)
#define HH_STR(str)				\
      HH_PR(hh_printf(cb, uncast_ctx, str))

      lpx->depth += HH_LISP_PRINT_DEPTH_INCR;
      if (HH_IS_CONS(word)) {
	HH_PR(hh_printf(cb, uncast_ctx, "(%@", hh_lisp_print, &HH_CAR(p)));
	while (HH_IS_CONS(HH_CDR(p))
	       && (lpx->max_depth < 0 || lpx->depth <= lpx->max_depth)) {
	  lpx->depth++;
	  p = HH_WORD_TO_PTR(ctx, HH_CDR(p));
	  HH_PR(hh_printf(cb, uncast_ctx, " %@", hh_lisp_print, &HH_CAR(p)));
	}
	if (lpx->max_depth >= 0 && lpx->depth > lpx->max_depth)
	  HH_STR(" ... ");
	else if (HH_CDR(p) != HH_NIL)
	  /* A dotted pair. */
	  HH_PR(hh_printf(cb, uncast_ctx, " . %@", hh_lisp_print, &HH_CDR(p)));
	HH_STR(")");
      } else if (HH_IS_TUPLE(word)) {
	int i;
	
	HH_STR("<");
	if (lpx->max_depth >= 0 && lpx->depth <= lpx->max_depth)
	  HH_STR("...");
	else {
	  for (i = 0; i < HH_TUPLE_ARITY(word) - 1; i++) {
	    HH_PR(hh_printf(cb, uncast_ctx, "%@, ", hh_lisp_print, &p[i]));
	    lpx->depth++;
	  }
	  HH_PR(hh_lisp_print(cb, uncast_ctx, &p[i]));
	}
	HH_STR(">");
      } else {
	hh_word_t l = HH_AVL_LEFT(p), r = HH_AVL_RIGHT(p);

	HH_ASSERT(HH_IS_AVL(p));
	HH_PR(hh_printf(cb, uncast_ctx, "AVL.<%d, %d, %d; %@, %@, %@, %@>",
			HH_AVL_HEIGHT(p),
			HH_AVL_LEFT_HEIGHT(p),
			HH_AVL_RIGHT_HEIGHT(p),
			hh_lisp_print, &HH_AVL_KEY(p),
			hh_lisp_print, &HH_AVL_VALUE(p),
			hh_lisp_print, &l,
			hh_lisp_print, &r));
      }
    out:
      lpx->depth = lpx_depth_backup;
      if (n < 0)
	return n;
      return total;
#endif /* Not HH_SMALL */
    }
  }
  return hh_printf(cb, uncast_ctx, "??0x%08X??", word);
}

#endif /* Not HH_COMPILER */
