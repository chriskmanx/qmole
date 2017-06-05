/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */


#ifndef HH_INCL_DATA
#define HH_INCL_DATA  1


#include "hh_common.h"
#include "hh_interp.h"
#include "hh_error.h"


/* Memory allocation macros.  Issue `HH_CAN_ALLOCATE' to test whether
   there is sufficient memory to allocate the given number of words,
   and `HH_ALLOCATE' to actually allocate the words. */

#ifdef HH_COMPILER

/* During compilation the heap is actually same as the constant pool,
   and it must grow upwards. */

#define HH_CAN_ALLOCATE(ctx, n_words)					\
  ((ctx)->heap_ptr + (n_words) < (ctx)->heap + (ctx)->heap_n_words)

#define HH_ALLOCATE(ctx, n_words)		\
  (((ctx)->heap_ptr += (n_words)) - (n_words))

#else  /* Not HH_COMPILER. */

/* The run-time dynamic heap grows downwards. */

#define HH_CAN_ALLOCATE(ctx, n_words)		\
  ((ctx)->heap_ptr - (n_words) >= (ctx)->heap_free)

#define HH_ALLOCATE(ctx, n_words)		\
  ((ctx)->heap_ptr -= (n_words))

#endif /* Not HH_COMPILER */


/* Basic pointer codec. */

#define HH_WORD_IS_PTR(word)			\
  (((word) & 0x3) == 0x0)

#define HH_WORD_IS_HEAP_PTR(word)		\
  (((word) & 0x83) == 0x00)

#define HH_WORD_TO_PTR(ctx, word)					\
  (((word) & 0x80 ? (ctx)->constant : (ctx)->heap) + ((word) >> 8))

#ifdef HH_COMPILER

/* During compilation, ptr necessarily refers to the constant pool. */

#define HH_PTR_TO_WORD(ctx, ptr, tag)		\
  ((((ptr) - (ctx)->heap) << 8) | (tag) | 0x80)

#else

/* During byte code interpretation, this is never called for pointers
   in the constant pool. */

#define HH_PTR_TO_WORD(ctx, ptr, tag)		\
  ((((ptr) - (ctx)->heap) << 8) | (tag))

#endif /* HH_COMPILER */


/* Integer codec.  See the file README for the algorithm of how this
   is done. */

#define HH_WORD_IS_INT(word)			\
  (((word) & 0x1)				\
   || ((word) & 0x7B) == 0x40)

#define HH_WORD_TO_UNSIGNED(ctx, word)			\
  ((word) & 0x1						\
   ? (word) >> 1					\
   : *HH_WORD_TO_PTR(ctx, word) ^ (((word) >> 2) & 0x1))

#define HH_WORD_TO_SIGNED(ctx, word)					      \
  ((word) & 0x1								      \
   ? ((hh_signed_word_t) (word)) >> 1					      \
   : (hh_signed_word_t) (*HH_WORD_TO_PTR(ctx, word) ^ (((word) >> 2) & 0x1)))

/* This function should be called only from either of the macros
   below.  It allocates a word from heap and stores 31 bits in it and
   creates a suitably tagged pointer to it. */
hh_word_t hh_box_integer(hh_context_t *ctx, hh_word_t w);

#define HH_UNSIGNED_TO_WORD(ctx, u)		\
  ((u) >> 31					\
   ? hh_box_integer(ctx, u)			\
   : (((hh_word_t) (u)) << 1) | 0x1)

extern unsigned char hh_is_large_abs_value[4];

#define HH_SIGNED_TO_WORD(ctx, i)			\
  (hh_is_large_abs_value[((hh_word_t) (i)) >> 30]	\
   ? hh_box_integer(ctx, (hh_word_t) i)			\
   : (((hh_word_t) (i)) << 1) | 0x1)

#define HH_BOX_N_WORDS  1

/* In many cases we can assume the integer is not realistically
   outside the range from -2^30 to 2^30-1.  In such cases we use the
   following macros. */
#define HH_WORD_IS_SHORT(word)			\
  ((word) & 0x1)

#define HH_WORD_TO_SHORT(word)			\
  (((hh_signed_word_t) word) >> 1)

#define HH_SHORT_TO_WORD(i)			\
  (((i) << 1) | 0x1)


/* Empty list.
 */

#define HH_NIL            0x2
#define HH_IS_NIL(word)   ((word) == HH_NIL)


/* Truth values.  The integer zero and empty list are regarded as
   false.  Incidentally also the forward pointer to the beginning of
   the heap is regarded as boolean false, but that is never seen by
   the user program, so it is irrelevant. */

#define HH_IS_FALSE(word) ((word) <= HH_NIL)
#define HH_IS_TRUE(word)  ((word) > HH_NIL)

#define HH_FALSE  HH_NIL
#define HH_TRUE   (0x102)


/* Byte code position.
 */

#define HH_IS_PC(word)				\
  (((word) & 0xFF) == 0x06)
#define HH_PC_TO_WORD(ctx, pc)			\
  ((((pc) - (ctx)->program) << 8) | 0x06)
#define HH_WORD_TO_PC(ctx, word)		\
  (((word) >> 8) + (ctx)->program)


/* Debugging information.
 */

#if defined(HH_TESTING) || defined(HH_COMPILER)
#define HH_DEBUG_INFO_HDR_WORD   0x1A
#endif


/* Cons processing.
 */

#define HH_IS_CONS(word)  (((word) & 0x7F) == 0x08)
#define HH_CAR(ptr)       ((ptr)[0])
#define HH_CDR(ptr)       ((ptr)[1])

#define HH_CONS(ctx, dest_ptr, dest_word, car, cdr)		\
  do {								\
    (dest_ptr) = HH_ALLOCATE((ctx), HH_CONS_N_WORDS);		\
    HH_CAR(dest_ptr) = (car);					\
    HH_CDR(dest_ptr) = (cdr);					\
    (dest_word) = HH_PTR_TO_WORD((ctx), (dest_ptr), 0x08);	\
  } while (0)

#define HH_CONS_N_WORDS   2


/* Tuple processing.
 */

#define HH_IS_TUPLE(word)  			\
  (HH_WORD_IS_PTR(word) 			\
   && (((word) & 0x7F) >= 0x08)			\
   && (((word) & 0x7F) <= 0x3C))
#define HH_TUPLE_ARITY(word)   (((word) >> 2) & 0xF)



/* Functions are cons-cells where the car is a byte code position.
 */

#define HH_WORD_IS_FN(ctx, word)					\
  (HH_IS_CONS(word) && HH_IS_PC(HH_CAR(HH_WORD_TO_PTR(ctx, word))))


/* Lisp-heap -allocated strings.
 */

#define HH_IS_STRING(ptr)      ((*(ptr) & 0xFF) == 0x0A)

/* How many bytes (octets) does the string contain? */
#define HH_STRING_LEN(ptr)     ((*(ptr)) >> 8)

/* How many heap-allocated words is needed for a string of given
   length? */
#define HH_STRING_N_WORDS(len) (((len) + 8) >> 2)

/* The header word of a string of given length. */
#define HH_STRING_HDR(len)     (((len) << 8) | 0x0A)

#define HH_STRING_PTR(ptr)     ((char *) ((ptr) + 1))

/* Allocate a string of given length and return an unboxed pointer to
   it.  The string's content is not initialized with this call except
   for the terminating '\0' character. */
hh_word_t *hh_alloc_string(hh_context_t *ctx, size_t n_bytes);

/* Copy the given array of bytes into the lisp heap.  The memory must
   be pre-reserved. */
hh_word_t hh_box_string(hh_context_t *ctx, const char *string, size_t n_bytes);

/* Perform string comparison similar to C's `strcmp', except for
   assuming null-character termination, on the two strings. */
int hh_strcmp(hh_word_t *s1, hh_word_t *s2);

/* Encode the given integer to a heap-allocated string with the given
   base.  The base must be within the range 2 to 36. */
hh_word_t hh_itoa(hh_context_t *ctx, hh_signed_word_t value,
		  unsigned int base);

#define HH_ITOA_N_WORDS  HH_STRING_N_WORDS(8 * sizeof(hh_word_t) + 1)

/* Decode the signed integer in the given string cell in the given
   base.  The base must be in range 2 to 36, and sufficient memory
   must be reserved that an integer boxing is possible. */
hh_word_t hh_atoi(hh_context_t *ctx, hh_word_t *str, unsigned int base);

/* Return the number of cons cells in the list. */
hh_word_t hh_list_length(hh_context_t *ctx, hh_word_t list);


#ifndef HH_COMPILER

/* Callback for printing Lisp heap values with hh_printf.  The first
   value in ctx must be a struct of type hh_lisp_print_ctx_t. */

#include "hh_printf.h"

typedef struct {
  hh_context_t *ctx;
  int depth, max_depth;
} hh_lisp_print_ctx_t;

int hh_lisp_print(hh_printf_callback_t cb, void *ctx, void *value);

#endif


/* Symbols.  Currently only a header and a pointer to the string. */

#define HH_IS_SYMBOL(ptr)	((*(ptr) & 0xFF) == 0x0E)
#define HH_SYMBOL_STRING(ptr)   ((ptr)[1])
#define HH_SYMBOL_N_WORDS	2
#define HH_SYMBOL_HDR		0x0E


/* Change the byte order of the program file to suit that of the
   processor currently used. */

hh_error_t hh_fix_byteorder(hh_word_t *heap, unsigned long n_words);


/* Garbage collection primitives.  First issue `hh_gc_start', then
   apply `HH_ROOT' for each word belonging to the root set, and
   finally issue `hh_gc_finish'.  `hh_gc_copy' is an internal function
   used by the garbage collector. */

void hh_gc_start(hh_context_t *ctx);

#define HH_ROOT(ctx, word)			\
  do {						\
    if (HH_WORD_IS_HEAP_PTR(word))		\
      (word) = hh_gc_copy((ctx), (word));	\
  } while (0)

hh_word_t hh_gc_copy(hh_context_t *ctx, hh_word_t word);

void hh_gc_finish(hh_context_t *ctx);
  

#endif /* !HH_INCL_DATA */
