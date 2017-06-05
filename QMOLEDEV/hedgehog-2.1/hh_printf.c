/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004, 2005 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

/* A concise, generic, callback-based printf routine, used by the byte
   code interpreter.  The byte code interpreter does NOT use standard
   C library printf-like functions or even the `FILE *' abstraction. */


#include <stdlib.h>
#include "hh_printf.h"


int hh_vprintf(hh_printf_callback_t cb, void *ctx, const char *fmt,
	       va_list args)
{
  int n_chars_printed = 0;
  int is_long, is_uppercase, is_negative;
  /* A temporary buffer, long enough to hold any 32-bit value in a
     base 2 representation, plus sign and terminating NUL
     character. */
#define BUF_LENGTH  34
  char buf[BUF_LENGTH], ch, *s, *w;
  int padding, min_width, base, length, left_justified, escape_unprintables; 
  hh_rec_printf_t rec_printf;
  unsigned long unsigned_long;
  signed long signed_long;
  int precision;
#ifdef HAVE_FLOATING_POINT
  double real;
#endif /* HAVE_FLOATING_POINT */

  buf[BUF_LENGTH - 1] = '\0';
  for (ch = *fmt++; ch != '\0'; ch = *fmt++)
    if (ch != '%') {
    print_char:
      if (!cb(ch, ctx))
	return -1;
      n_chars_printed++;
    } else {
      ch = *fmt++;
      if (ch == '%')
	/* The format string contains `%%', which is just an escaped
	   `%' to print. */
	goto print_char;
      /* What's the justification, padding and minimum width?  
	 Zero-padding is ignored with left-justification, 
	 negative minimum width is considered a left-justification. */
      left_justified = 0;
      padding = ' ';
      if (ch == '-') {
	left_justified = 1;
	ch = *fmt++;
      }
      if (ch == '0') {
	padding = '0';
	ch = *fmt++;
      }
      min_width = 0;
      if (ch == '*') {
	min_width = va_arg(args, int);
	if (min_width < 0) {
	  min_width = -min_width;
	  left_justified = 1;
	}
	ch = *fmt++;
      } else {
	while (HH_IS_DIGIT(ch)) {
	  min_width = 10 * min_width + ch - '0';
	  ch = *fmt++;
	}
      }
      if (left_justified)
	padding = ' ';
      /* What's the precision? */
      precision = -1;
      if (ch == '.') {
	ch = *fmt++;
	if (ch == '*') {
	  precision = va_arg(args, int);
	  ch = *fmt++;
	} else {
	  precision = 0;
	  while (HH_IS_DIGIT(ch)) {
	    precision = 10 * precision + ch - '0';
	    ch = *fmt++;
	  }
	}
      }

      /* Now we should be at length modifier or type. */
      base = 10;
      is_long = 0;
      is_uppercase = 0;
      is_negative = 0;
      escape_unprintables = 0;
    again:
      switch (ch) {
      case 'd':
	if (!is_long)
	  signed_long = va_arg(args, signed long);
	else
	  signed_long = (signed long) va_arg(args, signed int);
	if (signed_long < 0) {
	  is_negative = 1;
	  unsigned_long = -signed_long;
	} else
	  unsigned_long = signed_long;
	goto print_unsigned;

      case 'o':
	base = 8;
	goto get_and_print_unsigned;

      case 'P':
	is_uppercase = 1;
      case 'p':
	base = 16;
	unsigned_long = (unsigned long) va_arg(args, void *);
	goto print_unsigned;

      case 'X':
	is_uppercase = 1;
	/*FALLTHROUGH*/
      case 'x':
	base = 16;
	/*FALLTHROUGH*/
      case 'u':
      get_and_print_unsigned:
	if (!is_long)
	  unsigned_long = va_arg(args, unsigned long);
	else
	  unsigned_long = (unsigned long) va_arg(args, unsigned int);
      print_unsigned:
	s = buf + BUF_LENGTH - 1;
	do {
	  ch = unsigned_long % base;
	  if (ch < 10)
	    ch = '0' + ch;
	  else
	    if (is_uppercase)
	      ch = ('A' - 10) + ch;
	    else
	      ch = ('a' - 10) + ch;
	  *--s = ch;
	  unsigned_long /= base;
	} while (unsigned_long);
	if (is_negative)
	  *--s = '-';
	goto print_string;
	break;
	
      case 'C':
	escape_unprintables = 1;
	/*FALLTHROUGH*/
      case 'c':
	buf[0] = (char) va_arg(args, int);
	buf[1] = '\0';
	s = buf;
	goto print_string;
	break;

      case 'S':
	escape_unprintables = 1;
	/*FALLTHROUGH*/
      case 's':
	s = va_arg(args, char *);
      print_string:
	for (length = 0, w = s; 
	     (precision == -1 && *w) || w - s < precision;
	     /* (precision == -1 || length < precision) && *w; */
	     w++) {
	  length++;
	  if (escape_unprintables) {
	    if (*w == '\t' || *w == '\n' || !HH_IS_PRINT(*w))
	      length += 3;
	    else if (*w == '\\')
	      length++;
	  }
	}
	n_chars_printed += length;
	/* Left-padding. */
	if (length < min_width) {
	  min_width -= length;
	  n_chars_printed += min_width;
	  if (!left_justified)
	    while (min_width) {
	      if (!cb(padding, ctx))
		return -1;
	      min_width--;
	    }
	} else
	  min_width = 0;
	/* Print the string. */
	while (length) {
	  ch = *s++;
	  if (escape_unprintables) {
	    if (ch == '\\') {
	      if (!cb(ch, ctx))
		return -1;
	      length--;
	    } else if (ch == '\t' || ch == '\n' || !HH_IS_PRINT(ch)) {
	      if (!cb('\\', ctx)
		  || !cb('0' + ((ch >> 6) & 3), ctx)
		  || !cb('0' + ((ch >> 3) & 7), ctx))
		return -1;
	      ch = '0' + (ch & 7);
	      length -= 3;
	    }
	  }
	  if (!cb(ch, ctx))
	    return -1;
	  length--;
	}
	/* Right-padding. */
	while (min_width) {
	  if (!cb(padding, ctx))
	    return -1;
	  min_width--;
	}
	break;

      case '@':
	/* Recursive hh_printf.  The argument list is expected to
	   contain a function pointer and a value.  The function is
	   called with the callback (cb) and context (ctx) passed to
	   this function, and the given value.  Any modifiers of this
	   format directive are ignored. */
	rec_printf = va_arg(args, hh_rec_printf_t);
	length = rec_printf(cb, ctx, va_arg(args, void *));
	if (length < 0)
	  return length;
	n_chars_printed += length;
	break;

	/* Modifiers.  These jump to the label `again' to actually
	   perform the printing. */
      case 'B':
	is_uppercase = 1;
	/*FALLTHROUGH*/
      case 'b':
	/* This is an extension to the normal printf - this takes the
	   base for the printed signed (%d) or unsigned (%u) integer
	   from the argument list.  Printing aborts with return value
	   -2 if base illegal. */
	base = va_arg(args, int);
	if (base < 2 || base > 26)
	  return -2;
	ch = *fmt++;
	goto again;
	break;
      case 'l':
	if (!is_long) {
	  is_long = 1;
	  ch = *fmt++;
	  goto again;
	}
	/*FALLTHROUGH*/
      default:
	ch = '?';
	goto print_char;
      }
    }
  /* Format string scanned, all done. */
  return n_chars_printed;
}


int hh_printf(hh_printf_callback_t cb, void *ctx, const char *fmt, ...)
{
  va_list args;
  int n;
  
  va_start(args, fmt);
  n = hh_vprintf(cb, ctx, fmt, args);
  va_end(args);
  return n;
}


typedef struct {
  char *buf, *buf_end;
} hh_vsnprintf_ctx_t;

static int hh_vsnprintf_cb(char ch, void *ctx)
{
  hh_vsnprintf_ctx_t *p = (hh_vsnprintf_ctx_t *) ctx;

  if (p->buf > p->buf_end)
    return 0;
  *p->buf++ = ch;
  *p->buf = '\0';
  return 1;
}

int hh_vsnprintf(char *buf, unsigned long buf_size, const char *fmt,
		 va_list args)
{
  hh_vsnprintf_ctx_t ctx;
  int n;

  ctx.buf = buf;
  ctx.buf_end = buf + buf_size - 2;
  n = hh_vprintf(hh_vsnprintf_cb, &ctx, fmt, args);
  if (n < 0)
    return n;
  return n + 1;			/* Add one for the terminating '\0'. */
}

int hh_snprintf(char *buf, unsigned long buf_size, const char *fmt, ...)
{
  va_list args;
  int n;
  
  va_start(args, fmt);
  n = hh_vsnprintf(buf, buf_size, fmt, args);
  va_end(args);
  return n;
}


#ifdef HH_REALLOC
#ifdef HH_FREE

/* These routines are not used by Hedgehog, but left here as a
   suggestion of things that motivate the callback-ctx-based printing. */

typedef struct {
  char *buf_start, *buf, *buf_end;
} hh_vbprintf_ctx_t;

static int hh_vbprintf_cb(char ch, void *ctx)
{
  hh_vbprintf_ctx_t *p = (hh_vbprintf_ctx_t *) ctx;
  
  if (p->buf == p->buf_end) {
    int n_chars = p->buf_end - p->buf_start;
    /* The formula below produces the sequence 20, 50, 95, 162, 263,
       414, 641, 981, 1491, etc.  The exponential growth guarantees
       O(n log n) running time */
    int new_n_chars = 3 * n_chars / 2 + 20;
    char *new_buf_start = HH_REALLOC(p->buf_start, new_n_chars + 1);
    if (new_buf_start == NULL)
      /* Leave the `p->buf_start' untouched so that the generic
	 failure handler code in `hh_vbprintf' can free it. */
      return 0;
    p->buf_start = new_buf_start;
    p->buf = p->buf_start + n_chars;
    p->buf_end = p->buf_start + new_n_chars;
  }
  *p->buf++ = ch;
  *p->buf = '\0';
  return 1;
}

char *hh_vbprintf(const char *fmt, va_list args)
{
  hh_vbprintf_ctx_t ctx;

  ctx.buf_start = ctx.buf = ctx.buf_end = NULL;
  if (hh_vprintf(hh_vbprintf_cb, &ctx, fmt, args) < 0) {
    if (ctx.buf_start)
      HH_FREE(ctx.buf_start);
    return NULL;
  }
  return ctx.buf_start;
}

char *hh_bprintf(const char *fmt, ...)
{
  va_list args;
  char *s;
  
  va_start(args, fmt);
  s = hh_vbprintf(fmt, args);
  va_end(args);
  return s;
}

#endif /* HH_FREE */
#endif /* HH_REALLOC */



#ifdef TEST

#include <stdio.h>

typedef struct {
  char buf[80];
  int i;
} output_t;

static int cb(char ch, void *ctx)
{
#if 1
  output_t *output = (output_t *) ctx;
  output->buf[output->i++] = ch;
#endif
#if 0
  fputc(ch, stderr);
#endif
  return 1;
}

typedef struct cell_t {
  enum { CONS, INTEGER, SYMBOL } kind;
  union {
    struct {
      struct cell_t *car, *cdr;
    } cons;
    int integer;
    const char *symbol;
  } u;
} cell_t;

static int rec_print_cell(hh_printf_callback_t cb, void *ctx, void *value)
{
  cell_t *cell = (cell_t *) value;
  int n, m;

  if (cell == NULL)
    return hh_printf(cb, ctx, "NULL");
  if (cell->kind == INTEGER)
    return hh_printf(cb, ctx, "%d", cell->u.integer);
  if (cell->kind == SYMBOL)
    return hh_printf(cb, ctx, "%s", cell->u.symbol);
  n = hh_printf(cb, ctx, "(%@", rec_print_cell, cell->u.cons.car);
  if (n < 0)
    return n;
  while (cell->u.cons.cdr != NULL) {
    cell = cell->u.cons.cdr;
    if (cell->kind == CONS) {
      m = hh_printf(cb, ctx, " %@", rec_print_cell, cell->u.cons.car);
      if (m < 0)
	return m;
      n += m;
    } else {
      m = hh_printf(cb, ctx, " . %@)", rec_print_cell, cell);
      if (m < 0)
	return m;
      n += m;
      return n;
    }
  }
  m = hh_printf(cb, ctx, ")");
  if (m < 0)
    return m;
  n += m;
  return n;
}


int main(void)
{
  output_t output;
  int retval;

#define PRINT(args, output_exp, retval_exp)				 \
  do {									 \
    memset(output.buf, 0, 80);						 \
    output.i = 0;						 	 \
    retval = hh_printf args;						 \
    if (retval != (retval_exp))						 \
      fprintf(stderr, "hh_printf%s = %d (expected %d)\n", #args,	 \
	      retval, (retval_exp));					 \
    if (strcmp(output.buf, (output_exp)) != 0)				 \
      fprintf(stderr, "hh_printf%s --> '%s' (expected '%s')\n", #args,   \
	      output.buf, (output_exp));				 \
  } while (0)

  PRINT((cb, &output, "foo%cb%cr", 'x', 'a'), "fooxbar", 7);
  PRINT((cb, &output, "foo%cb%cr", 'x', ' '), "fooxb r", 7);
  PRINT((cb, &output, "f%sr", "ooba"), "foobar", 6);
  PRINT((cb, &output, "f%8sr", "ooba"), "f    oobar", 10);
  PRINT((cb, &output, "f%-8sr", "ooba"), "fooba    r", 10);
  PRINT((cb, &output, "f%*sr", 4, "ooba"), "foobar", 6);
  PRINT((cb, &output, "f%.*sr", 4, "oobax"), "foobar", 6);

  PRINT((cb, &output, "f%12Sr", "oo\ba"), "f     oo\\010ar", 14);

  PRINT((cb, &output, "%d %d", -13, 255), "-13 255", 7);
  PRINT((cb, &output, "f%*xr", -10, 0xDEADBEEF), "fdeadbeef  r", 12);
  PRINT((cb, &output, "f%*Xr", 11, 0xDEADBEEF), "f   DEADBEEFr", 13);
  PRINT((cb, &output, "f%*or", 11, 0xDEADBEEF), "f33653337357r", 13);
  PRINT((cb, &output, "f%Bur", 11, 0xDEADBEEF), "f164791A470r", 12);

  {
    cell_t i1, s1, c1, c2, c3;
    i1.kind = INTEGER; i1.u.integer = 1234;
    s1.kind = SYMBOL; s1.u.symbol = "hedgehog";
    c1.kind = CONS; c1.u.cons.car = &c2; c1.u.cons.cdr = &c3;
    c2.kind = CONS; c2.u.cons.car = &i1; c2.u.cons.cdr = &s1;
    c3.kind = CONS; c3.u.cons.car = &i1; c3.u.cons.cdr = NULL;

    PRINT((cb, &output, "c1 = %@", rec_print_cell, &c1), 
	  "c1 = ((1234 . hedgehog) 1234)", 29);
  }

  PRINT((cb, &output, "%*lBd", -7, 20, -7777), "-J8H   ", 7);
  PRINT((cb, &output, "f%8Sr", "oo\ba"), "f oo\\010ar", 10);

#if 0
  {
    /* Test for `hh_bprintf'.  Not very interesting per se, useful
       only with e.g. valgrind. */
    int i;
    char *s;
    for (i = 0; i < 1000; i++) {
      s = hh_bprintf("%0*d", i, 1);
      if (!s) {
	fprintf(stderr, "s == NULL for i = %d\n", i);
      } else {
	fprintf(stderr, "i = %d, strlen(s) = %d\n", i, strlen(s));
	HH_FREE(s);
      }
    }
  }
#endif

  return 0;
}

#endif /* TEST */

/*
 TODO: 
   - short integer values
   - floating point values
   - possibly integrate with hh_print in hh_data.c.
*/
