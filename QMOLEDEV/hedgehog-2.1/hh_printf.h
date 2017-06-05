/* This file is part of Hedgehog LISP.
 * Copyright (C) 2003, 2004 Oliotalo Ltd.
 * See file LICENSE.LGPL for pertinent licensing conditions.
 *
 * Author: Kenneth Oksanen <cessu@iki.fi>
 */

/* A concise, generic, callback-based printf routine, used by the byte
   code interpreter.  The byte code interpreter does NOT use standard
   c library printf-like functions or even the `FILE *' abstraction.
 */


#ifndef HH_INCL_PRINTF
#define HH_INCL_PRINTF  1

#include <stdarg.h>


/* A callback called to "print" one character.  The callback can do
   whatever it wants - put it on the screen, write it into a resizing
   or size-limited buffer or file, send it to a socket, ..., or
   perhaps just discard it.  Returns zero on failure, in which case
   printing will cease, or non-zero in which case printing will
   continue. */

typedef int (*hh_printf_callback_t)(char ch, void *ctx);

/* Type of the recursive printing function passed to the format
   directive `%@'. */

typedef int (*hh_rec_printf_t)(hh_printf_callback_t cb, void *ctx,
			       void *value);

/* Printf the given values according to the given format string using
   the given callback and context.  Returns the number of characters
   successfully written.  The format characters understood are as
   follows.  First printing of integer values:
     %d   print the given signed int in decimal base 10.
     %u   print the given unsigned int in decimal base 10.
     %o   print the given unsigned int in octal base 8.
     %x   print the given unsigned int in hexadecimal base 16.
     %X   print the given unsigned int in hexadecimal base 16
          with uppercase letters.
     %p   print the given pointer value in hexadecimal base 16.
     %P   print the given pointer value in hexadecimal base 16
          with uppercase letters.
   The modifier l tells the integer values are of type long instead of
   int.  The modifier b in conjunction with %d or %u tells to use the
   base given in the argument list.  The modifier B is identical, but
   tells to use uppercase letters.

   Next the textual values:
     %c   print the given character
     %C   print the given character so that unprintable characters 
          are printed in octal, e.g. \010
     %s   print the given string
     %S   print the given string so that unprintable characters
          are printed in octal, e.g. \010
   All integer and textual values can be specified to have a minimum
   width and justification in that minimum width.  For example %12d
   prints the given signed integer padding it from the left with
   spaces so that the pad and integer use (at least) 12 integers,
   %012d uses zeros as padding, and %-12d pads the integer from the
   right (i.e. justifies to the left).  %*d takes the minimum width
   and justification from the argument list.

   Examples:
     hh_printf(..., "%*lBd", -7, 20, -7777) prints the value -7777 in
       base 20 with uppercase letters justified to the left and using
       at least 7 characters: the printed characters are "-J8H   " and
       return value is 7.
     hh_printf(..., "f%8Sr", "oo\ba") prints first `f', then the given
       string as right-justified using at least 8 characters and
       escaping any non-printable characters, and finally prints `r':
       the printed characters are "f oo\010ar" and return value is 10.

   The most powerful feature in hh_printf is the possibility to invoke
   recursively printing functions dedicated for specific types.  The
   directive %@ takes the given function pointer of type
   `hh_rec_printf_t' and `void *'-value from the argument list, and
   calls the function with the print-callback and context that were
   given to the hh_printf.  The type-specific printing functions are
   of course able to use hh_printf again.

   The phrase to use hh_printf as fprintf is to define a function like
     int hh_fputc(char ch, void *ctx)
     {
       return fputc(ch, (FILE *) ctx) != EOF;
     }
   and use it like
     hh_printf(hh_fputc, stdout, "Hello, world!\n");
   However, I have not wanted to place such code in this file, because
   this file should be possible to use verbatim even if no stdio.h,
   stdout or stderr is present. */


int hh_printf(hh_printf_callback_t cb, void *ctx, const char *fmt, ...);
int hh_vprintf(hh_printf_callback_t cb, void *ctx, const char *fmt,
	       va_list args);

/* Functions built on the above ones.  First the equivalents for
   standard `snprintf' and `vsnprintf'. */

int hh_snprintf(char *buf, unsigned long buf_size, const char *fmt, ...);
int hh_vsnprintf(char *buf, unsigned long buf_size, const char *fmt,
		 va_list args);

#if 0

/* These functions are not used by Hedgehog, but left here as a
   suggestion of things that motivate the callback-ctx-based printing. */

/* These functions allocate the string into which the result will be
   written.  It is up to the caller to free the return value.  Returns
   NULL on failure of any kind. */

char *hh_bprintf(const char *fmt, ...);
char *hh_vbprintf(const char *fmt, va_list args);

#endif


/* Some convenience macros, in case we don't have or want to use
   `ctype.h'. */

#ifndef HH_IS_DIGIT
#define HH_IS_DIGIT(ch)  ((ch) >= '0' && ch <= '9')
#endif
#ifndef HH_IS_PRINT
#define HH_IS_PRINT(ch)  ((ch) >= 32 && (ch) <= 126)
#endif
 

#endif /* HH_INCL_PRINTF */
