/*****************************************************************************/
/*                                                                           */
/* Copyright (c) 1989-2008 Morgan Stanley All rights reserved.*/
/* See .../src/LICENSE for terms of distribution.                           */
/*                                                                           */
/*                                                                           */
/*****************************************************************************/
/* contributed by Daniel F. Fisher */

/*
 * used to patch the check sum on an already computed block
 * 
 * Reference: "Fletcher's Error Detection Algorithm: How to implement it
 * efficiently and how to avoid the most common pitfalls," by Anastase
 * Nakassis, COMPUTER COMMUNICATION REVIEW,Vol 18, No 5, October, 1988.
 */

/* header file inclusions */
#include <dap/balloc.h>
#include <dap/misc.h>
#include <dap/fletch.h>

/* external function definitions */
void 
fletchpatch(char *buf, int off, char *new, int len, int k)
{
  struct fletch *p = fletchalloc();
  char *diffs = (char *) balloc(len);
  char *dst_cp = diffs;
  char *end_cp = dst_cp + len;
  char *old_cp = buf + off;
  char *new_cp = new;
  int tmp_c;
  char *xp = buf + k;
  /* save previous x and y */
  int old_x = *((unsigned char *) (xp));
  int old_y = *((unsigned char *) (xp + 1));

  /* zero x and y in buffer */
  *(xp) = *(xp + 1) = (char) (0);

  /* compute difference vector */
  while (dst_cp < end_cp) {
    tmp_c = *(unsigned char *) (new_cp++) +
      (255 - *(unsigned char *) (old_cp++));
    if (tmp_c >= 0)
      tmp_c -= 255;
    *dst_cp++ = tmp_c;
  }

  /* compute c0 and c1 for difference vector */
  fletchsum(p, diffs, len);

  /* get rid of differences */
  bfree(diffs);

  /* copy new over old */
  bcopy(new, buf + off, len);

  /* compute dx and dy */
  fletchdone(p, buf + k, off + len, k);

  /* add back old x and y */
  tmp_c = old_x + *((unsigned char *) (xp));
  if (tmp_c > 255)
    tmp_c -= 255;
  *xp = (char) tmp_c;
  tmp_c = old_y + *((unsigned char *) (xp + 1));
  if (tmp_c > 255)
    tmp_c -= 255;
  *(xp + 1) = (char) tmp_c;

  fletchfree(p);

  return;
}
