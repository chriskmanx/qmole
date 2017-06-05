/* wvWare
 * Copyright (C) Caolan McNamara, Dom Lachowicz, and others
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

/* roman.c by Adam Rogoyski (apoc@laker.net) Temperanc on EFNet irc
 * Copyright (C) 1998 Adam Rogoyski
 * Converts Decimal numbers to Roman Numerals and Roman Numberals to
 * Decimals on the command line or in Interactive mode.
 * Uses an expanded Roman Numeral set to handle numbers up to 999999999
 * compile: gcc -o roman roman.c -O2
 * --- GNU General Public License Disclamer ---
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "wv.h"

#define I 1
#define V 5
#define X 10
#define L 50
#define C 100
#define D 500
#define M 1000
#define P 5000
#define Q 10000
#define R 50000
#define S 100000
#define T 500000
#define U 1000000
#define B 5000000
#define W 10000000
#define N 50000000
#define Y 100000000
#define Z 500000000

#define LARGEST 999999999


#include "roman.h"

long
formString (char **r, long v, char a, char b)
{
    *(*r)++ = a;
    if (b)
	*(*r)++ = b;
    return v;
}

char *
decimalToRoman (long decimal, char *roman)
{
    char *r = roman;
    memset (roman, 0, 81);
    r = roman;
    if (decimal > LARGEST || decimal < 1)
      {
	  *r = '\0';
	  wvError (("roman broke\n"));
	  return roman;
      }
    if (decimal >= Z)
	decimal -= formString (&r, Z, 'Z', '\0');
    if (decimal >= Z - Y)
	decimal -= formString (&r, Z - Y, 'Y', 'Z');
    while (decimal >= Y)
	decimal -= formString (&r, Y, 'Y', '\0');
    if (decimal >= Y - W)
	decimal -= formString (&r, Y - W, 'W', 'Y');
    if (decimal >= N)
	decimal -= formString (&r, N, 'N', '\0');
    if (decimal >= N - W)
	decimal -= formString (&r, N - W, 'W', 'N');
    while (decimal >= W)
	decimal -= formString (&r, W, 'W', '\0');
    if (decimal >= W - U)
	decimal -= formString (&r, W - U, 'U', 'W');
    if (decimal >= B)
	decimal -= formString (&r, B, 'B', '\0');
    if (decimal >= B - U)
	decimal -= formString (&r, B - U, 'U', 'B');
    while (decimal >= U)
	decimal -= formString (&r, U, 'U', '\0');
    if (decimal >= U - S)
	decimal -= formString (&r, U - S, 'S', 'U');
    if (decimal >= T)
	decimal -= formString (&r, T, 'T', '\0');
    if (decimal >= T - S)
	decimal -= formString (&r, T - S, 'S', 'T');
    while (decimal >= S)
	decimal -= formString (&r, S, 'S', '\0');
    if (decimal >= S - Q)
	decimal -= formString (&r, S - Q, 'Q', 'S');
    if (decimal >= R)
	decimal -= formString (&r, R, 'R', '\0');
    if (decimal >= R - Q)
	decimal -= formString (&r, R - Q, 'Q', 'R');
    while (decimal >= Q)
	decimal -= formString (&r, Q, 'Q', '\0');
    if (decimal >= Q - M)
	decimal -= formString (&r, Q - M, 'M', 'Q');
    if (decimal >= P)
	decimal -= formString (&r, P, 'P', '\0');
    if (decimal >= P - M)
	decimal -= formString (&r, P - M, 'M', 'P');
    while (decimal >= M)
	decimal -= formString (&r, M, 'M', '\0');
    if (decimal >= M - C)
	decimal -= formString (&r, M - C, 'C', 'M');
    if (decimal >= D)
	decimal -= formString (&r, D, 'D', '\0');
    if (decimal >= D - C)
	decimal -= formString (&r, D - C, 'C', 'D');
    while (decimal >= C)
	decimal -= formString (&r, C, 'C', '\0');
    if (decimal >= C - X)
	decimal -= formString (&r, C - X, 'X', 'C');
    if (decimal >= L)
	decimal -= formString (&r, L, 'L', '\0');
    if (decimal >= L - X)
	decimal -= formString (&r, L - X, 'X', 'L');
    while (decimal >= X)
	decimal -= formString (&r, X, 'X', '\0');
    switch ((int) decimal)
      {
      case 9:
	  *r++ = 'I';
	  *r++ = 'X';
	  break;
      case 8:
	  *r++ = 'V';
	  *r++ = 'I';
	  *r++ = 'I';
	  *r++ = 'I';
	  break;
      case 7:
	  *r++ = 'V';
	  *r++ = 'I';
	  *r++ = 'I';
	  break;
      case 6:
	  *r++ = 'V';
	  *r++ = 'I';
	  break;
      case 4:
	  *r++ = 'I';
      case 5:
	  *r++ = 'V';
	  break;
      case 3:
	  *r++ = 'I';
      case 2:
	  *r++ = 'I';
      case 1:
	  *r++ = 'I';
	  break;
      }
    return roman;
}


long
romanToDecimal (char *roman)
{
    long decimal = 0;
    for (; *roman; roman++)
      {
	  /* Check for four of a letter in a fow */
	  if ((*(roman + 1) && *(roman + 2) && *(roman + 3))
	      && (*roman == *(roman + 1))
	      && (*roman == *(roman + 2)) && (*roman == *(roman + 3)))
	      return 0;
	  /* Check for two five type numbers */
	  if (((*roman == 'V') && (*(roman + 1) == 'V'))
	      || ((*roman == 'L') && (*(roman + 1) == 'L'))
	      || ((*roman == 'D') && (*(roman + 1) == 'D'))
	      || ((*roman == 'P') && (*(roman + 1) == 'P'))
	      || ((*roman == 'R') && (*(roman + 1) == 'R'))
	      || ((*roman == 'T') && (*(roman + 1) == 'T'))
	      || ((*roman == 'B') && (*(roman + 1) == 'B'))
	      || ((*roman == 'N') && (*(roman + 1) == 'N'))
	      || ((*roman == 'Z') && (*(roman + 1) == 'Z')))
	      return 0;
	  /* Check for two lower characters before a larger one */
	  if ((value (*roman) == value (*(roman + 1))) && (*(roman + 2))
	      && (value (*(roman + 1)) < value (*(roman + 2))))
	      return 0;
	  /* Check for the same character on either side of a larger one */
	  if ((*(roman + 1) && *(roman + 2))
	      && (value (*roman) == value (*(roman + 2)))
	      && (value (*roman) < value (*(roman + 1))))
	      return 0;
	  /* Check for illegal nine type numbers */
	  if (!strncmp (roman, "LXL", 3) || !strncmp (roman, "DCD", 3)
	      || !strncmp (roman, "PMP", 3) || !strncmp (roman, "RQR", 3)
	      || !strncmp (roman, "TST", 3) || !strncmp (roman, "BUB", 3)
	      || !strncmp (roman, "NWN", 3) || !strncmp (roman, "VIV", 3))
	      return 0;
	  if (value (*roman) < value (*(roman + 1)))
	    {
		/* check that subtracted value is at least 10% larger, 
		   i.e. 1990 is not MXM, but MCMXC */
		if ((10 * value (*roman)) < value (*(roman + 1)))
		    return 0;
		/* check for double subtraction, i.e. IVX */
		if (value (*(roman + 1)) <= value (*(roman + 2)))
		    return 0;
		/* check for subtracting by a number starting with a 5
		   ie.  VX, LD LM */
		if (*roman == 'V' || *roman == 'L' || *roman == 'D'
		    || *roman == 'P' || *roman == 'R' || *roman == 'T'
		    || *roman == 'B' || *roman == 'N')
		    return 0;
		decimal += value (*(roman + 1)) - value (*roman);
		roman++;
	    }
	  else
	    {
		decimal += value (*roman);
	    }
      }
    return decimal;
}


long
value (char c)
{
    switch (c)
      {
      case 'I':
	  return I;
      case 'V':
	  return V;
      case 'X':
	  return X;
      case 'L':
	  return L;
      case 'C':
	  return C;
      case 'D':
	  return D;
      case 'M':
	  return M;
      case 'P':
	  return P;
      case 'Q':
	  return Q;
      case 'R':
	  return R;
      case 'S':
	  return S;
      case 'T':
	  return T;
      case 'U':
	  return U;
      case 'B':
	  return B;
      case 'W':
	  return W;
      case 'N':
	  return N;
      case 'Y':
	  return Y;
      case 'Z':
	  return Z;
      default:
	  return 0;
      }
}


/* chomp carriage return off end of string */
char *
chomp (char *str)
{
    int i = 0;
    while (*(str + i))
      {
	  if ((*(str + i) == '\n') || (*(str + i) == '\r')
	      || (*(str + i) == '\0'))
	    {
		*(str + i) = 0;
		break;
	    }
	  else
	      i++;
      }
    return (str - i);
}
