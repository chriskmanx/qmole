/* w32_string.c  - Posix emulation layer for Sylpheed (Claws)
 *
 * This file is part of w32lib.
 *
 * w32lib is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * w32lib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information and a list of changes, see w32lib.h
 */

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "w32lib.h"

int strcasecmp( const char *s1, const char *s2 ){
  size_t len1, len2, len;

  len1 = strlen( s1 );
  len2 = strlen( s2 );
  len = ( len1 > len2 )? len1 : len2;

  return strncasecmp( s1, s2, len );
}

int strncasecmp( const char *s1, const char *s2, size_t n ){
  int c1;
  int c2;

  while (n--) {
    c1 = tolower(*(const unsigned char*)s1++);
    c2 = tolower(*(const unsigned char*)s2++);
    if (c1 != c2)
      return c1 - c2;
    else if (c1 == 0 && c2 == 0)
      break;
   }

   return 0;
}

