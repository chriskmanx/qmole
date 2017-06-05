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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "rc4.h"

static void
swap_byte (unsigned char *a, unsigned char *b)
{
    unsigned char swapByte;

    swapByte = *a;
    *a = *b;
    *b = swapByte;
}

void
prepare_key (unsigned char *key_data_ptr, int key_data_len, rc4_key * key)
{
    unsigned char index1;
    unsigned char index2;
    unsigned char *state;
    unsigned int counter;

    state = &key->state[0];
    for (counter = 0; counter < 256; counter++)
	state[counter] = counter;
    key->x = 0;
    key->y = 0;
    index1 = 0;
    index2 = 0;
    for (counter = 0; counter < 256; counter++)
      {
	  index2 = (key_data_ptr[index1] + state[counter] + index2) % 256;
	  swap_byte (&state[counter], &state[index2]);

	  index1 = (index1 + 1) % key_data_len;
      }
}

void
rc4 (unsigned char *buffer_ptr, int buffer_len, rc4_key * key)
{
    unsigned char x;
    unsigned char y;
    unsigned char *state;
    unsigned char xorIndex;
    unsigned int counter;

    x = key->x;
    y = key->y;

    state = &key->state[0];
    for (counter = 0; counter < buffer_len; counter++)
      {
	  x = (x + 1) % 256;
	  y = (state[x] + y) % 256;
	  swap_byte (&state[x], &state[y]);

	  xorIndex = (state[x] + state[y]) % 256;

	  buffer_ptr[counter] ^= state[xorIndex];
      }
    key->x = x;
    key->y = y;
}

