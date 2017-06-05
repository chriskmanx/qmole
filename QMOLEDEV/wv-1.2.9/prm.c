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

#include <stdlib.h>
#include <stdio.h>
#include "wv.h"

void
wvGetPRM (PRM * item, wvStream * fd)
{
    U16 temp16;
    temp16 = read_16ubit (fd);
    item->fComplex = temp16 & 0x0001;
    wvTrace (
	     ("u16 is %x,fComplex is %d %d\n", temp16, temp16 & 0x0001,
	      item->fComplex));

    if (item->fComplex)
	item->para.var2.igrpprl = (temp16 & 0xfffe) >> 1;
    else
      {
	  item->para.var1.isprm = (temp16 & 0x00fe) >> 1;
	  item->para.var1.val = (temp16 & 0xff00) >> 8;
      }
}

void
wvInitPRM (PRM * item)
{
    item->fComplex = 0;
    item->para.var2.igrpprl = 0;
}
