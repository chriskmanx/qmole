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
#include <string.h>
#include "wv.h"

void
wvCopyTAP (TAP * dest, TAP * src)
{
    memcpy (dest, src, sizeof (TAP));
}

void
wvInitTAP (TAP * item)
{
    int i;
    static TAP cache;
    static int test = 0;
    if (!test)
      {
	  cache.jc = 0;
	  cache.dxaGapHalf = 0;
	  cache.dyaRowHeight = 0;
	  cache.fCantSplit = 0;
	  cache.fTableHeader = 0;

	  wvInitTLP (&cache.tlp);

	  cache.lwHTMLProps = 0;
	  cache.fCaFull = 0;
	  cache.fFirstRow = 0;
	  cache.fLastRow = 0;
	  cache.fOutline = 0;
	  cache.reserved = 0;
	  cache.itcMac = 0;
	  cache.dxaAdjust = 0;
	  cache.dxaScale = 0;
	  cache.dxsInch = 0;

	  for (i = 0; i < itcMax + 1; i++)
	      cache.rgdxaCenter[i] = 0;
	  for (i = 0; i < itcMax + 1; i++)
	      cache.rgdxaCenterPrint[i] = 0;
	  for (i = 0; i < itcMax; i++)
	      wvInitTC (&(cache.rgtc[i]));
	  for (i = 0; i < itcMax; i++)
	      wvInitSHD (&(cache.rgshd[i]));
	  for (i = 0; i < 6; i++)
	      wvInitBRC (&(cache.rgbrcTable[i]));
	  test++;
      }
    wvCopyTAP (item, &cache);
}
