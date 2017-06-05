/*
 * Copyright 2015 Chris Young <chris@unsatisfactorysoftware.co.uk>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/** \file
 * Abstract RTG functions for newer/older/non-P96 systems
 */

#include "amiga/rtg.h"

struct BitMap *ami_rtg_allocbitmap(ULONG width, ULONG height, ULONG depth,
	ULONG flags, struct BitMap *friend, RGBFTYPE format)
{
	if(P96Base == NULL) {
#ifndef __amigaos4__
		if(depth > 8) depth = 8;
#endif
		return AllocBitMap(width, height, depth, flags, friend);
	} else {
		return p96AllocBitMap(width, height, depth, flags, friend, format);
	}
}

void ami_rtg_freebitmap(struct BitMap *bm)
{
	if(P96Base == NULL) {
		return FreeBitMap(bm);
	} else {
		return p96FreeBitMap(bm);
	}
}

void ami_rtg_writepixelarray(UBYTE *pixdata, struct BitMap *bm,
	ULONG width, ULONG height, ULONG bpr, ULONG format)
{
	struct RenderInfo ri;
	struct RastPort trp;

	/* This requires P96 currently */
	if(P96Base == NULL) return;

	ri.Memory = pixdata;
	ri.BytesPerRow = bpr;
	ri.RGBFormat = format;

	InitRastPort(&trp);
	trp.BitMap = bm;

	p96WritePixelArray((struct RenderInfo *)&ri, 0, 0, &trp, 0, 0, width, height);
}

