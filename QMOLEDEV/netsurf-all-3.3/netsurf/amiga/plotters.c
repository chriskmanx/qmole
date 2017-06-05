/*
 * Copyright 2008-09, 2012-13 Chris Young <chris@unsatisfactorysoftware.co.uk>
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

#include "amiga/os3support.h"

#include <proto/exec.h>
#include <proto/intuition.h>

#include <intuition/intuition.h>
#include <graphics/rpattr.h>
#include <graphics/gfxmacros.h>
#include <graphics/gfxbase.h>

#ifdef __amigaos4__
#include <graphics/blitattr.h>
#include <graphics/composite.h>
#endif

#include <math.h>
#include <assert.h>

#include "utils/nsoption.h"
#include "utils/utils.h"
#include "utils/log.h"
#include "css/utils.h"
#include "desktop/mouse.h"
#include "desktop/gui_window.h"

#include "amiga/plotters.h"
#include "amiga/bitmap.h"
#include "amiga/font.h"
#include "amiga/gui.h"
#include "amiga/rtg.h"
#include "amiga/utf8.h"

#ifdef __amigaos4__
static void ami_bitmap_tile_hook(struct Hook *hook,struct RastPort *rp,struct BackFillMessage *bfmsg);
#endif

struct bfbitmap {
	struct BitMap *bm;
	ULONG width;
	ULONG height;
	int offsetx;
	int offsety;
	APTR mask;
};

struct ami_plot_pen {
	struct MinNode node;
	ULONG pen;
};

struct bez_point {
	float x;
	float y;
};

bool palette_mapped = false;

#ifndef M_PI /* For some reason we don't always get this from math.h */
#define M_PI		3.14159265358979323846
#endif

#define PATT_DOT  0xAAAA
#define PATT_DASH 0xCCCC
#define PATT_LINE 0xFFFF

/* This defines the size of the list for Area* functions.
   25000 = 5000 vectors
  */
#define AREA_SIZE 25000

/* Define the below to get additional debug */
#undef AMI_PLOTTER_DEBUG

void ami_init_layers(struct gui_globals *gg, ULONG width, ULONG height)
{
	/* init shared bitmaps                                               *
	 * Height is set to screen width to give enough space for thumbnails *
	 * Also applies to the further gfx/layers functions and memory below */
 
	ULONG depth = 32;
	struct BitMap *friend = NULL;

	depth = GetBitMapAttr(scrn->RastPort.BitMap, BMA_DEPTH);
	LOG(("Screen depth = %d", depth));

	if(depth < 16) {
		palette_mapped = true;
	} else {
		palette_mapped = false;
	}

#ifndef __amigaos4__
#warning OS3 locked to palette-mapped modes
	palette_mapped = true;
	if(depth > 8) depth = 8;
#endif

	if(!width) width = nsoption_int(redraw_tile_size_x);
	if(!height) height = nsoption_int(redraw_tile_size_y);

	gg->layerinfo = NewLayerInfo();
	gg->areabuf = AllocVecTagList(AREA_SIZE, NULL);
	gg->tmprasbuf = AllocVecTagList(width * height, NULL);

#ifndef __amigaos4__
	friend = scrn->RastPort.BitMap;
#endif

	if(palette_mapped == true) {
		gg->bm = AllocBitMap(width, height, depth, 0, friend);
	} else {
#ifdef __amigaos4__
		if(depth == 32) friend = scrn->RastPort.BitMap;
#endif
		gg->bm = ami_rtg_allocbitmap(width, height, 32, 0, friend, RGBFB_A8R8G8B8);
	}

	if(!gg->bm) warn_user("NoMemory","");

	gg->rp = AllocVecTagList(sizeof(struct RastPort), NULL);
	if(!gg->rp) warn_user("NoMemory","");

	InitRastPort(gg->rp);
	gg->rp->BitMap = gg->bm;

	SetDrMd(gg->rp,BGBACKFILL);

	gg->rp->Layer = CreateUpfrontLayer(gg->layerinfo,gg->rp->BitMap,0,0,
					width-1, height-1, LAYERSIMPLE, NULL);

	InstallLayerHook(gg->rp->Layer,LAYERS_NOBACKFILL);

	gg->rp->AreaInfo = AllocVecTagList(sizeof(struct AreaInfo), NULL);

	if((!gg->areabuf) || (!gg->rp->AreaInfo))	warn_user("NoMemory","");

	InitArea(gg->rp->AreaInfo,gg->areabuf, AREA_SIZE/5);
	gg->rp->TmpRas = AllocVecTagList(sizeof(struct TmpRas), NULL);

	if((!gg->tmprasbuf) || (!gg->rp->TmpRas))	warn_user("NoMemory","");

	InitTmpRas(gg->rp->TmpRas, gg->tmprasbuf, width*height);
}

void ami_free_layers(struct gui_globals *gg)
{
	if(gg->rp) {
		DeleteLayer(0,gg->rp->Layer);
		FreeVec(gg->rp->TmpRas);
		FreeVec(gg->rp->AreaInfo);
		FreeVec(gg->rp);
	}

	FreeVec(gg->tmprasbuf);
	FreeVec(gg->areabuf);
	DisposeLayerInfo(gg->layerinfo);
	if(palette_mapped == false) {
		ami_rtg_freebitmap(gg->bm);
	} else {
		FreeBitMap(gg->bm);
	}
}

void ami_clearclipreg(struct gui_globals *gg)
{
	struct Region *reg = NULL;

	reg = InstallClipRegion(gg->rp->Layer,NULL);
	if(reg) DisposeRegion(reg);

	gg->rect.MinX = 0;
	gg->rect.MinY = 0;
	gg->rect.MaxX = scrn->Width-1;
	gg->rect.MaxY = scrn->Height-1;
}

static ULONG ami_plot_obtain_pen(struct MinList *shared_pens, ULONG colr)
{
	struct ami_plot_pen *node;
	LONG pen = ObtainBestPenA(scrn->ViewPort.ColorMap,
			(colr & 0x000000ff) << 24,
			(colr & 0x0000ff00) << 16,
			(colr & 0x00ff0000) << 8,
			NULL);
	
	if(pen == -1) LOG(("WARNING: Cannot allocate pen for ABGR:%lx", colr));

	if(shared_pens != NULL) {
		if((node = (struct ami_plot_pen *)AllocVecTagList(sizeof(struct ami_plot_pen), NULL))) {
			node->pen = pen;
			AddTail((struct List *)shared_pens, (struct Node *)node);
		}
	} else {
		/* Immediately release the pen if we can't keep track of it. */
		ReleasePen(scrn->ViewPort.ColorMap, pen);
	}
	return pen;
}

void ami_plot_release_pens(struct MinList *shared_pens)
{
	struct ami_plot_pen *node;
	struct ami_plot_pen *nnode;

	if(shared_pens == NULL) return;
	if(IsMinListEmpty(shared_pens)) return;
	node = (struct ami_plot_pen *)GetHead((struct List *)shared_pens);

	do {
		nnode = (struct ami_plot_pen *)GetSucc((struct Node *)node);
		ReleasePen(scrn->ViewPort.ColorMap, node->pen);
		Remove((struct Node *)node);
		FreeVec(node);
	} while((node = nnode));
}

static void ami_plot_setapen(struct RastPort *rp, ULONG colr)
{
#ifdef __amigaos4__
	if(palette_mapped == false) {
		SetRPAttrs(rp, RPTAG_APenColor,
			ns_color_to_nscss(colr),
			TAG_DONE);
	} else
#endif
	{
		LONG pen = ami_plot_obtain_pen(glob->shared_pens, colr);
		if(pen != -1) SetAPen(rp, pen);
	}
}

static void ami_plot_setopen(struct RastPort *rp, ULONG colr)
{
#ifdef __amigaos4__
	if(palette_mapped == false) {
		SetRPAttrs(rp, RPTAG_OPenColor,
			ns_color_to_nscss(colr),
			TAG_DONE);
	} else
#endif
	{
		LONG pen = ami_plot_obtain_pen(glob->shared_pens, colr);
		if(pen != -1) SetOPen(rp, pen);
	}
}

void ami_plot_clear_bbox(struct RastPort *rp, struct IBox *bbox)
{
	if((bbox == NULL) || (rp == NULL)) return;

	EraseRect(rp, bbox->Left, bbox->Top,
		bbox->Width + bbox->Left, bbox->Height + bbox->Top);
}


static bool ami_rectangle(int x0, int y0, int x1, int y1, const plot_style_t *style)
{
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_rectangle()"));
	#endif

	if (style->fill_type != PLOT_OP_TYPE_NONE) { 
		ami_plot_setapen(glob->rp, style->fill_colour);
		RectFill(glob->rp, x0, y0, x1-1, y1-1);
	}

	if (style->stroke_type != PLOT_OP_TYPE_NONE) {
		glob->rp->PenWidth = style->stroke_width;
		glob->rp->PenHeight = style->stroke_width;

		switch (style->stroke_type) {
			case PLOT_OP_TYPE_SOLID: /**< Solid colour */
			default:
				glob->rp->LinePtrn = PATT_LINE;
			break;

			case PLOT_OP_TYPE_DOT: /**< Dotted plot */
				glob->rp->LinePtrn = PATT_DOT;
			break;

			case PLOT_OP_TYPE_DASH: /**< dashed plot */
				glob->rp->LinePtrn = PATT_DASH;
			break;
 		}

		ami_plot_setapen(glob->rp, style->stroke_colour);
		Move(glob->rp, x0,y0);
		Draw(glob->rp, x1, y0);
		Draw(glob->rp, x1, y1);
		Draw(glob->rp, x0, y1);
		Draw(glob->rp, x0, y0);

		glob->rp->PenWidth = 1;
		glob->rp->PenHeight = 1;
		glob->rp->LinePtrn = PATT_LINE;
	}

	return true;
}

static bool ami_line(int x0, int y0, int x1, int y1, const plot_style_t *style)
{
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_line()"));
	#endif

	glob->rp->PenWidth = style->stroke_width;
	glob->rp->PenHeight = style->stroke_width;

	switch (style->stroke_type) {
		case PLOT_OP_TYPE_SOLID: /**< Solid colour */
		default:
			glob->rp->LinePtrn = PATT_LINE;
		break;

		case PLOT_OP_TYPE_DOT: /**< Doted plot */
			glob->rp->LinePtrn = PATT_DOT;
		break;

		case PLOT_OP_TYPE_DASH: /**< dashed plot */
			glob->rp->LinePtrn = PATT_DASH;
		break;
	}

	ami_plot_setapen(glob->rp, style->stroke_colour);
	Move(glob->rp,x0,y0);
	Draw(glob->rp,x1,y1);

	glob->rp->PenWidth = 1;
	glob->rp->PenHeight = 1;
	glob->rp->LinePtrn = PATT_LINE;

	return true;
}

static bool ami_polygon(const int *p, unsigned int n, const plot_style_t *style)
{
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_polygon()"));
	#endif

	ami_plot_setapen(glob->rp, style->fill_colour);

	if(AreaMove(glob->rp,p[0],p[1]) == -1)
		LOG(("AreaMove: vector list full"));
			
	for(uint32 k = 1; k < n; k++) {
		if(AreaDraw(glob->rp,p[k*2],p[(k*2)+1]) == -1)
			LOG(("AreaDraw: vector list full"));
	}

	if(AreaEnd(glob->rp) == -1)
		LOG(("AreaEnd: error"));

	return true;
}


static bool ami_clip(const struct rect *clip)
{
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_clip()"));
	#endif

	struct Region *reg = NULL;

	if(glob->rp->Layer)
	{
		reg = NewRegion();

		glob->rect.MinX = clip->x0;
		glob->rect.MinY = clip->y0;
		glob->rect.MaxX = clip->x1-1;
		glob->rect.MaxY = clip->y1-1;

		OrRectRegion(reg,&glob->rect);

		reg = InstallClipRegion(glob->rp->Layer,reg);

		if(reg) DisposeRegion(reg);
	}

	return true;
}

static bool ami_text(int x, int y, const char *text, size_t length,
		const plot_font_style_t *fstyle)
{
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_text()"));
	#endif

	bool aa = true;
	
	if((nsoption_bool(font_antialiasing) == false) || (palette_mapped == true))
		aa = false;
	
	ami_plot_setapen(glob->rp, fstyle->foreground);
	ami_font_unicode_text(glob->rp, text, length, fstyle, x, y, aa);
	
	return true;
}

static bool ami_disc(int x, int y, int radius, const plot_style_t *style)
{
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_disc()"));
	#endif

	if (style->fill_type != PLOT_OP_TYPE_NONE) {
		ami_plot_setapen(glob->rp, style->fill_colour);
		AreaCircle(glob->rp,x,y,radius);
		AreaEnd(glob->rp);
	}

	if (style->stroke_type != PLOT_OP_TYPE_NONE) {
		ami_plot_setapen(glob->rp, style->stroke_colour);
		DrawEllipse(glob->rp,x,y,radius,radius);
	}

	return true;
}

static void ami_arc_gfxlib(int x, int y, int radius, int angle1, int angle2)
{
	double angle1_r = (double)(angle1) * (M_PI / 180.0);
	double angle2_r = (double)(angle2) * (M_PI / 180.0);
	double angle, b, c;
	double step = 0.1; //(angle2_r - angle1_r) / ((angle2_r - angle1_r) * (double)radius);
	int x0, y0, x1, y1;

	x0 = x;
	y0 = y;
	
	b = angle1_r;
	c = angle2_r;
	
	x1 = (int)(cos(b) * (double)radius);
	y1 = (int)(sin(b) * (double)radius);
	Move(glob->rp, x0 + x1, y0 - y1);
		
	for(angle = (b + step); angle <= c; angle += step) {
		x1 = (int)(cos(angle) * (double)radius);
		y1 = (int)(sin(angle) * (double)radius);
		Draw(glob->rp, x0 + x1, y0 - y1);
	}
}

static bool ami_arc(int x, int y, int radius, int angle1, int angle2, const plot_style_t *style)
{
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_arc()"));
	#endif

	if (angle2 < angle1) angle2 += 360;
		
	ami_plot_setapen(glob->rp, style->fill_colour);
	ami_arc_gfxlib(x, y, radius, angle1, angle2);
	
	return true;
}

static bool ami_bitmap(int x, int y, int width, int height, struct bitmap *bitmap)
{
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_bitmap()"));
	#endif

	struct BitMap *tbm;

	if(!width || !height) return true;

	if(((x + width) < glob->rect.MinX) ||
		((y + height) < glob->rect.MinY) ||
		(x > glob->rect.MaxX) ||
		(y > glob->rect.MaxY))
		return true;

	tbm = ami_bitmap_get_native(bitmap, width, height, glob->rp->BitMap);
	if(!tbm) return true;

	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] ami_bitmap() got native bitmap"));
	#endif

#ifdef __amigaos4__
	if(__builtin_expect((GfxBase->LibNode.lib_Version >= 53) && (palette_mapped == false) &&
		(nsoption_bool(direct_render) == false), 1)) {
		uint32 comptype = COMPOSITE_Src_Over_Dest;
		uint32 compflags = COMPFLAG_IgnoreDestAlpha;
		if(bitmap_get_opaque(bitmap)) {
			compflags |= COMPFLAG_SrcAlphaOverride;
			comptype = COMPOSITE_Src;
		}

		CompositeTags(comptype,tbm,glob->rp->BitMap,
					COMPTAG_Flags, compflags,
					COMPTAG_DestX,glob->rect.MinX,
					COMPTAG_DestY,glob->rect.MinY,
					COMPTAG_DestWidth,glob->rect.MaxX - glob->rect.MinX + 1,
					COMPTAG_DestHeight,glob->rect.MaxY - glob->rect.MinY + 1,
					COMPTAG_SrcWidth,width,
					COMPTAG_SrcHeight,height,
					COMPTAG_OffsetX,x,
					COMPTAG_OffsetY,y,
					COMPTAG_FriendBitMap, scrn->RastPort.BitMap,
					TAG_DONE);
	}
	else
#endif
	{
		ULONG tag, tag_data, minterm = 0xc0;
		
		if(palette_mapped == false) {
			tag = BLITA_UseSrcAlpha;
			tag_data = !bitmap->opaque;
			minterm = 0xc0;
		} else {
			tag = BLITA_MaskPlane;
			if((tag_data = (ULONG)ami_bitmap_get_mask(bitmap, width, height, tbm)))
				minterm = (ABC|ABNC|ANBC);
		}
#ifdef __amigaos4__
		BltBitMapTags(BLITA_Width,width,
						BLITA_Height,height,
						BLITA_Source,tbm,
						BLITA_Dest,glob->rp,
						BLITA_DestX,x,
						BLITA_DestY,y,
						BLITA_SrcType,BLITT_BITMAP,
						BLITA_DestType,BLITT_RASTPORT,
						BLITA_Minterm, minterm,
						tag, tag_data,
						TAG_DONE);
#else
		if(tag_data) {
			BltMaskBitMapRastPort(tbm, 0, 0, glob->rp, x, y, width, height, minterm, tag_data);
		} else {
			BltBitMapRastPort(tbm, 0, 0, glob->rp, x, y, width, height, 0xc0);
		}
#endif
	}

	if((bitmap->dto == NULL) && (tbm != bitmap->nativebm)) {
		ami_rtg_freebitmap(tbm);
	}

	return true;
}

static bool ami_bitmap_tile(int x, int y, int width, int height,
			struct bitmap *bitmap, colour bg,
			bitmap_flags_t flags)
{
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_bitmap_tile()"));
	#endif

	int xf,yf,xm,ym,oy,ox;
	struct BitMap *tbm = NULL;
	struct Hook *bfh = NULL;
	struct bfbitmap bfbm;
	bool repeat_x = (flags & BITMAPF_REPEAT_X);
	bool repeat_y = (flags & BITMAPF_REPEAT_Y);

	if((width == 0) || (height == 0)) return true;

	if(!(repeat_x || repeat_y))
		return ami_bitmap(x, y, width, height, bitmap);

	/* If it is a one pixel transparent image, we are wasting our time */
	if((bitmap->opaque == false) && (bitmap->width == 1) && (bitmap->height == 1))
		return true;

	tbm = ami_bitmap_get_native(bitmap,width,height,glob->rp->BitMap);
	if(!tbm) return true;
#ifdef __amigaos4__
	ox = x;
	oy = y;

	/* get left most tile position */
	for (; ox > 0; ox -= width)
	;

	/* get top most tile position */
	for (; oy > 0; oy -= height)
	;

	if(ox<0) ox = -ox;
	if(oy<0) oy = -oy;

	if(repeat_x)
	{
		xf = glob->rect.MaxX;
		xm = glob->rect.MinX;
	}
	else
	{
		xf = x + width;
		xm = x;
	}

	if(repeat_y)
	{
		yf = glob->rect.MaxY;
		ym = glob->rect.MinY;
	}
	else
	{
		yf = y + height;
		ym = y;
	}

	if(bitmap->opaque)
	{
		bfh = CreateBackFillHook(BFHA_BitMap,tbm,
							BFHA_Width,width,
							BFHA_Height,height,
							BFHA_OffsetX,ox,
							BFHA_OffsetY,oy,
							TAG_DONE);
	}
	else
	{
		bfbm.bm = tbm;
		bfbm.width = width;
		bfbm.height = height;
		bfbm.offsetx = ox;
		bfbm.offsety = oy;
		bfbm.mask = ami_bitmap_get_mask(bitmap, width, height, tbm);
		bfh = AllocVecTags(sizeof(struct Hook), AVT_ClearWithValue, 0, TAG_DONE); /* NB: Was not MEMF_PRIVATE */
		bfh->h_Entry = (HOOKFUNC)ami_bitmap_tile_hook;
		bfh->h_SubEntry = 0;
		bfh->h_Data = &bfbm;
	}

	InstallLayerHook(glob->rp->Layer,bfh);

	EraseRect(glob->rp,xm,ym,xf,yf);

	InstallLayerHook(glob->rp->Layer,LAYERS_NOBACKFILL);
	if(bitmap->opaque) DeleteBackFillHook(bfh);
		else FreeVec(bfh);
#else
	/* get left most tile position */
	if (repeat_x)
		for (; x > glob->rect.MinX; x -= width)
			;

	/* get top most tile position */
	if (repeat_y)
		for (; y > glob->rect.MinY; y -= height)
			;

	/* tile down and across to extents */
	for (xf = x; xf < glob->rect.MaxX; xf += width) {
		for (yf = y; yf < glob->rect.MaxY; yf += height) {

			ULONG tag, tag_data = NULL, minterm = 0xc0;
		
			if(bitmap->opaque) {
				minterm = 0xc0;
			} else {
				if((tag_data = (ULONG)ami_bitmap_get_mask(bitmap, width, height, tbm)))
					minterm = (ABC|ABNC|ANBC);
			}

			if(tag_data) {
				BltMaskBitMapRastPort(tbm, 0, 0, glob->rp, x, y, width, height, minterm, tag_data);
			} else {
				BltBitMapRastPort(tbm, 0, 0, glob->rp, x, y, width, height, 0xc0);
			}
		}
	}
#endif

	if((bitmap->dto == NULL) && (tbm != bitmap->nativebm))
	{
		ami_rtg_freebitmap(tbm);
	}

	return true;
}

#ifdef __amigaos4__
static void ami_bitmap_tile_hook(struct Hook *hook,struct RastPort *rp,struct BackFillMessage *bfmsg)
{
	int xf,yf;
	struct bfbitmap *bfbm = (struct bfbitmap *)hook->h_Data;

	/* tile down and across to extents  (bfmsg->Bounds.MinX)*/
	for (xf = -bfbm->offsetx; xf < bfmsg->Bounds.MaxX; xf += bfbm->width) {
		for (yf = -bfbm->offsety; yf < bfmsg->Bounds.MaxY; yf += bfbm->height) {
#ifdef __amigaos4__
			if(__builtin_expect((GfxBase->LibNode.lib_Version >= 53) &&
				(palette_mapped == false), 1)) {
				CompositeTags(COMPOSITE_Src_Over_Dest, bfbm->bm, rp->BitMap,
					COMPTAG_Flags, COMPFLAG_IgnoreDestAlpha,
					COMPTAG_DestX,bfmsg->Bounds.MinX,
					COMPTAG_DestY,bfmsg->Bounds.MinY,
					COMPTAG_DestWidth,bfmsg->Bounds.MaxX - bfmsg->Bounds.MinX + 1,
					COMPTAG_DestHeight,bfmsg->Bounds.MaxY - bfmsg->Bounds.MinY + 1,
					COMPTAG_SrcWidth,bfbm->width,
					COMPTAG_SrcHeight,bfbm->height,
					COMPTAG_OffsetX,xf,
					COMPTAG_OffsetY,yf,
					COMPTAG_FriendBitMap, scrn->RastPort.BitMap,
					TAG_DONE);
			}
			else
#endif
			{
				ULONG tag, tag_data, minterm = 0xc0;
		
				if(palette_mapped == false) {
					tag = BLITA_UseSrcAlpha;
					tag_data = TRUE;
					minterm = 0xc0;
				} else {
					tag = BLITA_MaskPlane;
					if((tag_data = (ULONG)bfbm->mask))
						minterm = (ABC|ABNC|ANBC);
				}
		
				BltBitMapTags(BLITA_Width, bfbm->width,
					BLITA_Height, bfbm->height,
					BLITA_Source, bfbm->bm,
					BLITA_Dest, rp,
					BLITA_DestX, xf,
					BLITA_DestY, yf,
					BLITA_SrcType, BLITT_BITMAP,
					BLITA_DestType, BLITT_RASTPORT,
					BLITA_Minterm, minterm,
					tag, tag_data,
					TAG_DONE);
			}			
		}
	}
}
#endif

static void ami_bezier(struct bez_point *a, struct bez_point *b, struct bez_point *c,
			struct bez_point *d, double t, struct bez_point *p) {
    p->x = pow((1 - t), 3) * a->x + 3 * t * pow((1 -t), 2) * b->x + 3 * (1-t) * pow(t, 2)* c->x + pow (t, 3)* d->x;
    p->y = pow((1 - t), 3) * a->y + 3 * t * pow((1 -t), 2) * b->y + 3 * (1-t) * pow(t, 2)* c->y + pow (t, 3)* d->y;
}

static bool ami_path(const float *p, unsigned int n, colour fill, float width,
			colour c, const float transform[6])
{
	unsigned int i;
	struct bez_point start_p = {0, 0}, cur_p = {0, 0}, p_a, p_b, p_c, p_r;
	
	#ifdef AMI_PLOTTER_DEBUG
	LOG(("[ami_plotter] Entered ami_path()"));
	#endif

	if (n == 0)
		return true;

	if (p[0] != PLOTTER_PATH_MOVE) {
		LOG(("Path does not start with move"));
		return false;
	}

	if (fill != NS_TRANSPARENT) {
		ami_plot_setapen(glob->rp, fill);
		if (c != NS_TRANSPARENT)
			ami_plot_setopen(glob->rp, c);
	} else {
		if (c != NS_TRANSPARENT) {
			ami_plot_setapen(glob->rp, c);
		} else {
			return true; /* wholly transparent */
		}
	}

	/* Construct path */
	for (i = 0; i < n; ) {
		if (p[i] == PLOTTER_PATH_MOVE) {
			if (fill != NS_TRANSPARENT) {
				if(AreaMove(glob->rp, p[i+1], p[i+2]) == -1)
					LOG(("AreaMove: vector list full"));
			} else {
				Move(glob->rp, p[i+1], p[i+2]);
			}
			/* Keep track for future Bezier curves/closes etc */
			start_p.x = p[i+1];
			start_p.y = p[i+2];
			cur_p.x = start_p.x;
			cur_p.y = start_p.y;
			i += 3;
		} else if (p[i] == PLOTTER_PATH_CLOSE) {
			if (fill != NS_TRANSPARENT) {
				if(AreaEnd(glob->rp) == -1)
					LOG(("AreaEnd: error"));
			} else {
				Draw(glob->rp, start_p.x, start_p.y);
			}
			i++;
		} else if (p[i] == PLOTTER_PATH_LINE) {
			if (fill != NS_TRANSPARENT) {
				if(AreaDraw(glob->rp, p[i+1], p[i+2]) == -1)
					LOG(("AreaDraw: vector list full"));
			} else {
				Draw(glob->rp, p[i+1], p[i+2]);
			}
			cur_p.x = p[i+1];
			cur_p.y = p[i+2];
			i += 3;
		} else if (p[i] == PLOTTER_PATH_BEZIER) {
			p_a.x = p[i+1];
			p_a.y = p[i+2];
			p_b.x = p[i+3];
			p_b.y = p[i+4];
			p_c.x = p[i+5];
			p_c.y = p[i+6];

			for(double t = 0.0; t <= 1.0; t += 0.1) {
				ami_bezier(&cur_p, &p_a, &p_b, &p_c, t, &p_r);
				if (fill != NS_TRANSPARENT) {
					if(AreaDraw(glob->rp, p_r.x, p_r.y) == -1)
						LOG(("AreaDraw: vector list full"));
				} else {
					Draw(glob->rp, p_r.x, p_r.y);
				}
			}
			cur_p.x = p_c.x;
			cur_p.y = p_c.y;
			i += 7;
		} else {
			LOG(("bad path command %f", p[i]));
			/* End path for safety if using Area commands */
			if (fill != NS_TRANSPARENT) {
				AreaEnd(glob->rp);
				BNDRYOFF(glob->rp);
			}
			return false;
		}
	}
	if (fill != NS_TRANSPARENT)
		BNDRYOFF(glob->rp);

	return true;
}

bool ami_plot_screen_is_palettemapped(void)
{
	return palette_mapped;
}

struct plotter_table plot;
const struct plotter_table amiplot = {
	.rectangle = ami_rectangle,
	.line = ami_line,
	.polygon = ami_polygon,
	.clip = ami_clip,
	.text = ami_text,
	.disc = ami_disc,
	.arc = ami_arc,
	.bitmap = ami_bitmap_tile,
	.path = ami_path,
	.option_knockout = true,
};

