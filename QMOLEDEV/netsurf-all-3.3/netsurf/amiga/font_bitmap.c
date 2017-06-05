/*
 * Copyright 2008 - 2015 Chris Young <chris@unsatisfactorysoftware.co.uk>
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

#include <assert.h>

#include <proto/diskfont.h>
#include <proto/exec.h>
#include <proto/graphics.h>
#include <proto/timer.h>
#include <proto/utility.h>

#include <graphics/rpattr.h>

#include "utils/log.h"
#include "utils/utf8.h"
#include "utils/utils.h"
#include "utils/nsoption.h"
#include "desktop/browser.h"
#include "desktop/font.h"
#include "desktop/gui_window.h"

#include "amiga/font.h"
#include "amiga/gui.h"
#include "amiga/utf8.h"

static struct TextFont *ami_font_bm_open(struct RastPort *rp, const plot_font_style_t *fstyle)
{
	struct TextFont *bmfont = NULL;
	struct TextAttr tattr;
	char *fontname, *font;

	if(rp == NULL) return NULL;

	switch(fstyle->family)
	{
		case PLOT_FONT_FAMILY_SANS_SERIF:
			fontname = nsoption_charp(font_sans);
		break;
		case PLOT_FONT_FAMILY_SERIF:
			fontname = nsoption_charp(font_serif);
		break;
		case PLOT_FONT_FAMILY_MONOSPACE:
			fontname = nsoption_charp(font_mono);
		break;
		case PLOT_FONT_FAMILY_CURSIVE:
			fontname = nsoption_charp(font_cursive);
		break;
		case PLOT_FONT_FAMILY_FANTASY:
			fontname = nsoption_charp(font_fantasy);
		break;
		default:
			return NULL;
		break;
	}

	tattr.ta_Style = FS_NORMAL;

	if (fstyle->flags & FONTF_OBLIQUE)
		tattr.ta_Style = FSF_ITALIC;

	if (fstyle->flags & FONTF_ITALIC)
		tattr.ta_Style = FSF_ITALIC;

	if (fstyle->weight >= 700)
		tattr.ta_Style |= FSF_BOLD;

	if((font = ASPrintf("%s.font", fontname))) {
		tattr.ta_Name = font;
		tattr.ta_YSize = fstyle->size / FONT_SIZE_SCALE;
		LOG(("font: %s/%d", tattr.ta_Name, tattr.ta_YSize));
		if((bmfont = OpenDiskFont(&tattr))) {
			SetRPAttrs(rp, RPTAG_Font, bmfont, TAG_DONE);
		}
		FreeVec(font);
	}

	return bmfont;
}

static void ami_font_bm_close(struct TextFont *bmfont)
{
	CloseFont(bmfont);
}

bool amiga_bm_nsfont_width(const plot_font_style_t *fstyle,
		const char *string, size_t length,
		int *width)
{
	*width = length;

	if((glob == NULL) || (glob->rp == NULL)) return false;

	struct TextFont *bmfont = ami_font_bm_open(glob->rp, fstyle);

	if(bmfont == NULL) return false;

// convert to local charset
	*width = TextLength(glob->rp, string, length);
	ami_font_bm_close(bmfont);

	return true;
}

/**
 * Find the position in a string where an x coordinate falls.
 *
 * \param  fstyle       style for this text
 * \param  string       UTF-8 string to measure
 * \param  length       length of string
 * \param  x            x coordinate to search for
 * \param  char_offset  updated to offset in string of actual_x, [0..length]
 * \param  actual_x     updated to x coordinate of character closest to x
 * \return  true on success, false on error and error reported
 */

bool amiga_bm_nsfont_position_in_string(const plot_font_style_t *fstyle,
		const char *string, size_t length,
		int x, size_t *char_offset, int *actual_x)
{
	struct TextExtent extent;
	struct TextFont *bmfont;

	if((glob == NULL) || (glob->rp == NULL)) return false;

	bmfont = ami_font_bm_open(glob->rp, fstyle);

	if(bmfont == NULL) return false;

	// convert to local charset
	*char_offset = TextFit(glob->rp, string, length,
						&extent, NULL, 1, x, 32767);

	*actual_x = extent.te_Extent.MaxX;

	ami_font_bm_close(bmfont);

	return true;
}


/**
 * Find where to split a string to make it fit a width.
 *
 * \param  fstyle       style for this text
 * \param  string       UTF-8 string to measure
 * \param  length       length of string
 * \param  x            width available
 * \param  char_offset  updated to offset in string of actual_x, [1..length]
 * \param  actual_x     updated to x coordinate of character closest to x
 * \return  true on success, false on error and error reported
 *
 * On exit, char_offset indicates first character after split point.
 *
 * Note: char_offset of 0 should never be returned.
 *
 * Returns:
 * char_offset giving split point closest to x, where actual_x <= x
 * else
 * char_offset giving split point closest to x, where actual_x > x
 *
 * Returning char_offset == length means no split possible
 */

bool amiga_bm_nsfont_split(const plot_font_style_t *fstyle,
		const char *string, size_t length,
		int x, size_t *char_offset, int *actual_x)
{
	struct TextExtent extent;
	ULONG co;
	char *charp;

	if((glob == NULL) || (glob->rp == NULL)) return false;

	struct TextFont *bmfont = ami_font_bm_open(glob->rp, fstyle);

	if(bmfont == NULL) return false;

	co = TextFit(glob->rp, string, length,
				&extent, NULL, 1, x, 32767);

	charp = string + co;

	while(((*charp != ' ')) && (charp > string)) {
		charp--;
		co--;
	}

	if(co > 0) {
		*actual_x = TextLength(glob->rp, string, co);
		*char_offset = co;
	} else {
		*actual_x = x;
		*char_offset = length;
	}

	ami_font_bm_close(bmfont);

	return true;
}

ULONG ami_font_bm_text(struct RastPort *rp, const char *string, ULONG length,
			const plot_font_style_t *fstyle, ULONG dx, ULONG dy)
{
	struct TextFont *bmfont = ami_font_bm_open(rp, fstyle);
	char *localtext = NULL;
	if(bmfont == NULL) return 0;
	if(utf8_to_local_encoding(string, length, &localtext) == NSERROR_OK) {
		Move(rp, dx, dy);
		Text(rp, localtext, length);
		free(localtext);
	}

	return 0;
}

