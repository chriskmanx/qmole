/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

static const char cvs_ident[] = "$Id: font.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <math.h>

#include "command.h"
#include "font.h"
#include "startup.h"
#include "options.h"
#include "screen.h"
#include "term.h"
#include "windows.h"

char **etfonts = NULL;
unsigned char font_idx = DEF_FONT_IDX, font_cnt = 0;
int def_font_idx = DEF_FONT_IDX;
char *rs_font[NFONTS];

#ifdef MULTI_CHARSET
char *rs_mfont[NFONTS];
char **etmfonts = NULL;
const char *def_mfontName[] = { MFONT0, MFONT1, MFONT2, MFONT3, MFONT4 };
#endif
const char *def_fontName[] = { FONT0, FONT1, FONT2, FONT3, FONT4 };
unsigned char font_chg = 0;
fontshadow_t fshadow = { {0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 1}, 1 };

static cachefont_t *font_cache = NULL, *cur_font = NULL;
static void font_cache_add(const char *name, unsigned char type, void *info);
static void font_cache_del(const void *info);
static cachefont_t *font_cache_find(const char *name, unsigned char type);
static void *font_cache_find_info(const char *name, unsigned char type);
static unsigned char get_corner(const char *corner);

/* The eterm_font_(add|delete) functions keep track of the names of the terminal fonts
   as defined in the attributes/multicharset contexts.  They do NOT call the cache
   routines.  The caller is responsible for adding/removing fonts to/from the cache. */
void
eterm_font_add(char ***plist, const char *fontname, unsigned char idx)
{

    char **flist;

    D_FONT(("Adding \"%s\" at %u (%8p)\n", NONULL(fontname), (unsigned int) idx, plist));
    ASSERT(plist != NULL);      /* plist is the address of either etfonts or etmfonts */

    /* If we're adding the font at an index we don't have yet, we must resize to fit it. */
    if (idx >= font_cnt) {
        unsigned char new_size = sizeof(char *) * (idx + 1);

        /* The below looks messy with all the cpp stuff, but it really just malloc's/realloc's
           both etfonts and etmfonts at the same time to the same size, zeros all the new
           memory space using MEMSET(), and then prints some goop. */
        if (etfonts) {
            etfonts = (char **) REALLOC(etfonts, new_size);
            MEMSET(etfonts + font_cnt, 0, sizeof(char *) * (idx - font_cnt + 1));
#ifdef MULTI_CHARSET
            etmfonts = (char **) REALLOC(etmfonts, new_size);
            MEMSET(etmfonts + font_cnt, 0, sizeof(char *) * (idx - font_cnt + 1));
            D_FONT((" -> Reallocated font lists:  %u bytes at %8p/%8p\n", new_size, etfonts, etmfonts));
#else
            D_FONT((" -> Reallocated font list:  %u bytes at %8p\n", new_size, etfonts));
#endif
        } else {
            etfonts = (char **) MALLOC(new_size);
            MEMSET(etfonts, 0, new_size);
#ifdef MULTI_CHARSET
            etmfonts = (char **) MALLOC(new_size);
            MEMSET(etmfonts, 0, new_size);
            D_FONT((" -> Allocated font lists:  %u bytes at %8p/%8p\n", new_size, etfonts, etmfonts));
#else
            D_FONT((" -> Allocating font list:  %u bytes at %8p\n", new_size, etfonts));
#endif
        }

        font_cnt = idx + 1;     /* Update the font count. */
#ifdef MULTI_CHARSET
        flist = ((plist == &etfonts) ? (etfonts) : (etmfonts));
#else
        flist = etfonts;
#endif
    } else {
        flist = *plist;
        if (flist[idx]) {
            if ((flist[idx] == fontname) || (!strcasecmp(flist[idx], fontname))) {
                return;         /* We've already got the right font. */
            }
            FREE(flist[idx]);   /* We're replacing an old font.  Get rid of the old name. */
        }
    }
    flist[idx] = STRDUP(fontname);
    DUMP_FONTS();
}

void
eterm_font_delete(char **flist, unsigned char idx)
{

    ASSERT(idx < font_cnt);

    if (flist[idx]) {
        FREE(flist[idx]);
    }
    flist[idx] = NULL;
}

void
eterm_font_list_clear(void)
{
    unsigned char idx;

    for (idx = 0; idx < font_cnt; idx++) {
        eterm_font_delete(etfonts, idx);
#ifdef MULTI_CHARSET
        eterm_font_delete(etmfonts, idx);
#endif
    }
    FREE(etfonts);
#ifdef MULTI_CHARSET
    FREE(etmfonts);
#endif
}

/* These font caching routines keep track of all the various fonts we allocate
   in the X server so that we only allocate each font once.  Saves memory. */
static void
font_cache_add(const char *name, unsigned char type, void *info)
{

    cachefont_t *font;

    D_FONT(("font_cache_add(%s, %d, %8p) called.\n", NONULL(name), type, info));

    /* Allocate the cache info for the font and store the data */
    font = (cachefont_t *) MALLOC(sizeof(cachefont_t));
    font->name = STRDUP(name);
    font->type = type;
    font->ref_cnt = 1;
    switch (type) {
        case FONT_TYPE_X:
            font->fontinfo.xfontinfo = (XFontStruct *) info;
            break;
        case FONT_TYPE_TTF:
            break;
        case FONT_TYPE_FNLIB:
            break;
        default:
            break;
    }
    D_FONT((" -> Created new cachefont_t struct at %p:  \"%s\", %d, %p\n", font, font->name, font->type, font->fontinfo.xfontinfo));

    /* Actually add the struct to the end of our cache linked list. */
    if (!font_cache) {
        font_cache = cur_font = font;
        font->next = NULL;
        D_FONT((" -> Stored as first font in cache.  font_cache == cur_font == font == %p\n", font_cache));
        D_FONT((" -> font_cache->next == cur_font->next == font->next == %p\n", font_cache->next));
    } else {
        D_FONT((" -> font_cache->next == %p, cur_font->next == %p\n", font_cache->next, cur_font->next));
        cur_font->next = font;
        font->next = NULL;
        cur_font = font;
        D_FONT((" -> Stored font in cache.  font_cache == %p, cur_font == %p\n", font_cache, cur_font));
        D_FONT((" -> font_cache->next == %p, cur_font->next == %p\n", font_cache->next, cur_font->next));
    }
}

static void
font_cache_del(const void *info)
{

    cachefont_t *current, *tmp;

    D_FONT(("font_cache_del(%8p) called.\n", info));

    if (!font_cache) {
        return;                 /* No fonts in the cache.  Theoretically this should never happen, but... */
    }

    /* Check the very first entry for a match.  It's a special case. */
    if (((font_cache->type == FONT_TYPE_X) && (font_cache->fontinfo.xfontinfo == (XFontStruct *) info))) {
        D_FONT((" -> Match found at font_cache (%8p).  Font name is \"%s\"\n", font_cache, NONULL(font_cache->name)));
        /* We've got a match.  Decrement the reference count, and if it goes to 0, remove it from the cache. */
        if (--(font_cache->ref_cnt) == 0) {
            D_FONT(("    -> Reference count is now 0.  Deleting from cache.\n"));
            current = font_cache;
            font_cache = current->next;
            XFreeFont(Xdisplay, (XFontStruct *) info);
            FREE(current->name);
            FREE(current);
        } else {
            D_FONT(("    -> Reference count is %d.  Returning.\n", font_cache->ref_cnt));
        }
        return;
#if UNUSED_BLOCK
    } else if ((font_cache->type == FONT_TYPE_TTF) && (0)) {
    } else if ((font_cache->type == FONT_TYPE_FNLIB) && (0)) {
#endif
    } else {
        /* Search for a match.  We test current->next, not current, so that we can
           update the "next" pointer of the font prior to the one we're actually deleting. */
        for (current = font_cache; current->next; current = current->next) {
            if (((current->next->type == FONT_TYPE_X) && (current->next->fontinfo.xfontinfo == (XFontStruct *) info))) {
                D_FONT((" -> Match found at current->next (%8p, current == %8p).  Font name is \"%s\"\n", current->next, current,
                        NONULL(current->next->name)));
                if (--(current->next->ref_cnt) == 0) {
                    D_FONT(("    -> Reference count is now 0.  Deleting from cache.\n"));
                    tmp = current->next;
                    current->next = current->next->next;
                    XFreeFont(Xdisplay, (XFontStruct *) info);
                    if (cur_font == tmp) {
                        cur_font = current;     /* If we're nuking the last entry in the cache, point cur_font to the *new* last entry. */
                    }
                    FREE(tmp->name);
                    FREE(tmp);
                } else {
                    D_FONT(("    -> Reference count is %d.  Returning.\n", font_cache->ref_cnt));
                }
                return;
#if UNUSED_BLOCK
            } else if ((current->next->type == FONT_TYPE_TTF) && (0)) {
            } else if ((current->next->type == FONT_TYPE_FNLIB) && (0)) {
#endif
            }
        }
    }
    /* If we get here, there was no match.  No big deal. */
}

void
font_cache_clear(void)
{
    cachefont_t *current, *tmp;

    D_FONT(("Clearing the font cache.\n"));
    for (current = font_cache; current;) {
        D_FONT((" -> Deleting \"%s\" from cache.\n", current->name));
        tmp = current;
        current = current->next;
        if (tmp->type == FONT_TYPE_X) {
            XFreeFont(Xdisplay, (XFontStruct *) tmp->fontinfo.xfontinfo);
            FREE(tmp->name);
            FREE(tmp);
#if UNUSED_BLOCK
        } else if (current->next->type == FONT_TYPE_TTF) {
        } else if (current->next->type == FONT_TYPE_FNLIB) {
#endif
        }
    }
    font_cache = cur_font = NULL;
}

static cachefont_t *font_cache_find(const char *name, unsigned char type)
{

    cachefont_t *current;

    ASSERT_RVAL(name != NULL, NULL);

    D_FONT(("font_cache_find(%s, %d) called.\n", NONULL(name), type));

    /* Find a matching name/type in the cache.  Just a search; no reference counting happens here. */
    for (current = font_cache; current; current = current->next) {
        D_FONT((" -> Checking current (%8p), type == %d, name == %s\n", current, current->type, NONULL(current->name)));
        if ((current->type == type) && !strcasecmp(current->name, name)) {
            D_FONT(("    -> Match!\n"));
            return (current);
        }
    }
    D_FONT(("No matches found. =(\n"));
    return ((cachefont_t *) NULL);
}

static void *
font_cache_find_info(const char *name, unsigned char type)
{

    cachefont_t *current;

    REQUIRE_RVAL(name != NULL, NULL);

    D_FONT(("font_cache_find_info(%s, %d) called.\n", NONULL(name), type));

    /* This is also a simple search, but it returns the fontinfo rather than the cache entry. */
    for (current = font_cache; current; current = current->next) {
        D_FONT((" -> Checking current (%8p), type == %d, name == %s\n", current, current->type, NONULL(current->name)));
        if ((current->type == type) && !strcasecmp(current->name, name)) {
            D_FONT(("    -> Match!\n"));
            switch (type) {
                case FONT_TYPE_X:
                    return ((void *) current->fontinfo.xfontinfo);
                    break;
                case FONT_TYPE_TTF:
                    return (NULL);
                    break;
                case FONT_TYPE_FNLIB:
                    return (NULL);
                    break;
                default:
                    return (NULL);
                    break;
            }
        }
    }
    D_FONT(("No matches found. =(\n"));
    return (NULL);
}

const char *
get_font_name(void *info)
{
    cachefont_t *current;

    REQUIRE_RVAL(info != NULL, NULL);

    D_FONT(("get_font_name(%8p) called.\n", info));

    /* This is also a simple search, but it returns the fontinfo rather than the cache entry. */
    for (current = font_cache; current; current = current->next) {
        D_FONT((" -> Checking current (%8p), type == %d, name == %s\n", current, current->type, NONULL(current->name)));
        if ((current->type == FONT_TYPE_X) && (((void *) current->fontinfo.xfontinfo) == info)) {
            D_FONT(("    -> Match!\n"));
            return current->name;
#if UNUSED_BLOCK
        } else if ((current->type == FONT_TYPE_TTF) && (0)) {
        } else if ((current->type == FONT_TYPE_FNLIB) && (0)) {
#endif
        }
    }
    D_FONT(("No matches found. =(\n"));
    return (NULL);
}

/* load_font() is the function that should be used to allocate fonts. */
void *
load_font(const char *name, const char *fallback, unsigned char type)
{

    cachefont_t *font;
    XFontStruct *xfont;

    D_FONT(("load_font(%s, %s, %d) called.\n", NONULL(name), NONULL(fallback), type));

    /* Default type is X font. */
    if (type == 0) {
        type = FONT_TYPE_X;
    }

    /* Specify some sane fallbacks */
    if (!name) {
        if (fallback) {
            name = fallback;
            fallback = "fixed";
        } else {
            name = "fixed";
#ifdef MULTI_CHARSET
            fallback = "-misc-fixed-medium-r-normal--13-120-75-75-c-60-iso10646-1";
#else
            fallback = "-misc-fixed-medium-r-normal--13-120-75-75-c-60-iso8859-1";
#endif
        }
    } else if (!fallback) {
        fallback = "fixed";
    }
    D_FONT((" -> Using name == \"%s\" and fallback == \"%s\"\n", name, fallback));

    /* Look for the font name in the cache.  If it's there, add one to the
       reference count and return the existing fontinfo pointer to the caller. */
    if ((font = font_cache_find(name, type))) {
        font_cache_add_ref(font);
        D_FONT((" -> Font found in cache.  Incrementing reference count to %d and returning existing data.\n", font->ref_cnt));
        switch (type) {
            case FONT_TYPE_X:
                return ((void *) font->fontinfo.xfontinfo);
                break;
            case FONT_TYPE_TTF:
                return (NULL);
                break;
            case FONT_TYPE_FNLIB:
                return (NULL);
                break;
            default:
                return (NULL);
                break;
        }
    }

    /* No match in the cache, so we'll have to add it. */
    if (type == FONT_TYPE_X) {
        if (!(xfont = XLoadQueryFont(Xdisplay, name))) {
            libast_print_error("Unable to load font \"%s\".  Falling back on \"%s\"\n", name, fallback);
            if (!(xfont = XLoadQueryFont(Xdisplay, fallback))) {
                libast_fatal_error("Couldn't load the fallback font either.  Giving up.\n");
            } else {
                font_cache_add(fallback, type, (void *) xfont);
            }
        } else {
            font_cache_add(name, type, (void *) xfont);
        }
        return ((void *) xfont);
#if UNUSED_BLOCK
    } else if (type == FONT_TYPE_TTF) {
        return (NULL);
    } else if (type == FONT_TYPE_FNLIB) {
        return (NULL);
#endif
    }
    ASSERT_NOTREACHED_RVAL(NULL);
}

/* free_font() is the external function for deallocating fonts. */
void
free_font(const void *info)
{
    ASSERT(info != NULL);
    font_cache_del(info);
}

/* change_font() handles the font changing escape sequences.  It's also called to
   initialize the terminal fonts (loading and setting up size hints/info).*/
void
change_font(int init, const char *fontname)
{
#ifndef NO_BOLDFONT
    static XFontStruct *boldFont = NULL;
#endif
    short idx = 0, old_idx = font_idx;
    int fh, fw = 0;

    D_FONT(("change_font(%d, \"%s\"):  def_font_idx == %u, font_idx == %u\n", init, NONULL(fontname), (unsigned int) def_font_idx,
            (unsigned int) font_idx));

    if (init) {
        ASSERT(etfonts != NULL);
        if ((def_font_idx >= font_cnt) || (!etfonts[def_font_idx])) {
            def_font_idx = font_idx;
        } else {
            font_idx = def_font_idx;
        }
        ASSERT(etfonts[font_idx] != NULL);
#ifdef MULTI_CHARSET
        ASSERT(etmfonts != NULL);
        ASSERT(etmfonts[font_idx] != NULL);
#endif
    } else {
        ASSERT(fontname != NULL);

        switch (*fontname) {
                /* Empty font name.  Reset to default. */
            case '\0':
                font_idx = def_font_idx;
                fontname = NULL;
                break;

                /* A font escape sequence.  See which one it is. */
            case FONT_CMD:
                idx = atoi(++fontname);
                switch (*fontname) {
                    case '+':
                        NEXT_FONT(idx);
                        break;

                    case '-':
                        PREV_FONT(idx);
                        break;

                    default:
                        if (*fontname != '\0' && !isdigit(*fontname))
                            return;     /* It's not a number.  Punt. */
                        /* Set current font to font N */
                        BOUND(idx, 0, (font_cnt - 1));
                        font_idx = idx;
                        break;
                }
                /* NULL out the fontname so we don't try to load it */
                fontname = NULL;
                break;

            default:
                /* Change to the named font.  See if we already have it, and if so, just set the index. */
                for (idx = 0; idx < font_cnt; idx++) {
                    if (!strcasecmp(etfonts[idx], fontname)) {
                        font_idx = idx;
                        fontname = NULL;
                        break;
                    }
                }
                break;
        }

        /* If we get here with a non-NULL fontname, we have to load a new font.  Rats. */
        if (fontname) {
            eterm_font_add(&etfonts, fontname, font_idx);
        } else if (font_idx == old_idx) {
            /* Sigh.  What a waste of time, changing to the same font. */
            D_FONT((" -> Change to the same font index (%d) we had before?  I don't think so.\n", font_idx));
            return;
        }
    }
    D_FONT((" -> Changing to font index %u (\"%s\")\n", (unsigned int) font_idx, NONULL(etfonts[font_idx])));
    if (TermWin.font) {
        /* If we have a terminal font, but it's not our new current font, free it and load the new one. */
        if (font_cache_find_info(etfonts[font_idx], FONT_TYPE_X) != TermWin.font) {
            free_font(TermWin.font);
            TermWin.font = load_font(etfonts[font_idx], "fixed", FONT_TYPE_X);
        }
    } else {
        /* Load the new font. */
        TermWin.font = load_font(etfonts[font_idx], "fixed", FONT_TYPE_X);
    }

#ifndef NO_BOLDFONT
    if (init && rs_boldFont) {
        /* If we're initializing, load the bold font too. */
        boldFont = load_font(rs_boldFont, "-misc-fixed-bold-r-semicondensed--13-120-75-75-c-60-iso8859-1", FONT_TYPE_X);
    }
#endif

#ifdef MULTI_CHARSET
    if (TermWin.mfont) {
        /* Ditto the above, but for the multi-byte fonts. */
        if (font_cache_find_info(etmfonts[font_idx], FONT_TYPE_X) != TermWin.mfont) {
            free_font(TermWin.mfont);
            TermWin.mfont = load_font(etmfonts[font_idx], "k14", FONT_TYPE_X);
        }
    } else {
        TermWin.mfont = load_font(etmfonts[font_idx], "k14", FONT_TYPE_X);
    }
# ifdef USE_XIM
    /* Changing fonts requires updating the FontSet */
    if (xim_input_context) {
        if (TermWin.fontset) {
            XFreeFontSet(Xdisplay, TermWin.fontset);
        }
        TermWin.fontset = create_fontset(etfonts[font_idx], etmfonts[font_idx]);
        xim_set_fontset();
    }
# endif
#endif /* MULTI_CHARSET */

    if (!init) {
        /* Unless we're initializing, set the font ID in the GC to our new font. */
        XSetFont(Xdisplay, TermWin.gc, TermWin.font->fid);
    }

    /* Check the font dimensions to update our TermWin info */
    fw = TermWin.font->min_bounds.width;
#ifdef MULTI_CHARSET
    fh = (MAX((encoding_method == LATIN1 ? 0 : TermWin.mfont->ascent), TermWin.font->ascent)
          + MAX((encoding_method == LATIN1 ? 0 : TermWin.mfont->descent), TermWin.font->descent)
          + rs_line_space);
#else
    fh = TermWin.font->ascent + TermWin.font->descent + rs_line_space;
#endif

    D_FONT(("Font information:  Ascent == %hd, Descent == %hd, width min/max %d/%d\n", TermWin.font->ascent, TermWin.font->descent,
            TermWin.font->min_bounds.width, TermWin.font->max_bounds.width));
    if (TermWin.font->min_bounds.width == TermWin.font->max_bounds.width)
        TermWin.fprop = 0;      /* Mono-spaced (fixed width) font */
    else
        TermWin.fprop = 1;      /* Proportional font */

    /* For proportional fonts with large size variations, do some math-fu to try and help the appearance */
    if (TermWin.fprop && (BITFIELD_IS_SET(vt_options, VT_OPTIONS_PROPORTIONAL)) && TermWin.font->per_char
        && (TermWin.font->max_bounds.width - TermWin.font->min_bounds.width >= 3)) {
        int cw, n = 0, sum = 0, sumsq = 0, min_w, max_w;
        unsigned int i;
        double dev;

        min_w = fw;
        LOWER_BOUND(min_w, 1);
        max_w = TermWin.font->max_bounds.width;
        UPPER_BOUND(max_w, 1024);
        for (i = TermWin.font->min_char_or_byte2; i <= TermWin.font->max_char_or_byte2; i++) {
            cw = TermWin.font->per_char[i].width;
            if (cw >= min_w && cw <= max_w) {
                sum += cw;
                sumsq += (cw * cw);
                n++;
            }
        }
        if (n) {
            dev = sqrt((sumsq - (sum * sum) / n) / n);
            /* Final font width is the average width plus 2 standard
               deviations, but no larger than the font's max width */
            fw = ((sum / n) + (((int) dev) << 1));
            D_FONT(("Proportional font optimizations:  Average width %d, standard deviation %3.2f, new width %d\n", (sum / n), dev,
                    fw));
            UPPER_BOUND(fw, max_w);
        } else {
            LOWER_BOUND(fw, TermWin.font->max_bounds.width);
        }
    } else {
        LOWER_BOUND(fw, TermWin.font->max_bounds.width);
    }

    /* If the sizes haven't changed, we don't have to update the hints */
    if (fw == TermWin.fwidth && fh == TermWin.fheight) {
        /* but we _do_ need to redraw to show the new font */
        scr_touch();
        return;
    }

    TermWin.fwidth = fw;
    TermWin.fheight = fh;

    /* Check the bold font size and make sure it matches the normal font */
#ifndef NO_BOLDFONT
    TermWin.boldFont = NULL;    /* FIXME:  Memory leak?  Not that anyone uses bold fonts.... */
    if (boldFont) {

        fw = boldFont->min_bounds.width;
        fh = boldFont->ascent + boldFont->descent + rs_line_space;
        if (TermWin.fprop == 0) {       /* bold font must also be monospaced */
            if (fw != boldFont->max_bounds.width)
                fw = -1;
        } else {
            LOWER_BOUND(fw, boldFont->max_bounds.width);
        }

        if (fw == TermWin.fwidth && fh == TermWin.fheight) {
            TermWin.boldFont = boldFont;
        }
    }
#endif /* NO_BOLDFONT */

    set_colorfgbg();

#ifdef ESCREEN
    D_FONT((" -> Escreen mode is %s\n", ((NS_MAGIC_LINE(TermWin.screen_mode)) ? ("on") : ("off"))));
#endif

    TERM_WINDOW_SET_WIDTH();
    TERM_WINDOW_SET_HEIGHT();

    D_FONT((" -> New font width/height = %ldx%ld, making the terminal size %ldx%ld\n",
            TermWin.fwidth, TermWin.fheight, TERM_WINDOW_GET_WIDTH(), TERM_WINDOW_GET_HEIGHT()));

    /* If we're initializing, *we* do the size hints.  If not, resize the parent window. */
    if (init) {
        szHint.width_inc = TermWin.fwidth;
        szHint.height_inc = TermWin.fheight;

        szHint.min_width = szHint.base_width + szHint.width_inc;
        szHint.min_height = szHint.base_height + szHint.height_inc;

        szHint.width = szHint.base_width + TERM_WINDOW_GET_WIDTH();
        szHint.height = szHint.base_height + TERM_WINDOW_GET_HEIGHT();

        szHint.flags = PMinSize | PResizeInc | PBaseSize;
    } else {
        parent_resize();
        font_chg++;
    }
    return;
}

static unsigned char
get_corner(const char *corner)
{
    if (!BEG_STRCASECMP(corner, "tl ") || !BEG_STRCASECMP(corner, "top_left")) {
        return SHADOW_TOP_LEFT;
    } else if (!BEG_STRCASECMP(corner, "t ") || !BEG_STRCASECMP(corner, "top")) {
        return SHADOW_TOP;
    } else if (!BEG_STRCASECMP(corner, "tr ") || !BEG_STRCASECMP(corner, "top_right")) {
        return SHADOW_TOP_RIGHT;
    } else if (!BEG_STRCASECMP(corner, "l ") || !BEG_STRCASECMP(corner, "left")) {
        return SHADOW_LEFT;
    } else if (!BEG_STRCASECMP(corner, "r ") || !BEG_STRCASECMP(corner, "right")) {
        return SHADOW_RIGHT;
    } else if (!BEG_STRCASECMP(corner, "bl ") || !BEG_STRCASECMP(corner, "bottom_left")) {
        return SHADOW_BOTTOM_LEFT;
    } else if (!BEG_STRCASECMP(corner, "b ") || !BEG_STRCASECMP(corner, "bottom")) {
        return SHADOW_BOTTOM;
    } else if (!BEG_STRCASECMP(corner, "br ") || !BEG_STRCASECMP(corner, "bottom_right")) {
        return SHADOW_BOTTOM_RIGHT;
    } else {
        return 255;
    }
}

void
set_shadow_color_by_name(unsigned char which, const char *color_name)
{
    Pixel p;

    ASSERT(which <= 7); // which = 0-7 [SHADOW_TOP_LEFT - SHADOW_BOTTOM_RIGHT]

    p = get_color_by_name(color_name, "#000000");
    fshadow.color[which] = p;
    fshadow.shadow[which] = fshadow.do_shadow = 1;
}

void
set_shadow_color_by_pixel(unsigned char which, Pixel p)
{
    ASSERT(which <= 7); // which = 0-7 [SHADOW_TOP_LEFT - SHADOW_BOTTOM_RIGHT]

    fshadow.color[which] = p;
    fshadow.shadow[which] = fshadow.do_shadow = 1;
}

/* Possible syntax for the font effects line:
   font fx <topleft_color> <topright_color> <bottomleft_color> <bottomright_color>
   font fx outline <color>
   font fx shadow <color>
   font fx emboss <dark_color> <light_color>
   font fx carved <dark_color> <light_color>
   ^^^^^^^
      |
       \- This part is not included in the contents of the line variable.
*/
unsigned char
parse_font_fx(char *line)
{
    char *color, *corner;
    unsigned char which, n;
    Pixel p;

    ASSERT_RVAL(line != NULL, 0);

    n = spiftool_num_words(line);

    if (!BEG_STRCASECMP(line, "none")) {
        MEMSET(&fshadow, 0, sizeof(fontshadow_t));
    } else if (!BEG_STRCASECMP(line, "outline")) {
        if (n != 2) {
            return 0;
        }
        color = spiftool_get_word(2, line);
        p = get_color_by_name(color, "black");
        FREE(color);
        for (which = 0; which < 8; which++) {
            set_shadow_color_by_pixel(which, p);
        }
    } else if (!BEG_STRCASECMP(line, "shadow")) {
        if (n == 2) {
            which = SHADOW_BOTTOM_RIGHT;
            color = spiftool_get_word(2, line);
        } else if (n == 3) {
            color = spiftool_get_word(3, line);
            corner = spiftool_get_pword(2, line);
            which = get_corner(corner);
            if (which >= 8) {
                return 0;
            } else if (which != SHADOW_BOTTOM_RIGHT) {
            	fshadow.shadow[SHADOW_BOTTOM_RIGHT] = 0;
            }
        } else {
            return 0;
        }
        set_shadow_color_by_name(which, color);
        FREE(color);
    } else if (!BEG_STRCASECMP(line, "emboss")) {
        if (n != 3) {
            return 0;
        }
        color = spiftool_get_word(2, line);
        p = get_color_by_name(color, "black");
        set_shadow_color_by_pixel(SHADOW_BOTTOM, p);
        set_shadow_color_by_pixel(SHADOW_BOTTOM_RIGHT, p);
        FREE(color);

        color = spiftool_get_word(3, line);
        p = get_color_by_name(color, "white");
        set_shadow_color_by_pixel(SHADOW_TOP_LEFT, p);
        set_shadow_color_by_pixel(SHADOW_TOP, p);
        FREE(color);
    } else if (!BEG_STRCASECMP(line, "carved")) {
        if (n != 3) {
            return 0;
        }
        color = spiftool_get_word(2, line);
        p = get_color_by_name(color, "black");
        set_shadow_color_by_pixel(SHADOW_TOP_LEFT, p);
        set_shadow_color_by_pixel(SHADOW_TOP, p);
        FREE(color);

        color = spiftool_get_word(3, line);
        p = get_color_by_name(color, "white");
        set_shadow_color_by_pixel(SHADOW_BOTTOM, p);
        set_shadow_color_by_pixel(SHADOW_BOTTOM_RIGHT, p);
        FREE(color);
    } else {
        unsigned char i;

        for (i = 0; i < 4; i++) {
            which = get_corner(line);
            if (which >= 4) {
                which = i;
                color = spiftool_get_word(1, line);
                line = spiftool_get_pword(2, line);
            } else {
                color = spiftool_get_word(2, line);
                line = spiftool_get_pword(3, line);
            }
            set_shadow_color_by_name(which, color);
            FREE(color);
            if (!line) {
                break;
            }
        }
    }
    return 1;
}
