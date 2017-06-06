/*
 * File:      screen.c
 *
 */

static const char cvs_ident[] = "$Id: screen.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

/* includes */
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif
#include <fcntl.h>
#include <errno.h>
#include <X11/Xatom.h>
#include <X11/Xmd.h>            /* CARD32 */

#include "buttons.h"
#include "command.h"
#include "font.h"
#include "startup.h"
#include "screen.h"
#include "scrollbar.h"
#include "options.h"
#include "pixmap.h"
#include "profile.h"
#include "term.h"

#ifdef ESCREEN
#  include "screamcfg.h"
#endif

static int pb = 0;

/* These arrays store the text and rendering info that were last drawn to the screen. */
static text_t **drawn_text = NULL;
static rend_t **drawn_rend = NULL;

/* These are used for buffering during text scrolls. */
static text_t **buf_text = NULL;
static rend_t **buf_rend = NULL;

/* Tab stop locations */
static char *tabs = NULL;

#ifndef ESCREEN
static
#endif
screen_t screen = {
    NULL, NULL, 0, 0, 0, 0, 0, Screen_DefaultFlags
};

static screen_t swap = {
    NULL, NULL, 0, 0, 0, 0, 0, Screen_DefaultFlags
};

static save_t save = {
    0, 0, 0, 'B', DEFAULT_RSTYLE
};

static selection_t selection = {
    NULL, 0, SELECTION_CLEAR, PRIMARY, 0,
    {0, 0},
    {0, 0},
    {0, 0}
};

static char charsets[4] = {
    'B', 'B', 'B', 'B'          /* all ascii */
};

static short current_screen = PRIMARY;
static rend_t rstyle = DEFAULT_RSTYLE;
static short rvideo = 0;
int prev_nrow = -1, prev_ncol = -1;
unsigned char refresh_all = 0;

#ifdef MULTI_CHARSET
static short multi_byte = 0;
static short lost_multi = 0;
static enum {
    SBYTE, WBYTE
} chstat = SBYTE;
encoding_t encoding_method = LATIN1;

#define RESET_CHSTAT	if (chstat == WBYTE) chstat = SBYTE, lost_multi = 1
#else
#define RESET_CHSTAT
#endif

/* Fill part/all of a drawn line with blanks. */
inline void blank_line(text_t *, rend_t *, int, rend_t);
inline void
blank_line(text_t *et, rend_t *er, int width, rend_t efs)
{
/*    int             i = width; */
    register unsigned int i = width;
    rend_t *r = er, fs = efs;

    MEMSET(et, ' ', i);
    for (; i--;)
        *r++ = fs;
}

/* Create a new row in the screen buffer and initialize it. */
inline void blank_screen_mem(text_t **, rend_t **, int, rend_t);
inline void
blank_screen_mem(text_t **tp, rend_t **rp, int row, rend_t efs)
{
    register unsigned int i = TERM_WINDOW_GET_REPORTED_COLS();
    rend_t *r, fs = efs;

    if (!tp[row]) {
        tp[row] = MALLOC(sizeof(text_t) * (TERM_WINDOW_GET_REPORTED_COLS() + 1));
        rp[row] = MALLOC(sizeof(rend_t) * TERM_WINDOW_GET_REPORTED_COLS());
    }
    MEMSET(tp[row], ' ', i);
    tp[row][i] = 0;
    for (r = rp[row]; i--;)
        *r++ = fs;
}

void
scr_reset(void)
{
    int total_rows, prev_total_rows, chscr = 0;
    register int i, j, k;
    text_t tc;

    D_SCREEN(("scr_reset()\n"));

    TermWin.view_start = 0;
    RESET_CHSTAT;

    if (TERM_WINDOW_GET_REPORTED_COLS() == prev_ncol && TERM_WINDOW_GET_REPORTED_ROWS() == prev_nrow)
        return;

    if (current_screen != PRIMARY) {
        short tmp = TermWin.nrow;

        TermWin.nrow = prev_nrow;
        chscr = scr_change_screen(PRIMARY);
        TermWin.nrow = tmp;
    }
    if (TermWin.ncol <= 0)
        TermWin.ncol = 80;
    if (TermWin.nrow <= 0)
        TermWin.nrow = 24;
    if (TermWin.saveLines <= 0)
        TermWin.saveLines = 0;

    total_rows = TermWin.nrow + TermWin.saveLines;
    prev_total_rows = prev_nrow + TermWin.saveLines;

    screen.tscroll = 0;
    screen.bscroll = (TERM_WINDOW_GET_REPORTED_ROWS() - 1);
    if (prev_nrow == -1) {
        /*
         * A: first time called so just malloc everything : don't rely on realloc
         *    Note: this is still needed so that all the scrollback lines are NULL
         */
        screen.text = CALLOC(text_t *, total_rows);
        buf_text = CALLOC(text_t *, total_rows);
        drawn_text = CALLOC(text_t *, TERM_WINDOW_GET_REPORTED_ROWS());
        swap.text = CALLOC(text_t *, TERM_WINDOW_GET_REPORTED_ROWS());

        screen.rend = CALLOC(rend_t *, total_rows);
        buf_rend = CALLOC(rend_t *, total_rows);
        drawn_rend = CALLOC(rend_t *, TERM_WINDOW_GET_REPORTED_ROWS());
        swap.rend = CALLOC(rend_t *, TERM_WINDOW_GET_REPORTED_ROWS());
        D_SCREEN(("screen.text == %8p, screen.rend == %8p, swap.text == %8p, swap.rend == %8p\n",
                  screen.text, screen.rend, swap.text, swap.rend));

        for (i = 0; i < TERM_WINDOW_GET_REPORTED_ROWS(); i++) {
            j = i + TermWin.saveLines;
            blank_screen_mem(screen.text, screen.rend, j, DEFAULT_RSTYLE);
            blank_screen_mem(swap.text, swap.rend, i, DEFAULT_RSTYLE);
            blank_screen_mem(drawn_text, drawn_rend, i, DEFAULT_RSTYLE);
        }
        TermWin.nscrolled = 0;  /* no saved lines */

    } else {
        /*
         * B1: add or delete rows as appropriate
         */
        if (TERM_WINDOW_GET_REPORTED_ROWS() < prev_nrow) {
            /* delete rows */
            k = MIN(TermWin.nscrolled, prev_nrow - TERM_WINDOW_GET_REPORTED_ROWS());
            scroll_text(0, prev_nrow - 1, k, 1);

            for (i = TERM_WINDOW_GET_REPORTED_ROWS(); i < prev_nrow; i++) {
                j = i + TermWin.saveLines;
                if (screen.text[j]) {
                    FREE(screen.text[j]);
                    FREE(screen.rend[j]);
                }
                if (swap.text[i]) {
                    FREE(swap.text[i]);
                    FREE(swap.rend[i]);
                }
                if (drawn_text[i]) {
                    FREE(drawn_text[i]);
                    FREE(drawn_rend[i]);
                }
            }
            screen.text = REALLOC(screen.text, total_rows * sizeof(text_t *));
            buf_text = REALLOC(buf_text, total_rows * sizeof(text_t *));
            drawn_text = REALLOC(drawn_text, TERM_WINDOW_GET_REPORTED_ROWS() * sizeof(text_t *));
            swap.text = REALLOC(swap.text, TERM_WINDOW_GET_REPORTED_ROWS() * sizeof(text_t *));

            screen.rend = REALLOC(screen.rend, total_rows * sizeof(rend_t *));
            buf_rend = REALLOC(buf_rend, total_rows * sizeof(rend_t *));
            drawn_rend = REALLOC(drawn_rend, TERM_WINDOW_GET_REPORTED_ROWS() * sizeof(rend_t *));
            swap.rend = REALLOC(swap.rend, TERM_WINDOW_GET_REPORTED_ROWS() * sizeof(rend_t *));
            D_SCREEN(("screen.text == %8p, screen.rend == %8p, swap.text == %8p, swap.rend == %8p\n",
                      screen.text, screen.rend, swap.text, swap.rend));

            /* we have fewer rows so fix up number of scrolled lines */
            UPPER_BOUND(screen.row, TERM_WINDOW_GET_REPORTED_ROWS() - 1);

        } else if (TERM_WINDOW_GET_REPORTED_ROWS() > prev_nrow) {
            /* add rows */
            screen.text = REALLOC(screen.text, total_rows * sizeof(text_t *));
            buf_text = REALLOC(buf_text, total_rows * sizeof(text_t *));
            drawn_text = REALLOC(drawn_text, TERM_WINDOW_GET_REPORTED_ROWS() * sizeof(text_t *));
            swap.text = REALLOC(swap.text, TERM_WINDOW_GET_REPORTED_ROWS() * sizeof(text_t *));

            screen.rend = REALLOC(screen.rend, total_rows * sizeof(rend_t *));
            buf_rend = REALLOC(buf_rend, total_rows * sizeof(rend_t *));
            drawn_rend = REALLOC(drawn_rend, TERM_WINDOW_GET_REPORTED_ROWS() * sizeof(rend_t *));
            swap.rend = REALLOC(swap.rend, TERM_WINDOW_GET_REPORTED_ROWS() * sizeof(rend_t *));
            D_SCREEN(("screen.text == %8p, screen.rend == %8p, swap.text == %8p, swap.rend == %8p\n",
                      screen.text, screen.rend, swap.text, swap.rend));

            k = MIN(TermWin.nscrolled, TERM_WINDOW_GET_REPORTED_ROWS() - prev_nrow);
            for (i = prev_total_rows; i < total_rows - k; i++) {
                screen.text[i] = NULL;
                blank_screen_mem(screen.text, screen.rend, i, DEFAULT_RSTYLE);
            }
            for ( /* i = total_rows - k */ ; i < total_rows; i++) {
                screen.text[i] = NULL;
                screen.rend[i] = NULL;
            }
            for (i = prev_nrow; i < TERM_WINDOW_GET_REPORTED_ROWS(); i++) {
                swap.text[i] = NULL;
                drawn_text[i] = NULL;
                blank_screen_mem(swap.text, swap.rend, i, DEFAULT_RSTYLE);
                blank_screen_mem(drawn_text, drawn_rend, i, DEFAULT_RSTYLE);
            }
            if (k > 0) {
                scroll_text(0, TERM_WINDOW_GET_REPORTED_ROWS() - 1, -k, 1);
                screen.row += k;
                TermWin.nscrolled -= k;
                for (i = TermWin.saveLines - TermWin.nscrolled; k--; i--) {
                    if (!screen.text[i]) {
                        blank_screen_mem(screen.text, screen.rend, i, DEFAULT_RSTYLE);
                    }
                }
            }
        }
        /* B2: resize columns */
        if (TERM_WINDOW_GET_REPORTED_COLS() != prev_ncol) {
            for (i = 0; i < total_rows; i++) {
                if (screen.text[i]) {
                    tc = screen.text[i][prev_ncol];
                    screen.text[i] = REALLOC(screen.text[i], (TERM_WINDOW_GET_REPORTED_COLS() + 1) * sizeof(text_t));
                    screen.rend[i] = REALLOC(screen.rend[i], TERM_WINDOW_GET_REPORTED_COLS() * sizeof(rend_t));
                    screen.text[i][TERM_WINDOW_GET_REPORTED_COLS()] = MIN(tc, TERM_WINDOW_GET_REPORTED_COLS());
                    if (TERM_WINDOW_GET_REPORTED_COLS() > prev_ncol)
                        blank_line(&(screen.text[i][prev_ncol]), &(screen.rend[i][prev_ncol]),
                                   TERM_WINDOW_GET_REPORTED_COLS() - prev_ncol, DEFAULT_RSTYLE);
                }
            }
            for (i = 0; i < TERM_WINDOW_GET_REPORTED_ROWS(); i++) {
                drawn_text[i] = REALLOC(drawn_text[i], (TERM_WINDOW_GET_REPORTED_COLS() + 1) * sizeof(text_t));
                drawn_rend[i] = REALLOC(drawn_rend[i], TERM_WINDOW_GET_REPORTED_COLS() * sizeof(rend_t));
                if (swap.text[i]) {
                    tc = swap.text[i][prev_ncol];
                    swap.text[i] = REALLOC(swap.text[i], (TERM_WINDOW_GET_REPORTED_COLS() + 1) * sizeof(text_t));
                    swap.rend[i] = REALLOC(swap.rend[i], TERM_WINDOW_GET_REPORTED_COLS() * sizeof(rend_t));
                    swap.text[i][TERM_WINDOW_GET_REPORTED_COLS()] = MIN(tc, TERM_WINDOW_GET_REPORTED_COLS());
                    if (TERM_WINDOW_GET_REPORTED_COLS() > prev_ncol)
                        blank_line(&(swap.text[i][prev_ncol]), &(swap.rend[i][prev_ncol]),
                                   TERM_WINDOW_GET_REPORTED_COLS() - prev_ncol, DEFAULT_RSTYLE);
                }
                if (TERM_WINDOW_GET_REPORTED_COLS() > prev_ncol)
                    blank_line(&(drawn_text[i][prev_ncol]), &(drawn_rend[i][prev_ncol]),
                               TERM_WINDOW_GET_REPORTED_COLS() - prev_ncol, DEFAULT_RSTYLE);
            }
        }
        if (tabs)
            FREE(tabs);
    }
    tabs = MALLOC(TERM_WINDOW_GET_REPORTED_COLS());

    for (i = 0; i < TERM_WINDOW_GET_REPORTED_COLS(); i++)
        tabs[i] = (i % TABSIZE == 0) ? 1 : 0;

    prev_nrow = TERM_WINDOW_GET_REPORTED_ROWS();
    prev_ncol = TERM_WINDOW_GET_REPORTED_COLS();

    tt_resize();
    if (chscr) {
        scr_change_screen(chscr);
    }
}

/* Release all screen memory. */
void
scr_release(void)
{
    int total_rows;
    register int i;

    total_rows = TERM_WINDOW_GET_REPORTED_ROWS() + TermWin.saveLines;
    for (i = 0; i < total_rows; i++) {
        if (screen.text[i]) {
            FREE(screen.text[i]);
            FREE(screen.rend[i]);
        }
    }
    for (i = 0; i < TERM_WINDOW_GET_REPORTED_ROWS(); i++) {
        FREE(drawn_text[i]);
        FREE(drawn_rend[i]);
        FREE(swap.text[i]);
        FREE(swap.rend[i]);
    }
    FREE(screen.text);
    FREE(screen.rend);
    FREE(drawn_text);
    FREE(drawn_rend);
    FREE(swap.text);
    FREE(swap.rend);
    FREE(buf_text);
    FREE(buf_rend);
    FREE(tabs);
}

/* Perform a full reset on the terminal.  Called by the "\ec" sequence or by an xterm color change. */
void
scr_poweron(void)
{
    D_SCREEN(("scr_poweron()\n"));

    /* Reset all character sets to Latin1 */
    MEMSET(charsets, 'B', sizeof(charsets));
    rvideo = 0;
    /* Reset the rendering style to the default colors/style */
    scr_rendition(0, ~RS_None);
#if NSCREENS
    if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_SECONDARY_SCREEN)) {
        /* Reset the secondary screen */
        scr_change_screen(SECONDARY);
        scr_erase_screen(2);
        swap.tscroll = 0;
        swap.bscroll = TERM_WINDOW_GET_REPORTED_ROWS() - 1;
        swap.row = swap.col = 0;
        swap.charset = 0;
        swap.flags = Screen_DefaultFlags;
    }
#endif
    /* Reset the primary screen */
    scr_change_screen(PRIMARY);
    scr_erase_screen(2);
    screen.row = screen.col = 0;
    screen.charset = 0;
    screen.flags = Screen_DefaultFlags;

    scr_cursor(SAVE);
    TermWin.nscrolled = 0;
    scr_reset();
    scr_refresh(SLOW_REFRESH);
}

/*
 * Save and Restore cursor
 * XTERM_SEQ: Save cursor   : ESC 7
 * XTERM_SEQ: Restore cursor: ESC 8
 */
void
scr_cursor(int mode)
{
    D_SCREEN(("scr_cursor(%s)\n", (mode == SAVE ? "SAVE" : "RESTORE")));

    switch (mode) {
        case SAVE:
            save.row = screen.row;
            save.col = screen.col;
            save.rstyle = rstyle;
            save.charset = screen.charset;
            save.charset_char = charsets[screen.charset];
            break;
        case RESTORE:
            screen.row = save.row;
            screen.col = save.col;
            rstyle = save.rstyle;
            screen.charset = save.charset;
            charsets[screen.charset] = save.charset_char;
            set_font_style();
            break;
    }
}

/*
 * Swap between primary and secondary screens
 * XTERM_SEQ: Primary screen  : ESC [ ? 4 7 h
 * XTERM_SEQ: Secondary screen: ESC [ ? 4 7 l
 */
int
scr_change_screen(int scrn)
{
    int i, offset, tmp;
    text_t *t0;
    rend_t *r0;

    D_SCREEN(("scr_change_screen(%d)\n", scrn));

    TermWin.view_start = 0;
    RESET_CHSTAT;

    if (current_screen == scrn)
        return current_screen;

    SWAP_IT(current_screen, scrn, tmp);
#if NSCREENS
    if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_SECONDARY_SCREEN)) {
        offset = TermWin.saveLines;
        if (!screen.text || !screen.rend)
            return (current_screen);
        for (i = TERM_WINDOW_GET_REPORTED_ROWS(); i--;) {
            SWAP_IT(screen.text[i + offset], swap.text[i], t0);
            SWAP_IT(screen.rend[i + offset], swap.rend[i], r0);
        }
        SWAP_IT(screen.row, swap.row, tmp);
        SWAP_IT(screen.col, swap.col, tmp);
        SWAP_IT(screen.charset, swap.charset, tmp);
        SWAP_IT(screen.flags, swap.flags, tmp);
        screen.flags |= Screen_VisibleCursor;
        swap.flags |= Screen_VisibleCursor;
    }
#else
# ifndef DONT_SCROLL_ME
    if (current_screen == PRIMARY) {
        scroll_text(0, (TERM_WINDOW_GET_REPORTED_ROWS() - 1), TERM_WINDOW_GET_REPORTED_ROWS(), 0);
        for (i = TermWin.saveLines; i < TERM_WINDOW_GET_REPORTED_ROWS() + TermWin.saveLines; i++)
            if (!screen.text[i]) {
                blank_screen_mem(screen.text, screen.rend, i, DEFAULT_RSTYLE);
            }
    }
# endif
#endif

    return scrn;
}

/*
 * Change the color for following text
 */
void
scr_color(unsigned int color, unsigned int Intensity)
{

    D_SCREEN(("scr_color(%u, %u) called.\n", color, Intensity));
    if (color == restoreFG)
        color = fgColor;
    else if (color == restoreBG)
        color = bgColor;
    else {
        if (Xdepth <= 2) {      /* Monochrome - ignore color changes */
            switch (Intensity) {
                case RS_Bold:
                    color = fgColor;
                    break;
                case RS_Blink:
                    color = bgColor;
                    break;
            }
        } else {
            if ((rstyle & Intensity) && ((int) color >= minColor) && (color <= maxColor)) {
                switch (Intensity) {
                    case RS_Bold:
                        if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_BOLD_BRIGHTENS_FOREGROUND)) {
                            color += (minBright - minColor);
                        }
                        break;
                    case RS_Blink:
                        if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_BLINK_BRIGHTENS_BACKGROUND)) {
                            color += (minBright - minColor);
                        }
                        break;
                }
            } else if (!(rstyle & Intensity) && ((int) color >= minBright) && (color <= maxBright)) {
                switch (Intensity) {
                    case RS_Bold:
                        if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_BOLD_BRIGHTENS_FOREGROUND)) {
                            color -= (minBright - minColor);
                        }
                        break;
                    case RS_Blink:
                        if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_BLINK_BRIGHTENS_BACKGROUND)) {
                            color -= (minBright - minColor);
                        }
                        break;
                }
            }
        }
    }
    switch (Intensity) {
        case RS_Bold:
            rstyle = SET_FGCOLOR(rstyle, color);
            break;
        case RS_Blink:
            rstyle = SET_BGCOLOR(rstyle, color);
            break;
    }
}

/*
 * Change the rendition style for following text
 */
void
scr_rendition(int set, int style)
{
    unsigned int color;
    int old_style = rstyle;

    D_SCREEN(("scr_rendition(%d, %d) called.\n", set, style));
    if (set) {
/* A: Set style */
        rstyle |= style;
        switch (style) {
            case RS_RVid:
                if (rvideo)
                    rstyle &= ~RS_RVid;
                break;
            case RS_Bold:
                color = GET_FGCOLOR(rstyle);
                scr_color((color == fgColor ? GET_FGCOLOR(colorfgbg) : color), RS_Bold);
                break;
            case RS_Blink:
                color = GET_BGCOLOR(rstyle);
                scr_color((color == bgColor ? GET_BGCOLOR(colorfgbg) : color), RS_Blink);
                break;
        }
    } else {
/* B: Unset style */
        rstyle &= ~style;

        switch (style) {
            case ~RS_None:     /* default fg/bg colors */
                rstyle = DEFAULT_RSTYLE | (old_style & RS_fontMask);
                /* FALLTHROUGH */
            case RS_RVid:
                if (rvideo)
                    rstyle |= RS_RVid;
                break;
            case RS_Bold:
                color = GET_FGCOLOR(rstyle);
                if (color >= minBright && color <= maxBright) {
                    scr_color(color, RS_Bold);
                    if ((rstyle & RS_fgMask) == (colorfgbg & RS_fgMask))
                        scr_color(restoreFG, RS_Bold);
                }
                break;
            case RS_Blink:
                color = GET_BGCOLOR(rstyle);
                if (color >= minBright && color <= maxBright) {
                    scr_color(color, RS_Blink);
                    if ((rstyle & RS_bgMask) == (colorfgbg & RS_bgMask))
                        scr_color(restoreBG, RS_Blink);
                }
                break;
        }
    }
}

/* Scroll text region from <row1> through <row2> by <count> lines */
int
scroll_text(int row1, int row2, int count, int spec)
{
    register int i, j;

    PROF_INIT(scroll_text);
    D_SCREEN(("scroll_text(%d,%d,%d,%d): %s\n", row1, row2, count, spec, (current_screen == PRIMARY) ? "Primary" : "Secondary"));

    if (count == 0 || (row1 > row2))
        return 0;
    if ((count > 0) && (row1 == 0) && (current_screen == PRIMARY)) {
        TermWin.nscrolled += count;
        UPPER_BOUND(TermWin.nscrolled, TermWin.saveLines);
    } else if (!spec)
        row1 += TermWin.saveLines;
    row2 += TermWin.saveLines;

    if (selection.op && current_screen == selection.screen) {
        i = selection.beg.row + TermWin.saveLines;
        j = selection.end.row + TermWin.saveLines;
        if ((i < row1 && j > row1)
            || (i < row2 && j > row2)
            || (i - count < row1 && i >= row1)
            || (i - count > row2 && i <= row2)
            || (j - count < row1 && j >= row1)
            || (j - count > row2 && j <= row2)) {
            CLEAR_ALL_SELECTION;
            selection.op = SELECTION_CLEAR;
        } else if (j >= row1 && j <= row2) {
            /* move selected region too */
            selection.beg.row -= count;
            selection.end.row -= count;
            selection.mark.row -= count;
        }
    }
    CHECK_SELECTION;

    if (count > 0) {

/* A: scroll up */

        UPPER_BOUND(count, row2 - row1 + 1);

/* A1: Copy and blank out lines that will get clobbered by the rotation */
        for (i = 0, j = row1; i < count; i++, j++) {
            buf_text[i] = screen.text[j];
            buf_rend[i] = screen.rend[j];
            if (!buf_text[i]) {
                /* A new ALLOC is done with size ncol and
                   blankline with size prev_ncol -- Sebastien van K */
                buf_text[i] = MALLOC(sizeof(text_t) * (prev_ncol + 1));
                buf_rend[i] = MALLOC(sizeof(rend_t) * prev_ncol);
            }
            blank_line(buf_text[i], buf_rend[i], prev_ncol, DEFAULT_RSTYLE);
            buf_text[i][prev_ncol] = 0;
        }
/* A2: Rotate lines */
        for (j = row1; (j + count) <= row2; j++) {
            screen.text[j] = screen.text[j + count];
            screen.rend[j] = screen.rend[j + count];
        }
/* A3: Resurrect lines */
        for (i = 0; i < count; i++, j++) {
            screen.text[j] = buf_text[i];
            screen.rend[j] = buf_rend[i];
        }
    } else if (count < 0) {
/* B: scroll down */

        count = MIN(-count, row2 - row1 + 1);
/* B1: Copy and blank out lines that will get clobbered by the rotation */
        for (i = 0, j = row2; i < count; i++, j--) {
            buf_text[i] = screen.text[j];
            buf_rend[i] = screen.rend[j];
            if (!buf_text[i]) {
                /* A new ALLOC is done with size ncol and
                   blankline with size prev_ncol -- Sebastien van K */
                buf_text[i] = MALLOC(sizeof(text_t) * (prev_ncol + 1));
                buf_rend[i] = MALLOC(sizeof(rend_t) * prev_ncol);
            }
            blank_line(buf_text[i], buf_rend[i], prev_ncol, DEFAULT_RSTYLE);
            buf_text[i][prev_ncol] = 0;
        }
/* B2: Rotate lines */
        for (j = row2; (j - count) >= row1; j--) {
            screen.text[j] = screen.text[j - count];
            screen.rend[j] = screen.rend[j - count];
        }
/* B3: Resurrect lines */
        for (i = 0, j = row1; i < count; i++, j++) {
            screen.text[j] = buf_text[i];
            screen.rend[j] = buf_rend[i];
        }
        count = -count;
    }
    PROF_DONE(scroll_text);
    PROF_TIME(scroll_text);
    return count;
}

/*
 * Add text given in <str> of length <len> to screen struct
 */
void
scr_add_lines(const unsigned char *str, int nlines, int len)
{
/*    char            c; */
    register char c;

/*    int             i, j, row, last_col; */
    int last_col;
    register int i, j, row;
    text_t *stp;
    rend_t *srp;
    row_col_t beg, end;

    if (len <= 0)               /* sanity */
        return;

    last_col = TERM_WINDOW_GET_REPORTED_COLS();

    D_SCREEN(("scr_add_lines(*,%d,%d)\n", nlines, len));
    ZERO_SCROLLBACK;
    if (nlines > 0) {
        nlines += (screen.row - screen.bscroll);
        D_SCREEN((" -> screen.row == %d, screen.bscroll == %d, new nlines == %d\n", screen.row, screen.bscroll, nlines));
        if ((nlines > 0) && (screen.tscroll == 0) && (screen.bscroll == (TERM_WINDOW_GET_REPORTED_ROWS() - 1))) {
            /* _at least_ this many lines need to be scrolled */
            scroll_text(screen.tscroll, screen.bscroll, nlines, 0);
            for (i = nlines, row = screen.bscroll + TermWin.saveLines + 1; row > 0 && i--;) {
                /* Move row-- to beginning of loop to avoid segfault. -- added by Sebastien van K */
                row--;
                blank_screen_mem(screen.text, screen.rend, row, rstyle);
            }
            screen.row -= nlines;
        }
    }
    UPPER_BOUND(screen.col, last_col - 1);
    BOUND(screen.row, -TermWin.nscrolled, TERM_WINDOW_GET_REPORTED_ROWS() - 1);

    row = screen.row + TermWin.saveLines;
    if (!screen.text[row]) {
        blank_screen_mem(screen.text, screen.rend, row, DEFAULT_RSTYLE);
    }                           /* avoid segfault -- added by Sebastien van K */
    beg.row = screen.row;
    beg.col = screen.col;
    stp = screen.text[row];
    srp = screen.rend[row];

#ifdef MULTI_CHARSET
    if (lost_multi && screen.col > 0 && ((srp[screen.col - 1] & RS_multiMask) == RS_multi1)
        && *str != '\n' && *str != '\r' && *str != '\t')
        chstat = WBYTE;
#endif

    for (i = 0; i < len;) {
        c = str[i++];
#ifdef MULTI_CHARSET
        if ((encoding_method != LATIN1) && (chstat == WBYTE)) {
            rstyle |= RS_multiMask;     /* multibyte 2nd byte */
            chstat = SBYTE;
            if (encoding_method == EUCJ) {
                c |= 0x80;      /* maybe overkill, but makes it selectable */
            }
        } else if (chstat == SBYTE) {
            if ((encoding_method != LATIN1) && (multi_byte || (c & 0x80))) {    /* multibyte 1st byte */
                rstyle &= ~RS_multiMask;
                rstyle |= RS_multi1;
                chstat = WBYTE;
                if (encoding_method == EUCJ) {
                    c |= 0x80;  /* maybe overkill, but makes it selectable */
                }
            } else
#endif
                switch (c) {
                    case 127:
                        continue;       /* ummmm..... */
                    case '\t':
                        scr_tab(1);
                        continue;
                    case '\n':
                        LOWER_BOUND(stp[last_col], screen.col);
                        screen.flags &= ~Screen_WrapNext;
                        if (screen.row == screen.bscroll) {
                            scroll_text(screen.tscroll, screen.bscroll, 1, 0);
                            j = screen.bscroll + TermWin.saveLines;
                            blank_screen_mem(screen.text, screen.rend, j, rstyle & ~(RS_Uline | RS_Overscore));
                        } else if (screen.row < (TERM_WINDOW_GET_REPORTED_ROWS() - 1)) {
                            screen.row++;
                            row = screen.row + TermWin.saveLines;
                        }
                        stp = screen.text[row]; /* _must_ refresh */
                        srp = screen.rend[row]; /* _must_ refresh */
                        continue;
                    case '\r':
                        LOWER_BOUND(stp[last_col], screen.col);
                        screen.flags &= ~Screen_WrapNext;
                        screen.col = 0;
                        continue;
                    default:
#ifdef MULTI_CHARSET
                        rstyle &= ~RS_multiMask;
#endif
                        break;
                }
#ifdef MULTI_CHARSET
        }
#endif
        if (screen.flags & Screen_WrapNext) {
            stp[last_col] = WRAP_CHAR;
            if (screen.row == screen.bscroll) {
                scroll_text(screen.tscroll, screen.bscroll, 1, 0);
                j = screen.bscroll + TermWin.saveLines;
                /* blank_line(screen.text[j], screen.rend[j], TermWin.ncol,
                   rstyle);    Bug fix from John Ellison - need to reset rstyle */
                blank_screen_mem(screen.text, screen.rend, j, rstyle & ~(RS_Uline | RS_Overscore));
            } else if (screen.row < (TERM_WINDOW_GET_REPORTED_ROWS() - 1)) {
                screen.row++;
                row = screen.row + TermWin.saveLines;
            }
            stp = screen.text[row];     /* _must_ refresh */
            srp = screen.rend[row];     /* _must_ refresh */
            screen.col = 0;
            screen.flags &= ~Screen_WrapNext;
        }
        if (screen.flags & Screen_Insert)
            scr_insdel_chars(1, INSERT);
        stp[screen.col] = c;
        srp[screen.col] = rstyle;
        if (screen.col < (last_col - 1))
            screen.col++;
        else {
            stp[last_col] = last_col;
            if (screen.flags & Screen_Autowrap)
                screen.flags |= Screen_WrapNext;
            else
                screen.flags &= ~Screen_WrapNext;
        }
    }
    LOWER_BOUND(stp[last_col], screen.col);
    if (screen.col == 0) {
        end.col = last_col - 1;
        end.row = screen.row - 1;
    } else {
        end.col = screen.col - 1;
        end.row = screen.row;
    }
    if (((selection.end.row > beg.row)
         || (selection.end.row == beg.row && selection.end.col >= beg.col))
        && ((selection.beg.row < end.row)
            || (selection.beg.row == end.row && selection.beg.col <= end.col)))
        selection_reset();

#ifdef ESCREEN
    if (NS_MAGIC_LINE(TermWin.screen_mode)) {
        if (screen.row >= TERM_WINDOW_GET_ROWS()) {     /* last row -> upd-flag */
            TermWin.screen_pending |= 1;
        }
    }
#endif
}

/*
 * Process Backspace.  Move back the cursor back a position, wrap if have to
 * XTERM_SEQ: CTRL-H
 */
void
scr_backspace(void)
{

    RESET_CHSTAT;
    if (screen.col == 0 && screen.row > 0) {
        screen.col = TERM_WINDOW_GET_REPORTED_COLS() - 1;
        screen.row--;
    } else if (screen.flags & Screen_WrapNext) {
        screen.flags &= ~Screen_WrapNext;
    } else
        scr_gotorc(0, -1, RELATIVE);
}

/*
 * Process Horizontal Tab
 * count: +ve = forward; -ve = backwards
 * XTERM_SEQ: CTRL-I
 */
void
scr_tab(int count)
{
    int i, x;

    RESET_CHSTAT;
    x = screen.col;
    if (count == 0)
        return;
    else if (count > 0) {
        for (i = x + 1; i < TERM_WINDOW_GET_REPORTED_COLS(); i++) {
            if (tabs[i]) {
                x = i;
                if (!--count)
                    break;
            }
        }
    } else if (count < 0) {
        for (i = x - 1; i >= 0; i--) {
            if (tabs[i]) {
                x = i;
                if (!++count)
                    break;
            }
        }
    }
    if (x != screen.col)
        scr_gotorc(0, x, R_RELATIVE);
}

/*
 * Goto Row/Column
 */
void
scr_gotorc(int row, int col, int relative)
{
    ZERO_SCROLLBACK;
    RESET_CHSTAT;

    screen.col = ((relative & C_RELATIVE) ? (screen.col + col) : col);
    BOUND(screen.col, 0, TERM_WINDOW_GET_REPORTED_COLS() - 1);

    if (screen.flags & Screen_WrapNext) {
        screen.flags &= ~Screen_WrapNext;
    }
    if (relative & R_RELATIVE) {
        if (row > 0) {
            if (screen.row <= screen.bscroll && (screen.row + row) > screen.bscroll)
                screen.row = screen.bscroll;
            else
                screen.row += row;
        } else if (row < 0) {
            if (screen.row >= screen.tscroll && (screen.row + row) < screen.tscroll)
                screen.row = screen.tscroll;
            else
                screen.row += row;
        }
    } else {
        if (screen.flags & Screen_Relative) {   /* relative origin mode */
            screen.row = row + screen.tscroll;
            UPPER_BOUND(screen.row, screen.bscroll);
        } else
            screen.row = row;
    }
#ifdef ESCREEN
    if (NS_MAGIC_LINE(TermWin.screen_mode)) {
        if (screen.row >= TERM_WINDOW_GET_ROWS()) {     /* last row -> upd-flag */
            TermWin.screen_pending |= 1;
        } else if (TermWin.screen_pending) {    /* left last -> upd-finis */
            TermWin.screen_pending |= 2;
        }
    }
#endif
    BOUND(screen.row, 0, TERM_WINDOW_GET_REPORTED_ROWS() - 1);
}

/*
 * direction  should be UP or DN
 */
void
scr_index(int direction)
{
    int dirn;

    dirn = ((direction == UP) ? 1 : -1);
    D_SCREEN(("scr_index(%d)\n", dirn));

    ZERO_SCROLLBACK;
    RESET_CHSTAT;

    if (screen.flags & Screen_WrapNext) {
        screen.flags &= ~Screen_WrapNext;
    }
    if ((screen.row == screen.bscroll && direction == UP)
        || (screen.row == screen.tscroll && direction == DN)) {
        scroll_text(screen.tscroll, screen.bscroll, dirn, 0);
        if (direction == UP)
            dirn = screen.bscroll + TermWin.saveLines;
        else
            dirn = screen.tscroll + TermWin.saveLines;
        blank_screen_mem(screen.text, screen.rend, dirn, rstyle);
    } else
        screen.row += dirn;
    BOUND(screen.row, 0, TERM_WINDOW_GET_REPORTED_ROWS() - 1);
    CHECK_SELECTION;
}

/*
 * Erase part or whole of a line
 * XTERM_SEQ: Clear line to right: ESC [ 0 K
 * XTERM_SEQ: Clear line to left : ESC [ 1 K
 * XTERM_SEQ: Clear whole line   : ESC [ 2 K
 */
void
scr_erase_line(int mode)
{
    int row, col, num;

    D_SCREEN(("scr_erase_line(%d) at screen row: %d\n", mode, screen.row));
    ZERO_SCROLLBACK;
    RESET_CHSTAT;

    if (screen.flags & Screen_WrapNext) {
        screen.flags &= ~Screen_WrapNext;
    }

    row = TermWin.saveLines + screen.row;
    ASSERT(row < TERM_WINDOW_GET_REPORTED_ROWS() + TermWin.saveLines);

    if (screen.text[row]) {
        switch (mode) {
            case 0:            /* erase to end of line */
                col = screen.col;
                num = TERM_WINDOW_GET_REPORTED_COLS() - col;
                UPPER_BOUND(screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()], col);
                break;
            case 1:            /* erase to beginning of line */
                col = 0;
                num = screen.col + 1;
                break;
            case 2:            /* erase whole line */
                col = 0;
                num = TERM_WINDOW_GET_REPORTED_COLS();
                screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()] = 0;
                break;
            default:
                return;
        }
        blank_line(&(screen.text[row][col]), &(screen.rend[row][col]), num, rstyle & ~(RS_Uline | RS_Overscore));
    } else {
        blank_screen_mem(screen.text, screen.rend, row, rstyle & ~(RS_Uline | RS_Overscore));
    }
}

/*
 * Erase part of whole of the screen
 * XTERM_SEQ: Clear screen after cursor : ESC [ 0 J
 * XTERM_SEQ: Clear screen before cursor: ESC [ 1 J
 * XTERM_SEQ: Clear whole screen        : ESC [ 2 J
 */
void
scr_erase_screen(int mode)
{
    int row, num, row_offset;
    rend_t ren;
    long gcmask;
    XGCValues gcvalue;
    Pixmap pmap = None;
    Drawable draw_buffer;

    if (buffer_pixmap) {
        draw_buffer = buffer_pixmap;
        pmap = images[image_bg].current->pmap->pixmap;
    } else {
        draw_buffer = TermWin.vt;
    }

    D_SCREEN(("scr_erase_screen(%d) at screen row: %d\n", mode, screen.row));
    REFRESH_ZERO_SCROLLBACK;
    RESET_CHSTAT;
    row_offset = TermWin.saveLines;


    switch (mode) {
        case 0:                /* erase to end of screen */
            scr_erase_line(0);
            row = screen.row + 1;       /* possible OOB */
            num = TERM_WINDOW_GET_REPORTED_ROWS() - row;
            break;
        case 1:                /* erase to beginning of screen */
            scr_erase_line(1);
            row = 0;            /* possible OOB */
            num = screen.row;
            break;
        case 2:                /* erase whole screen */
            row = 0;
            num = TERM_WINDOW_GET_REPORTED_ROWS();
            break;
        default:
            return;
    }
    if (row >= 0 && row <= TERM_WINDOW_GET_REPORTED_ROWS()) {   /* check OOB */
        UPPER_BOUND(num, (TERM_WINDOW_GET_REPORTED_ROWS() - row));
        if (rstyle & RS_RVid || rstyle & RS_Uline || rstyle & RS_Overscore)
            ren = -1;
        else {
            if (GET_BGCOLOR(rstyle) == bgColor) {
                ren = DEFAULT_RSTYLE;
                CLEAR_ROWS(row, num);
            } else {
                ren = (rstyle & (RS_fgMask | RS_bgMask));
                gcvalue.foreground = PixColors[GET_BGCOLOR(ren)];
                gcmask = GCForeground;
                XChangeGC(Xdisplay, TermWin.gc, gcmask, &gcvalue);
                ERASE_ROWS(row, num);
                gcvalue.foreground = PixColors[fgColor];
                XChangeGC(Xdisplay, TermWin.gc, gcmask, &gcvalue);
            }
        }
        for (; num--; row++) {
            blank_screen_mem(screen.text, screen.rend, row + row_offset, rstyle & ~(RS_RVid | RS_Uline | RS_Overscore));
            blank_screen_mem(drawn_text, drawn_rend, row, ren);
        }
    }
}

/*
 * Fill the screen with `E's
 * XTERM_SEQ: Screen Alignment Test: ESC # 8
 */
void
scr_E(void)
{
    int i, j;
    text_t *t;
    rend_t *r, fs;

    ZERO_SCROLLBACK;
    RESET_CHSTAT;

    fs = rstyle;
    for (i = TermWin.saveLines; i < TERM_WINDOW_GET_REPORTED_ROWS() + TermWin.saveLines; i++) {
        t = screen.text[i];
        r = screen.rend[i];
        for (j = 0; j < TERM_WINDOW_GET_REPORTED_COLS(); j++) {
            *t++ = 'E';
            *r++ = fs;
        }
        *t = '\0';
    }
}

/*
 * Insert/Delete <count> lines
 */
void
scr_insdel_lines(int count, int insdel)
{
    int end;

    ZERO_SCROLLBACK;
    RESET_CHSTAT;

    if (screen.row > screen.bscroll)
        return;

    end = screen.bscroll - screen.row + 1;
    if (count > end) {
        if (insdel == DELETE)
            return;
        else if (insdel == INSERT)
            count = end;
    }
    if (screen.flags & Screen_WrapNext) {
        screen.flags &= ~Screen_WrapNext;
    }
    scroll_text(screen.row, screen.bscroll, insdel * count, 0);

/* fill the inserted or new lines with rstyle. TODO: correct for delete? */
    if (insdel == DELETE) {
        end = screen.bscroll + TermWin.saveLines;
    } else if (insdel == INSERT) {
        end = screen.row + count - 1 + TermWin.saveLines;
    }
    for (; count--; end--) {
        blank_screen_mem(screen.text, screen.rend, end, rstyle);
    }
}

/*
 * Insert/Delete <count> characters from the current position
 */
void
scr_insdel_chars(int count, int insdel)
{
    int col, row;

    ZERO_SCROLLBACK;
    RESET_CHSTAT;

    if (count <= 0)
        return;

    CHECK_SELECTION;
    UPPER_BOUND(count, (TERM_WINDOW_GET_REPORTED_COLS() - screen.col));

    row = screen.row + TermWin.saveLines;
    screen.flags &= ~Screen_WrapNext;

    switch (insdel) {
        case INSERT:
            for (col = TERM_WINDOW_GET_REPORTED_COLS() - 1; (col - count) >= screen.col; col--) {
                screen.text[row][col] = screen.text[row][col - count];
                screen.rend[row][col] = screen.rend[row][col - count];
            }
            screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()] += count;
            UPPER_BOUND(screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()], TERM_WINDOW_GET_REPORTED_COLS());
            /* FALLTHROUGH */
        case ERASE:
            blank_line(&(screen.text[row][screen.col]), &(screen.rend[row][screen.col]), count, rstyle);
            break;
        case DELETE:
            for (col = screen.col; (col + count) < TERM_WINDOW_GET_REPORTED_COLS(); col++) {
                screen.text[row][col] = screen.text[row][col + count];
                screen.rend[row][col] = screen.rend[row][col + count];
            }
            blank_line(&(screen.text[row][TERM_WINDOW_GET_REPORTED_COLS() - count]),
                       &(screen.rend[row][TERM_WINDOW_GET_REPORTED_COLS() - count]), count, rstyle);
            screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()] -= count;
            if (((signed char) screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()]) < 0)
                screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()] = 0;
            break;
    }
#ifdef MULTI_CHARSET
    if ((screen.rend[row][0] & RS_multiMask) == RS_multi2) {
        screen.rend[row][0] &= ~RS_multiMask;
        screen.text[row][0] = ' ';
    }
    if ((screen.rend[row][TERM_WINDOW_GET_REPORTED_COLS() - 1] & RS_multiMask) == RS_multi1) {
        screen.rend[row][TERM_WINDOW_GET_REPORTED_COLS() - 1] &= ~RS_multiMask;
        screen.text[row][TERM_WINDOW_GET_REPORTED_COLS() - 1] = ' ';
    }
#endif
}

/*
 * Set the scrolling region
 * XTERM_SEQ: Set region <top> - <bot> inclusive: ESC [ <top> ; <bot> r
 */
void
scr_scroll_region(int top, int bot)
{
    LOWER_BOUND(top, 0);
    UPPER_BOUND(bot, TERM_WINDOW_GET_REPORTED_ROWS() - 1);
    if (top > bot)
        return;
    screen.tscroll = top;
    screen.bscroll = bot;
    scr_gotorc(0, 0, 0);
}

/*
 * Make the cursor visible/invisible
 * XTERM_SEQ: Make cursor visible  : ESC [ ? 25 h
 * XTERM_SEQ: Make cursor invisible: ESC [ ? 25 l
 */
void
scr_cursor_visible(int mode)
{
    if (mode)
        screen.flags |= Screen_VisibleCursor;
    else
        screen.flags &= ~Screen_VisibleCursor;
}

/*
 * Set/unset automatic wrapping
 * XTERM_SEQ: Set Wraparound  : ESC [ ? 7 h
 * XTERM_SEQ: Unset Wraparound: ESC [ ? 7 l
 */
void
scr_autowrap(int mode)
{
    if (mode)
        screen.flags |= Screen_Autowrap;
    else
        screen.flags &= ~Screen_Autowrap;
}

/*
 * Set/unset margin origin mode
 * Absolute mode: line numbers are counted relative to top margin of screen
 *      and the cursor can be moved outside the scrolling region.
 * Relative mode: line numbers are relative to top margin of scrolling region
 *      and the cursor cannot be moved outside.
 * XTERM_SEQ: Set Absolute: ESC [ ? 6 h
 * XTERM_SEQ: Set Relative: ESC [ ? 6 l
 */
void
scr_relative_origin(int mode)
{
    if (mode)
        screen.flags |= Screen_Relative;
    else
        screen.flags &= ~Screen_Relative;
    scr_gotorc(0, 0, 0);
}

/*
 * Set insert/replace mode
 * XTERM_SEQ: Set Insert mode : ESC [ ? 4 h
 * XTERM_SEQ: Set Replace mode: ESC [ ? 4 l
 */
void
scr_insert_mode(int mode)
{
    if (mode)
        screen.flags |= Screen_Insert;
    else
        screen.flags &= ~Screen_Insert;
}

/*
 * Set/Unset tabs
 * XTERM_SEQ: Set tab at current column  : ESC H
 * XTERM_SEQ: Clear tab at current column: ESC [ 0 g
 * XTERM_SEQ: Clear all tabs             : ESC [ 3 g
 */
void
scr_set_tab(int mode)
{
    if (mode < 0)
        MEMSET(tabs, 0, (unsigned int) TERM_WINDOW_GET_REPORTED_COLS());

    else if (screen.col < TERM_WINDOW_GET_REPORTED_COLS())
        tabs[screen.col] = (mode ? 1 : 0);
}

/*
 * Set reverse/normal video
 * XTERM_SEQ: Reverse video: ESC [ ? 5 h
 * XTERM_SEQ: Normal video : ESC [ ? 5 l
 */
void
scr_rvideo_mode(int mode)
{
    int i, j, maxlines;

    if (rvideo != mode) {
        rvideo = mode;
        rstyle ^= RS_RVid;

        maxlines = TermWin.saveLines + TERM_WINDOW_GET_REPORTED_ROWS();
        for (i = TermWin.saveLines; i < maxlines; i++)
            for (j = 0; j < TERM_WINDOW_GET_REPORTED_COLS(); j++)
                screen.rend[i][j] ^= RS_RVid;
        scr_refresh(SLOW_REFRESH);
    }
}

/*
 * Report current cursor position
 * XTERM_SEQ: Report position: ESC [ 6 n
 */
void
scr_report_position(void)
{
    tt_printf((unsigned char *) "\033[%d;%dR", screen.row + 1, screen.col + 1);
}

/* Set font style */
void
set_font_style(void)
{
    rstyle &= ~RS_fontMask;
    switch (charsets[screen.charset]) {
        case '0':              /* DEC Special Character & Line Drawing Set */
            rstyle |= RS_acsFont;
            break;
        case 'A':              /* United Kingdom (UK) */
            rstyle |= RS_ukFont;
            break;
        case 'B':              /* United States (USASCII) */
            break;
        case '<':              /* Multinational character set */
            break;
        case '5':              /* Finnish character set */
            break;
        case 'C':              /* Finnish character set */
            break;
        case 'K':              /* German character set */
            break;
    }
}

/*
 * Choose a font
 * XTERM_SEQ: Invoke G0 character set: CTRL-O
 * XTERM_SEQ: Invoke G1 character set: CTRL-N
 * XTERM_SEQ: Invoke G2 character set: ESC N
 * XTERM_SEQ: Invoke G3 character set: ESC O
 */
void
scr_charset_choose(int set)
{
    screen.charset = set;
    set_font_style();
}

/*
 * Set a font
 * XTERM_SEQ: Set G0 character set: ESC ( <C>
 * XTERM_SEQ: Set G1 character set: ESC ) <C>
 * XTERM_SEQ: Set G2 character set: ESC * <C>
 * XTERM_SEQ: Set G3 character set: ESC + <C>
 * See set_font_style for possible values for <C>
 */
void
scr_charset_set(int set, unsigned int ch)
{
#ifdef MULTI_CHARSET
    multi_byte = (set < 0);
    set = abs(set);
#endif
    charsets[set] = (unsigned char) ch;
    set_font_style();
}

#ifdef MULTI_CHARSET

static void latin1(unsigned char *str, int len);
static void eucj2jis(unsigned char *str, int len);
static void sjis2jis(unsigned char *str, int len);
static void big5dummy(unsigned char *str, int len);

static void (*multichar_decode) (unsigned char *str, int len) = latin1;

static void
latin1(unsigned char *str, int len)
{
    return;
    str = NULL;
    len = 0;
}

static void
eucj2jis(unsigned char *str, int len)
{
    register int i;

    for (i = 0; i < len; i++)
        str[i] &= 0x7F;
}

static void
sjis2jis(unsigned char *str, int len)
{
    register int i;
    unsigned char *high, *low;

    for (i = 0; i < len; i += 2, str += 2) {
        high = str;
        low = str + 1;
        (*high) -= (*high > 0x9F ? 0xB1 : 0x71);
        *high = (*high) * 2 + 1;
        if (*low > 0x9E) {
            *low -= 0x7E;
            (*high)++;
        } else {
            if (*low > 0x7E)
                (*low)--;
            *low -= 0x1F;
        }
    }
}

static void
big5dummy(unsigned char *str, int len)
{
    str = NULL;
    len = 0;
}
#endif

void
set_multichar_encoding(const char *str)
{
#ifdef MULTI_CHARSET
    if (str && *str) {
        if (!strcasecmp(str, "utf8") || !strcasecmp(str, "ucs2")) {
            encoding_method = UCS2;
            multichar_decode = latin1;
        } else if (!strcasecmp(str, "sjis")) {
            encoding_method = SJIS;
            multichar_decode = sjis2jis;
        } else if (!strcasecmp(str, "eucj") || !strcasecmp(str, "euckr") || !strcasecmp(str, "gb")) {
            encoding_method = EUCJ;
            multichar_decode = eucj2jis;
        } else if (!strcasecmp(str, "big5")) {
            encoding_method = BIG5;
            multichar_decode = big5dummy;
        } else {
            encoding_method = LATIN1;
            multichar_decode = latin1;
        }
    }
#else
    return;
    str = NULL;
#endif /* MULTI_CHARSET */
}

/* Refresh an area */
void
scr_expose(int x, int y, int width, int height)
{
    int i;
    register short nc, nr;
    row_col_t rect_beg, rect_end;

    REQUIRE(drawn_text != NULL);

    nc = TERM_WINDOW_GET_REPORTED_COLS() - 1;
    nr = TERM_WINDOW_GET_ROWS() - 1;

    rect_beg.col = Pixel2Col(x);
    BOUND(rect_beg.col, 0, nc);
    rect_beg.row = Pixel2Row(y);
    BOUND(rect_beg.row, 0, nr);
    rect_end.col = Pixel2Width(x + width + TermWin.fwidth - 1);
    BOUND(rect_end.col, 0, nc);
    rect_end.row = Pixel2Row(y + height + TermWin.fheight - 1);
    BOUND(rect_end.row, 0, nr);

    D_SCREEN(("scr_expose(x:%d, y:%d, w:%d, h:%d) area (c:%d,r:%d)-(c:%d,r:%d)\n", x, y, width, height, rect_beg.col, rect_beg.row,
              rect_end.col, rect_end.row));

    for (i = rect_beg.row; i <= rect_end.row; i++) {
        MEMSET(&(drawn_text[i][rect_beg.col]), 0, rect_end.col - rect_beg.col + 1);
    }
}

/* Move the display so that the line represented by scrollbar value Y is at
   the top of the screen */
int
scr_move_to(int y, int len)
{
    int start;

    start = TermWin.view_start;
    TermWin.view_start = ((len - y) * (TERM_WINDOW_GET_REPORTED_ROWS() - 1 + TermWin.nscrolled)
                          / (len)) - (TERM_WINDOW_GET_REPORTED_ROWS() - 1);
    D_SCREEN(("scr_move_to(%d, %d) view_start:%d\n", y, len, TermWin.view_start));

    BOUND(TermWin.view_start, 0, TermWin.nscrolled);

    return (TermWin.view_start - start);
}

/* Scroll the visible region up/down by <nlines> lines */
int
scr_page(int direction, int nlines)
{
    int start;

    D_SCREEN(("scr_page(%s, %d) view_start:%d\n", ((direction == UP) ? "UP" : "DN"), nlines, TermWin.view_start));

    start = TermWin.view_start;
    BOUND(nlines, 1, TermWin.nscrolled);
    TermWin.view_start += ((direction == UP) ? nlines : (-nlines));
    BOUND(TermWin.view_start, 0, TermWin.nscrolled);
    return (TermWin.view_start - start);
}

void
scr_bell(void)
{
    XWMHints *wm_hints;

    if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_URG_ALERT)) {
        wm_hints = XGetWMHints(Xdisplay, TermWin.parent);
        wm_hints->flags |= XUrgencyHint;
        XSetWMHints(Xdisplay, TermWin.parent, wm_hints);
        XFree(wm_hints);
    }
#ifndef NO_MAPALERT
#ifdef MAPALERT_OPTION
    if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_MAP_ALERT))
#endif
        XMapWindow(Xdisplay, TermWin.parent);
#endif
    if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_VISUAL_BELL)) {
        scr_rvideo_mode(!rvideo);
        scr_rvideo_mode(!rvideo);
    } else if (!SPIF_PTR_ISNULL(rs_beep_command) && (*rs_beep_command)) {
        system_no_wait((char *) rs_beep_command);
    } else {
        XBell(Xdisplay, 0);
    }
}

void
scr_printscreen(int fullhist)
{
#ifdef PRINTPIPE
    int i, r, nrows, row_offset;
    text_t *t;
    FILE *fd;

    if (!(fd = popen_printer()))
        return;
    nrows = TERM_WINDOW_GET_REPORTED_ROWS();
    if (fullhist) {
        /* Print the entire scrollback buffer.  Always start from the top and go all the way to the bottom. */
        nrows += TermWin.nscrolled;
        row_offset = TermWin.saveLines - TermWin.nscrolled;
    } else {
        /* Just print what's on the screen. */
        row_offset = TermWin.saveLines - TermWin.view_start;
    }

    for (r = 0; r < nrows; r++) {
        t = screen.text[r + row_offset];
        for (i = TERM_WINDOW_GET_REPORTED_COLS() - 1; i >= 0; i--)
            if (!isspace(t[i]))
                break;
        fprintf(fd, "%.*s\n", (i + 1), t);
    }
    pclose_printer(fd);
#endif
}

#ifdef MULTI_CHARSET
int
scr_multi1(void)
{
    rend_t rend;

    rend = screen.rend[screen.row + TermWin.saveLines][screen.col];
    return ((rend & RS_multiMask) == RS_multi1);
}

int
scr_multi2(void)
{
    rend_t rend;

    if (screen.col == 0)
        return 0;
    rend = screen.rend[screen.row + TermWin.saveLines][screen.col - 1];
    return ((rend & RS_multiMask) == RS_multi2);
}
#endif /* MULTI_CHARSET */

/*
 * Refresh the screen
 * drawn_text/drawn_rend contain the screen information before the update.
 * screen.text/screen.rend contain what the screen will change to.
 */

void
scr_refresh(int type)
{
    int i,                      /* tmp                                       */
     scrrow,                    /* screen row offset                         */
     row_offset,                /* basic offset in screen structure          */
     boldlast = 0,              /* last character in some row was bold       */
        len, wlen,              /* text length screen/buffer                 */
        fprop,                  /* proportional font used                    */
        is_cursor,              /* cursor this position                      */
        rvid,                   /* reverse video this position               */
        fore, back,             /* desired foreground/background             */
        wbyte,                  /* we're in multibyte                        */
        xpixel,                 /* x offset for start of drawing (font)      */
        ypixel;                 /* y offset for start of drawing (font)      */
    register int col, row,      /* column/row we're processing               */
     rend;                      /* rendition                                 */
    static int focus = -1;      /* screen in focus?                          */
    long gcmask;                /* Graphics Context mask                     */
    unsigned long ltmp;
    rend_t rt1, rt2,            /* tmp rend values                           */
     lastrend;                  /* rend type of last char in drawing set     */
    text_t lasttext;            /* last char being replaced in drawing set   */
    rend_t *drp, *srp;          /* drawn-rend-pointer, screen-rend-pointer   */
    text_t *dtp, *stp;          /* drawn-text-pointer, screen-text-pointer   */
    XGCValues gcvalue;          /* Graphics Context values                   */
    char buf[MAX_COLS + 1];
    register char *buffer = buf;
    Pixmap pmap = images[image_bg].current->pmap->pixmap;
    int (*draw_string) (), (*draw_image_string) ();
    register int low_x = 99999, low_y = 99999, high_x = 0, high_y = 0;
    Drawable draw_buffer;

#ifndef NO_BOLDFONT
    int bfont = 0;              /* we've changed font to bold font           */
#endif
#ifdef OPTIMIZE_HACKS
    register int nrows = TERM_WINDOW_GET_ROWS();
    register int ncols = TERM_WINDOW_GET_COLS();
#endif
    int ascent, descent;

    PROF_INIT(scr_refresh);

    switch (type) {
        case NO_REFRESH:
            D_SCREEN(("scr_refresh(NO_REFRESH) called.\n"));
            break;
        case SLOW_REFRESH:
            D_SCREEN(("scr_refresh(SLOW_REFRESH) called.\n"));
            break;
        case FAST_REFRESH:
            D_SCREEN(("scr_refresh(FAST_REFRESH) called.\n"));
            break;
    }
    if (type == NO_REFRESH)
        return;

    if (buffer_pixmap) {
        draw_buffer = buffer_pixmap;
    } else {
        draw_buffer = TermWin.vt;
    }

    row_offset = TermWin.saveLines - TermWin.view_start;
    fprop = TermWin.fprop;

    gcvalue.foreground = PixColors[fgColor];
    gcvalue.background = PixColors[bgColor];
    wbyte = 0;

    XSetFont(Xdisplay, TermWin.gc, TermWin.font->fid);

#if FIXME_BLOCK
    draw_string = XmbDrawString;
    draw_image_string = XmbDrawImageString;
#else
    draw_string = XDrawString;
    draw_image_string = XDrawImageString;
#endif

    BOUND(screen.row, 0, TERM_WINDOW_GET_REPORTED_ROWS() - 1);
    BOUND(screen.col, 0, TERM_WINDOW_GET_REPORTED_COLS() - 1);

    row = screen.row + TermWin.saveLines;
    col = screen.col;
    if (screen.flags & Screen_VisibleCursor) {
        screen.rend[row][col] |= RS_Cursor;
#ifdef MULTI_CHARSET
        srp = &screen.rend[row][col];
        if ((col < ncols - 1) && ((srp[0] & RS_multiMask) == RS_multi1)
            && ((srp[1] & RS_multiMask) == RS_multi2)) {
            screen.rend[row][col + 1] |= RS_Cursor;
        } else if ((col > 0) && ((srp[0] & RS_multiMask) == RS_multi2)
                   && ((srp[-1] & RS_multiMask) == RS_multi1)) {
            screen.rend[row][col - 1] |= RS_Cursor;
        }
#endif
        if (focus != TermWin.focus) {
            focus = TermWin.focus;
            if ((i = screen.row - TermWin.view_start) >= 0) {
                drawn_rend[i][col] = RS_attrMask;
#ifdef MULTI_CHARSET
                if ((col < ncols - 1) && ((srp[1] & RS_multiMask) == RS_multi2)) {
                    drawn_rend[i][col + 1] = RS_attrMask;
                } else if ((col > 0) && ((srp[-1] & RS_multiMask) == RS_multi1)) {
                    drawn_rend[i][col - 1] = RS_attrMask;
                }
#endif
            }
        }
    }

    for (row = 0; row < nrows; row++) {
        scrrow = row + row_offset;
        stp = screen.text[scrrow];
        srp = screen.rend[scrrow];
        dtp = drawn_text[row];
        drp = drawn_rend[row];

        for (col = 0; col < ncols; col++) {
            if (!refresh_all) {
                /* compare new text with old - if exactly the same then continue */
                rt1 = srp[col];
                rt2 = drp[col];
                if ((stp[col] == dtp[col])      /* must match characters to skip */
                    &&((rt1 == rt2)     /* either rendition the same or  */
                       ||((stp[col] == ' ')     /* space w/ no bg change */
                          &&(GET_BGATTR(rt1) == GET_BGATTR(rt2))))) {
#ifdef MULTI_CHARSET
                    /* if first byte is multibyte then compare second bytes */
                    if ((rt1 & RS_multiMask) != RS_multi1)
                        continue;
                    else if (stp[col + 1] == dtp[col + 1]) {
                        /* assume no corrupt characters on the screen */
                        col++;
                        continue;
                    }
#else
                    continue;
#endif
                }
            }
            lasttext = dtp[col];
            lastrend = drp[col];
            /* redraw one or more characters */
            dtp[col] = stp[col];
            rend = drp[col] = srp[col];

            len = 0;
            buffer[len++] = stp[col];
            xpixel = Col2Pixel(col);
            wlen = 1;

/*
 * Find out the longest string we can write out at once
 */
            if (fprop == 0) {   /* Fixed width font */
#ifdef MULTI_CHARSET
                if (((rend & RS_multiMask) == RS_multi1) && (col < ncols - 1)
                    && ((srp[col + 1]) & RS_multiMask) == RS_multi2) {
                    if (!wbyte) {
                        wbyte = 1;
                        XSetFont(Xdisplay, TermWin.gc, TermWin.mfont->fid);
# if FIXME_BLOCK
                        draw_string = XmbDrawString;
                        draw_image_string = XmbDrawImageString;
# else
                        draw_string = XDrawString16;
                        draw_image_string = XDrawImageString16;
# endif
                    }
                    /* double stepping - we're in Multibyte mode */
                    for (; ++col < ncols;) {
                        /* XXX: could check sanity on 2nd byte */
                        dtp[col] = stp[col];
                        drp[col] = srp[col];
                        buffer[len++] = stp[col];
                        col++;
                        if ((col == ncols) || (srp[col] != (unsigned int) rend))
                            break;
                        if ((stp[col] == dtp[col])
                            && (srp[col] == drp[col])
                            && (stp[col + 1] == dtp[col + 1]))
                            break;
                        if (len == MAX_COLS)
                            break;
                        dtp[col] = stp[col];
                        drp[col] = srp[col];
                        buffer[len++] = stp[col];
                    }
                    col--;
                    if (buffer[0] & 0x80)
                        multichar_decode(buffer, len);
                    wlen = len / 2;
                } else {
                    if ((rend & RS_multiMask) == RS_multi1) {
                        /* XXX : maybe do the same thing for RS_multi2 */
                        /* corrupt character - you're outta there */
                        rend &= ~RS_multiMask;
                        drp[col] = rend;        /* TODO check: may also want */
                        dtp[col] = ' '; /* to poke into stp/srp      */
                        buffer[0] = ' ';
                    }
                    if (wbyte) {
                        wbyte = 0;
                        XSetFont(Xdisplay, TermWin.gc, TermWin.font->fid);
# if FIXME_BLOCK
                        draw_string = XmbDrawString;
                        draw_image_string = XmbDrawImageString;
# else
                        draw_string = XDrawString;
                        draw_image_string = XDrawImageString;
# endif
                    }
#endif
                    /* single stepping - `normal' mode */
                    for (; ++col < ncols - 1;) {
                        if ((unsigned int) rend != srp[col])
                            break;
                        if ((stp[col] == dtp[col]) && (srp[col] == drp[col]))
                            break;
                        if (len == MAX_COLS)
                            break;
                        lasttext = dtp[col];
                        lastrend = drp[col];
                        dtp[col] = stp[col];
                        drp[col] = srp[col];
                        buffer[len++] = stp[col];
                    }
                    col--;
                    wlen = len;
#ifdef MULTI_CHARSET
                }
#endif
            }
            buffer[len] = '\0';

            /* Determine the attributes for the string */
            fore = GET_FGCOLOR(rend);
            back = GET_BGCOLOR(rend);
            rend = GET_ATTR(rend);
            gcmask = 0;
            rvid = (rend & RS_RVid) ? 1 : 0;
            if (rend & RS_Select)
                rvid = !rvid;
            if (rend & RS_Cursor) {
                if (focus) {
                    is_cursor = 2;      /* normal cursor */
                    rvid = !rvid;
                } else {
                    is_cursor = 1;      /* outline cursor */
                    rend &= ~RS_Cursor;
                }
                srp[col] &= ~RS_Cursor;
            } else
                is_cursor = 0;
            switch (rend & RS_fontMask) {
                case RS_acsFont:
                    for (i = 0; i < len; i++)
                        if (buffer[i] == 0x5f)
                            buffer[i] = 0x7f;
                        else if (buffer[i] > 0x5f && buffer[i] < 0x7f)
                            buffer[i] -= 0x5f;
                    break;
                case RS_ukFont:
                    for (i = 0; i < len; i++)
                        if (buffer[i] == '#')
                            buffer[i] = 0x1e;
                    break;
            }
            if (rvid)
                SWAP_IT(fore, back, i);
            if (back != bgColor) {
                gcvalue.background = PixColors[back];
                gcmask |= GCBackground;
            }
            if (fore != fgColor) {
                gcvalue.foreground = PixColors[fore];
                gcmask |= GCForeground;
            }
#ifndef NO_BOLDUNDERLINE
            else if (rend & RS_Bold) {
                if (PixColors[fore] != PixColors[colorBD]
                    && PixColors[back] != PixColors[colorBD]) {
                    gcvalue.foreground = PixColors[colorBD];
                    gcmask |= GCForeground;
                }
            } else if (rend & RS_Uline) {
                if (PixColors[fore] != PixColors[colorUL]
                    && PixColors[back] != PixColors[colorUL]) {
                    gcvalue.foreground = PixColors[colorUL];
                    gcmask |= GCForeground;
                }
            }
#endif
#ifndef NO_CURSORCOLOR
            if (rend & RS_Cursor) {
                if (PixColors[cursorColor] != PixColors[bgColor]) {
                    gcvalue.background = PixColors[cursorColor];
                    back = cursorColor;
                    gcmask |= GCBackground;
                }
                if (PixColors[cursorColor2] != PixColors[fgColor]) {
                    gcvalue.foreground = PixColors[cursorColor2];
                    gcmask |= GCForeground;
                }
            }
#endif
            if (gcmask) {
                XChangeGC(Xdisplay, TermWin.gc, gcmask, &gcvalue);
            }
#ifndef NO_BOLDFONT
            if (!wbyte && MONO_BOLD(rend) && TermWin.boldFont) {
                XSetFont(Xdisplay, TermWin.gc, TermWin.boldFont->fid);
                bfont = 1;
            } else if (bfont) {
                bfont = 0;
                XSetFont(Xdisplay, TermWin.gc, TermWin.font->fid);
            }
#endif

#ifdef MULTI_CHARSET
            ascent = MAX((encoding_method == LATIN1 ? 0 : TermWin.mfont->ascent), TermWin.font->ascent);
            descent = MAX((encoding_method == LATIN1 ? 0 : TermWin.mfont->descent), TermWin.font->descent);
#else
            ascent = TermWin.font->ascent;
            descent = TermWin.font->descent;
#endif
            ypixel = ascent + Row2Pixel(row);


            /* The actual drawing of the string is done here. */
            if (fprop) {
                if (back != bgColor) {
                    SWAP_IT(gcvalue.foreground, gcvalue.background, ltmp);
                    gcmask |= (GCForeground | GCBackground);
                    XChangeGC(Xdisplay, TermWin.gc, gcmask, &gcvalue);
                    XFillRectangle(Xdisplay, draw_buffer, TermWin.gc, xpixel, ypixel - ascent, Width2Pixel(1), Height2Pixel(1));
                    SWAP_IT(gcvalue.foreground, gcvalue.background, ltmp);
                    XChangeGC(Xdisplay, TermWin.gc, gcmask, &gcvalue);
                } else {
                    CLEAR_CHARS(xpixel, ypixel - ascent, 1);
                }
                if (TermWin.font->per_char) {
                    int fw, cw;

                    fw = TermWin.fwidth;
                    cw = TermWin.font->per_char[((int) (*buffer))].width;
                    if (cw > 0 && cw < TermWin.font->max_bounds.width) {
                        if (fw > cw) {
                            xpixel += ((fw - cw) >> 1);
                        } else {
                            xpixel -= ((cw - fw) >> 1);
                            if (col < ncols - 1) {
                                dtp[col + 1] = 0;
                            }
                        }
                    }
                }
                DRAW_STRING(draw_string, xpixel, ypixel, buffer, 1);
                UPDATE_BOX(xpixel, ypixel - ascent, xpixel + Width2Pixel(1), ypixel + Height2Pixel(1));
                if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_OVERSTRIKE_BOLD) && MONO_BOLD(rend)) {
                    DRAW_STRING(draw_string, xpixel + 1, ypixel, buffer, 1);
                    UPDATE_BOX(xpixel + 1, ypixel - ascent, xpixel + 1 + Width2Pixel(1), ypixel + Height2Pixel(1));
                }
            } else {
#ifdef PIXMAP_SUPPORT
                if (background_is_pixmap() && (back == bgColor)) {
                    if (fshadow.do_shadow) {
                        Pixel tmp;
                        int xx, yy, ww, hh;

                        tmp = gcvalue.foreground;
                        xx = xpixel;
                        yy = ypixel - ascent;
                        ww = Width2Pixel(wlen);
                        hh = Height2Pixel(1);
                        CLEAR_CHARS(xpixel, ypixel - ascent, len);
                        if (fshadow.shadow[SHADOW_TOP_LEFT] || fshadow.shadow[SHADOW_TOP] || fshadow.shadow[SHADOW_TOP_RIGHT]) {
                            yy--;
                            hh++;
                        }
                        if (fshadow.shadow[SHADOW_BOTTOM_LEFT] || fshadow.shadow[SHADOW_BOTTOM] || fshadow.shadow[SHADOW_BOTTOM_RIGHT]) {
                            hh++;
                            if (row < nrows - 1) {
                                int ii;

                                for (ii = col - len + 1; ii <= col; ii++) {
                                    drawn_text[row + 1][ii] = 0;
                                }
                            }
                        }
                        if (fshadow.shadow[SHADOW_TOP_LEFT]) {
                            XSetForeground(Xdisplay, TermWin.gc, fshadow.color[SHADOW_TOP_LEFT]);
                            DRAW_STRING(draw_string, xpixel - 1, ypixel - 1, buffer, wlen);
                            if (col) {
                                dtp[col - 1] = 0;
                            }
                        }
                        if (fshadow.shadow[SHADOW_TOP]) {
                            XSetForeground(Xdisplay, TermWin.gc, fshadow.color[SHADOW_TOP]);
                            DRAW_STRING(draw_string, xpixel, ypixel - 1, buffer, wlen);
                            if (col) {
                                dtp[col] = 0;
                            }
                        }
                        if (fshadow.shadow[SHADOW_TOP_RIGHT]) {
                            XSetForeground(Xdisplay, TermWin.gc, fshadow.color[SHADOW_TOP_RIGHT]);
                            DRAW_STRING(draw_string, xpixel + 1, ypixel - 1, buffer, wlen);
                            if (col < ncols - 1) {
                                dtp[col + 1] = 0;
                            }
                        }
                        if (fshadow.shadow[SHADOW_LEFT]) {
                            XSetForeground(Xdisplay, TermWin.gc, fshadow.color[SHADOW_LEFT]);
                            DRAW_STRING(draw_string, xpixel - 1, ypixel, buffer, wlen);
                            if (col) {
                                dtp[col - 1] = 0;
                            }
                        }
                        if (fshadow.shadow[SHADOW_RIGHT]) {
                            XSetForeground(Xdisplay, TermWin.gc, fshadow.color[SHADOW_RIGHT]);
                            DRAW_STRING(draw_string, xpixel + 1, ypixel, buffer, wlen);
                            if (col < ncols - 1) {
                                dtp[col + 1] = 0;
                            }
                        }
                        if (fshadow.shadow[SHADOW_BOTTOM_LEFT]) {
                            XSetForeground(Xdisplay, TermWin.gc, fshadow.color[SHADOW_BOTTOM_LEFT]);
                            DRAW_STRING(draw_string, xpixel - 1, ypixel + 1, buffer, wlen);
                            if (col) {
                                dtp[col - 1] = 0;
                            }
                        }
                        if (fshadow.shadow[SHADOW_BOTTOM]) {
                            XSetForeground(Xdisplay, TermWin.gc, fshadow.color[SHADOW_BOTTOM]);
                            DRAW_STRING(draw_string, xpixel, ypixel + 1, buffer, wlen);
                            if (col) {
                                dtp[col] = 0;
                            }
                        }
                        if (fshadow.shadow[SHADOW_BOTTOM_RIGHT]) {
                            XSetForeground(Xdisplay, TermWin.gc, fshadow.color[SHADOW_BOTTOM_RIGHT]);
                            DRAW_STRING(draw_string, xpixel + 1, ypixel + 1, buffer, wlen);
                            if (col < ncols - 1) {
                                dtp[col + 1] = 0;
                            }
                        }
                        XSetForeground(Xdisplay, TermWin.gc, tmp);
                        DRAW_STRING(draw_string, xpixel, ypixel, buffer, wlen);
                        UPDATE_BOX(xx, yy, xx + ww, yy + hh);
                    } else {
                        CLEAR_CHARS(xpixel, ypixel - ascent, len);
                        DRAW_STRING(draw_string, xpixel, ypixel, buffer, wlen);
                        UPDATE_BOX(xpixel, ypixel - ascent, xpixel + Width2Pixel(wlen), ypixel + Height2Pixel(1));
                    }
                } else
#endif
                {
#ifdef FORCE_CLEAR_CHARS
                    CLEAR_CHARS(xpixel, ypixel - ascent, len);
#endif
                    DRAW_STRING(draw_image_string, xpixel, ypixel, buffer, wlen);
#ifdef MULTI_CHARSET
                    {
                        XFontStruct *font = wbyte ? TermWin.mfont : TermWin.font;

                        if (font->ascent < ascent || font->descent < descent) {
                            SWAP_IT(gcvalue.foreground, gcvalue.background, ltmp);
                            gcmask |= (GCForeground | GCBackground);
                            XChangeGC(Xdisplay, TermWin.gc, gcmask, &gcvalue);
                            if (font->ascent < ascent) {
                                XFillRectangle(Xdisplay, draw_buffer, TermWin.gc, xpixel, Row2Pixel(row), Width2Pixel(len),
                                               ascent - font->ascent);
                            }
                            if (font->descent < descent) {
                                XFillRectangle(Xdisplay, draw_buffer, TermWin.gc, xpixel, Row2Pixel(row) + ascent + font->descent,
                                               Width2Pixel(len), descent - font->descent);
                            }
                            SWAP_IT(gcvalue.foreground, gcvalue.background, ltmp);
                            XChangeGC(Xdisplay, TermWin.gc, gcmask, &gcvalue);
                        }
                    }
#endif
                    UPDATE_BOX(xpixel, ypixel - ascent, xpixel + Width2Pixel(wlen), ypixel + Height2Pixel(1));
                }
            }

            /* do the convoluted bold overstrike */
            if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_OVERSTRIKE_BOLD) && MONO_BOLD(rend)) {
                DRAW_STRING(draw_string, xpixel + 1, ypixel, buffer, wlen);
                UPDATE_BOX(xpixel + 1, ypixel - ascent, xpixel + 1 + Width2Pixel(wlen), ypixel + Height2Pixel(1));
            }

            if (rend & RS_Uline) {
                if (descent > 1) {
                    XDrawLine(Xdisplay, draw_buffer, TermWin.gc, xpixel, ypixel + 1, xpixel + Width2Pixel(wlen) - 1, ypixel + 1);
                    UPDATE_BOX(xpixel, ypixel + 1, xpixel + Width2Pixel(wlen) - 1, ypixel + 1);
                } else {
                    XDrawLine(Xdisplay, draw_buffer, TermWin.gc, xpixel, ypixel - 1, xpixel + Width2Pixel(wlen) - 1, ypixel - 1);
                    UPDATE_BOX(xpixel, ypixel - 1, xpixel + Width2Pixel(wlen) - 1, ypixel - 1);
                }
            }
            if (rend & RS_Overscore) {
                if (ascent > 1) {
                    XDrawLine(Xdisplay, draw_buffer, TermWin.gc, xpixel, ypixel - ascent, xpixel + Width2Pixel(wlen) - 1,
                              ypixel - ascent);
                    UPDATE_BOX(xpixel, ypixel + 1, xpixel + Width2Pixel(wlen) - 1, ypixel + 1);
                } else {
                    XDrawLine(Xdisplay, draw_buffer, TermWin.gc, xpixel, ypixel - 1, xpixel + Width2Pixel(wlen) - 1, ypixel - 1);
                    UPDATE_BOX(xpixel, ypixel - 1, xpixel + Width2Pixel(wlen) - 1, ypixel - 1);
                }
            }
            if (is_cursor == 1) {
#ifndef NO_CURSORCOLOR
                if (PixColors[cursorColor] != PixColors[bgColor]) {
                    XSetForeground(Xdisplay, TermWin.gc, PixColors[cursorColor]);
                }
#endif
                XDrawRectangle(Xdisplay, draw_buffer, TermWin.gc, xpixel, ypixel - ascent, Width2Pixel(1 + wbyte) - 1,
                               Height2Pixel(1) - 1);
                UPDATE_BOX(xpixel, ypixel - ascent, Width2Pixel(1 + wbyte) - 1, Height2Pixel(1) - 1);
                XSetForeground(Xdisplay, TermWin.gc, PixColors[fgColor]);
            }
            if (gcmask) {       /* restore normal colors */
                gcvalue.foreground = PixColors[fgColor];
                gcvalue.background = PixColors[bgColor];
                XChangeGC(Xdisplay, TermWin.gc, gcmask, &gcvalue);
            }
            if (MONO_BOLD(lastrend)) {
                if (col < ncols - 1) {
                    dtp[col + 1] = 0;
                } else {
                    boldlast = 1;
                }
            }
        }                       /* for (col = 0; col < TERM_WINDOW_GET_REPORTED_COLS(); col++) */
    }                           /* for (row = 0; row < TERM_WINDOW_GET_REPORTED_ROWS(); row++) */

    row = screen.row + TermWin.saveLines;
    col = screen.col;
    if (screen.flags & Screen_VisibleCursor) {
        screen.rend[row][col] &= ~RS_Cursor;
#ifdef MULTI_CHARSET
        /* very low overhead so don't check properly, just wipe it all out */
        if (screen.col < ncols - 1)
            screen.rend[row][col + 1] &= ~RS_Cursor;
        if (screen.col > 0)
            screen.rend[row][col - 1] &= ~RS_Cursor;
#endif
    }
    if (buffer_pixmap) {
        D_SCREEN(("Update box dimensions:  from (%d, %d) to (%d, %d).  Dimensions %dx%d\n", low_x, low_y, high_x, high_y,
                  high_x - low_x + 1, high_y - low_y + 1));
        XClearArea(Xdisplay, TermWin.vt, low_x, low_y, high_x - low_x + 1, high_y - low_y + 1, False);
        if (fshadow.shadow[SHADOW_TOP_LEFT] || fshadow.shadow[SHADOW_LEFT] || fshadow.shadow[SHADOW_BOTTOM_LEFT]) {
            XCopyArea(Xdisplay, pmap, buffer_pixmap, TermWin.gc, TermWin.internalBorder - 1, 0, 1, TermWin_TotalHeight() - 1,
                      TermWin.internalBorder - 1, 0);
            XClearArea(Xdisplay, TermWin.vt, TermWin.internalBorder - 1, 0, 1, TermWin_TotalHeight() - 1, False);
        }
        if (fshadow.shadow[SHADOW_TOP_RIGHT] || fshadow.shadow[SHADOW_RIGHT] || fshadow.shadow[SHADOW_BOTTOM_RIGHT] || boldlast) {
            XCopyArea(Xdisplay, pmap, buffer_pixmap, TermWin.gc, TermWin_TotalWidth() - 2, 0, 1, TermWin_TotalHeight() - 1,
                      TermWin_TotalWidth() - 2, 0);
            XClearArea(Xdisplay, TermWin.vt, TermWin_TotalWidth() - 2, 0, 1, TermWin_TotalHeight() - 1, False);
        }
        if (fshadow.shadow[SHADOW_TOP_LEFT] || fshadow.shadow[SHADOW_TOP] || fshadow.shadow[SHADOW_TOP_RIGHT]) {
            XCopyArea(Xdisplay, pmap, buffer_pixmap, TermWin.gc, 0, TermWin.internalBorder - 1, TermWin_TotalWidth() - 1, 1, 0,
                      TermWin.internalBorder - 1);
            XClearArea(Xdisplay, TermWin.vt, 0, TermWin.internalBorder - 1, TermWin_TotalWidth() - 1, 1, False);
        }
        if (fshadow.shadow[SHADOW_BOTTOM_LEFT] || fshadow.shadow[SHADOW_BOTTOM] || fshadow.shadow[SHADOW_BOTTOM_RIGHT]) {
            XCopyArea(Xdisplay, pmap, buffer_pixmap, TermWin.gc, 0, TermWin_TotalHeight() - TermWin.internalBorder,
                      TermWin_TotalWidth() - 1, 1, 0, TermWin_TotalHeight() - TermWin.internalBorder);
            XClearArea(Xdisplay, TermWin.vt, 0, TermWin_TotalHeight() - TermWin.internalBorder, TermWin_TotalWidth() - 1, 1, False);
        }
    } else {
        if (fshadow.shadow[SHADOW_TOP_LEFT] || fshadow.shadow[SHADOW_LEFT] || fshadow.shadow[SHADOW_BOTTOM_LEFT]) {
            XClearArea(Xdisplay, TermWin.vt, TermWin.internalBorder - 1, 0, 1, TermWin_TotalHeight() - 1, False);
        }
        if ((fshadow.shadow[SHADOW_TOP_RIGHT] || fshadow.shadow[SHADOW_RIGHT] || fshadow.shadow[SHADOW_BOTTOM_RIGHT] || boldlast) && TermWin.internalBorder) {
            XClearArea(Xdisplay, TermWin.vt, TermWin_TotalWidth() - 2, 0, 1, TermWin_TotalHeight() - 1, False);
        }
        if (fshadow.shadow[SHADOW_TOP_LEFT] || fshadow.shadow[SHADOW_TOP] || fshadow.shadow[SHADOW_TOP_RIGHT]) {
            XClearArea(Xdisplay, TermWin.vt, 0, TermWin.internalBorder - 1, TermWin_TotalWidth() - 1, 1, False);
        }
        if (fshadow.shadow[SHADOW_BOTTOM_LEFT] || fshadow.shadow[SHADOW_BOTTOM] || fshadow.shadow[SHADOW_BOTTOM_RIGHT]) {
            XClearArea(Xdisplay, TermWin.vt, 0, TermWin_TotalHeight() - TermWin.internalBorder, TermWin_TotalWidth() - 1, 1, False);
        }
    }
    if (type == SLOW_REFRESH) {
        XSync(Xdisplay, False);
    }
    refresh_all = 0;
    D_SCREEN(("Exiting.\n"));

    PROF_DONE(scr_refresh);
    PROF_TIME(scr_refresh);
}

int
scr_strmatch(unsigned long row, unsigned long col, const char *str)
{
    unsigned char c;
    const char *s;

    for (c = screen.text[row][col], s = str; s; s++) {
        if (c != *s) {
            return (0);
        }
    }
    return 1;
}

/* Find and highlight all occurances of "str" in the scrollback. */
void
scr_search_scrollback(char *str)
{
    unsigned char *c;
    char *s;
    static char *last_str = NULL;
    unsigned int *i;
    unsigned long row, lrow, col, rows, cols, len, k;

    if (!str) {
        if (!(str = last_str)) {
            return;
        }
    } else {
        last_str = STRDUP(str);
    }
    lrow = rows = TERM_WINDOW_GET_REPORTED_ROWS() + TermWin.saveLines;
    cols = TERM_WINDOW_GET_REPORTED_COLS();
    len = strlen(str);

    D_SCREEN(("%d, %d\n", rows, cols));
    for (row = 0; row < rows; row++) {
        if (screen.text[row]) {
            c = screen.text[row];
            for (s = strstr(c, str); s; s = strstr(s + 1, str)) {
                unsigned long j;

                col = (long) s - (long) c;
                for (i = screen.rend[row] + col, j = 0; j < len; i++, j++) {
                    if (*i & RS_RVid) {
                        *i &= ~RS_RVid;
                    } else {
                        *i |= RS_RVid;
                    }
                }
                if ((long) row <= TermWin.saveLines) {
                    lrow = row;
                }
            }
            for (s = screen.text[row] + cols - len + 1, k = len - 1; k; s++, k--) {
                unsigned long j;

                if ((row < rows - 1) && !strncasecmp(s, str, k) && screen.text[row + 1]
                    && !strncasecmp(screen.text[row + 1], str + k, len - k)) {
                    col = (long) s - (long) c;
                    for (i = &(screen.rend[row][cols - k]), j = 0; j < k; i++, j++) {
                        (*i & RS_RVid) ? (*i &= ~RS_RVid) : (*i |= RS_RVid);
                    }
                    for (i = screen.rend[row + 1], j = 0, k = len - k; j < k; i++, j++) {
                        (*i & RS_RVid) ? (*i &= ~RS_RVid) : (*i |= RS_RVid);
                    }
                    if ((long) row <= TermWin.saveLines) {
                        lrow = row;
                    }
                    break;
                }
            }
        }
    }
    if (last_str == str) {
        FREE(last_str);
    } else {
        if (lrow != rows) {
            TermWin.view_start = rows - lrow - TERM_WINDOW_GET_REPORTED_ROWS();
            BOUND(TermWin.view_start, 0, TermWin.nscrolled);
            D_SCREEN(("New view start is %d\n", TermWin.view_start));
        }
    }
    scr_refresh(refresh_type);
}

/* Dump the entire contents of the scrollback buffer to stderr in hex and ASCII */
void
scr_dump(void)
{
    unsigned char *c;
    unsigned int *i;
    unsigned long row, col, rows, cols;

    rows = TERM_WINDOW_GET_REPORTED_ROWS() + TermWin.saveLines;
    cols = TERM_WINDOW_GET_REPORTED_COLS();

    D_SCREEN(("%d, %d\n", rows, cols));
    for (row = 0; row < rows; row++) {
        fprintf(stderr, "%lu:  ", row);
        if (screen.text[row]) {
            for (col = 0, c = screen.text[row]; col < cols; c++, col++) {
                fprintf(stderr, "%02x ", *c);
            }
            fprintf(stderr, "\"");
            for (col = 0, c = screen.text[row]; col < cols; c++, col++) {
                fprintf(stderr, "%c", ((isprint(*c)) ? (*c) : '.'));
            }
            fprintf(stderr, "\"");
            for (col = 0, i = screen.rend[row]; col < cols; i++, col++) {
                fprintf(stderr, " %08x", *i);
            }
        } else {
            fprintf(stderr, "NULL");
        }
        fprintf(stderr, "\n");
        fflush(stderr);
    }
}

/* Dump the entire contents of the scrollback buffer to a file */
void
scr_dump_to_file(const char *fname)
{
    int outfd;
    char *buff, *src, *dest;
    unsigned long row, col, rows, cols;
    struct stat st;

    REQUIRE(fname != NULL);

    rows = TERM_WINDOW_GET_REPORTED_ROWS() + TermWin.saveLines;
    cols = TERM_WINDOW_GET_REPORTED_COLS();
    D_SCREEN(("Dumping to file \"%s\".  %d rows, %d cols\n", fname, rows, cols));

    /* Remove it if it's there.  If this fails, we don't
       care, because open() will do the right thing. */
    if ((stat(fname, &st) == 0) || (errno != ENOENT)) {
        D_SCREEN(("Refusing to use log file \"%s\" -- %s\n", fname, (errno ? strerror(errno) : "File exists")));
        return;
    }

    /* Only open if it's a new file, and open with permissions 0600 */
    outfd = open(fname, O_CREAT | O_EXCL | O_NDELAY | O_WRONLY, S_IRUSR | S_IWUSR);
    if (outfd < 0) {
        D_SCREEN(("Unable to open \"%s\" for writing -- %s\n", fname, strerror(errno)));
        return;
    }
    if (stat(fname, &st) || !S_ISREG(st.st_mode)) {
        D_SCREEN(("Race condition exploit attempt detected on \"%s\"!\n", fname));
        close(outfd);
        return;
    }
    buff = MALLOC(cols + 1);
    for (row = 0; row < rows; row++) {
        if (screen.text[row]) {
            for (src = screen.text[row], dest = buff, col = 0; col < cols; col++)
                *dest++ = *src++;
            *dest++ = '\n';
            *dest = 0;
            write(outfd, buff, dest - buff);
        }
    }
    close(outfd);
    FREE(buff);
}

/*
 * If (row,col) is within a selected region of text, remove the selection
 * -TermWin.nscrolled <= (selection row) <= TermWin.nrow - 1
 */
void
selection_check(void)
{
    int c1, c2, r1, r2;

    if (current_screen != selection.screen)
        return;

    if ((selection.mark.row < -TermWin.nscrolled)
        || (selection.mark.row >= TERM_WINDOW_GET_ROWS())
        || (selection.beg.row < -TermWin.nscrolled)
        || (selection.beg.row >= TERM_WINDOW_GET_ROWS())
        || (selection.end.row < -TermWin.nscrolled)
        || (selection.end.row >= TERM_WINDOW_GET_ROWS())) {
        selection_reset();
        return;
    }
    r1 = (screen.row - TermWin.view_start);

    c1 = ((r1 - selection.mark.row) * (r1 - selection.end.row));

/*
 * selection.mark.row > screen.row - TermWin.view_start
 * or
 * selection.end.row > screen.row - TermWin.view_start
 */
    if (c1 < 0)
        selection_reset();
    else if (c1 == 0) {
        if ((selection.mark.row < selection.end.row)
            || ((selection.mark.row == selection.end.row)
                && (selection.mark.col < selection.end.col))) {
            r1 = selection.mark.row;
            c1 = selection.mark.col;
            r2 = selection.end.row;
            c2 = selection.end.col;
        } else {
            r1 = selection.end.row;
            c1 = selection.end.col;
            r2 = selection.mark.row;
            c2 = selection.mark.col;
        }
        if ((screen.row == r1) && (screen.row == r2)) {
            if ((screen.col >= c1) && (screen.col <= c2))
                selection_reset();
        } else if (((screen.row == r1) && (screen.col >= c1))
                   || ((screen.row == r2) && (screen.col <= c2)))
            selection_reset();
    }
}

/* Write the selection out to the tty. */
void
selection_write(unsigned char *data, size_t len)
{
    size_t num;
    unsigned char *p, *cr = "\r";

    D_SELECT(("Writing %lu characters of selection data to tty.\n", len));
    D_SELECT(("\n%s\n\n", safe_print_string((char *) data, len)));
    for (p = data, num = 0; len--; p++) {
        /* Write out each line, replacing newlines with carriage returns. */
        if (*p != '\n') {
            num++;
        } else {
            tt_write(data, num);
            tt_write(cr, 1);
            data += num + 1;
            num = 0;
        }
    }
    /* If there's anything left, write it out too. */
    if (num) {
        tt_write(data, num);
    }
}

/* Fetch the selection from the specified property and write it to the tty. */
void
selection_fetch(Window win, unsigned prop, int delete)
{
    long nread;
    unsigned long bytes_after, nitems;
    unsigned char *data;
    Atom actual_type;
    int actual_fmt;

    D_SELECT(("Fetching selection in property %d from window 0x%08x\n", (int) prop, (int) win));
    if (prop == None) {
        return;
    }
    for (nread = 0, bytes_after = 1; bytes_after > 0;) {
        if ((XGetWindowProperty
             (Xdisplay, win, prop, (nread / 4), PROP_SIZE, delete, AnyPropertyType, &actual_type, &actual_fmt, &nitems,
              &bytes_after, &data) != Success)
            || (actual_type == None) || (!data)) {
            D_SELECT(("Unable to fetch the value of property %d from window 0x%08x\n", (int) prop, (int) win));
            if (data) {
                XFree(data);
            }
            return;
        }
        nread += nitems;
        D_SELECT(("Got selection info:  Actual type %d (format %d), %lu items at 0x%08x, %lu bytes left over.\n",
                  (int) actual_type, actual_fmt, nitems, data, bytes_after));

        if (nitems == 0) {
            D_SELECT(("Retrieval of incremental selection complete.\n"));
            TermWin.mask &= ~(PropertyChangeMask);
            XSelectInput(Xdisplay, TermWin.vt, TermWin.mask);
            return;
        }
        if (actual_type == XA_STRING) {
            /* We can handle strings directly. */
            selection_write(data, nitems);
        } else if (actual_type == props[PROP_SELECTION_INCR]) {
            D_SELECT(("Incremental selection transfer initiated.  Length is at least %u bytes.\n",
                      (unsigned) *((unsigned *) data)));
            TermWin.mask |= PropertyChangeMask;
            XSelectInput(Xdisplay, TermWin.vt, TermWin.mask);
        } else {
            int size, i;
            XTextProperty xtextp;
            char **cl = NULL;

            /* It's not a string, so convert it to one (or more). */
            D_SELECT(("Selection is not a string.  Converting.\n"));
            xtextp.value = data;
            xtextp.encoding = actual_type;
            xtextp.format = actual_fmt;
            xtextp.nitems = nitems;
            XmbTextPropertyToTextList(Xdisplay, &xtextp, &cl, &size);

            if (cl) {
                D_SELECT(("Got string list 0x%08x with %d strings.\n", cl, size));
                for (i = 0; i < size; i++) {
                    if (cl[i]) {
                        selection_write(cl[i], strlen(cl[i]));
                    }
                }
                XFreeStringList(cl);
            }
        }
        if (data) {
            XFree(data);
        }
    }
}

/* Copy a specific string of a given length to the buffer specified. */
void
selection_copy_string(Atom sel, char *str, size_t len)
{
    D_SELECT(("Copying %ul bytes from 0x%08x to selection %d\n", len, str, (int) sel));
    if (!str || len == 0) {
        return;
    }
    if (IS_SELECTION(sel)) {
        D_SELECT(("Changing ownership of selection %d to my window 0x%08x\n", (int) sel, (int) TermWin.vt));
        XSetSelectionOwner(Xdisplay, sel, TermWin.vt, CurrentTime);
        if (XGetSelectionOwner(Xdisplay, sel) != TermWin.vt) {
            libast_print_error("Can't take ownership of selection\n");
        }
    } else {
        D_SELECT(("Copying selection to cut buffer %d\n", (int) sel));
        XChangeProperty(Xdisplay, Xroot, sel, XA_STRING, 8, PropModeReplace, str, len);
    }
}

/* Copy the currently-selected text to the buffer specified. */
void
selection_copy(Atom sel)
{
    selection_copy_string(sel, selection.text, selection.len);
}

/* Paste the specified selection from the specified buffer. */
void
selection_paste(Atom sel)
{
    D_SELECT(("Attempting to paste selection %d.\n", (int) sel));
    if (selection.text) {
        /* If we have a selection of our own, paste it. */
        D_SELECT(("Pasting my current selection of length %lu\n", selection.len));
        selection_write(selection.text, selection.len);
    } else if (IS_SELECTION(sel)) {
        /* Request the current selection be converted to the appropriate
           form (usually XA_STRING) and save it for us in the VT_SELECTION
           property.  We'll then get a SelectionNotify. */
        D_SELECT(("Requesting current selection (%d) -> VT_SELECTION (%d)\n", sel, props[PROP_SELECTION_DEST]));
#if defined(MULTI_CHARSET)
        if (encoding_method != LATIN1) {
            XConvertSelection(Xdisplay, sel, props[PROP_COMPOUND_TEXT], props[PROP_SELECTION_DEST], TermWin.vt, CurrentTime);
        } else {
            XConvertSelection(Xdisplay, sel, XA_STRING, props[PROP_SELECTION_DEST], TermWin.vt, CurrentTime);
        }
#else
        XConvertSelection(Xdisplay, sel, XA_STRING, props[PROP_SELECTION_DEST], TermWin.vt, CurrentTime);
#endif
    } else {
        D_SELECT(("Pasting cut buffer %d.\n", (int) sel));
        selection_fetch(Xroot, sel, False);
    }
}

/* Clear the selected state of all selected text. */
void
selection_reset(void)
{
    int i, j, lrow, lcol;

    D_SELECT(("selection_reset()\n"));

    lrow = TERM_WINDOW_GET_REPORTED_ROWS() + TermWin.saveLines;
    lcol = TERM_WINDOW_GET_REPORTED_COLS();
    selection.op = SELECTION_CLEAR;

    i = (current_screen == PRIMARY) ? 0 : TermWin.saveLines;
    for (; i < lrow; i++) {
        if (screen.text[i]) {
            for (j = 0; j < lcol; j++) {
                screen.rend[i][j] &= ~RS_Select;
            }
        }
    }
}

/* Delete the current selection. */
void
selection_clear(void)
{
    D_SELECT(("selection_clear()\n"));

    if (selection.text) {
        FREE(selection.text);
    }
    selection.len = 0;
    selection_reset();
}

/* Set or clear between selected points (inclusive) */
void
selection_setclr(int set, int startr, int startc, int endr, int endc)
{
    int row, col, last_col;
    rend_t *rend;

    D_SELECT(("selection_setclr(%d) %s (%d,%d)-(%d,%d)\n", set, (set ? "set  " : "clear"), startc, startr, endc, endr));

    if ((startr < -TermWin.nscrolled) || (endr >= TERM_WINDOW_GET_REPORTED_ROWS())) {
        selection_reset();
        return;
    }
    last_col = TERM_WINDOW_GET_REPORTED_COLS() - 1;

    LOWER_BOUND(startc, 0);
    UPPER_BOUND(endc, last_col);
    BOUND(startr, -TermWin.nscrolled, TERM_WINDOW_GET_REPORTED_ROWS() - 1);
    BOUND(endr, -TermWin.nscrolled, TERM_WINDOW_GET_REPORTED_ROWS() - 1);

    startr += TermWin.saveLines;
    endr += TermWin.saveLines;

    col = startc;
    if (set) {
        for (row = startr; row < endr; row++) {
            rend = &(screen.rend[row][col]);
            for (; col <= last_col; col++, rend++)
                *rend |= RS_Select;
            col = 0;
        }
        rend = &(screen.rend[row][col]);
        for (; col <= endc; col++, rend++)
            *rend |= RS_Select;
    } else {
        for (row = startr; row < endr; row++) {
            rend = &(screen.rend[row][col]);
            for (; col <= last_col; col++, rend++)
                *rend &= ~RS_Select;
            col = 0;
        }
        rend = &(screen.rend[row][col]);
        for (; col <= endc; col++, rend++)
            *rend &= ~RS_Select;
    }
}

/*
 * Mark a selection at the specified x/y pixel location
 */
void
selection_start(int x, int y)
{
    D_SELECT(("selection_start(%d, %d)\n", x, y));
    selection_start_colrow(Pixel2Col(x), Pixel2Row(y));
}

/*
 * Mark a selection at the specified col/row
 */
void
selection_start_colrow(int col, int row)
{
    int end_col;

    D_SELECT(("selection_start_colrow(%d, %d)\n", col, row));

    if (selection.op) {
        /* clear the old selection */

        if (selection.beg.row < -TermWin.nscrolled)
            selection_reset();
        else
            selection_setclr(0, selection.beg.row, selection.beg.col, selection.end.row, selection.end.col);
    }
    selection.op = SELECTION_INIT;
    BOUND(row, 0, TERM_WINDOW_GET_REPORTED_ROWS() - 1);

    row -= TermWin.view_start;
    end_col = screen.text[row + TermWin.saveLines][TERM_WINDOW_GET_REPORTED_COLS()];
    if (end_col != WRAP_CHAR && col > end_col)
        col = TERM_WINDOW_GET_REPORTED_COLS();
    selection.mark.col = col;
    selection.mark.row = row;
}

/*
 * Copy a selection into the cut buffer
 * EXT: button 1 or 3 release
 */
void
selection_make(Time tm)
{
    int i, col, end_col, row, end_row;
    text_t *new_selection_text;
    char *str;
    text_t *t;

    D_SELECT(("selection.op=%d, selection.clicks=%d\n", selection.op, selection.clicks));
    switch (selection.op) {
        case SELECTION_CONT:
            break;
        case SELECTION_INIT:
            selection_reset();
            selection.end.row = selection.beg.row = selection.mark.row;
            selection.end.col = selection.beg.col = selection.mark.col;
            /* FALLTHROUGH */
        case SELECTION_BEGIN:
            selection.op = SELECTION_DONE;
            /* FALLTHROUGH */
        default:
            return;
    }
    selection.op = SELECTION_DONE;

    if (selection.clicks == 4)
        return;                 /* nothing selected, go away */

    if (selection.beg.row < -TermWin.nscrolled || selection.end.row >= TERM_WINDOW_GET_REPORTED_ROWS()) {
        selection_reset();
        return;
    }
    i = (selection.end.row - selection.beg.row + 1) * (TERM_WINDOW_GET_REPORTED_COLS() + 1) + 1;
    str = MALLOC(i * sizeof(char));
    new_selection_text = (unsigned char *) str;

    col = MAX(selection.beg.col, 0);
    row = selection.beg.row + TermWin.saveLines;
    end_row = selection.end.row + TermWin.saveLines;
/*
 * A: rows before end row
 */
    for (; row < end_row; row++) {
        t = &(screen.text[row][col]);
        if ((end_col = screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()]) == WRAP_CHAR)
            end_col = TERM_WINDOW_GET_REPORTED_COLS();
        for (; col < end_col; col++)
            *str++ = *t++;
        col = 0;
        if (screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()] != WRAP_CHAR) {
            if (!(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SELECT_TRAILING_SPACES))) {
                for (str--; *str == ' ' || *str == '\t'; str--);
                str++;
            }
            *str++ = '\n';
        }
    }
/*
 * B: end row
 */
    t = &(screen.text[row][col]);
    end_col = screen.text[row][TERM_WINDOW_GET_REPORTED_COLS()];
    if (end_col == WRAP_CHAR || selection.end.col <= end_col) {
        i = 0;
        end_col = selection.end.col + 1;
    } else
        i = 1;
    UPPER_BOUND(end_col, TERM_WINDOW_GET_REPORTED_COLS());
    for (; col < end_col; col++)
        *str++ = *t++;
    if (!(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SELECT_TRAILING_SPACES))) {
        for (str--; *str == ' ' || *str == '\t'; str--);
        str++;
    }
    if (i)
        *str++ = '\n';
    *str = '\0';
    if ((i = strlen((char *) new_selection_text)) == 0) {
        FREE(new_selection_text);
        return;
    }
    selection.len = i;
    if (selection.text)
        FREE(selection.text);
    selection.text = new_selection_text;
    selection.screen = current_screen;

    selection_copy(XA_PRIMARY);
    D_SELECT(("selection.len=%d\n", selection.len));
    return;
    tm = 0;
}

/*
 * Mark or select text based upon number of clicks: 1, 2, or 3
 * EXT: button 1 press
 */
void
selection_click(int clicks, int x, int y)
{

/*
 *  int             r, c;
 *  row_col_t       ext_beg, ext_end;
 */

    D_SELECT(("selection_click(%d, %d, %d)\n", clicks, x, y));

    clicks = ((clicks - 1) % 3) + 1;
    selection.clicks = clicks;  /* save clicks so extend will work */

    selection_start(x, y);      /* adjusts for scroll offset */
    if (clicks == 2 || clicks == 3)
        selection_extend_colrow(selection.mark.col, selection.mark.row + TermWin.view_start, 0, 1);
}

/*
 * Select text for 2 clicks
 * row is given as a normal selection row value
 * beg.row, end.row are returned as normal selection row values
 */

/* what do we want: spaces/tabs are delimiters or cutchars or non-cutchars */
#ifdef CUTCHAR_OPTION
#  define DELIMIT_TEXT(x) (strchr((rs_cutchars ? rs_cutchars : CUTCHARS), (x)))
#else
#  define DELIMIT_TEXT(x) (strchr(CUTCHARS, (x)))
#endif
#ifdef MULTI_CHARSET
#define DELIMIT_REND(x)	(((x) & RS_multiMask) ? 1 : 0)
#endif

void
selection_delimit_word(int col, int row, row_col_t *beg, row_col_t *end)
{
    int beg_col, beg_row, end_col, end_row, last_col;
    int row_offset, w1;
    text_t *stp, *stp1, t;

#ifdef MULTI_CHARSET
    int w2;
    rend_t *srp, r;

#endif

    if (selection.clicks != 2)  /* We only handle double clicks: go away */
        return;

    if (!screen.text || !screen.rend)
        return;

    last_col = TERM_WINDOW_GET_REPORTED_COLS() - 1;

    if (row >= TERM_WINDOW_GET_REPORTED_ROWS()) {
        row = TERM_WINDOW_GET_REPORTED_ROWS() - 1;
        col = last_col;
    } else if (row < -TermWin.saveLines) {
        row = -TermWin.saveLines;
        col = 0;
    }
    beg_col = end_col = col;
    beg_row = end_row = row;

    row_offset = TermWin.saveLines;

/* A: find the beginning of the word */

    if (!screen.text[beg_row + row_offset] || !screen.rend[beg_row + row_offset])
        return;
    if (!screen.text[end_row + row_offset] || !screen.rend[end_row + row_offset])
        return;
#if 0
    if (!screen.text[beg_row + row_offset - 1] || !screen.rend[beg_row + row_offset - 1])
        return;
    if (!screen.text[end_row + row_offset + 1] || !screen.rend[end_row + row_offset + 1])
        return;
#endif

    stp1 = stp = &(screen.text[beg_row + row_offset][beg_col]);
    w1 = DELIMIT_TEXT(*stp);
    if (w1 == 2)
        w1 = 0;
#ifdef MULTI_CHARSET
    srp = &(screen.rend[beg_row + row_offset][beg_col]);
    w2 = DELIMIT_REND(*srp);
#endif

    for (;;) {
        for (; beg_col > 0; beg_col--) {
            t = *--stp;
            if (DELIMIT_TEXT(t) != w1 || (w1 && *stp1 != t && BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT)))
                break;
#ifdef MULTI_CHARSET
            r = *--srp;
            if (DELIMIT_REND(r) != w2)
                break;
#endif
        }
        if (!(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT))) {
            if (beg_col == col && beg_col > 0) {
                if (DELIMIT_TEXT(*stp)) /* space or tab or cutchar */
                    break;
#ifdef MULTI_CHARSET
                srp = &(screen.rend[beg_row + row_offset][beg_col - 1]);
#endif
                for (; --beg_col > 0;) {
                    t = *--stp;
                    if (DELIMIT_TEXT(t))
                        break;
#ifdef MULTI_CHARSET
                    r = *--srp;
                    if (DELIMIT_REND(r) != w2)
                        break;
#endif
                }
            }
        }
        if (beg_col == 0 && (beg_row > -TermWin.nscrolled)) {
            stp = &(screen.text[beg_row + row_offset - 1][last_col + 1]);
            if (*stp == WRAP_CHAR) {
                t = *(stp - 1);
#ifdef MULTI_CHARSET
                srp = &(screen.rend[beg_row + row_offset - 1][last_col + 1]);
                r = *(srp - 1);
                if (DELIMIT_TEXT(t) == w1 && (!w1 || *stp == t || !(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT)))
                    && DELIMIT_REND(r) == w2) {
                    srp--;
#else
                if (DELIMIT_TEXT(t) == w1 && (!w1 || *stp == t || !(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT)))) {
#endif
                    stp--;
                    beg_row--;
                    beg_col = last_col;
                    continue;
                }
            }
        }
        break;
    }

/* B: find the end of the word */

# ifdef OPTIMIZE_HACKS
    stp = stp1;
# else
    stp1 = stp = &(screen.text[end_row + row_offset][end_col]);
# endif

#ifdef MULTI_CHARSET
    srp = &(screen.rend[end_row + row_offset][end_col]);
#endif
    for (;;) {
        for (; end_col < last_col; end_col++) {
            t = *++stp;
            if (DELIMIT_TEXT(t) != w1 || (w1 && *stp1 != t && BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT)))
                break;
#ifdef MULTI_CHARSET
            r = *++srp;
            if (DELIMIT_REND(r) != w2)
                break;
#endif
        }
        if (!(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT))) {
            if (end_col == col && end_col < last_col) {
                if (DELIMIT_TEXT(*stp)) /* space or tab or cutchar */
                    break;
#ifdef MULTI_CHARSET
                srp = &(screen.rend[end_row + row_offset][end_col + 1]);
#endif
                for (; ++end_col < last_col;) {
                    t = *++stp;
                    if (DELIMIT_TEXT(t))
                        break;
#ifdef MULTI_CHARSET
                    r = *++srp;
                    if (DELIMIT_REND(r) != w2)
                        break;
#endif
                }
            }
        }
        if (end_col == last_col && (end_row < (TERM_WINDOW_GET_REPORTED_ROWS() - 1))) {
            if (*++stp == WRAP_CHAR) {
                stp = screen.text[end_row + row_offset + 1];
#ifdef MULTI_CHARSET
                srp = screen.rend[end_row + row_offset + 1];
                if (DELIMIT_TEXT(*stp) == w1
                    && (!w1 || *stp1 == *stp || !(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT)))
                    && DELIMIT_REND(*srp) == w2) {
#else
                if (DELIMIT_TEXT(*stp) == w1
                    && (!w1 || *stp1 == *stp || !(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT)))) {
#endif
                    end_row++;
                    end_col = 0;
                    continue;
                }
            }
        }
        break;
    }

    D_SELECT(("selection_delimit_word(%d, %d) says (%d,%d)->(%d,%d)\n", col, row, beg_col, beg_row, end_col, end_row));

/* Poke the values back in */
    beg->col = beg_col;
    beg->row = beg_row;
    end->col = end_col;
    end->row = end_row;
}

/*
 * Extend the selection to the specified x/y pixel location
 * EXT: button 3 press; button 1 or 3 drag
 * flag == 0 ==> button 1
 * flag != 0 ==> button 3
 */
void
selection_extend(int x, int y, int flag)
{
    int col, row;

/*
 * If we're selecting characters (single click) then we must check first
 * if we are at the same place as the original mark.  If we are then
 * select nothing.  Otherwise, if we're to the right of the mark, you have to
 * be _past_ a character for it to be selected.
 */
    col = Pixel2Col(x);
    row = Pixel2Row(y);
    BOUND(row, 0, TERM_WINDOW_GET_REPORTED_ROWS() - 1);
    if (((selection.clicks % 3) == 1) && !flag && (col == selection.mark.col && (row == selection.mark.row + TermWin.view_start))) {
        /* select nothing */
        selection_setclr(0, selection.beg.row, selection.beg.col, selection.end.row, selection.end.col);
        selection.beg.row = selection.end.row = selection.mark.row;
        selection.beg.col = selection.end.col = selection.mark.col;
        selection.clicks = 4;
        D_SELECT(("selection.clicks = 4\n"));
        return;
    }
    if (selection.clicks == 4)
        selection.clicks = 1;
    selection_extend_colrow(col, row, flag, 0);
}

/*
 * Extend the selection to the specified col/row
 */
void
selection_extend_colrow(int col, int row, int flag, int cont)
{
    int old_col, end_col;
    row_col_t new_beg1, new_beg2, new_end1, new_end2, old_beg, old_end;
    enum {
        LEFT, RIGHT
    } closeto = RIGHT;

#ifdef MULTI_CHARSET
    int r;

#endif

    D_SELECT(("selection_extend_colrow(%d, %d, %d, %d) clicks:%d\n", col, row, flag, cont, selection.clicks));

    switch (selection.op) {
        case SELECTION_INIT:
            selection_reset();
            selection.end.col = selection.beg.col = selection.mark.col;
            selection.end.row = selection.beg.row = selection.mark.row;
            selection.op = SELECTION_BEGIN;
            /* FALLTHROUGH */
        case SELECTION_BEGIN:
            break;
        case SELECTION_DONE:
            selection.op = SELECTION_CONT;
            /* FALLTHROUGH */
        case SELECTION_CONT:
            break;
        case SELECTION_CLEAR:
            selection_start_colrow(col, row);
            /* FALLTHROUGH */
        default:
            return;
    }
    if ((selection.beg.row < -TermWin.nscrolled)
        || (selection.end.row < -TermWin.nscrolled)) {
        selection_reset();
        return;
    }
    old_col = col;
    BOUND(col, -1, TERM_WINDOW_GET_REPORTED_COLS());
    old_beg.col = selection.beg.col;
    old_beg.row = selection.beg.row;
    old_end.col = selection.end.col;
    old_end.row = selection.end.row;

    if ((selection.op == SELECTION_BEGIN) && (cont || (row != selection.mark.row || col != selection.mark.col)))
        selection.op = SELECTION_CONT;

    row -= TermWin.view_start;  /* adjust for scroll */

    if (flag) {
        if (row < selection.beg.row || (row == selection.beg.row && col < selection.beg.col))
            closeto = LEFT;
        else if (row > selection.end.row || (row == selection.end.row && col >= selection.end.col)) {
            /* */ ;
        } else if (((col - selection.beg.col)
                    + ((row - selection.beg.row) * TERM_WINDOW_GET_REPORTED_COLS()))
                   < ((selection.end.col - col)
                      + ((selection.end.row - row) * TERM_WINDOW_GET_REPORTED_COLS())))
            closeto = LEFT;
    }
    if (selection.clicks == 1) {
/*
 * A1: extension on single click - selection between points
 */
        if (flag) {             /* button 3 extension */
            if (closeto == LEFT) {
                selection.beg.row = row;
                selection.beg.col = col;
                end_col = screen.text[row + TermWin.saveLines][TERM_WINDOW_GET_REPORTED_COLS()];
                if (end_col != WRAP_CHAR && selection.beg.col > end_col) {
                    if (selection.beg.row < selection.end.row) {
                        selection.beg.col = -1;
                        selection.beg.row++;
                    } else {
                        selection.beg.col = selection.mark.col;
                        selection.beg.row = selection.mark.row;
                    }
                }
            } else {
                selection.end.row = row;
                selection.end.col = col - 1;
                end_col = screen.text[row + TermWin.saveLines][TERM_WINDOW_GET_REPORTED_COLS()];
                if (end_col != WRAP_CHAR && selection.end.col >= end_col)
                    selection.end.col = TERM_WINDOW_GET_REPORTED_COLS() - 1;
            }
        } else if ((row < selection.mark.row)
                   || (row == selection.mark.row && col < selection.mark.col)) {
            /* select left of mark character excluding mark */
            selection.beg.row = row;
            selection.beg.col = col;
            selection.end.row = selection.mark.row;
            selection.end.col = selection.mark.col - 1;
            if (selection.end.col >= 0) {
                end_col = screen.text[row + TermWin.saveLines][TERM_WINDOW_GET_REPORTED_COLS()];
                if (end_col != WRAP_CHAR && selection.beg.col > end_col) {
                    if (selection.beg.row < selection.end.row) {
                        selection.beg.col = -1;
                        selection.beg.row++;
                    } else {
                        selection.beg.col = selection.mark.col;
                        selection.beg.row = selection.mark.row;
                    }
                }
            }
        } else {
            /* select right of mark character including mark */
            selection.beg.row = selection.mark.row;
            selection.beg.col = selection.mark.col;
            selection.end.row = row;
            selection.end.col = col - 1;
            if (old_col >= 0) {
                end_col = screen.text[row + TermWin.saveLines][TERM_WINDOW_GET_REPORTED_COLS()];
                if (end_col != WRAP_CHAR && selection.end.col >= end_col)
                    selection.end.col = TERM_WINDOW_GET_REPORTED_COLS() - 1;
            }
        }
#ifdef MULTI_CHARSET
        if ((selection.beg.col > 0) && (selection.beg.col < TERM_WINDOW_GET_REPORTED_COLS())) {
            r = selection.beg.row + TermWin.saveLines;
            if (((screen.rend[r][selection.beg.col] & RS_multiMask) == RS_multi2)
                && ((screen.rend[r][selection.beg.col - 1] & RS_multiMask) == RS_multi1))
                selection.beg.col--;
        }
        if ((selection.end.col > 0) && (selection.end.col < (TERM_WINDOW_GET_REPORTED_COLS() - 1))) {
            r = selection.end.row + TermWin.saveLines;
            if (((screen.rend[r][selection.end.col] & RS_multiMask) == RS_multi1)
                && ((screen.rend[r][selection.end.col + 1] & RS_multiMask) == RS_multi2))
                selection.end.col++;
        }
#endif
    } else if (selection.clicks == 2) {
/*
 * A2: extension on double click - selection between words
 */
        selection_delimit_word(col, row, &new_beg2, &new_end2);
        if (flag && closeto == LEFT)
            selection_delimit_word(selection.end.col, selection.end.row, &new_beg1, &new_end1);
        else if (flag && closeto == RIGHT)
            selection_delimit_word(selection.beg.col, selection.beg.row, &new_beg1, &new_end1);
        else
            selection_delimit_word(selection.mark.col, selection.mark.row, &new_beg1, &new_end1);
        if ((!flag && (selection.mark.row < row || (selection.mark.row == row && selection.mark.col <= col)))
            || (flag && closeto == RIGHT)) {
            selection.beg.col = new_beg1.col;
            selection.beg.row = new_beg1.row;
            selection.end.col = new_end2.col;
            selection.end.row = new_end2.row;
        } else {
            selection.beg.col = new_beg2.col;
            selection.beg.row = new_beg2.row;
            selection.end.col = new_end1.col;
            selection.end.row = new_end1.row;
        }
    } else if (selection.clicks == 3) {
/*
 * A3: extension on triple click - selection between lines
 */
        if (flag) {
            if (closeto == LEFT)
                selection.beg.row = row;
            else
                selection.end.row = row;
        } else if (row <= selection.mark.row) {
            selection.beg.row = row;
            selection.end.row = selection.mark.row;
        } else {
            selection.beg.row = selection.mark.row;
            selection.end.row = row;
        }
        if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SELECT_WHOLE_LINE)) {
            selection.beg.col = 0;
        } else {
            selection.clicks = 2;
            selection_delimit_word(col, row, &new_beg2, &new_end2);
            selection.beg.col = new_beg2.col;
            selection.clicks = 3;
        }
        selection.end.col = TERM_WINDOW_GET_REPORTED_COLS() - 1;
    }
    D_SELECT(("(c:%d,r:%d)-(c:%d,r:%d) old (c:%d,r:%d)-(c:%d,r:%d)\n", selection.beg.col, selection.beg.row,
              selection.end.col, selection.end.row, old_beg.col, old_beg.row, old_end.col, old_end.row));

/*
 * B1: clear anything before the current selection
 */
    if ((old_beg.row < selection.beg.row) || (old_beg.row == selection.beg.row && old_beg.col < selection.beg.col)) {
        if (selection.beg.col < TERM_WINDOW_GET_REPORTED_COLS() - 1) {
            row = selection.beg.row;
            col = selection.beg.col + 1;
        } else {
            row = selection.beg.row + 1;
            col = 0;
        }
        selection_setclr(0, old_beg.row, old_beg.col, row, col);
    }
/*
 * B2: clear anything after the current selection
 */
    if ((old_end.row > selection.end.row) || (old_end.row == selection.end.row && old_end.col > selection.end.col)) {
        if (selection.end.col > 0) {
            row = selection.end.row;
            col = selection.end.col - 1;
        } else {
            row = selection.end.row - 1;
            col = TERM_WINDOW_GET_REPORTED_COLS() - 1;
        }
        selection_setclr(0, row, col, old_end.row, old_end.col);
    }
/*
 * B3: set everything
 */
/* TODO: optimise this */
    selection_setclr(1, selection.beg.row, selection.beg.col, selection.end.row, selection.end.col);
    return;
}

/*
 * Double click on button 3 when already selected
 * EXT: button 3 double click
 */
void
selection_rotate(int x, int y)
{
    int col, row;

    col = Pixel2Col(x);
    row = Pixel2Row(y);
    selection.clicks = selection.clicks % 3 + 1;
    selection_extend_colrow(col, row, 1, 0);
}

/*
 * Respond to a request for our current selection
 * EXT: SelectionRequest
 */
void
selection_send(XSelectionRequestEvent * rq)
{
    XEvent ev;
    long target_list[2];

    ev.xselection.type = SelectionNotify;
    ev.xselection.property = None;
    ev.xselection.display = rq->display;
    ev.xselection.requestor = rq->requestor;
    ev.xselection.selection = rq->selection;
    ev.xselection.target = rq->target;
    ev.xselection.time = rq->time;

    if (rq->target == props[PROP_SELECTION_TARGETS]) {
        target_list[0] = props[PROP_SELECTION_TARGETS];
        target_list[1] = XA_STRING;
        XChangeProperty(Xdisplay, rq->requestor, rq->property, rq->target,
                        32, PropModeReplace, (unsigned char *) target_list,
                        (sizeof(target_list) / sizeof(target_list[0])));
        ev.xselection.property = rq->property;
#ifdef MULTI_CHARSET
#  ifdef X_HAVE_UTF8_STRING
    } else if (rq->target == props[PROP_UTF8_STRING]) {
        XTextProperty xtextp;
        char *l[1];

        *l = selection.text;
        xtextp.value = NULL;
        xtextp.nitems = 0;
        if (XmbTextListToTextProperty(Xdisplay, l, 1, XUTF8StringStyle, &xtextp) == Success) {
            if (xtextp.nitems > 0 && xtextp.value) {
                XChangeProperty(Xdisplay, rq->requestor, rq->property,
                                rq->target, 8, PropModeReplace, xtextp.value, xtextp.nitems);
                ev.xselection.property = rq->property;
                XFree(xtextp.value);
            }
        }
#  endif /* X_HAVE_UTF8_STRING */
    } else if (rq->target == props[PROP_TEXT] || rq->target == props[PROP_COMPOUND_TEXT]) {
        XTextProperty xtextp;
        char *l[1];

        *l = selection.text;
        xtextp.value = NULL;
        xtextp.nitems = 0;
        if (XmbTextListToTextProperty(Xdisplay, l, 1, XCompoundTextStyle, &xtextp) == Success) {
            if (xtextp.nitems > 0 && xtextp.value) {
                XChangeProperty(Xdisplay, rq->requestor, rq->property, props[PROP_COMPOUND_TEXT],
                                8, PropModeReplace, xtextp.value, xtextp.nitems);
                ev.xselection.property = rq->property;
                XFree(xtextp.value);
            }
        }
#endif /* MULTI_CHARSET */
    } else {
        XChangeProperty(Xdisplay, rq->requestor, rq->property, XA_STRING, 8, PropModeReplace, selection.text, selection.len);
        ev.xselection.property = rq->property;
    }
    XSendEvent(Xdisplay, rq->requestor, False, 0, &ev);
}

void                            /* drag report as used by the "twin" program */
twin_mouse_drag_report(XButtonEvent * ev)
{
    int button_number, key_state, x = Pixel2Col(ev->x), y = Pixel2Row(ev->y);

    switch (ev->button) {
        case AnyButton:        /* Button release */
            button_number = pb + Button1;       /* yeah, yeah */
            break;
        case Button1:          /* Button press */
        case Button2:
        case Button3:
            pb = button_number = ev->button - Button1;
            break;
        default:               /* Wheel mouse */
            button_number = 64 + ev->button - Button3 - 1;
            break;
    }
    key_state = ((ev->state & (ShiftMask | ControlMask))
                 + ((ev->state & Mod1Mask) ? 2 : 0));
    tt_printf((unsigned char *) "\033[5M%c%c%c%c%c",
              (32 + button_number + (key_state << 2)), (32 + (x & 0x7f) + 1), (32 + ((x >> 7) & 0x7f) + 1), (32 + (y & 0x7f) + 1),
              (32 + ((y >> 7) & 0x7f) + 1));
}

void
mouse_report(XButtonEvent * ev)
{
    int button_number, key_state;

    switch (ev->button) {
        case AnyButton:        /* Button release */
            button_number = 3;
            break;
        case Button1:          /* Button press */
        case Button2:
        case Button3:
            pb = button_number = ev->button - Button1;
            break;
        default:               /* Wheel mouse */
            button_number = 64 + ev->button - Button3 - 1;
            break;
    }
    key_state = ((ev->state & (ShiftMask | ControlMask))
                 + ((ev->state & Mod1Mask) ? 2 : 0));
    tt_printf((unsigned char *) "\033[M%c%c%c", (32 + button_number + (key_state << 2)), (32 + Pixel2Col(ev->x) + 1),
              (32 + Pixel2Row(ev->y) + 1));
}

void
debug_colors(void)
{
    int color;
    char *name[] = {
        "fg", "bg",
        "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"
    };

    fprintf(stderr, "Color ( ");
    if (rstyle & RS_RVid)
        fprintf(stderr, "rvid ");
    if (rstyle & RS_Bold)
        fprintf(stderr, "bold ");
    if (rstyle & RS_Blink)
        fprintf(stderr, "blink ");
    if (rstyle & RS_Uline)
        fprintf(stderr, "uline ");
    if (rstyle & RS_Overscore)
        fprintf(stderr, "overscore ");
    if (rstyle & RS_Italic)
        fprintf(stderr, "italic ");
    if (rstyle & RS_Dim)
        fprintf(stderr, "dim ");
    if (rstyle & RS_Conceal)
        fprintf(stderr, "conceal ");
    fprintf(stderr, "): ");

    color = GET_FGCOLOR(rstyle);
    if (color >= minBright && color <= maxBright) {
        color -= (minBright - minColor);
        fprintf(stderr, "bright ");
    }
    fprintf(stderr, "%s on ", name[color]);

    color = GET_BGCOLOR(rstyle);
    if (color >= minBright && color <= maxBright) {
        color -= (minBright - minColor);
        fprintf(stderr, "bright ");
    }
    fprintf(stderr, "%s\n", name[color]);
}

#ifdef USE_XIM
void
xim_get_position(XPoint * pos)
{
    pos->x = Col2Pixel(screen.col);
    if (scrollbar_is_visible() && !(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT))) {
        pos->x += scrollbar_trough_width();
    }
    pos->y = (Height2Pixel(screen.row)
# ifdef MULTI_CHARSET
              + MAX((encoding_method == LATIN1 ? 0 : TermWin.mfont->ascent), TermWin.font->ascent)
# else
              + TermWin.font->ascent
# endif
              + TermWin.internalBorder + bbar_calc_docked_height(BBAR_DOCKED_TOP));
}
#endif

#ifdef ESCREEN
#  ifdef NS_HAVE_SCREEN
void
parse_screen_status_if_necessary(void)
{
    ns_parse_screen(TermWin.screen, (TermWin.screen_pending > 1),
                    TERM_WINDOW_GET_REPORTED_COLS(), screen.text[TERM_WINDOW_GET_REPORTED_ROWS() + TermWin.saveLines - 1]);
    if (TermWin.screen_pending > 1)
        TermWin.screen_pending = 0;
}
#  endif
#endif
