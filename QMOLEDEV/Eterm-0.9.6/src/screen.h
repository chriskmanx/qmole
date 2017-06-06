/*--------------------------------*-C-*---------------------------------*
 * File:    screen.h
 *
 * This module is all new by Robert Nation
 * <nation@rocket.sanders.lockheed.com>
 *
 * Additional modifications by mj olesen <olesen@me.QueensU.CA>
 * No additional restrictions are applied.
 *
 * As usual, the author accepts no responsibility for anything, nor does
 * he guarantee anything whatsoever.
 *----------------------------------------------------------------------*/
#ifndef _SCREEN_H
#define _SCREEN_H

#include <X11/Xfuncproto.h>
#include "startup.h"

/************ Macros and Definitions ************/
#define WRAP_CHAR       (0xff)
#define PROP_SIZE           4096
#define TABSIZE             8   /* default tab size */

#define IS_SELECTION(a)         (((a) == XA_PRIMARY) || ((a) == XA_SECONDARY) || ((a) == props[PROP_CLIPBOARD]))
#define IS_CUT_BUFFER(a)        (((a) >= XA_CUT_BUFFER0) && ((a) <= XA_CUT_BUFFER7))

#define ZERO_SCROLLBACK do { \
                          if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_HOME_ON_OUTPUT)) TermWin.view_start = 0; \
                        } while (0)
#define REFRESH_ZERO_SCROLLBACK do { \
                                  if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_HOME_ON_OUTPUT)) TermWin.view_start = 0; \
                                } while (0)
#define CHECK_SELECTION do { \
                          if (selection.op) selection_check(); \
                        } while (0)
#define CLEAR_SELECTION (selection.beg.row = selection.beg.col = selection.end.row = selection.end.col = 0)
#define CLEAR_ALL_SELECTION (selection.beg.row = selection.beg.col = selection.mark.row = selection.mark.col = selection.end.row = selection.end.col = 0)

#define scr_touch()  (refresh_all = 1)

/*
 * CLEAR_ROWS : clear <num> rows starting from row <row>
 * CLEAR_CHARS: clear <num> chars starting from pixel position <x,y>
 * ERASE_ROWS : set <num> rows starting from row <row> to the foreground color
 */
#define CLEAR_ROWS(row, num)  do {if (buffer_pixmap) {XCopyArea(Xdisplay, pmap, buffer_pixmap, TermWin.gc, Col2Pixel(0), Row2Pixel(row), TERM_WINDOW_GET_WIDTH(), Height2Pixel(num), \
                                  Col2Pixel(0), Row2Pixel(row));} XClearArea(Xdisplay, TermWin.vt, Col2Pixel(0), Row2Pixel(row), TERM_WINDOW_GET_WIDTH(), Height2Pixel(num), 0);} while (0)
#define CLEAR_CHARS(x, y, num) ((buffer_pixmap) \
                               ? (XCopyArea(Xdisplay, pmap, buffer_pixmap, TermWin.gc, x, y, Width2Pixel(num), Height2Pixel(1), x, y)) \
                               : (XClearArea(Xdisplay, TermWin.vt, x, y, Width2Pixel(num), Height2Pixel(1), 0)))
#define CLEAR_RECT(x, y, w, h) ((buffer_pixmap) \
                               ? (XCopyArea(Xdisplay, pmap, buffer_pixmap, TermWin.gc, x, y, w, h, x, y)) \
                               : (XClearArea(Xdisplay, TermWin.vt, x, y, w, h, 0)))
#define UPDATE_BOX(x1, y1, x2, y2)  do {if (buffer_pixmap) {if (x1 < low_x) low_x = x1; if (x2 > high_x) high_x = x2; \
                                                            if (y1 < low_y) low_y = y1; if (y2 > high_y) high_y = y2;}} while (0)
#define ERASE_ROWS(row, num)  do {XFillRectangle(Xdisplay, draw_buffer, TermWin.gc, Col2Pixel(0), Row2Pixel(row), TERM_WINDOW_GET_WIDTH(), Height2Pixel(num)); \
                                  if (buffer_pixmap) {XClearArea(Xdisplay, TermWin.vt, Col2Pixel(0), Row2Pixel(row), TERM_WINDOW_GET_WIDTH(), Height2Pixel(num), 0);}} while (0)
#if FIXME_BLOCK
# define DRAW_STRING(Func, x, y, str, len)  Func(Xdisplay, draw_buffer, TermWin.fontset, TermWin.gc, x, y, str, len)
#else
# define DRAW_STRING(Func, x, y, str, len)  Func(Xdisplay, draw_buffer, TermWin.gc, x, y, str, len)
#endif

/* Make bold if bold flag is set and either we're drawing the foreground color or we're not suppressing bold.
   In other words, the foreground color can always be bolded, but other colors can't if bold is suppressed. */
#define MONO_BOLD(x) (((x) & RS_Bold) && (!BITFIELD_IS_SET(vt_options, VT_OPTIONS_COLORS_SUPPRESS_BOLD) || fore == fgColor))

/* Screen refresh methods */
#define NO_REFRESH              0       /* Window not visible at all!        */
#define FAST_REFRESH            (1<<1)  /* Fully exposed window              */
#define SLOW_REFRESH            (1<<2)  /* Partially exposed window          */
#define SMOOTH_REFRESH          (1<<3)  /* Do sync'ing to make it smooth     */

#define IGNORE  0
#define SAVE    's'
#define RESTORE 'r'
#define REVERT IGNORE
#define INVOKE RESTORE

/* flags for scr_gotorc() */
#define C_RELATIVE      1   /* col movement is relative */
#define R_RELATIVE      2   /* row movement is relative */
#define RELATIVE        (R_RELATIVE|C_RELATIVE)

/* modes for scr_insdel_chars(), scr_insdel_lines() */
#define INSERT          -1  /* don't change these values */
#define DELETE          +1
#define ERASE           +2

/* modes for scr_page() - scroll page. used by scrollbar window */
enum {
    UP,
    DN,
    NO_DIR
};

/* arguments for scr_change_screen() */
enum {
    PRIMARY,
    SECONDARY
};

#ifdef MULTI_CHARSET
#define RS_multi1       0x80000000u /* multibyte 1st byte */
#define RS_multi0       0x40000000u /* only multibyte characters */
#define RS_multi2       (RS_multi0|RS_multi1)   /* multibyte 2nd byte */
#define RS_multiMask    (RS_multi0|RS_multi1)   /* multibyte mask */
#endif
#define RS_ukFont       0x20000000u /* UK character set */
#define RS_acsFont      0x10000000u /* ACS graphics character set */
#define RS_fontMask     (RS_acsFont|RS_ukFont)
#define RS_Uline        0x08000000u /* underline */
#define RS_RVid         0x04000000u /* reverse video */
#define RS_Select       0x02000000u /* selected text */
#define RS_Cursor       0x01000000u /* cursor location */
#define RS_Blink        0x00800000u /* blink */
#define RS_Conceal      0x00400000u /* conceal */
#define RS_Dim          0x00200000u /* dim (apply alpha) */
#define RS_Bold         0x00100000u /* bold */
#define RS_Italic       0x00080000u /* italic */
#define RS_Overscore    0x00040000u /* overscore */
#define RS_fgMask       0x0003FE00u /* 512 colors */
#define RS_bgMask       0x000001FFu /* 512 colors */
#define RS_None         0x00000000u /* Normal */

#define RS_attrMask     (0xFF000000u|RS_Overscore|RS_Italic|RS_Bold|RS_Dim|RS_Conceal|RS_Blink)

/* how to build & extract colors and attributes */
#define GET_FGCOLOR(r)        (((r) & RS_fgMask)>>9)
#define GET_BGCOLOR(r)        (((r) & RS_bgMask))
#define GET_ATTR(r)           (((r) & RS_attrMask))
#define GET_BGATTR(r)         (((r) & (RS_attrMask | RS_bgMask)))

#define SET_FGCOLOR(r,fg)     (((r) & ~RS_fgMask)  | ((fg)<<9))
#define SET_BGCOLOR(r,bg)     (((r) & ~RS_bgMask)  | (bg))
#define SET_ATTR(r,a)         (((r) & ~RS_attrMask)| (a))
#define DEFAULT_RSTYLE        (RS_None | (fgColor<<9) | (bgColor))

/* screen_t flags */
#define Screen_Relative       (1<<0)    /* relative origin mode flag         */
#define Screen_VisibleCursor  (1<<1)    /* cursor visible?                   */
#define Screen_Autowrap       (1<<2)    /* auto-wrap flag                    */
#define Screen_Insert         (1<<3)    /* insert mode (vs. overstrike)      */
#define Screen_WrapNext       (1<<4)    /* need to wrap for next char?       */
#define Screen_DefaultFlags   (Screen_VisibleCursor|Screen_Autowrap)

/************ Structures ************/
/* General overview of the screen stuff:

   TermWin.saveLines tells us how many lines are in the scrollback buffer.
   There are a total of TermWin.saveLines + TermWin.nrow rows in the
   screen buffer, with the scrollback coming before the on-screen data.
   TermWin.nscrolled tells us how many lines of the scrollback buffer have
   actually been used (i.e., allocated).  TermWin.view_start tells us how
   many lines back into the scrollback buffer the currently-visible data
   is.  (0 means we're at the bottom and not in scrollback.)
*/
typedef unsigned char text_t;
typedef unsigned int rend_t;
typedef enum {
    SELECTION_CLEAR = 0,
    SELECTION_INIT,
    SELECTION_BEGIN,
    SELECTION_CONT,
    SELECTION_DONE
} selection_op_t;
typedef enum {
    LATIN1 = 0, UCS2, EUCJ, EUCKR = EUCJ, GB = EUCJ, SJIS, BIG5
} encoding_t;
typedef struct {
    short row, col;
} row_col_t;
/* screen_t:

   screen.text contains a 2-D array of the screen data.  screen.rend contains
   a matching 2-D array of rendering information (as 32-bit masks).  They are
   allocated together, so you can always be sure that screen.rend[r] will be
   allocated if screen.text[r] is.  You are also guaranteed that each row of
   screen.text is TermWin.ncol + 1 columns long, and each row of screen.rend
   is TermWin.ncol columns long.  They both have (TermWin.nrow +
   TermWin.saveLines) rows, but only TermWin.nrow + TermWin.nscrolled lines
   are actually allocated.  The extra column in the text array is for storing
   line wrap information.  It will either be the length of the line, or 
   WRAP_CHAR if the line wraps into the next line.

   screen.row and screen.col contain the current cursor position.  It is always
   somewhere on the visible screen.  screen.tscroll and screen.bscroll are the
   top and bottom rows of the current scroll region.  screen.charset is the
   character set currently being used (0-3).
*/
typedef struct {
    text_t **text;
    rend_t **rend;
    short row, col;
    short tscroll, bscroll;
    unsigned char charset:2;
    unsigned char flags:5;
} screen_t;
/* A save_t object is used to save/restore the cursor position and other
   relevant data when requested to do so by the application. */
typedef struct {
    short row, col;
    short charset;
    char charset_char;
    rend_t rstyle;
} save_t;
/* selection_t:

   selection.text is a string containing the current selection text.  It is
   duplicated from the screen data.  selection.len is the length of that string.
   selection.op represents the current state, selection-wise.  selection.screen
   gives the number (0 or 1) of the current screen.  selection.clicks tells how
   many clicks created the current selection (0-3, or 4 if nothing is selected).
   beg, mark, and end represent the row and column of the beginning of the
   selection, the click that created the selection, and the end of the selection,
   respectively.

   -TermWin.nscrolled <= beg.row <= mark.row <= end.row < TermWin.nrow
*/
typedef struct {
    text_t  *text;
    int len;
    selection_op_t op;
    unsigned short screen:1;
    unsigned char clicks:3;
    row_col_t beg, mark, end;
} selection_t;

/************ Variables ************/
extern unsigned int colorfgbg;
extern unsigned char refresh_all;
#ifdef MULTI_CHARSET
extern encoding_t encoding_method;
#endif
#ifdef ESCREEN
extern screen_t screen;
#endif

/************ Function Prototypes ************/
_XFUNCPROTOBEGIN

extern void blank_dline(text_t *, rend_t *, int, rend_t);
extern void blank_sline(text_t *, rend_t *, int);
extern void make_screen_mem(text_t **, rend_t **, int);
extern void scr_reset(void);
extern void scr_release(void);
extern void scr_poweron(void);
extern void scr_cursor(int);
extern int scr_change_screen(int);
extern void scr_color(unsigned int, unsigned int);
extern void scr_rendition(int, int);
extern int scroll_text(int, int, int, int);
extern void scr_add_lines(const unsigned char *, int, int);
extern void scr_backspace(void);
extern void scr_tab(int);
extern void scr_gotorc(int, int, int);
extern void scr_index(int);
extern void scr_erase_line(int);
extern void scr_erase_screen(int);
extern void scr_E(void);
extern void scr_insdel_lines(int, int);
extern void scr_insdel_chars(int, int);
extern void scr_scroll_region(int, int);
extern void scr_cursor_visible(int);
extern void scr_autowrap(int);
extern void scr_relative_origin(int);
extern void scr_insert_mode(int);
extern void scr_set_tab(int);
extern void scr_rvideo_mode(int);
extern void scr_report_position(void);
extern void set_font_style(void);
extern void scr_charset_choose(int);
extern void scr_charset_set(int, unsigned int);
extern void set_multichar_encoding(const char *);
extern int scr_get_fgcolor(void);
extern int scr_get_bgcolor(void);
extern void scr_expose(int, int, int, int);
extern int scr_move_to(int, int);
extern int scr_page(int, int);
extern void scr_bell(void);
extern void scr_printscreen(int);
extern void scr_refresh(int);
extern int scr_strmatch(unsigned long, unsigned long, const char *);
extern void scr_search_scrollback(char *);
extern void scr_dump(void);
extern void scr_dump_to_file(const char *);
extern void selection_check(void);
extern void selection_write(unsigned char *, size_t);
extern void selection_fetch(Window, unsigned, int);
extern void selection_copy_string(Atom, char *, size_t);
extern void selection_copy(Atom);
extern void selection_paste(Atom);
extern void selection_reset(void);
extern void selection_clear(void);
extern void selection_setclr(int, int, int, int, int);
extern void selection_start(int, int);
extern void selection_start_colrow(int, int);
extern void selection_make(Time);
extern void selection_click(int, int, int);
extern void selection_delimit_word(int, int, row_col_t *, row_col_t *);
extern void selection_extend(int, int, int);
extern void selection_extend_colrow(int, int, int, int);
extern void selection_rotate(int, int);
extern void selection_send(XSelectionRequestEvent *);
extern void mouse_report(XButtonEvent *);
extern void twin_mouse_drag_report(XButtonEvent *);
extern void mouse_tracking(int, int, int, int, int);
extern void debug_colors(void);
#ifdef MULTI_CHARSET
extern int scr_multi2(void);
extern int scr_multi1(void);
#endif /* MULTI_CHARSET */
#ifdef ESCREEN
extern void parse_screen_status_if_necessary(void);
#endif

_XFUNCPROTOEND

#endif
