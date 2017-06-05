/*--------------------------------*-H-*---------------------------------*
 * File:	protos.h
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1997-2001   Geoff Wing <gcw@pobox.com>
 * Copyright (c) 2004        Sergey Popov <p_sergey@jungo.com>
 * Copyright (c) 2004        Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *----------------------------------------------------------------------*/
/*
** $Id: protos.h,v 1.86 2005/05/12 20:32:27 cvs Exp $
*/

#ifndef __PROTOS_H__
#define __PROTOS_H__


/* Begin prototypes of command.c */
void             rxvt_init_hotkey_handlers        __PROTO((rxvt_t* r));
unsigned int     rxvt_cmd_write                   __PROTO((rxvt_t* r, int page, const unsigned char* str, unsigned int count));
int              rxvt_check_our_parents           __PROTO((rxvt_t *r));
FILE*            rxvt_popen_printer               __PROTO((rxvt_t *r));
int              rxvt_pclose_printer              __PROTO((FILE *stream));
void             rxvt_xterm_seq                   __PROTO((rxvt_t* r, int page, int op, const char* str, unsigned char resp __attribute__((unused))));
void             rxvt_tt_printf                   __PROTO((rxvt_t* r, int page, const char* fmt,...));
void             rxvt_tt_write                    __PROTO((rxvt_t* r, int page, const unsigned char* d, int len));
void             rxvt_pointer_unblank             __PROTO((rxvt_t* r, int page));
#if defined(TRANSPARENT) || defined(BACKGROUND_IMAGE)
# ifdef TINTING_SUPPORT
void             rxvt_shade_pixmap                __PROTO((rxvt_t* r, Drawable d, GC gc, int sx, int sy, unsigned int nw, unsigned int nh, int shade, unsigned long pixel));
# endif
#endif
void             rxvt_resize_on_subwin            __PROTO((rxvt_t* r, resize_reason_t reason));
/* End prototypes of command.c */


/* Begin prototypes of encoding.c */
#ifdef MULTICHAR_SET
void             rxvt_decode_euc2jis              __PROTO((unsigned char* str, int len));
void             rxvt_decode_sjis2jis             __PROTO((unsigned char* str, int len));
void             rxvt_decode_gb180302jis          __PROTO((unsigned char* str, int len));
void             rxvt_set_multichar_encoding      __PROTO((rxvt_t* r, const char* str));
#endif
void             rxvt_decode_dummy                __PROTO((unsigned char* str, int len));
void             rxvt_set_default_locale          __PROTO((rxvt_t *r));
void             rxvt_set_default_font_x11        __PROTO((rxvt_t *r));
char*            rxvt_fallback_mfont_x11          __PROTO((rxvt_t *r));
#ifdef XFT_SUPPORT
void             rxvt_set_default_font_xft        __PROTO((rxvt_t *r));
char*            rxvt_fallback_mfont_xft          __PROTO((rxvt_t *r));
#endif
char*            rxvt_encoding_name               __PROTO((rxvt_t *r));
/* End prototypes of defaultfont.c */


/* Begin prototypes of grkelot.c */
#ifdef GREEK_SUPPORT
void             greek_init                       __PROTO((void));
void             greek_end                        __PROTO((void));
void             greek_reset                      __PROTO((void));
void             greek_setmode                    __PROTO((int greek_mode));
int              greek_getmode                    __PROTO((void));
int              greek_xlat                       __PROTO((char* s, int num_chars));
#endif
/* End prototypes of grkelot.c */


/* Begin prototypes of init.c */
int              rxvt_init_vars                   __PROTO((rxvt_t* r));
void             rxvt_init_secondary              __PROTO((rxvt_t* r));
void             rxvt_init_hotkeys                __PROTO((rxvt_t* r));
void             rxvt_toggle_hotkeys              __PROTO((rxvt_t* r, int enable));
const char    ** rxvt_init_resources              __PROTO((rxvt_t* r, int argc, const char* const *argv));
unsigned long    rxvt_fade_color                  __PROTO((rxvt_t* r, unsigned long));
void             rxvt_switch_fgbg_color           __PROTO((rxvt_t* r, int page));
int              rxvt_restore_ufbg_color          __PROTO((rxvt_t* r));
int              rxvt_switch_ufbg_color           __PROTO((rxvt_t* r));
#ifdef OFF_FOCUS_FADING
int              rxvt_restore_pix_color           __PROTO((rxvt_t* r));
int              rxvt_switch_pix_color            __PROTO((rxvt_t* r));
#endif
void             rxvt_init_env                    __PROTO((rxvt_t *r));
void             rxvt_init_xlocale                __PROTO((rxvt_t *r));
void             rxvt_init_command                __PROTO((rxvt_t* r, const char* const *argv));
CARD32           rxvt_get_desktop                 __PROTO((rxvt_t* r));
void             rxvt_create_show_windows         __PROTO((rxvt_t* r, int argc, const char* const *argv));
void             rxvt_create_termwin              __PROTO((rxvt_t* r, int page, const char TAINTED * title));
void             rxvt_destroy_termwin             __PROTO((rxvt_t* r, int page));
int              rxvt_run_command                 __PROTO((rxvt_t* r, int page, const char** argv));
termenv_t        rxvt_get_termenv                 __PROTO((const char* str));
/* End prototypes of init.c */


/* Begin prototypes of logging.c */
#ifdef UTMP_SUPPORT
void             rxvt_makeutent                   __PROTO((rxvt_t* r, int page, const char* pty, const char* hostname));
void             rxvt_cleanutent                  __PROTO((rxvt_t* r, int page));
#endif
/* End prototypes of logging.c */


/* Begin prototypes of main.c */
void             rxvt_privileges                  __PROTO((int mode));
RETSIGTYPE       rxvt_Child_signal                __PROTO((int sig __attribute__((unused))));
RETSIGTYPE       rxvt_Exit_signal                 __PROTO((int sig));
void             rxvt_clean_exit                  __PROTO((rxvt_t* r));
void*            rxvt_malloc                      __PROTO((size_t size));
void*            rxvt_calloc                      __PROTO((size_t number, size_t size));
void*            rxvt_realloc                     __PROTO((void *ptr, size_t size));
void             rxvt_privileged_utmp             __PROTO((rxvt_t* r, int page, char action));
void             rxvt_privileged_ttydev           __PROTO((rxvt_t* r, int page, char action));
void             rxvt_tt_winsize                  __PROTO((int fd, unsigned short col, unsigned short row, pid_t pid));
void             rxvt_init_font_x11               __PROTO((rxvt_t *r));
int              rxvt_change_font_x11             __PROTO((rxvt_t* r, const char* fontname));
#ifdef XFT_SUPPORT
int              rxvt_init_font_xft               __PROTO((rxvt_t *r));
int              rxvt_change_font_xft             __PROTO((rxvt_t* r, const char* fontname));
#endif
void             rxvt_set_win_title               __PROTO((rxvt_t* r, Window win, const char* str));
void             rxvt_set_term_title              __PROTO((rxvt_t* r, const unsigned char* str));
void             rxvt_set_icon_name               __PROTO((rxvt_t* r, const unsigned char* str));
void             rxvt_set_window_color            __PROTO((rxvt_t* r, int idx, const char* color));
void             rxvt_recolour_cursor             __PROTO((rxvt_t *r));
#ifdef XFT_SUPPORT
int              rxvt_alloc_xft_color             __PROTO((rxvt_t *r, unsigned long pixel, XftColor* xftcolor));
#endif
int              rxvt_parse_alloc_color           __PROTO((rxvt_t* r, XColor *screen_in_out, const char* colour));
int              rxvt_alloc_color                 __PROTO((rxvt_t* r, XColor *screen_in_out, const char* colour));
void             rxvt_set_widthheight             __PROTO((rxvt_t* r, unsigned int width, unsigned int height));
void             rxvt_IM_send_spot                __PROTO((rxvt_t *r));
void             rxvt_IM_set_fontset              __PROTO((rxvt_t* r, int idx));
void             rxvt_IM_init_callback            __PROTO((Display *unused __attribute__((unused)), XPointer client_data __attribute__((unused)), XPointer call_data __attribute__((unused))));
void             rxvt_IM_set_status_pos           __PROTO((rxvt_t *r));
rxvt_t         * rxvt_get_r                       __PROTO((void));
/* End prototypes of main.c */


/* Begin prototypes of menubar.c */
#ifdef HAVE_MENUBAR
void             rxvt_menubar_resize              __PROTO((rxvt_t *r));
void             rxvt_menubar_create              __PROTO((rxvt_t *r));
void             rxvt_menubar_clean_exit          __PROTO((rxvt_t *r));
int              rxvt_menubar_hide                __PROTO((rxvt_t *r));
int              rxvt_menubar_show                __PROTO((rxvt_t *r));
void             rxvt_menubar_expose              __PROTO((rxvt_t *r));
void             rxvt_menubar_dispatcher          __PROTO((rxvt_t* r, char* str));
void             rxvt_menubar_control             __PROTO((rxvt_t* r, XButtonEvent *ev));
void             rxvt_menubar_load_file           __PROTO((rxvt_t* r, const char* filename));
int              rxvt_is_menubar_win              __PROTO((rxvt_t* r, Window w));
unsigned short   rxvt_menubar_height              __PROTO((rxvt_t *r));
unsigned short   rxvt_menubar_rheight             __PROTO((rxvt_t *r));
int              rxvt_menubar_visible             __PROTO((rxvt_t* r));
#endif
/* End prototypes of menubar.c */


/* Begin prototypes of misc.c */
char           * rxvt_r_basename                  __PROTO((const char* str));
void             rxvt_print_error                 __PROTO((const char* fmt,...));
int              rxvt_str_match                   __PROTO((const char* s1, const char* s2));
const char*      rxvt_str_skip_space              __PROTO((const char* str));
char*            rxvt_str_trim                    __PROTO((char* str));
int              rxvt_str_escaped                 __PROTO((char* str));
char**           rxvt_splitcommastring            __PROTO((const char* cs));
char*            rxvt_File_find                   __PROTO((const char* file, const char* ext, const char* path));
void             rxvt_draw_shadow                 __PROTO((Display *Xdisplay, Window win, GC gc, unsigned long topShadow, unsigned long botShadow, int x, int y, int w, int h));
void             rxvt_draw_triangle               __PROTO((Display *Xdisplay, Window win, GC gc, unsigned long topShadow, unsigned long botShadow, int x, int y, int w, int type));
/* End prototypes of misc.c */


/* Begin prototypes of netdisp.c */
#ifdef NET_DISPLAY
char           * rxvt_network_display             __PROTO((const char* display));
#endif
/* End prototypes of netdisp.c */


/* Begin prototypes of ptytty.c */
int              rxvt_get_pty                     __PROTO((int *fd_tty, char** ttydev));
int              rxvt_get_tty                     __PROTO((const char* ttydev));
int              rxvt_control_tty                 __PROTO((int fd_tty, const char* ttydev));
/* End prototypes of ptytty.c */


/* Begin prototypes of screen.c */
void             rxvt_draw_string_x11             __PROTO((rxvt_t* r, Window win, GC gc, int x, int y, char* str, int len, int (*func)()));
#ifdef XFT_SUPPORT
void             rxvt_draw_string_xft             __PROTO((rxvt_t* r, XftDraw* win, GC gc, XftColor* fore, int x, int y, char* str, int len, void (*func)()));
#endif
void             rxvt_init_screen                 __PROTO((rxvt_t* r));
void             rxvt_scr_reset                   __PROTO((rxvt_t* r, int page));
void             rxvt_scr_release                 __PROTO((rxvt_t* r, int page));
void             rxvt_scr_poweron                 __PROTO((rxvt_t* r, int page));
void             rxvt_scr_cursor                  __PROTO((rxvt_t* r, int page, int mode));
int              rxvt_scr_change_screen           __PROTO((rxvt_t* r, int page, int scrn));
void             rxvt_scr_color                   __PROTO((rxvt_t* r, int page, unsigned int color, int fgbg));
void             rxvt_scr_rendition               __PROTO((rxvt_t* r, int page, int set, int style));
int              rxvt_scroll_text                 __PROTO((rxvt_t* r, int page, int row1, int row2, int count, int spec));
void             rxvt_scr_add_lines               __PROTO((rxvt_t* r, int page, const unsigned char* str, int nlines, int len));
void             rxvt_scr_backspace               __PROTO((rxvt_t* r, int page));
void             rxvt_scr_tab                     __PROTO((rxvt_t* r, int page, int count));
void             rxvt_scr_backindex               __PROTO((rxvt_t* r, int page));
void             rxvt_scr_forwardindex            __PROTO((rxvt_t* r, int page));
void             rxvt_scr_gotorc                  __PROTO((rxvt_t* r, int page, int row, int col, int relative));
void             rxvt_scr_index                   __PROTO((rxvt_t* r, int page, enum page_dirn direction));
void             rxvt_scr_erase_line              __PROTO((rxvt_t* r, int page, int mode));
void             rxvt_scr_erase_screen            __PROTO((rxvt_t* r, int page, int mode));
void             rxvt_scr_E                       __PROTO((rxvt_t* r, int page));
void             rxvt_scr_insdel_lines            __PROTO((rxvt_t* r, int page, int count, int insdel));
void             rxvt_scr_insdel_chars            __PROTO((rxvt_t* r, int page, int count, int insdel));
void             rxvt_scr_scroll_region           __PROTO((rxvt_t* r, int page, int top, int bot));
void             rxvt_scr_cursor_visible          __PROTO((rxvt_t* r, int page, int mode));
void             rxvt_scr_autowrap                __PROTO((rxvt_t* r, int page, int mode));
void             rxvt_scr_relative_origin         __PROTO((rxvt_t* r, int page, int mode));
void             rxvt_scr_insert_mode             __PROTO((rxvt_t* r, int page, int mode));
void             rxvt_scr_set_tab                 __PROTO((rxvt_t* r, int page, int mode));
void             rxvt_scr_rvideo_mode             __PROTO((rxvt_t* r, int page, int mode));
void             rxvt_scr_report_position         __PROTO((rxvt_t* r, int page));
void             rxvt_scr_charset_choose          __PROTO((rxvt_t* r, int page, int set));
void             rxvt_scr_charset_set             __PROTO((rxvt_t* r, int page, int set, unsigned int ch));
int              rxvt_scr_get_fgcolor             __PROTO((rxvt_t *r));
int              rxvt_scr_get_bgcolor             __PROTO((rxvt_t *r));
void             rxvt_scr_expose                  __PROTO((rxvt_t* r, int page, int x, int y, int width, int height, Bool refresh));
void             rxvt_scr_touch                   __PROTO((rxvt_t* r, int page, Bool refresh));
int              rxvt_scr_move_to                 __PROTO((rxvt_t* r, int page, int y, int len));
int              rxvt_scr_page                    __PROTO((rxvt_t* r, int page, enum page_dirn direction, int nlines));
void             rxvt_scr_bell                    __PROTO((rxvt_t *r));
void             rxvt_scr_printscreen             __PROTO((rxvt_t* r, int page, int fullhist));
void             rxvt_scr_refresh                 __PROTO((rxvt_t* r, int page, unsigned char refresh_type));
void             rxvt_scr_clear                   __PROTO((rxvt_t* r, int page));
void             rxvt_scr_dump                    __PROTO((rxvt_t* r, int page, int fd));
void             rxvt_selection_check             __PROTO((rxvt_t* r, int page, int check_more));
int              rxvt_selection_paste             __PROTO((rxvt_t* r, Window win, Atom prop, Bool delete_prop));
void             rxvt_selection_property          __PROTO((rxvt_t* r, Window win, Atom prop));
void             rxvt_selection_request           __PROTO((rxvt_t* r, int page, Time tm, int x, int y));
void             rxvt_process_selectionclear      __PROTO((rxvt_t* r, int page));
void             rxvt_selection_make              __PROTO((rxvt_t* r, int page, Time tm));
void             rxvt_selection_click             __PROTO((rxvt_t* r, int page, int clicks, int x, int y));
void             rxvt_selection_extend            __PROTO((rxvt_t* r, int page, int x, int y, int flag));
void             rxvt_selection_rotate            __PROTO((rxvt_t* r, int page, int x, int y));
void             rxvt_process_selectionrequest    __PROTO((rxvt_t* r, int page, const XSelectionRequestEvent *rq));
void             rxvt_pixel_position              __PROTO((rxvt_t* r, int *x, int *y));
void             rxvt_setPosition                 __PROTO((rxvt_t* r, XPoint *pos));
/* End prototypes of screen.c */


/* Begin prototypes of scrollbar-next.c */
#ifdef NEXT_SCROLLBAR
void             rxvt_scrollbar_init_next         __PROTO((rxvt_t *r));
void             rxvt_scrollbar_exit_next         __PROTO((rxvt_t *r));
int              rxvt_scrollbar_show_next         __PROTO((rxvt_t* r, int update, int last_top, int last_bot, int scroller_len));
#endif
/* End prototypes of scrollbar-next.c */


/* Begin prototypes of scrollbar-rxvt.c */
#ifdef RXVT_SCROLLBAR
void             rxvt_scrollbar_init_rxvt         __PROTO((rxvt_t *r));
void             rxvt_scrollbar_exit_rxvt         __PROTO((rxvt_t *r));
int              rxvt_scrollbar_show_rxvt         __PROTO((rxvt_t* r, int update __attribute__((unused)), int last_top, int last_bot, int scroller_len));
#endif
/* End prototypes of scrollbar-rxvt.c */


/* Begin prototypes of scrollbar-xterm.c */
#ifdef XTERM_SCROLLBAR
void             rxvt_scrollbar_init_xterm        __PROTO((rxvt_t *r));
void             rxvt_scrollbar_exit_xterm        __PROTO((rxvt_t *r));
int              rxvt_scrollbar_show_xterm        __PROTO((rxvt_t* r, int update __attribute__((unused)), int last_top, int last_bot, int scroller_len));
#endif
/* End prototypes of scrollbar-xterm.c */


/* Begin prototypes of scrollbar-plain.c */
#ifdef PLAIN_SCROLLBAR
void             rxvt_scrollbar_init_plain        __PROTO((rxvt_t *r));
void             rxvt_scrollbar_exit_plain        __PROTO((rxvt_t *r));
int              rxvt_scrollbar_show_plain        __PROTO((rxvt_t* r, int update __attribute__((unused)), int last_top, int last_bot, int scroller_len));
#endif
/* End prototypes of scrollbar-plain.c */


/* Begin prototypes of scrollbar-sgi.c */
#ifdef SGI_SCROLLBAR
void             rxvt_scrollbar_init_sgi          __PROTO((rxvt_t *r));
void             rxvt_scrollbar_exit_sgi          __PROTO((rxvt_t *r));
int              rxvt_scrollbar_show_sgi          __PROTO((rxvt_t* r, int update __attribute__((unused)), int last_top, int last_bot, int scroller_len));
#endif
/* End prototypes of scrollbar-sgi.c */


/* Begin prototypes of scrollbar.c */
#ifdef HAVE_SCROLLBARS
int              rxvt_scrollbar_visible           __PROTO((rxvt_t *r));
void             rxvt_scrollbar_init              __PROTO((rxvt_t *r));
void             rxvt_scrollbar_create            __PROTO((rxvt_t *r));
void             rxvt_scrollbar_clean_exit        __PROTO((rxvt_t *r));
void             rxvt_scrollbar_resize            __PROTO((rxvt_t *r));
int              rxvt_scrollbar_hide              __PROTO((rxvt_t *r));
int              rxvt_scrollbar_show              __PROTO((rxvt_t *r));
int              rxvt_scrollbar_update            __PROTO((rxvt_t* r, int update));
unsigned short   rxvt_scrollbar_width             __PROTO((rxvt_t *r));
unsigned short   rxvt_scrollbar_rwidth            __PROTO((rxvt_t *r));
int              rxvt_is_scrollbar_win            __PROTO((rxvt_t* r, Window w));
#endif
/* End prototypes of scrollbar.c */


/* Begin prototypes of strings.c */
#ifdef HAVE_WCHAR_H
char*            rxvt_wcstombs                    __PROTO((const wchar_t* str, int len));
wchar_t*         rxvt_mbstowcs                    __PROTO((const char* str));
char*            rxvt_wcstoutf8                   __PROTO((const wchar_t* str));
wchar_t*         rxvt_utf8towcs                   __PROTO((const char* str));
#endif	/* HAVE_WCHAR_H */
int              ma_strcasecmp                    __PROTO((const char* s1, const char* s2));
int              ma_strncasecmp                   __PROTO((const char* s1, const char* s2, size_t n));
char*            ma_strcpy                        __PROTO((char* d, const char* s));
char*            ma_strncpy                       __PROTO((char* d, const char* s, size_t len));
int              ma_strcmp                        __PROTO((const char* s1, const char* s2));
int              ma_strncmp                       __PROTO((const char* s1, const char* s2, size_t len));
char*            ma_strcat                        __PROTO((char* s1, const char* s2));
char*            ma_strncat                       __PROTO((char* s1, const char* s2, size_t len));
size_t           ma_strlen                        __PROTO((const char* s));
char*            ma_strdup                        __PROTO((const char* s));
char UNTAINTED * ma_strndup                       __PROTO((const char TAINTED * s, size_t sz));
char*            ma_index                         __PROTO((const char* s, int c));
char*            ma_strchr                        __PROTO((const char* s, int c));
char*            ma_rindex                        __PROTO((const char* s, int c));
char*            ma_strrchr                       __PROTO((const char* s, int c));
void*            ma_memcpy                        __PROTO((void *s1, const void *s2, size_t len));
void*            ma_memmove                       __PROTO((void *d, const void *s, size_t len));
void             ma_bzero                         __PROTO((void *b, size_t len));
void*            ma_memset                        __PROTO((void *p, int c1, size_t len));
/* End prototypes of strings.c */


/* Begin prototypes of xdefaults.c */
void             rxvt_usage                       __PROTO((int type));
int              rxvt_save_options                __PROTO((rxvt_t* r, const char* filename));
void             rxvt_get_options                 __PROTO((rxvt_t* r, int argc, const char* const *argv));
void             rxvt_extract_resources           __PROTO((rxvt_t* r, Display *display __attribute__((unused)), const char* name));
/* End prototypes of xdefaults.c */


#if defined(BACKGROUND_IMAGE) || defined(TRANSPARENT)
/* Begin prototypes of pixmap.c */
int              rxvt_scale_pixmap                __PROTO((rxvt_t* r, int page, const char* geom));
void             rxvt_resize_pixmap               __PROTO((rxvt_t* r, int page));
Pixmap           rxvt_load_bg_pixmap              __PROTO((rxvt_t* r, int page, const char* file));
Pixmap           rxvt_load_pixmap                 __PROTO((rxvt_t* r, const char* file, long* pw, long* ph));
/* End prototypes of pixmap.c */


/* Begin prototypes of jpg.c */
#ifdef USE_JPEG
long             JpegReadFileToPixmap             __PROTO((Display* display, Window window, GC gc, char* filename, Pixmap* pixmap, long* w, long* h));
#endif
/* End prototypes of jpg.c */


/* Begin prototypes of png.c */
#ifdef USE_PNG
long             PngReadFileToPixmap             __PROTO((Display* display, Window window, GC gc, char* filename, Pixmap* pixmap, long* w, long* h));
#endif
/* End prototypes of png.c */
#endif /* BACKGROUND_IMAGE || TRANSPARENT */


/* Begin prototypes of tabbar.c */
void             rxvt_append_page               __PROTO((rxvt_t* r, const char TAINTED * title));
void             rxvt_kill_page                 __PROTO((rxvt_t* r, int page));
void             rxvt_remove_page               __PROTO((rxvt_t* r, int page));
void             rxvt_activate_page             __PROTO((rxvt_t* r, int page));
void             rxvt_tabbar_set_visible_tabs   __PROTO((rxvt_t* r));
void             rxvt_tabbar_set_title          __PROTO((rxvt_t* r, int page, const unsigned char TAINTED * str));
void             rxvt_tabbar_resize             __PROTO((rxvt_t* r));
void             rxvt_tabbar_dispatcher         __PROTO((rxvt_t* r, XButtonEvent* ev));
int              rxvt_tabbar_visible            __PROTO((rxvt_t* r));
void             rxvt_tabbar_expose             __PROTO((rxvt_t* r));
int              rxvt_tabbar_hide               __PROTO((rxvt_t* r));
int              rxvt_tabbar_show               __PROTO((rxvt_t* r));
void             rxvt_tabbar_create             __PROTO((rxvt_t* r));
void             rxvt_tabbar_clean_exit         __PROTO((rxvt_t* r));
unsigned short   rxvt_tabbar_height             __PROTO((rxvt_t* r));
unsigned short   rxvt_tabbar_rheight            __PROTO((rxvt_t* r));
unsigned int     rxvt_tab_width                 __PROTO((rxvt_t* r, const char* str));
int              rxvt_is_tabbar_win             __PROTO((rxvt_t* r, Window w));
void             rxvt_tabbar_draw_buttons       __PROTO((rxvt_t* r));
void             rxvt_tabbar_change_color       __PROTO((rxvt_t* r, int item, const char* color));
void             rxvt_tabbar_highlight_tab      __PROTO((rxvt_t* r, int page));
void             rxvt_tabbar_move_tab           __PROTO((rxvt_t* r, int to_right));
/* Begin prototypes of tabbar.c */


/* Begin prototypes of thai.c */
#ifdef THAI
int              ThaiIsMiddleLineCh             __PROTO((char ch));
int              ThaiPixel2Col                  __PROTO((rxvt_t* r, int page, int x, int y));
int              ThaiCol2Pixel                  __PROTO((rxvt_t* r, int col, char* start));
int              ThaiUpdateMap2                 __PROTO((rxvt_t* r, text_t* stp, text_t* dtp, rend_t* srp, rend_t* drp, char* map, int len));
int              ThaiUpdateMap                  __PROTO((text_t* stp, text_t* dtp, rend_t* srp, rend_t* drp, char* map, int len));
int              Thai_CursorArea                __PROTO((char* stp, char* map, int len, int col));
int              Thai_ColMaxPaint               __PROTO((text_t* str, int len));
#endif	/* THAI */
/* End prototypes of thai.c */


/* Begin prototypes of session.c */
#ifdef HAVE_X11_SM_SMLIB_H
void            rxvt_process_ice_msgs          __PROTO((rxvt_t* r));
void            rxvt_session_init              __PROTO((rxvt_t* r));
void            rxvt_session_exit              __PROTO((rxvt_t* r));
#endif	/* HAVE_X11_SM_SMLIB_H */
/* End prototypes of session.c */

#endif  /* __PROTOS_H__ */
/*----------------------- end-of-file (H source) -----------------------*/
