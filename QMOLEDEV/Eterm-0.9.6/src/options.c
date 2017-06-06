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

static const char cvs_ident[] = "$Id: options.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <X11/keysym.h>

#include "actions.h"
#include "buttons.h"
#include "command.h"
#include "events.h"
#include "font.h"
#include "grkelot.h"
#include "startup.h"
#include "menus.h"
#include "options.h"
#include "pixmap.h"
#include "scrollbar.h"
#include "screen.h"
#include "system.h"
#include "term.h"
#include "windows.h"
#include "defaultfont.h"

#ifndef CONFIG_BUFF
# define CONFIG_BUFF 20480
#endif

static void usage(void);
static void version(void);
static void handle_attribute(char *);
static void *parse_color(char *, void *);
static void *parse_attributes(char *, void *);
static void *parse_toggles(char *, void *);
static void *parse_keyboard(char *, void *);
static void *parse_misc(char *, void *);
static void *parse_imageclasses(char *, void *);
static void *parse_image(char *, void *);
static void *parse_actions(char *, void *);
static void *parse_menu(char *, void *);
static void *parse_menuitem(char *, void *);
static void *parse_bbar(char *, void *);
static void *parse_xim(char *, void *);
static void *parse_multichar(char *, void *);
static void *parse_escreen(char *, void *);

static char *rs_pipe_name = NULL;

#ifdef PIXMAP_SUPPORT
static int rs_shade = 0;
static char *rs_tint = NULL;
#endif
static unsigned long rs_buttonbars = 1;
static char *rs_font_effects = NULL;

#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
static char *rs_bigfont_key = NULL;
static char *rs_smallfont_key = NULL;
#endif
#ifdef MULTI_CHARSET
static char *rs_multichar_encoding = NULL;
#endif
#ifdef GREEK_SUPPORT
static char *rs_greek_keyboard = NULL;
#endif

unsigned long eterm_options = (ETERM_OPTIONS_SCROLLBAR | ETERM_OPTIONS_SELECT_TRAILING_SPACES);
unsigned long vt_options = (VT_OPTIONS_SECONDARY_SCREEN | VT_OPTIONS_OVERSTRIKE_BOLD | VT_OPTIONS_BOLD_BRIGHTENS_FOREGROUND |
                            VT_OPTIONS_BLINK_BRIGHTENS_BACKGROUND | VT_OPTIONS_COLORS_SUPPRESS_BOLD);
unsigned long image_options = 0;
char *theme_dir = NULL, *user_dir = NULL;
char **rs_exec_args = NULL;     /* Args to exec (-e or --exec) */
char *rs_title = NULL;          /* Window title */
char *rs_iconName = NULL;       /* Icon name */
char *rs_geometry = NULL;       /* Geometry string */
int rs_desktop = -1;
char *rs_path = NULL;
int rs_saveLines = SAVELINES;   /* Lines in the scrollback buffer */

#ifdef USE_XIM
char *rs_input_method = NULL;
char *rs_preedit_type = NULL;
#endif
char *rs_name = NULL;

#ifndef NO_BOLDFONT
char *rs_boldFont = NULL;
#endif
#ifdef PRINTPIPE
char *rs_print_pipe = NULL;
#endif
char *rs_cutchars = NULL;
unsigned short rs_min_anchor_size = 0;
char *rs_scrollbar_type = NULL;
unsigned long rs_scrollbar_width = 0;
char *rs_finished_title = NULL;
char *rs_finished_text = NULL;
char *rs_term_name = NULL;

#ifdef PIXMAP_SUPPORT
char *rs_pixmapScale = NULL;
char *rs_icon = NULL;
char *rs_cmod_image = NULL;
char *rs_cmod_red = NULL;
char *rs_cmod_green = NULL;
char *rs_cmod_blue = NULL;
unsigned long rs_cache_size = (unsigned long) -1;

# ifdef BACKGROUND_CYCLING_SUPPORT
char *rs_anim_pixmap_list = NULL;
char **rs_anim_pixmaps = NULL;
time_t rs_anim_delay = 0;
# endif
static char *rs_pixmaps[image_max];
#endif
char *rs_theme = NULL;
char *rs_config_file = NULL;

#ifdef ESCREEN
char *rs_url = NULL;
char *rs_hop = NULL;
int rs_delay = -1;
unsigned char rs_es_dock = BBAR_DOCKED_BOTTOM;
char *rs_es_font = NULL;
#endif
spif_charptr_t rs_beep_command = NULL;
spif_uint32_t rs_opacity = 0xffffffff;
unsigned int rs_line_space = 0;
unsigned int rs_meta_mod = 0, rs_alt_mod = 0, rs_numlock_mod = 0;

#ifdef KEYSYM_ATTRIBUTE
unsigned char *KeySym_map[256]; /* probably mostly empty */
#endif
#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
/* recognized when combined with HOTKEY */
KeySym ks_bigfont = XK_greater;
KeySym ks_smallfont = XK_less;
#endif

/* Eterm options structure */
spifopt_t option_list[] = {
    SPIFOPT_STR_PP('t', "theme", "select a theme", rs_theme),
    SPIFOPT_STR_PP('X', "config-file", "choose an alternate config file", rs_config_file),
    SPIFOPT_STR_PP('d', "display", "X server to connect to", display_name),
#if DEBUG <= 0
    SPIFOPT_INT_LONG_PP("debug", "level of debugging information to show (support not compiled in)", DEBUG_LEVEL),
#elif DEBUG == 1
    SPIFOPT_INT_LONG_PP("debug", "level of debugging information to show (0-1)", DEBUG_LEVEL),
#elif DEBUG == 2
    SPIFOPT_INT_LONG_PP("debug", "level of debugging information to show (0-2)", DEBUG_LEVEL),
#elif DEBUG == 3
    SPIFOPT_INT_LONG_PP("debug", "level of debugging information to show (0-3)", DEBUG_LEVEL),
#elif DEBUG == 4
    SPIFOPT_INT_LONG_PP("debug", "level of debugging information to show (0-4)", DEBUG_LEVEL),
#else
    SPIFOPT_INT_LONG_PP("debug", "level of debugging information to show (0-5)", DEBUG_LEVEL),
#endif
    SPIFOPT_BOOL_LONG_PP("install", "install a private colormap", eterm_options, ETERM_OPTIONS_INSTALL),

    SPIFOPT_ABST_PP('h', "help", "display usage information", usage),
    SPIFOPT_ABST_LONG_PP("version", "display version and configuration information", version),

/* =======[ Color options ]======= */
    SPIFOPT_BOOL('r', "reverse-video", "reverse video", vt_options, VT_OPTIONS_REVERSE_VIDEO),
    SPIFOPT_STR('b', "background-color", "background color", rs_color[bgColor]),
    SPIFOPT_STR('f', "foreground-color", "foreground color", rs_color[fgColor]),
    SPIFOPT_STR_LONG("color0", "color 0", rs_color[minColor]),
    SPIFOPT_STR_LONG("color1", "color 1", rs_color[minColor + 1]),
    SPIFOPT_STR_LONG("color2", "color 2", rs_color[minColor + 2]),
    SPIFOPT_STR_LONG("color3", "color 3", rs_color[minColor + 3]),
    SPIFOPT_STR_LONG("color4", "color 4", rs_color[minColor + 4]),
    SPIFOPT_STR_LONG("color5", "color 5", rs_color[minColor + 5]),
    SPIFOPT_STR_LONG("color6", "color 6", rs_color[minColor + 6]),
    SPIFOPT_STR_LONG("color7", "color 7", rs_color[minColor + 7]),
    SPIFOPT_STR_LONG("color8", "color 8", rs_color[minBright]),
    SPIFOPT_STR_LONG("color9", "color 9", rs_color[minBright + 1]),
    SPIFOPT_STR_LONG("color10", "color 10", rs_color[minBright + 2]),
    SPIFOPT_STR_LONG("color11", "color 11", rs_color[minBright + 3]),
    SPIFOPT_STR_LONG("color12", "color 12", rs_color[minBright + 4]),
    SPIFOPT_STR_LONG("color13", "color 13", rs_color[minBright + 5]),
    SPIFOPT_STR_LONG("color14", "color 14", rs_color[minBright + 6]),
    SPIFOPT_STR_LONG("color15", "color 15", rs_color[minBright + 7]),
#ifndef NO_BOLDUNDERLINE
    SPIFOPT_STR_LONG("colorBD", "bold color", rs_color[colorBD]),
    SPIFOPT_STR_LONG("colorUL", "underline color", rs_color[colorUL]),
#endif /* NO_BOLDUNDERLINE */
    SPIFOPT_STR_LONG("pointer-color", "mouse pointer color", rs_color[pointerColor]),
#ifndef NO_CURSORCOLOR
    SPIFOPT_STR('c', "cursor-color", "cursor color", rs_color[cursorColor]),
    SPIFOPT_STR_LONG("cursor-text-color", "cursor text color", rs_color[cursorColor2]),
#endif /* NO_CURSORCOLOR */

    /* =======[ X11 options ]======= */
    SPIFOPT_STR('g', "geometry", "WxH+X+Y = size and position", rs_geometry),
    SPIFOPT_BOOL('i', "iconic", "start iconified", eterm_options, ETERM_OPTIONS_ICONIC),
    SPIFOPT_STR('n', "name", "client instance, icon, and title strings", rs_name),
    SPIFOPT_STR('T', "title", "title string", rs_title),
    SPIFOPT_STR_LONG("icon-name", "icon name", rs_iconName),
    SPIFOPT_STR('B', "scrollbar-type", "choose the scrollbar type (motif, next, xterm)", rs_scrollbar_type),
    SPIFOPT_INT_LONG("scrollbar-width", "choose the width (in pixels) of the scrollbar", rs_scrollbar_width),
    SPIFOPT_INT('D', "desktop", "desktop to start on (requires compliant WM)", rs_desktop),
    SPIFOPT_INT_LONG("line-space", "number of extra dots between lines", rs_line_space),
#ifndef NO_BOLDFONT
    SPIFOPT_STR_LONG("bold-font", "bold text font", rs_boldFont),
#endif
    SPIFOPT_STR('F', "font", "normal text font", rs_font[0]),
    SPIFOPT_INT_LONG("default-font-index", "set the index of the default font", def_font_idx),
    SPIFOPT_STR_LONG("font1", "font 1", rs_font[1]),
    SPIFOPT_STR_LONG("font2", "font 2", rs_font[2]),
    SPIFOPT_STR_LONG("font3", "font 3", rs_font[3]),
    SPIFOPT_STR_LONG("font4", "font 4", rs_font[4]),
    SPIFOPT_BOOL_LONG("proportional", "toggle proportional font optimizations", vt_options, VT_OPTIONS_PROPORTIONAL),
    SPIFOPT_STR_LONG("font-fx", "specify font effects for the terminal fonts", rs_font_effects),

    /* =======[ Pixmap options ]======= */
#ifdef PIXMAP_SUPPORT
    SPIFOPT_STR('P', "background-pixmap", "background pixmap", rs_pixmaps[image_bg]),
    SPIFOPT_STR('I', "icon", "icon pixmap", rs_icon),
    SPIFOPT_STR_LONG("up-arrow-pixmap", "up arrow pixmap", rs_pixmaps[image_up]),
    SPIFOPT_STR_LONG("down-arrow-pixmap", "down arrow pixmap", rs_pixmaps[image_down]),
    SPIFOPT_STR_LONG("trough-pixmap", "scrollbar background (trough) pixmap", rs_pixmaps[image_sb]),
    SPIFOPT_STR_LONG("anchor-pixmap", "scrollbar anchor pixmap", rs_pixmaps[image_sa]),
    SPIFOPT_STR_LONG("menu-pixmap", "menu pixmap", rs_pixmaps[image_menu]),
    SPIFOPT_INT('o', "opacity", "window opacity (0-255; requires X COMPOSITE extension)", rs_opacity),
    SPIFOPT_BOOL('O', "trans", "creates a pseudo-transparent Eterm", image_options, IMAGE_OPTIONS_TRANS),
    SPIFOPT_BOOL('0', "itrans", "use immotile-optimized transparency", image_options, IMAGE_OPTIONS_ITRANS),
    SPIFOPT_BOOL_LONG("viewport-mode", "use viewport mode for the background image", image_options, IMAGE_OPTIONS_VIEWPORT),
    SPIFOPT_INT_LONG("shade", "old-style shade percentage (deprecated)", rs_shade),
    SPIFOPT_STR_LONG("tint", "old-style tint mask (deprecated)", rs_tint),
    SPIFOPT_STR_LONG("cmod", "image color modifier (\"brightness contrast gamma\")", rs_cmod_image),
    SPIFOPT_STR_LONG("cmod-red", "red-only color modifier (\"brightness contrast gamma\")", rs_cmod_red),
    SPIFOPT_STR_LONG("cmod-green", "green-only color modifier (\"brightness contrast gamma\")", rs_cmod_green),
    SPIFOPT_STR_LONG("cmod-blue", "blue-only color modifier (\"brightness contrast gamma\")", rs_cmod_blue),
    SPIFOPT_STR('p', "path", "pixmap file search path", rs_path),
    SPIFOPT_INT_LONG("cache", "set Imlib2 image/pixmap cache size", rs_cache_size),
# ifdef BACKGROUND_CYCLING_SUPPORT
    SPIFOPT_STR('N', "anim", "a delay and list of pixmaps for cycling", rs_anim_pixmap_list),
# endif /* BACKGROUND_CYCLING_SUPPORT */
#endif /* PIXMAP_SUPPORT */

    /* =======[ Kanji options ]======= */
#ifdef MULTI_CHARSET
    SPIFOPT_STR('M', "mfont", "normal text multichar font", rs_mfont[0]),
    SPIFOPT_STR_LONG("mfont1", "multichar font 1", rs_mfont[1]),
    SPIFOPT_STR_LONG("mfont2", "multichar font 2", rs_mfont[2]),
    SPIFOPT_STR_LONG("mfont3", "multichar font 3", rs_mfont[3]),
    SPIFOPT_STR_LONG("mfont4", "multichar font 4", rs_mfont[4]),
    SPIFOPT_STR_LONG("mencoding", "multichar encoding mode (eucj/sjis/euckr/big5/gb)", rs_multichar_encoding),
#endif /* MULTI_CHARSET */
#ifdef USE_XIM
    SPIFOPT_STR_LONG("input-method", "XIM input method", rs_input_method),
    SPIFOPT_STR_LONG("preedit-type", "XIM preedit type", rs_preedit_type),
#endif

    /* =======[ Toggles ]======= */
    SPIFOPT_BOOL('l', "login-shell", "login shell, prepend - to shell name", eterm_options, ETERM_OPTIONS_LOGIN_SHELL),
    SPIFOPT_BOOL('s', "scrollbar", "display scrollbar", eterm_options, ETERM_OPTIONS_SCROLLBAR),
    SPIFOPT_BOOL('u', "utmp-logging", "make a utmp entry", eterm_options, ETERM_OPTIONS_WRITE_UTMP),
    SPIFOPT_BOOL('v', "visual-bell", "visual bell", vt_options, VT_OPTIONS_VISUAL_BELL),
    SPIFOPT_BOOL('H', "home-on-output", "jump to bottom on output", vt_options, VT_OPTIONS_HOME_ON_OUTPUT),
    SPIFOPT_BOOL_LONG("home-on-input", "jump to bottom on input", vt_options, VT_OPTIONS_HOME_ON_INPUT),
    SPIFOPT_BOOL('q', "no-input", "configure for output only", eterm_options, ETERM_OPTIONS_NO_INPUT),
    SPIFOPT_BOOL_LONG("scrollbar-right", "display the scrollbar on the right", eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT),
    SPIFOPT_BOOL_LONG("scrollbar-floating", "display the scrollbar with no trough", eterm_options,
                      ETERM_OPTIONS_SCROLLBAR_FLOATING),
    SPIFOPT_BOOL_LONG("scrollbar-popup", "popup the scrollbar only when focused", eterm_options, ETERM_OPTIONS_SCROLLBAR_POPUP),
    SPIFOPT_BOOL('x', "borderless", "force Eterm to have no borders", eterm_options, ETERM_OPTIONS_BORDERLESS),
    SPIFOPT_BOOL_LONG("overstrike-bold", "simulate bold by overstriking characters", vt_options, VT_OPTIONS_OVERSTRIKE_BOLD),
    SPIFOPT_BOOL_LONG("bold-brightens-foreground", "\"bold\" attribute brightens foreground color", vt_options,
                      VT_OPTIONS_BOLD_BRIGHTENS_FOREGROUND),
    SPIFOPT_BOOL_LONG("blink-brightens-background", "\"blink\" attribute brightens background color", vt_options,
                      VT_OPTIONS_BLINK_BRIGHTENS_BACKGROUND),
    SPIFOPT_BOOL_LONG("colors-suppress-bold", "do not make ANSI colors 0-16 bold", vt_options, VT_OPTIONS_COLORS_SUPPRESS_BOLD),
    SPIFOPT_BOOL('S', "sticky", "start window sticky", eterm_options, ETERM_OPTIONS_STICKY),
#ifndef NO_MAPALERT
# ifdef MAPALERT_OPTION
    SPIFOPT_BOOL('m', "map-alert", "uniconify on beep", vt_options, VT_OPTIONS_MAP_ALERT),
# endif
#endif
    SPIFOPT_BOOL_LONG("urg-alert", "set urgent hint on beep", vt_options, VT_OPTIONS_URG_ALERT),
#ifdef META8_OPTION
    SPIFOPT_BOOL('8', "meta-8", "Meta key toggles 8-bit", vt_options, VT_OPTIONS_META8),
#endif
    SPIFOPT_BOOL_LONG("double-buffer", "reduce exposes using double-buffering (and more memory)", eterm_options,
                      ETERM_OPTIONS_DOUBLE_BUFFER),
    SPIFOPT_BOOL_LONG("no-cursor", "disable the text cursor", eterm_options, ETERM_OPTIONS_NO_CURSOR),
    SPIFOPT_BOOL_LONG("pause", "pause after the child process exits", eterm_options, ETERM_OPTIONS_PAUSE),
    SPIFOPT_BOOL_LONG("xterm-select", "duplicate xterm's selection behavior", eterm_options, ETERM_OPTIONS_XTERM_SELECT),
    SPIFOPT_BOOL_LONG("select-line", "triple-click selects whole line", eterm_options, ETERM_OPTIONS_SELECT_WHOLE_LINE),
    SPIFOPT_BOOL_LONG("select-trailing-spaces", "do not skip trailing spaces when selecting", eterm_options,
                      ETERM_OPTIONS_SELECT_TRAILING_SPACES),
    SPIFOPT_BOOL_LONG("report-as-keysyms", "report special keys as keysyms", vt_options, VT_OPTIONS_REPORT_AS_KEYSYMS),
    SPIFOPT_BOOL_LONG("buttonbar", "toggle the display of all buttonbars", rs_buttonbars, BBAR_FORCE_TOGGLE),
    SPIFOPT_BOOL_LONG("resize-gravity", "toggle gravitation to nearest corner on resize", eterm_options,
                      ETERM_OPTIONS_RESIZE_GRAVITY),
    SPIFOPT_BOOL_LONG("secondary-screen", "toggle use of secondary screen", vt_options, VT_OPTIONS_SECONDARY_SCREEN),

/* =======[ Keyboard options ]======= */
#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
    SPIFOPT_STR_LONG("big-font-key", "keysym for font size increase", rs_bigfont_key),
    SPIFOPT_STR_LONG("small-font-key", "keysym for font size decrease", rs_smallfont_key),
#endif
    SPIFOPT_INT_LONG("meta-mod", "modifier to interpret as the Meta key", rs_meta_mod),
    SPIFOPT_INT_LONG("alt-mod", "modifier to interpret as the Alt key", rs_alt_mod),
    SPIFOPT_INT_LONG("numlock-mod", "modifier to interpret as the NumLock key", rs_numlock_mod),
#ifdef GREEK_SUPPORT
    SPIFOPT_STR_LONG("greek-keyboard", "greek keyboard mapping (iso or ibm)", rs_greek_keyboard),
#endif
    SPIFOPT_BOOL_LONG("app-keypad", "application keypad mode", PrivateModes, PrivMode_aplKP),
    SPIFOPT_BOOL_LONG("app-cursor", "application cursor key mode", PrivateModes, PrivMode_aplCUR),

    /* =======[ Misc options ]======= */
    SPIFOPT_INT('L', "save-lines", "lines to save in scrollback buffer", rs_saveLines),
    SPIFOPT_INT_LONG("min-anchor-size", "minimum size of the scrollbar anchor", rs_min_anchor_size),
#ifdef BORDER_WIDTH_OPTION
    SPIFOPT_INT('w', "border-width", "term window border width", TermWin.internalBorder),
#endif
#ifdef PRINTPIPE
    SPIFOPT_STR_LONG("print-pipe", "print command", rs_print_pipe),
#endif
#ifdef CUTCHAR_OPTION
    SPIFOPT_STR_LONG("cut-chars", "seperators for double-click selection", rs_cutchars),
#endif /* CUTCHAR_OPTION */
    SPIFOPT_STR_LONG("finished-title", "post-termination window title text", rs_finished_title),
    SPIFOPT_STR_LONG("finished-text", "post-termination terminal text", rs_finished_text),
    SPIFOPT_STR_LONG("term-name", "value to use for setting $TERM", rs_term_name),
    SPIFOPT_STR_LONG("pipe-name", "filename of console pipe to emulate -C", rs_pipe_name),
    SPIFOPT_STR_LONG("beep-command", "command to run instead of normal beep", rs_beep_command),
#ifdef ESCREEN
    SPIFOPT_STR('U', "url", "a URL pointing to a screen session to pick up", rs_url),
    SPIFOPT_STR('Z', "firewall", "connect session via forwarded port", rs_hop),
    SPIFOPT_INT('z', "delay", "initial delay in seconds", rs_delay),
#endif
    SPIFOPT_ABST('a', "attribute", "parse an attribute in the specified context", handle_attribute),
    SPIFOPT_BOOL('C', "console", "grab console messages", vt_options, VT_OPTIONS_CONSOLE),
    SPIFOPT_ARGS('e', "exec", "execute a command rather than a shell", rs_exec_args)
};

/* Print usage information */
#define INDENT "5"
static void
usage(void)
{
    printf("Eterm Enlightened Terminal Emulator for the X Window System\n");
    printf("Copyright (c) 1997-2009, " AUTHORS "\n\n");

    printf("OPTION types:\n");
    printf("  (bool) -- Boolean option ('1', 'on', 'yes', or 'true' to activate, '0', 'off', 'no', or 'false' to deactivate)\n");
    printf("  (int)  -- Integer option (any signed number of reasonable value, usually in decimal/octal/hex)\n");
    printf("  (str)  -- String option (be sure to quote strings if needed to avoid shell expansion)\n");
    printf("  (strs) -- Stringlist option (quoting strings will be split on whitespace)\n\n");

    printf("NOTE:  Long options can be separated from their values by an equal sign ('='), or you can\n");
    printf("       pass the value as the following argument on the command line (e.g., '--scrollbar 0'\n");
    printf("       or '--scrollbar=0').  Short options must have their values passed after them on the\n");
    printf("       command line, and in the case of boolean short options, cannot have values (they\n");
    printf("       default to true) (e.g., '-F shine' or '-s').\n");

    printf("\nPlease consult the Eterm(1) man page for more detailed\n");
    printf("information on command line options.\n\n");

    spifopt_usage();
}

/* Print version and configuration information */
static void
version(void)
{

    printf("Eterm " VERSION "\n");
    printf("Copyright (c) 1997-2009, " AUTHORS "\n\n");

    printf("Build info:\n");
    printf("    Built on " BUILD_DATE "\n");
    printf("    " ACTIONS_IDENT "\n"
           "    " BUTTONS_IDENT "\n"
           "    " COMMAND_IDENT "\n"
           "    " DRAW_IDENT "\n"
           "    " E_IDENT "\n"
           "    " EVENTS_IDENT "\n"
           "    " FONT_IDENT "\n"
           "    " GRKELOT_IDENT "\n"
           "    " MAIN_IDENT "\n"
           "    " MENUS_IDENT "\n"
           "    " MISC_IDENT "\n"
           "    " OPTIONS_IDENT "\n"
           "    " PIXMAP_IDENT "\n"
           "    " SCREEN_IDENT "\n"
           "    " SCROLLBAR_IDENT "\n"
           "    " STARTUP_IDENT "\n"
           "    " SYSTEM_IDENT "\n"
           "    " TERM_IDENT "\n" "    " TIMER_IDENT "\n" "    " UTMP_IDENT "\n" "    " WINDOWS_IDENT "\n" "\n");

    printf("Debugging configuration:  ");
#ifdef DEBUG
    printf("DEBUG=%d", DEBUG);
#else
    printf("-DEBUG");
#endif

#if DEBUG >= DEBUG_SCREEN
    printf(" +DEBUG_SCREEN");
#endif
#if DEBUG >= DEBUG_CMD
    printf(" +DEBUG_CMD");
#endif
#if DEBUG >= DEBUG_TTY
    printf(" +DEBUG_TTY");
#endif
#if DEBUG >= DEBUG_SELECTION
    printf(" +DEBUG_SELECTION");
#endif
#if DEBUG >= DEBUG_UTMP
    printf(" +DEBUG_UTMP");
#endif
#if DEBUG >= DEBUG_OPTIONS
    printf(" +DEBUG_OPTIONS");
#endif
#if DEBUG >= DEBUG_IMLIB
    printf(" +DEBUG_IMLIB");
#endif
#if DEBUG >= DEBUG_PIXMAP
    printf(" +DEBUG_PIXMAP");
#endif
#if DEBUG >= DEBUG_EVENTS
    printf(" +DEBUG_EVENTS");
#endif
#if DEBUG >= DEBUG_MEM
    printf(" +DEBUG_MEM");
#endif
#if DEBUG >= DEBUG_X11
    printf(" +DEBUG_X11");
#endif
#if DEBUG >= DEBUG_SCROLLBAR
    printf(" +DEBUG_SCROLLBAR");
#endif
#if DEBUG >= DEBUG_MENU
    printf(" +DEBUG_MENU");
#endif
#if DEBUG >= DEBUG_TTYMODE
    printf(" +DEBUG_TTYMODE");
#endif
#if DEBUG >= DEBUG_COLORS
    printf(" +DEBUG_COLORS");
#endif
#if DEBUG >= DEBUG_X
    printf(" +DEBUG_X");
#endif

    printf("\n\nCompile-time toggles: ");

#ifdef PROFILE
    printf(" +PROFILE");
#else
    printf(" -PROFILE");
#endif
#ifdef PROFILE_SCREEN
    printf(" +PROFILE_SCREEN");
#else
    printf(" -PROFILE_SCREEN");
#endif
#ifdef PROFILE_X_EVENTS
    printf(" +PROFILE_X_EVENTS");
#else
    printf(" -PROFILE_X_EVENTS");
#endif
#ifdef COUNT_X_EVENTS
    printf(" +COUNT_X_EVENTS");
#else
    printf(" -COUNT_X_EVENTS");
#endif
#ifdef OPTIMIZE_HACKS
    printf(" +OPTIMIZE_HACKS");
#else
    printf(" -OPTIMIZE_HACKS");
#endif
#ifdef PIXMAP_SUPPORT
    printf(" +PIXMAP_SUPPORT");
#else
    printf(" -PIXMAP_SUPPORT");
#endif
#ifdef PIXMAP_OFFSET
    printf(" +PIXMAP_OFFSET");
#else
    printf(" -PIXMAP_OFFSET");
#endif
#ifdef BACKGROUND_CYCLING_SUPPORT
    printf(" +BACKGROUND_CYCLING_SUPPORT");
#else
    printf(" -BACKGROUND_CYCLING_SUPPORT");
#endif
#ifdef USE_EFFECTS
    printf(" +USE_EFFECTS");
#else
    printf(" -USE_EFFECTS");
#endif
#ifdef NO_CURSORCOLOR
    printf(" +NO_CURSORCOLOR");
#else
    printf(" -NO_CURSORCOLOR");
#endif
#ifdef NO_BOLDUNDERLINE
    printf(" +NO_BOLDUNDERLINE");
#else
    printf(" -NO_BOLDUNDERLINE");
#endif
#ifdef NO_BOLDFONT
    printf(" +NO_BOLDFONT");
#else
    printf(" -NO_BOLDFONT");
#endif
#ifdef NO_SECONDARY_SCREEN
    printf(" +NO_SECONDARY_SCREEN");
#else
    printf(" -NO_SECONDARY_SCREEN");
#endif
#ifdef FORCE_CLEAR_CHARS
    printf(" +FORCE_CLEAR_CHARS");
#else
    printf(" -FORCE_CLEAR_CHARS");
#endif
#ifdef PREFER_24BIT
    printf(" +PREFER_24BIT");
#else
    printf(" -PREFER_24BIT");
#endif
#ifdef OFFIX_DND
    printf(" +OFFIX_DND");
#else
    printf(" -OFFIX_DND");
#endif
#ifdef BORDER_WIDTH_OPTION
    printf(" +BORDER_WIDTH_OPTION");
#else
    printf(" -BORDER_WIDTH_OPTION");
#endif
#ifdef NO_DELETE_KEY
    printf(" +NO_DELETE_KEY");
#else
    printf(" -NO_DELETE_KEY");
#endif
#ifdef FORCE_BACKSPACE
    printf(" +FORCE_BACKSPACE");
#else
    printf(" -FORCE_BACKSPACE");
#endif
#ifdef FORCE_DELETE
    printf(" +FORCE_DELETE");
#else
    printf(" -FORCE_DELETE");
#endif
#ifdef HOTKEY_CTRL
    printf(" +HOTKEY_CTRL");
#else
    printf(" -HOTKEY_CTRL");
#endif
#ifdef HOTKEY_META
    printf(" +HOTKEY_META");
#else
    printf(" -HOTKEY_META");
#endif
#ifdef LINUX_KEYS
    printf(" +LINUX_KEYS");
#else
    printf(" -LINUX_KEYS");
#endif
#ifdef KEYSYM_ATTRIBUTE
    printf(" +KEYSYM_ATTRIBUTE");
#else
    printf(" -KEYSYM_ATTRIBUTE");
#endif
#ifdef USE_XIM
    printf(" +XIM");
#else
    printf(" -XIM");
#endif
#ifdef UNSHIFTED_SCROLLKEYS
    printf(" +UNSHIFTED_SCROLLKEYS");
#else
    printf(" -UNSHIFTED_SCROLLKEYS");
#endif
#ifdef NO_SCROLLBAR_REPORT
    printf(" +NO_SCROLLBAR_REPORT");
#else
    printf(" -NO_SCROLLBAR_REPORT");
#endif
#ifdef CUTCHAR_OPTION
    printf(" +CUTCHAR_OPTION");
#else
    printf(" -CUTCHAR_OPTION");
#endif
#ifdef MOUSE_REPORT_DOUBLECLICK
    printf(" +MOUSE_REPORT_DOUBLECLICK");
#else
    printf(" -MOUSE_REPORT_DOUBLECLICK");
#endif
#ifdef XTERM_SCROLLBAR
    printf(" +XTERM_SCROLLBAR");
#else
    printf(" -XTERM_SCROLLBAR");
#endif
#ifdef MOTIF_SCROLLBAR
    printf(" +MOTIF_SCROLLBAR");
#else
    printf(" -MOTIF_SCROLLBAR");
#endif
#ifdef NEXT_SCROLLBAR
    printf(" +NEXT_SCROLLBAR");
#else
    printf(" -NEXT_SCROLLBAR");
#endif
#ifdef SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
    printf(" +SCROLLBAR_BUTTON_CONTINUAL_SCROLLING");
#else
    printf(" -SCROLLBAR_BUTTON_CONTINUAL_SCROLLING");
#endif
#ifdef META8_OPTION
    printf(" +META8_OPTION");
#else
    printf(" -META8_OPTION");
#endif
#ifdef GREEK_SUPPORT
    printf(" +GREEK_SUPPORT");
#else
    printf(" -GREEK_SUPPORT");
#endif
#ifdef MULTI_CHARSET
    printf(" +MULTI_CHARSET");
#else
    printf(" -MULTI_CHARSET");
#endif
#ifdef ENABLE_DISPLAY_ANSWER
    printf(" +ENABLE_DISPLAY_ANSWER");
#else
    printf(" -ENABLE_DISPLAY_ANSWER");
#endif
#ifdef NO_VT100_ANS
    printf(" +NO_VT100_ANS");
#else
    printf(" -NO_VT100_ANS");
#endif
#ifdef XTERM_COLOR_CHANGE
    printf(" +XTERM_COLOR_CHANGE");
#else
    printf(" -XTERM_COLOR_CHANGE");
#endif
#ifdef DEFINE_XTERM_COLOR
    printf(" +DEFINE_XTERM_COLOR");
#else
    printf(" -DEFINE_XTERM_COLOR");
#endif
#ifdef NO_MAPALERT
    printf(" +NO_MAPALERT");
#else
    printf(" -NO_MAPALERT");
#endif
#ifdef MAPALERT_OPTION
    printf(" +MAPALERT_OPTION");
#else
    printf(" -MAPALERT_OPTION");
#endif
#ifdef UTMP_SUPPORT
    printf(" +UTMP_SUPPORT");
#else
    printf(" -UTMP_SUPPORT");
#endif
#ifdef HAVE_SAVED_UIDS
    printf(" +HAVE_SAVED_UIDS");
#else
    printf(" -HAVE_SAVED_UIDS");
#endif
#ifdef ALLOW_BACKQUOTE_EXEC
    printf(" +ALLOW_BACKQUOTE_EXEC");
#else
    printf(" -ALLOW_BACKQUOTE_EXEC");
#endif
#ifdef WARN_OLDER
    printf(" +WARN_OLDER");
#else
    printf(" -WARN_OLDER");
#endif
#ifdef ESCREEN
    printf(" +ESCREEN");
#else
    printf(" -ESCREEN");
#endif

    printf("\n\nCompile-time definitions:\n");

#ifdef PATH_ENV
    printf(" PATH_ENV=\"%s\"\n", safe_print_string(PATH_ENV, sizeof(PATH_ENV) - 1));
#else
    printf(" -PATH_ENV\n");
#endif
#ifdef REFRESH_PERIOD
    printf(" REFRESH_PERIOD=%d\n", REFRESH_PERIOD);
#else
    printf(" -REFRESH_PERIOD\n");
#endif
#ifdef PRINTPIPE
    printf(" PRINTPIPE=\"%s\"\n", safe_print_string(PRINTPIPE, sizeof(PRINTPIPE) - 1));
#else
    printf(" -PRINTPIPE\n");
#endif
#ifdef KS_DELETE
    printf(" KS_DELETE=\"%s\"\n", safe_print_string(KS_DELETE, sizeof(KS_DELETE) - 1));
#else
    printf(" -KS_DELETE\n");
#endif
#ifdef SAVELINES
    printf(" SAVELINES=%d\n", SAVELINES);
#else
    printf(" -SAVELINES\n");
#endif
#ifdef CUTCHARS
    printf(" CUTCHARS=\"%s\"\n", safe_print_string(CUTCHARS, sizeof(CUTCHARS) - 1));
#else
    printf(" -CUTCHARS\n");
#endif
#ifdef MULTICLICK_TIME
    printf(" MULTICLICK_TIME=%d\n", MULTICLICK_TIME);
#else
    printf(" -MULTICLICK_TIME\n");
#endif
#ifdef SCROLLBAR_DEFAULT_TYPE
    printf(" SCROLLBAR_DEFAULT_TYPE=%d\n", SCROLLBAR_DEFAULT_TYPE);
#else
    printf(" -SCROLLBAR_DEFAULT_TYPE\n");
#endif
#ifdef SB_WIDTH
    printf(" SB_WIDTH=%d\n", SB_WIDTH);
#else
    printf(" -SB_WIDTH\n");
#endif
#ifdef SCROLLBAR_INITIAL_DELAY
    printf(" SCROLLBAR_INITIAL_DELAY=%d\n", SCROLLBAR_INITIAL_DELAY);
#else
    printf(" -SCROLLBAR_INITIAL_DELAY\n");
#endif
#ifdef SCROLLBAR_CONTINUOUS_DELAY
    printf(" SCROLLBAR_CONTINUOUS_DELAY=%d\n", SCROLLBAR_CONTINUOUS_DELAY);
#else
    printf(" -SCROLLBAR_CONTINUOUS_DELAY\n");
#endif
#ifdef ESCZ_ANSWER
    printf(" ESCZ_ANSWER=\"%s\"\n", safe_print_string(ESCZ_ANSWER, sizeof(ESCZ_ANSWER) - 1));
#else
    printf(" -ESCZ_ANSWER\n");
#endif
#ifdef PTY_GRP_NAME
    printf(" PTY_GRP_NAME=\"%s\"\n", PTY_GRP_NAME);
#else
    printf(" -PTY_GRP_NAME\n");
#endif
#ifdef CONFIG_SEARCH_PATH
    printf(" CONFIG_SEARCH_PATH=\"%s\"\n", CONFIG_SEARCH_PATH);
#else
    printf(" -CONFIG_SEARCH_PATH\n");
#endif
#ifdef THEME_CFG
    printf(" THEME_CFG=\"%s\"\n", THEME_CFG);
#else
    printf(" -THEME_CFG\n");
#endif
#ifdef USER_CFG
    printf(" USER_CFG=\"%s\"\n", USER_CFG);
#else
    printf(" -USER_CFG\n");
#endif

    printf("\n");
    exit(EXIT_SUCCESS);
}

static void
handle_attribute(char *val_ptr)
{
    spifconf_parse_line(NULL, val_ptr);
}

/* The config file parsers.  Each function handles a given context. */
static void *
parse_color(char *buff, void *state)
{
    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }
    if (!BEG_STRCASECMP(buff, "foreground ")) {
        RESET_AND_ASSIGN(rs_color[fgColor], spiftool_get_word(2, buff));
    } else if (!BEG_STRCASECMP(buff, "background ")) {
        RESET_AND_ASSIGN(rs_color[bgColor], spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "cursor ")) {

#ifndef NO_CURSORCOLOR
        RESET_AND_ASSIGN(rs_color[cursorColor], spiftool_get_word(2, buff));
#else
        libast_print_warning("Support for the cursor attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "cursor_text ")) {
#ifndef NO_CURSORCOLOR
        RESET_AND_ASSIGN(rs_color[cursorColor2], spiftool_get_word(2, buff));
#else
        libast_print_warning("Support for the cursor_text attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "pointer ")) {
        RESET_AND_ASSIGN(rs_color[pointerColor], spiftool_get_word(2, buff));

#ifdef ESCREEN
    } else if (!BEG_STRCASECMP(buff, "es_current ")) {
        RESET_AND_ASSIGN(rs_color[ES_COLOR_CURRENT], spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "es_active ")) {
        RESET_AND_ASSIGN(rs_color[ES_COLOR_ACTIVE], spiftool_get_word(2, buff));
#endif

    } else if (!BEG_STRCASECMP(buff, "video ")) {

        char *tmp = spiftool_get_pword(2, buff);

        if (!BEG_STRCASECMP(tmp, "reverse")) {
            BITFIELD_SET(vt_options, VT_OPTIONS_REVERSE_VIDEO);
        } else if (BEG_STRCASECMP(tmp, "normal")) {
            libast_print_error("Parse error in file %s, line %lu:  Invalid value \"%s\" for attribute video\n",
                        file_peek_path(), file_peek_line(), tmp);
        }
    } else if (!BEG_STRCASECMP(buff, "color ")) {

        char *tmp = 0, *r1, *g1, *b1;
        unsigned int n, r, g, b, index = 0;

        n = spiftool_num_words(buff);
        if (n < 3) {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter list \"%s\" for \n"
                        "attribute color", file_peek_path(), file_peek_line(), NONULL(tmp));
            return NULL;
        }
        tmp = spiftool_get_pword(2, buff);
        r1 = spiftool_get_pword(3, buff);
        if (!isdigit(*r1)) {
            if (isdigit(*tmp)) {
                n = strtoul(tmp, (char **) NULL, 0);
                if (n <= 7) {
                    index = minColor + n;
                } else if (n >= 8 && n <= 15) {
                    index = minBright + n - 8;
                }
                RESET_AND_ASSIGN(rs_color[index], spiftool_get_word(1, r1));
                return NULL;
            } else {
                if (!BEG_STRCASECMP(tmp, "bd ")) {
#ifndef NO_BOLDUNDERLINE
                    RESET_AND_ASSIGN(rs_color[colorBD], spiftool_get_word(1, r1));
#else
                    libast_print_warning("Support for the color bd attribute was not compiled in, ignoring\n");
#endif
                    return NULL;
                } else if (!BEG_STRCASECMP(tmp, "ul ")) {
#ifndef NO_BOLDUNDERLINE
                    RESET_AND_ASSIGN(rs_color[colorUL], spiftool_get_word(1, r1));
#else
                    libast_print_warning("Support for the color ul attribute was not compiled in, ignoring\n");
#endif
                    return NULL;
                } else {
                    tmp = spiftool_get_word(1, tmp);
                    libast_print_error("Parse error in file %s, line %lu:  Invalid color index \"%s\"\n",
                                file_peek_path(), file_peek_line(), NONULL(tmp));
                    FREE(tmp);
                }
            }
        }
        if (n != 5) {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter list \"%s\" for \n"
                        "attribute color", file_peek_path(), file_peek_line(), NONULL(tmp));
            return NULL;
        }
        g1 = spiftool_get_pword(4, buff);
        b1 = spiftool_get_pword(5, buff);
        if (isdigit(*tmp)) {
            n = strtoul(tmp, (char **) NULL, 0);
            r = strtoul(r1, (char **) NULL, 0);
            g = strtoul(g1, (char **) NULL, 0);
            b = strtoul(b1, (char **) NULL, 0);
            if (n <= 7) {
                index = minColor + n;
                RESET_AND_ASSIGN(rs_color[index], MALLOC(14));
                sprintf(rs_color[index], "#%02x%02x%02x", r, g, b);
            } else if (n >= 8 && n <= 15) {
                index = minBright + n - 8;
                RESET_AND_ASSIGN(rs_color[index], MALLOC(14));
                sprintf(rs_color[index], "#%02x%02x%02x", r, g, b);
            } else {
                libast_print_error("Parse error in file %s, line %lu:  Invalid color index %lu\n", file_peek_path(), file_peek_line(), n);
            }

        } else if (!BEG_STRCASECMP(tmp, "bd ")) {
#ifndef NO_BOLDUNDERLINE
            RESET_AND_ASSIGN(rs_color[colorBD], MALLOC(14));
            r = strtoul(r1, (char **) NULL, 0);
            g = strtoul(g1, (char **) NULL, 0);
            b = strtoul(b1, (char **) NULL, 0);
            sprintf(rs_color[colorBD], "#%02x%02x%02x", r, g, b);
#else
            libast_print_warning("Support for the color bd attribute was not compiled in, ignoring\n");
#endif

        } else if (!BEG_STRCASECMP(tmp, "ul ")) {
#ifndef NO_BOLDUNDERLINE
            RESET_AND_ASSIGN(rs_color[colorUL], MALLOC(14));
            r = strtoul(r1, (char **) NULL, 0);
            g = strtoul(g1, (char **) NULL, 0);
            b = strtoul(b1, (char **) NULL, 0);
            sprintf(rs_color[colorUL], "#%02x%02x%02x", r, g, b);
#else
            libast_print_warning("Support for the color ul attribute was not compiled in, ignoring\n");
#endif

        } else {
            tmp = spiftool_get_word(1, tmp);
            libast_print_error("Parse error in file %s, line %lu:  Invalid color index \"%s\"\n", file_peek_path(), file_peek_line(),
                        NONULL(tmp));
            FREE(tmp);
        }
    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context color\n",
                    file_peek_path(), file_peek_line(), buff);
    }
    return state;
}

static void *
parse_attributes(char *buff, void *state)
{
    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }
    if (!BEG_STRCASECMP(buff, "geometry ")) {
        RESET_AND_ASSIGN(rs_geometry, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "title ")) {
        RESET_AND_ASSIGN(rs_title, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "name ")) {
        RESET_AND_ASSIGN(rs_name, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "iconname ")) {
        RESET_AND_ASSIGN(rs_iconName, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "desktop ")) {
        rs_desktop = (int) strtol(buff, (char **) NULL, 0);

    } else if (!BEG_STRCASECMP(buff, "scrollbar_type ")) {
        RESET_AND_ASSIGN(rs_scrollbar_type, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "scrollbar_width ")) {
        rs_scrollbar_width = strtoul(spiftool_get_pword(2, buff), (char **) NULL, 0);

    } else if (!BEG_STRCASECMP(buff, "font ")) {

        char *tmp = spiftool_get_pword(2, buff);
        unsigned long n;

        if (!BEG_STRCASECMP(tmp, "fx ") || !BEG_STRCASECMP(tmp, "effect")) {
            if (parse_font_fx(spiftool_get_pword(2, tmp)) != 1) {
                libast_print_error("Parse error in file %s, line %lu:  Syntax error in font effects specification\n",
                            file_peek_path(), file_peek_line());
            }
        } else if (!BEG_STRCASECMP(tmp, "prop")) {
            tmp = spiftool_get_pword(2, tmp);
            if (BOOL_OPT_ISTRUE(tmp)) {
                BITFIELD_SET(vt_options, VT_OPTIONS_PROPORTIONAL);
            } else if (BOOL_OPT_ISFALSE(tmp)) {
                BITFIELD_CLEAR(vt_options, VT_OPTIONS_PROPORTIONAL);
            } else {
                libast_print_error("Parse error in file %s, line %lu:  Invalid/missing boolean value for attribute proportional\n",
                            file_peek_path(), file_peek_line());
            }
        } else if (isdigit(*tmp)) {
            n = strtoul(tmp, (char **) NULL, 0);
            if (n <= 255) {
                eterm_font_add(&etfonts, spiftool_get_pword(2, tmp), n);
            } else {
                libast_print_error("Parse error in file %s, line %lu:  Invalid font index %d\n", file_peek_path(), file_peek_line(), n);
            }
        } else if (!BEG_STRCASECMP(tmp, "bold ")) {
#ifndef NO_BOLDFONT
            RESET_AND_ASSIGN(rs_boldFont, spiftool_get_word(2, tmp));
#else
            libast_print_warning("Support for the bold font attribute was not compiled in, ignoring\n");
#endif

        } else if (!BEG_STRCASECMP(tmp, "default ")) {
            def_font_idx = strtoul(spiftool_get_pword(2, tmp), (char **) NULL, 0);

        } else {
            tmp = spiftool_get_word(1, tmp);
            libast_print_error("Parse error in file %s, line %lu:  Invalid font index \"%s\"\n", file_peek_path(), file_peek_line(),
                        NONULL(tmp));
            FREE(tmp);
        }

    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context attributes\n",
                    file_peek_path(), file_peek_line(), (buff ? buff : ""));
    }
    return state;
}

static void *
parse_toggles(char *buff, void *state)
{
    char *tmp;
    unsigned char bool_val;

    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }
    if (!(tmp = spiftool_get_pword(2, buff))) {
        libast_print_error("Parse error in file %s, line %lu:  Missing boolean value in context toggles\n", file_peek_path(),
                    file_peek_line());
        return NULL;
    }
    if (BOOL_OPT_ISTRUE(tmp)) {
        bool_val = 1;
    } else if (BOOL_OPT_ISFALSE(tmp)) {
        bool_val = 0;
    } else {
        libast_print_error("Parse error in file %s, line %lu:  Invalid boolean value \"%s\" in context toggles\n",
                    file_peek_path(), file_peek_line(), tmp);
        return NULL;
    }

    if (!BEG_STRCASECMP(buff, "map_alert ")) {
#if !defined(NO_MAPALERT) && defined(MAPALERT_OPTION)
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_MAP_ALERT);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_MAP_ALERT);
        }
#else
        libast_print_warning("Support for the map_alert attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "urg_alert ")) {
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_URG_ALERT);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_URG_ALERT);
        }
    } else if (!BEG_STRCASECMP(buff, "visual_bell ")) {
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_VISUAL_BELL);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_VISUAL_BELL);
        }
    } else if (!BEG_STRCASECMP(buff, "login_shell ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_LOGIN_SHELL);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_LOGIN_SHELL);
        }
    } else if (!BEG_STRCASECMP(buff, "scrollbar ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_SCROLLBAR);
        }

    } else if (!BEG_STRCASECMP(buff, "utmp_logging ")) {
#ifdef UTMP_SUPPORT
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_WRITE_UTMP);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_WRITE_UTMP);
        }
#else
        libast_print_warning("Support for the utmp_logging attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "meta8 ")) {
#ifdef META8_OPTION
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_META8);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_META8);
        }
#else
        libast_print_warning("Support for the meta8 attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "iconic ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_ICONIC);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_ICONIC);
        }

    } else if (!BEG_STRCASECMP(buff, "home_on_output ")) {
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_HOME_ON_OUTPUT);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_HOME_ON_OUTPUT);
        }

    } else if (!BEG_STRCASECMP(buff, "home_on_input ")) {
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_HOME_ON_INPUT);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_HOME_ON_INPUT);
        }

    } else if (!BEG_STRCASECMP(buff, "no_input ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_NO_INPUT);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_NO_INPUT);
        }

    } else if (!BEG_STRCASECMP(buff, "scrollbar_floating ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_FLOATING);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_SCROLLBAR_FLOATING);
        }

    } else if (!BEG_STRCASECMP(buff, "scrollbar_right ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT);
        }
    } else if (!BEG_STRCASECMP(buff, "scrollbar_popup ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_POPUP);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_SCROLLBAR_POPUP);
        }
    } else if (!BEG_STRCASECMP(buff, "borderless ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_BORDERLESS);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_BORDERLESS);
        }
    } else if (!BEG_STRCASECMP(buff, "double_buffer ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_DOUBLE_BUFFER);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_DOUBLE_BUFFER);
        }

    } else if (!BEG_STRCASECMP(buff, "no_cursor ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_NO_CURSOR);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_NO_CURSOR);
        }

    } else if (!BEG_STRCASECMP(buff, "pause ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_PAUSE);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_PAUSE);
        }

    } else if (!BEG_STRCASECMP(buff, "xterm_select ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_XTERM_SELECT);
        }

    } else if (!BEG_STRCASECMP(buff, "select_line ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_SELECT_WHOLE_LINE);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_SELECT_WHOLE_LINE);
        }

    } else if (!BEG_STRCASECMP(buff, "select_trailing_spaces ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_SELECT_TRAILING_SPACES);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_SELECT_TRAILING_SPACES);
        }

    } else if (!BEG_STRCASECMP(buff, "report_as_keysyms ")) {
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_REPORT_AS_KEYSYMS);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_REPORT_AS_KEYSYMS);
        }

    } else if (!BEG_STRCASECMP(buff, "mbyte_cursor ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_MBYTE_CURSOR);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_MBYTE_CURSOR);
        }

    } else if (!BEG_STRCASECMP(buff, "itrans ") || !BEG_STRCASECMP(buff, "immotile_trans ")) {
        if (bool_val) {
            BITFIELD_SET(image_options, IMAGE_OPTIONS_ITRANS);
        } else {
            BITFIELD_CLEAR(image_options, IMAGE_OPTIONS_ITRANS);
        }

    } else if (!BEG_STRCASECMP(buff, "buttonbar")) {
        if (bool_val) {
            FOREACH_BUTTONBAR(bbar_set_visible(bbar, 1););
            rs_buttonbars = 1;  /* Reset for future use. */
        } else {
            FOREACH_BUTTONBAR(bbar_set_visible(bbar, 0););
            rs_buttonbars = 1;  /* Reset for future use. */
        }

    } else if (!BEG_STRCASECMP(buff, "resize_gravity")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_RESIZE_GRAVITY);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_RESIZE_GRAVITY);
        }

    } else if (!BEG_STRCASECMP(buff, "overstrike_bold ")) {
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_OVERSTRIKE_BOLD);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_OVERSTRIKE_BOLD);
        }

    } else if (!BEG_STRCASECMP(buff, "bold_brightens_foreground ")) {
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_BOLD_BRIGHTENS_FOREGROUND);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_BOLD_BRIGHTENS_FOREGROUND);
        }

    } else if (!BEG_STRCASECMP(buff, "blink_brightens_background ")) {
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_BLINK_BRIGHTENS_BACKGROUND);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_BLINK_BRIGHTENS_BACKGROUND);
        }

    } else if (!BEG_STRCASECMP(buff, "colors_suppress_bold ")) {
        if (bool_val) {
            BITFIELD_SET(vt_options, VT_OPTIONS_COLORS_SUPPRESS_BOLD);
        } else {
            BITFIELD_CLEAR(vt_options, VT_OPTIONS_COLORS_SUPPRESS_BOLD);
        }

    } else if (!BEG_STRCASECMP(buff, "sticky ")) {
        if (bool_val) {
            BITFIELD_SET(eterm_options, ETERM_OPTIONS_STICKY);
        } else {
            BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_STICKY);
        }

    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context toggles\n", file_peek_path(),
                    file_peek_line(), buff);
    }
    return state;
}

static void *
parse_keyboard(char *buff, void *state)
{
    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }
    if (!BEG_STRCASECMP(buff, "smallfont_key ")) {
#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
        RESET_AND_ASSIGN(rs_smallfont_key, spiftool_get_word(2, buff));
        TO_KEYSYM(&ks_smallfont, rs_smallfont_key);
#else
        libast_print_warning("Support for the smallfont_key attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "bigfont_key ")) {
#if defined (HOTKEY_CTRL) || defined (HOTKEY_META)
        RESET_AND_ASSIGN(rs_bigfont_key, spiftool_get_word(2, buff));
        TO_KEYSYM(&ks_bigfont, rs_bigfont_key);
#else
        libast_print_warning("Support for the bigfont_key attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "keysym ")) {
#ifdef KEYSYM_ATTRIBUTE

        int sym, len;
        char *str = buff + 7, *s;

        sym = (int) strtol(str, (char **) NULL, 0);
        if (sym != (int) 2147483647L) {

            if (sym >= 0xff00)
                sym -= 0xff00;
            if (sym < 0 || sym > 0xff) {
                libast_print_error("Parse error in file %s, line %lu:  Keysym 0x%x out of range 0xff00-0xffff\n",
                            file_peek_path(), file_peek_line(), sym + 0xff00);
                return NULL;
            }
            s = spiftool_get_word(3, buff);
            str = (char *) MALLOC(strlen(s) + 2);
            strcpy(str, s);
            FREE(s);
            spiftool_chomp(str);
            len = parse_escaped_string(str);
            if (len > 255)
                len = 255;      /* We can only handle lengths that will fit in a char */
            if (len && !KeySym_map[sym]) {

                char *p = MALLOC(len + 1);

                *p = len;
                strncpy(p + 1, str, len);
                KeySym_map[sym] = (unsigned char *) p;
            }
        }
#else
        libast_print_warning("Support for the keysym attributes was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "meta_mod ")) {
        char *tmp = spiftool_get_pword(2, buff);

        if (!tmp) {
            libast_print_error("Parse error in file %s, line %lu:  Missing modifier value for attribute meta_mod\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        rs_meta_mod = (unsigned int) strtoul(tmp, (char **) NULL, 0);

    } else if (!BEG_STRCASECMP(buff, "alt_mod ")) {
        char *tmp = spiftool_get_pword(2, buff);

        if (!tmp) {
            libast_print_error("Parse error in file %s, line %lu:  Missing modifier value for attribute alt_mod\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        rs_alt_mod = (unsigned int) strtoul(tmp, (char **) NULL, 0);

    } else if (!BEG_STRCASECMP(buff, "numlock_mod ")) {
        char *tmp = spiftool_get_pword(2, buff);

        if (!tmp) {
            libast_print_error("Parse error in file %s, line %lu:  Missing modifier value for attribute numlock_mod\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        rs_numlock_mod = (unsigned int) strtoul(tmp, (char **) NULL, 0);

    } else if (!BEG_STRCASECMP(buff, "greek ")) {
#ifdef GREEK_SUPPORT

        char *tmp = spiftool_get_pword(2, buff);

        if (!tmp) {
            libast_print_error("Parse error in file %s, line %lu:  Missing boolean value for attribute greek\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        if (BOOL_OPT_ISTRUE(tmp)) {
            RESET_AND_ASSIGN(rs_greek_keyboard, spiftool_get_word(3, buff));
            if (BEG_STRCASECMP(rs_greek_keyboard, "iso")) {
                greek_setmode(GREEK_ELOT928);
            } else if (BEG_STRCASECMP(rs_greek_keyboard, "ibm")) {
                greek_setmode(GREEK_IBM437);
            } else {
                libast_print_error("Parse error in file %s, line %lu:  Invalid greek keyboard mode \"%s\"\n",
                            file_peek_path(), file_peek_line(), (rs_greek_keyboard ? rs_greek_keyboard : ""));
            }
        } else if (BOOL_OPT_ISFALSE(tmp)) {
            /* This space intentionally left no longer blank =^) */
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid boolean value \"%s\" for attribute %s\n",
                        file_peek_path(), file_peek_line(), tmp, buff);
            return NULL;
        }
#else
        libast_print_warning("Support for the greek attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "app_keypad ")) {

        char *tmp = spiftool_get_pword(2, buff);

        if (!tmp) {
            libast_print_error("Parse error in file %s, line %lu:  Missing boolean value for attribute app_keypad\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        if (BOOL_OPT_ISTRUE(tmp)) {
            PrivateModes |= PrivMode_aplKP;
        } else if (BOOL_OPT_ISFALSE(tmp)) {
            PrivateModes &= ~(PrivMode_aplKP);
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid boolean value \"%s\" for attribute app_keypad\n",
                        file_peek_path(), file_peek_line(), tmp);
            return NULL;
        }

    } else if (!BEG_STRCASECMP(buff, "app_cursor ")) {

        char *tmp = spiftool_get_pword(2, buff);

        if (!tmp) {
            libast_print_error("Parse error in file %s, line %lu:  Missing boolean value for attribute app_cursor\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        if (BOOL_OPT_ISTRUE(tmp)) {
            PrivateModes |= PrivMode_aplCUR;
        } else if (BOOL_OPT_ISFALSE(tmp)) {
            PrivateModes &= ~(PrivMode_aplCUR);
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid boolean value \"%s\" for attribute app_cursor\n",
                        file_peek_path(), file_peek_line(), tmp);
            return NULL;
        }

    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context keyboard\n",
                    file_peek_path(), file_peek_line(), buff);
    }
    return state;
}

static void *
parse_misc(char *buff, void *state)
{
    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }
    if (!BEG_STRCASECMP(buff, "print_pipe ")) {
#ifdef PRINTPIPE
        RESET_AND_ASSIGN(rs_print_pipe, spiftool_get_word(2, buff));
#else
        libast_print_warning("Support for the print_pipe attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "save_lines ")) {
        rs_saveLines = strtol(spiftool_get_pword(2, buff), (char **) NULL, 0);

    } else if (!BEG_STRCASECMP(buff, "min_anchor_size ")) {
        rs_min_anchor_size = strtol(spiftool_get_pword(2, buff), (char **) NULL, 0);

    } else if (!BEG_STRCASECMP(buff, "border_width ")) {
#ifdef BORDER_WIDTH_OPTION
        TermWin.internalBorder = (short) strtol(spiftool_get_pword(2, buff), (char **) NULL, 0);
#else
        libast_print_warning("Support for the border_width attribute was not compiled in, ignoring\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "line_space ")) {
        rs_line_space = strtol(spiftool_get_pword(2, buff), (char **) NULL, 0);

    } else if (!BEG_STRCASECMP(buff, "finished_title ")) {
        RESET_AND_ASSIGN(rs_finished_title, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "finished_text ")) {
        RESET_AND_ASSIGN(rs_finished_text, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "term_name ")) {
        RESET_AND_ASSIGN(rs_term_name, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "beep_command ")) {
        RESET_AND_ASSIGN(rs_beep_command, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "debug ")) {
        DEBUG_LEVEL = (unsigned int) strtoul(spiftool_get_pword(2, buff), (char **) NULL, 0);

    } else if (!BEG_STRCASECMP(buff, "exec ")) {

        register unsigned short k, n;

        RESET_AND_ASSIGN(rs_exec_args, (char **) MALLOC(sizeof(char *) * ((n = spiftool_num_words(spiftool_get_pword(2, buff))) + 1)));

        for (k = 0; k < n; k++) {
            rs_exec_args[k] = spiftool_get_word(k + 2, buff);
            D_OPTIONS(("rs_exec_args[%d] == %s\n", k, rs_exec_args[k]));
        }
        rs_exec_args[n] = (char *) NULL;

    } else if (!BEG_STRCASECMP(buff, "cut_chars ")) {
#ifdef CUTCHAR_OPTION
        RESET_AND_ASSIGN(rs_cutchars, spiftool_get_word(2, buff));
        spiftool_chomp(rs_cutchars);
#else
        libast_print_warning("Support for the cut_chars attribute was not compiled in, ignoring\n");
#endif

    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context misc\n",
                    file_peek_path(), file_peek_line(), buff);
    }
    return state;
}

static void *
parse_imageclasses(char *buff, void *state)
{
    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }

    if (!BEG_STRCASECMP(buff, "icon ")) {
#ifdef PIXMAP_SUPPORT
        RESET_AND_ASSIGN(rs_icon, spiftool_get_word(2, buff));
#else
        libast_print_warning("Pixmap support was not compiled in, ignoring \"icon\" attribute\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "cache")) {
#ifdef PIXMAP_SUPPORT
        rs_cache_size = strtoul(spiftool_get_pword(2, buff), (char **) NULL, 0);
#else
        libast_print_warning("Pixmap support was not compiled in, ignoring \"cache\" attribute\n");
#endif

    } else if (!BEG_STRCASECMP(buff, "path ")) {
        RESET_AND_ASSIGN(rs_path, spiftool_get_word(2, buff));

    } else if (!BEG_STRCASECMP(buff, "anim ")) {
#ifdef BACKGROUND_CYCLING_SUPPORT
        char *tmp = spiftool_get_pword(2, buff);

        if (tmp) {
            rs_anim_pixmap_list = STRDUP(tmp);
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter list \"\" for attribute anim\n", file_peek_path(),
                        file_peek_line());
        }
#else
        libast_print_warning("Support for the anim attribute was not compiled in, ignoring\n");
#endif

    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context imageclasses\n",
                    file_peek_path(), file_peek_line(), buff);
    }
    return state;
}

static void *
parse_image(char *buff, void *state)
{
    int idx;

    if (*buff == SPIFCONF_BEGIN_CHAR) {
        int *tmp;

        tmp = (int *) MALLOC(sizeof(int));
        *tmp = -1;
        return ((void *) tmp);
    }
    ASSERT_RVAL(state != NULL, (void *) (file_skip_to_end(), NULL));
    if (*buff == SPIFCONF_END_CHAR) {
        int *tmp;

        tmp = (int *) state;
        FREE(tmp);
        return NULL;
    }
    idx = *((int *) state);
    if (!BEG_STRCASECMP(buff, "type ")) {
        char *type = spiftool_get_pword(2, buff);

        if (!type) {
            libast_print_error("Parse error in file %s, line %lu:  Missing image type\n", file_peek_path(), file_peek_line());
            return NULL;
        }
        if (!strcasecmp(type, "background")) {
            idx = image_bg;
        } else if (!strcasecmp(type, "trough")) {
            idx = image_sb;
        } else if (!strcasecmp(type, "anchor")) {
            idx = image_sa;
        } else if (!strcasecmp(type, "thumb")) {
            idx = image_st;
        } else if (!strcasecmp(type, "up_arrow")) {
            idx = image_up;
        } else if (!strcasecmp(type, "down_arrow")) {
            idx = image_down;
        } else if (!strcasecmp(type, "left_arrow")) {
            idx = image_left;
        } else if (!strcasecmp(type, "right_arrow")) {
            idx = image_right;
        } else if (!strcasecmp(type, "menu")) {
            idx = image_menu;
        } else if (!strcasecmp(type, "menuitem")) {
            idx = image_menuitem;
        } else if (!strcasecmp(type, "submenu")) {
            idx = image_submenu;
        } else if (!strcasecmp(type, "button")) {
            idx = image_button;
        } else if (!strcasecmp(type, "button_bar") || !strcasecmp(type, "buttonbar")) {
            idx = image_bbar;
        } else if (!strcasecmp(type, "grab_bar")) {
            idx = image_gbar;
        } else if (!strcasecmp(type, "dialog_box")) {
            idx = image_dialog;
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid image type \"%s\"\n", file_peek_path(), file_peek_line(), type);
            return NULL;
        }
        *((int *) state) = idx;

    } else if (!BEG_STRCASECMP(buff, "mode ")) {
        char *mode = spiftool_get_pword(2, buff);
        char *allow_list = spiftool_get_pword(4, buff);

        if (!CHECK_VALID_INDEX(idx)) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"mode\" with no image type defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!mode) {
            libast_print_error("Parse error in file %s, line %lu:  Missing parameters for mode line\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!BEG_STRCASECMP(mode, "image")) {
            images[idx].mode = (MODE_IMAGE | ALLOW_IMAGE);
        } else if (!BEG_STRCASECMP(mode, "trans")) {
            images[idx].mode = (MODE_TRANS | ALLOW_TRANS);
        } else if (!BEG_STRCASECMP(mode, "viewport")) {
            images[idx].mode = (MODE_VIEWPORT | ALLOW_VIEWPORT);
        } else if (!BEG_STRCASECMP(mode, "auto")) {
            images[idx].mode = (MODE_AUTO | ALLOW_AUTO);
        } else if (!BEG_STRCASECMP(mode, "solid")) {
            images[idx].mode = MODE_SOLID;
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid mode \"%s\"\n", file_peek_path(), file_peek_line(), mode);
        }
        if (allow_list) {
            char *allow;

            for (; (allow = (char *)strsep(&allow_list, " "));) {
                if (!BEG_STRCASECMP(allow, "image")) {
                    images[idx].mode |= ALLOW_IMAGE;
                } else if (!BEG_STRCASECMP(allow, "trans")) {
                    images[idx].mode |= ALLOW_TRANS;
                } else if (!BEG_STRCASECMP(allow, "viewport")) {
                    images[idx].mode |= ALLOW_VIEWPORT;
                } else if (!BEG_STRCASECMP(allow, "auto")) {
                    images[idx].mode |= ALLOW_AUTO;
                } else if (!BEG_STRCASECMP(allow, "solid")) {
                } else {
                    libast_print_error("Parse error in file %s, line %lu:  Invalid mode \"%s\"\n", file_peek_path(), file_peek_line(),
                                allow);
                }
            }
        }
    } else if (!BEG_STRCASECMP(buff, "state ")) {
        char *state = spiftool_get_pword(2, buff), new = 0;

        if (!state) {
            libast_print_error("Parse error in file %s, line %lu:  Missing state\n", file_peek_path(), file_peek_line());
            return NULL;
        }
        if (!CHECK_VALID_INDEX(idx)) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"state\" with no image type defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!strcasecmp(state, "normal")) {
            if (!images[idx].norm) {
                images[idx].norm = (simage_t *) MALLOC(sizeof(simage_t));
                new = 1;
            }
            images[idx].current = images[idx].norm;
        } else if (!strcasecmp(state, "selected")) {
            if (!images[idx].selected) {
                images[idx].selected = (simage_t *) MALLOC(sizeof(simage_t));
                new = 1;
            }
            images[idx].current = images[idx].selected;
        } else if (!strcasecmp(state, "clicked")) {
            if (!images[idx].clicked) {
                images[idx].clicked = (simage_t *) MALLOC(sizeof(simage_t));
                new = 1;
            }
            images[idx].current = images[idx].clicked;
        } else if (!strcasecmp(state, "disabled")) {
            if (!images[idx].disabled) {
                images[idx].disabled = (simage_t *) MALLOC(sizeof(simage_t));
                new = 1;
            }
            images[idx].current = images[idx].disabled;
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid state \"%s\"\n", file_peek_path(), file_peek_line(), state);
            return NULL;
        }
        if (new) {
            MEMSET(images[idx].current, 0, sizeof(simage_t));
            images[idx].current->pmap = (pixmap_t *) MALLOC(sizeof(pixmap_t));
            images[idx].current->iml = (imlib_t *) MALLOC(sizeof(imlib_t));
            MEMSET(images[idx].current->pmap, 0, sizeof(pixmap_t));
            MEMSET(images[idx].current->iml, 0, sizeof(imlib_t));
        }
    } else if (!BEG_STRCASECMP(buff, "color ")) {
        char *fg = spiftool_get_word(2, buff), *bg = spiftool_get_word(3, buff);

        if (!CHECK_VALID_INDEX(idx)) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"color\" with no image type defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!images[idx].current) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"color\" with no image state defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!fg || !bg) {
            libast_print_error("Parse error in file %s, line %lu:  Foreground and background colors must be specified with \"color\"\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        if (!BEG_STRCASECMP(fg, "0x")) {
            images[idx].current->fg = get_color_by_pixel((Pixel) strtoul(fg, (char **) NULL, 0), WhitePixel(Xdisplay, Xscreen));
        } else {
            images[idx].current->fg = get_color_by_name(fg, "white");
        }
        if (!BEG_STRCASECMP(bg, "0x")) {
            images[idx].current->bg = get_color_by_pixel((Pixel) strtoul(bg, (char **) NULL, 0), BlackPixel(Xdisplay, Xscreen));
        } else {
            images[idx].current->bg = get_color_by_name(bg, "black");
        }
        FREE(fg);
        FREE(bg);

    } else if (!BEG_STRCASECMP(buff, "file ")) {
#ifdef PIXMAP_SUPPORT
        char *filename = spiftool_get_pword(2, buff);

        if (!CHECK_VALID_INDEX(idx)) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"file\" with no image type defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!images[idx].current) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"file\" with no image state defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!filename) {
            libast_print_error("Parse error in file %s, line %lu:  Missing filename\n", file_peek_path(), file_peek_line());
            return NULL;
        }
        if (!load_image(filename, images[idx].current)) {
            images[idx].mode &= ~(MODE_IMAGE | ALLOW_IMAGE);
            D_PIXMAP(("New image mode is 0x%02x, iml->im is 0x%08x\n", images[idx].mode, images[idx].current->iml->im));
        }
#endif

    } else if (!BEG_STRCASECMP(buff, "geom ")) {
#ifdef PIXMAP_SUPPORT
        char *geom = spiftool_get_pword(2, buff);

        if (!CHECK_VALID_INDEX(idx)) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"geom\" with no image type defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!images[idx].current) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"geom\" with no image state defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!geom) {
            libast_print_error("Parse error in file %s, line %lu:  Missing geometry string\n", file_peek_path(), file_peek_line());
            return NULL;
        }
        set_pixmap_scale(geom, images[idx].current->pmap);
#endif

    } else if (!BEG_STRCASECMP(buff, "cmod ") || !BEG_STRCASECMP(buff, "colormod ")) {
#ifdef PIXMAP_SUPPORT
        char *color = spiftool_get_pword(2, buff);
        char *mods = spiftool_get_pword(3, buff);
        unsigned char n;
        imlib_t *iml = images[idx].current->iml;

        if (!CHECK_VALID_INDEX(idx)) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered color modifier with no image type defined\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        if (!images[idx].current) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered color modifier with no image state defined\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        if (!color) {
            libast_print_error("Parse error in file %s, line %lu:  Missing color name\n", file_peek_path(), file_peek_line());
            return NULL;
        }
        if (!mods) {
            libast_print_error("Parse error in file %s, line %lu:  Missing modifier(s)\n", file_peek_path(), file_peek_line());
            return NULL;
        }
        n = spiftool_num_words(mods);

        if (!BEG_STRCASECMP(color, "image ")) {
            if (iml->mod) {
                free_colormod(iml->mod);
            }
            iml->mod = create_colormod();
            iml->mod->brightness = (int) strtol(mods, (char **) NULL, 0);
            if (n > 1) {
                iml->mod->contrast = (int) strtol(spiftool_get_pword(2, mods), (char **) NULL, 0);
            }
            if (n > 2) {
                iml->mod->gamma = (int) strtol(spiftool_get_pword(3, mods), (char **) NULL, 0);
            }
            update_cmod(iml->mod);
        } else if (!BEG_STRCASECMP(color, "red ")) {
            if (iml->rmod) {
                free_colormod(iml->rmod);
            }
            iml->rmod = create_colormod();
            iml->rmod->brightness = (int) strtol(mods, (char **) NULL, 0);
            if (n > 1) {
                iml->rmod->contrast = (int) strtol(spiftool_get_pword(2, mods), (char **) NULL, 0);
            }
            if (n > 2) {
                iml->rmod->gamma = (int) strtol(spiftool_get_pword(3, mods), (char **) NULL, 0);
            }
            update_cmod(iml->rmod);
        } else if (!BEG_STRCASECMP(color, "green ")) {
            if (iml->gmod) {
                free_colormod(iml->gmod);
            }
            iml->gmod = create_colormod();
            iml->gmod->brightness = (int) strtol(mods, (char **) NULL, 0);
            if (n > 1) {
                iml->gmod->contrast = (int) strtol(spiftool_get_pword(2, mods), (char **) NULL, 0);
            }
            if (n > 2) {
                iml->gmod->gamma = (int) strtol(spiftool_get_pword(3, mods), (char **) NULL, 0);
            }
            update_cmod(iml->gmod);
        } else if (!BEG_STRCASECMP(color, "blue ")) {
            if (iml->bmod) {
                free_colormod(iml->bmod);
            }
            iml->bmod = create_colormod();
            iml->bmod->brightness = (int) strtol(mods, (char **) NULL, 0);
            if (n > 1) {
                iml->bmod->contrast = (int) strtol(spiftool_get_pword(2, mods), (char **) NULL, 0);
            }
            if (n > 2) {
                iml->bmod->gamma = (int) strtol(spiftool_get_pword(3, mods), (char **) NULL, 0);
            }
            update_cmod(iml->bmod);
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Color must be either \"image\", \"red\", \"green\", or \"blue\"\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
#endif

    } else if (!BEG_STRCASECMP(buff, "border ")) {
        if (!CHECK_VALID_INDEX(idx)) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"border\" with no image type defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (spiftool_num_words(buff + 7) < 4) {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter list for attribute \"border\"\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        RESET_AND_ASSIGN(images[idx].current->iml->border, (Imlib_Border *) MALLOC(sizeof(Imlib_Border)));

        images[idx].current->iml->border->left = (unsigned short) strtoul(spiftool_get_pword(2, buff), (char **) NULL, 0);
        images[idx].current->iml->border->right = (unsigned short) strtoul(spiftool_get_pword(3, buff), (char **) NULL, 0);
        images[idx].current->iml->border->top = (unsigned short) strtoul(spiftool_get_pword(4, buff), (char **) NULL, 0);
        images[idx].current->iml->border->bottom = (unsigned short) strtoul(spiftool_get_pword(5, buff), (char **) NULL, 0);

        if ((images[idx].current->iml->border->left == 0) && (images[idx].current->iml->border->right == 0)
            && (images[idx].current->iml->border->top == 0) && (images[idx].current->iml->border->bottom == 0)) {
            FREE(images[idx].current->iml->border);
            images[idx].current->iml->border = (Imlib_Border *) NULL;   /* No sense in wasting CPU time and memory if there are no borders */
        }
    } else if (!BEG_STRCASECMP(buff, "bevel ")) {
        if (!CHECK_VALID_INDEX(idx)) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"bevel\" with no image type defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!images[idx].current) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"bevel\" with no image state defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (spiftool_num_words(buff + 6) < 5) {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter list for attribute \"bevel\"\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (images[idx].current->iml->bevel) {
            FREE(images[idx].current->iml->bevel->edges);
            FREE(images[idx].current->iml->bevel);
        }
        images[idx].current->iml->bevel = (bevel_t *) MALLOC(sizeof(bevel_t));
        images[idx].current->iml->bevel->edges = (Imlib_Border *) MALLOC(sizeof(Imlib_Border));

        if (!BEG_STRCASECMP(spiftool_get_pword(2, buff), "down")) {
            images[idx].current->iml->bevel->up = 0;
        } else {
            images[idx].current->iml->bevel->up = 1;
        }
        images[idx].current->iml->bevel->edges->left = (unsigned short) strtoul(spiftool_get_pword(3, buff), (char **) NULL, 0);
        images[idx].current->iml->bevel->edges->right = (unsigned short) strtoul(spiftool_get_pword(4, buff), (char **) NULL, 0);
        images[idx].current->iml->bevel->edges->top = (unsigned short) strtoul(spiftool_get_pword(5, buff), (char **) NULL, 0);
        images[idx].current->iml->bevel->edges->bottom = (unsigned short) strtoul(spiftool_get_pword(6, buff), (char **) NULL, 0);

        if ((images[idx].current->iml->bevel->edges->left == 0) && (images[idx].current->iml->bevel->edges->right == 0)
            && (images[idx].current->iml->bevel->edges->top == 0) && (images[idx].current->iml->bevel->edges->bottom == 0)) {
            FREE(images[idx].current->iml->bevel->edges);
            images[idx].current->iml->bevel->edges = (Imlib_Border *) NULL;
            FREE(images[idx].current->iml->bevel);
            images[idx].current->iml->bevel = (bevel_t *) NULL;
        }
    } else if (!BEG_STRCASECMP(buff, "padding ")) {
        if (!CHECK_VALID_INDEX(idx)) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"padding\" with no image type defined\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        if (!images[idx].current) {
            libast_print_error("Parse error in file %s, line %lu:  Encountered \"padding\" with no image state defined\n",
                        file_peek_path(), file_peek_line());
            return NULL;
        }
        if (spiftool_num_words(buff + 8) < 4) {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter list for attribute \"padding\"\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        RESET_AND_ASSIGN(images[idx].current->iml->pad, (Imlib_Border *) MALLOC(sizeof(Imlib_Border)));

        images[idx].current->iml->pad->left = (unsigned short) strtoul(spiftool_get_pword(2, buff), (char **) NULL, 0);
        images[idx].current->iml->pad->right = (unsigned short) strtoul(spiftool_get_pword(3, buff), (char **) NULL, 0);
        images[idx].current->iml->pad->top = (unsigned short) strtoul(spiftool_get_pword(4, buff), (char **) NULL, 0);
        images[idx].current->iml->pad->bottom = (unsigned short) strtoul(spiftool_get_pword(5, buff), (char **) NULL, 0);

        if ((images[idx].current->iml->pad->left == 0) && (images[idx].current->iml->pad->right == 0)
            && (images[idx].current->iml->pad->top == 0) && (images[idx].current->iml->pad->bottom == 0)) {
            FREE(images[idx].current->iml->pad);
            images[idx].current->iml->pad = (Imlib_Border *) NULL;
        }
    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context image\n",
                    file_peek_path(), file_peek_line(), buff);
    }
    return ((void *) state);
}

static void *
parse_actions(char *buff, void *state)
{
    unsigned short mod = MOD_NONE;
    unsigned char button = BUTTON_NONE;
    KeySym keysym = 0;
    char *str;
    unsigned short i;

    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }

    if (!BEG_STRCASECMP(buff, "bind ")) {
        for (i = 2; (str = spiftool_get_word(i, buff)) && strcasecmp(str, "to"); i++) {
            if (!BEG_STRCASECMP(str, "anymod")) {
                mod = MOD_ANY;
            } else if (!BEG_STRCASECMP(str, "ctrl")) {
                mod |= MOD_CTRL;
            } else if (!BEG_STRCASECMP(str, "shift")) {
                mod |= MOD_SHIFT;
            } else if (!BEG_STRCASECMP(str, "lock")) {
                mod |= MOD_LOCK;
            } else if (!BEG_STRCASECMP(str, "meta")) {
                mod |= MOD_META;
            } else if (!BEG_STRCASECMP(str, "alt")) {
                mod |= MOD_ALT;
            } else if (!BEG_STRCASECMP(str, "mod1")) {
                mod |= MOD_MOD1;
            } else if (!BEG_STRCASECMP(str, "mod2")) {
                mod |= MOD_MOD2;
            } else if (!BEG_STRCASECMP(str, "mod3")) {
                mod |= MOD_MOD3;
            } else if (!BEG_STRCASECMP(str, "mod4")) {
                mod |= MOD_MOD4;
            } else if (!BEG_STRCASECMP(str, "mod5")) {
                mod |= MOD_MOD5;
            } else if (!BEG_STRCASECMP(str, "button")) {
                button = *(str + 6) - '0';
            } else if (isdigit(*str)) {
                keysym = (KeySym) strtoul(str, (char **) NULL, 0);
            } else {
                keysym = XStringToKeysym(str);
            }
            FREE(str);
        }
        if (!str) {
            libast_print_error("Parse error in file %s, line %lu:  Syntax error (\"to\" not found)\n", file_peek_path(), file_peek_line());
            return NULL;
        }
        FREE(str);
        if ((button == BUTTON_NONE) && (keysym == 0)) {
            libast_print_error("Parse error in file %s, line %lu:  No valid button/keysym found for action\n", file_peek_path(),
                        file_peek_line());
            return NULL;
        }
        i++;
        str = spiftool_get_pword(i, buff);
        if (!BEG_STRCASECMP(str, "string")) {
            str = spiftool_get_word(i + 1, buff);
            action_add(mod, button, keysym, ACTION_STRING, (void *) str);
            FREE(str);
        } else if (!BEG_STRCASECMP(str, "echo")) {
            str = spiftool_get_word(i + 1, buff);
            action_add(mod, button, keysym, ACTION_ECHO, (void *) str);
            FREE(str);
        } else if (!BEG_STRCASECMP(str, "menu")) {
            menu_t *menu;

            str = spiftool_get_word(i + 1, buff);
            menu = find_menu_by_title(menu_list, str);
            action_add(mod, button, keysym, ACTION_MENU, (void *) menu);
            FREE(str);
        } else if (!BEG_STRCASECMP(str, "script")) {
            str = spiftool_get_word(i + 1, buff);
            action_add(mod, button, keysym, ACTION_SCRIPT, (void *) str);
            FREE(str);
        } else {
            libast_print_error
                ("Parse error in file %s, line %lu:  No valid action type found.  Valid types are \"string,\" \"echo,\" \"menu,\" and \"script.\"\n",
                 file_peek_path(), file_peek_line());
            return NULL;
        }

    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context action\n",
                    file_peek_path(), file_peek_line(), buff);
    }
    return state;
}

static void *
parse_menu(char *buff, void *state)
{
    menu_t *menu;

    if (*buff == SPIFCONF_BEGIN_CHAR) {
        char *title = spiftool_get_pword(2, buff + 6);

        menu = menu_create(title);
        return ((void *) menu);
    }
    ASSERT_RVAL(state != NULL, (void *) (file_skip_to_end(), NULL));
    menu = (menu_t *) state;
    if (*buff == SPIFCONF_END_CHAR) {
        if (!(*(menu->title))) {
            char tmp[20];

            sprintf(tmp, "Eterm_Menu_%u", menu_list->nummenus);
            menu_set_title(menu, tmp);
            libast_print_error("Parse error in file %s, line %lu:  Menu context ended without giving a title.  Defaulted to \"%s\".\n",
                        file_peek_path(), file_peek_line(), tmp);
        }
        menu_list = menulist_add_menu(menu_list, menu);
        return NULL;
    }
    if (!BEG_STRCASECMP(buff, "title ")) {
        char *title = spiftool_get_word(2, buff);

        menu_set_title(menu, title);
        FREE(title);

    } else if (!BEG_STRCASECMP(buff, "font ")) {
        char *name = spiftool_get_word(2, buff);

        if (!name) {
            libast_print_error("Parse error in file %s, line %lu:  Missing font name.\n", file_peek_path(), file_peek_line());
            return ((void *) menu);
        }
        menu_set_font(menu, name);
        FREE(name);

    } else if (!BEG_STRCASECMP(buff, "sep") || !BEG_STRCASECMP(buff, "-")) {
        menuitem_t *item;

        item = menuitem_create((char *) NULL);
        menu_add_item(menu, item);
        menuitem_set_action(item, MENUITEM_SEP, (char *) NULL);

    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context menu\n", file_peek_path(),
                    file_peek_line(), buff);
    }
    return ((void *) menu);
}

static void *
parse_menuitem(char *buff, void *state)
{
    static menu_t *menu;
    menuitem_t *curitem;

    ASSERT_RVAL(state != NULL, (void *) (file_skip_to_end(), NULL));
    if (*buff == SPIFCONF_BEGIN_CHAR) {
        menu = (menu_t *) state;
        curitem = menuitem_create(NULL);
        return ((void *) curitem);
    }
    curitem = (menuitem_t *) state;
    ASSERT_RVAL(menu != NULL, state);
    if (*buff == SPIFCONF_END_CHAR) {
        if (!(curitem->text)) {
            libast_print_error("Parse error in file %s, line %lu:  Menuitem context ended with no text given.  Discarding this entry.\n",
                        file_peek_path(), file_peek_line());
            FREE(curitem);
        } else {
            menu_add_item(menu, curitem);
        }
        return ((void *) menu);
    }
    if (!BEG_STRCASECMP(buff, "text ")) {
        char *text = spiftool_get_word(2, buff);

        if (!text) {
            libast_print_error("Parse error in file %s, line %lu:  Missing menuitem text.\n", file_peek_path(), file_peek_line());
            return ((void *) curitem);
        }
        menuitem_set_text(curitem, text);
        FREE(text);

    } else if (!BEG_STRCASECMP(buff, "rtext ")) {
        char *rtext = spiftool_get_word(2, buff);

        if (!rtext) {
            libast_print_error("Parse error in file %s, line %lu:  Missing menuitem right-justified text.\n", file_peek_path(),
                        file_peek_line());
            return ((void *) curitem);
        }
        menuitem_set_rtext(curitem, rtext);
        FREE(rtext);

    } else if (!BEG_STRCASECMP(buff, "icon ")) {

    } else if (!BEG_STRCASECMP(buff, "action ")) {
        char *type = spiftool_get_pword(2, buff);
        char *action = spiftool_get_word(3, buff);

        if (!BEG_STRCASECMP(type, "submenu ")) {
            menuitem_set_action(curitem, MENUITEM_SUBMENU, action);

        } else if (!BEG_STRCASECMP(type, "string ")) {
            menuitem_set_action(curitem, MENUITEM_STRING, action);

        } else if (!BEG_STRCASECMP(type, "script ")) {
            menuitem_set_action(curitem, MENUITEM_SCRIPT, action);

        } else if (!BEG_STRCASECMP(type, "echo ")) {
            menuitem_set_action(curitem, MENUITEM_ECHO, action);

        } else if (!BEG_STRCASECMP(type, "separator")) {
            menuitem_set_action(curitem, MENUITEM_SEP, action);

        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid menu item action \"%s\"\n", file_peek_path(), file_peek_line(),
                        NONULL(type));
        }
        FREE(action);

    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context menu\n", file_peek_path(),
                    file_peek_line(), buff);
    }
    return ((void *) curitem);
}

static void *
parse_bbar(char *buff, void *state)
{
    buttonbar_t *bbar;

    if (*buff == SPIFCONF_BEGIN_CHAR) {
        bbar = bbar_create();
        return ((void *) bbar);
    }
    ASSERT_RVAL(state != NULL, (void *) (file_skip_to_end(), NULL));
    bbar = (buttonbar_t *) state;
    if (*buff == SPIFCONF_END_CHAR) {
        bbar_add(bbar);
        return NULL;
    }
    if (!BEG_STRCASECMP(buff, "font ")) {
        char *font = spiftool_get_word(2, buff);

        bbar_set_font(bbar, font);
        FREE(font);

    } else if (!BEG_STRCASECMP(buff, "dock ")) {
        char *where = spiftool_get_pword(2, buff);

        if (!where) {
            libast_print_error("Parse error in file %s, line %lu:  Attribute dock requires a parameter\n", file_peek_path(),
                        file_peek_line());
        } else if (!BEG_STRCASECMP(where, "top")) {
            bbar_set_docked(bbar, BBAR_DOCKED_TOP);
        } else if (!BEG_STRCASECMP(where, "bot")) {     /* "bot" or "bottom" */
            bbar_set_docked(bbar, BBAR_DOCKED_BOTTOM);
        } else if (!BEG_STRCASECMP(where, "no")) {      /* "no" or "none" */
            bbar_set_docked(bbar, BBAR_UNDOCKED);
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter \"%s\" to attribute dock\n", file_peek_path(),
                        file_peek_line(), where);
        }

    } else if (!BEG_STRCASECMP(buff, "visible ")) {
        char *tmp = spiftool_get_pword(2, buff);

        if (BOOL_OPT_ISTRUE(tmp)) {
            bbar_set_visible(bbar, 1);
        } else if (BOOL_OPT_ISFALSE(tmp)) {
            bbar_set_visible(bbar, 0);
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid boolean value \"%s\" in context button_bar\n", file_peek_path(),
                        file_peek_line(), tmp);
        }

    } else if (!BEG_STRCASECMP(buff, "button ") || !BEG_STRCASECMP(buff, "rbutton ")) {
        char *text = spiftool_get_pword(2, buff);
        char *icon = strcasestr(buff, "icon ");
        char *action = strcasestr(buff, "action ");
        button_t *button;

        if (text == icon) {
            text = NULL;
        } else {
            text = spiftool_get_word(2, buff);
        }
        if (!text && !icon) {
            libast_print_error("Parse error in file %s, line %lu:  Missing button specifications\n", file_peek_path(), file_peek_line());
            return ((void *) bbar);
        }

        button = button_create(text);
        if (icon) {
            simage_t *simg;

            icon = spiftool_get_word(2, icon);
            simg = create_simage();
            if (load_image(icon, simg)) {
                button_set_icon(button, simg);
            } else {
                free_simage(simg);
            }
            FREE(icon);
        }
        if (action) {
            char *type = spiftool_get_pword(2, action);

            action = spiftool_get_word(2, type);
            if (!BEG_STRCASECMP(type, "menu ")) {
                button_set_action(button, ACTION_MENU, action);
            } else if (!BEG_STRCASECMP(type, "string ")) {
                button_set_action(button, ACTION_STRING, action);
            } else if (!BEG_STRCASECMP(type, "echo ")) {
                button_set_action(button, ACTION_ECHO, action);
            } else if (!BEG_STRCASECMP(type, "script ")) {
                button_set_action(button, ACTION_SCRIPT, action);
            } else {
                libast_print_error("Parse error in file %s, line %lu:  Invalid button action \"%s\"\n", file_peek_path(), file_peek_line(),
                            type);
                FREE(action);
                FREE(button);
                return ((void *) bbar);
            }
            FREE(action);
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Missing button action\n", file_peek_path(), file_peek_line());
            FREE(button);
            return ((void *) bbar);
        }
        if (tolower(*buff) == 'r') {
            bbar_add_rbutton(bbar, button);
        } else {
            bbar_add_button(bbar, button);
        }
    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context menu\n",
                    file_peek_path(), file_peek_line(), buff);
    }
    return ((void *) bbar);
}

static void *
parse_xim(char *buff, void *state)
{
#ifdef USE_XIM
    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }
    if (!BEG_STRCASECMP(buff, "input_method ")) {
        RESET_AND_ASSIGN(rs_input_method, spiftool_get_word(2, buff));
    } else if (!BEG_STRCASECMP(buff, "preedit_type ")) {
        RESET_AND_ASSIGN(rs_preedit_type, spiftool_get_word(2, buff));
    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context xim\n",
                    file_peek_path(), file_peek_line(), buff);
    }
#else
    libast_print_warning("XIM support was not compiled in, ignoring entire context\n");
    file_poke_skip(1);
#endif
    return state;
    buff = NULL;
}

static void *
parse_multichar(char *buff, void *state)
{
#ifdef MULTI_CHARSET
    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }
    if (!BEG_STRCASECMP(buff, "encoding ")) {
        RESET_AND_ASSIGN(rs_multichar_encoding, spiftool_get_word(2, buff));
        if (rs_multichar_encoding) {
            if (BEG_STRCASECMP(rs_multichar_encoding, "eucj")
                && BEG_STRCASECMP(rs_multichar_encoding, "sjis")
                && BEG_STRCASECMP(rs_multichar_encoding, "euckr")
                && BEG_STRCASECMP(rs_multichar_encoding, "big5")
                && BEG_STRCASECMP(rs_multichar_encoding, "gb")
                && BEG_STRCASECMP(rs_multichar_encoding, "iso-10646")
                && BEG_STRCASECMP(rs_multichar_encoding, "none")) {
                libast_print_error("Parse error in file %s, line %lu:  Invalid multichar encoding mode \"%s\"\n",
                            file_peek_path(), file_peek_line(), rs_multichar_encoding);
                FREE(rs_multichar_encoding);
                return NULL;
            }
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter list \"\" for attribute encoding\n",
                        file_peek_path(), file_peek_line());
        }
    } else if (!BEG_STRCASECMP(buff, "font ")) {

        char *tmp = spiftool_get_pword(2, buff);
        unsigned long n;

        if (spiftool_num_words(buff) != 3) {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter list \"%s\" for attribute font\n",
                        file_peek_path(), file_peek_line(), NONULL(tmp));
            return NULL;
        }
        if (isdigit(*tmp)) {
            n = strtoul(tmp, (char **) NULL, 0);
            if (n <= 255) {
                eterm_font_add(&etmfonts, spiftool_get_pword(2, tmp), n);
            } else {
                libast_print_error("Parse error in file %s, line %lu:  Invalid font index %d\n", file_peek_path(), file_peek_line(), n);
            }
        } else {
            tmp = spiftool_get_word(1, tmp);
            libast_print_error("Parse error in file %s, line %lu:  Invalid font index \"%s\"\n", file_peek_path(), file_peek_line(),
                        NONULL(tmp));
            FREE(tmp);
        }

    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context multichar\n",
                    file_peek_path(), file_peek_line(), buff);
    }
#else
    if (*buff == SPIFCONF_BEGIN_CHAR) {
        libast_print_warning("Multichar support was not compiled in, ignoring entire context\n");
        file_poke_skip(1);
    }
#endif
    return state;
    buff = NULL;
}

static void *
parse_escreen(char *buff, void *state)
{
#ifdef ESCREEN
    if ((*buff == SPIFCONF_BEGIN_CHAR) || (*buff == SPIFCONF_END_CHAR)) {
        return NULL;
    }
    if (!BEG_STRCASECMP(buff, "url ")) {
        RESET_AND_ASSIGN(rs_url, spiftool_get_word(2, buff));
    } else if (!BEG_STRCASECMP(buff, "firewall ")) {
        RESET_AND_ASSIGN(rs_hop, spiftool_get_word(2, buff));
    } else if (!BEG_STRCASECMP(buff, "delay ")) {
        rs_delay = strtol(spiftool_get_pword(2, buff), (char **) NULL, 0);
    } else if (!BEG_STRCASECMP(buff, "bbar_font ")) {
        RESET_AND_ASSIGN(rs_es_font, spiftool_get_word(2, buff));
    } else if (!BEG_STRCASECMP(buff, "bbar_dock ")) {
        char *where = spiftool_get_pword(2, buff);

        if (!where) {
            libast_print_error("Parse error in file %s, line %lu:  Attribute bbar_dock requires a parameter\n", file_peek_path(),
                        file_peek_line());
        } else if (!BEG_STRCASECMP(where, "top")) {
            rs_es_dock = BBAR_DOCKED_TOP;
        } else if (!BEG_STRCASECMP(where, "bot")) {     /* "bot" or "bottom" */
            rs_es_dock = BBAR_DOCKED_BOTTOM;
        } else if (!BEG_STRCASECMP(where, "no")) {      /* "no" or "none" */
            rs_es_dock = BBAR_UNDOCKED;
        } else {
            libast_print_error("Parse error in file %s, line %lu:  Invalid parameter \"%s\" to attribute bbar_dock\n", file_peek_path(),
                        file_peek_line(), where);
        }
    } else {
        libast_print_error("Parse error in file %s, line %lu:  Attribute \"%s\" is not valid within context escreen\n",
                    file_peek_path(), file_peek_line(), buff);
    }
#else
    libast_print_warning("Escreen support was not compiled in, ignoring entire context\n");
    file_poke_skip(1);
#endif
    return state;
    buff = NULL;
}

char *
spifconf_parse_theme(char **theme, char *spifconf_name, unsigned char fallback)
{
    static char path[CONFIG_BUFF];
    char *ret = NULL;

    if (!(*path)) {
        char *path_env;

        path_env = getenv(PATH_ENV);
        if (path_env) {
            snprintf(path, sizeof(path), "%s:%s", CONFIG_SEARCH_PATH, path_env);
        } else {
            snprintf(path, sizeof(path), CONFIG_SEARCH_PATH);
        }
        spifconf_shell_expand(path);
    }
    if (fallback & PARSE_TRY_USER_THEME) {
        if (theme && *theme && (ret = spifconf_parse(spifconf_name, *theme, path))) {
            return ret;
        }
    }
    if (fallback & PARSE_TRY_DEFAULT_THEME) {
        RESET_AND_ASSIGN(*theme, STRDUP(PACKAGE));
        if ((ret = spifconf_parse(spifconf_name, *theme, path))) {
            return ret;
        }
    }
    if (fallback & PARSE_TRY_NO_THEME) {
        RESET_AND_ASSIGN(*theme, NULL);
        return (spifconf_parse(spifconf_name, *theme, path));
    }
    return NULL;
}

void
init_libast(void)
{
    libast_set_program_name(PACKAGE);
    libast_set_program_version(VERSION);
    SPIFOPT_OPTLIST_SET(option_list);
    SPIFOPT_NUMOPTS_SET(sizeof(option_list) / sizeof(spifopt_t));
    SPIFOPT_ALLOWBAD_SET(3);
}

/* Initialize the default values for everything */
void
init_defaults(void)
{
#ifndef AUTO_ENCODING
    unsigned char i;
#endif

#if DEBUG >= DEBUG_MEM
    if (DEBUG_LEVEL >= DEBUG_MEM) {
        spifmem_init();
    }
#endif

    Xdisplay = NULL;
    rs_term_name = NULL;
#ifdef CUTCHAR_OPTION
    rs_cutchars = NULL;
#endif
#ifndef NO_BOLDFONT
    rs_boldFont = NULL;
#endif
#ifdef PRINTPIPE
    rs_print_pipe = NULL;
#endif
    rs_title = NULL;            /* title name for window */
    rs_iconName = NULL;         /* icon name for window */
    rs_geometry = NULL;         /* window geometry */

#ifdef PIXMAP_SUPPORT
    rs_path = NULL;
#endif
    colorfgbg = DEFAULT_RSTYLE;
    MEMSET(PixColors, 0, sizeof(PixColors));

    /* Font stuff. */
    MEMSET(rs_font, 0, sizeof(char *) * NFONTS);
#if AUTO_ENCODING
    /* Auto-encoding means the default fonts are chosen by locale. */
# ifdef MULTI_CHARSET
    eterm_default_font_locale(&etfonts, &etmfonts, &rs_multichar_encoding, &def_font_idx);
# else
    eterm_default_font_locale(&etfonts, NULL, NULL, &def_font_idx);
# endif
#else
    /* No auto-encoding, so use built-in ISO-8859-1 fonts. */
    for (i = 0; i < NFONTS; i++) {
        eterm_font_add(&etfonts, def_fontName[i], i);
# ifdef MULTI_CHARSET
        eterm_font_add(&etmfonts, def_mfontName[i], i);
# endif
    }
# ifdef MULTI_CHARSET
    rs_multichar_encoding = STRDUP(MULTICHAR_ENCODING);
# endif
#endif

    TermWin.internalBorder = DEFAULT_BORDER_WIDTH;

    /* Initialize the parser */
    spifconf_init_subsystem();

    /* Register Eterm's context parsers. */
    spifconf_register_context("color", (ctx_handler_t) parse_color);
    spifconf_register_context("attributes", (ctx_handler_t) parse_attributes);
    spifconf_register_context("toggles", (ctx_handler_t) parse_toggles);
    spifconf_register_context("keyboard", (ctx_handler_t) parse_keyboard);
    spifconf_register_context("misc", (ctx_handler_t) parse_misc);
    spifconf_register_context("imageclasses", (ctx_handler_t) parse_imageclasses);
    spifconf_register_context("image", (ctx_handler_t) parse_image);
    spifconf_register_context("actions", (ctx_handler_t) parse_actions);
    spifconf_register_context("menu", (ctx_handler_t) parse_menu);
    spifconf_register_context("menuitem", (ctx_handler_t) parse_menuitem);
    spifconf_register_context("button_bar", (ctx_handler_t) parse_bbar);
    spifconf_register_context("xim", (ctx_handler_t) parse_xim);
    spifconf_register_context("multichar", (ctx_handler_t) parse_multichar);
    spifconf_register_context("escreen", (ctx_handler_t) parse_escreen);
}

/* Sync up options with our internal data after parsing options and configs */
void
post_parse(void)
{
    register int i;

#if DEBUG > 0
    if (DEBUG_LEVEL > DEBUG) {
        libast_print_warning("Requested debug level of %d exceeds compile-time maximum of %d\n", DEBUG_LEVEL, DEBUG);
    } else if (DEBUG_LEVEL > 0) {
        DPRINTF1(("Now running with debugging level of %d\n", DEBUG_LEVEL));
    }
#endif

    if (rs_scrollbar_type) {
        if (!strcasecmp(rs_scrollbar_type, "xterm")) {
#ifdef XTERM_SCROLLBAR
            scrollbar_set_type(SCROLLBAR_XTERM);
#else
            libast_print_error("Support for xterm scrollbars was not compiled in.  Sorry.\n");
#endif
        } else if (!strcasecmp(rs_scrollbar_type, "next")) {
#ifdef NEXT_SCROLLBAR
            scrollbar_set_type(SCROLLBAR_NEXT);
#else
            libast_print_error("Support for NeXT scrollbars was not compiled in.  Sorry.\n");
#endif
        } else if (!strcasecmp(rs_scrollbar_type, "motif")) {
#ifdef MOTIF_SCROLLBAR
            scrollbar_set_type(SCROLLBAR_MOTIF);
#else
            libast_print_error("Support for motif scrollbars was not compiled in.  Sorry.\n");
#endif
        } else {
            libast_print_error("Unrecognized scrollbar type \"%s\".\n", rs_scrollbar_type);
        }
    }
    if (rs_scrollbar_width) {
        scrollbar_set_width(rs_scrollbar_width);
    }

    /* set any defaults not already set */
    if (!rs_name) {
        if (rs_exec_args) {
            rs_name = STRDUP(rs_exec_args[0]);
        } else {
            rs_name = STRDUP(APL_NAME " " VERSION);
        }
    }
    if (!rs_title) {
        rs_title = rs_name;
    }
    if (!rs_iconName) {
        rs_iconName = rs_name;
    }
    if ((TermWin.saveLines = rs_saveLines) < 0) {
        TermWin.saveLines = SAVELINES;
    }
    /* no point having a scrollbar without having any scrollback! */
    if (!TermWin.saveLines) {
        BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_SCROLLBAR);
    }
#ifdef PRINTPIPE
    if (!rs_print_pipe) {
        rs_print_pipe = STRDUP(PRINTPIPE);
    }
#endif
#ifdef CUTCHAR_OPTION
    if (!rs_cutchars) {
        rs_cutchars = STRDUP(CUTCHARS);
    }
#endif

#ifndef NO_BOLDFONT
    if (!rs_font[0] && rs_boldFont) {
        rs_font[0] = rs_boldFont;
        rs_boldFont = NULL;
    }
#endif

    /* Add any fonts we got from the command line. */
    for (i = 0; i < NFONTS; i++) {
        if (rs_font[i]) {
            if (def_font_idx == 0) {
                eterm_font_add(&etfonts, rs_font[i], i);
                RESET_AND_ASSIGN(rs_font[i], NULL);
            } else {
                /* If they changed the default font index to something other than 0,
                   put rs_font[0] in that position.  Any indexes less than that are
                   reduced by one so that rs_font[1-n] (n being the default font
                   index) will be assigned to etfonts[0-(n-1)], and rs_font[0] will
                   be etfonts[n].  Anything higher than n stays as is. */
                eterm_font_add(&etfonts, rs_font[i], ((i == 0) ? def_font_idx : ((i <= def_font_idx) ? (i - 1) : i)));
                RESET_AND_ASSIGN(rs_font[i], NULL);
            }
        }
#ifdef MULTI_CHARSET
        if (rs_mfont[i]) {
            if (def_font_idx == 0) {
                eterm_font_add(&etmfonts, rs_mfont[i], i);
                RESET_AND_ASSIGN(rs_mfont[i], NULL);
            } else {
                eterm_font_add(&etmfonts, rs_mfont[i], ((i == 0) ? def_font_idx : ((i <= def_font_idx) ? (i - 1) : i)));
                RESET_AND_ASSIGN(rs_mfont[i], NULL);
            }
        } else if (!etmfonts[i]) {
            eterm_font_add(&etmfonts, etfonts[i], i);
        }
#endif
    }
    /* Make sure all fonts 0 through font_cnt are populated for both normal
       and multi-byte font structures. */
    for (; i < font_cnt; i++) {
        if (!etfonts[i]) {
            eterm_font_add(&etfonts, etfonts[def_font_idx], i);
        }
#ifdef MULTI_CHARSET
        if (!etmfonts[i]) {
            if (etmfonts[def_font_idx]) {
                eterm_font_add(&etmfonts, etmfonts[def_font_idx], i);
            } else {
                eterm_font_add(&etmfonts, etfonts[i], i);
            }
        }
#endif
    }
#ifdef MULTI_CHARSET
    if (rs_multichar_encoding) {
        set_multichar_encoding(rs_multichar_encoding);
    }
#endif

    if (rs_font_effects) {
        if (parse_font_fx(rs_font_effects) != 1) {
            libast_print_error("Syntax error in the font effects specified on the command line.\n");
        }
        RESET_AND_ASSIGN(rs_font_effects, NULL);
    }

    /* Clean up image stuff */
    for (i = 0; i < image_max; i++) {
        simage_t *simg;
        imlib_t *iml;

        if (images[i].norm) {
            simg = images[i].norm;
            iml = simg->iml;
            /* If we have a bevel but no border, use the bevel as a border. */
            if (iml->bevel && !(iml->border)) {
                iml->border = iml->bevel->edges;
            }
#ifdef PIXMAP_SUPPORT
            if (iml->im) {
                imlib_context_set_image(iml->im);
                update_cmod_tables(iml);
            }
#endif
            images[i].userdef = 1;
        } else {
            simg = images[i].norm = (simage_t *) MALLOC(sizeof(simage_t));
            simg->pmap = (pixmap_t *) MALLOC(sizeof(pixmap_t));
            simg->iml = (imlib_t *) MALLOC(sizeof(imlib_t));
            simg->fg = WhitePixel(Xdisplay, Xscreen);
            simg->bg = BlackPixel(Xdisplay, Xscreen);
            MEMSET(simg->pmap, 0, sizeof(pixmap_t));
            MEMSET(simg->iml, 0, sizeof(imlib_t));
            images[i].mode = MODE_IMAGE & ALLOW_IMAGE;
        }
        images[i].current = simg;
#ifdef PIXMAP_SUPPORT
        if (rs_pixmaps[i]) {
            reset_simage(images[i].norm, RESET_ALL_SIMG);
            load_image(rs_pixmaps[i], images[i].norm);
            FREE(rs_pixmaps[i]);        /* These are created by STRDUP() */
        }
#else
        /* Right now, solid mode is the only thing we can do without pixmap support. */
        images[i].mode = MODE_SOLID & ALLOW_SOLID;
#endif

        if (images[i].selected) {
            simage_t *norm_simg = images[i].norm;

            simg = images[i].selected;
            iml = simg->iml;
            /* If we have a bevel but no border, use the bevel as a border. */
            if (iml->bevel && !(iml->border)) {
                iml->border = iml->bevel->edges;
            }
            /* If normal has an image but we don't, copy it. */
            if (!(simg->iml->im) && (norm_simg->iml->im)) {
                simg->iml->im = norm_simg->iml->im;
                *(simg->pmap) = *(norm_simg->pmap);
            }
            if (simg->fg == 0 && simg->bg == 0) {
                simg->fg = norm_simg->fg;
                simg->bg = norm_simg->bg;
            }
#ifdef PIXMAP_SUPPORT
            if (iml->im) {
                imlib_context_set_image(iml->im);
                update_cmod_tables(iml);
            }
#endif
        } else {
            D_PIXMAP(("No \"selected\" state for image %s.  Setting fallback to the normal state.\n", get_image_type(i)));
            images[i].selected = images[i].norm;
        }
        if (images[i].clicked) {
            simage_t *norm_simg = images[i].norm;

            simg = images[i].clicked;
            iml = simg->iml;
            /* If we have a bevel but no border, use the bevel as a border. */
            if (iml->bevel && !(iml->border)) {
                iml->border = iml->bevel->edges;
            }
            /* If normal has an image but we don't, copy it. */
            if (!(simg->iml->im) && (norm_simg->iml->im)) {
                simg->iml->im = norm_simg->iml->im;
                *(simg->pmap) = *(norm_simg->pmap);
            }
            if (simg->fg == 0 && simg->bg == 0) {
                simg->fg = norm_simg->fg;
                simg->bg = norm_simg->bg;
            }
#ifdef PIXMAP_SUPPORT
            if (iml->im) {
                imlib_context_set_image(iml->im);
                update_cmod_tables(iml);
            }
#endif
        } else {
            D_PIXMAP(("No \"clicked\" state for image %s.  Setting fallback to the selected state.\n", get_image_type(i)));
            images[i].clicked = images[i].selected;
        }
        if (images[i].disabled) {
            simage_t *norm_simg = images[i].norm;

            simg = images[i].disabled;
            iml = simg->iml;
            /* If we have a bevel but no border, use the bevel as a border. */
            if (iml->bevel && !(iml->border)) {
                iml->border = iml->bevel->edges;
            }
            /* If normal has an image but we don't, copy it. */
            if (!(simg->iml->im) && (norm_simg->iml->im)) {
                simg->iml->im = norm_simg->iml->im;
                *(simg->pmap) = *(norm_simg->pmap);
            }
            if (simg->fg == 0 && simg->bg == 0) {
                simg->fg = norm_simg->fg;
                simg->bg = norm_simg->bg;
            }
#ifdef PIXMAP_SUPPORT
            if (iml->im) {
                imlib_context_set_image(iml->im);
                update_cmod_tables(iml);
            }
#endif
        } else {
            D_PIXMAP(("No \"disabled\" state for image %s.  Setting fallback to the normal state.\n", get_image_type(i)));
            images[i].disabled = images[i].norm;
        }
        if ((BITFIELD_IS_SET(image_options, IMAGE_OPTIONS_TRANS)) && (image_mode_is(i, ALLOW_TRANS))) {
            D_PIXMAP(("Detected transparency option.  Enabling transparency on image %s\n", get_image_type(i)));
            image_set_mode(i, MODE_TRANS);
        } else if ((BITFIELD_IS_SET(image_options, IMAGE_OPTIONS_VIEWPORT)) && (image_mode_is(i, ALLOW_VIEWPORT))) {
            D_PIXMAP(("Detected viewport option.  Enabling viewport mode on image %s\n", get_image_type(i)));
            image_set_mode(i, MODE_VIEWPORT);
        }
    }
    if (images[image_bg].norm->fg || images[image_bg].norm->bg) {
        /* They specified their colors here, so copy them to the right place. */
        PixColors[fgColor] = images[image_bg].norm->fg;
        PixColors[bgColor] = images[image_bg].norm->bg;
    }

    /* rs_buttonbars is set to 1.  If it stays 1, the option was never
       specified.  If specified, it will either become 3 (on) or 0 (off). */
    if (rs_buttonbars != 1) {
        if (rs_buttonbars) {
            FOREACH_BUTTONBAR(bbar_set_visible(bbar, 1););
        } else {
            FOREACH_BUTTONBAR(bbar_set_visible(bbar, 0););
        }
        rs_buttonbars = 1;      /* Reset for future use. */
    }
    /* Update buttonbar sizes based on new imageclass info. */
    bbar_resize_all(-1);

#ifdef PIXMAP_SUPPORT
    /* Support the deprecated forms by converting the syntax to the new system */
    if (rs_shade != 0) {
        char buff[10];

        sprintf(buff, "0x%03x", ((100 - rs_shade) << 8) / 100);
        rs_cmod_image = STRDUP(buff);
        D_PIXMAP(("--shade value of %d converted to cmod value of %s\n", rs_shade, rs_cmod_image));
    }
    if (rs_tint) {
        char buff[10];
        unsigned long r, g, b, t;

        if (!isdigit(*rs_tint)) {
            t = get_tint_by_color_name(rs_tint);
        } else {
            t = (unsigned long) strtoul(rs_tint, (char **) NULL, 0);
            D_PIXMAP(("Got numerical tint 0x%06x\n", t));
        }
        if (t != 0xffffff) {
            r = (t & 0xff0000) >> 16;
            if (r != 0xff) {
                sprintf(buff, "0x%03lx", r);
                rs_cmod_red = STRDUP(buff);
            }
            g = (t & 0xff00) >> 8;
            if (g != 0xff) {
                sprintf(buff, "0x%03lx", g);
                rs_cmod_green = STRDUP(buff);
            }
            b = t & 0xff;
            if (b != 0xff) {
                sprintf(buff, "0x%03lx", b);
                rs_cmod_blue = STRDUP(buff);
            }
        }
        FREE(rs_tint);
    }
    if (rs_cmod_image) {
        unsigned char n = spiftool_num_words(rs_cmod_image);
        imlib_t *iml = images[image_bg].norm->iml;

        if (iml->mod) {
            free_colormod(iml->mod);
        }
        iml->mod = create_colormod();
        iml->mod->brightness = (int) strtol(rs_cmod_image, (char **) NULL, 0);
        if (n > 1) {
            iml->mod->contrast = (int) strtol(spiftool_get_pword(2, rs_cmod_image), (char **) NULL, 0);
        }
        if (n > 2) {
            iml->mod->gamma = (int) strtol(spiftool_get_pword(3, rs_cmod_image), (char **) NULL, 0);
        }
        D_PIXMAP(("From image cmod string %s to brightness %d, contrast %d, and gamma %d\n", rs_cmod_image,
                  iml->mod->brightness, iml->mod->contrast, iml->mod->gamma));
        FREE(rs_cmod_image);
    }
    if (rs_cmod_red) {
        unsigned char n = spiftool_num_words(rs_cmod_red);
        imlib_t *iml = images[image_bg].norm->iml;

        if (iml->rmod) {
            free_colormod(iml->rmod);
        }
        iml->rmod = create_colormod();
        iml->rmod->brightness = (int) strtol(rs_cmod_red, (char **) NULL, 0);
        if (n > 1) {
            iml->rmod->contrast = (int) strtol(spiftool_get_pword(2, rs_cmod_red), (char **) NULL, 0);
        }
        if (n > 2) {
            iml->rmod->gamma = (int) strtol(spiftool_get_pword(3, rs_cmod_red), (char **) NULL, 0);
        }
        D_PIXMAP(("From red cmod string %s to brightness %d, contrast %d, and gamma %d\n", rs_cmod_red,
                  iml->rmod->brightness, iml->rmod->contrast, iml->rmod->gamma));
        FREE(rs_cmod_red);
        update_cmod(iml->rmod);
    }
    if (rs_cmod_green) {
        unsigned char n = spiftool_num_words(rs_cmod_green);
        imlib_t *iml = images[image_bg].norm->iml;

        if (iml->gmod) {
            free_colormod(iml->gmod);
        }
        iml->gmod = create_colormod();
        iml->gmod->brightness = (int) strtol(rs_cmod_green, (char **) NULL, 0);
        if (n > 1) {
            iml->gmod->contrast = (int) strtol(spiftool_get_pword(2, rs_cmod_green), (char **) NULL, 0);
        }
        if (n > 2) {
            iml->gmod->gamma = (int) strtol(spiftool_get_pword(3, rs_cmod_green), (char **) NULL, 0);
        }
        D_PIXMAP(("From green cmod string %s to brightness %d, contrast %d, and gamma %d\n", rs_cmod_green,
                  iml->gmod->brightness, iml->gmod->contrast, iml->gmod->gamma));
        FREE(rs_cmod_green);
        update_cmod(iml->gmod);
    }
    if (rs_cmod_blue) {
        unsigned char n = spiftool_num_words(rs_cmod_blue);
        imlib_t *iml = images[image_bg].norm->iml;

        if (iml->bmod) {
            free_colormod(iml->bmod);
        }
        iml->bmod = create_colormod();
        iml->bmod->brightness = (int) strtol(rs_cmod_blue, (char **) NULL, 0);
        if (n > 1) {
            iml->bmod->contrast = (int) strtol(spiftool_get_pword(2, rs_cmod_blue), (char **) NULL, 0);
        }
        if (n > 2) {
            iml->bmod->gamma = (int) strtol(spiftool_get_pword(3, rs_cmod_blue), (char **) NULL, 0);
        }
        D_PIXMAP(("From blue cmod string %s to brightness %d, contrast %d, and gamma %d\n", rs_cmod_blue,
                  iml->bmod->brightness, iml->bmod->contrast, iml->bmod->gamma));
        FREE(rs_cmod_blue);
        update_cmod(iml->bmod);
    }
    if (images[image_bg].norm->iml->im) {
        imlib_context_set_image(images[image_bg].norm->iml->im);
        update_cmod_tables(images[image_bg].norm->iml);
    }

    if (rs_cache_size == (unsigned long) -1) {
        imlib_set_cache_size(0);
    } else {
        imlib_set_cache_size(rs_cache_size);
    }
#endif

    if (rs_opacity < 0x100) {
        rs_opacity |= (rs_opacity << 24) | (rs_opacity << 16) | (rs_opacity << 8);
    } else {
        rs_opacity = 0xffffffff;
    }

    if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_REVERSE_VIDEO)) {
        char *tmp;

        /* swap foreground/background colors */

        tmp = rs_color[fgColor];
        rs_color[fgColor] = rs_color[bgColor];
        rs_color[bgColor] = tmp;

        tmp = def_colorName[fgColor];
        def_colorName[fgColor] = def_colorName[bgColor];
        def_colorName[bgColor] = tmp;
    }

    if (rs_meta_mod) {
        MetaMask = modmasks[rs_meta_mod - 1];
    }
    if (rs_alt_mod) {
        AltMask = modmasks[rs_alt_mod - 1];
    }
    if (rs_numlock_mod) {
        NumLockMask = modmasks[rs_numlock_mod - 1];
    }
#ifdef BACKGROUND_CYCLING_SUPPORT
    if (rs_anim_pixmap_list) {
        rs_anim_delay = strtoul(rs_anim_pixmap_list, (char **) NULL, 0);
        fflush(stdout);
        if (rs_anim_delay == 0) {
            FREE(rs_anim_pixmap_list);
            rs_anim_pixmap_list = NULL;
        } else {
            char *w1, *h1, *temp;
            unsigned long w, h;
            int count;

            count = spiftool_num_words(rs_anim_pixmap_list) - 1; /* -1 for the delay */
            rs_anim_pixmaps = (char **) MALLOC(sizeof(char *) * (count + 1));

            for (i = 0; i < count; i++) {
                temp = spiftool_get_word(i + 2, rs_anim_pixmap_list);    /* +2 rather than +1 to account for the delay */
                if (!temp)
                    break;
                if (spiftool_num_words(temp) != 3) {
                    if (spiftool_num_words(temp) == 1) {
                        rs_anim_pixmaps[i] = temp;
                    }
                } else {
                    w1 = spiftool_get_pword(1, temp);
                    h1 = spiftool_get_pword(2, temp);
                    w = strtol(w1, (char **) NULL, 0);
                    h = strtol(h1, (char **) NULL, 0);
                    if (w || h) {
                        rs_anim_pixmaps[i] = spiftool_get_word(3, temp);
                        rs_anim_pixmaps[i] = (char *) REALLOC(rs_anim_pixmaps[i], strlen(rs_anim_pixmaps[i]) + 9);
                        strcat(rs_anim_pixmaps[i], "@100x100");
                    } else {
                        rs_anim_pixmaps[i] = spiftool_get_word(3, temp);
                        rs_anim_pixmaps[i] = (char *) REALLOC(rs_anim_pixmaps[i], strlen(rs_anim_pixmaps[i]) + 5);
                        strcat(rs_anim_pixmaps[i], "@0x0");
                    }
                    FREE(temp);
                }
            }
            rs_anim_pixmaps[count] = NULL;
            FREE(rs_anim_pixmap_list);
        }
    } else {
        rs_anim_delay = 0;
    }
#endif

    if (rs_pipe_name) {
        struct stat fst;

        if (lstat(rs_pipe_name, &fst) != 0) {
            libast_print_error("Unable to stat console pipe \"%s\" -- %s\n", rs_pipe_name, strerror(errno));
        } else {
            if (S_ISREG(fst.st_mode) || S_ISDIR(fst.st_mode)) {
                libast_print_error("Directories and regular files are not valid console pipes.  Sorry.\n");
            } else {
                pipe_fd = open(rs_pipe_name, O_RDONLY | O_NDELAY | O_NOCTTY);
                if (pipe_fd < 0) {
                    libast_print_error("Unable to open console pipe -- %s\n", strerror(errno));
                }
            }
        }
    }
}

unsigned char
save_config(char *path, unsigned char save_theme)
{
    register FILE *fp;
    register short i;
    char *tmp_str, dt_stamp[50];
    time_t cur_time = time(NULL);
    struct tm *cur_tm;
    struct stat fst;
    simage_t *simg;
    action_t *action;
    buttonbar_t *bbar;

    D_OPTIONS(("Saving %s config to \"%s\"\n", (save_theme ? "theme" : "user"), NONULL(path)));

    cur_tm = localtime(&cur_time);

    if (save_theme) {
        if (!path) {
            path = (char *) MALLOC(PATH_MAX + 1);
            strncpy(path, (theme_dir ? theme_dir : PKGDATADIR "/themes/Eterm"), PATH_MAX - sizeof("/" THEME_CFG));
            path[PATH_MAX] = 0;
            if (stat(path, &fst) || !S_ISDIR(fst.st_mode) || !CAN_WRITE(fst)) {
                char *tmp = NULL;

                D_OPTIONS(("Problem with \"%s\".  S_ISDIR == %d, CAN_WRITE == %d\n", path, S_ISDIR(fst.st_mode), CAN_WRITE(fst)));
                if (theme_dir) {
                    tmp = strrchr(theme_dir, '/');
                    if (tmp) {
                        *tmp++ = 0;
                    }
                }
                snprintf(path, PATH_MAX, "%s/.Eterm/themes/%s", getenv("HOME"), (tmp ? tmp : "Eterm"));
                D_OPTIONS(("Trying \"%s\" instead, tmp == \"%s\"\n", path, tmp));
                if (tmp) {
                    *(--tmp) = '/';
                }
                if (!mkdirhier(path) || (stat(path, &fst) && !CAN_WRITE(fst))) {
                    libast_print_error("I couldn't write to \"%s\" or \"%s\".  I give up.",
                                (theme_dir ? theme_dir : PKGDATADIR "/themes/Eterm\n"), path);
                    return errno;
                }
            }
            strcat(path, "/" THEME_CFG);
            D_OPTIONS(("Final path is \"%s\"\n", path));
            path[PATH_MAX] = 0;
        }
    } else {
        if (!path) {
            path = (char *) MALLOC(PATH_MAX + 1);
            strncpy(path, (user_dir ? user_dir : PKGDATADIR "/themes/Eterm"), PATH_MAX - sizeof("/" USER_CFG));
            path[PATH_MAX] = 0;
            if (stat(path, &fst) || !S_ISDIR(fst.st_mode) || !CAN_WRITE(fst)) {
                char *tmp = NULL;

                D_OPTIONS(("Problem with \"%s\".  S_ISDIR == %d, CAN_WRITE == %d\n", path, S_ISDIR(fst.st_mode), CAN_WRITE(fst)));
                if (user_dir) {
                    tmp = strrchr(user_dir, '/');
                    if (tmp) {
                        *tmp++ = 0;
                    }
                }
                snprintf(path, PATH_MAX, "%s/.Eterm/themes/%s", getenv("HOME"), (tmp ? tmp : "Eterm"));
                D_OPTIONS(("Trying \"%s\" instead, tmp == \"%s\"\n", path, tmp));
                if (tmp) {
                    *(--tmp) = '/';
                }
                if (!mkdirhier(path) || (stat(path, &fst) && !CAN_WRITE(fst))) {
                    libast_print_error("I couldn't write to \"%s\" or \"%s\".  I give up.",
                                (user_dir ? user_dir : PKGDATADIR "/themes/Eterm\n"), path);
                    return errno;
                }
            }
            strcat(path, "/" USER_CFG);
            D_OPTIONS(("Final path is \"%s\"\n", path));
            path[PATH_MAX] = 0;
        }
    }
    if (!lstat(path, &fst)) {
        char bak_path[PATH_MAX], timestamp[16];

        /* File exists.  Make a backup. */
        strftime(timestamp, 16, "%Y%m%d.%H%M%S", cur_tm);
        snprintf(bak_path, PATH_MAX - 1, "%s.%s", path, timestamp);
        link(path, bak_path);
        unlink(path);
    }
    if (!(fp = fopen(path, "w"))) {
        libast_print_error("Unable to save configuration to file \"%s\" -- %s\n", path, strerror(errno));
        return errno;
    }
    strftime(dt_stamp, 50, "%Y/%m/%d at %X", cur_tm);
    fprintf(fp, "<" APL_NAME "-" VERSION ">\n");
    fprintf(fp, "# Eterm Configuration File\n");
    fprintf(fp, "# Automatically generated by " APL_NAME "-" VERSION " on %s\n", dt_stamp);

    fprintf(fp, "begin color\n");
    fprintf(fp, "    foreground %s\n", COLOR_NAME(fgColor));
    fprintf(fp, "    background %s\n", COLOR_NAME(bgColor));
    fprintf(fp, "    cursor %s\n", COLOR_NAME(cursorColor));
    fprintf(fp, "    cursor_text %s\n", COLOR_NAME(cursorColor2));
    fprintf(fp, "    pointer %s\n", COLOR_NAME(pointerColor));
    fprintf(fp, "    video normal\n");
    for (i = 0; i < 16; i++) {
        fprintf(fp, "    color %d %s\n", i, COLOR_NAME(minColor + i));
    }
#ifndef NO_BOLDUNDERLINE
    if (rs_color[colorBD]) {
        fprintf(fp, "    color bd %s\n", COLOR_NAME(colorBD));
    }
    if (rs_color[colorUL]) {
        fprintf(fp, "    color ul %s\n", COLOR_NAME(colorUL));
    }
#endif
    fprintf(fp, "end color\n\n");

    fprintf(fp, "begin attributes\n");
    if (save_theme) {
        if (rs_geometry) {
            fprintf(fp, "    geometry %s\n", rs_geometry);
        }
        XFetchName(Xdisplay, TermWin.parent, &tmp_str);
        fprintf(fp, "    title %s\n", tmp_str);
        fprintf(fp, "    name %s\n", rs_name);
        XGetIconName(Xdisplay, TermWin.parent, &tmp_str);
        fprintf(fp, "    iconname %s\n", tmp_str);
        if (rs_desktop != -1) {
            fprintf(fp, "    desktop %d\n", rs_desktop);
        }
    }
    fprintf(fp, "    scrollbar_type %s\n",
            (scrollbar_get_type() == SCROLLBAR_XTERM ? "xterm" : (scrollbar_get_type() == SCROLLBAR_MOTIF ? "motif" : "next")));
    fprintf(fp, "    scrollbar_width %d\n", scrollbar_anchor_width());
    fprintf(fp, "    font default %u\n", (unsigned int) font_idx);
    fprintf(fp, "    font proportional %d\n", ((BITFIELD_IS_SET(vt_options, VT_OPTIONS_PROPORTIONAL)) ? 1 : 0));
    for (i = 0; i < font_cnt; i++) {
        if (etfonts[i]) {
            fprintf(fp, "    font %d %s\n", i, etfonts[i]);
        }
    }
#ifndef NO_BOLDFONT
    if (rs_boldFont) {
        fprintf(fp, "    font bold %s\n", rs_boldFont);
    }
#endif
    if (fshadow.do_shadow) {
        const char *corners[] = {
            "top_left",
            "top",
            "top_right",
            "left",
            "right",
            "bottom_left",
            "bottom",
            "bottom_right"
        };
        unsigned char shad = 0;

        fprintf(fp, "    font effects");
        for (i = 0; i < 8; i++) {
            if (fshadow.shadow[i]) {
                fprintf(fp, " %s 0x%08x", corners[i], (unsigned int) fshadow.color[i]);
                shad = 1;
            }
        }
        if (shad) {
            fprintf(fp, "\n");
        } else {
            fprintf(fp, " none\n");
        }
    } else {
        fprintf(fp, "    font effects none\n");
    }
    fprintf(fp, "end attributes\n\n");

    if (save_theme) {
        fprintf(fp, "begin imageclasses\n");
        fprintf(fp, "    path \"%s\"\n", rs_path);
#ifdef PIXMAP_SUPPORT
        if (rs_icon) {
            fprintf(fp, "    icon %s\n", rs_icon);
        }
        if (rs_anim_delay) {
            /* FIXME:  Do something here! */
        }
#endif
        for (i = 0; i < image_max; i++) {
            fprintf(fp, "    begin image\n");
            switch (i) {
                case image_bg:
                    fprintf(fp, "      type background\n");
                    break;
                case image_sb:
                    fprintf(fp, "      type trough\n");
                    break;
                case image_sa:
                    fprintf(fp, "      type anchor\n");
                    break;
                case image_st:
                    fprintf(fp, "      type thumb\n");
                    break;
                case image_up:
                    fprintf(fp, "      type up_arrow\n");
                    break;
                case image_down:
                    fprintf(fp, "      type down_arrow\n");
                    break;
                case image_left:
                    fprintf(fp, "      type left_arrow\n");
                    break;
                case image_right:
                    fprintf(fp, "      type right_arrow\n");
                    break;
                case image_menu:
                    fprintf(fp, "      type menu\n");
                    break;
                case image_menuitem:
                    fprintf(fp, "      type menuitem\n");
                    break;
                case image_submenu:
                    fprintf(fp, "      type submenu\n");
                    break;
                case image_button:
                    fprintf(fp, "      type button\n");
                    break;
                case image_bbar:
                    fprintf(fp, "      type button_bar\n");
                    break;
                case image_gbar:
                    fprintf(fp, "      type grab_bar\n");
                    break;
                case image_dialog:
                    fprintf(fp, "      type dialog_box\n");
                    break;
            }
            fprintf(fp, "      mode ");
            switch (images[i].mode & MODE_MASK) {
                case MODE_IMAGE:
                    fprintf(fp, "image");
                    break;
                case MODE_TRANS:
                    fprintf(fp, "trans");
                    break;
                case MODE_VIEWPORT:
                    fprintf(fp, "viewport");
                    break;
                case MODE_AUTO:
                    fprintf(fp, "auto");
                    break;
                default:
                    fprintf(fp, "solid");
                    break;
            }
            if (images[i].mode & ALLOW_MASK) {
                fprintf(fp, " allow");
                if (image_mode_is(i, ALLOW_IMAGE)) {
                    fprintf(fp, " image");
                }
                if (image_mode_is(i, ALLOW_TRANS)) {
                    fprintf(fp, " trans");
                }
                if (image_mode_is(i, ALLOW_VIEWPORT)) {
                    fprintf(fp, " viewport");
                }
                if (image_mode_is(i, ALLOW_AUTO)) {
                    fprintf(fp, " auto");
                }
            }
            fprintf(fp, "\n");

            /* Now save each state. */
            simg = images[i].norm;
#ifdef PIXMAP_SUPPORT
            if (simg->iml->im) {
                imlib_context_set_image(simg->iml->im);
            }
#endif
            fprintf(fp, "      state normal\n");
            if (simg->fg || simg->bg) {
                fprintf(fp, "      color 0x%08x 0x%08x\n", (unsigned int) simg->fg, (unsigned int) simg->bg);
            }
#ifdef PIXMAP_SUPPORT
            if (simg->iml->im) {
                fprintf(fp, "      file %s\n", NONULL(imlib_image_get_filename()));
            }
            fprintf(fp, "      geom %hdx%hd+%hd+%hd", simg->pmap->w, simg->pmap->h, simg->pmap->x, simg->pmap->y);
            if (simg->pmap->op & OP_TILE) {
                fprintf(fp, ":tiled");
            }
            if ((simg->pmap->op & OP_SCALE) || ((simg->pmap->op & OP_HSCALE) && (simg->pmap->op & OP_VSCALE))) {
                fprintf(fp, ":scaled");
            } else if (simg->pmap->op & OP_HSCALE) {
                fprintf(fp, ":hscaled");
            } else if (simg->pmap->op & OP_VSCALE) {
                fprintf(fp, ":vscaled");
            }
            if (simg->pmap->op & OP_PROPSCALE) {
                fprintf(fp, ":propscaled");
            }
            fprintf(fp, "\n");
            if (simg->iml->mod) {
                fprintf(fp, "      colormod image 0x%02x 0x%02x 0x%02x\n", simg->iml->mod->brightness, simg->iml->mod->contrast,
                        simg->iml->mod->gamma);
            }
            if (simg->iml->rmod) {
                fprintf(fp, "      colormod red 0x%02x 0x%02x 0x%02x\n", simg->iml->rmod->brightness, simg->iml->rmod->contrast,
                        simg->iml->rmod->gamma);
            }
            if (simg->iml->gmod) {
                fprintf(fp, "      colormod green 0x%02x 0x%02x 0x%02x\n", simg->iml->gmod->brightness, simg->iml->gmod->contrast,
                        simg->iml->gmod->gamma);
            }
            if (simg->iml->bmod) {
                fprintf(fp, "      colormod blue 0x%02x 0x%02x 0x%02x\n", simg->iml->bmod->brightness, simg->iml->bmod->contrast,
                        simg->iml->bmod->gamma);
            }
#endif
            if (simg->iml->border) {
                fprintf(fp, "      border %hu %hu %hu %hu\n", simg->iml->border->left, simg->iml->border->right,
                        simg->iml->border->top, simg->iml->border->bottom);
            }
            if (simg->iml->bevel) {
                fprintf(fp, "      bevel %s %hu %hu %hu %hu\n", ((simg->iml->bevel->up) ? "up" : "down"),
                        simg->iml->bevel->edges->left, simg->iml->bevel->edges->right, simg->iml->bevel->edges->top,
                        simg->iml->bevel->edges->bottom);
            }
            if (simg->iml->pad) {
                fprintf(fp, "      padding %hu %hu %hu %hu\n", simg->iml->pad->left, simg->iml->pad->right, simg->iml->pad->top,
                        simg->iml->pad->bottom);
            }

            /* Selected state */
            if (images[i].selected != images[i].norm) {
                simg = images[i].selected;
#ifdef PIXMAP_SUPPORT
                if (simg->iml->im) {
                    imlib_context_set_image(simg->iml->im);
                }
#endif
                fprintf(fp, "      state selected\n");
                if (simg->fg || simg->bg) {
                    fprintf(fp, "      color 0x%08x 0x%08x\n", (unsigned int) simg->fg, (unsigned int) simg->bg);
                }
#ifdef PIXMAP_SUPPORT
                if (simg->iml->im) {
                    fprintf(fp, "      file %s\n", NONULL(imlib_image_get_filename()));
                }
                fprintf(fp, "      geom %hdx%hd+%hd+%hd", simg->pmap->w, simg->pmap->h, simg->pmap->x, simg->pmap->y);
                if (simg->pmap->op & OP_TILE) {
                    fprintf(fp, ":tiled");
                }
                if ((simg->pmap->op & OP_SCALE) || ((simg->pmap->op & OP_HSCALE) && (simg->pmap->op & OP_VSCALE))) {
                    fprintf(fp, ":scaled");
                } else if (simg->pmap->op & OP_HSCALE) {
                    fprintf(fp, ":hscaled");
                } else if (simg->pmap->op & OP_VSCALE) {
                    fprintf(fp, ":vscaled");
                }
                if (simg->pmap->op & OP_PROPSCALE) {
                    fprintf(fp, ":propscaled");
                }
                fprintf(fp, "\n");
                if (simg->iml->mod) {
                    fprintf(fp, "      colormod image 0x%02x 0x%02x 0x%02x\n", simg->iml->mod->brightness, simg->iml->mod->contrast,
                            simg->iml->mod->gamma);
                }
                if (simg->iml->rmod) {
                    fprintf(fp, "      colormod red 0x%02x 0x%02x 0x%02x\n", simg->iml->rmod->brightness, simg->iml->rmod->contrast,
                            simg->iml->rmod->gamma);
                }
                if (simg->iml->gmod) {
                    fprintf(fp, "      colormod green 0x%02x 0x%02x 0x%02x\n", simg->iml->gmod->brightness,
                            simg->iml->gmod->contrast, simg->iml->gmod->gamma);
                }
                if (simg->iml->bmod) {
                    fprintf(fp, "      colormod blue 0x%02x 0x%02x 0x%02x\n", simg->iml->bmod->brightness,
                            simg->iml->bmod->contrast, simg->iml->bmod->gamma);
                }
#endif
                if (simg->iml->border) {
                    fprintf(fp, "      border %hu %hu %hu %hu\n", simg->iml->border->left, simg->iml->border->right,
                            simg->iml->border->top, simg->iml->border->bottom);
                }
                if (simg->iml->bevel) {
                    fprintf(fp, "      bevel %s %hu %hu %hu %hu\n", ((simg->iml->bevel->up) ? "up" : "down"),
                            simg->iml->bevel->edges->left, simg->iml->bevel->edges->right, simg->iml->bevel->edges->top,
                            simg->iml->bevel->edges->bottom);
                }
                if (simg->iml->pad) {
                    fprintf(fp, "      padding %hu %hu %hu %hu\n", simg->iml->pad->left, simg->iml->pad->right, simg->iml->pad->top,
                            simg->iml->pad->bottom);
                }
            }

            /* Clicked state */
            if (images[i].clicked != images[i].norm) {
                simg = images[i].clicked;
#ifdef PIXMAP_SUPPORT
                if (simg->iml->im) {
                    imlib_context_set_image(simg->iml->im);
                }
#endif
                fprintf(fp, "      state clicked\n");
                if (simg->fg || simg->bg) {
                    fprintf(fp, "      color 0x%08x 0x%08x\n", (unsigned int) simg->fg, (unsigned int) simg->bg);
                }
#ifdef PIXMAP_SUPPORT
                if (simg->iml->im) {
                    fprintf(fp, "      file %s\n", NONULL(imlib_image_get_filename()));
                }
                fprintf(fp, "      geom %hdx%hd+%hd+%hd", simg->pmap->w, simg->pmap->h, simg->pmap->x, simg->pmap->y);
                if (simg->pmap->op & OP_TILE) {
                    fprintf(fp, ":tiled");
                }
                if ((simg->pmap->op & OP_SCALE) || ((simg->pmap->op & OP_HSCALE) && (simg->pmap->op & OP_VSCALE))) {
                    fprintf(fp, ":scaled");
                } else if (simg->pmap->op & OP_HSCALE) {
                    fprintf(fp, ":hscaled");
                } else if (simg->pmap->op & OP_VSCALE) {
                    fprintf(fp, ":vscaled");
                }
                if (simg->pmap->op & OP_PROPSCALE) {
                    fprintf(fp, ":propscaled");
                }
                fprintf(fp, "\n");
                if (simg->iml->mod) {
                    fprintf(fp, "      colormod image 0x%02x 0x%02x 0x%02x\n", simg->iml->mod->brightness, simg->iml->mod->contrast,
                            simg->iml->mod->gamma);
                }
                if (simg->iml->rmod) {
                    fprintf(fp, "      colormod red 0x%02x 0x%02x 0x%02x\n", simg->iml->rmod->brightness, simg->iml->rmod->contrast,
                            simg->iml->rmod->gamma);
                }
                if (simg->iml->gmod) {
                    fprintf(fp, "      colormod green 0x%02x 0x%02x 0x%02x\n", simg->iml->gmod->brightness,
                            simg->iml->gmod->contrast, simg->iml->gmod->gamma);
                }
                if (simg->iml->bmod) {
                    fprintf(fp, "      colormod blue 0x%02x 0x%02x 0x%02x\n", simg->iml->bmod->brightness,
                            simg->iml->bmod->contrast, simg->iml->bmod->gamma);
                }
#endif
                if (simg->iml->border) {
                    fprintf(fp, "      border %hu %hu %hu %hu\n", simg->iml->border->left, simg->iml->border->right,
                            simg->iml->border->top, simg->iml->border->bottom);
                }
                if (simg->iml->bevel) {
                    fprintf(fp, "      bevel %s %hu %hu %hu %hu\n", ((simg->iml->bevel->up) ? "up" : "down"),
                            simg->iml->bevel->edges->left, simg->iml->bevel->edges->right, simg->iml->bevel->edges->top,
                            simg->iml->bevel->edges->bottom);
                }
                if (simg->iml->pad) {
                    fprintf(fp, "      padding %hu %hu %hu %hu\n", simg->iml->pad->left, simg->iml->pad->right, simg->iml->pad->top,
                            simg->iml->pad->bottom);
                }
            }

            /* Disabled state */
            if (images[i].disabled != images[i].norm) {
                simg = images[i].disabled;
#ifdef PIXMAP_SUPPORT
                if (simg->iml->im) {
                    imlib_context_set_image(simg->iml->im);
                }
#endif
                fprintf(fp, "      state disabled\n");
                if (simg->fg || simg->bg) {
                    fprintf(fp, "      color 0x%08x 0x%08x\n", (unsigned int) simg->fg, (unsigned int) simg->bg);
                }
#ifdef PIXMAP_SUPPORT
                if (simg->iml->im) {
                    fprintf(fp, "      file %s\n", NONULL(imlib_image_get_filename()));
                }
                fprintf(fp, "      geom %hdx%hd+%hd+%hd", simg->pmap->w, simg->pmap->h, simg->pmap->x, simg->pmap->y);
                if (simg->pmap->op & OP_TILE) {
                    fprintf(fp, ":tiled");
                }
                if ((simg->pmap->op & OP_SCALE) || ((simg->pmap->op & OP_HSCALE) && (simg->pmap->op & OP_VSCALE))) {
                    fprintf(fp, ":scaled");
                } else if (simg->pmap->op & OP_HSCALE) {
                    fprintf(fp, ":hscaled");
                } else if (simg->pmap->op & OP_VSCALE) {
                    fprintf(fp, ":vscaled");
                }
                if (simg->pmap->op & OP_PROPSCALE) {
                    fprintf(fp, ":propscaled");
                }
                fprintf(fp, "\n");
                if (simg->iml->mod) {
                    fprintf(fp, "      colormod image 0x%02x 0x%02x 0x%02x\n", simg->iml->mod->brightness, simg->iml->mod->contrast,
                            simg->iml->mod->gamma);
                }
                if (simg->iml->rmod) {
                    fprintf(fp, "      colormod red 0x%02x 0x%02x 0x%02x\n", simg->iml->rmod->brightness, simg->iml->rmod->contrast,
                            simg->iml->rmod->gamma);
                }
                if (simg->iml->gmod) {
                    fprintf(fp, "      colormod green 0x%02x 0x%02x 0x%02x\n", simg->iml->gmod->brightness,
                            simg->iml->gmod->contrast, simg->iml->gmod->gamma);
                }
                if (simg->iml->bmod) {
                    fprintf(fp, "      colormod blue 0x%02x 0x%02x 0x%02x\n", simg->iml->bmod->brightness,
                            simg->iml->bmod->contrast, simg->iml->bmod->gamma);
                }
#endif
                if (simg->iml->border) {
                    fprintf(fp, "      border %hu %hu %hu %hu\n", simg->iml->border->left, simg->iml->border->right,
                            simg->iml->border->top, simg->iml->border->bottom);
                }
                if (simg->iml->bevel) {
                    fprintf(fp, "      bevel %s %hu %hu %hu %hu\n", ((simg->iml->bevel->up) ? "up" : "down"),
                            simg->iml->bevel->edges->left, simg->iml->bevel->edges->right, simg->iml->bevel->edges->top,
                            simg->iml->bevel->edges->bottom);
                }
                if (simg->iml->pad) {
                    fprintf(fp, "      padding %hu %hu %hu %hu\n", simg->iml->pad->left, simg->iml->pad->right, simg->iml->pad->top,
                            simg->iml->pad->bottom);
                }
            }
            fprintf(fp, "    end image\n");
        }
        fprintf(fp, "end imageclasses\n\n");

        for (i = 0; i < menu_list->nummenus; i++) {
            menu_t *menu = menu_list->menus[i];
            unsigned short j;

            fprintf(fp, "begin menu\n");
            fprintf(fp, "    title \"%s\"\n", menu->title);
            if (menu->font) {
                unsigned long tmp;

                if ((XGetFontProperty(menu->font, XA_FONT_NAME, &tmp)) == True) {
                    fprintf(fp, "    font '%s'\n", ((char *) tmp));
                }
            }
            for (j = 0; j < menu->numitems; j++) {
                menuitem_t *item = menu->items[j];

                if (item->type == MENUITEM_SEP) {
                    fprintf(fp, "    -\n");
                } else {
                    fprintf(fp, "    begin menuitem\n");
                    fprintf(fp, "      text \"%s\"\n", item->text);
                    if (item->rtext) {
                        fprintf(fp, "      rtext \"%s\"\n", item->rtext);
                    }
                    fprintf(fp, "      action ");
                    if (item->type == MENUITEM_STRING) {
                        fprintf(fp, "string '%s'\n", safe_print_string(item->action.string, -1));
                    } else if (item->type == MENUITEM_ECHO) {
                        fprintf(fp, "echo '%s'\n", safe_print_string(item->action.string, -1));
                    } else if (item->type == MENUITEM_SUBMENU) {
                        fprintf(fp, "submenu \"%s\"\n", (item->action.submenu)->title);
                    } else if (item->type == MENUITEM_SCRIPT) {
                        fprintf(fp, "script '%s'\n", item->action.script);
                    }
                    fprintf(fp, "    end\n");
                }
            }
            fprintf(fp, "end menu\n");
        }
        fprintf(fp, "\n");
    }

    fprintf(fp, "begin actions\n");
    for (action = action_list; action; action = action->next) {
        fprintf(fp, "    bind ");
        if (action->mod != MOD_NONE) {
            if (action->mod & MOD_ANY) {
                fprintf(fp, "anymod ");
            }
            if (action->mod & MOD_CTRL) {
                fprintf(fp, "ctrl ");
            }
            if (action->mod & MOD_SHIFT) {
                fprintf(fp, "shift ");
            }
            if (action->mod & MOD_LOCK) {
                fprintf(fp, "lock ");
            }
            if (action->mod & MOD_META) {
                fprintf(fp, "meta ");
            }
            if (action->mod & MOD_ALT) {
                fprintf(fp, "alt ");
            }
            if (action->mod & MOD_MOD1) {
                fprintf(fp, "mod1 ");
            }
            if (action->mod & MOD_MOD2) {
                fprintf(fp, "mod2 ");
            }
            if (action->mod & MOD_MOD3) {
                fprintf(fp, "mod3 ");
            }
            if (action->mod & MOD_MOD4) {
                fprintf(fp, "mod4 ");
            }
            if (action->mod & MOD_MOD5) {
                fprintf(fp, "mod5 ");
            }
        }
        if (action->keysym) {
            fprintf(fp, "0x%04x", (unsigned int) action->keysym);
        } else {
            fprintf(fp, "button%d", (int) action->button);
        }
        fprintf(fp, " to ");
        if (action->type == ACTION_STRING) {
            fprintf(fp, "string '%s'\n", safe_print_string(action->param.string, -1));
        } else if (action->type == ACTION_ECHO) {
            fprintf(fp, "echo '%s'\n", safe_print_string(action->param.string, -1));
        } else if (action->type == ACTION_MENU) {
            fprintf(fp, "menu \"%s\"\n", (action->param.menu)->title);
        } else if (action->type == ACTION_SCRIPT) {
            fprintf(fp, "script '%s'\n", action->param.script);
        }
    }
    fprintf(fp, "end actions\n\n");

#ifdef MULTI_CHARSET
    fprintf(fp, "begin multichar\n");
    if (rs_multichar_encoding) {
        fprintf(fp, "    encoding %s\n", rs_multichar_encoding);
    }
    for (i = 0; i < font_cnt; i++) {
        if (etmfonts[i]) {
            fprintf(fp, "    font %d %s\n", i, etmfonts[i]);
        }
    }
    fprintf(fp, "end multichar\n\n");
#endif

#ifdef USE_XIM
    fprintf(fp, "begin xim\n");
    if (rs_input_method) {
        fprintf(fp, "    input_method %s\n", rs_input_method);
    }
    if (rs_preedit_type) {
        fprintf(fp, "    preedit_type %s\n", rs_preedit_type);
    }
    fprintf(fp, "end xim\n\n");
#endif

    if (save_theme) {
        for (bbar = buttonbar; bbar; bbar = bbar->next) {
            button_t *b;

            fprintf(fp, "begin button_bar\n");
            fprintf(fp, "    font '%s'\n", NONULL(get_font_name(bbar->font)));
            if (bbar_is_top_docked(bbar)) {
                fprintf(fp, "    dock top\n");
            } else if (bbar_is_bottom_docked(bbar)) {
                fprintf(fp, "    dock bottom\n");
            } else {
                fprintf(fp, "    dock none\n");
            }
            fprintf(fp, "    visible %s\n", (bbar_is_visible(bbar) ? "yes" : "no"));
            for (b = bbar->buttons; b; b = b->next) {
                if (b->len) {
                    fprintf(fp, "    button \"%s\" ", safe_print_string(b->text, b->len));
                } else {
                    fprintf(fp, "    button ");
                }
#ifdef PIXMAP_SUPPORT
                if (b->icon && b->icon->iml) {
                    imlib_context_set_image(b->icon->iml->im);
                    fprintf(fp, "icon \"%s\" ", NONULL(imlib_image_get_filename()));
                }
#endif
                fprintf(fp, "action ");
                if (b->type == ACTION_STRING) {
                    fprintf(fp, "string '%s'\n", safe_print_string(b->action.string, -1));
                } else if (b->type == ACTION_ECHO) {
                    fprintf(fp, "echo '%s'\n", safe_print_string(b->action.string, -1));
                } else if (b->type == ACTION_MENU) {
                    fprintf(fp, "menu \"%s\"\n", (b->action.menu)->title);
                } else if (b->type == ACTION_SCRIPT) {
                    fprintf(fp, "script '%s'\n", b->action.script);
                }
            }
            for (b = bbar->rbuttons; b; b = b->next) {
                if (b->len) {
                    fprintf(fp, "    rbutton \"%s\" ", safe_print_string(b->text, b->len));
                } else {
                    fprintf(fp, "    rbutton ");
                }
#ifdef PIXMAP_SUPPORT
                if (b->icon && b->icon->iml) {
                    imlib_context_set_image(b->icon->iml->im);
                    fprintf(fp, "icon \"%s\" ", NONULL(imlib_image_get_filename()));
                }
#endif
                fprintf(fp, "action ");
                if (b->type == ACTION_STRING) {
                    fprintf(fp, "string '%s'\n", safe_print_string(b->action.string, -1));
                } else if (b->type == ACTION_ECHO) {
                    fprintf(fp, "echo '%s'\n", safe_print_string(b->action.string, -1));
                } else if (b->type == ACTION_MENU) {
                    fprintf(fp, "menu \"%s\"\n", (b->action.menu)->title);
                } else if (b->type == ACTION_SCRIPT) {
                    fprintf(fp, "script '%s'\n", b->action.script);
                }
            }
        }
        fprintf(fp, "end button_bar\n\n");
    }

    fprintf(fp, "begin toggles\n");
    fprintf(fp, "    map_alert %d\n", (BITFIELD_IS_SET(vt_options, VT_OPTIONS_MAP_ALERT) ? 1 : 0));
    fprintf(fp, "    urg_alert %d\n", (BITFIELD_IS_SET(vt_options, VT_OPTIONS_URG_ALERT) ? 1 : 0));
    fprintf(fp, "    visual_bell %d\n", (BITFIELD_IS_SET(vt_options, VT_OPTIONS_VISUAL_BELL) ? 1 : 0));
    fprintf(fp, "    login_shell %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_LOGIN_SHELL) ? 1 : 0));
    fprintf(fp, "    scrollbar %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR) ? 1 : 0));
    fprintf(fp, "    utmp_logging %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_WRITE_UTMP) ? 1 : 0));
    fprintf(fp, "    meta8 %d\n", (BITFIELD_IS_SET(vt_options, VT_OPTIONS_META8) ? 1 : 0));
    fprintf(fp, "    iconic %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_ICONIC) ? 1 : 0));
    fprintf(fp, "    home_on_output %d\n", (BITFIELD_IS_SET(vt_options, VT_OPTIONS_HOME_ON_OUTPUT) ? 1 : 0));
    fprintf(fp, "    home_on_input %d\n", (BITFIELD_IS_SET(vt_options, VT_OPTIONS_HOME_ON_INPUT) ? 1 : 0));
    fprintf(fp, "    no_input %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_NO_INPUT) ? 1 : 0));
    fprintf(fp, "    scrollbar_floating %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_FLOATING) ? 1 : 0));
    fprintf(fp, "    scrollbar_right %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT) ? 1 : 0));
    fprintf(fp, "    scrollbar_popup %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_POPUP) ? 1 : 0));
    fprintf(fp, "    borderless %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_BORDERLESS) ? 1 : 0));
    fprintf(fp, "    double_buffer %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_DOUBLE_BUFFER) ? 1 : 0));
    fprintf(fp, "    no_cursor %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_NO_CURSOR) ? 1 : 0));
    fprintf(fp, "    pause %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_PAUSE) ? 1 : 0));
    fprintf(fp, "    xterm_select %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_XTERM_SELECT) ? 1 : 0));
    fprintf(fp, "    select_line %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SELECT_WHOLE_LINE) ? 1 : 0));
    fprintf(fp, "    select_trailing_spaces %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SELECT_TRAILING_SPACES) ? 1 : 0));
    fprintf(fp, "    report_as_keysyms %d\n", (BITFIELD_IS_SET(vt_options, VT_OPTIONS_REPORT_AS_KEYSYMS) ? 1 : 0));
    fprintf(fp, "    itrans %d\n", (BITFIELD_IS_SET(image_options, IMAGE_OPTIONS_ITRANS) ? 1 : 0));
    fprintf(fp, "    buttonbar %d\n", ((buttonbar && bbar_is_visible(buttonbar)) ? 1 : 0));
    fprintf(fp, "    resize_gravity %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_RESIZE_GRAVITY) ? 1 : 0));
    fprintf(fp, "    sticky %d\n", (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_STICKY) ? 1 : 0));
    fprintf(fp, "end toggles\n\n");

    fprintf(fp, "begin keyboard\n");
    tmp_str = XKeysymToString(ks_smallfont);
    if (tmp_str) {
        fprintf(fp, "    smallfont_key %s\n", tmp_str);
    }
    tmp_str = XKeysymToString(ks_bigfont);
    if (tmp_str) {
        fprintf(fp, "    bigfont_key %s\n", tmp_str);
    }
    if (rs_meta_mod) {
        fprintf(fp, "    meta_mod %d\n", rs_meta_mod);
    }
    if (rs_alt_mod) {
        fprintf(fp, "    alt_mod %d\n", rs_alt_mod);
    }
    if (rs_numlock_mod) {
        fprintf(fp, "    numlock_mod %d\n", rs_numlock_mod);
    }
    for (i = 0; i < 256; i++) {
        if (KeySym_map[i]) {
            fprintf(fp, "    keysym 0xff%02x \'%s\'\n", i,
                    safe_print_string((char *) (KeySym_map[i] + 1), (unsigned long) KeySym_map[i][0]));
        }
    }
#ifdef GREEK_SUPPORT
    if (rs_greek_keyboard) {
        fprintf(fp, "    greek on %s\n", rs_greek_keyboard);
    }
#endif
    fprintf(fp, "    app_keypad %d\n", (PrivateModes & PrivMode_aplKP ? 1 : 0));
    fprintf(fp, "    app_cursor %d\n", (PrivateModes & PrivMode_aplCUR ? 1 : 0));
    fprintf(fp, "end keyboard\n\n");

    fprintf(fp, "begin misc\n");
#ifdef PRINTPIPE
    if (rs_print_pipe) {
        fprintf(fp, "    print_pipe '%s'\n", rs_print_pipe);
    }
#endif
    fprintf(fp, "    save_lines %d\n", rs_saveLines);
    fprintf(fp, "    min_anchor_size %d\n", rs_min_anchor_size);
    fprintf(fp, "    border_width %d\n", TermWin.internalBorder);
    fprintf(fp, "    term_name %s\n", getenv("TERM"));
    fprintf(fp, "    beep_command \"%s\"\n", (char *) ((rs_beep_command) ? (rs_beep_command) : ("")));

    fprintf(fp, "    debug %d\n", DEBUG_LEVEL);
    if (save_theme && rs_exec_args && rs_theme && strcmp(rs_theme, PACKAGE)) {
        fprintf(fp, "    exec ");
        for (i = 0; rs_exec_args[i]; i++) {
            fprintf(fp, "'%s' ", rs_exec_args[i]);
        }
        fprintf(fp, "\n");
    }
#ifdef CUTCHAR_OPTION
    if (rs_cutchars) {
        spif_charptr_t cut_chars_escaped;

        cut_chars_escaped = escape_string((spif_charptr_t) rs_cutchars, '\"', 0);
        fprintf(fp, "    cut_chars \"%s\"\n", (char *) cut_chars_escaped);
    }
#endif
    fprintf(fp, "end misc\n\n");

    fclose(fp);
    return 0;
}
