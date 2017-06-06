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

static const char cvs_ident[] = "$Id: startup.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <ctype.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>

#include "startup.h"
#include "actions.h"
#include "buttons.h"
#include "command.h"
#include "eterm_utmp.h"
#include "events.h"
#include "options.h"
#include "pixmap.h"
#include "screen.h"
#include "scrollbar.h"
#include "term.h"
#include "windows.h"

#include "screamcfg.h"

char *orig_argv0;

#ifdef PIXMAP_SUPPORT
/* Set to one in case there is no WM, or a lousy one
   that doesn't send the right events (*cough*
   Window Maker *cough*) -- mej */
short bg_needs_update = 1;
#endif
TermWin_t TermWin;
Display *Xdisplay;              /* display */
Colormap cmap;
char *display_name = NULL;
unsigned int colorfgbg;
Atom props[NUM_PROPS];

int
eterm_bootstrap(int argc, char *argv[])
{
    int i;
    char *val;

    /* "WINDOWID=\0" = 10 chars, UINT_MAX = 10 chars */
    static char windowid_string[20], *display_string, *term_string;

    orig_argv0 = argv[0];
    
    /* Security enhancements -- mej */
    putenv("IFS= \t\n");
    my_ruid = getuid();
    my_euid = geteuid();
    my_rgid = getgid();
    my_egid = getegid();
    privileges(REVERT);
    install_handlers();

    PABLO_START_TRACING();
    getcwd(initial_dir, PATH_MAX);
    init_libast();

    /* Open display, get options/resources and create the window */
    if (getenv("DISPLAY")) {
        display_name = STRDUP(getenv("DISPLAY"));
    }

    /* This MUST be called before any other Xlib functions */
#ifdef SPIFOPT_SETTING_PREPARSE
    SPIFOPT_FLAGS_SET(SPIFOPT_SETTING_PREPARSE);
#endif
    spifopt_parse(argc, argv);
    init_defaults();

#ifdef NEED_LINUX_HACK
    privileges(INVOKE);         /* xdm in new Linux versions requires ruid != root to open the display -- mej */
#endif
    Xdisplay = XOpenDisplay(display_name);
#ifdef NEED_LINUX_HACK
    privileges(REVERT);
#endif

    if (!Xdisplay && !(Xdisplay = XOpenDisplay(display_name))) {
        libast_print_error("Can't open display %s.  Set $DISPLAY or use --display\n",
                           NONULL(display_name));
        exit(EXIT_FAILURE);
    }
    XSetErrorHandler((XErrorHandler) xerror_handler);

    if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_INSTALL)) {
        cmap = XCreateColormap(Xdisplay, Xroot, Xvisual, AllocNone);
        XInstallColormap(Xdisplay, cmap);
    } else {
        cmap = Xcmap;
    }
#ifdef PIXMAP_SUPPORT
    imlib_context_set_display(Xdisplay);
    imlib_context_set_visual(Xvisual);
    imlib_context_set_colormap(cmap);
    imlib_context_set_dither_mask(0);
#endif

    get_modifiers();            /* Set up modifier masks before parsing config files. */

    /* Get all our properties set up. */
    MEMSET(props, 0, sizeof(props));
    props[PROP_TEXT] = XInternAtom(Xdisplay, "TEXT", False);
    props[PROP_COMPOUND_TEXT] = XInternAtom(Xdisplay, "COMPOUND_TEXT", False);
    props[PROP_UTF8_STRING] = XInternAtom(Xdisplay, "UTF8_STRING", False);
    props[PROP_CLIPBOARD] = XInternAtom(Xdisplay, "CLIPBOARD", False);
    props[PROP_DESKTOP] = XInternAtom(Xdisplay, "_NET_WM_DESKTOP", False);
    props[PROP_TRANS_PIXMAP] = XInternAtom(Xdisplay, "_XROOTPMAP_ID", False);
    props[PROP_TRANS_COLOR] = XInternAtom(Xdisplay, "_XROOTCOLOR_PIXEL", False);
    props[PROP_SELECTION_DEST] = XInternAtom(Xdisplay, "VT_SELECTION", False);
    props[PROP_SELECTION_INCR] = XInternAtom(Xdisplay, "INCR", False);
    props[PROP_SELECTION_TARGETS] = XInternAtom(Xdisplay, "TARGETS", False);
    props[PROP_ENL_COMMS] = XInternAtom(Xdisplay, "ENLIGHTENMENT_COMMS", True);
    props[PROP_ENL_VERSION] = XInternAtom(Xdisplay, "ENLIGHTENMENT_VERSION", True);
    props[PROP_ENL_MSG] = XInternAtom(Xdisplay, "ENL_MSG", False);
    props[PROP_DELETE_WINDOW] = XInternAtom(Xdisplay, "WM_DELETE_WINDOW", False);
    props[PROP_DND_PROTOCOL] = XInternAtom(Xdisplay, "DndProtocol", False);
    props[PROP_DND_SELECTION] = XInternAtom(Xdisplay, "DndSelection", False);
    props[PROP_EWMH_ICON] = XInternAtom(Xdisplay, "_NET_WM_ICON", False);
    props[PROP_EWMH_OPACITY] = XInternAtom(Xdisplay, "_NET_WM_WINDOW_OPACITY", True);
    props[PROP_EWMH_STARTUP_ID] = XInternAtom(Xdisplay, "_NET_STARTUP_ID", False);
    props[PROP_EWMH_STATE] = XInternAtom(Xdisplay, "_NET_WM_STATE", False);
    props[PROP_EWMH_STATE_STICKY] = XInternAtom(Xdisplay, "_NET_WM_STATE_STICKY", False);

    if ((theme_dir = spifconf_parse_theme(&rs_theme, THEME_CFG, PARSE_TRY_ALL))) {
        char *tmp;

        D_OPTIONS(("spifconf_parse_theme() returned \"%s\"\n", theme_dir));
        tmp = (char *) MALLOC(strlen(theme_dir) + sizeof("ETERM_THEME_ROOT=\0"));
        sprintf(tmp, "ETERM_THEME_ROOT=%s", theme_dir);
        putenv(tmp);
    }
    if ((user_dir = spifconf_parse_theme(&rs_theme, (rs_config_file ? rs_config_file : USER_CFG), (PARSE_TRY_USER_THEME | PARSE_TRY_NO_THEME)))) {
        char *tmp;

        D_OPTIONS(("spifconf_parse_theme() returned \"%s\"\n", user_dir));
        tmp = (char *) MALLOC(strlen(user_dir) + sizeof("ETERM_USER_ROOT=\0"));
        sprintf(tmp, "ETERM_USER_ROOT=%s", user_dir);
        putenv(tmp);
    }
#if defined(PIXMAP_SUPPORT)
    if (rs_path || theme_dir || user_dir) {
        register unsigned long len;
        register char *tmp;

        len = strlen(initial_dir);
        if (rs_path) {
            len += strlen(rs_path) + 1; /* +1 for the colon */
        }
        if (theme_dir) {
            len += strlen(theme_dir) + 1;
        }
        if (user_dir) {
            len += strlen(user_dir) + 1;
        }
        tmp = MALLOC(len + 1);  /* +1 here for the NUL */
        snprintf(tmp, len + 1, "%s%s%s%s%s%s%s", (rs_path ? rs_path : ""), (rs_path ? ":" : ""), initial_dir,
                 (theme_dir ? ":" : ""), (theme_dir ? theme_dir : ""), (user_dir ? ":" : ""), (user_dir ? user_dir : ""));
        tmp[len] = '\0';
        FREE(rs_path);
        rs_path = tmp;
        D_OPTIONS(("New rs_path set to \"%s\"\n", rs_path));
    }
#endif
    spifopt_parse(argc, argv);
    D_UTMP(("Saved real uid/gid = [ %d, %d ]  effective uid/gid = [ %d, %d ]\n", my_ruid, my_rgid, my_euid, my_egid));
    D_UTMP(("Now running with real uid/gid = [ %d, %d ]  effective uid/gid = [ %d, %d ]\n", getuid(), getgid(), geteuid(),
            getegid()));

#ifdef ESCREEN
#  define ESCREEN_PREFIX "Escreen"

    TermWin.screen = NULL;
    TermWin.screen_mode = NS_MODE_NONE;
    if (rs_url) {
        if (!BEG_STRCASECMP(rs_url, NS_TWIN_PROTO)) {
            TermWin.screen_mode = NS_MODE_TWIN;
        } else if (!BEG_STRCASECMP(rs_url, NS_SCREEN_PROTO)) {
            TermWin.screen_mode = NS_MODE_SCREEN;
        } else if (!BEG_STRCASECMP(rs_url, NS_SCREAM_PROTO)) {
            TermWin.screen_mode = NS_MODE_SCREAM;
        } else {
            TermWin.screen_mode = NS_MODE_NEGOTIATE;
        }
    } else if (!strcmp(ESCREEN_PREFIX, my_basename(orig_argv0))) {
        TermWin.screen_mode = NS_MODE_SCREEN;
    }
#endif

    post_parse();

#ifdef PREFER_24BIT
    /*
     * If depth is not 24, look for a 24bit visual.
     */
    if (Xdepth != 24) {
        XVisualInfo vinfo;

        if (XMatchVisualInfo(Xdisplay, Xscreen, 24, TrueColor, &vinfo)) {
            Xdepth = 24;
            Xvisual = vinfo.visual;
            cmap = XCreateColormap(Xdisplay, RootWindow(Xdisplay, Xscreen), Xvisual, AllocNone);
        }
    }
#endif

    process_colors();

    Create_Windows(argc, argv);
    scr_reset();                /* initialize screen */

    /* Initialize the scrollbar */
    scrollbar_init(szHint.width, szHint.height - bbar_calc_docked_height(BBAR_DOCKED));
    scrollbar_mapping((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR))
                      && !((BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_POPUP)) && !TermWin.focus));

    /* Initialize the menu subsystem. */
    menu_init();

    if (buttonbar) {
        bbar_init(buttonbar, szHint.width);
    }
#if DEBUG >= DEBUG_X
    if (DEBUG_LEVEL >= DEBUG_X) {
        XSync(Xdisplay, False);
        XSynchronize(Xdisplay, True);
    }
#endif

    val = XDisplayString(Xdisplay);
    if (!display_name) {
        display_name = val;
    }

    i = strlen(val);
    display_string = MALLOC(i + 9);

    sprintf(display_string, "DISPLAY=%s", val);
    sprintf(windowid_string, "WINDOWID=%u", (unsigned int) TermWin.parent);

    /* add entries to the environment:
     * DISPLAY:       X display name
     * WINDOWID:      X windowid of the window
     * COLORTERM:     Terminal supports color
     * COLORTERM_BCE: Terminal supports BCE
     * TERM:          Terminal type for termcap/terminfo
     */
    putenv(display_string);
    putenv(windowid_string);
    if (Xdepth <= 2) {
        putenv("COLORTERM=" COLORTERMENV "-mono");
        putenv("COLORTERM_BCE=" COLORTERMENV "-mono");
        putenv("TERM=" TERMENV);
    } else {
        if (rs_term_name) {
            i = strlen(rs_term_name);
            term_string = MALLOC(i + 6);

            sprintf(term_string, "TERM=%s", rs_term_name);
            putenv(term_string);
        } else {
#ifdef DEFINE_XTERM_COLOR
            if (Xdepth <= 2)
                putenv("TERM=" TERMENV);
            else
                putenv("TERM=" TERMENV "-color");
#else
            putenv("TERM=" TERMENV);
#endif
        }
        putenv("COLORTERM=" COLORTERMENV);
        putenv("COLORTERM_BCE=" COLORTERMENV);
    }
    putenv("ETERM_VERSION=" VERSION);

#ifdef NO_UTF8_LOCALE
    /* Check locale for UTF-8 and deactivate if needed. */
    val = getenv("LANG");
    D_CMD(("Checking locale \"%s\" for UTF-8.\n", NONULL(val)));
    if (val && *val) {
        char *tmp;

        tmp = strcasestr(val, ".utf");
        if (tmp) {
            *tmp = 0;
            D_CMD((" -> Deactivating unsupported UTF-8 locale; now using \"%s\"\n", val));
        }
    }
#endif

    D_CMD(("init_command()\n"));
    init_command(rs_exec_args);

    main_loop();

    return (EXIT_SUCCESS);
}
