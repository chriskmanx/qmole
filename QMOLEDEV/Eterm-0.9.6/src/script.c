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

static const char cvs_ident[] = "$Id: script.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <X11/cursorfont.h>
#include <signal.h>

#include "command.h"
#include "menus.h"
#include "options.h"
#include "pixmap.h"
#include "screen.h"
#include "script.h"
#include "startup.h"
#include "system.h"

static eterm_script_handler_t script_handlers[] = {
    {"copy", script_handler_copy},
    {"die", script_handler_exit},
    {"echo", script_handler_echo},
#ifdef ESCREEN
    {"es_display", script_handler_es_display},
    {"es_disp", script_handler_es_display},
    {"es_region", script_handler_es_region},
    {"es_reg", script_handler_es_region},
    {"es_win", script_handler_es_region},
    {"es_window", script_handler_es_region},
    {"es_statement", script_handler_es_statement},
    {"es_reset", script_handler_es_reset},
    {"es_rst", script_handler_es_reset},
#endif
    {"exec", script_handler_spawn},
    {"exec_dialog", script_handler_exec_dialog},
    {"exit", script_handler_exit},
    {"kill", script_handler_kill},
    {"msgbox", script_handler_msgbox},
    {"paste", script_handler_paste},
    {"quit", script_handler_exit},
    {"save", script_handler_save},
    {"save_buff", script_handler_save_buff},
    {"scroll", script_handler_scroll},
    {"search", script_handler_search},
    {"spawn", script_handler_spawn},
    {"string", script_handler_string},

    {"nop", script_handler_nop}
};
static size_t handler_count = sizeof(script_handlers) / sizeof(eterm_script_handler_t);

#if UNUSED_BLOCK
void
eterm_handle_winop(char *action)
{
    char *winid;
    Window win = 0;

    ASSERT(action != NULL);

    winid = strchr(action, ' ');
    if (winid) {
        win = (Window) strtoul(winid + 1, (char **) NULL, 0);
    }
    if (win == 0) {             /* If no window ID was given, or if the strtoul() call failed */
        win = TermWin.parent;
    }
    if (!BEG_STRCASECMP(action, "raise")) {
        XRaiseWindow(Xdisplay, win);
    } else if (!BEG_STRCASECMP(action, "lower")) {
        XLowerWindow(Xdisplay, win);
    } else if (!BEG_STRCASECMP(action, "map")) {
        XMapWindow(Xdisplay, win);
    } else if (!BEG_STRCASECMP(action, "unmap")) {
        XUnmapWindow(Xdisplay, win);
    } else if (!BEG_STRCASECMP(action, "move")) {
        int x, y, n;
        char *xx, *yy;

        n = spiftool_num_words(action);
        if (n == 3 || n == 4) {
            if (n == 3) {
                win = TermWin.parent;
            }
            xx = spiftool_get_pword(n - 1, action);
            yy = spiftool_get_pword(n, action);
            x = (int) strtol(xx, (char **) NULL, 0);
            y = (int) strtol(yy, (char **) NULL, 0);
            XMoveWindow(Xdisplay, win, x, y);
        }
    } else if (!BEG_STRCASECMP(action, "resize")) {
        int w, h, n;
        char *ww, *hh;

        n = spiftool_num_words(action);
        if (n == 3 || n == 4) {
            if (n == 3) {
                win = TermWin.parent;
            }
            ww = spiftool_get_pword(n - 1, action);
            hh = spiftool_get_pword(n, action);
            w = (int) strtol(ww, (char **) NULL, 0);
            h = (int) strtol(hh, (char **) NULL, 0);
            XResizeWindow(Xdisplay, win, w, h);
        }
    } else if (!BEG_STRCASECMP(action, "kill")) {
        XKillClient(Xdisplay, win);
    } else if (!BEG_STRCASECMP(action, "iconify")) {
        XIconifyWindow(Xdisplay, win, Xscreen);
    } else {
        libast_print_error("IPC Error:  Unrecognized window operation \"%s\"\n", action);
    }
}
#endif

/********* HANDLERS **********/

/* copy():  Copy the current selection to the specified clipboard or cut
 *          buffer
 *
 * Syntax:  copy([ <buffer> ])
 *
 * <buffer> is either a number 0-7, in which case the selection is copied to
 * the cut buffer specified, or one of the words "clipboard," "primary," or
 * "secondary" (or any initial substring thereof), in which case the
 * selection is copied to the specified clipboard.  The default buffer is
 * the "primary" buffer (XA_PRIMARY in Xlib-speak).
 */
void
script_handler_copy(spif_charptr_t *params)
{
    unsigned char i;
    char *buffer_id;
    Atom sel = XA_PRIMARY;

    if (params) {
        for (i = 0; (buffer_id = params[i]); i++) {
            if (*buffer_id) {
                if (*buffer_id >= '0' && *buffer_id <= '7') {
                    sel = (Atom) ((int) XA_CUT_BUFFER0 + (int) (*buffer_id - '0'));
                } else if (!BEG_STRCASECMP(buffer_id, "clipboard")) {
                    sel = props[PROP_CLIPBOARD];
                } else if (!BEG_STRCASECMP(buffer_id, "primary")) {
                    sel = XA_PRIMARY;
                } else if (!BEG_STRCASECMP(buffer_id, "secondary")) {
                    sel = XA_SECONDARY;
                } else {
                    libast_print_error("Invalid parameter to copy():  \"%s\"\n", buffer_id);
                }
            }
        }
    }
    selection_copy(sel);
}

/* echo():  Send a string to the pty
 *
 * Syntax:  echo(<string>)
 *
 * <string> is the string of characters to send to the pty.
 */
void
script_handler_echo(spif_charptr_t *params)
{
    spif_charptr_t *tmp;

    for (tmp = params; tmp && *tmp; tmp++) {
        tt_write(*tmp, strlen(*tmp));
    }
}

/* exit():  Exit Eterm with an optional message or return code
 *
 * Syntax:  exit([ { <msg> | <code> } ])
 *
 * <msg> is an optional exit message.  <code> is a positive or
 * negative integer return code.  Either one may be specified, but not
 * both.  If neither is specified, Eterm exits with a return code of 0
 * and no message.
 */
void
script_handler_exit(spif_charptr_t *params)
{
    unsigned char code = 0;
    char *tmp;

    if (params && *params) {
        if (isdigit(params[0][0]) || (params[0][0] == '-' && isdigit(params[0][1]))) {
            code = (unsigned char) atoi(params[0]);
        } else {
            tmp = spiftool_join(" ", params);
            printf("Exiting:  %s\n", tmp);
            FREE(tmp);
        }
    }
    exit(code);
}

/* kill():  Send a given signal to Eterm's child process
 *
 * Syntax:  kill([ <signal> ])
 *
 * <signal> is the numeric signal to send to the process.  If not
 * specified, SIGTERM is the default.
 */
void
script_handler_kill(spif_charptr_t *params)
{
    int sig;

    if (params && *params) {
        sig = atoi(params[0]);
    } else {
        sig = SIGTERM;
    }
    kill(cmd_pid, sig);
}

/* paste():  Paste the contents of the specified clipboard or cut buffer
 *           into the terminal window
 *
 * Syntax:  paste([ <buffer> ])
 *
 * <buffer> is either a number 0-7, in which case the contents of the cut
 * buffer specified are pasted, or one of the words "clipboard," "primary,"
 * or "secondary" (or any initial substring thereof), in which case the
 * contents of the specified clipboard are pasted.  The default buffer is
 * the "primary" buffer (XA_PRIMARY in Xlib-speak).
 */
void
script_handler_paste(spif_charptr_t *params)
{
    unsigned char i;
    char *buffer_id;
    Atom sel = XA_PRIMARY;

    if (params) {
        for (i = 0; (buffer_id = params[i]); i++) {
            if (*buffer_id) {
                if (*buffer_id >= '0' && *buffer_id <= '7') {
                    sel = (Atom) ((int) XA_CUT_BUFFER0 + (int) (*buffer_id - '0'));
                } else if (!BEG_STRCASECMP(buffer_id, "clipboard")) {
                    sel = props[PROP_CLIPBOARD];
                } else if (!BEG_STRCASECMP(buffer_id, "primary")) {
                    sel = XA_PRIMARY;
                } else if (!BEG_STRCASECMP(buffer_id, "secondary")) {
                    sel = XA_SECONDARY;
                } else {
                    libast_print_error("Invalid parameter to paste():  \"%s\"\n", buffer_id);
                }
            }
        }
    }
    selection_paste(sel);
}

/* save():  Save the current theme/user configuration
 *
 * Syntax:  save([ { theme | user } ,] [ <filename> ])
 *
 * The "user" settings are saved by default, and the default
 * filename is user.cfg.  So save() by itself will save the
 * current user settings to user.cfg.  save(theme) will save
 * the theme settings instead; the default filename in that case
 * will be theme.cfg.
 */
void
script_handler_save(spif_charptr_t *params)
{
    if (params && *params) {
        if (!strcasecmp(params[0], "theme")) {
            save_config(params[1], SAVE_THEME_CONFIG);
        } else {
            save_config(params[0], SAVE_USER_CONFIG);
        }
    } else {
        save_config(NULL, SAVE_USER_CONFIG);
    }
}

/* save_buff():  Dump the scrollback buffer to a file
 *
 * Syntax:  save_buff(<filename>)
 *
 * Dumps the entire contents of the screen buffer into
 * the specified file.
 */
void
script_handler_save_buff(spif_charptr_t *params)
{
    if (params && *params) {
        scr_dump_to_file(params[0]);
    } else {
        scr_dump();
    }
}

/* scroll():  Scroll backward or forward in the scrollback buffer
 *
 * Syntax:  scroll(N) or scroll(Nl) -- Scroll N lines
 *          scroll(Np)              -- Scroll N pages/screensful
 *          scroll(Nb)              -- Scroll N buffers
 * 
 * N is a floating point number.  Use a negative number to scroll
 * up and a positive number to scroll down.  Fractions can be used
 * also (e.g., to scroll one half page, use scroll(0.5p)).  It is
 * possible to spell out "lines," "pages," and "buffers" as well,
 * and the type may be passed as a second parameter if you wish.
 */
void
script_handler_scroll(spif_charptr_t *params)
{
    char *type;
    double cnt_float;
    long count;
    int direction = DN;

    if (params && *params) {
        cnt_float = strtod(params[0], &type);
        if (cnt_float == 0.0) {
            return;
        } else if (cnt_float < 0.0) {
            cnt_float = -cnt_float;
            direction = UP;
        }
        if (!type) {
            type = params[1];
        }
        if (type && *type) {
            for (; *type && !isalpha(*type); type++);
            if (str_leading_match("lines", type)) {
                count = (long) cnt_float;
            } else if (str_leading_match("pages", type) || str_leading_match("screens", type)) {
                count = (long) ((cnt_float * TermWin.nrow) - CONTEXT_LINES);
            } else if (str_leading_match("buffers", type)) {
                count = (long) (cnt_float * (TermWin.nrow + TermWin.saveLines));
            } else {
                libast_print_error("Invalid modifier \"%s\" in scroll()\n", type);
                return;
            }
        } else {
            count = (long) cnt_float;
        }

        if (count <= 0) {
            return;
        }
        scr_page(direction, count);
    }
}

/* search():  Search the scrollback buffer for a string and highlight
 *            any occurances of it.
 *
 * Syntax:  search([ <str> ])
 *
 * <str> is an optional search string to highlight.  If none is given,
 * search() will clear the previously-highlighted search term.
 */
void
script_handler_search(spif_charptr_t *params)
{
    static char *search = NULL;

    if (params && *params) {
        if (search) {
            FREE(search);
        }
        search = STRDUP(*params);
    }
    if ((menu_dialog(NULL, "Enter Search Term:", TERM_WINDOW_GET_REPORTED_COLS(), &search, NULL)) != -2) {
        scr_search_scrollback(search);
    }
}

/* spawn():  Spawns a child process to execute a sub-command
 *
 * Syntax:  spawn([ <command> ])
 *
 * If no command is specified, the default is to execute another Eterm.
 */
void
script_handler_spawn(spif_charptr_t *params)
{
    char *tmp;

    if (params && *params) {
        tmp = spiftool_join(" ", params);
        system_no_wait(tmp);
        FREE(tmp);
    } else {
        system_no_wait("Eterm");
    }
}

/* string():  Send a string to Eterm's escape code handler
 *
 * Syntax:  string(<string>)
 *
 * <string> is the string of characters to send to the handler.
 */
void
script_handler_string(spif_charptr_t *params)
{
    spif_charptr_t *tmp;

    for (tmp = params; tmp && *tmp; tmp++) {
        cmd_write(*tmp, strlen(*tmp));
    }
}

/* exec_dialog():  Execute a program after prompting
 *
 * Syntax:  exec_dialog(<command>)
 *
 * <command> is the command to be executed.
 */
void
script_handler_exec_dialog(spif_charptr_t *params)
{
    char *tmp;
    int ret;

    if (params && *params) {
        tmp = spiftool_join(" ", params);
    } else {
        tmp = NULL;
    }
    scr_refresh(DEFAULT_REFRESH);
    ret = menu_dialog(NULL, "Confirm Command (ESC to cancel)", PATH_MAX, &tmp, NULL);
    if (ret != -2) {
        system_no_wait(tmp);
    }
    if (tmp) {
        FREE(tmp);
    }
}

/* msgbox():  Present a brief message box and wait for a keypress
 *
 * Syntax:  msgbox(<message>)
 *
 * <message> is the message to present.
 */
void
script_handler_msgbox(spif_charptr_t *params)
{
    char *tmp;

    if (params && *params) {
        tmp = spiftool_join(" ", params);
        scr_refresh(DEFAULT_REFRESH);
        menu_dialog(NULL, tmp, 1, NULL, NULL);
        FREE(tmp);
    }
}

#ifdef ESCREEN
/* es_display():  Master command for manipulating Escreen displays
 *
 * Syntax:  es_display(<subcommand>[, <subcommand params>])
 *
 * <subcommand> is the secondary command, one of the following:
 *
 * goto       - Switch to the specified display (e.g., goto 2)
 * prev       - Switch to previous display
 * next       - Switch to next display
 * toggle     - Toggle display
 * new        - Create a new display with optional name, or "ask" to
 *              prompt the user for its name
 * rename     - Change the name of the current/specified display
 * kill       - Terminate a display
 * watch      - Toggle monitoring of a display for activity
 * scrollback - View the scrollback for a display
 */
void
script_handler_es_display(spif_charptr_t *params)
{
    _ns_sess *sess = TermWin.screen;
    char *p, *a;
    int index = 1;
    int no = -1;                /* which display? */

    if (!params || !*params || !sess) {
        return;
    }

    p = spiftool_downcase_str(*params);
    a = params[index++];
    if (a && isdigit(*a)) {
        no = atoi(a);
        a = params[index++];
        D_ESCREEN(("disp #%d\n", no));
    }

    if (!strcmp(p, "goto") || !strcmp(p, "go") || !strcmp(p, "focus") || !strcmp(p, "raise")) {
        D_ESCREEN(("Go to display %d\n", no));
        ns_go2_disp(sess, no);
    } else if (!strcmp(p, "prvs") || !strcmp(p, "prev") || !strcmp(p, "previous")) {
        D_ESCREEN(("Go to previous display\n"));
        ns_rel_disp(sess, -1);
    } else if (!strcmp(p, "next")) {
        D_ESCREEN(("Go to next display\n"));
        ns_rel_disp(sess, 1);
    } else if (!strcmp(p, "toggle")) {
        D_ESCREEN(("Toggle display\n"));
        ns_tog_disp(sess);
    } else if (!strcmp(p, "new")) {
        if (!a || !*a) {
            D_ESCREEN(("disp new\n"));
            ns_add_disp(sess, no, "");
        } else if (!a || !*a || !strcasecmp(a, "ask")) {
            D_ESCREEN(("disp new ask\n"));
            ns_add_disp(sess, no, NULL);
        } else {
            D_ESCREEN(("disp new \"%s\"\n", a));
            ns_add_disp(sess, no, a);
        }
    } else if (!strcmp(p, "title") || !strcmp(p, "name") || !strcmp(p, "rename")) {
        if (!a || !*a || !strcasecmp(a, "ask")) {
            D_ESCREEN(("disp name ask\n"));
            ns_ren_disp(sess, no, NULL);
        } else {
            D_ESCREEN(("disp name \"%s\"\n", a));
            ns_ren_disp(sess, no, a);
        }
    } else if (!strcmp(p, "kill") || !strcmp(p, "close")) {
        if (!a || !*a || !strcasecmp(a, "ask")) {
            D_ESCREEN(("disp kill ask\n"));
            ns_rem_disp(sess, no, TRUE);
        } else {
            D_ESCREEN(("disp kill \"%s\"\n", a));
            ns_rem_disp(sess, no, FALSE);
        }
    } else if (!strcmp(p, "watch") || !strcmp(p, "monitor")) {
        D_ESCREEN(("Monitor display %d\n", no));
        ns_mon_disp(sess, no, NS_MON_TOGGLE_NOISY);
    } else if (!strcmp(p, "back") || !strcmp(p, "backlog") || !strcmp(p, "scrollback")) {
        D_ESCREEN(("View scrollback on display %d\n", no));
        ns_sbb_disp(sess, no);
    } else {
        libast_print_error("Error in script:  \"display\" has no sub-function \"%s\".\n", p);
    }
}

/* es_region():  Master command for manipulating Escreen regions
 *
 * Syntax:  es_region(<subcommand>[, <subcommand params>])
 *
 * <subcommand> is the secondary command, one of the following:
 *
 * goto       - Switch to the specified region (e.g., goto 2)
 * prev       - Switch to previous region
 * next       - Switch to next region
 * toggle     - Toggle region
 * new        - Create a new region with optional name, or "ask" to
 *              prompt the user for its name
 * rename     - Change the name of the current/specified region
 * kill       - Terminate a region
 * only       - Maximize this region to the full display
 * watch      - Toggle monitoring of a region for activity
 * scrollback - View the scrollback for a region
 */
void
script_handler_es_region(spif_charptr_t *params)
{
    _ns_sess *sess = TermWin.screen;
    _ns_disp *disp;
    char *p, *a;
    int index = 1;
    int no = -1;

    if (!params || !*params || !sess) {
        return;
    }
    if (!TermWin.screen->curr) {
        TermWin.screen->curr = TermWin.screen->dsps;
    }
    if (!(disp = TermWin.screen->curr)) {
        return;
    }

    p = spiftool_downcase_str(*params);
    a = params[index++];
    if (a && isdigit(*a)) {
        no = atoi(a);
        a = params[index++];
        D_ESCREEN(("region #%d\n", no));
    }

    if (!strcmp(p, "goto") || !strcmp(p, "go") || !strcmp(p, "focus") || !strcmp(p, "raise")) {
        D_ESCREEN(("Go to region %d of display %8p\n", no, disp));
        ns_go2_region(sess, disp, no);
    } else if (!strcmp(p, "prvs") || !strcmp(p, "prev") || !strcmp(p, "previous")) {
        D_ESCREEN(("Go to previous region of display %8p\n", disp));
        ns_rel_region(sess, disp, -1);
    } else if (!strcmp(p, "next")) {
        D_ESCREEN(("Go to next region of display %8p\n", disp));
        ns_rel_region(sess, disp, 1);
    } else if (!strcmp(p, "toggle")) {
        D_ESCREEN(("Toggle region of display %8p\n", disp));
        ns_tog_region(sess, disp);
    } else if (!strcmp(p, "new") || !strcmp(p, "spiftool_split")) {
        if (!a || !*a || !strcasecmp(a, "ask")) {
            D_ESCREEN(("region new ask\n"));
            ns_add_region(sess, disp, no, NULL);
        } else {
            D_ESCREEN(("region new \"%s\"\n", a));
            ns_add_region(sess, disp, no, a);
        }
    } else if (!strcmp(p, "title") || !strcmp(p, "name") || !strcmp(p, "rename")) {
        if (!a || !*a || !strcasecmp(a, "ask")) {
            D_ESCREEN(("region name ask\n"));
            ns_ren_region(sess, disp, no, NULL);
        } else {
            D_ESCREEN(("region name \"%s\"\n", a));
            ns_ren_region(sess, disp, no, a);
        }
    } else if (!strcmp(p, "kill") || !strcmp(p, "close")) {
        if (!a || !*a || !strcasecmp(a, "ask")) {
            D_ESCREEN(("region kill ask\n"));
            ns_rem_region(sess, disp, no, TRUE);
        } else {
            D_ESCREEN(("disp kill \"%s\"\n", a));
            ns_rem_region(sess, disp, no, FALSE);
        }
    } else if (!strcmp(p, "only") || !strcmp(p, "unsplit") || !strcmp(p, "full") || !strcmp(p, "fullscreen")) {
        D_ESCREEN(("Maximizing region %d of display %8p\n", no, disp));
        ns_one_region(sess, disp, no);
    } else if (!strcmp(p, "watch") || !strcmp(p, "monitor")) {
        D_ESCREEN(("Monitor region %d of display %8p\n", no, disp));
        ns_mon_region(sess, disp, no);
    } else if (!strcmp(p, "back") || !strcmp(p, "backlog") || !strcmp(p, "scrollback")) {
        D_ESCREEN(("View scrollback for region %d of display %8p\n", no, disp));
        ns_sbb_region(sess, disp, no);
    } else {
        libast_print_error("Error in script:  \"region\" has no sub-function \"%s\".\n", p);
    }
}

/* es_statement():  Execute an Escreen statement
 *
 * Syntax:  es_statement(<statement>)
 *
 * <statement> is the Escreen (screen) statement to execute.
 */
void
script_handler_es_statement(spif_charptr_t *params)
{
    char *tmp;

    if (params && *params) {
        tmp = spiftool_join(" ", params);
        ns_statement(TermWin.screen, tmp);
        FREE(tmp);
    } else {
        ns_statement(TermWin.screen, NULL);
    }
}

/* es_reset():  Reset the Escreen session
 *
 * Syntax:  es_reset()
 */
void
script_handler_es_reset(spif_charptr_t *params)
{
    USE_VAR(params);
    ns_reset(TermWin.screen, 0);
}
#endif

/* nop():  Do nothing
 *
 * Syntax:  nop()
 *
 * This function can be used to cancel undesired default behavior.
 */
void
script_handler_nop(spif_charptr_t *params)
{
    USE_VAR(params);
}



/********* ENGINE *********/
eterm_script_handler_t *script_find_handler(const char *name)
{
    register unsigned long i;

    for (i = 0; i < handler_count; i++) {
        /* Small optimization.  Only call strcasecmp() if the first letter matches. */
        if ((tolower(name[0]) == tolower(script_handlers[i].name[0]))
            && !strcasecmp(name, script_handlers[i].name)) {
            return &script_handlers[i];
        }
    }
    return NULL;
}

void
script_parse(char *s)
{
    spif_charptr_t *token_list, *param_list;
    register char *pstr;
    register unsigned long i;
    char *func_name, *params, *tmp;
    size_t len;
    eterm_script_handler_t *func;

    REQUIRE(s != NULL);

    D_SCRIPT(("Parsing:  \"%s\"\n", s));

    token_list = spiftool_split(";", s);
    if (!token_list) {
        D_SCRIPT(("No tokens found; ignoring script.\n"));
        return;
    }

    for (i = 0; token_list[i]; i++) {
        pstr = token_list[i];
        spiftool_chomp(pstr);
        if (!(*pstr)) {
            continue;
        }
        if ((params = strchr(pstr, '('))) {
            if (params != pstr) {
                len = params - pstr;
                func_name = (char *) MALLOC(len + 1);
                strncpy(func_name, pstr, len);
                func_name[len] = 0;
            } else {
                libast_print_error("Error in script \"%s\":  Missing function name before \"%s\".\n", s, params);
                spiftool_free_array((void **) token_list, 0);
                return;
            }
        } else {
            func_name = STRDUP(pstr);
        }
        if (!func_name) {
            spiftool_free_array((void **) token_list, 0);
            return;
        }
        if (params) {
            params++;
            if ((tmp = strrchr(params, ')'))) {
                *tmp = 0;
            } else {
                libast_print_error("Error in script \"%s\":  Missing closing parentheses for \"%s\".\n", s, token_list[i]);
                spiftool_free_array((void **) token_list, 0);
                return;
            }
            param_list = spiftool_split(", \t", params);
        } else {
            param_list = NULL;
        }
        D_SCRIPT(("Calling function %s with parameters:  %s\n", func_name, NONULL(params)));
        if ((func = script_find_handler(func_name))) {
            (func->handler) (param_list);
        } else {
            libast_print_error("Error in script \"%s\":  No such function \"%s\".\n", s, func_name);
        }
    }

    if (params) {
        spiftool_free_array((void **) param_list, 0);
    }
    spiftool_free_array((void **) token_list, 0);
}
