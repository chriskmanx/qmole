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

static const char cvs_ident[] = "$Id: actions.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <unistd.h>
#include <limits.h>

#include "startup.h"
#include "actions.h"
#include "command.h"
#include "e.h"
#include "events.h"
#include "menus.h"
#include "options.h"
#include "pixmap.h"
#include "screen.h"
#include "script.h"
#include "scrollbar.h"
#include "term.h"
#include "windows.h"
#ifdef ESCREEN
#  include "screamcfg.h"
#endif

action_t *action_list = NULL;

unsigned char
action_handle_string(event_t *ev, action_t *action)
{
    USE_VAR(ev);
    REQUIRE_RVAL(action->param.string != NULL, 0);
    cmd_write((unsigned char *) action->param.string, strlen(action->param.string));
    return 1;
}

unsigned char
action_handle_echo(event_t *ev, action_t *action)
{
    USE_VAR(ev);
    REQUIRE_RVAL(action->param.string != NULL, 0);
#ifdef ESCREEN
    if (TermWin.screen && TermWin.screen->backend) {
#  ifdef NS_HAVE_SCREEN
        /* translate escapes */
        ns_parse_screen_interactive(TermWin.screen, action->param.string);
#  endif
    } else
#endif
        tt_write((unsigned char *) action->param.string, strlen(action->param.string));
    return 1;
}

unsigned char
action_handle_script(event_t *ev, action_t *action)
{
    USE_VAR(ev);
    REQUIRE_RVAL(action->param.script != NULL, 0);
    script_parse(action->param.script);
    return 1;
}

unsigned char
action_handle_menu(event_t *ev, action_t *action)
{
    REQUIRE_RVAL(action->param.menu != NULL, 0);
    menu_invoke(ev->xbutton.x, ev->xbutton.y, TermWin.parent, action->param.menu, ev->xbutton.time);
    return 1;
}

action_t *action_find_match(unsigned short mod, unsigned char button, KeySym keysym)
{
    action_t *action;

    D_ACTIONS(("mod == 0x%08x, button == %d, keysym == 0x%08x\n", mod, button, keysym));
    for (action = action_list; action; action = action->next) {
        D_ACTIONS(("Checking action.  mod == 0x%08x, button == %d, keysym == 0x%08x\n", action->mod, action->button,
                   action->keysym));
        if ((action->mod == mod) && (action->button == button) && (action->keysym == keysym)) {
            D_ACTIONS(("Match found at %8p\n", action));
            return action;
        }
    }
    return NULL;
}

unsigned char
action_check_button(unsigned char button, int x_button)
{
    /* The event we're looking at is a button press.  Make sure the
       current action is also, and that it matches.  Continue if not. */
    D_ACTIONS(("Checking button %d vs x_button %d\n", button, x_button));
    if (button == BUTTON_NONE) {
        /* It was a button press, and this action is not a button action. */
        return FALSE;
    }
    if ((button != BUTTON_ANY) && (button != x_button)) {
        /* It's a specific button, and the two don't match. */
        return FALSE;
    }
    D_ACTIONS(("Button match confirmed.\n"));
    return TRUE;
}

unsigned char
action_check_keysym(KeySym keysym, KeySym x_keysym)
{
    /* The event we're looking at is a key press.  Make sure the
       current action is also, and that it matches.  Continue if not. */
    D_ACTIONS(("Checking keysym 0x%08x vs x_keysym 0x%08x\n", keysym, x_keysym));
    if (keysym == None) {
        return FALSE;
    } else if (keysym != x_keysym) {
        return FALSE;
    }
    D_ACTIONS(("Keysym match confirmed.\n"));
    return TRUE;
}

unsigned char
action_check_modifiers(unsigned short mod, int x_mod)
{
    unsigned int m = (AltMask | MetaMask | NumLockMask);

    /* When we do have to check the modifiers, we do so in this order to eliminate the
       most popular choices first.  If any test fails, we return FALSE. */
    D_ACTIONS(("Checking modifier set 0x%08x (" MOD_FMT ") vs. X modifier set 0x%08x (" MOD_FMT ")\n", mod, SHOW_MODS(mod), x_mod,
               SHOW_X_MODS(x_mod)));
    if (mod != MOD_ANY) {
        /* LOGICAL_XOR() returns true if either the first parameter or the second parameter
           is true, but not both...just like XOR.  If the mask we're looking for is set in
           mod but not in x_mod, or set in x_mod but not in mod, we don't have a match. */
        if (LOGICAL_XOR((mod & MOD_CTRL), (x_mod & ControlMask))) {
            return FALSE;
        }
        if (LOGICAL_XOR((mod & MOD_SHIFT), (x_mod & ShiftMask))) {
            return FALSE;
        }
        if (MetaMask != AltMask) {
            if (LOGICAL_XOR((mod & MOD_ALT), (x_mod & AltMask))) {
                return FALSE;
            }
            if (LOGICAL_XOR((mod & MOD_META), (x_mod & MetaMask))) {
                return FALSE;
            }
        } else {
            if (LOGICAL_XOR((mod & (MOD_META | MOD_ALT)), (x_mod & (MetaMask | AltMask)))) {
                return FALSE;
            }
        }
        if (LOGICAL_XOR((mod & MOD_LOCK), (x_mod & LockMask))) {
            return FALSE;
        }
        /* These tests can't use LOGICAL_XOR because the second test has an additional
           restriction that the Mod?Mask cannot be set in m; i.e., we want to ignore
           any Mod?Mask assigned to Alt, Meta, or the NumLock On state. */
        if (((mod & MOD_MOD1) && !(x_mod & Mod1Mask)) || (!(mod & MOD_MOD1) && (x_mod & Mod1Mask) && !(Mod1Mask & m))) {
            return FALSE;
        }
        if (((mod & MOD_MOD2) && !(x_mod & Mod2Mask)) || (!(mod & MOD_MOD2) && (x_mod & Mod2Mask) && !(Mod2Mask & m))) {
            return FALSE;
        }
        if (((mod & MOD_MOD3) && !(x_mod & Mod3Mask)) || (!(mod & MOD_MOD3) && (x_mod & Mod3Mask) && !(Mod3Mask & m))) {
            return FALSE;
        }
        if (((mod & MOD_MOD4) && !(x_mod & Mod4Mask)) || (!(mod & MOD_MOD4) && (x_mod & Mod4Mask) && !(Mod4Mask & m))) {
            return FALSE;
        }
        if (((mod & MOD_MOD5) && !(x_mod & Mod5Mask)) || (!(mod & MOD_MOD5) && (x_mod & Mod5Mask) && !(Mod5Mask & m))) {
            return FALSE;
        }
    }
    D_ACTIONS(("Modifier match confirmed.\n"));
    return TRUE;
}

unsigned char
action_dispatch(event_t *ev, KeySym keysym)
{
    action_t *action;

    ASSERT_RVAL(ev != NULL, 0);
    ASSERT_RVAL(ev->xany.type == ButtonPress || ev->xany.type == KeyPress, 0);
    D_ACTIONS(("Event %8p:  Button %d, Keysym 0x%08x, Key State 0x%08x (modifiers " MOD_FMT ")\n", ev, ev->xbutton.button, keysym,
               ev->xkey.state, SHOW_X_MODS(ev->xkey.state)));
    for (action = action_list; action; action = action->next) {
        /* The very first thing we do is match the event type to the type
           of the current action.  This means that we'll only run through
           the modifier checks below if we absolutely have to. */
        if ((ev->xany.type == ButtonPress && action_check_button(action->button, ev->xbutton.button))
            || (ev->xany.type == KeyPress && action_check_keysym(action->keysym, keysym))) {
            if (action_check_modifiers(action->mod, ev->xkey.state)) {
                D_ACTIONS(("Match found.\n"));
                /* If we've passed all the above tests, it's a match.  Dispatch the handler. */
                return ((action->handler) (ev, action));
            }
        }
    }
    return (0);
}

void
action_add(unsigned short mod, unsigned char button, KeySym keysym, action_type_t type, void *param)
{

    action_t *action;

    if (!action_list || !(action = action_find_match(mod, button, keysym))) {
        action = (action_t *) MALLOC(sizeof(action_t));
        action->next = action_list;
        action_list = action;
    } else {
        if (action->type == ACTION_STRING || action->type == ACTION_ECHO || action->type == ACTION_SCRIPT) {
            if (action->param.string) {
                FREE(action->param.string);
            }
        }
    }
    action->mod = mod;
    action->button = button;
    action->type = type;
    action->keysym = keysym;
    switch (type) {
        case ACTION_STRING:
            action->handler = (action_handler_t) action_handle_string;
            action->param.string = (char *) MALLOC(strlen((char *) param) + 2);
            strcpy(action->param.string, (char *) param);
            parse_escaped_string(action->param.string);
            break;
        case ACTION_ECHO:
            action->handler = (action_handler_t) action_handle_echo;
            action->param.string = (char *) MALLOC(strlen((char *) param) + 2);
            strcpy(action->param.string, (char *) param);
            parse_escaped_string(action->param.string);
            break;
        case ACTION_SCRIPT:
            action->handler = (action_handler_t) action_handle_script;
            action->param.script = (char *) MALLOC(strlen((char *) param) + 2);
            strcpy(action->param.script, (char *) param);
            break;
        case ACTION_MENU:
            action->handler = (action_handler_t) action_handle_menu;
            action->param.menu = (menu_t *) param;
            break;
        default:
            break;
    }
    D_ACTIONS(("Added action.  mod == 0x%08x, button == %d, keysym == 0x%08x\n", action->mod, action->button, action->keysym));
}
