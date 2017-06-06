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

static const char cvs_ident[] = "$Id: e.c 51650 2010-08-26 01:34:13Z lucas $";

#include "config.h"
#include "feature.h"

#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <X11/cursorfont.h>
#include <signal.h>

#include "e.h"
#include "command.h"
#include "startup.h"
#include "options.h"
#include "pixmap.h"
#include "system.h"

Window ipc_win = None, my_ipc_win = None;
static unsigned char timeout = 0;

/* Returns true if running under E, false otherwise */
unsigned char
check_for_enlightenment(void)
{
    static signed char have_e = -1;

    if (have_e == -1) {
        if (props[PROP_ENL_COMMS] != None) {
            D_ENL(("Enlightenment detected.\n"));
            have_e = 1;
        } else {
            D_ENL(("Enlightenment not detected.\n"));
            have_e = 0;
        }
    }
    return (have_e);
}

Window
enl_ipc_get_win(void)
{
    unsigned char *str = NULL;
    Atom prop;
    unsigned long num, after;
    int format;
    Window dummy_win;
    int dummy_int;
    unsigned int dummy_uint;

    D_ENL(("Searching for IPC window.\n"));

    if ((props[PROP_ENL_COMMS] == None) || (props[PROP_ENL_VERSION] == None)) {
        D_ENL((" -> Enlightenment is not running.  You lose!\n"));
        return None;
    }
    XGetWindowProperty(Xdisplay, Xroot, props[PROP_ENL_COMMS], 0, 14, False, AnyPropertyType, &prop, &format, &num, &after, &str);
    if (str) {
        sscanf((char *) str, "%*s %x", (unsigned int *) &ipc_win);
        XFree(str);
    }
    if (ipc_win != None) {
        XGetWindowProperty(Xdisplay, Xroot, props[PROP_ENL_VERSION], 0, 14, False, AnyPropertyType,
                           &prop, &format, &num, &after, &str);
        if (str) {
            char *ver, *tmp;

            tmp = strchr((char *) str, ' ');
            if (!tmp) {
                tmp = strchr((char *) str, '-');
            }
            if (tmp) {
                ver = tmp + 1;
                tmp = strchr((char *) ver, ' ');
                if (! tmp) {
                    tmp = strchr((char *) ver, '-');
                }
                if (tmp) {
                    *tmp = 0;
                }

                /* Make sure the version string is within the acceptable range. */
                if (SPIF_CMP_IS_LESS(spiftool_version_compare((spif_charptr_t) str, (spif_charptr_t) "0.16.4"))
                    || SPIF_CMP_IS_GREATER(spiftool_version_compare((spif_charptr_t) str, (spif_charptr_t) "0.16.999"))) {
                    D_ENL((" -> IPC version string \"%s\" out of range.  I'll have to ignore it.\n"));
                    ipc_win = None;
                }
            }
            XFree(str);
        }
    }
    if (ipc_win != None) {
        if (!XGetGeometry
            (Xdisplay, ipc_win, &dummy_win, &dummy_int, &dummy_int, &dummy_uint, &dummy_uint, &dummy_uint, &dummy_uint)) {
            D_ENL((" -> IPC Window property is valid, but the window doesn't exist.  I give up!\n"));
            ipc_win = None;
        }
        str = NULL;
        if (ipc_win != None) {
            XGetWindowProperty(Xdisplay, ipc_win, props[PROP_ENL_COMMS], 0, 14, False, AnyPropertyType, &prop, &format, &num,
                               &after, &str);
            if (str) {
                XFree(str);
            } else {
                D_ENL((" -> IPC Window lacks the proper atom.  I can't talk to fake IPC windows....\n"));
                ipc_win = None;
            }
        }
    }
    if (ipc_win != None) {
        D_ENL((" -> IPC Window found and verified as 0x%08x.  Registering Eterm as an IPC client.\n", (int) ipc_win));
        XSelectInput(Xdisplay, ipc_win, StructureNotifyMask | SubstructureNotifyMask);
        enl_ipc_send("set clientname " APL_NAME);
        enl_ipc_send("set version " VERSION);
        enl_ipc_send("set email mej@eterm.org");
        enl_ipc_send("set web http://www.eterm.org/");
        enl_ipc_send("set info Eterm Enlightened Terminal Emulator");
    }
    if (my_ipc_win == None) {
        my_ipc_win = XCreateSimpleWindow(Xdisplay, Xroot, -2, -2, 1, 1, 0, 0, 0);
    }
    return (ipc_win);
}

void
enl_ipc_send(char *str)
{
    static char *last_msg = NULL;
    char buff[21];
    register unsigned short i;
    register unsigned char j;
    unsigned short len;
    XEvent ev;

    if (!str) {
        ASSERT(last_msg != NULL);
        str = last_msg;
        D_ENL(("Resending last message \"%s\" to Enlightenment.\n", str));
    } else {
        if (last_msg) {
            FREE(last_msg);
        }
        last_msg = STRDUP(str);
        D_ENL(("Sending \"%s\" to Enlightenment.\n", str));
    }

    if (ipc_win == None) {
        if ((ipc_win = enl_ipc_get_win()) == None) {
            D_ENL(("...or perhaps not, since Enlightenment doesn't seem to be running.  No IPC window, no IPC.  Sorry....\n"));
            return;
        }
    }
    len = strlen(str);
    for (; XCheckTypedWindowEvent(Xdisplay, my_ipc_win, ClientMessage, &ev););  /* Discard any out-of-sync messages */
    ev.xclient.type = ClientMessage;
    ev.xclient.serial = 0;
    ev.xclient.send_event = True;
    ev.xclient.window = ipc_win;
    ev.xclient.message_type = props[PROP_ENL_MSG];
    ev.xclient.format = 8;

    for (i = 0; i < len + 1; i += 12) {
        sprintf(buff, "%8x", (int) my_ipc_win);
        for (j = 0; j < 12; j++) {
            buff[8 + j] = str[i + j];
            if (!str[i + j]) {
                break;
            }
        }
        buff[20] = 0;
        for (j = 0; j < 20; j++) {
            ev.xclient.data.b[j] = buff[j];
        }
        XSendEvent(Xdisplay, ipc_win, False, 0, (XEvent *) & ev);
    }
    D_ENL(("Message sent to IPC window 0x%08x.\n", ipc_win));
}

static RETSIGTYPE
enl_ipc_timeout(int sig)
{
    timeout = 1;
    SIG_RETURN(sig);
    sig = 0;
}

char *
enl_wait_for_reply(void)
{

    XEvent ev;
    static char msg_buffer[20];
    register unsigned char i;

    alarm(3);
    for (; !XCheckTypedWindowEvent(Xdisplay, my_ipc_win, ClientMessage, &ev) && !timeout;);
    alarm(0);
    if (ev.xany.type != ClientMessage) {
        return (IPC_TIMEOUT);
    }
    for (i = 0; i < 20; i++) {
        msg_buffer[i] = ev.xclient.data.b[i];
    }
    return (msg_buffer + 8);
}

char *
enl_ipc_get(const char *msg_data)
{

    static char *message = NULL;
    static unsigned short len = 0;
    char buff[13], *ret_msg = NULL;
    register unsigned char i;
    unsigned char blen;

    if (msg_data == IPC_TIMEOUT) {
        return (IPC_TIMEOUT);
    }
    for (i = 0; i < 12; i++) {
        buff[i] = msg_data[i];
    }
    buff[12] = 0;
    blen = strlen(buff);
    if (message) {
        len += blen;
        message = (char *) REALLOC(message, len + 1);
        strcat(message, buff);
    } else {
        len = blen;
        message = (char *) MALLOC(len + 1);
        strcpy(message, buff);
    }
    if (blen < 12) {
        ret_msg = message;
        message = NULL;
        D_ENL(("Received complete reply:  \"%s\"\n", ret_msg));
    }
    return (ret_msg);
}

char *
enl_send_and_wait(char *msg)
{

    char *reply = IPC_TIMEOUT;
    eterm_sighandler_t old_alrm;

    if (ipc_win == None) {
        /* The IPC window is missing.  Wait for it to return or Eterm to be killed. */
        for (; enl_ipc_get_win() == None;) {
            sleep(1);
        }
    }
    old_alrm = (eterm_sighandler_t) signal(SIGALRM, (eterm_sighandler_t) enl_ipc_timeout);
    for (; reply == IPC_TIMEOUT;) {
        timeout = 0;
        enl_ipc_send(msg);
        for (; !(reply = enl_ipc_get(enl_wait_for_reply())););
        if (reply == IPC_TIMEOUT) {
            /* We timed out.  The IPC window must be AWOL.  Reset and resend message. */
            D_ENL(("IPC timed out.  IPC window 0x%08x has gone AWOL.  Clearing ipc_win.\n", ipc_win));
            XSelectInput(Xdisplay, ipc_win, None);
            ipc_win = None;
            (void) check_image_ipc(1);
        }
    }
    signal(SIGALRM, old_alrm);
    return (reply);
}
