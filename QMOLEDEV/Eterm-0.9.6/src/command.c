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

static const char cvs_ident[] = "$Id: command.c 59413 2011-05-15 21:19:59Z mej $";

/* includes: */
#include "config.h"
#include "feature.h"

/* System Headers */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <sys/types.h>
#include <limits.h>

/* X11 Headers */
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>
#include <X11/Xproto.h>
#include <X11/IntrinsicP.h>
#ifdef HAVE_XRES_EXT
# include <X11/extensions/XRes.h>
#endif
#ifdef PTY_GRP_NAME
# include <grp.h>
#endif
#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#if defined (__svr4__)
# include <sys/resource.h>      /* for struct rlimit */
# include <sys/stropts.h>       /* for I_PUSH */
# ifdef HAVE_SYS_STRTIO_H
#  include <sys/strtio.h>
# endif
# ifdef HAVE_BSDTTY_H
#  include <bsdtty.h>
# endif
# define _NEW_TTY_CTRL          /* to get proper defines in <termios.h> */
#endif
#ifdef HAVE_SYS_SELECT_H
# include <sys/select.h>
#endif
#ifdef HAVE_TERMIOS_H
# include <termios.h>
#else
# include <sgtty.h>
#endif
#if defined(__sun) && defined(__SVR4)
# include <sys/strredir.h>
#endif
#include <sys/wait.h>
#include <sys/stat.h>
#if defined(linux)
# include <linux/tty.h>         /* For N_TTY_BUF_SIZE. */
#endif
#ifdef MULTI_CHARSET
# include <locale.h>
# include <langinfo.h>
# include <iconv.h>
# include <wchar.h>
#endif

/* Eterm-specific Headers */
#include "command.h"
#include "startup.h"
#include "events.h"
#include "font.h"
#include "grkelot.h"
#include "options.h"
#include "pixmap.h"
#ifdef PROFILE
# include "profile.h"
#endif
#include "screen.h"
#include "scrollbar.h"
#include "string.h"
#include "term.h"
#ifdef UTMP_SUPPORT
# include "eterm_utmp.h"
#endif
#include "windows.h"
#include "buttons.h"
#include "menus.h"

#ifdef ESCREEN
# include "screamcfg.h"
# ifdef NS_HAVE_TWIN
#  include <Tw/Tw.h>
TW_DECL_MAGIC(libscream_magic);
# endif
#endif

static RETSIGTYPE handle_child_signal(int);
static RETSIGTYPE handle_exit_signal(int);
static RETSIGTYPE handle_crash(int);
static RETSIGTYPE x_resource_dump(int);

/* local variables */
int my_ruid, my_euid, my_rgid, my_egid;
char initial_dir[PATH_MAX + 1];
static char *ptydev = NULL, *ttydev = NULL;     /* pty/tty name */
int cmd_fd = -1;                /* file descriptor connected to the command */
int pipe_fd = -1;
pid_t cmd_pid = -1;             /* process id if child */
int Xfd = -1;                   /* file descriptor of X server connection */
unsigned int num_fds = 0;       /* number of file descriptors being used */
struct stat ttyfd_stat;         /* original status of the tty we will use */
int refresh_count = 0, refresh_limit = 1, refresh_type = FAST_REFRESH;
unsigned char cmdbuf_base[CMD_BUF_SIZE], *cmdbuf_ptr, *cmdbuf_endp;

/* Addresses pasting large amounts of data
 * code pinched from xterm
 */
static char *v_buffer;          /* pointer to physical buffer */
static char *v_bufstr = NULL;   /* beginning of area to write */
static char *v_bufptr;          /* end of area to write */
static char *v_bufend;          /* end of physical buffer */

#ifdef USE_XIM
XIM xim_input_method = NULL;
XIC xim_input_context = NULL;   /* input context */
static XIMStyle xim_input_style = 0;

# ifndef XSetIMValues
extern char *XSetIMValues(XIM im, ...);
# endif
#endif

/* Substitutes for missing system functions */
#if !defined(_POSIX_VERSION) && defined(__svr4__)
int
getdtablesize(void)
{
    struct rlimit rlim;

    getrlimit(RLIMIT_NOFILE, &rlim);
    return rlim.rlim_cur;
}
# endif

/* Take care of suid/sgid super-user (root) privileges */
void
privileges(int mode)
{

#ifdef __CYGWIN32__
    return;
#endif

    switch (mode) {
        case IGNORE:
            /* Revoke suid/sgid privs and return to normal uid/gid -- mej */
            D_UTMP(("[%ld]: Before privileges(REVERT): [ %ld, %ld ]  [ %ld, %ld ]\n", getpid(), getuid(), getgid(), geteuid(),
                    getegid()));

#ifdef HAVE_SETRESGID
            setresgid(my_rgid, my_rgid, my_egid);
#elif defined(HAVE_SAVED_UIDS)
            setregid(my_rgid, my_rgid);
#else
            setregid(my_egid, -1);
            setregid(-1, my_rgid);
#endif

#ifdef HAVE_SETRESUID
            setresuid(my_ruid, my_ruid, my_euid);
#elif defined(HAVE_SAVED_UIDS)
            setreuid(my_ruid, my_ruid);
#else
            setreuid(my_euid, -1);
            setreuid(-1, my_ruid);
#endif

            D_UTMP(("[%ld]: After privileges(REVERT): [ %ld, %ld ]  [ %ld, %ld ]\n", getpid(), getuid(), getgid(), geteuid(),
                    getegid()));
            break;

        case SAVE:
            break;

        case RESTORE:
            D_UTMP(("[%ld]: Before privileges(INVOKE): [ %ld, %ld ]  [ %ld, %ld ]\n", getpid(), getuid(), getgid(), geteuid(),
                    getegid()));

#ifdef HAVE_SETRESUID
            setresuid(my_ruid, my_euid, my_euid);
#elif defined(HAVE_SAVED_UIDS)
            setreuid(my_ruid, my_euid);
#else
            setreuid(-1, my_euid);
            setreuid(my_ruid, -1);
#endif

#ifdef HAVE_SETRESGID
            setresgid(my_rgid, my_egid, my_egid);
#elif defined(HAVE_SAVED_UIDS)
            setregid(my_rgid, my_egid);
#else
            setregid(-1, my_egid);
            setregid(my_rgid, -1);
#endif

            D_UTMP(("[%ld]: After privileges(INVOKE): [ %ld, %ld ]  [ %ld, %ld ]\n", getpid(), getuid(), getgid(), geteuid(),
                    getegid()));
            break;
    }
}

char *
sig_to_str(int sig)
{
#ifdef SIGHUP
    if (sig == SIGHUP) {
        return ("SIGHUP");
    }
#endif

#ifdef SIGINT
    if (sig == SIGINT) {
        return ("SIGINT");
    }
#endif

#ifdef SIGQUIT
    if (sig == SIGQUIT) {
        return ("SIGQUIT");
    }
#endif

#ifdef SIGILL
    if (sig == SIGILL) {
        return ("SIGILL");
    }
#endif

#ifdef SIGTRAP
    if (sig == SIGTRAP) {
        return ("SIGTRAP");
    }
#endif

#ifdef SIGABRT
    if (sig == SIGABRT) {
        return ("SIGABRT");
    }
#endif

#ifdef SIGIOT
    if (sig == SIGIOT) {
        return ("SIGIOT");
    }
#endif

#ifdef SIGEMT
    if (sig == SIGEMT) {
        return ("SIGEMT");
    }
#endif

#ifdef SIGFPE
    if (sig == SIGFPE) {
        return ("SIGFPE");
    }
#endif

#ifdef SIGKILL
    if (sig == SIGKILL) {
        return ("SIGKILL");
    }
#endif

#ifdef SIGBUS
    if (sig == SIGBUS) {
        return ("SIGBUS");
    }
#endif

#ifdef SIGSEGV
    if (sig == SIGSEGV) {
        return ("SIGSEGV");
    }
#endif

#ifdef SIGSYS
    if (sig == SIGSYS) {
        return ("SIGSYS");
    }
#endif

#ifdef SIGPIPE
    if (sig == SIGPIPE) {
        return ("SIGPIPE");
    }
#endif

#ifdef SIGALRM
    if (sig == SIGALRM) {
        return ("SIGALRM");
    }
#endif

#ifdef SIGTERM
    if (sig == SIGTERM) {
        return ("SIGTERM");
    }
#endif

#ifdef SIGUSR1
    if (sig == SIGUSR1) {
        return ("SIGUSR1");
    }
#endif

#ifdef SIGUSR2
    if (sig == SIGUSR2) {
        return ("SIGUSR2");
    }
#endif

#ifdef SIGCHLD
    if (sig == SIGCHLD) {
        return ("SIGCHLD");
    }
#endif

#ifdef SIGCLD
    if (sig == SIGCLD) {
        return ("SIGCLD");
    }
#endif

#ifdef SIGPWR
    if (sig == SIGPWR) {
        return ("SIGPWR");
    }
#endif

#ifdef SIGVTALRM
    if (sig == SIGVTALRM) {
        return ("SIGVTALRM");
    }
#endif

#ifdef SIGPROF
    if (sig == SIGPROF) {
        return ("SIGPROF");
    }
#endif

#ifdef SIGIO
    if (sig == SIGIO) {
        return ("SIGIO");
    }
#endif

#ifdef SIGPOLL
    if (sig == SIGPOLL) {
        return ("SIGPOLL");
    }
#endif

#ifdef SIGWINCH
    if (sig == SIGWINCH) {
        return ("SIGWINCH");
    }
#endif

#ifdef SIGWINDOW
    if (sig == SIGWINDOW) {
        return ("SIGWINDOW");
    }
#endif

#ifdef SIGSTOP
    if (sig == SIGSTOP) {
        return ("SIGSTOP");
    }
#endif

#ifdef SIGTSTP
    if (sig == SIGTSTP) {
        return ("SIGTSTP");
    }
#endif

#ifdef SIGCONT
    if (sig == SIGCONT) {
        return ("SIGCONT");
    }
#endif

#ifdef SIGTTIN
    if (sig == SIGTTIN) {
        return ("SIGTTIN");
    }
#endif

#ifdef SIGTTOU
    if (sig == SIGTTOU) {
        return ("SIGTTOU");
    }
#endif

#ifdef SIGURG
    if (sig == SIGURG) {
        return ("SIGURG");
    }
#endif

#ifdef SIGLOST
    if (sig == SIGLOST) {
        return ("SIGLOST");
    }
#endif

#ifdef SIGRESERVE
    if (sig == SIGRESERVE) {
        return ("SIGRESERVE");
    }
#endif

#ifdef SIGDIL
    if (sig == SIGDIL) {
        return ("SIGDIL");
    }
#endif

#ifdef SIGXCPU
    if (sig == SIGXCPU) {
        return ("SIGXCPU");
    }
#endif

#ifdef SIGXFSZ
    if (sig == SIGXFSZ) {
        return ("SIGXFSZ");
    }
#endif
    return ("Unknown signal");
}

const char *
event_type_to_name(int type)
{

    if (type == KeyPress) {
        return "KeyPress";
    }
    if (type == KeyRelease) {
        return "KeyRelease";
    }
    if (type == ButtonPress) {
        return "ButtonPress";
    }
    if (type == ButtonRelease) {
        return "ButtonRelease";
    }
    if (type == MotionNotify) {
        return "MotionNotify";
    }
    if (type == EnterNotify) {
        return "EnterNotify";
    }
    if (type == LeaveNotify) {
        return "LeaveNotify";
    }
    if (type == FocusIn) {
        return "FocusIn";
    }
    if (type == FocusOut) {
        return "FocusOut";
    }
    if (type == KeymapNotify) {
        return "KeymapNotify";
    }
    if (type == Expose) {
        return "Expose";
    }
    if (type == GraphicsExpose) {
        return "GraphicsExpose";
    }
    if (type == NoExpose) {
        return "NoExpose";
    }
    if (type == VisibilityNotify) {
        return "VisibilityNotify";
    }
    if (type == CreateNotify) {
        return "CreateNotify";
    }
    if (type == DestroyNotify) {
        return "DestroyNotify";
    }
    if (type == UnmapNotify) {
        return "UnmapNotify";
    }
    if (type == MapNotify) {
        return "MapNotify";
    }
    if (type == MapRequest) {
        return "MapRequest";
    }
    if (type == ReparentNotify) {
        return "ReparentNotify";
    }
    if (type == ConfigureNotify) {
        return "ConfigureNotify";
    }
    if (type == ConfigureRequest) {
        return "ConfigureRequest";
    }
    if (type == GravityNotify) {
        return "GravityNotify";
    }
    if (type == ResizeRequest) {
        return "ResizeRequest";
    }
    if (type == CirculateNotify) {
        return "CirculateNotify";
    }
    if (type == CirculateRequest) {
        return "CirculateRequest";
    }
    if (type == PropertyNotify) {
        return "PropertyNotify";
    }
    if (type == SelectionClear) {
        return "SelectionClear";
    }
    if (type == SelectionRequest) {
        return "SelectionRequest";
    }
    if (type == SelectionNotify) {
        return "SelectionNotify";
    }
    if (type == ColormapNotify) {
        return "ColormapNotify";
    }
    if (type == ClientMessage) {
        return "ClientMessage";
    }
    if (type == MappingNotify) {
        return "MappingNotify";
    }
    return "Bad Event!";
}

const char *
request_code_to_name(int code)
{

    if (code == X_CreateWindow) {
        return "XCreateWindow";
    }
    if (code == X_ChangeWindowAttributes) {
        return "XChangeWindowAttributes";
    }
    if (code == X_GetWindowAttributes) {
        return "XGetWindowAttributes";
    }
    if (code == X_DestroyWindow) {
        return "XDestroyWindow";
    }
    if (code == X_DestroySubwindows) {
        return "XDestroySubwindows";
    }
    if (code == X_ChangeSaveSet) {
        return "XChangeSaveSet";
    }
    if (code == X_ReparentWindow) {
        return "XReparentWindow";
    }
    if (code == X_MapWindow) {
        return "XMapWindow";
    }
    if (code == X_MapSubwindows) {
        return "XMapSubwindows";
    }
    if (code == X_UnmapWindow) {
        return "XUnmapWindow";
    }
    if (code == X_UnmapSubwindows) {
        return "XUnmapSubwindows";
    }
    if (code == X_ConfigureWindow) {
        return "XConfigureWindow";
    }
    if (code == X_CirculateWindow) {
        return "XCirculateWindow";
    }
    if (code == X_GetGeometry) {
        return "XGetGeometry";
    }
    if (code == X_QueryTree) {
        return "XQueryTree";
    }
    if (code == X_InternAtom) {
        return "XInternAtom";
    }
    if (code == X_GetAtomName) {
        return "XGetAtomName";
    }
    if (code == X_ChangeProperty) {
        return "XChangeProperty";
    }
    if (code == X_DeleteProperty) {
        return "XDeleteProperty";
    }
    if (code == X_GetProperty) {
        return "XGetProperty";
    }
    if (code == X_ListProperties) {
        return "XListProperties";
    }
    if (code == X_SetSelectionOwner) {
        return "XSetSelectionOwner";
    }
    if (code == X_GetSelectionOwner) {
        return "XGetSelectionOwner";
    }
    if (code == X_ConvertSelection) {
        return "XConvertSelection";
    }
    if (code == X_SendEvent) {
        return "XSendEvent";
    }
    if (code == X_GrabPointer) {
        return "XGrabPointer";
    }
    if (code == X_UngrabPointer) {
        return "XUngrabPointer";
    }
    if (code == X_GrabButton) {
        return "XGrabButton";
    }
    if (code == X_UngrabButton) {
        return "XUngrabButton";
    }
    if (code == X_ChangeActivePointerGrab) {
        return "XChangeActivePointerGrab";
    }
    if (code == X_GrabKeyboard) {
        return "XGrabKeyboard";
    }
    if (code == X_UngrabKeyboard) {
        return "XUngrabKeyboard";
    }
    if (code == X_GrabKey) {
        return "XGrabKey";
    }
    if (code == X_UngrabKey) {
        return "XUngrabKey";
    }
    if (code == X_AllowEvents) {
        return "XAllowEvents";
    }
    if (code == X_GrabServer) {
        return "XGrabServer";
    }
    if (code == X_UngrabServer) {
        return "XUngrabServer";
    }
    if (code == X_QueryPointer) {
        return "XQueryPointer";
    }
    if (code == X_GetMotionEvents) {
        return "XGetMotionEvents";
    }
    if (code == X_TranslateCoords) {
        return "XTranslateCoords";
    }
    if (code == X_WarpPointer) {
        return "XWarpPointer";
    }
    if (code == X_SetInputFocus) {
        return "XSetInputFocus";
    }
    if (code == X_GetInputFocus) {
        return "XGetInputFocus";
    }
    if (code == X_QueryKeymap) {
        return "XQueryKeymap";
    }
    if (code == X_OpenFont) {
        return "XOpenFont";
    }
    if (code == X_CloseFont) {
        return "XCloseFont";
    }
    if (code == X_QueryFont) {
        return "XQueryFont";
    }
    if (code == X_QueryTextExtents) {
        return "XQueryTextExtents";
    }
    if (code == X_ListFonts) {
        return "XListFonts";
    }
    if (code == X_ListFontsWithInfo) {
        return "XListFontsWithInfo";
    }
    if (code == X_SetFontPath) {
        return "XSetFontPath";
    }
    if (code == X_GetFontPath) {
        return "XGetFontPath";
    }
    if (code == X_CreatePixmap) {
        return "XCreatePixmap";
    }
    if (code == X_FreePixmap) {
        return "XFreePixmap";
    }
    if (code == X_CreateGC) {
        return "XCreateGC";
    }
    if (code == X_ChangeGC) {
        return "XChangeGC";
    }
    if (code == X_CopyGC) {
        return "XCopyGC";
    }
    if (code == X_SetDashes) {
        return "XSetDashes";
    }
    if (code == X_SetClipRectangles) {
        return "XSetClipRectangles";
    }
    if (code == X_FreeGC) {
        return "XFreeGC";
    }
    if (code == X_ClearArea) {
        return "XClearArea";
    }
    if (code == X_CopyArea) {
        return "XCopyArea";
    }
    if (code == X_CopyPlane) {
        return "XCopyPlane";
    }
    if (code == X_PolyPoint) {
        return "XPolyPoint";
    }
    if (code == X_PolyLine) {
        return "XPolyLine";
    }
    if (code == X_PolySegment) {
        return "XPolySegment";
    }
    if (code == X_PolyRectangle) {
        return "XPolyRectangle";
    }
    if (code == X_PolyArc) {
        return "XPolyArc";
    }
    if (code == X_FillPoly) {
        return "XFillPoly";
    }
    if (code == X_PolyFillRectangle) {
        return "XPolyFillRectangle";
    }
    if (code == X_PolyFillArc) {
        return "XPolyFillArc";
    }
    if (code == X_PutImage) {
        return "XPutImage";
    }
    if (code == X_GetImage) {
        return "XGetImage";
    }
    if (code == X_PolyText8) {
        return "XPolyText8";
    }
    if (code == X_PolyText16) {
        return "XPolyText16";
    }
    if (code == X_ImageText8) {
        return "XImageText8";
    }
    if (code == X_ImageText16) {
        return "XImageText16";
    }
    if (code == X_CreateColormap) {
        return "XCreateColormap";
    }
    if (code == X_FreeColormap) {
        return "XFreeColormap";
    }
    if (code == X_CopyColormapAndFree) {
        return "XCopyColormapAndFree";
    }
    if (code == X_InstallColormap) {
        return "XInstallColormap";
    }
    if (code == X_UninstallColormap) {
        return "XUninstallColormap";
    }
    if (code == X_ListInstalledColormaps) {
        return "XListInstalledColormaps";
    }
    if (code == X_AllocColor) {
        return "XAllocColor";
    }
    if (code == X_AllocNamedColor) {
        return "XAllocNamedColor";
    }
    if (code == X_AllocColorCells) {
        return "XAllocColorCells";
    }
    if (code == X_AllocColorPlanes) {
        return "XAllocColorPlanes";
    }
    if (code == X_FreeColors) {
        return "XFreeColors";
    }
    if (code == X_StoreColors) {
        return "XStoreColors";
    }
    if (code == X_StoreNamedColor) {
        return "XStoreNamedColor";
    }
    if (code == X_QueryColors) {
        return "XQueryColors";
    }
    if (code == X_LookupColor) {
        return "XLookupColor";
    }
    if (code == X_CreateCursor) {
        return "XCreateCursor";
    }
    if (code == X_CreateGlyphCursor) {
        return "XCreateGlyphCursor";
    }
    if (code == X_FreeCursor) {
        return "XFreeCursor";
    }
    if (code == X_RecolorCursor) {
        return "XRecolorCursor";
    }
    if (code == X_QueryBestSize) {
        return "XQueryBestSize";
    }
    if (code == X_QueryExtension) {
        return "XQueryExtension";
    }
    if (code == X_ListExtensions) {
        return "XListExtensions";
    }
    if (code == X_ChangeKeyboardMapping) {
        return "XChangeKeyboardMapping";
    }
    if (code == X_GetKeyboardMapping) {
        return "XGetKeyboardMapping";
    }
    if (code == X_ChangeKeyboardControl) {
        return "XChangeKeyboardControl";
    }
    if (code == X_GetKeyboardControl) {
        return "XGetKeyboardControl";
    }
    if (code == X_Bell) {
        return "XBell";
    }
    if (code == X_ChangePointerControl) {
        return "XChangePointerControl";
    }
    if (code == X_GetPointerControl) {
        return "XGetPointerControl";
    }
    if (code == X_SetScreenSaver) {
        return "XSetScreenSaver";
    }
    if (code == X_GetScreenSaver) {
        return "XGetScreenSaver";
    }
    if (code == X_ChangeHosts) {
        return "XChangeHosts";
    }
    if (code == X_ListHosts) {
        return "XListHosts";
    }
    if (code == X_SetAccessControl) {
        return "XSetAccessControl";
    }
    if (code == X_SetCloseDownMode) {
        return "XSetCloseDownMode";
    }
    if (code == X_KillClient) {
        return "XKillClient";
    }
    if (code == X_RotateProperties) {
        return "XRotateProperties";
    }
    if (code == X_ForceScreenSaver) {
        return "XForceScreenSaver";
    }
    if (code == X_SetPointerMapping) {
        return "XSetPointerMapping";
    }
    if (code == X_GetPointerMapping) {
        return "XGetPointerMapping";
    }
    if (code == X_SetModifierMapping) {
        return "XSetModifierMapping";
    }
    if (code == X_GetModifierMapping) {
        return "XGetModifierMapping";
    }
    if (code == X_NoOperation) {
        return "XNoOperation";
    }
    return "Unknown";
}

const char *
get_ctrl_char_name(char c)
{
    const char *lookup[] = {
        "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", /*  0-7  */
        "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", /*  8-15 */
        "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", /* 16-23 */
        "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US"       /* 24-31 */
    };

    return ((c < ' ') ? (lookup[(int) c]) : (""));
}

static void
hard_exit(void)
{

#ifdef HAVE__EXIT
    _exit(-1);
#elif defined(SIGKILL)
    kill(cmd_pid, SIGKILL);
    abort();
#else
    kill(cmd_pid, 9);
    abort();
#endif

}

/* Try to get a stack trace when we croak */
void
dump_stack_trace(void)
{
    char cmd[256];
#ifdef GDB
    struct stat st;
#endif

#ifdef NO_STACK_TRACE
    return;
#endif

    libast_print_error("Attempting to dump a stack trace....\n");
    signal(SIGTSTP, exit);      /* Don't block on tty output, just die */

#ifdef HAVE_U_STACK_TRACE
    U_STACK_TRACE();
    return;
#elif defined(GDB)
    if (((stat(GDB_CMD_FILE, &st)) != 0) || (!S_ISREG(st.st_mode))) {
        return;
    }
    snprintf(cmd, sizeof(cmd), GDB " -x " GDB_CMD_FILE " " APL_NAME " %d", getpid());
#elif defined(PSTACK)
    snprintf(cmd, sizeof(cmd), PSTACK " %d", getpid());
#elif defined(DBX)
#  ifdef _AIX
    snprintf(cmd, sizeof(cmd), "/bin/echo 'where\ndetach' | " DBX " -a %d", getpid());
#  elif defined(__sgi)
    snprintf(cmd, sizeof(cmd), "/bin/echo 'where\ndetach' | " DBX " -p %d", getpid());
#  else
    snprintf(cmd, sizeof(cmd), "/bin/echo 'where\ndetach' | " DBX " %s %d", orig_argv0, getpid());
#  endif
#else
    libast_print_error("Your system does not support any of the methods Eterm uses.  Exiting.\n");
    return;
#endif
    signal(SIGALRM, (eterm_sighandler_t) hard_exit);
    alarm(3);
    system(cmd);
}

/* signal handling, exit handler */
/*
 * Catch a SIGCHLD signal and exit if the direct child has died
 */
static RETSIGTYPE
handle_child_signal(int sig)
{

    int pid, save_errno = errno;

    D_CMD(("Received signal %s (%d)\n", sig_to_str(sig), sig));

    do {
        errno = 0;
    } while ((-1 == (pid = waitpid(-1, NULL, WNOHANG))) && (errno == EINTR));

    D_CMD(("pid == %d, cmd_pid == %d\n", pid, cmd_pid));
    /* If the child that exited is the command we spawned, or if the
       child exited before fork() returned in the parent, it must be
       our immediate child that exited.  We exit gracefully. */
    if ((cmd_pid != -1)
        && ((pid == cmd_pid)
            || ((pid == -1) && (errno == ECHILD))
            || ((pid == 0) && ((kill(cmd_pid, 0)) < 0)))) {
        cmd_pid = -1;
        if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_PAUSE)) {
            paused = 1;
            return;
        }
        exit(EXIT_SUCCESS);
    }

    errno = save_errno;

    D_CMD(("handle_child_signal: installing signal handler\n"));
    signal(SIGCHLD, handle_child_signal);

    SIG_RETURN(0);
}

/* Handles signals usually sent by a user, like HUP, TERM, INT. */
static RETSIGTYPE
handle_exit_signal(int sig)
{

    libast_print_error("Received terminal signal %s (%d)\n", sig_to_str(sig), sig);
    signal(sig, SIG_DFL);

#ifdef UTMP_SUPPORT
    privileges(INVOKE);
    remove_utmp_entry();
    privileges(REVERT);
#endif

    D_CMD(("exit(%s)\n", sig_to_str(sig)));
    exit(sig);
    SIG_RETURN(0);
}

/* Handles abnormal termination signals -- mej */
static RETSIGTYPE
handle_crash(int sig)
{

    libast_print_error("Received terminal signal %s (%d)\n", sig_to_str(sig), sig);
    signal(sig, SIG_DFL);       /* Let the OS handle recursive seg faults */

    /* Lock down security so we don't write any core files as root. */
    privileges(REVERT);
    umask(077);

    /* Make an attempt to dump a stack trace */
    dump_stack_trace();

    /* Exit */
    exit(sig);
    SIG_RETURN(0);
}

#ifdef HAVE_XRES_EXT
static RETSIGTYPE
x_resource_dump(int sig)
{
    int event_base, error_base, count, i;
    unsigned long bytes;
    XResClient *clients = NULL;
    XResType *types = NULL;
    Atom pixmap_atom, gc_atom, font_atom;
    pid_t my_pid;
    char *title, *ptitle;

    USE_VAR(sig);
    my_pid = getpid();

    /* Create type atoms for future use. */
    pixmap_atom = XInternAtom(Xdisplay, "PIXMAP", False);
    gc_atom = XInternAtom(Xdisplay, "GC", False);
    font_atom = XInternAtom(Xdisplay, "FONT", False);

    /* Look at what *we* think our consumption is. */
#if DEBUG >= DEBUG_MEM
    if (DEBUG_LEVEL >= DEBUG_MEM) {
        PIXMAP_DUMP();
        GC_DUMP();
    }
#endif

    /* Make sure we have the extension loaded. */
    if (!XResQueryExtension(Xdisplay, &event_base, &error_base)) {
        fprintf(stderr, "XResource extension not available on current display.\n");
        return;
    }
    D_X11(("Got XResource extension values:  %d (0x%08x) / %d (0x%08x)\n",
           event_base, event_base, error_base, error_base));

    /* Get a list of X clients and find our window ID in the list. */
    if (!XResQueryClients(Xdisplay, &count, &clients)) {
        if (clients) {
            XFree(clients);
        }
        D_X11((" -> Unable to query clients.\n"));
        return;
    }
    D_X11((" -> Got %d clients.\n", count));

    if (count == 0) {
        D_X11((" -> Nothing to do!\n"));
        return;
    }

    for (i = 0; i < count; i++) {
        Window win;

        win = clients[i].resource_base & (~clients[i].resource_mask);
        D_X11(("Checking client:  base %d, mask %d, window 0x%08x\n", clients[i].resource_base,
               clients[i].resource_mask, win));
        if ((TermWin.parent & (~clients[i].resource_mask)) == win) {
            break;
        }
    }
    if (i == count) {
        D_X11((" -> No client found with window 0x%08x (0x%08x\n", TermWin.parent,
               (TermWin.parent & (~clients[i].resource_mask))));
        return;
    }

    /* Request resource info for our client ID. */
    if (!XResQueryClientResources(Xdisplay, clients[i].resource_base, &count, &types)
        || !XResQueryClientPixmapBytes(Xdisplay, clients[i].resource_base, &bytes)) {
        if (types) {
            XFree(types);
        }
        D_X11((" -> Unable to query resources.\n"));
        return;
    }
    D_X11((" -> Got %d types.\n", count));

    /* Get and sanitize window title for easier identification. */
    XFetchName(Xdisplay, TermWin.parent, &title);
    if (title) {
        for (ptitle = title; *ptitle; ptitle++) {
            if (!isprint(*ptitle)) {
                *ptitle = ' ';
            }
        }
    }

    for (i = 0; i < count; i++) {
        if (types[i].resource_type == pixmap_atom) {
            fprintf(stderr, "Process %lu, window 0x%08x (%s):  %d pixmaps (%lu bytes).\n", (unsigned long) my_pid,
                    (unsigned int) TermWin.parent, NONULL(title), types[i].count, bytes);
        } else if (types[i].resource_type == gc_atom) {
            fprintf(stderr, "Process %lu, window 0x%08x (%s):  %d GC's (%d bytes).\n", (unsigned long) my_pid,
                    (unsigned int) TermWin.parent, NONULL(title), types[i].count,
                    types[i].count * (sizeof(XGCValues) + sizeof(GC)));
        } else if (types[i].resource_type == font_atom) {
            fprintf(stderr, "Process %lu, window 0x%08x (%s):  %d fonts (%d bytes).\n", (unsigned long) my_pid,
                    (unsigned int) TermWin.parent, NONULL(title), types[i].count,
                    types[i].count * (sizeof(XFontStruct) + sizeof(Font)));
        }
    }
    XFree(clients);
    XFree(types);
    if (title) {
        XFree(title);
    }
}
#endif

void
install_handlers(void)
{
    /* Ignore SIGHUP */
    /* signal(SIGHUP, handle_exit_signal); */
    signal(SIGHUP, SIG_IGN);
#ifndef __svr4__
    signal(SIGINT, handle_exit_signal);
#endif
    signal(SIGTERM, handle_exit_signal);
    signal(SIGCHLD, handle_child_signal);
    signal(SIGQUIT, handle_crash);
    signal(SIGSEGV, handle_crash);
    signal(SIGBUS, handle_crash);
    signal(SIGABRT, handle_crash);
    signal(SIGFPE, handle_crash);
    signal(SIGILL, handle_crash);
    signal(SIGSYS, handle_crash);
    signal(SIGPIPE, SIG_IGN);
#ifdef HAVE_XRES_EXT
    signal(SIGUSR1, x_resource_dump);
#else
    signal(SIGUSR1, SIG_IGN);
#endif
}

/* Exit gracefully, clearing the utmp entry and restoring tty attributes */
void
clean_exit(void)
{
#if DEBUG >= DEBUG_MEM
    if (DEBUG_LEVEL >= DEBUG_MEM) {
        unsigned short i;

        /* Deallocate all our crap to help find memory leaks */
        selection_clear();
        scr_release();
        bbar_free(buttonbar);
        menulist_clear(menu_list);
        font_cache_clear();
        eterm_font_list_clear();
# ifdef PIXMAP_SUPPORT
        FOREACH_IMAGE(free_eterm_image(&(images[idx])););
# endif
        for (i = 0; i < NRS_COLORS; i++) {
            if (rs_color[i]) {
                FREE(rs_color[i]);
            }
        }
        spifconf_free_subsystem();
# ifdef USE_XIM
        if (xim_input_context) {
            XUnsetICFocus(xim_input_context);
            XDestroyIC(xim_input_context);
        }
        if (xim_input_method) {
            XCloseIM(xim_input_method);
            xim_input_method = NULL;
        }
# endif
        XCloseDisplay(Xdisplay);
    }
#endif

    privileges(INVOKE);

#ifndef __CYGWIN32__
    if (ttydev) {
        D_CMD(("Restoring \"%s\" to mode %03o, uid %d, gid %d\n", ttydev, ttyfd_stat.st_mode, ttyfd_stat.st_uid,
               ttyfd_stat.st_gid));
        if (chmod(ttydev, ttyfd_stat.st_mode) != 0) {
            D_UTMP(("chmod(\"%s\", %03o) failed:  %s\n", ttydev, ttyfd_stat.st_mode, strerror(errno)));
        }
        if (chown(ttydev, ttyfd_stat.st_uid, ttyfd_stat.st_gid) != 0) {
            D_UTMP(("chown(\"%s\", %d, %d) failed:  %s\n", ttydev, ttyfd_stat.st_uid, ttyfd_stat.st_gid, strerror(errno)));
        }
    }
#endif /* __CYGWIN32__ */

#ifdef UTMP_SUPPORT
    remove_utmp_entry();
#endif
    privileges(REVERT);
#if DEBUG >= DEBUG_MEM
    if (DEBUG_LEVEL >= DEBUG_MEM) {
        MALLOC_DUMP();
        PIXMAP_DUMP();
        GC_DUMP();
    }
#endif
    PABLO_STOP_TRACING();
    DPRINTF1(("Cleanup done.  I am outta here!\n"));
}

/* Acquire a pseudo-teletype from the system. */
/*
 * On failure, returns -1.
 * On success, returns the file descriptor.
 *
 * If successful, ttydev and ptydev point to the names of the
 * master and slave parts
 */

#ifdef __sgi
inline int sgi_get_pty(void);

inline int
sgi_get_pty(void)
{
    int fd = -1;

    privileges(INVOKE);
    ptydev = ttydev = _getpty(&fd, O_RDWR | O_NDELAY, 0620, 0);
    privileges(REVERT);
    return (!ptydev ? -1 : fd);

}
#endif

#ifdef HAVE_DEV_PTC
inline int aix_get_pty(void);

inline int
aix_get_pty(void)
{

    int fd = -1;

    if ((fd = open("/dev/ptc", O_RDWR)) < 0)
        return (-1);
    else
        ptydev = ttydev = ttyname(fd);
    return (fd);
}
#endif

#ifdef HAVE_SCO_PTYS
inline int sco_get_pty(void);

inline int
sco_get_pty(void)
{

    static char pty_name[] = "/dev/ptyp??\0\0\0";
    static char tty_name[] = "/dev/ttyp??\0\0\0";
    int idx;
    int fd = -1;

    ptydev = pty_name;
    ttydev = tty_name;

    for (idx = 0; idx < 256; idx++) {

        sprintf(ptydev, "/dev/ptyp%d", idx);
        sprintf(ttydev, "/dev/ttyp%d", idx);

        if (access(ttydev, F_OK) < 0) {
            idx = 256;
            break;
        }
        if ((fd = open(ptydev, O_RDWR)) >= 0) {
            if (access(ttydev, R_OK | W_OK) == 0)
                return (fd);
            close(fd);
        }
    }
    return (-1);
}
#endif

#if defined(HAVE_POSIX_OPENPT) || defined(HAVE_DEV_PTMX)
inline int posix_get_pty(void);

inline int
posix_get_pty(void)
{

    int fd = -1;

    /* open the STREAMS, clone device /dev/ptmx (master pty) */
#if defined(HAVE_POSIX_OPENPT)
    if ((fd = posix_openpt(O_RDWR|O_NOCTTY)) < 0) {
#else
    if ((fd = open("/dev/ptmx", O_RDWR)) < 0) {
#endif
        return (-1);
    } else {
        if (grantpt(fd) != 0) {
            libast_print_error("grantpt(%d) failed:  %s\n", fd, strerror(errno));
            return (-1);
        } else if (unlockpt(fd) != 0) {
            libast_print_error("unlockpt(%d) failed:  %s\n", fd, strerror(errno));
            return (-1);
        } else {
            ptydev = ttydev = ptsname(fd);
            if (!ttydev) {
                libast_print_error("ptsname(%d) failed:  %s\n", fd, strerror(errno));
                return (-1);
            }
        }
    }
    return (fd);
}
#endif

#define PTYCHAR1 "pqrstuvwxyz"
#define PTYCHAR2 "0123456789abcdefghijklmnopqrstuvwxyz"

inline int gen_get_pty(void);

inline int
gen_get_pty(void)
{

    static char pty_name[] = "/dev/pty??";
    static char tty_name[] = "/dev/tty??";
    int len = sizeof(tty_name);
    char *c1, *c2;
    int fd = -1;

    ptydev = pty_name;
    ttydev = tty_name;

    for (c1 = PTYCHAR1; *c1; c1++) {
        ptydev[len - 3] = ttydev[len - 3] = *c1;
        for (c2 = PTYCHAR2; *c2; c2++) {
            ptydev[len - 2] = ttydev[len - 2] = *c2;
            if ((fd = open(ptydev, O_RDWR)) >= 0) {
                if (access(ttydev, R_OK | W_OK) == 0)
                    return (fd);
                close(fd);
            }
        }
    }
    return (-1);
}

int
get_pty(void)
{

    int fd = -1;

#if defined(HAVE_POSIX_OPENPT) || defined(HAVE_DEV_PTMX)
    fd = posix_get_pty();
#elif defined(__sgi)
    fd = sgi_get_pty();
#elif defined(HAVE_DEV_PTC)
    fd = aix_get_pty();
#elif defined(HAVE_SCO_PTYS)
    fd = sco_get_pty();
#endif

    /* Fall back on this method */
    if (fd == -1) {
        fd = gen_get_pty();
    }
    if (fd != -1) {
        fcntl(fd, F_SETFL, O_NDELAY);
        return (fd);
    } else {
        libast_print_error("Can't open pseudo-tty -- %s\n", strerror(errno));
        return (-1);
    }
}

/* establish a controlling teletype for new session */
/*
 * On some systems this can be done with ioctl() but on others we
 * need to re-open the slave tty.
 */
int
get_tty(void)
{
    int fd;
    pid_t pid;

    /*
     * setsid() [or setpgrp] must be before open of the terminal,
     * otherwise there is no controlling terminal (Solaris 2.4, HP-UX 9)
     */
#ifndef ultrix
# ifdef NO_SETSID
    pid = setpgrp(0, 0);
# else
    pid = setsid();
# endif
    if (pid < 0) {
        D_TTYMODE(("%s: setsid() failed: %s, PID == %d\n", rs_name, strerror(errno), pid));
    }
#endif /* ultrix */

    privileges(INVOKE);
    if (!ttydev) {
        libast_print_error("Slave tty device name is NULL.  Failed to open slave pty.\n");
        exit(EXIT_FAILURE);
    } else if ((fd = open(ttydev, O_RDWR)) < 0) {
        libast_print_error("Can't open slave tty %s -- %s\n", ttydev, strerror(errno));
        exit(EXIT_FAILURE);
    } else {
        D_TTY(("Opened slave tty %s\n", ttydev));
        privileges(REVERT);
    }

#if defined (__svr4__)
    /*
     * Push STREAMS modules:
     *  ptem: pseudo-terminal hardware emulation module.
     *  ldterm: standard terminal line discipline.
     *  ttcompat: V7, 4BSD and XENIX STREAMS compatibility module.
     */
    ioctl(fd, I_PUSH, "ptem");
    ioctl(fd, I_PUSH, "ldterm");
    ioctl(fd, I_PUSH, "ttcompat");
#else /* __svr4__ */
    {
        unsigned int mode = 0620;
        gid_t gid = my_rgid;

# ifdef PTY_GRP_NAME
        {
            struct group *gr = getgrnam(PTY_GRP_NAME);

            if (gr) {
                gid = gr->gr_gid;
                mode = 0620;
            }
        }
# endif

        privileges(INVOKE);
# ifndef __CYGWIN32__
        fchown(fd, my_ruid, gid);       /* fail silently */
        fchmod(fd, mode);
# endif
        privileges(REVERT);
    }
#endif /* __svr4__ */

    /*
     * Close all file descriptors.  If only stdin/out/err are closed,
     * child processes remain alive upon deletion of the window.
     */
    {
        unsigned short i;
        unsigned long max_fds;

        /* get number of available file descriptors */
#ifdef _POSIX_VERSION
        max_fds = sysconf(_SC_OPEN_MAX);
#else
        max_fds = getdtablesize();
#endif

        D_TTY(("Closing file descriptors 0-%d.\n", max_fds));
        for (i = 0; i < max_fds; i++) {
            if (i != fd)
                close(i);
        }
        D_TTY(("...closed.\n"));
    }

    /* Reopen stdin, stdout and stderr over the tty file descriptor */
    dup(fd);                    /* 0: stdin */
    dup(fd);                    /* 1: stdout */
    dup(fd);                    /* 2: stderr */

    if (fd > 2)
        close(fd);

    privileges(INVOKE);

#ifdef ultrix
    if ((fd = open("/dev/tty", O_RDONLY)) >= 0) {
        ioctl(fd, TIOCNOTTY, 0);
        close(fd);
    } else {
# ifdef NO_SETSID
        pid = setpgrp(0, 0);
# else
        pid = setsid();
# endif
        if (pid < 0) {
            D_TTYMODE(("%s: setsid() failed: %s, PID == %d\n", rs_name, strerror(errno), pid));
        }
    }

    /* no error, we could run with no tty to begin with */
#else /* ultrix */

# ifdef TIOCSCTTY
    ioctl(0, TIOCSCTTY, 0);
# endif

    /* set process group */
# if defined (_POSIX_VERSION) || defined (__svr4__)
    tcsetpgrp(0, pid);
# elif defined (TIOCSPGRP)
    ioctl(0, TIOCSPGRP, &pid);
# endif

    /* svr4 problems: reports no tty, no job control */
    /* # if !defined (__svr4__) && defined (TIOCSPGRP) */

    close(open(ttydev, O_RDWR, 0));
    /* # endif */
#endif /* ultrix */

    privileges(REVERT);

    D_TTY(("Returning fd == %d\n", fd));
    return (fd);
}

/* debug_ttymode() */
#if DEBUG >= DEBUG_TTYMODE && defined(HAVE_TERMIOS_H)

  /* cpp token stringize doesn't work on all machines <sigh> */
#  define SHOW_TTY_FLAG(flag,name) do { \
                                     if ((ttymode->c_iflag) & (flag)) { \
                                       fprintf(stderr, "+%s ", name); \
                                     } else { \
                                       fprintf(stderr, "-%s ", name); \
                                     } \
                                   } while (0)
#  define SHOW_CONT_CHAR(entry, name) fprintf(stderr, "%s=%#3o ", name, ttymode->c_cc[entry])
static void
debug_ttymode(ttymode_t *ttymode)
{

    /* c_iflag bits */
    fprintf(stderr, "Input flags:  ");
    SHOW_TTY_FLAG(IGNBRK, "IGNBRK");
    SHOW_TTY_FLAG(BRKINT, "BRKINT");
    SHOW_TTY_FLAG(IGNPAR, "IGNPAR");
    SHOW_TTY_FLAG(PARMRK, "PARMRK");
    SHOW_TTY_FLAG(INPCK, "INPCK");
    SHOW_TTY_FLAG(ISTRIP, "ISTRIP");
    SHOW_TTY_FLAG(INLCR, "INLCR");
    SHOW_TTY_FLAG(IGNCR, "IGNCR");
    SHOW_TTY_FLAG(ICRNL, "ICRNL");
    SHOW_TTY_FLAG(IXON, "IXON");
    SHOW_TTY_FLAG(IXOFF, "IXOFF");
#  ifdef IUCLC
    SHOW_TTY_FLAG(IUCLC, "IUCLC");
#  endif
#  ifdef IXANY
    SHOW_TTY_FLAG(IXANY, "IXANY");
#  endif
#  ifdef IMAXBEL
    SHOW_TTY_FLAG(IMAXBEL, "IMAXBEL");
#  endif
    fprintf(stderr, "\n");

    fprintf(stderr, "Control character mappings:  ");
    SHOW_CONT_CHAR(VINTR, "VINTR");
    SHOW_CONT_CHAR(VQUIT, "VQUIT");
    SHOW_CONT_CHAR(VERASE, "VERASE");
    SHOW_CONT_CHAR(VKILL, "VKILL");
    SHOW_CONT_CHAR(VEOF, "VEOF");
    SHOW_CONT_CHAR(VEOL, "VEOL");
#  ifdef VEOL2
    SHOW_CONT_CHAR(VEOL2, "VEOL2");
#  endif
#  ifdef VSWTC
    SHOW_CONT_CHAR(VSWTC, "VSWTC");
#  endif
#  ifdef VSWTCH
    SHOW_CONT_CHAR(VSWTCH, "VSWTCH");
#  endif
    SHOW_CONT_CHAR(VSTART, "VSTART");
    SHOW_CONT_CHAR(VSTOP, "VSTOP");
    SHOW_CONT_CHAR(VSUSP, "VSUSP");
#  ifdef VDSUSP
    SHOW_CONT_CHAR(VDSUSP, "VDSUSP");
#  endif
#  ifdef VREPRINT
    SHOW_CONT_CHAR(VREPRINT, "VREPRINT");
#  endif
#  ifdef VDISCRD
    SHOW_CONT_CHAR(VDISCRD, "VDISCRD");
#  endif
#  ifdef VWERSE
    SHOW_CONT_CHAR(VWERSE, "VWERSE");
#  endif
#  ifdef VLNEXT
    SHOW_CONT_CHAR(VLNEXT, "VLNEXT");
#  endif
    fprintf(stderr, "\n\n");
}

#  undef SHOW_TTY_FLAG
#  undef SHOW_CONT_CHAR
#endif /* DEBUG_TTYMODE */

/* get_ttymode() */
static void
get_ttymode(ttymode_t *tio)
{
#ifdef HAVE_TERMIOS_H
    /*
     * standard System V termios interface
     */
    if (GET_TERMIOS(0, tio) < 0) {
        /* return error - use system defaults */
        tio->c_cc[VINTR] = CINTR;
        tio->c_cc[VQUIT] = CQUIT;
        tio->c_cc[VERASE] = CERASE;
        tio->c_cc[VKILL] = CKILL;
        tio->c_cc[VSTART] = CSTART;
        tio->c_cc[VSTOP] = CSTOP;
        tio->c_cc[VSUSP] = CSUSP;
# ifdef VDSUSP
        tio->c_cc[VDSUSP] = CDSUSP;
# endif
# ifdef VREPRINT
        tio->c_cc[VREPRINT] = CRPRNT;
# endif
# ifdef VDISCRD
        tio->c_cc[VDISCRD] = CFLUSH;
# endif
# ifdef VWERSE
        tio->c_cc[VWERSE] = CWERASE;
# endif
# ifdef VLNEXT
        tio->c_cc[VLNEXT] = CLNEXT;
# endif
# ifdef VSTATUS
        tio->c_cc[VSTATUS] = CSTATUS;
# endif
    }
    tio->c_cc[VEOF] = CEOF;
    tio->c_cc[VEOL] = VDISABLE;
# ifdef VEOL2
    tio->c_cc[VEOL2] = VDISABLE;
# endif
# ifdef VSWTC
    tio->c_cc[VSWTC] = VDISABLE;
# endif
# ifdef VSWTCH
    tio->c_cc[VSWTCH] = VDISABLE;
# endif
# if VMIN != VEOF
    tio->c_cc[VMIN] = 1;
# endif
# if VTIME != VEOL
    tio->c_cc[VTIME] = 0;
# endif

    /* input modes */
    tio->c_iflag = (BRKINT | IGNPAR | ICRNL | IXON
# ifdef IMAXBEL
                    | IMAXBEL
# endif
        );

    /* output modes */
    tio->c_oflag = (OPOST | ONLCR);

    /* control modes */
    tio->c_cflag = (CS8 | CREAD);

    /* line discipline modes */
    tio->c_lflag = (ISIG | ICANON | IEXTEN | ECHO | ECHOE | ECHOK
# if defined (ECHOCTL) && defined (ECHOKE)
                    | ECHOCTL | ECHOKE
# endif
        );

#if defined(FORCE_BACKSPACE) && 0
    PrivMode(1, PrivMode_BackSpace);
    tio->c_cc[VERASE] = '\b';   /* force ^H for stty setting...   */
    SET_TERMIOS(0, tio);        /*  ...and make it stick -- casey */
#elif defined(FORCE_DELETE) && 0
    PrivMode(0, PrivMode_BackSpace);
    tio->c_cc[VERASE] = 0x7f;   /* force ^? for stty setting...   */
    SET_TERMIOS(0, tio);        /*  ...and make it stick -- casey */
#else
    PrivMode((tio->c_cc[VERASE] == '\b'), PrivMode_BackSpace);
#endif

#else /* HAVE_TERMIOS_H */

    /*
     * sgtty interface
     */

    /* get parameters -- gtty */
    if (ioctl(0, TIOCGETP, &(tio->sg)) < 0) {
        tio->sg.sg_erase = CERASE;      /* ^H */
        tio->sg.sg_kill = CKILL;        /* ^U */
    }
    tio->sg.sg_flags = (CRMOD | ECHO | EVENP | ODDP);

    /* get special characters */
    if (ioctl(0, TIOCGETC, &(tio->tc)) < 0) {
        tio->tc.t_intrc = CINTR;        /* ^C */
        tio->tc.t_quitc = CQUIT;        /* ^\ */
        tio->tc.t_startc = CSTART;      /* ^Q */
        tio->tc.t_stopc = CSTOP;        /* ^S */
        tio->tc.t_eofc = CEOF;  /* ^D */
        tio->tc.t_brkc = -1;
    }
    /* get local special chars */
    if (ioctl(0, TIOCGLTC, &(tio->lc)) < 0) {
        tio->lc.t_suspc = CSUSP;        /* ^Z */
        tio->lc.t_dsuspc = CDSUSP;      /* ^Y */
        tio->lc.t_rprntc = CRPRNT;      /* ^R */
        tio->lc.t_flushc = CFLUSH;      /* ^O */
        tio->lc.t_werasc = CWERASE;     /* ^W */
        tio->lc.t_lnextc = CLNEXT;      /* ^V */
    }
    /* get line discipline */
    ioctl(0, TIOCGETD, &(tio->line));
# ifdef NTTYDISC
    tio->line = NTTYDISC;
# endif /* NTTYDISC */
    tio->local = (LCRTBS | LCRTERA | LCTLECH | LPASS8 | LCRTKIL);

# ifdef FORCE_BACKSPACE
    PrivMode(1, PrivMode_BackSpace);
    tio->sg.sg_erase = '\b';
    SET_TTYMODE(0, tio);
# elif defined (FORCE_DELETE)
    PrivMode(0, PrivMode_BackSpace);
    tio->sg.sg_erase = 0x7f;
    SET_TTYMODE(0, tio);
# else
    PrivMode((tio->sg.sg_erase == '\b'), PrivMode_BackSpace);
# endif

#endif /* HAVE_TERMIOS_H */
}

/* Xlocale */
XFontSet
create_fontset(const char *font1, const char *font2)
{
    XFontSet fontset = 0;
    char *fontname, **ml, *ds;
    int mc;
    /*const char fs_base[] = ",-misc-fixed-*-r-*-*-*-120-*-*-*-*-*-*,*";*/

    ASSERT_RVAL(font1 != NULL, (XFontSet) 0);

    if (font2) {
        fontname = MALLOC(strlen(font1) + strlen(font2) /*+ sizeof(fs_base)*/ + 2);
        if (fontname) {
            strcpy(fontname, font1);
            strcat(fontname, ",");
            strcat(fontname, font2);
            /*strcat(fontname, fs_base);*/
        }
    } else {
        fontname = MALLOC(strlen(font1) /*+ sizeof(fs_base)*/ + 1);
        if (fontname) {
            strcpy(fontname, font1);
            /*strcat(fontname, fs_base);*/
        }
    }
    if (fontname) {
        setlocale(LC_ALL, "");
        fontset = XCreateFontSet(Xdisplay, fontname, &ml, &mc, NULL);
        D_FONT(("Created fontset from %s, %d missing charsets (\"%s\").\n", fontname, mc,
                ((mc > 0) ? (ml[0]) : ("N/A"))));
        FREE(fontname);
        if (mc) {
            XFreeStringList(ml);
            /*fontset = 0;*/
        }
    }
    return fontset;
}

#if defined(USE_XIM) || defined(MULTI_CHARSET)

#ifdef USE_XIM
static int xim_real_init(void);

# ifdef USE_X11R6_XIM
static void xim_destroy_cb(XIM xim, XPointer client_data, XPointer call_data);
static void xim_instantiate_cb(Display * display, XPointer client_data, XPointer call_data);
# endif
#endif
void
init_locale(void)
{
    char *locale = NULL;

    locale = setlocale(LC_ALL, "");
    XSetLocaleModifiers("");
    TermWin.fontset = (XFontSet) 0;
    if ((!locale) || (!XSupportsLocale())) {
        libast_print_warning("Locale not supported; defaulting to portable \"C\" locale.\n");
        locale = setlocale(LC_ALL, "C");
        XSetLocaleModifiers("");
        REQUIRE(locale);
        REQUIRE(XSupportsLocale());
    } else {
#ifdef USE_XIM
#ifdef MULTI_CHARSET
        TermWin.fontset = create_fontset(etfonts[def_font_idx], etmfonts[def_font_idx]);
#else
        TermWin.fontset = create_fontset(etfonts[def_font_idx], "-misc-fixed-medium-r-semicondensed--13-*-75-*-c-*-iso10646-1");
#endif
        if ((TermWin.fontset == (XFontSet) 0) || (xim_real_init() != -1)) {
            return;
        }
# ifdef USE_X11R6_XIM
        XRegisterIMInstantiateCallback(Xdisplay, NULL, NULL, NULL, xim_instantiate_cb, NULL);
# endif
#endif
    }
}
#endif /* USE_XIM || MULTI_CHARSET */

#ifdef USE_XIM
static void
xim_set_size(XRectangle * size)
{
    size->x = TermWin.internalBorder;
    size->y = TermWin.internalBorder + bbar_calc_docked_height(BBAR_DOCKED_TOP);
    size->width = Width2Pixel(TERM_WINDOW_GET_COLS());
    size->height = Height2Pixel(TERM_WINDOW_GET_ROWS());
}

static void
xim_set_color(unsigned long *fg, unsigned long *bg)
{
    *fg = PixColors[fgColor];
    *bg = PixColors[bgColor];
}

static void
xim_send_spot(void)
{
    XPoint spot;
    static XPoint oldSpot = { -1, -1 };
    XVaNestedList preedit_attr;

    if (!xim_input_context) {
        return;
    }

    if (xim_input_style & XIMPreeditPosition) {
        xim_get_position(&spot);
        if (spot.x != oldSpot.x || spot.y != oldSpot.y) {
            oldSpot.x = spot.x;
            oldSpot.y = spot.y;
            preedit_attr = XVaCreateNestedList(0, XNSpotLocation, &spot, NULL);
            XSetICValues(xim_input_context, XNPreeditAttributes, preedit_attr, NULL);
            XFree(preedit_attr);
        }
    }
}

static void
xim_get_area(XRectangle * preedit_rect, XRectangle * status_rect, XRectangle * needed_rect)
{
    preedit_rect->x = needed_rect->width + (scrollbar_is_visible()
                                            && !(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT))
                                            ? (scrollbar_trough_width()) : (0));
    preedit_rect->y = Height2Pixel(TERM_WINDOW_GET_ROWS() - 1);

    preedit_rect->width =
        Width2Pixel(TERM_WINDOW_GET_COLS() + 1) - needed_rect->width +
        (!(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT)) ? (scrollbar_trough_width()) : 0);
    preedit_rect->height = Height2Pixel(1);

    status_rect->x = (scrollbar_is_visible()
                      && !(BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_SCROLLBAR_RIGHT))) ? (scrollbar_trough_width()) : 0;
    status_rect->y = Height2Pixel(TERM_WINDOW_GET_ROWS() - 1);

    status_rect->width = needed_rect->width ? needed_rect->width : Width2Pixel(TERM_WINDOW_GET_COLS() + 1);
    status_rect->height = Height2Pixel(1);
}

#ifdef USE_X11R6_XIM
static void
xim_destroy_cb(XIM xim, XPointer client_data, XPointer call_data)
{
    xim_input_context = NULL;
    xim_input_method = NULL;
    XRegisterIMInstantiateCallback(Xdisplay, NULL, NULL, NULL, xim_instantiate_cb, NULL);
    xim = NULL;
    client_data = call_data = (XPointer) 0;
}

static void
xim_instantiate_cb(Display * display, XPointer client_data, XPointer call_data)
{
    xim_real_init();
    if (xim_input_context) {
        XUnregisterIMInstantiateCallback(Xdisplay, NULL, NULL, NULL, xim_instantiate_cb, NULL);
    }
    display = NULL;
    client_data = call_data = (XPointer) 0;
}
#endif

static int
xim_real_init(void)
{
    char *p, *s, buf[64], tmp[1024];
    char *end, *next_s;
    XIMStyles *xim_styles = NULL;
    int found;
    XPoint spot;
    XRectangle rect, status_rect, needed_rect;
    unsigned long fg, bg;
    XVaNestedList preedit_attr = NULL;
    XVaNestedList status_attr = NULL;

    REQUIRE_RVAL(xim_input_context == NULL, -1);

    xim_input_style = 0;

    if (rs_input_method && *rs_input_method) {
        strncpy(tmp, rs_input_method, sizeof(tmp) - 1);
        for (s = tmp; *s; s = next_s + 1) {
            for (; *s && isspace(*s); s++);
            if (!*s) {
                break;
            }
            for (end = s; (*end && (*end != ',')); end++);
            for (next_s = end--; ((end >= s) && isspace(*end)); end--);
            *(end + 1) = '\0';
            if (*s) {
                snprintf(buf, sizeof(buf), "@im=%s", s);
                if (((p = XSetLocaleModifiers(buf))) && (*p)
                    && ((xim_input_method = XOpenIM(Xdisplay, NULL, NULL, NULL)))) {
                    break;
                }
            }
            if (!*next_s) {
                break;
            }
        }
    }

    /* try with XMODIFIERS env. var. */
    if (!xim_input_method && getenv("XMODIFIERS") && (p = XSetLocaleModifiers("")) && *p) {
        xim_input_method = XOpenIM(Xdisplay, NULL, NULL, NULL);
    }

    /* try with no modifiers base */
    if (!xim_input_method && (p = XSetLocaleModifiers("@im=none")) && *p) {
        xim_input_method = XOpenIM(Xdisplay, NULL, NULL, NULL);
    }

    if (!xim_input_method) {
        xim_input_method = XOpenIM(Xdisplay, NULL, NULL, NULL);
    }

    if (!xim_input_method) {
        return -1;
    }
#ifdef USE_X11R6_XIM
    {
        XIMCallback destroy_cb;

        destroy_cb.callback = xim_destroy_cb;
        destroy_cb.client_data = NULL;
        if (XSetIMValues(xim_input_method, XNDestroyCallback, &destroy_cb, NULL)) {
            libast_print_error("Could not set destroy callback to IM\n");
        }
    }
#endif

    if ((XGetIMValues(xim_input_method, XNQueryInputStyle, &xim_styles, NULL)) || (!xim_styles)) {
        libast_print_error("input method doesn't support any style\n");
        XCloseIM(xim_input_method);
        xim_input_method = NULL;
        return -1;
    }
    strncpy(tmp, (rs_preedit_type ? rs_preedit_type : "OverTheSpot,OffTheSpot,Root"), sizeof(tmp) - 1);
    for (found = 0, s = tmp; *s && !found; s = next_s + 1) {
        unsigned short i;

        for (; *s && isspace(*s); s++);
        if (!*s) {
            break;
        }
        for (end = s; (*end && (*end != ',')); end++);
        next_s = ((*end) ? (end) : (end + 1));
        for (end--; ((end >= s) && isspace(*end)); end--);
        *(end + 1) = '\0';

        if (!strcmp(s, "OverTheSpot")) {
            xim_input_style = (XIMPreeditPosition | XIMStatusNothing);
        } else if (!strcmp(s, "OffTheSpot")) {
            xim_input_style = (XIMPreeditArea | XIMStatusArea);
        } else if (!strcmp(s, "Root")) {
            xim_input_style = (XIMPreeditNothing | XIMStatusNothing);
        }

        for (i = 0; i < xim_styles->count_styles; i++) {
            if (xim_input_style == xim_styles->supported_styles[i]) {
                found = 1;
                break;
            }
        }
    }
    XFree(xim_styles);

    if (found == 0) {
        libast_print_error("input method doesn't support my preedit type\n");
        XCloseIM(xim_input_method);
        xim_input_method = NULL;
        return -1;
    }
    if ((xim_input_style != (XIMPreeditNothing | XIMStatusNothing))
        && (xim_input_style != (XIMPreeditArea | XIMStatusArea))
        && (xim_input_style != (XIMPreeditPosition | XIMStatusNothing))) {
        libast_print_error("This program does not support the preedit type\n");
        XCloseIM(xim_input_method);
        xim_input_method = NULL;
        return -1;
    }
    if (xim_input_style & XIMPreeditPosition) {
        xim_set_size(&rect);
        xim_get_position(&spot);
        xim_set_color(&fg, &bg);
        preedit_attr =
            XVaCreateNestedList(0, XNArea, &rect, XNSpotLocation, &spot, XNForeground, fg, XNBackground, bg, XNFontSet,
                                TermWin.fontset, NULL);
    } else if (xim_input_style & XIMPreeditArea) {
        xim_set_color(&fg, &bg);
        /* The necessary width of preedit area is unknown until create input context. */
        needed_rect.width = 0;
        xim_get_area(&rect, &status_rect, &needed_rect);
        preedit_attr = XVaCreateNestedList(0, XNArea, &rect, XNForeground, fg, XNBackground, bg, XNFontSet, TermWin.fontset, NULL);
        status_attr =
            XVaCreateNestedList(0, XNArea, &status_rect, XNForeground, fg, XNBackground, bg, XNFontSet, TermWin.fontset, NULL);
    }
    xim_input_context =
        XCreateIC(xim_input_method, XNInputStyle, xim_input_style, XNClientWindow, TermWin.parent, XNFocusWindow, TermWin.parent,
                  preedit_attr ? XNPreeditAttributes : NULL, preedit_attr, status_attr ? XNStatusAttributes : NULL, status_attr,
                  NULL);
    if (preedit_attr) {
        XFree(preedit_attr);
    }
    if (status_attr) {
        XFree(status_attr);
    }
    if (!xim_input_context) {
        libast_print_error("Failed to create input context\n");
        XCloseIM(xim_input_method);
        xim_input_method = NULL;
        return -1;
    }
    if (xim_input_style & XIMPreeditArea)
        xim_set_status_position();
    return 0;
}

void
xim_set_status_position(void)
{
    XRectangle preedit_rect, status_rect, *needed_rect, rect;
    XVaNestedList preedit_attr, status_attr;
    XPoint spot;

    REQUIRE(xim_input_context != NULL);

    if (xim_input_style & XIMPreeditPosition) {
        xim_set_size(&rect);
        xim_get_position(&spot);

        preedit_attr = XVaCreateNestedList(0, XNArea, &rect, XNSpotLocation, &spot, NULL);
        XSetICValues(xim_input_context, XNPreeditAttributes, preedit_attr, NULL);
        XFree(preedit_attr);
    } else if (xim_input_style & XIMPreeditArea) {
        /* Getting the necessary width of preedit area */
        status_attr = XVaCreateNestedList(0, XNAreaNeeded, &needed_rect, NULL);
        XGetICValues(xim_input_context, XNStatusAttributes, status_attr, NULL);
        XFree(status_attr);

        xim_get_area(&preedit_rect, &status_rect, needed_rect);

        preedit_attr = XVaCreateNestedList(0, XNArea, &preedit_rect, NULL);
        status_attr = XVaCreateNestedList(0, XNArea, &status_rect, NULL);
        XSetICValues(xim_input_context, XNPreeditAttributes, preedit_attr, XNStatusAttributes, status_attr, NULL);
        XFree(preedit_attr);
        XFree(status_attr);
    }
}

void
xim_set_fontset(void)
{
    XVaNestedList preedit_attr = NULL;
    XVaNestedList status_attr = NULL;

    REQUIRE(xim_input_context != NULL);

    if (xim_input_style & XIMStatusArea) {
        status_attr = XVaCreateNestedList(0, XNFontSet, TermWin.fontset, NULL);
    }
    if (xim_input_style & (XIMPreeditArea | XIMPreeditPosition)) {
        preedit_attr = XVaCreateNestedList(0, XNFontSet, TermWin.fontset, NULL);
    }

    if (status_attr && preedit_attr) {
        XSetICValues(xim_input_context, XNPreeditAttributes, preedit_attr, XNStatusAttributes, status_attr, NULL);
    } else if (preedit_attr) {
        XSetICValues(xim_input_context, XNPreeditAttributes, preedit_attr, NULL);
    } else if (status_attr) {
        XSetICValues(xim_input_context, XNStatusAttributes, status_attr, NULL);
    }

    if (preedit_attr) {
        XFree(preedit_attr);
    }
    if (status_attr) {
        XFree(status_attr);
    }
}
#endif /* USE_XIM */

/* run_command() */
/*
 * Run the command in a subprocess and return a file descriptor for the
 * master end of the pseudo-teletype pair with the command talking to
 * the slave.
 */
int
run_command(char **argv)
{

    ttymode_t tio;
    int ptyfd;

    /* Save and then give up any super-user privileges */
    privileges(IGNORE);

    ptyfd = get_pty();
    if (ptyfd < 0)
        return (-1);
    AT_LEAST(num_fds, ((unsigned int) (ptyfd + 1)));

    /* store original tty status for restoration clean_exit() -- rgg 04/12/95 */
    lstat(ttydev, &ttyfd_stat);
    D_CMD(("Original settings of %s are mode %o, uid %d, gid %d\n", ttydev, ttyfd_stat.st_mode, ttyfd_stat.st_uid,
           ttyfd_stat.st_gid));

    /* install exit handler for cleanup */
#ifdef HAVE_ATEXIT
    atexit(clean_exit);
#else
# if defined (__sun__)
    on_exit(clean_exit, NULL);  /* non-ANSI exit handler */
# else
    libast_print_error("no atexit(), UTMP entries can't be cleaned\n");
# endif
#endif

    /*
     * get tty settings before fork()
     * and make a reasonable guess at the value for BackSpace
     */
    get_ttymode(&tio);
    /* add Backspace value */
    SavedModes |= (PrivateModes & PrivMode_BackSpace);

    /* add value for scrollbar */
    if (scrollbar_is_visible()) {
        PrivateModes |= PrivMode_scrollbar;
        SavedModes |= PrivMode_scrollbar;
    }
    tt_winsize(ptyfd);
#if DEBUG >= DEBUG_TTYMODE && defined(HAVE_TERMIOS_H)
    if (DEBUG_LEVEL >= DEBUG_TTYMODE) {
        debug_ttymode(&tio);
    }
#endif

    D_CMD(("Forking\n"));
    cmd_pid = fork();
    D_CMD(("After fork(), cmd_pid == %d\n", cmd_pid));
    if (cmd_pid < 0) {
        libast_print_error("fork(): %s\n", strerror(errno));
        return (-1);
    }
    if (cmd_pid == 0) {

        /* Child process.  Reset the signal handlers right away. */
        signal(SIGINT, SIG_DFL);
        signal(SIGQUIT, SIG_DFL);
        signal(SIGCHLD, SIG_DFL);
        signal(SIGSEGV, SIG_DFL);
        signal(SIGBUS, SIG_DFL);
        signal(SIGABRT, SIG_DFL);
        signal(SIGFPE, SIG_DFL);
        signal(SIGILL, SIG_DFL);
        signal(SIGSYS, SIG_DFL);
        signal(SIGALRM, SIG_DFL);
        signal(SIGPIPE, SIG_DFL);
#ifdef SIGTSTP
        signal(SIGTSTP, SIG_IGN);
        signal(SIGTTIN, SIG_IGN);
        signal(SIGTTOU, SIG_IGN);
#endif

#ifdef HAVE_UNSETENV
        unsetenv("LINES");
        unsetenv("COLUMNS");
        unsetenv("TERMCAP");
#endif

        DEBUG_LEVEL = 0;

        get_tty();
        SET_TTYMODE(0, &tio);
#if 0
        if (TermWin.screen_mode != NS_MODE_NONE) {
            tt_winsize(0);
        }
#endif

        /* become virtual console, fail silently */
        if (BITFIELD_IS_SET(vt_options, VT_OPTIONS_CONSOLE)) {
            int fd = 1;

            privileges(INVOKE);
#ifdef SRIOCSREDIR
            fd = open(CONSOLE, O_WRONLY);
            if (fd < 0 || ioctl(fd, SRIOCSREDIR, 0) < 0) {
                if (fd >= 0)
                    close(fd);
            }
#elif defined(TIOCCONS)
            ioctl(0, TIOCCONS, &fd);
#endif /* SRIOCSREDIR */
            privileges(REVERT);
        }

        /* Permanently revoke all privileges for the child process.  
           Root shells for everyone are tres uncool.... ;^) -- mej */
#ifdef _HPUX_SOURCE
        setresuid(my_ruid, my_ruid, my_euid);
        setresgid(my_rgid, my_rgid, my_egid);
#else
        /* No special treatment is needed for systems with saved uids/gids,
           because the exec*() calls reset the saved uid/gid to the
           effective uid/gid                               -- mej */
# ifndef __CYGWIN32__
        setregid(my_rgid, my_rgid);
        setreuid(my_ruid, my_ruid);
# endif /* __CYGWIN32__ */
#endif /* _HPUX_SOURCE */

        D_UTMP(("Child process reset\n"));
        my_euid = my_ruid;
        my_egid = my_rgid;

#ifdef HAVE_USLEEP
        usleep(10);             /* Attempt to force a context switch so that the parent runs before us. */
#else
        sleep(1);               /* ugliness */
#endif
        D_CMD(("[%d] About to spawn shell\n", getpid()));
        if (chdir(initial_dir)) {
            libast_print_warning("Unable to chdir to \"%s\" -- %s\n", initial_dir, strerror(errno));
        }
        if (argv) {
#if DEBUG >= DEBUG_CMD
            if (DEBUG_LEVEL >= DEBUG_CMD) {
                int i;

                for (i = 0; argv[i]; i++) {
                    DPRINTF(("argv[%d] = \"%s\"\n", i, argv[i]));
                }
            }
#endif
            D_CMD(("[%d] execvp(\"%s\", %8p) is next.  I'm outta here!\n", getpid(), NONULL(argv[0]), argv));
            execvp(argv[0], argv);
            libast_print_error("execvp() failed, cannot execute \"%s\": %s\n", argv[0], strerror(errno));
        } else {

            const char *argv0, *shell;

            if (!(shell = getenv("SHELL")) || *shell == '\0')
                shell = "/bin/sh";

            argv0 = my_basename(shell);
            if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_LOGIN_SHELL)) {
                char *p = MALLOC(strlen(argv0) + 2);

                p[0] = '-';
                strcpy(&p[1], argv0);
                argv0 = p;
            }
            execlp(shell, argv0, NULL);
            libast_print_error("execlp() failed, cannot execute \"%s\": %s\n", shell, strerror(errno));
        }
        sleep(3);               /* Sleep to make sure fork() returns in the parent, and so user can read error message */
        exit(EXIT_FAILURE);
    }
#ifdef UTMP_SUPPORT
    privileges(RESTORE);
    if (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_WRITE_UTMP)) {
        add_utmp_entry(ttydev, display_name, ptyfd);
    }
    privileges(IGNORE);
#endif

    D_CMD(("Returning ptyfd == %d\n", ptyfd));
    return (ptyfd);
}


#ifdef ESCREEN

/***************************************************************************/
/* Escreen:  callbacks */
/***********************/

static int
set_scroll_x(void *xd, int x)
{
    USE_VAR(xd);
    D_ESCREEN(("%d\n", x));
    return NS_FAIL;
}

static int
set_scroll_y(void *xd, int y)
{
    USE_VAR(xd);
    D_ESCREEN(("%d\n", y));
    return NS_FAIL;
}

static int
set_scroll_w(void *xd, int w)
{
    USE_VAR(xd);
    D_ESCREEN(("%d\n", w));
    return NS_FAIL;
}

static int
set_scroll_h(void *xd, int h)
{
    USE_VAR(xd);
    D_ESCREEN(("%d\n", h));
    return NS_FAIL;
}

static int
redraw(void *xd)
{
    USE_VAR(xd);
    D_ESCREEN(("redraw\n"));
    return NS_FAIL;
}

static int
redraw_xywh(void *xd, int x, int y, int w, int h)
{
    USE_VAR(xd);
    D_ESCREEN(("%d,%d %dx%d\n", x, y, w, h));
    return NS_FAIL;
}

static button_t *screen_button_create(char *text, char code)
{
    button_t *b;
    char p[3];

    REQUIRE_RVAL(text, NULL);
    REQUIRE_RVAL(*text, NULL);
    b = button_create(text);
    REQUIRE_RVAL(b, NULL);

    p[0] = NS_SCREEN_ESCAPE;
    p[1] = code;
    p[2] = '\0';
    D_ESCREEN(("Creating button \"%s\" for display %c (%s)\n", NONULL(text), code, safe_print_string(p, 2)));
    button_set_action(b, ACTION_ECHO, p);
    b->flags |= NS_SCREAM_BUTTON;

    return b;
}

/* add a new screen display to button bar.
   if our user's configured a bbar, we'll add to that,
   otherwise, we'll create one.
   xd     address of the pointer to the buttonbar in question
   after  insert after which display?
   name   the display's name */
static int
ins_disp(void *xd, int after, int as, char *name)
{
    buttonbar_t *bbar = (buttonbar_t *) xd;
    button_t *button;

    USE_VAR(after);
    REQUIRE_RVAL(xd, NS_FAIL);
    REQUIRE_RVAL(name, NS_FAIL);
    REQUIRE_RVAL(*name, NS_FAIL);

    if (!(button = screen_button_create(name, '0' + as))) {
        return NS_FAIL;
    }

    bbar_add_button(bbar, button);
    return NS_SUCC;
}

#if 0

/* add supa-dupa right buttons for screen-features.
   if our user's configured a bbar, we'll add to that,
   otherwise, we'll create one. */
static int
add_screen_ctl_button(buttonbar_t *bbar, char *name, char key)
{
    button_t *button;

    REQUIRE_RVAL(bbar, NS_FAIL);
    REQUIRE_RVAL(name, NS_FAIL);
    REQUIRE_RVAL(*name, NS_FAIL);

    if (!(button = screen_button_create(name, key)))
        return NS_FAIL;

    bbar_add_button(bbar, button);
    return NS_SUCC;
}
#endif

/* delete n'th button
   xd     address of the pointer to the buttonbar in question
   n      index of the button (not screen, not data -- the button) */
static int
del_disp(void *xd, int n)
{
    buttonbar_t *bbar = (buttonbar_t *) xd;
    button_t *button, *b2;
    int bi = n;

    REQUIRE_RVAL(bbar, NS_FAIL);
    REQUIRE_RVAL(bbar->buttons, NS_FAIL);

    b2 = button = bbar->buttons;
    if (n == 0) {
        bbar->buttons = bbar->buttons->next;
        if (bbar->current == button) {
            bbar->current = bbar->buttons;
        }
    } else {
        for (; n > 0; n--) {
            b2 = button;
            if (!(button = button->next)) {
                D_ESCREEN(("cannot delete button %d: does not exist...\n", bi));
                return NS_FAIL;
            }
        }
        b2->next = button->next;
        if (bbar->current == button) {
            bbar->current = b2;
        }
    }

    button->next = NULL;
    button_free(button);

    bbar_redraw(bbar);

    return NS_SUCC;
}

/* update the button-representation of a screen-display.
   xd     address of the pointer to the buttonbar in question
   n      the button's index (in the list of buttons)
   flags  the new flags for the display (or -1 to ignore)
   name   the new name for the display (or NULL)
   <-     error code */
static int
upd_disp(void *xd, int n, int flags, char *name)
{
    buttonbar_t *bbar = (buttonbar_t *) xd;
    button_t *button;

    REQUIRE_RVAL(bbar, NS_FAIL);
    REQUIRE_RVAL(bbar->buttons, NS_FAIL);

    button = bbar->buttons;
    for (; (n > 0) && (button->next); n--, button = button->next);

    /* See if we actually need to update anything.  If not, don't. */
    if ((name && button->text && !strcmp(name, button->text))
        && ((flags | NS_SCREAM_BUTTON) == button->flags)) {
        return NS_SUCC;
    }

    /* Update display name */
    if (name && (!button->text || strcmp(name, button->text))) {
        button_set_text(button, name);
    }

    /* Update flags */
    if (flags >= 0) {
        button->flags = (flags | NS_SCREAM_BUTTON);
        D_ESCREEN(("upd_disp: new flags for \"%s\": %d\n", button->text, flags));
    }

    /* Redraw buttonbar to reflect new information */
    bbar_redraw(bbar);

    return NS_SUCC;
}

/* expire all buttons
   xd     address of the pointer to the buttonbar in question
   n      how many buttons do we want to throw out (normally all of them)?
   <-     error code */
static int
expire_buttons(void *xd, int n)
{
    buttonbar_t *bbar = (buttonbar_t *) xd;
    button_t *b, *p;

    REQUIRE_RVAL(bbar, NS_FAIL);
    if (n < 1) {
        return NS_FAIL;
    }

    if ((b = bbar->buttons)) {
        for (; n; n--) {
            p = b;
            b = b->next;
        }
        p->next = NULL;
        button_free(bbar->buttons);
        bbar->buttons = b;
    }

    return NS_SUCC;
}

/* display a status line the screen program sent us */
static int
err_msg(void *xd, int err, char *msg)
{
#if DEBUG >= DEBUG_ESCREEN
    if (DEBUG_LEVEL >= DEBUG_ESCREEN) {
        char *sc[] = { "Copy mode", "Bell in" };
        int n, nsc = sizeof(sc) / sizeof(char *);

        /* there are certain things that would make sense if we were displaying
           a status-line; they do not, however, warrant an alert-box, so we drop
           them here. */
        if (strlen(msg)) {
            for (n = 0; n < nsc; n++) {
                if (!strncmp(msg, sc[n], strlen(sc[n]))) {
                    break;
                }
            }
            if (n >= nsc) {
                menu_dialog(NULL, msg, 0, NULL, NULL);
            }
        }
    } else
#endif

    if (!BEG_STRCASECMP(msg, "Wuff")) {
        /* screen beeped, so go ahead and use our normal beep. */
        scr_bell();
    }

    USE_VAR(xd);
    USE_VAR(err);
    return NS_SUCC;
}

/* send text to the application (normally "screen") in the terminal */
static int
inp_text(void *xd, int id, char *txt)
{
    int which = (int) xd;
    USE_VAR(id);

    if (which) {
        D_ESCREEN(("Writing \"%s\" to screen.\n", safe_print_string(txt, strlen(txt))));
        cmd_write(txt, strlen(txt));
    } else {
        D_ESCREEN(("Writing \"%s\" to subprocess.\n", safe_print_string(txt, strlen(txt))));
        tt_write(txt, strlen(txt));
    }
    return NS_SUCC;
}

/* open a dialog */
static int
input_dialog(void *xd, char *prompt, int maxlen, char **retstr, int (*inp_tab) (void *, char *, size_t, size_t))
{
    switch (menu_dialog(xd, prompt, maxlen, retstr, inp_tab)) {
        case 0:
            return NS_SUCC;
        case -2:
            return NS_USER_CXL;
        default:
            return NS_FAIL;
    }
}

/* run a program (normally "screen") inside the terminal */
static int
exe_prg(void *xd, char **argv)
{
    USE_VAR(xd);
    return run_command(argv);
}


/****** Azundris' playthings :-) ******/

#define DIRECT_MASK (~(RS_Cursor|RS_Select|RS_fontMask))
#define COLOUR_MASK (RS_fgMask|RS_bgMask)
#define DIRECT_SET_SCREEN(x,y,fg,bg) (screen.text[ys+y])[x]=fg; (screen.rend[ys+y])[x]=bg&DIRECT_MASK;
#define CLEAR (RS_None | bgColor)

static void
direct_write_screen(int x, int y, char *fg, rend_t bg)
{
    int ys = TermWin.saveLines - TermWin.view_start;
    text_t *t = screen.text[ys + y];
    rend_t *r = screen.rend[ys + y];

    REQUIRE(fg);

    while (*fg && (x >= 0) && (x < TERM_WINDOW_GET_REPORTED_COLS())) {
        t[x] = *(fg++);
        r[x++] = bg & DIRECT_MASK;
    }
}

#ifdef ESCREEN_FX
static void
bosconian(int n)
{
    int x, y;
    int ys = TermWin.saveLines - TermWin.view_start;

    for (; n != 0; n--) {
        for (y = 0; y < TERM_WINDOW_GET_ROWS(); y++) {
            text_t *t = screen.text[ys + y];
            rend_t *r = screen.rend[ys + y];

            for (x = 0; x < TERM_WINDOW_GET_COLS(); x++) {
                t[x] = random() & 0xff;
                r[x] = random() & COLOUR_MASK;
            }
        }
        scr_refresh(FAST_REFRESH);
    }
}

static void
unbosconian(void)
{
    int x, y;
    int ys = TermWin.saveLines - TermWin.view_start;
    rend_t bg;

    do {
        bg = CLEAR;
        for (y = 0; (bg == CLEAR) && y < TERM_WINDOW_GET_ROWS(); y++) {
            rend_t *r = screen.rend[ys + y];

            for (x = 0; (bg == CLEAR) && x < TERM_WINDOW_GET_COLS(); x++) {
                if (r[x] != CLEAR) {
                    bg = r[x];
                }
            }
        }
        if (bg != CLEAR) {
            for (y = 0; y < TERM_WINDOW_GET_ROWS(); y++) {
                text_t *t = screen.text[ys + y];
                rend_t *r = screen.rend[ys + y];

                for (x = 0; x < TERM_WINDOW_GET_COLS(); x++) {
                    if (r[x] == bg) {
                        r[x] = CLEAR;
                        t[x] = ' ';
                    }
                }
            }
            scr_refresh(FAST_REFRESH);
        }
    } while (bg != CLEAR);
}

#undef DIRECT_MASK
#undef COLOUR_MASK
#undef DIRECT_SET_SCREEN

static void
matrix(int n)
{
    int x, y, w, f;
    int ys = TermWin.saveLines - TermWin.view_start;
    text_t *s = MALLOC(TERM_WINDOW_GET_COLS());
    text_t *t, *t2;
    rend_t *r, *r2;

    if (!s) {
        puts("fail");
        return;
    }

    MEMSET(s, 0, TERM_WINDOW_GET_COLS());
#define MATRIX_HI CLEAR
#define MATRIX_LO ((4<<8)|CLEAR)

    while (n--) {
        for (x = 0; x < TERM_WINDOW_GET_COLS(); x++) {
            if (!(random() & 3)) {
                if ((y = s[x])) {
                    w = random() & 15;
                } else {
                    w = 0;
                }
                t = screen.text[ys + y];
                r = screen.rend[ys + y];

                switch (w) {
                    case 0:    /* restart */
                        if (s[x]) {
                            r[x] = MATRIX_LO;
                            s[x] = 0;
                            t = screen.text[ys];
                            r = screen.rend[ys];
                        }
                        r[x] = MATRIX_HI;
                        t[x] = random() & 0xff;
                        s[x]++;
                        /* fall-through */

                    case 1:    /* continue */
                    case 2:
                    case 3:
                        for (f = random() & 7; f != 0; f--) {
                            if (y < TERM_WINDOW_GET_ROWS() - 1) {
                                t2 = screen.text[ys + y + 1];
                                r2 = screen.rend[ys + y + 1];
                                t2[x] = t[x];
                                r2[x] = r[x];
                                s[x]++;
                                y++;
                            } else {
                                s[x] = 0;
                                f = 0;
                            }
                            r[x] = MATRIX_LO;
                            t[x] = random() & 0xff;
                            if (f) {
                                scr_refresh(FAST_REFRESH);
                                t = screen.text[ys + y];
                                r = screen.rend[ys + y];
                            }
                        }
                        break;

                    default:
                        t[x] = random() & 0xff; /* hold */
                }
            }
        }
        scr_refresh(FAST_REFRESH);
    }
    FREE(s);
}

#undef MATRIX_HI
#undef MATRIX_LO

/* do whatever for ms milli-seconds */
static int
waitstate(void *xd, int ms)
{
    int y = 1;
    time_t dur = (time_t) (ms / 1000);

    USE_VAR(xd);

    if (!(random() & 7)) {
        if (!(random() & 3)) {
            matrix(31);
            unbosconian();
        }
        bosconian(20);
        unbosconian();
        scr_refresh(SLOW_REFRESH);
    }

    direct_write_screen(0, y++, "    **** COMMODORE 64 BASIC V2 ****", (0 << 8) | CLEAR);
    direct_write_screen(0, y++, " 64K RAM SYSTEM  38911 BASIC BYTES FREE", (0 << 8) | CLEAR);
    y += 2;
    direct_write_screen(0, y++, "READY.", (0 << 8) | CLEAR);
    screen.row = y;
    screen.col = 0;

    scr_refresh(SLOW_REFRESH);

    sleep(dur);

    return 0;
}
#else
static int
waitstate(void *xd, int ms)
{
    USE_VAR(xd);

    direct_write_screen(TERM_WINDOW_GET_REPORTED_COLS() / 2 - 17, TERM_WINDOW_GET_ROWS() / 2,
                        "**** Initializing, please wait ****", (15 << 9) | CLEAR);
    screen.row = 0;
    screen.col = 0;

    scr_refresh(SLOW_REFRESH);

    sleep((time_t) (ms / 1000));

    return 0;

}
#endif

static _ns_efuns *
escreen_reg_funcs(void)
{
    _ns_efuns *efuns;

    /* Register functions */
    efuns = ns_new_efuns();

    ns_register_ssx(efuns, set_scroll_x);
    ns_register_ssy(efuns, set_scroll_y);
    ns_register_ssw(efuns, set_scroll_w);
    ns_register_ssh(efuns, set_scroll_h);

    ns_register_red(efuns, redraw);
    ns_register_rda(efuns, redraw_xywh);
    ns_register_exb(efuns, expire_buttons);

    ns_register_ins(efuns, ins_disp);
    ns_register_del(efuns, del_disp);
    ns_register_upd(efuns, upd_disp);

    ns_register_err(efuns, err_msg);
    ns_register_exe(efuns, exe_prg);

    ns_register_txt(efuns, inp_text);
    ns_register_inp(efuns, input_dialog);
    ns_register_tab(efuns, menu_tab);

    ns_register_fun(efuns, waitstate);

    return efuns;
}

static int
make_escreen_menu(buttonbar_t *bbar)
{
    static int been_here = 0;
    button_t *button;

#if 0
    menu_t *m;
    menuitem_t *i;
#endif

    if (been_here) {            /* the start function may be called more than once */
        return 0;               /* in later versions, but we only want one Escreen menu */
    }

    been_here = 1;

#if 0
    if ((m = menu_create(NS_MENU_TITLE))) {
        char *sc[] = {
            /* display functions */
            "New", "es_display(new)",   /* \x01:screen\r */
            "New...", "es_display(new,ask)",
            "Rename...", "es_display(name,ask)",
            "Backlog...", "es_display(backlog)",
            "Monitor", "es_display(monitor)",
            "Close", "es_display(close)",
            "-", "",
            /* region functions */
            "Split", "es_region(new)",
            "Unsplit", "es_region(full)",
            "Prev region", "es_region(prev)",   /* NS_SCREEN_PRVS_REG */
            "Next region", "es_region(next)",
            "Kill region", "es_region(kill)",
            "-", "",
            /* screen functions */
            "Reset", "es_reset",
            "Statement", "es_statement",
            "-", ""
        };
        int n, nsc = sizeof(sc) / sizeof(char *);
        int j, k = menu_list ? menu_list->nummenus : 0;

        if (menu_list) {
            for (n = 0; n < menu_list->nummenus; n++) {
                /* blend in w/ l&f */
                if (menu_list->menus[n]->font) {
                    m->font = menu_list->menus[n]->font;
                    m->fwidth = menu_list->menus[n]->fwidth;
                    m->fheight = menu_list->menus[n]->fheight;
#ifdef MULTI_CHARSET
                    m->fontset = menu_list->menus[n]->fontset;
#endif
                    break;
                }
            }
        }

        for (n = 0; n < (nsc - 1); n += 2) {
            if (!strcmp(sc[n], "-")) {
                /* separator */
                if ((i = menuitem_create(NULL))) {
                    menu_add_item(m, i);
                    menuitem_set_action(i, MENUITEM_SEP, NULL);
                }
            } else if ((i = menuitem_create(sc[n]))) {
                menuitem_set_action(i, MENUITEM_SCRIPT, sc[n + 1]);
                menu_add_item(m, i);
            }
        }
        if ((i = menuitem_create("About..."))) {
            menuitem_set_action(i, MENUITEM_ALERT, "Screen/Twin compatibility layer by Azundris <scream@azundris.com>");
            menu_add_item(m, i);
        }
        menu_list = menulist_add_menu(menu_list, m);
        for (j = k; j < menu_list->nummenus; j++) {
            event_data_add_mywin(&menu_event_data, menu_list->menus[j]->win);
        }
        if (!k) {
            menu_init();
        }
#endif

        if ((button = button_create(NS_MENU_TITLE))) {
            if ((button_set_action(button, ACTION_MENU, NS_MENU_TITLE))) {
                bbar_add_rbutton(bbar, button);
                bbar_calc_button_sizes(bbar);
            }
        }
        return 1;

#if 0
    }
    return 0;
#endif
}

/* Set everything up for escreen mode */
int
escreen_init(char **argv)
{
    int ns_err;
    _ns_efuns *efuns;
    buttonbar_t *bbar;
    spif_uchar_t old_pause = (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_PAUSE) ? (1) : (0));

    if (TermWin.screen_mode == NS_MODE_NONE) {
        return run_command(argv);
    }

    efuns = escreen_reg_funcs();

    /* Create buttonbar for Escreen's use. */
    if (!(bbar = bbar_create())) {
        if (buttonbar) {
            bbar = buttonbar;
        } else {
            return -1;
        }
    } else {
        if (!buttonbar) {
            buttonbar = bbar;
        }
        bbar_set_font(bbar, ((rs_es_font) ? (rs_es_font) : ("-*-helvetica-medium-r-normal--10-*-*-*-p-*-iso8859-1")));
        bbar_init(bbar, TERM_WINDOW_GET_WIDTH());
        bbar_add(bbar);
    }

    BITFIELD_SET(eterm_options, ETERM_OPTIONS_PAUSE);
    if (!(TermWin.screen = ns_attach_by_URL(rs_url, rs_hop, &efuns, &ns_err, bbar))) {
        D_CMD(("ns_attach_by_URL(%s,%s) failed\n", rs_url, rs_hop));
        return -1;
    }
    if (rs_delay >= 0) {
        TermWin.screen->delay = rs_delay;       /* more flexible ways later */
    }
    if (!old_pause) {
        BITFIELD_CLEAR(eterm_options, ETERM_OPTIONS_PAUSE);
    }

    make_escreen_menu(bbar);

    bbar_set_docked(bbar, rs_es_dock);
    bbar_set_visible(bbar, 0);
    bbar_show(bbar, 1);
    parent_resize();

    bbar_redraw(bbar);          /* get a bar in twin too */

    /* add_screen_ctl_button(bbar,"New",'c'); */
    D_CMD(("TermWin.screen->fd = %d\n", TermWin.screen->fd));
    return TermWin.screen->fd;
}
#endif



/* init_command() */
void
init_command(char **argv)
{
    int (*command_func) (char **);

    /* Use init function appropriate for how we were compiled. */
#ifdef ESCREEN
    command_func = escreen_init;
#else
    command_func = run_command;
#endif

    /* Initialize the command connection.  This should be called after
       the X server connection is established. */

    /* Enable delete window protocol */
    XSetWMProtocols(Xdisplay, TermWin.parent, &(props[PROP_DELETE_WINDOW]), 1);

    init_locale();

#ifdef META8_OPTION
    meta_char = (BITFIELD_IS_SET(vt_options, VT_OPTIONS_META8) ? 0x80 : 033);
#endif

#ifdef GREEK_SUPPORT
    greek_init();
#endif

    Xfd = XConnectionNumber(Xdisplay);
    D_CMD(("Xfd = %d\n", Xfd));
    cmdbuf_ptr = cmdbuf_endp = cmdbuf_base;
    AT_LEAST(num_fds, ((unsigned int) (Xfd + 1)));
    if (pipe_fd >= 0) {
        AT_LEAST(num_fds, ((unsigned int) (pipe_fd + 1)));
    }
    if ((cmd_fd = command_func(argv)) < 0) {
        libast_print_error("Unable to run sub-command.\n");
        paused = 1;
        rs_finished_text = "Hit a key to exit...";
    }
}

/* window resizing */
/*
 * Tell the teletype handler what size the window is.
 * Called after a window size change.
 */
void
tt_winsize(int fd)
{
    struct winsize ws;

    if (fd < 0)
        return;

    MEMSET(&ws, 0, sizeof(struct winsize));

    ws.ws_row = (unsigned short) TERM_WINDOW_GET_REPORTED_ROWS();
    ws.ws_col = (unsigned short) TERM_WINDOW_GET_REPORTED_COLS();
#ifndef __CYGWIN32__
    ws.ws_xpixel = (unsigned short) TERM_WINDOW_GET_WIDTH();
    ws.ws_ypixel = (unsigned short) TERM_WINDOW_GET_HEIGHT();
#endif
    D_CMD(("Sending TIOCSWINSZ to fd %d:  %hdx%hd (%hdx%hd)\n", fd, ws.ws_row, ws.ws_col, ws.ws_xpixel, ws.ws_ypixel));
    ioctl(fd, TIOCSWINSZ, &ws);
}

void
tt_resize(void)
{
    tt_winsize(cmd_fd);
}

/* attempt to `write' COUNT to the input buffer */
unsigned int
cmd_write(const unsigned char *str, unsigned int count)
{

    int n;

    n = (count - (cmdbuf_ptr - cmdbuf_base));
    /* need to insert more chars that space available in the front */
    if (n > 0) {
        /* try and get more space from the end */
        unsigned char *src, *dst;

        dst = (cmdbuf_base + sizeof(cmdbuf_base) - 1);  /* max pointer */

        if ((cmdbuf_ptr + n) > dst)
            n = (dst - cmdbuf_ptr);     /* max # chars to insert */

        if ((cmdbuf_endp + n) > dst)
            cmdbuf_endp = (dst - n);    /* truncate end if needed */

        /* equiv: memmove ((cmdbuf_ptr+n), cmdbuf_ptr, n); */
        src = cmdbuf_endp;
        dst = src + n;
        /* FIXME: anything special to avoid possible pointer wrap? */
        while (src >= cmdbuf_ptr)
            *dst-- = *src--;

        /* done */
        cmdbuf_ptr += n;
        cmdbuf_endp += n;
    }
    while (count-- && cmdbuf_ptr > cmdbuf_base) {
        /* sneak one in */
        cmdbuf_ptr--;
        *cmdbuf_ptr = str[count];
    }

    return (0);
}

#ifdef BACKGROUND_CYCLING_SUPPORT
RETSIGTYPE
check_pixmap_change(int sig)
{

    static time_t last_update = 0;
    time_t now;
    static unsigned long image_idx = 0;
    void (*old_handler) (int);
    static unsigned char in_cpc = 0;

    if (in_cpc)
        SIG_RETURN(0);
    in_cpc = 1;
    D_PIXMAP(("check_pixmap_change(%d):  rs_anim_delay == %lu seconds, last_update == %lu\n", sig, rs_anim_delay, last_update));
    if (!rs_anim_delay)
        SIG_RETURN(0);
    if (last_update == 0) {
        last_update = time(NULL);
        old_handler = signal(SIGALRM, check_pixmap_change);
        alarm(rs_anim_delay);
        in_cpc = 0;
        SIG_RETURN(0);
    }
    now = time(NULL);
    D_PIXMAP(("now %lu >= %lu (last_update %lu + rs_anim_delay %lu) ?\n", now, last_update + rs_anim_delay, last_update,
              rs_anim_delay));
    if (now >= last_update + rs_anim_delay || 1) {
        D_PIXMAP(("Time to update pixmap.  now == %lu\n", now));
        imlib_context_set_image(images[image_bg].current->iml->im);
        imlib_free_image_and_decache();
        images[image_bg].current->iml->im = NULL;
        xterm_seq(ESCSEQ_XTERM_PIXMAP, rs_anim_pixmaps[image_idx++]);
        last_update = now;
        old_handler = signal(SIGALRM, check_pixmap_change);
        alarm(rs_anim_delay);
        if (!rs_anim_pixmaps[image_idx]) {
            image_idx = 0;
        }
    }
    in_cpc = 0;
    if (old_handler) {
        SIG_RETURN((*old_handler) (sig));
    } else {
        SIG_RETURN(sig);
    }
}
#endif /* BACKGROUND_CYCLING_SUPPORT */

/* cmd_getc() - Return next input character */
/*
 * Return the next input character after first passing any keyboard input
 * to the command.
 */
unsigned char
cmd_getc(void)
{
#define TIMEOUT_USEC 2500
    static short refreshed = 0;
    fd_set readfds;
    int retval;
    struct timeval value, *delay;

    /* If there has been a lot of new lines, then update the screen
     * What the heck I'll cheat and only refresh less than every page-full.
     * the number of pages between refreshes is refresh_limit, which
     * is incremented here because we must be doing flat-out scrolling.
     *
     * refreshing should be correct for small scrolls, because of the
     * time-out
     */
    if (refresh_count >= (refresh_limit * (TERM_WINDOW_GET_ROWS() - 1))) {
        D_CMD(("Refresh count %d >= limit %d * rows.  (Refresh period %d.)\n", refresh_count, refresh_limit, REFRESH_PERIOD));
        if (refresh_limit < REFRESH_PERIOD)
            refresh_limit++;
        refresh_count = 0;
        refreshed = 1;
#ifdef PROFILE
        P_CALL(scr_refresh(refresh_type), "cmd_getc()->scr_refresh()");
#else
        scr_refresh(refresh_type);
#endif
    }
#ifdef ESCREEN
    if (TermWin.screen) {
        switch (TermWin.screen->backend) {
            case NS_MODE_NONE:
                break;
            case NS_MODE_NEGOTIATE:
#  ifdef NS_HAVE_SCREEN
            case NS_MODE_SCREEN:
                parse_screen_status_if_necessary();
                break;
#  endif
#  ifdef NS_HAVE_SCREAM
            case NS_MODE_SCREAM:
                break;
#  endif
#  ifdef NS_HAVE_TWIN
            case NS_MODE_TWIN:
                if (!TermWin.screen->twin) {
                    if (!TermWin.screen->timestamp) {
                        TermWin.screen->timestamp = time(NULL);
                    } else if (TermWin.screen->timestamp < time(NULL)) {
                        if (!Tw_CheckMagic(libscream_magic)) {
                            D_ESCREEN(("ns_attach_by_sess: Tw_CheckMagic failed\n"));
                            TermWin.screen->backend = TermWin.screen_mode = NS_MODE_NONE;
                        } else {
                            if (!(TermWin.screen->twin = Tw_Open(TermWin.screen->twin_str))) {
                                ns_desc_twin(TermWin.screen, "cmd_getc->Tw_Open");
                                TermWin.screen->backend = TermWin.screen_mode = NS_MODE_NONE;
                            } else {
                                D_ESCREEN(("ns_attach_by_sess: Tw_Open(%s) succeeded: handle @ %p\n", TermWin.screen->twin_str,
                                           TermWin.screen->twin));
                            }
                        }
                    }
                }
                break;
#  endif
            default:
                D_ESCREEN(("mode %d not supported...\n", TermWin.screen->backend));
                TermWin.screen->backend = TermWin.screen_mode = NS_MODE_NONE;
        }
    }
#endif

    /* characters already read in */
    if (CHARS_READ()) {
        RETURN_CHAR();
    }

    for (;;) {
        v_doPending();
        while (XPending(Xdisplay)) {    /* process pending X events */

            XEvent ev;

            refreshed = 0;
            XNextEvent(Xdisplay, &ev);

#ifdef USE_XIM
            if (xim_input_context) {
                if (!XFilterEvent(&ev, ev.xkey.window)) {
                    event_dispatch(&ev);
                }
            } else
#endif
                event_dispatch(&ev);

            /* in case button actions pushed chars to cmdbuf */
            if (CHARS_READ()) {
                RETURN_CHAR();
            }
        }
        if (paused == 1) {
            const char *done = " -- Task Finished, ESC to exit";

            append_to_title(rs_finished_title ? rs_finished_title : done);
            append_to_icon_name(rs_finished_title ? rs_finished_title : done);

            paused++;

            if (rs_finished_text) {
                cmd_write((unsigned char *) rs_finished_text, strlen(rs_finished_text));
            }
        } else if (!paused && pipe_fd < 0 && cmd_fd < 0) {
            exit(0);
        }
#ifdef SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
        if (scrollbar_uparrow_is_pressed()) {
            if (!scroll_arrow_delay-- && scr_page(UP, 1)) {
                scroll_arrow_delay = SCROLLBAR_CONTINUOUS_DELAY;
                refreshed = 0;
            }
        } else if (scrollbar_downarrow_is_pressed()) {
            if (!scroll_arrow_delay-- && scr_page(DN, 1)) {
                scroll_arrow_delay = SCROLLBAR_CONTINUOUS_DELAY;
                refreshed = 0;
            }
        }
#endif /* SCROLLBAR_BUTTON_CONTINUAL_SCROLLING */

        /* Nothing to do! */
        FD_ZERO(&readfds);
        if (cmd_fd >= 0) {
            FD_SET(cmd_fd, &readfds);
        }
        FD_SET(Xfd, &readfds);
        if (pipe_fd >= 0) {
            FD_SET(pipe_fd, &readfds);
        }
        value.tv_usec = TIMEOUT_USEC;
        value.tv_sec = 0;

        if (refreshed
#ifdef SCROLLBAR_BUTTON_CONTINUAL_SCROLLING
            && !(scrollbar_arrow_is_pressed())
#endif
            ) {
            delay = NULL;
        } else {
            delay = &value;
        }
        retval = select(num_fds, &readfds, NULL, NULL, delay);

        if (retval < 0) {
            if (cmd_fd >= 0 && FD_ISSET(cmd_fd, &readfds)) {
                if (errno != EINTR) {   /* may have rcvd SIGCHLD or so */
                    libast_print_error(" (%ld) Error reading from tty -- %s\n", getpid(), strerror(errno));
                    cmd_fd = -1;
                }
            }
            if (pipe_fd >= 0 && FD_ISSET(pipe_fd, &readfds)) {
                libast_print_error("Error reading from pipe -- %s\n", strerror(errno));
                pipe_fd = -1;
            }
            if (pipe_fd < 0 && cmd_fd < 0 && !paused) {
                exit(errno);
            }
        } else if (retval == 0) {
            refresh_count = 0;
            refresh_limit = 1;
            if (!refreshed) {
                refreshed = 1;
                D_CMD(("select() timed out, time to update the screen.\n"));
                scr_refresh(refresh_type);
                if (scrollbar_is_visible()) {
                    scrollbar_anchor_update_position(1);
                }
#ifdef USE_XIM
                xim_send_spot();
#endif
            }
        } else {
            /* We have something to read from. */
            if (cmd_fd >= 0 && FD_ISSET(cmd_fd, &readfds)) {
                /* See if we can read from the application */
                register unsigned int count = CMD_BUF_SIZE;

                cmdbuf_ptr = cmdbuf_endp = cmdbuf_base;
                while (count) {
                    register int n = read(cmd_fd, cmdbuf_endp, count);

                    if (n < 0) {
                        if (errno == EINTR) {
                            /* Ignore and continue reading. */
                            continue;
                        } else if (errno == EAGAIN) {
                            /* Stop looping. */
                            break;
                        } else {
                            /* Our file descriptor went bye-bye. */
                            cmd_fd = -1;
                            if (!paused && (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_PAUSE))) {
                                paused = 1;
                            }
                            break;
                        }
                    } else if (n == 0) {
                        /* EOF */
                        cmd_fd = -1;
                        if (!paused && (BITFIELD_IS_SET(eterm_options, ETERM_OPTIONS_PAUSE))) {
                            paused = 1;
                        }
                        break;
                    }
                    cmdbuf_endp += n;
                    count -= n;
                }
                /* some characters read in */
                if (CHARS_BUFFERED()) {
                    RETURN_CHAR();
                }
            }
            if (pipe_fd >= 0 && FD_ISSET(pipe_fd, &readfds)) {
                register unsigned int count = CMD_BUF_SIZE / 2;

                cmdbuf_ptr = cmdbuf_endp = cmdbuf_base;
                while (count) {

                    register int n = read(pipe_fd, cmdbuf_endp, count);

                    if (n < 0) {
                        if (errno == EINTR) {
                            /* Ignore and continue reading. */
                            continue;
                        } else if (errno == EAGAIN) {
                            /* Stop looping. */
                            break;
                        } else {
                            /* Our file descriptor went bye-bye. */
                            pipe_fd = -1;
                            break;
                        }
                    } else if (n == 0) {
                        /* EOF */
                        pipe_fd = -1;
                        break;
                    }
                    n = add_carriage_returns(cmdbuf_endp, n);
                    cmdbuf_endp += n;
                    count -= n;
                }
                /* some characters read in */
                if (CHARS_BUFFERED()) {
                    RETURN_CHAR();
                }
            }
        }
    }
    return (0);
}

/* Put a character back in the buffer.  Only use this once at a time. */
void
cmd_ungetc(void)
{
    cmdbuf_ptr--;
}

/* tt_write(), tt_printf() - output to command */
/*
 * Send count characters directly to the command
 */
void
tt_write(const unsigned char *buf, unsigned int count)
{

    v_writeBig(cmd_fd, (char *) buf, count);

#if 0                           /* Fixes the bug that hung Eterm when pasting a lot of stuff */
    while (count > 0) {
        int n = write(cmd_fd, buf, count);

        if (n > 0) {
            count -= n;
            buf += n;
        }
    }
#endif
}

/*
 * Send printf() formatted output to the command.
 * Only use for small ammounts of data.
 */
void
tt_printf(const unsigned char *fmt, ...)
{
    static unsigned char buf[256];
    va_list arg_ptr;

    va_start(arg_ptr, fmt);
    vsnprintf((char *) buf, sizeof(buf), (char *) fmt, arg_ptr);
    va_end(arg_ptr);
    tt_write(buf, strlen((char *) buf));
}

/* Read and process output from the application */
void
main_loop(void)
{
    /*   int ch; */
    register int ch;

    D_CMD(("PID %d\n", getpid()));
    D_CMD(("Command buffer base == %8p, length %lu, end at %8p\n", cmdbuf_base, CMD_BUF_SIZE, cmdbuf_base + CMD_BUF_SIZE - 1));

#ifdef BACKGROUND_CYCLING_SUPPORT
    if (rs_anim_delay) {
        check_pixmap_change(0);
    }
#endif

    do {
        while ((ch = cmd_getc()) == 0); /* wait for something */
        if (ch >= ' ' || ch == '\t' || ch == '\n' || ch == '\r') {
            /* Read a text string from the input buffer */
            int nlines = 0;
            unsigned char *str;

            D_CMD(("Command buffer contains %d characters.\n", cmdbuf_endp - cmdbuf_ptr));
            D_VT(("\n%s\n\n", safe_print_string(cmdbuf_ptr - 1, cmdbuf_endp - cmdbuf_ptr + 1)));

            /*
             * point to the start of the string,
             * decrement first since already did get_com_char ()
             */
            str = --cmdbuf_ptr;
            while (cmdbuf_ptr < cmdbuf_endp) {

                ch = *cmdbuf_ptr++;
#if DEBUG >= DEBUG_VT
                if (DEBUG_LEVEL >= DEBUG_VT) {
                    if (ch < 32) {
                        D_VT(("\'%s\' (%d 0x%02x %03o)\n", get_ctrl_char_name(ch), ch, ch, ch));
                    } else {
                        D_VT(("\'%c\' (%d 0x%02x %03o)\n", ch, ch, ch, ch));
                    }
                }
#endif
                if (ch >= ' ' || ch == '\t' || ch == '\r') {
                    NOP;
                } else if (ch == '\n') {
                    nlines++;
                    if (++refresh_count >= (refresh_limit * (TERM_WINDOW_GET_ROWS() - 1)))
                        break;
                } else {
                    /* unprintable */
                    cmdbuf_ptr--;
                    break;
                }
            }
            D_SCREEN(("Adding %d lines (%d chars); str == %8p, cmdbuf_ptr == %8p, cmdbuf_endp == %8p\n",
                      nlines, cmdbuf_ptr - str, str, cmdbuf_ptr, cmdbuf_endp));
#if FIXME_BLOCK
            /* 
             * iconv() is not my friend. :-( I've tried various things
             * to make this work (including UCS2, SJIS, EUCJ, and
             * WCHAR_T), but nothing has worked.  I'm obviously
             * missing something, so if you know what, kindly throw me
             * a clue.  :-)                                       -- mej
             */
            if (!strcmp(nl_langinfo(CODESET), "UTF-8")) {
                iconv_t handle;

                if (encoding_method != UCS2) {
                    set_multichar_encoding("utf8");
                }
                handle = iconv_open("WCHAR_T", "UTF-8");
                if (handle == (iconv_t) -1) {
                    libast_print_error("Unable to decode UTF-8 locale %s to WCHAR_T.  Defaulting to portable C locale.\n",
                                setlocale(LC_ALL, ""));
                    setlocale(LC_ALL, "C");
                    scr_add_lines(str, nlines, (cmdbuf_ptr - str));
                } else {
                    char *outbuff, *pinbuff, *poutbuff;
                    wchar_t *wcbuff;
                    mbstate_t mbs;
                    size_t bufflen, outlen = 0, retval;

                    pinbuff = (char *) str;
                    bufflen = cmdbuf_ptr - str;
                    outlen = bufflen * 6;
                    poutbuff = outbuff = (char *) MALLOC(outlen);

                    errno = 0;
                    D_VT(("Allocated output buffer of %lu chars at %010p against input buffer of %lu\n", bufflen * 6, outbuff,
                          bufflen));
                    libast_print_warning("Moo:  %s\n", safe_print_string(str, bufflen));
                    retval = iconv(handle, &pinbuff, &bufflen, &poutbuff, &outlen);
                    outlen = (size_t) (poutbuff - outbuff);
                    if (retval != (size_t) - 1) {
                        errno = 0;
                    }
                    if (errno == E2BIG) {
                        libast_print_error("My UTF-8 decode buffer was too small by %lu bytes?!\n", bufflen);
                    } else if (errno == EILSEQ) {
                        libast_print_error("Illegal multibyte sequence encountered at \'%c\' (0x%02x); skipping.\n", *pinbuff, *pinbuff);
                        *pinbuff = ' ';
                        pinbuff++;
                    } else if (errno == EINVAL) {
                        D_VT(("Incomplete multibyte sequence encountered.\n"));
                        libast_print_warning("Converted %lu input chars to %lu output chars before incomplete sequence.\n",
                                      (cmdbuf_ptr - str), outlen);
                    } else {
                        libast_print_warning("Converted %lu input chars to %lu output chars.\n", (cmdbuf_ptr - str), outlen);
                    }

                    libast_print_warning("Moo2:  %s\n", safe_print_string(outbuff, outlen));
                    MEMSET(outbuff + outlen, 0, sizeof(wchar_t));
                    wcbuff = (wchar_t *) outbuff;
                    MEMSET(&mbs, 0, sizeof(mbstate_t));
                    outlen = wcsrtombs(NULL, &wcbuff, 0, &mbs) + 1;
                    if (outlen > 0) {
                        outbuff = (char *) MALLOC(outlen);

                        outlen = wcsrtombs(outbuff, &wcbuff, outlen, &mbs);
                        if ((long) outlen >= 0) {
                            FREE(wcbuff);
                            libast_print_error("I win!\n");
                        } else {
                            libast_print_error("wcsrtombs() returned %ld (errno is %d (%s))\n", (unsigned long) outlen, errno,
                                        strerror(errno));
                        }
                        if (pinbuff > (char *) str) {
                            cmdbuf_ptr = (unsigned char *) pinbuff;
                            scr_add_lines(outbuff, nlines, outlen);
                        }
                    } else {
                        libast_print_error("wcsrtombs(NULL, %10p, 0) returned %ld (errno is %d (%s))\n", wcbuff, (unsigned long) outlen,
                                    errno, strerror(errno));
                    }
                    FREE(outbuff);
                }
            } else
#endif
                scr_add_lines(str, nlines, (cmdbuf_ptr - str));
        } else {
            switch (ch) {
# ifdef NO_ENQ_ANS
                case 005:
                    break;
# else
                case 005:      /* ^E (ENQ) terminal status enquiry */
                    tt_printf(VT100_ANS);
                    break;
# endif
                case 007:      /* ^G (BEL) */
                    scr_bell();
                    break;
                case '\b':
                    scr_backspace();
                    break;
                case 013:      /* ^K (VT) */
                case 014:      /* ^L (FF) */
                    scr_index(UP);
                    break;
                case 016:      /* ^N (SO) shift out (enter ACS mode) */
                    scr_charset_choose(1);
                    break;
                case 017:      /* ^O (SI) shift in (leave ACS mode) */
                    scr_charset_choose(0);
                    break;
                case 033:
                    process_escape_seq();
                    break;
            }
        }
    } while (ch != EOF);
}

/* output a burst of any pending data from a paste... */
int
v_doPending(void)
{

    if (v_bufstr >= v_bufptr)
        return (0);
    v_writeBig(cmd_fd, NULL, 0);
    return (1);
}

/* Write data to the pty as typed by the user, pasted with the mouse,
 * or generated by us in response to a query ESC sequence.
 * Code stolen from xterm 
 */
void
v_writeBig(int f, char *d, int len)
{

    int written;
    int c = len;

    if (!v_bufstr && len > 0) {

        v_buffer = MALLOC(len);
        v_bufstr = v_buffer;
        v_bufptr = v_buffer;
        v_bufend = v_buffer + len;
    }
    /*
     * Append to the block we already have.
     * Always doing this simplifies the code, and
     * isn't too bad, either.  If this is a short
     * block, it isn't too expensive, and if this is
     * a long block, we won't be able to write it all
     * anyway.
     */

    if (len > 0) {
        if (v_bufend < v_bufptr + len) {        /* we've run out of room */
            if (v_bufstr != v_buffer) {
                /* there is unused space, move everything down */
                /* possibly overlapping bcopy here */

                /* bcopy(v_bufstr, v_buffer, v_bufptr - v_bufstr); */
                memcpy(v_buffer, v_bufstr, v_bufptr - v_bufstr);
                v_bufptr -= v_bufstr - v_buffer;
                v_bufstr = v_buffer;
            }
            if (v_bufend < v_bufptr + len) {
                /* still won't fit: get more space */
                /* Don't use XtRealloc because an error is not fatal. */
                int size = v_bufptr - v_buffer; /* save across realloc */

                v_buffer = REALLOC(v_buffer, size + len);
                if (v_buffer) {
                    v_bufstr = v_buffer;
                    v_bufptr = v_buffer + size;
                    v_bufend = v_bufptr + len;
                } else {
                    /* no memory: ignore entire write request */
                    libast_print_error("cannot allocate buffer space\n");
                    v_buffer = v_bufstr;        /* restore clobbered pointer */
                    c = 0;
                }
            }
        }
        if (v_bufend >= v_bufptr + len) {       /* new stuff will fit */
            memcpy(v_bufptr, d, len);   /* bcopy(d, v_bufptr, len); */
            v_bufptr += len;
        }
    }
    /*
     * Write out as much of the buffer as we can.
     * Be careful not to overflow the pty's input silo.
     * We are conservative here and only write
     * a small amount at a time.
     *
     * If we can't push all the data into the pty yet, we expect write
     * to return a non-negative number less than the length requested
     * (if some data written) or -1 and set errno to EAGAIN,
     * EWOULDBLOCK, or EINTR (if no data written).
     *
     * (Not all systems do this, sigh, so the code is actually
     * a little more forgiving.)
     */

    if (v_bufptr > v_bufstr) {
        written = write(f, v_bufstr, v_bufptr - v_bufstr <= MAX_PTY_WRITE ? v_bufptr - v_bufstr : MAX_PTY_WRITE);
        if (written < 0) {
            written = 0;
        }
        D_TTY(("Wrote %d characters\n", written));
        v_bufstr += written;
        if (v_bufstr >= v_bufptr)       /* we wrote it all */
            v_bufstr = v_bufptr = v_buffer;
    }
    /*
     * If we have lots of unused memory allocated, return it
     */
    if (v_bufend - v_bufptr > 1024) {   /* arbitrary hysteresis */
        /* save pointers across realloc */
        int start = v_bufstr - v_buffer;
        int size = v_bufptr - v_buffer;
        int allocsize = size ? size : 1;

        v_buffer = REALLOC(v_buffer, allocsize);
        if (v_buffer) {
            v_bufstr = v_buffer + start;
            v_bufptr = v_buffer + size;
            v_bufend = v_buffer + allocsize;
        } else {
            /* should we print a warning if couldn't return memory? */
            v_buffer = v_bufstr - start;        /* restore clobbered pointer */
        }
    }
}
