/* $Id: mwm.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $ */
/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/
/****************************************************************************
 * This module is based on Twm, but has been siginificantly modified 
 * by Rob Nation 
 ****************************************************************************/
/***********************************************************************
 * The rest of it is all my fault -- MLM
 *
 * Copyright (C) 1995-2002 LessTif Development Team
 * mwm - "LessTif Window Manager"
 ***********************************************************************/

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#else
#error "you lose (I don't know how to fix this)"
#endif
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#ifdef HAVE_GETRLIMIT
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <sys/resource.h>
#endif

#ifdef __EMX__
#include <netdb.h>
#include <X11/Xlibint.h>    /* for __XOS2RedirRoot() */
#endif

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/Xproto.h>

#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/XmosP.h>
#include <Xm/DisplayP.h>
#include <Xm/ScreenP.h>

#if XmVERSION >= 2
#include <XmI/XmI.h>
#endif

#include "mwm.h"


#ifndef HAVE_GETHOSTNAME
/* our fallback implementation (gethostname.c) */
extern int gethostname (char *name, size_t len);
#endif


/*
 * application globals
 */
#define MAXHOSTNAME 255

MwmInternalInfo Mwm;
Display *dpy;			/* which display are we talking to */
Widget toplevel;
static Widget xmDisplay;
static Boolean multiscreen = False;
static char *mwm_name = "mwm";

XContext MwmContext;		/* context for mwm windows */
XContext MenuContext;		/* context for mwm menus */

int JunkX = 0, JunkY = 0;
Window JunkRoot, JunkChild;	/* junk window */
unsigned int JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;
char **g_argv;
int fd_width, x_fd;
static XtAppContext app;
volatile int alarmed;

Bool debugging = False;

static int last_event_type = 0;

/****************************************************************************/

/*
 * print usage
 */
static void
usage(void)
{
    fprintf(stderr, 
            "Mwm Ver %s\n\nusage: mwm [-display dpy] [-debug] [-xrm resourcestring] [-multiscreen] [-name name] [-screens name [name [...]]]\n", 
            VERSION);
}

/*
 * restart on a signal
 */
static void
sig_restart(int sig)
{
    MWM_Done(1, *g_argv);
}

/*
 * exit on a signal
 */
static void
sig_done(int nonsense)
{
    MWM_Done(0, NULL);
}

/*
 * For auto-raising windows, this routine is called
 */
static void
sig_alarm(int nonsense)
{
    alarmed = True;
    signal(SIGALRM, sig_alarm);
}


/*
 * figures out if there's another WM running
 */
static void
catch_redirect(Display *dpy, XErrorEvent *event)
{
    fprintf(stderr, "mwm: Error: Another WM is running\n");
    exit(1); /* we don't return (this determines function type) */
}

/*
 * displays info on internal errors
 */
static XErrorHandler
general_error(Display *dpy, XErrorEvent *event)
{
    /* some errors are acceptable, mostly they're caused by 
     * trying to update a lost  window */
    if ((event->error_code == BadWindow) ||
	(event->request_code == X_GetGeometry) ||
	(event->error_code == BadDrawable) ||
	(event->request_code == X_SetInputFocus) ||
	(event->request_code == X_GrabButton) ||
	(event->request_code == X_ChangeWindowAttributes) ||
	(event->request_code == X_InstallColormap))
	return 0;


    fprintf(stderr, "mwm: internal error");
    fprintf(stderr, "      Request %d, Error %d\n", event->request_code,
	    event->error_code);
    fprintf(stderr, "      EventType: %d", last_event_type);
    fprintf(stderr, "\n");
    return 0;
}

/*
 * do global mwm initialization
 */
static void
initialize_mwm( void)
{
    int i;
    ScreenInfo *scr;

    xmDisplay = XmGetXmDisplay(dpy);

    RES_Initialize();

    if (Mwm.multi_screen)
	multiscreen = True;

    EVENT_Initialize();

    PROP_Initialize();

    MwmContext = XUniqueContext();
    MenuContext = XUniqueContext();

    if (multiscreen)
    {

	Mwm.number_of_screens = ScreenCount(dpy);

	Mwm.screen_info = (ScreenInfo **)XtMalloc(Mwm.number_of_screens *
						  sizeof(ScreenInfo *));
	for (i = 0; i < Mwm.number_of_screens; i++)
	{

	    scr = (ScreenInfo *)XtCalloc(1, sizeof(ScreenInfo));

	    scr->screen = i;
	    scr->root_win = RootWindow(dpy, scr->screen);
	    if (scr->root_win == None)
	    {
		fprintf(stderr, "Screen %ld is not a valid screen", scr->screen);
		exit(1);
	    }

	/*TODO: only default screen names are supported
	 *   since '-screens' mwm command line option is not implemented yet
	 *		A.R.
	 */
	    /* By default, the screen number is used for the screen name. */
	    scr->screen_name = (String)XtMalloc(12);
	    sprintf(scr->screen_name, "%lu", scr->screen);

	    SCREEN_Initialize(scr);

	    Mwm.screen_info[i] = scr;
	}
    }
    else
    {
	Mwm.number_of_screens = 1;

	Mwm.screen_info = (ScreenInfo **)XtMalloc(Mwm.number_of_screens *
						  sizeof(ScreenInfo *));

	scr = (ScreenInfo *)XtCalloc(1, sizeof(ScreenInfo));

	scr->screen = DefaultScreen(dpy);
	scr->root_win = RootWindow(dpy, scr->screen);
	if (scr->root_win == None)
	{
	    fprintf(stderr, "Screen %ld is not a valid screen", scr->screen);
	    exit(1);
	}

	/*TODO: only default screen names are supported
	 *   since '-screens' mwm command line option is not implemented yet
	 *		A.R.
	 */
	/* By default, the screen number is used for the screen name. */
	scr->screen_name = (String)XtMalloc(12);
	sprintf(scr->screen_name, "%lu", scr->screen);

	SCREEN_Initialize(scr);

	Mwm.screen_info[0] = scr;
    }
}

/*
 * set the appropriate error handler
 */
void
MWM_SetErrorHandler(int which)
{
    if (which == REDIRECT)
	XSetErrorHandler((XErrorHandler)catch_redirect);
    else
	XSetErrorHandler((XErrorHandler)general_error);
}

/*
 * cleanup and exit mwm
 */
void
MWM_Done(int restart, const char *command)
{
    ScreenInfo *scr;
    int i, done, j;


    for (i = 0; i < Mwm.number_of_screens; i++)
    {
	PAGER_MoveViewPort(Mwm.screen_info[i], 0, 0, False);

	WIN_ReleaseWindows(Mwm.screen_info[i]);
    }

    /*
     * serious cleanup
     */
    if (restart)
    {
	for (i = 0; i < Mwm.number_of_screens; i++)
	{

	    scr = Mwm.screen_info[i];
	    DT_SaveState(scr);

	    /* Really make sure that the connection is closed and cleared! */
	    XSelectInput(dpy, scr->root_win, 0);
	}

	XSync(dpy, 0);
	XCloseDisplay(dpy);

	i = 0;
	j = 0;
	done = 0;

	/* really need to destroy all windows, explicitly,
	 * not sleep, but this is adequate for now */
	_XmSleep(1);
	ReapChildren();

	execvp(command, g_argv);
	fprintf(stderr, "MWM: Call of '%s' failed!!!!\n", command);

	execvp(g_argv[0], g_argv);	/* that _should_ work */
	fprintf(stderr, "MWM: Call of '%s' failed!!!!\n", g_argv[0]);
    }
    else
    {

	for (i = 0; i < Mwm.number_of_screens; i++)
	    PROP_ClearBehavior(Mwm.screen_info[i]);

	XCloseDisplay(dpy);
	exit(0);
    }
}


/*
 * ReapChildren - wait() for all dead child processes
 */
void
ReapChildren(void)
{
#ifdef HAVE_WAITPID
    while ((waitpid(-1, NULL, WNOHANG)) > 0);
#else
#ifdef HAVE_WAIT3
    while ((wait3(NULL, WNOHANG, NULL)) > 0);
#else
#error You lose: neither waitpid() nor wait3() is available!
#endif
#endif
}


/*
 * main - start of mwm
 */
int
main(int argc, char **argv)
{
    int i, len;
    char *display_string;
    Bool option_error = False;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "Mwm", NULL, 0, &argc, argv, NULL, NULL);
    dpy = XtDisplay(toplevel);

    for (i = 1; i < argc; i++)
    {
	if (strncmp(argv[i], "-debug", 6) == 0) /* non-standard option */
	{
	    debugging = True;
	}
	else if (strncmp(argv[i], "-multiscreen", 12) == 0)
	{
	    multiscreen = True;
	}
	else if (strncmp(argv[i], "-name", 5) == 0)
	{
	    if (++i >= argc)
		usage();
	    mwm_name = argv[i];
	}
	else if (strncmp(argv[i], "-screens", 8) == 0)
	{
	    if (++i >= argc)
		usage();
	    fprintf(stderr, "-screens is not yet supported\n");
	}
	else if (strncmp(argv[i], "-version", 8) == 0) /* non-standard option */
	{   
	    fprintf(stdout, "Mwm Version %s\n", VERSION);
	}
	else
	{
	    fprintf(stderr, "mwm:  Unknown option: `%s'\n", argv[i]);
	    option_error = True;
	}
    }

    if (option_error)
	usage();

    g_argv = argv;

    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
	signal(SIGINT, sig_done);
    if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
	signal(SIGHUP, sig_done);
    if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
	signal(SIGQUIT, sig_done);
    if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
	signal(SIGTERM, sig_done);

    signal(SIGUSR1, sig_restart);
    signal(SIGALRM, sig_alarm);

#ifdef HAVE_SYSCONF
    fd_width = sysconf(_SC_OPEN_MAX);
#else
# ifdef HAVE_GETDTABLESIZE
    fd_width = getdtablesize();
# else
#  ifdef	HAVE_GETRLIMIT
   {
	struct rlimit rl;
	getrlimit(RLIMIT_NOFILE, &rl);
	fd_width = rl.rlim_cur;
   }
#  else
#   error You lose
#  endif
# endif
#endif

#ifdef __CYGWIN__
    /*
     * Ugly hack because sys/types.h defines FD_SETSIZE as 64,
     * when the comments there say it should be >= NOFILE in param.h,
     * which happens to be 8192.
     *
     * This drops fd_width to 64 to match FD_SETSIZE;
     */
    if (fd_width > FD_SETSIZE) fd_width = FD_SETSIZE;
#endif

    x_fd = XConnectionNumber(dpy);

    /*
     * this is enormously dangerous, but _is_ the original code. MLM
     */
    if (fcntl(x_fd, F_SETFD, 1) == -1)
    {
	fprintf(stderr, "close-on-exec failed");
	exit(1);
    }

    /*
     * Add a DISPLAY entry to the environment, in case we were started
     * with mwm -display term:0.0
     */
    len = strlen(XDisplayString(dpy));
    display_string = XtMalloc(len + 10);
    sprintf(display_string, "DISPLAY=%s", XDisplayString(dpy));
    SetEnvironment("DISPLAY", XDisplayString(dpy));

    /*
     * Add a HOSTDISPLAY environment variable, which is the same as
     * DISPLAY, unless display = :0.0 or unix:0.0, in which case the full
     * host name will be used for ease in networking .
     */
    if (strncmp(display_string, "DISPLAY=:", 9) == 0)
    {
	char client[MAXHOSTNAME], *rdisplay_string;

	gethostname(client, MAXHOSTNAME);
	rdisplay_string = XtMalloc(len + 14 + strlen(client));
	sprintf(rdisplay_string, "%s:%s", client, &display_string[9]);
	SetEnvironment("HOSTDISPLAY", rdisplay_string);
	XtFree(rdisplay_string);
    }
    else if (strncmp(display_string, "DISPLAY=unix:", 13) == 0)
    {
	char client[MAXHOSTNAME], *rdisplay_string;

	gethostname(client, MAXHOSTNAME);
	rdisplay_string = XtMalloc(len + 14 + strlen(client));
	sprintf(rdisplay_string, "%s:%s", client,
		&display_string[13]);
	SetEnvironment("HOSTDISPLAY", rdisplay_string);
	XtFree(rdisplay_string);
    }
    else
    {
	char *rdisplay_string;

	rdisplay_string = XtMalloc(len + 14);
	sprintf(rdisplay_string, "%s", XDisplayString(dpy));
	SetEnvironment("HOSTDISPLAY", rdisplay_string);
	XtFree(rdisplay_string);
    }
    XtFree(display_string);

    initialize_mwm();

    if (debugging) {
	MouseButton *MouseEntry = Mwm.screen_info[0]->buttons;

	fprintf(stderr, "Button Bindings:\n");
	while (MouseEntry) {
		fprintf(stderr,
			"\tfunc %d %s button %d modifier %d context %s\n",
			MouseEntry->func, _MwmPrintF(MouseEntry->func),
			MouseEntry->button, MouseEntry->modifier,
			_MwmPrintC(MouseEntry->context));

		MouseEntry = MouseEntry->next;
	}
    }

    while (True)
    {
	XEvent event;

	last_event_type = 0;
	if (EVENT_Next(&event))
	{
	    EVENT_Dispatch(&event);
	}
    }

    return(0);
}


extern int
SetEnvironment(const char *key, const char *value)
#ifdef HAVE_PUTENV
{
  char *str;
  int len, rc;
 
  len=strlen(key)+strlen(value)+2;
  str=(char *)malloc(len);
  strcpy(str, key);
  strcat(str, "=");
  strcat(str, value);
  rc=putenv(str);
  /* do not free 'str' here! */
  return rc;
}
#else
#ifdef HAVE_SETENV
{
  int rc;
  int overwrite=1;

  rc=setenv(key, value, overwrite);
  return rc;
}
#else
#error You lose (neither putenv() nor setenv() are available!)
#endif /* #ifdef HAVE_SETENV */
#endif /* #ifdef HAVE_PUTENV */


/*
 * find the config file
 */
/*
 * The "Motif User's Guide [1]"  didn't mention what to do if; a. configFile
 *  is NOT defined,   b. configFile IS defined but it does not refer to a proper file
 *  (mulformed or unreadable), c. configFile does begin with "~" but not "~/".
 * The code below deals with these cases for conveniences.
 *
 * [1] (http://w3.pppl.gov/misc/motif/MotifUserGuide/en_US/Understanding_the_Resource_Description_File.html)
 */
 
extern char *
find_config_file(void)
{
    char *buf=NULL;
    char *head=NULL, *lang=NULL, *home=NULL;
    char *ptr=Mwm.config_file;
    struct stat st;
    Boolean rc=False;
#if defined(HAVE_SYS_TYPES_H) && defined(HAVE_PWD_H)
    struct passwd *passwd;
#endif

    home=getenv("HOME");
    lang=getenv("LANG");

    /* check for an absolute path in the given resource */
#ifdef __EMX__
    if ( Mwm.config_file[0]=='/' ||
         (isalpha(Mwm.config_file[0]) && Mwm.config_file[0]==':') )
#else
    if ( Mwm.config_file[0]=='/' )
#endif
    {
	head = "";
	ptr = Mwm.config_file;
    }
    else if (Mwm.config_file[0]=='~')
    {
	ptr++;
	if (Mwm.config_file[1] == '/')
	{   /* ~ => $HOME */
	    head = home;
	    ptr++;
	}
	else
#if defined(HAVE_SYS_TYPES_H) && defined(HAVE_PWD_H)	    
	{   /* ~hoge => hoge's home */
	    char *idx;

            /* copy user name from "~hoge/foo" */
	    idx = strchr(ptr, '/');
	    if (idx == NULL)
		 head = NULL;
	    else
	    {
	         char *tmpbuf;
		 
	         tmpbuf=XtMalloc(idx-ptr+1);
		 strncpy(tmpbuf, ptr, idx-ptr);
		 tmpbuf[idx-ptr]='\0';
		 passwd = getpwnam(tmpbuf);
		 XtFree(tmpbuf);
		 if (passwd != NULL)  {
		    head = XtNewString(passwd->pw_dir);
		    ptr = idx+1;
		 }
		 else
		    head = NULL; /* no such user "hoge" */
	    }
	}
#else
	;  /* no <pwd.h>, no ~hoge */
	head = NULL;
#endif
    }
    else
    {
        /* search for the current dir */
	head = getcwd(XtMalloc(MAX_PATH_LEN), MAX_PATH_LEN);
    }

/*
 * Head is either
 *	home directory, or
 *	NULL, or
 *	current directory
 */
    if (head && lang)
    {
	buf=XtRealloc(buf, strlen(head) + strlen(lang) + strlen(ptr) + 3);
	sprintf(buf, "%s/%s/%s", head, lang, ptr);
	if (stat(buf, &st) == 0)
	{
	    return buf;
	}
    }

    if (head)
    {
	buf=XtRealloc(buf, strlen(head) + strlen(ptr) + 2);
	sprintf(buf, "%s/%s", head, ptr);
	if (stat(buf, &st) == 0)
	{
	    return buf;
	}
    }

    if (home && lang)
    {
	buf=XtRealloc(buf, strlen(home)+strlen(lang)+strlen(HOME_MWMRC)+3);
	sprintf(buf, "%s/%s/%s", home, lang, HOME_MWMRC);
	if (stat(buf, &st) == 0)
	{
	    return buf;
	}
    }
    if (home)
    {
	buf=XtRealloc(buf, strlen(home)+strlen(HOME_MWMRC)+2);
	sprintf(buf, "%s/%s", home, HOME_MWMRC);
	if (stat(buf, &st) == 0)
	{
	    return buf;
	}
    }

    if (lang)
    {
	buf=XtRealloc(buf, strlen(MWM_DDIR)+strlen(lang)+strlen(SYSTEM_MWMRC)+3);
	sprintf(buf, "%s/%s/%s", MWM_DDIR, lang, SYSTEM_MWMRC); 
	if (stat(buf, &st) == 0)
	{
	    return buf;
	}
    }
	
    buf=XtRealloc(buf, strlen(MWM_DDIR)+strlen(SYSTEM_MWMRC)+2);
    sprintf(buf, "%s/%s", MWM_DDIR, SYSTEM_MWMRC); 
    if (stat(buf, &st) == 0)
	{
	    return buf;
	}
    
    XtFree(buf);
    /* head is not always a pointer allocated in this function,
       let's leak those few bytes here ... */
    return NULL;
}
