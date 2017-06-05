/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Print.c,v 1.40 2002/04/30 22:05:16 dannybackx Exp $
 * 
 * Copyright © 2000,2001,2002 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/


static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/Print.c,v 1.40 2002/04/30 22:05:16 dannybackx Exp $";

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <Xm/XmP.h>
#include <Xm/PrintSP.h>

#ifndef HAVE_LIB_XP
/* some prototypes */
XtEnum XmPrintPopupPDM(Widget print_shell,
                       Widget video_transient_for);
Widget XmPrintSetup(Widget video_widget,
                    Screen *print_screen,
                    String print_shell_name,
                    ArgList args,
                    Cardinal num_args);
Boolean XmIsPrintShell(Widget w);
XtEnum XmPrintToFile(Display *dpy,
                     String filename,
                     /*XPFinishProc*/ XtPointer finish_proc,
                     XtPointer client_data);
	
#else
#include <Xm/Print.h>
#endif

#include <XmI/XmI.h>

#include <XmI/DebugUtil.h>


/* Check whether all required UN*X APIs are available */
#if defined(HAVE_FORK) && defined(HAVE_PIPE) && defined(HAVE__EXIT)
#ifndef UNIXLIKE
#define UNIXLIKE
#endif
#endif

/* 
   amai:  
   In addition to the checks above:
   OS/2 EMX can't do a fork() in OMF objects like our LessTif DLLs.
   Also there's no working libXp. Many reasons to neglect this for now!
   Similar problems for cygwin.
 */
#if defined(__EMX__) || defined(CYGWIN)
#ifdef UNIXLIKE
#undef UNIXLIKE
#endif
#endif /* defined(__EMX__) || defined(CYGWIN) */



extern void
XmRedisplayWidget(Widget w)
{
	int		i;
	XExposeEvent	e;
	Region		r;

	/* See if sending a complete event helps ! */
	e.type = Expose;
	e.display = XtDisplay(w);
	e.window = XtWindowOfObject(w);

	e.width = XtWidth(w);
	e.height = XtHeight(w);
	e.x = XtX(w);
	e.y = XtY(w);

	DEBUGOUT(_LtDebug(__FILE__, w, "XmRedisplayWidget: size is %dx%d\n", e.width, e.height));

	e.count = 0;
	e.send_event = False;
	e.serial = LastKnownRequestProcessed(e.display);

	r = XCreateRegion();
	XtAddExposureToRegion((XEvent *)&e, r);

	if (w->core.widget_class->core_class.expose) {
		DEBUGOUT(_LtDebug(__FILE__, w,
			"XmRedisplayWidget: shell (%d children)\n", MGR_NumChildren(w)));
		(w->core.widget_class->core_class.expose) (w, (XEvent *)&e, r);
	} else {
		DEBUGOUT(_LtDebug(__FILE__, w, "XmRedisplayWidget(%d children)\n",
			MGR_NumChildren(w)));
		if (XtIsSubclass(w, compositeWidgetClass) || XmIsManager(w)) {
		    for (i=0; i<(int)MGR_NumChildren(w); i++) {
			Widget child = MGR_Children(w)[i];
			if (child->core.widget_class->core_class.expose) {
				DEBUGOUT(_LtDebug2(__FILE__, w, child, "XmRedisplayWidget\n"));
				child->core.widget_class->core_class.expose(child, (XEvent *)&e, r);
			} else {
				DEBUGOUT(_LtDebug2(__FILE__, w, child,
					"XmRedisplayWidget NULL method\n"));
			}
		    }
		}
	}

	XDestroyRegion(r);
}


/*
 * XmPrintPopupPDM sends a notification to start a print dialog manager
 * on behalf of the application.
 * XmPrintPopupPDM hides the details of the X selection mechanism used for this.
 * The environment variable XPDMDISPLAY, which can have values "print" or "video"
 * which of the widgets passed is used.
 * Possible return values : XmPDM_NOTIFY_FAIL, XmPDM_NOTIFY_SUCCESS.
 */
extern XtEnum
XmPrintPopupPDM(Widget print_shell,
                Widget video_transient_for)
{
#ifndef HAVE_LIB_XP
	_XmWarning(NULL, "XmPrintPopupPDM(): not available here!");
#else
	_XmWarning(NULL, "XmPrintPopupPDM(): not yet implemented!");
#endif
	return XmPDM_NOTIFY_FAIL;
}


/*
 * XmPrintSetup()
 *
 * Do the appropriate setting
 * Create a realised XmPrintShell (the return value),
 *	which is the child of an unrealized ApplicationShell.
 *
 * Goals :
 *	- hide details of Xt
 *	- encourage consistency in setting up print widget hierarchy
 *
 * Parameters :
 *	- video_widget is used to get at the application context etc
 *	- print_screen is a Screen belonging to a Display that's already
 *	  known to Xt.
 *	- print_shell_name is the name of a XmPrintShell to be created
 *	  by this function
 */
extern Widget
XmPrintSetup(Widget video_widget,
             Screen *print_screen,
             String print_shell_name,
             ArgList args,
             Cardinal num_args)
{
#ifndef HAVE_LIB_XP
	_XmWarning(NULL, "XmPrintSetup(): not available here!");
	return NULL;
#else
	XtAppContext	appc = XtWidgetToApplicationContext(video_widget);
	Widget		as, s;
	char		*appname, *classname;
	Arg		a;

	if (! print_screen) {
		_XmWarning(NULL, "XmPrintSetup: NULL print_screen");
		return NULL;
	}
	if (! video_widget) {
		_XmWarning(NULL, "XmPrintSetup: NULL video_widget");
		return NULL;
	}

	XtGetApplicationNameAndClass(XtDisplay(video_widget),
		&appname, &classname);

	XtSetArg(a, XmNscreen, print_screen);

	as = XtAppCreateShell(appname, classname,
		applicationShellWidgetClass,
		DisplayOfScreen(print_screen),
		&a, 1);

	s = XtCreatePopupShell(print_shell_name,
		xmPrintShellWidgetClass,
		as,
		args, num_args);

	if (!s) {
		_XmWarning(NULL, "XmPrintSetup: NULL XmPrintShell");
		return NULL;
	}

	XtSetMappedWhenManaged(s, False);
	XtRealizeWidget(s);

	return s;
#endif /* #ifndef HAVE_LIB_XP */
}


extern Boolean
XmIsPrintShell(Widget w)
{
#ifndef HAVE_LIB_XP
	_XmWarning(NULL, "XmPrintShell(): not available here!");
	return False;
#else
	if (XtIsSubclass(w, xmPrintShellWidgetClass))
		return True;
	else	
 		return False;
#endif
}


/*
 * XmPrintToFile and its internal functions and structures
 *
 * Beware : there are OS dependencies here.
 *
 * The reason for this is simple: the XpGetDocumentData() call
 *	requires a separate process (says the documentation) or thread
 *	(not so sure if this is true), and a separate display connection.
 * This alone is reason for OS-dependency - creating a thread or process -
 *	but it is not all. The child process needs to talk to its parent
 *	to verify whether the parent is still alive (the application might
 *	be unstable, we don't want this to cause dangling processes).
 *	OpenMotif appears less than perfect in this area - it sometimes
 *	leaves processes around.
 * A second reason for OS dependency is the FinishProc. The child process
 *	needs to send a signal (not in the UNIX sense !) to the parent
 *	to tell it that printing has ended.
 *	I've decided that the good old UNIX pipe is probably an efficient
 *	way to handle this, unfortunately it is OS dependent.
 * Also there appears to be a synchronisation issue between the processes.
 *	One of the tests doesn't consistently work.
 */

#ifdef HAVE_LIB_XP
typedef struct _XmPrintToFileData {
	char		*file_name;
	int		fd;
	int		pipe;		/* Synchronise parent/child */
	Display		*dpy;
	XPFinishProc	finish;
	XtPointer	client;
	XPContext	ctxt;
} _XmPrintToFileData;


/*
 * Child process : save chunks of data into the file.
 */
static void
_XmPrintToFileSaveProc(Display *dpy,
		XPContext cts,
		unsigned char *data,
		unsigned int data_len,
		XPointer client)
{
	_XmPrintToFileData	*p = (_XmPrintToFileData *)client;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmPrintToFileSaveProc(%d bytes)\n", data_len));

	write(p->fd, data, data_len);
}


/*
 * Child process : the print job has ended.
 * Close the file, tell the parent what happened, and die.
 */
static void
_XmPrintToFileChildFinishProc(Display *dpy,
		XPContext ctx,
		XPGetDocStatus status,
		XPointer client)
{
	_XmPrintToFileData	*p = (_XmPrintToFileData *)client;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmPrintToFileChildFinishProc()\n"));

	/* Close the file */
	close(p->fd);

	if (status != XPGetDocFinished) {
		/* Need to unlink the file */
		remove(p->file_name);
	}

	/* Send status information to the parent */
	write(p->pipe, &status, sizeof(status));
	close(p->pipe);

	/* The job's done, just die */
	XtFree((char *)p);
	_exit(0);
}


/*
 * Parent process : grab the input from the child process, call the
 *	application's finish proc if there's one.
 */
static void
_XmPrintToFileParentFinish(XtPointer client, int *fd, XtInputId *id)
{
	_XmPrintToFileData	*p = (_XmPrintToFileData *)client;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmPrintToFileParentFinish()\n"));

	if (p && p->finish)
		(p->finish)(p->dpy, p->ctxt, XPGetDocFinished, (XPointer)p->client);
	XtFree((char *)p);
}


/*
 * Child process : delayed death.
 */
static void
_XmPrintToFileChildDelayedDeath(XtPointer client, XtIntervalId *id)
{
	_exit(0);
}


/*
 * Child process : detect parent's death.
 */
static void
_XmPrintToFileChildPipeHandler(XtPointer client, int *fd, XtInputId *id)
{
	_XmPrintToFileData	*p = (_XmPrintToFileData *)client;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmPrintToFileChildPipeHandler()\n"));

	/* Maybe we should wait for a while before dying */
	XtAppAddTimeOut(XtDisplayToApplicationContext(p->dpy),
		10000L,
		_XmPrintToFileChildDelayedDeath,
		(XtPointer)0);
}


/*
 * XmPrintToFile retrieves the data being sent by a Print Server and prints it
 * to a file on the client side.
 *
 * If you care to R the FM, it says (in italics so it must make a difference)
 * that XpGetDocumentData needs to act on a *different* display connection
 * than its peer.
 *
 * Also it says that you need a different address space - you need to be
 * in another process - for this to work. I am not entirely sure that the
 * documentation's comment about threads is correct. On UNIX systems,
 * threads share address spaces.
 */
extern XtEnum
XmPrintToFile(Display *dpy, String filename, XPFinishProc finish_proc, XtPointer client)
{
	int			fd;
	Status			st;
	_XmPrintToFileData	*p;
	char			*ds, *name, *appclass;
	XtAppContext		appc;
	int			argc = 0;
	String			argv[] = { NULL };
	int			child;
	int			pfd[2];	/* pipe */

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmPrintToFile()\n"));

	if (!filename)
		return False;
	if ((fd = open(filename, O_RDWR)) < 0)
		return False;

	/* Both for parent and child */
	p = (_XmPrintToFileData *)XtMalloc(sizeof(_XmPrintToFileData));

	ds = XDisplayString(dpy);
	XtGetApplicationNameAndClass(dpy, &name, &appclass);
	p->ctxt = XpGetContext(dpy);
	p->file_name = filename;
	p->finish = finish_proc;
	p->client = client;
	p->fd = fd;

#ifdef UNIXLIKE
	/* OS dependency : create a pipe */
	if (pipe(pfd) < 0) {
		return False;
	}

	/* OS dependency as described above : create another process */
	if ((child = fork()) < 0) {
		/* Fork failed */
		/* we should clean up the pipe here?! */
		return False;
	}

	if (child>0) {	/* In parent */
		DEBUGOUT(_LtDebug(__FILE__, NULL, "XmPrintToFile() parent %d return\n",
		                       getpid()));
		close(fd);
		p->fd = -1;

		close(pfd[1]);	/* Only want to read from it */
		p->pipe = pfd[0];
		p->finish = finish_proc;
		p->client = client;

		XtAppAddInput(XtDisplayToApplicationContext(dpy),
			p->pipe,
			(XtPointer) (XtInputReadMask | XtInputExceptMask),
			_XmPrintToFileParentFinish,
			p);

		return True;
	}
        else {  /* In child */

	/*
	 * FIX ME
	 *
	 * I need to find a way to
	 *	1. keep the reference to the FinishProc
	 *	2. get some signal from child to parent to trigger FinishProc
	 */
		close(pfd[0]);
		p->pipe = pfd[1];

		/* I would expect closing the display in the child is a good thing,
		 * however it causes "Broken pipe" error messages. */
#if 0
		XtCloseDisplay(dpy);
#endif

		appc = XtCreateApplicationContext();
		p->dpy = XtOpenDisplay(appc, ds, name, appclass, NULL, 0, &argc, argv);

		DEBUGOUT(_LtDebug(__FILE__, NULL, "XpGetDocumentData(pid %d)\n", getpid()));
		st = XpGetDocumentData(p->dpy, p->ctxt,
			_XmPrintToFileSaveProc,
			_XmPrintToFileChildFinishProc, (XPointer)p);
	
		XtAppAddInput(XtDisplayToApplicationContext(dpy),
			p->pipe,
			(XtPointer) XtInputExceptMask,
			_XmPrintToFileChildPipeHandler,
			p);

		XtAppMainLoop(appc);
		_exit(0);
	}
#else
	/* Non-UNIX case: currently just fail */
	return False;
#warning "You are not on an unix-like platform"
#endif /* !UNIXLIKE */
}
#else /* ifdef HAVE_LIB_XP */

#if 1
extern XtEnum
XmPrintToFile(Display		*dpy,
              String		filename,
              /*XPFinishProc*/ XtPointer	finish_proc,
              XtPointer		client)
{
	_XmWarning(NULL, "XmPrintToFile(): not available here!");
	return False;
}
#endif
#endif /* #ifdef HAVE_LIB_XP */
