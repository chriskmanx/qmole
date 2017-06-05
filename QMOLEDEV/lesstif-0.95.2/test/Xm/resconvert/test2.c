/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/resconvert/test2.c,v 1.4 2002/04/13 12:05:29 amai Exp $
 * This test should demonstrate the crash which happens in xmstring/test8
 *    
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>


/* only try with LessTif <= 0.93.18 if at all */
#undef TRY_IT /* use LessTif XmI.h for Motif ... */

#if defined(LESSTIF_VERSION) || defined(TRY_IT)
#include <XmI/XmI.h>
#endif


static void create_shell(Display *display,
                  char *app_name,
                  int app_argc,
                  char **app_argv);
/* static void initialise_objects(Widget parent, XmFontList *f1); */

Widget shell = (Widget) NULL;
Widget form = (Widget) NULL;
Widget list1_sw = (Widget) NULL;
Widget list1 = (Widget) NULL;
Widget da = (Widget) NULL;

static  XmFontList f1;


static void
initialise_objects(Widget parent)
{
	const char from_s[] = "-*-fixed-*-*-*-*-24-*-*-*-*-*-*-*=japanese,-*-lucidabright-medium-i-*-*-24-*-*-*-*-*-*-*=large_italic,-*-lucidabright-demibold-r-*-*-24-*-*-*-*-*-*-*=large_bold,-*-lucidabright-demibold-i-*-*-14-*-*-*-*-*-*-*=small_italic,-*-lucidabright-demibold-r-*-*-14-*-*-*-*-*-*-*=small_bold";
	XrmValue from, to;
        static int _xd_initialised = 0;
	XmFontList newfl;

	if ( _xd_initialised ) return;
	_xd_initialised = 1;
	from.size = strlen(from_s)+1;
	from.addr = XtMalloc ( from.size );
	strcpy ( from.addr, from_s );
	to.addr=NULL;
	XtConvert( parent, XmRString, &from, XmRFontList, &to);
	XtFree ( from.addr );

        newfl = *((XmFontList*)to.addr);
#if defined(LESSTIF_VERSION) && defined(TRY_IT)
	if(newfl)
  		printf("tag0=%s\n", newfl[0].tag);
	else
		printf("No fontlist returned\n");
#endif
	f1 = XmFontListCopy(newfl);
}


static void
create_shell(Display *display,
             char *app_name,
             int app_argc,
             char **app_argv)
{
	Arg al[12];                 /* Arg List */
	int ac;       		    /* Arg Count */
	/* XmFontList f1; */

	ac = 0;
	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	shell = XtAppCreateShell ( app_name, "XApplication", applicationShellWidgetClass, display, al, ac );

	initialise_objects ( shell );
}


int
main (int argc, char **argv)
{

	XtAppContext app_context;
	Display *display;       /*  Display             */

	XtSetLanguageProc ( (XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL );
	XtToolkitInitialize ();
	app_context = XtCreateApplicationContext ();
	display = XtOpenDisplay (app_context, NULL, argv[0], "XApplication",
				 NULL, 0, &argc, argv);
	if (!display)
	{
	    printf("%s: can't open display, exiting...\n", argv[0]);
	    exit (-1);
	}
	create_shell ( display, argv[0], argc, argv );

	exit (0);
}
