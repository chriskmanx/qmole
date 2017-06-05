/*
 *  $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test4.c,v 1.13 2002/05/01 15:39:21 amai Exp $
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/FileSB.h>
#include <Xm/List.h>
#include <Xm/ScrollBar.h>
#include <Xm/SelectioB.h>
#include <Xm/TextF.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>

#include "../../common/Test.h"

#include "mkdirtree.h"

extern int GlobalErrors;
extern Boolean aardvarkConverter ();
extern Boolean objStringConverter ();
Widget appshell = (Widget) NULL;
Widget fsb = (Widget) NULL;
Widget fsb_text = (Widget) NULL;
Widget fsb_filter_text = (Widget) NULL;



XmSearchProc default_search_proc;
extern void file_search();

static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

void
set_search_proc(Widget fsb)
{
    Arg al[2];
    int ac;
    XmString dir_mask;

    /* Get and save pointer to default fileSearchProc */
    ac = 0;
    XtSetArg (al[ac], XmNfileSearchProc, &default_search_proc); ac++;
    XtSetArg (al[ac], XmNdirMask, &dir_mask); ac++;
    XtGetValues (fsb, al, ac);
    /* Change fileSearchProc and refresh files list (which will have
     been initialised using the default fileSearchProc when the
     XmFileSelectionBox was created) */
    ac = 0;
    XtSetArg (al[ac], XmNfileSearchProc, file_search); ac++;
    XtSetValues (fsb, al, ac);
    XmFileSelectionDoSearch (fsb, dir_mask);
}

void
file_search(
	XmFileSelectionBoxWidget fs,
	XmFileSelectionBoxCallbackStruct *search_data)
{
	char           *filename;
	XmString       *default_files;
	int             default_file_count;
	XmString       *writable_files = NULL;
	int             writable_file_count = 0;
	int             i;
	Arg             args[10];
	int             ac = 0;
	/*
	 * Use default file search proc to do all the wildcard matching and
	 * so on, then remove unwritable files from its list. Even if the
	 * default proc doesn't change the file list, we still need to check
	 * in case permissions have changed.
	 */
	(*default_search_proc) ((Widget)fs, search_data);

	/* Get the list of filenames */
	XtVaGetValues((Widget)fs,
		      XmNfileListItems, &default_files,
		      XmNfileListItemCount, &default_file_count,
		      NULL);
	/* Run down the list, copying the names of all writable files */
	writable_files = (XmString *) XtMalloc(default_file_count *
					       sizeof(XmString));
	for (i = 0; i < default_file_count; i++) {
		/* T. Straumann: make sure the filename is found */
		if ( ! XmStringGetLtoR(default_files[i], XmFONTLIST_DEFAULT_TAG,
				&filename) )
		{
			printf("unable to get filename; strings corrupted?\n");
			GlobalErrors++;
		}
		if (access(filename, W_OK) == 0)
			writable_files[writable_file_count++] =
				XmStringCopy(default_files[i]);
		XtFree(filename);
	}

	XtSetArg(args[ac], XmNfileListItems, writable_files);
	ac++;
	XtSetArg(args[ac], XmNfileListItemCount, writable_file_count);
	ac++;
	XtSetArg(args[ac], XmNlistUpdated, True);
	ac++;
	XtSetValues((Widget)fs, args, ac);

	for (i = 0; i < writable_file_count; i++)
		XmStringFree(writable_files[i]);
	XtFree((char *)writable_files);
}

void create_appshell(Display *display, char *app_name,
                     int app_argc, char **app_argv)
{
	Arg al[64];                    /* Arg List */
	register int ac = 0;           /* Arg Count */

	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNtitle, "File Selection Box - Writable Files List"); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	appshell = XtAppCreateShell ( app_name, "XApplication", applicationShellWidgetClass, display, al, ac );
	ac = 0;
	fsb = XmCreateFileSelectionBox ( appshell, "fsb", al, ac );
	fsb_text = XmSelectionBoxGetChild ( fsb, XmDIALOG_TEXT );
	fsb_filter_text = XmFileSelectionBoxGetChild ( fsb, XmDIALOG_FILTER_TEXT );

    set_search_proc(fsb);
	set_path(fsb);
	XtManageChild ( fsb);
}



XtAppContext app_context;
Display *display;       /*  Display             */

int main (int    argc, char **argv)
{
  make_tmp_dir_tree();
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
	create_appshell ( display, argv[0], argc, argv );
	XtRealizeWidget (appshell);

  
    
{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,    0,    0,  278,  372, 0,0,0, /* fsb */},
   {CWWidth | CWHeight | CWX | CWY,  190,   69,   77,   17, 0,0,0, /* Items */},
   {CWWidth | CWHeight | CWX | CWY,  190,   86,   77,  154, 0,0,0, /* ItemsListSW */},
   {CWWidth | CWHeight | CWX | CWY,   62,    0,   15,  135, 0,0,0, /* VertScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,  139,   58,   15, 0,0,0, /* HorScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   58,  135, 0,0,0, /* ItemsList */},
   {CWWidth | CWHeight | CWX | CWY,   11,  250,  256,   17, 0,0,0, /* Selection */},
   {CWWidth | CWHeight | CWX | CWY,   11,  267,  256,   31, 0,0,0, /* Text */},
   {CWWidth | CWHeight | CWX | CWY,    0,  308,  278,    2, 0,0,0, /* Separator */},
   {CWWidth | CWHeight | CWX | CWY,   11,  320,   64,   41, 0,0,0, /* OK */},
   {CWWidth | CWHeight | CWX | CWY,   75,  320,   64,   41, 0,0,0, /* Apply */},
   {CWWidth | CWHeight | CWX | CWY,  139,  320,   64,   41, 0,0,0, /* Cancel */},
   {CWWidth | CWHeight | CWX | CWY,  203,  320,   64,   41, 0,0,0, /* Help */},
   {CWWidth | CWHeight | CWX | CWY,   11,   11,  256,   17, 0,0,0, /* FilterLabel */},
   {CWWidth | CWHeight | CWX | CWY,   11,   69,  169,   17, 0,0,0, /* Dir */},
   {CWWidth | CWHeight | CWX | CWY,   11,   28,  256,   31, 0,0,0, /* FilterText */},
   {CWWidth | CWHeight | CWX | CWY,   11,   86,  169,  154, 0,0,0, /* DirListSW */},
   {CWWidth | CWHeight | CWX | CWY,  154,    0,   15,  135, 0,0,0, /* VertScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,  139,  150,   15, 0,0,0, /* HorScrollBar */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,  150,  135, 0,0,0, /* DirList */},
};
#else
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  286,  374, 0,0,0, /* fsb */
   CWWidth | CWHeight | CWX | CWY,  198,   69,   77,   17, 0,0,0, /* Items */
   CWWidth | CWHeight | CWX | CWY,  198,   86,   77,  154, 0,0,0, /* ItemsListSW */
   CWWidth | CWHeight | CWX | CWY,   62,    0,   15,  135, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  139,   58,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   58,  135, 0,0,0, /* ItemsList */
   CWWidth | CWHeight | CWX | CWY,   11,  250,  264,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   11,  267,  264,   31, 0,0,0, /* Text */
   CWWidth | CWHeight | CWX | CWY,    0,  308,  286,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,  320,   66,   43, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   77,  320,   66,   43, 0,0,0, /* Apply */
   CWWidth | CWHeight | CWX | CWY,  143,  320,   66,   43, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  209,  320,   66,   43, 0,0,0, /* Help */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  264,   17, 0,0,0, /* FilterLabel */
   CWWidth | CWHeight | CWX | CWY,   11,   69,  177,   17, 0,0,0, /* Dir */
   CWWidth | CWHeight | CWX | CWY,   11,   28,  264,   31, 0,0,0, /* FilterText */
   CWWidth | CWHeight | CWX | CWY,   11,   86,  177,  154, 0,0,0, /* DirListSW */
   CWWidth | CWHeight | CWX | CWY,  162,    0,   15,  135, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  139,  158,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  158,  135, 0,0,0, /* DirList */ 
};
#endif
    LessTifTestSetSlop(appshell, 1);
    PrintDetails(appshell,Expected);
};
  LessTifTestMainLoop(appshell);

	exit (0);
}
