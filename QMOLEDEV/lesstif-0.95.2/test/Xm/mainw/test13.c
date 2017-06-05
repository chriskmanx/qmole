/* $Id: test13.c,v 1.8 2001/05/15 14:08:33 amai Exp $ */
/*
From:        "Dr. Peer Griebel" <griebel@rocketmail.com>
- shows a sizing problem when the top window is resized
- the CascadeButton in the pulldown menu shows it's graphic even though it
  does not have a sub-menu
*/
/*
** BuildMain.c ( Motifation generated )
**
** Motifation@allow-overwrite@
** If you want to protect this file to be overwritten by Motifation
** modify the line above.
**
*/
#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <X11/Shell.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>

#include "../../common/Test.h"


static Widget appshell = NULL;
static Widget pgHelpMainWindow = NULL;
static Widget HelpBoard = NULL;
static Widget mainbull = NULL;

static Widget menuFile = NULL;
static Widget menuOpen = NULL;
static Widget tInfo = NULL;

static Display		*display;
static XtAppContext	application_context;
static int		quad_width;

void Resize(void)

{
    Arg            args[10];
    Cardinal      argcount;
    Widget Label;
    
    if( HelpBoard ) {
	XtDestroyWidget( HelpBoard );
	XmUpdateDisplay( mainbull );
	printf( "Resize! Helpboard destroyed!\n" );
    }

    argcount = 0;
    XtSetArg( args[argcount], XmNresizePolicy, XmRESIZE_ANY ); argcount++;
    XtSetArg( args[argcount], XmNunitType, XmPIXELS ); argcount++;

    HelpBoard = XmCreateBulletinBoard( mainbull, "Board2", args, argcount );
    XtRealizeWidget( HelpBoard );
    XtManageChild( HelpBoard );

    printf( "Resize! XtWidth: %d\n", XtWidth( pgHelpMainWindow ) );

    argcount = 0;
    XtSetArg( args[argcount], XmNx, XtWidth( pgHelpMainWindow ) -50 ); argcount++;
    XtSetArg( args[argcount], XmNy, 50 ); argcount++;
    Label = XmCreateLabel( mainbull, "xxx", args, argcount );
    XtManageChild( Label );
}


/**********************************************************************
**
**
*/
void BuildMainApplication( display )
     Display *display;
{
    Arg            args[10];
    Cardinal      argcount;

    Widget menubar = NULL;
    Widget pulldown = NULL;
   
    argcount = 0;
    XtSetArg( args[argcount], XmNallowShellResize, True ); argcount++;
    XtSetArg( args[argcount], XmNtitle, "XpgHelp" ); argcount++;
    XtSetArg( args[argcount], XmNiconName, "XpgHelp" ); argcount++;
    appshell = XtAppCreateShell( NULL, "xpghelp", applicationShellWidgetClass,
				 display, args, argcount );
   
    /*
    ** Codegeneration for the Widget pgHelpMainWindow
    */
    argcount = 0;
    XtSetArg( args[argcount], XmNscrollingPolicy, XmAUTOMATIC ); argcount++;
    XtSetArg( args[argcount], XmNscrollBarDisplayPolicy, XmSTATIC ); argcount++;
    XtSetArg( args[argcount], XmNvisualPolicy, XmVARIABLE ); argcount++;
    XtSetArg( args[argcount], XmNspacing, (short) 1 ); argcount++;
    pgHelpMainWindow = XmCreateMainWindow( appshell, "pgHelpMainWindow", args,
					   argcount );
    XtAddEventHandler( pgHelpMainWindow, StructureNotifyMask, False,
                       (XtEventHandler)Resize, (XtPointer) NULL ); /* help.c */
    XtManageChild( pgHelpMainWindow );
   
    /*
    ** Codegeneration for the Widget menubar
    */
    argcount = 0;
    menubar = XmCreateMenuBar( pgHelpMainWindow, "menubar", args, argcount );
    XtManageChild( menubar );
    
    /*
    ** Codegeneration for the Widget pulldown
    */
    argcount = 0;
    pulldown = XmCreatePulldownMenu( menubar, "pulldown", args, argcount );
   
   
    /*
    ** Codegeneration for the Widget menuOpen
    */
    argcount = 0;
    menuOpen = XmCreateCascadeButton( pulldown, "menuOpen", args, argcount );
    XtManageChild( menuOpen );
    
    /*
    ** Codegeneration for the Widget menuFile
    */
    argcount = 0;
    XtSetArg( args[argcount], XmNsubMenuId, pulldown ); argcount++;
    menuFile = XmCreateCascadeButton( menubar, "menuFile", args, argcount );
    XtManageChild( menuFile );
   
    /*
    ** Codegeneration for the Widget mainbull
    */
    argcount = 0;
    XtSetArg( args[argcount], XmNmarginHeight, (short) 6 ); argcount++;
    XtSetArg( args[argcount], XmNmarginWidth, (short) 6 ); argcount++;
    XtSetArg( args[argcount], XmNheight, 60 ); argcount++;
    XtSetArg( args[argcount], XmNwidth, 60 ); argcount++;
    mainbull = XmCreateBulletinBoard( pgHelpMainWindow, "canvas", args, argcount );
    XtManageChild( mainbull );
    
    /*
    ** Codegeneration for the Widget tInfo
    */
    argcount = 0;
    tInfo = XmCreateText( pgHelpMainWindow, "tInfo", args, argcount );
    XtManageChild( tInfo );
    
    argcount = 0;
    XtSetArg( args[argcount], XmNmessageWindow, tInfo ); argcount++;
    XtSetArg( args[argcount], XmNworkWindow, mainbull ); argcount++;
    XtSetValues( pgHelpMainWindow, args, argcount );
    
    XmMainWindowSetAreas( pgHelpMainWindow, menubar, NULL, NULL, NULL, mainbull );

    XtRealizeWidget( appshell );
    
    XmUpdateDisplay( appshell );
}


/**********************************************************************
**
**
*/
int main( argc, argv )
     int   argc;
     char *argv[];
{
    XrmDatabase database;
    XFontStruct *font_struct;
    XrmValue value;
    char *dummy;

    /*
    **	initialize Toolkit and set some global variable
    */
    XtToolkitInitialize();

    application_context = XtCreateApplicationContext();

    if( ( display = XtOpenDisplay( application_context, NULL, NULL, "Xpghelp", NULL, 0, &argc, argv)) == NULL ) {
	fprintf( stderr,"\n%s:  Can't open display\n", argv[0] );
	exit( 1 );
    }

    /*
    ** Get the QUAD_WIDTH
    */
    database = XtDatabase( display );
    if( XrmGetResource( database, XmNfontList, XmCFontList, &dummy, &value )
	&& (font_struct = XLoadQueryFont( display, value.addr )) ) {
	quad_width = font_struct->ascent+font_struct->descent;
	XFreeFont( display, font_struct );
    } else {
	if( XrmGetResource( database, XmNfont, XmCFont, &dummy, &value )
	    && (font_struct = XLoadQueryFont( display, value.addr )) ) {
	    quad_width = font_struct->ascent+font_struct->descent;
	    XFreeFont( display, font_struct );
	} else {
	    if( ( font_struct = XLoadQueryFont( display, "Fixed" ) ) ) {
		quad_width = font_struct->ascent+font_struct->descent;
		XFreeFont( display, font_struct );
	    } else {
		quad_width = 10;
	    }
	}
    }
    XmSetFontUnit( display, quad_width );

    BuildMainApplication( display );


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  185,  315,  138,  190, 0,0,0, /* pgHelpMainWindow */
   CWWidth | CWHeight | CWX | CWY,    4,   35,  110,  100, 0,0,0, /* ClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  116,   73, 0,0,0, /* canvas */
   CWWidth | CWHeight | CWX | CWY,    6,    6,    1,    1, 0,0,0, /* Board2 */
   CWWidth | CWHeight | CWX | CWY,   88,   50,   22,   17, 0,0,0, /* xxx */
   CWWidth | CWHeight | CWX | CWY,    0,  159,  138,   31, 0,0,0, /* tInfo */
   CWWidth | CWHeight | CWX | CWY,  119,   31,   19,  108, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,  140,  118,   19, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  138,   31, 0,0,0, /* menubar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   64,   21, 0,0,0, /* menuFile */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(appshell, Expected);
}
LessTifTestMainLoop(appshell);
    return( 0 );
}
