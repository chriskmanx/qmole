/* $Header: /cvsroot/lesstif/lesstif/test/Xm/list/test16.c,v 1.6 2001/06/18 08:45:30 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>

#include "../../common/Test.h"

#define WIERD
/* #define WEIRDER */

#define SL_WIDTH        200

typedef struct DragInfo
{
  Pixmap                p;
  int                   numDragNames;
  char                  dirName[BUFSIZ], *dragName;
} DragInfo;

struct DirInfo
{
  Widget                list, form, label, nameLabel;
  DragInfo              di;
  struct FileList*      fileList;
  int                   nFiles;
  struct stat           s;
  struct DirInfo*	next;
};

struct FileManager
{
  Widget 		parent, mainForm, addAccelButton, topSW, topSWRC,
  			menubar, panedWindow, topForm, bottomForm, 
			devForm, devSW, devSWRC, transferAsciiTB, 
			transferBinaryTB;
  Widget		sw, clipWindow, sb, form;
  Boolean		runInXterm, isManaged;
  int			type, nDirInfo, nTotal, ht, transferType;
  struct DirInfo*	dirInfo;
  char			host[256], user[50], pass[50], *cwd, *cwdPtr;
  int			cwdIndex;
  XtInputId		cmdId;
  int			cmdSocketId;
  struct FtpDataConn*	ftpDataConn;
  struct FileManager*	next;
};

static XtTranslations parsed_resize_xlations = (XtTranslations)NULL;
static XtTranslations parsed_cmd_xlations = (XtTranslations)NULL;
static char resizeTranslations[] = "#augment <Configure> : resizeReq()";
static char cmdTranslations[] = "#override <Btn3Down>: cmdPopupReq()";


void addListObj(struct FileManager* f)
{
  struct DirInfo *di;
  Pixel fg, bg;
  Arg args[20];
  int n_args, i;
  Dimension ht;

  di =(struct DirInfo *)malloc(sizeof(struct DirInfo));
  di->fileList = (struct FileList *)NULL;
  f->nTotal++;

  XtVaSetValues( f->form,
                 XmNwidth, SL_WIDTH*f->nTotal,
                 NULL );

  if( f->isManaged )
    di->form = XtVaCreateWidget( "form",
                xmFormWidgetClass, f->form,
                XmNresizable, False,
                XmNwidth, SL_WIDTH,
                XmNresizePolicy, XmRESIZE_NONE,
                NULL );
  else
    di->form = XtVaCreateWidget( "form",
                xmFormWidgetClass, f->form,
                XmNresizable, False,
                XmNwidth, SL_WIDTH,
                NULL );

  n_args = 0;
  XtSetArg( args[n_args], XmNtraversalOn, False );  n_args++;
  XtSetArg( args[n_args], XmNshadowThickness, 2 );  n_args++;
  XtSetArg( args[n_args], XmNscrollBarDisplayPolicy, XmSTATIC );  n_args++;
  XtSetArg( args[n_args], XmNselectionPolicy, XmEXTENDED_SELECT );  n_args++;
  XtSetArg( args[n_args], XmNspacing, 0 );  n_args++;
  XtSetArg( args[n_args], XmNtopAttachment, XmATTACH_FORM );  n_args++;
  XtSetArg( args[n_args], XmNtopOffset, 100 );  n_args++;
  XtSetArg( args[n_args], XmNbottomAttachment, XmATTACH_FORM );  n_args++;
  XtSetArg( args[n_args], XmNbottomOffset, 0 );  n_args++;
  XtSetArg( args[n_args], XmNleftAttachment, XmATTACH_FORM );  n_args++;
  XtSetArg( args[n_args], XmNleftOffset, 0 );  n_args++;
  XtSetArg( args[n_args], XmNrightAttachment, XmATTACH_FORM );  n_args++;
  XtSetArg( args[n_args], XmNrightOffset, 0 );  n_args++;
  XtSetArg( args[n_args], XmNlistSizePolicy, XmCONSTANT );  n_args++;
  XtSetArg( args[n_args], XmNscrollBarPlacement, XmBOTTOM_LEFT );  n_args++;
  XtSetArg( args[n_args], XmNuserData, (XtPointer)di );  n_args++;
  di->list = XmCreateScrolledList( di->form, "dirSL", args, n_args );

  XtVaGetValues( di->form, XmNforeground, &fg, XmNbackground, &bg, NULL );

  di->label = XtVaCreateManagedWidget( "",
                xmPushButtonWidgetClass, di->form,
                XmNshadowThickness, 0,
                XmNresizable, False,
                XmNrecomputeSize, False,
                XmNlabelType, XmPIXMAP,
                XmNtopAttachment, XmATTACH_FORM,
                XmNbottomAttachment, XmATTACH_WIDGET,
                XmNbottomWidget, di->list,
                XmNbottomOffset, 25,
                XmNleftAttachment, XmATTACH_FORM,
                XmNrightAttachment, XmATTACH_FORM,
                XmNtraversalOn, False,
                XmNforeground, fg,
                XmNbackground, bg,
		XmNuserData, (XtPointer)di,
		XmNtranslations, parsed_cmd_xlations,
                NULL );

  di->nameLabel = XtVaCreateManagedWidget( "",
                xmLabelWidgetClass, di->form,
                XmNresizable, False,
                XmNrecomputeSize, False,
                XmNtopAttachment, XmATTACH_WIDGET,
                XmNtopWidget, di->label,
                XmNbottomAttachment, XmATTACH_WIDGET,
                XmNbottomWidget, di->list,
                XmNleftAttachment, XmATTACH_FORM,
                XmNrightAttachment, XmATTACH_FORM,
                XmNforeground, fg,
                XmNbackground, bg,
                NULL );

  XtManageChild( di->list );
  XtManageChild( di->form );

  XFlush( XtDisplay(f->form) );
  XSync( XtDisplay(f->form), False );

  if( f->isManaged )
  {
    XtVaGetValues( f->clipWindow, XmNheight, &ht, NULL );
    if( ht-2 != f->ht )
    {
      f->ht = ht - 2;
      XtVaSetValues( f->form, XmNheight, f->ht, NULL );
    }
  }
}

void createFileSelectionBox(Widget top, int type)
{
  struct FileManager fm;
  struct FileManager *f=&fm;
  Widget parent = f->bottomForm = top;
  static int firstTime = 1;
  int i, j;
  char dirBuffer[BUFSIZ], *tokptr, *strptr;

  /*  parse resize translations  */

  if( firstTime == 1 )
  {
    parsed_resize_xlations = XtParseTranslationTable( resizeTranslations );
    parsed_cmd_xlations = XtParseTranslationTable( cmdTranslations );
    firstTime = 0;
  }

  /*  create scrolled window and form as child  */

  f->ht = 0;
  f->isManaged = False;

  f->sw = XtVaCreateManagedWidget( "FSBsw",
                xmScrolledWindowWidgetClass, parent,
                XmNshadowThickness, 2,
                XmNtopAttachment, XmATTACH_FORM,
                XmNtopOffset, 5,
                XmNleftAttachment, XmATTACH_FORM,
                XmNleftOffset, 0,
                XmNrightAttachment, XmATTACH_FORM,
                XmNrightOffset, 0,
                XmNbottomAttachment, XmATTACH_FORM,
                XmNbottomOffset, 0,
                XmNvisualPolicy, XmCONSTANT,
                XmNscrollingPolicy, XmAUTOMATIC,
                XmNscrollVertical, False,
                XmNscrollHorizontal, True,
                XmNspacing, 0,
                XmNscrollBarPlacement, XmTOP_RIGHT,
		XmNtranslations, parsed_resize_xlations,
		XmNuserData, (XtPointer)f,
                NULL );

  f->form = XtVaCreateManagedWidget( "FSBform",
		xmRowColumnWidgetClass, f->sw,
                XmNorientation, XmHORIZONTAL,
                XmNpacking, XmPACK_COLUMN,
                XmNresizeHeight, False,
		XmNmarginHeight, 0,
                XmNspacing, 0,
		XmNuserData, (XtPointer)f,
                NULL );

  XtVaSetValues( f->sw, XmNworkWindow, f->form, NULL );
  XtVaGetValues( f->sw, XmNclipWindow, &f->clipWindow,
                 XmNhorizontalScrollBar, &f->sb, NULL );

  f->nDirInfo = 0;
  f->nTotal = 0;
#ifdef WEIRDER
  f->isManaged = True;
#endif

  addListObj(f);
  addListObj(f);
  addListObj(f);
#ifdef WEIRD
  f->isManaged = True;
#endif
}


int
main(int argc, char **argv)
{
  Widget toplevel, one, two;
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "mfm_sw_test", NULL, 0, &argc, argv,
			       NULL, NULL); 
  
  createFileSelectionBox(toplevel,0);
  XtRealizeWidget(toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  100,  100, 0,0,0, /* FSBsw */
   CWWidth | CWHeight | CWX | CWY,    4,   21,   92,   75, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  606,   16, 0,0,0, /* FSBform */
   CWWidth | CWHeight | CWX | CWY,    3,    0,  200,   16, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,  100,  200,    1, 0,0,0, /* dirSLSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   15,    2, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,   15,  -14,  185,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,   15,    0,  185,    2, 0,0,0, /* dirSL */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  200,   75, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,    0,   75,  200,   25, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,  203,    0,  200,   16, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,  100,  200,    1, 0,0,0, /* dirSLSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   15,    2, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,   15,  -14,  185,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,   15,    0,  185,    2, 0,0,0, /* dirSL */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  200,   75, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,    0,   75,  200,   25, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,  403,    0,  200,   16, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,  100,  200,    1, 0,0,0, /* dirSLSW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   15,    2, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,   15,  -14,  185,   15, 0,0,0, /* HorScrollBar */
   CWWidth | CWHeight | CWX | CWY,   15,    0,  185,    2, 0,0,0, /* dirSL */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  200,   75, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,    0,   75,  200,   25, 0,0,0, /*  */
   CWWidth | CWHeight | CWX | CWY,  100,    0,   19,  100, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  100,   19, 0,0,0, /* HorScrollBar */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

  exit(0);
}
