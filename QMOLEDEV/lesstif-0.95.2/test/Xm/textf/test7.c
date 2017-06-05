/*
From:        "Jon A. Christopher" <jon@quorum.tamu.edu>
To:          Lesstif Mailing List <lesstif@Hungry.COM>
Subject:     Another problem with TextFields
Date:        Tue, 31 Mar 1998 20:14:34 -0600 (CST)
*/

#include <stdio.h>
#include <ctype.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>

void focusCB( Widget w, XtPointer client, XmAnyCallbackStruct *call)
{
  XmTextFieldSetSelection(w,0,XmTextFieldGetLastPosition(w),
			  CurrentTime);
}


void upper(Widget w, XtPointer client, XtPointer call)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call;
  int len;

  if (cbs->text->ptr) {

    for (len=0; len<cbs->text->length; len++) {
      printf("upper: pos=%d before=%c ",len,cbs->text->ptr[len]);
      if (islower(cbs->text->ptr[len]))
        cbs->text->ptr[len]=toupper(cbs->text->ptr[len]);
      printf("after=%c\n",cbs->text->ptr[len]);
    }
    
  }
}

void cursor(Widget w, XtPointer client, XtPointer call)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)call;
  
  printf("cursor: cur=%d new=%d\n",(int)cbs->currInsert,(int)cbs->newInsert);
  if (cbs->currInsert<cbs->newInsert) cbs->doit=False;
}

void activate(Widget w, XtPointer client, XtPointer call)
{
  char *name,*value;

  name=(char *)client;
  value=XmTextFieldGetString(w);
  printf("activate %s: value=%s\n",name,value);
  XtFree(value);
}

char *labels[]={
  "Flux capacitance",
  "Photon torpedo inventory",
  "Warp factor",
  "Dilithium level",
  "Tardis color",
};

char *responses[]={
  "High",
  "Full",
  "9.8",
  "falling",
  "red",
};

int
main(int argc, char **argv)
{
  Widget toplevel, rc, tf, w;
  XtAppContext app;
  int i;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app,"Label",NULL,0,&argc,argv,NULL,NULL);

  rc=XtVaCreateManagedWidget("rowcol",xmRowColumnWidgetClass,toplevel,
    XmNpacking,XmPACK_COLUMN,
    XmNnumColumns,XtNumber(labels),
    XmNorientation,XmHORIZONTAL,
    XmNisAligned,True,
    XmNentryAlignment,XmALIGNMENT_END,
    NULL);

  for (i=0; i<XtNumber(labels); i++) {
    char name[16];

    w = XtVaCreateManagedWidget(labels[i],xmLabelWidgetClass,rc, 
      NULL);
    sprintf(name,"tf_%d",i);
    tf = XtVaCreateManagedWidget(name,xmTextFieldWidgetClass,rc, 
      XmNcolumns, 20, NULL);
    XmTextFieldSetString(tf,responses[i]);
    XmTextFieldSetSelection(tf,0,strlen(responses[i]),
			    XtLastTimestampProcessed(XtDisplay(tf)));

    XtAddCallback(tf,XmNfocusCallback,(XtCallbackProc)focusCB,NULL);
    XtAddCallback(tf,XmNactivateCallback,activate,labels[i]);
  }
  
  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  305,  173, 0,0,0, /* rowcol */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  148,   31, 0,0,0, /* Flux capacitance */
   CWWidth | CWHeight | CWX | CWY,  154,    3,  148,   31, 0,0,0, /* tf_0 */
   CWWidth | CWHeight | CWX | CWY,    3,   37,  148,   31, 0,0,0, /* Photon torpedo inventory */
   CWWidth | CWHeight | CWX | CWY,  154,   37,  148,   31, 0,0,0, /* tf_1 */
   CWWidth | CWHeight | CWX | CWY,    3,   71,  148,   31, 0,0,0, /* Warp factor */
   CWWidth | CWHeight | CWX | CWY,  154,   71,  148,   31, 0,0,0, /* tf_2 */
   CWWidth | CWHeight | CWX | CWY,    3,  105,  148,   31, 0,0,0, /* Dilithium level */
   CWWidth | CWHeight | CWX | CWY,  154,  105,  148,   31, 0,0,0, /* tf_3 */
   CWWidth | CWHeight | CWX | CWY,    3,  139,  148,   31, 0,0,0, /* Tardis color */
   CWWidth | CWHeight | CWX | CWY,  154,  139,  148,   31, 0,0,0, /* tf_4 */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
