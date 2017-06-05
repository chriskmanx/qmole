#include <stdio.h>
#include <ctype.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>

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

void focus(Widget w, XtPointer client, XtPointer call)
{
  XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call;
  int len;
  char *text,*c;

  c=text=XmTextFieldGetString(w);
  len=strlen(text);
  
  if (cbs->reason==XmCR_FOCUS) {
    while (*c) {
      if (islower(*c)) *c=toupper(*c);
      c++;
    }
  }
  else {
    while (*c) {
      if (isupper(*c)) *c=tolower(*c);
      c++;
    }
  }
#ifdef DOESNT_WORK
  XmTextFieldSetString(w,text); /* calls modifyVerify in real Motif */
#else  
  XtVaSetValues(w,
    XtNvalue,text,
    NULL);
#endif
  XtFree(text);
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
  "Normal",
  "ModifyVerify",
  "MotionVerify",
  "Fixed Length",
  "wchar_t",
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

    if (i==1) {
      XtAddCallback(tf,XmNmodifyVerifyCallback,upper,NULL);
      XtAddCallback(tf,XmNfocusCallback,focus,NULL);
      XtAddCallback(tf,XmNlosingFocusCallback,focus,NULL);
    }
    else if (i==2) {
      XtAddCallback(tf,XmNmotionVerifyCallback,cursor,NULL);
    }
    else if (i==3) {
      XtVaSetValues(tf,
        XmNmaxLength,16,
        NULL);
    }
    
    XtAddCallback(tf,XmNactivateCallback,activate,labels[i]);
  }
  
  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  285,  173, 0,0,0, /* rowcol */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  138,   31, 0,0,0, /* Normal */
   CWWidth | CWHeight | CWX | CWY,  144,    3,  138,   31, 0,0,0, /* tf_0 */
   CWWidth | CWHeight | CWX | CWY,    3,   37,  138,   31, 0,0,0, /* ModifyVerify */
   CWWidth | CWHeight | CWX | CWY,  144,   37,  138,   31, 0,0,0, /* tf_1 */
   CWWidth | CWHeight | CWX | CWY,    3,   71,  138,   31, 0,0,0, /* MotionVerify */
   CWWidth | CWHeight | CWX | CWY,  144,   71,  138,   31, 0,0,0, /* tf_2 */
   CWWidth | CWHeight | CWX | CWY,    3,  105,  138,   31, 0,0,0, /* Fixed Length */
   CWWidth | CWHeight | CWX | CWY,  144,  105,  138,   31, 0,0,0, /* tf_3 */
   CWWidth | CWHeight | CWX | CWY,    3,  139,  138,   31, 0,0,0, /* wchar_t */
   CWWidth | CWHeight | CWX | CWY,  144,  139,  138,   31, 0,0,0, /* tf_4 */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
