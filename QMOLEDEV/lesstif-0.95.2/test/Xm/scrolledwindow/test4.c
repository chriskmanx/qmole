/*
 * Sender: Ulrich.Raich@cern.ch
 * Message-Id: <319760A6.59E2@cern.ch>
 * Date: Mon, 13 May 1996 15:17:42 -0100
 * From: Ulrich Raich <Ulrich.Raich@cern.ch>
 * Organization: CERN. European Lab. for Particle Physics
 * To: Danny Backx <u27113@s852955.ackb.kb.be>
 * Subject: Re: Form Widget
 */ 
#include <Xm/Xm.h>
#include <Xm/ScrolledWP.h>
#include <Xm/Label.h>
#include <Xm/Form.h>

#define MAX_NO_OF_DIGITS        4
#define LABEL_MARGIN            4
#define DIGIT_DISTANCE          5
#define SCREEN_NUMBER           0

Widget         digit[4];

int
main(int argc, char **argv)
{

  XtAppContext   theApp;
  Widget         toplevel,main_window;
  Widget         form;
  Display        *display;
  Window         root_window;
  Arg args[5];
  XmString label_string;
  int i;

  toplevel = XtVaAppInitialize(&theApp, "ICTP_examples", NULL, 0,
                               &argc, argv, NULL, NULL);
  
  display     = XtDisplay(toplevel);
  root_window = DefaultRootWindow(display);


  XtSetArg(args[0],XmNshowSeparator,TRUE);
  main_window = XtCreateManagedWidget("main_window",xmScrolledWindowWidgetClass,
                                       toplevel,args,1);

  form = XtVaCreateManagedWidget("form",xmFormWidgetClass,
                                 main_window,
#if 1
                                 XmNwidth,400,
                                 XmNheight,100,
#endif
 NULL);

  XtSetArg(args[0],XmNworkWindow,form);
  XtSetValues(main_window,args,1);

  label_string = XmStringCreateLtoR("digit",XmSTRING_DEFAULT_CHARSET);

  digit[0]= XtVaCreateManagedWidget("digit", xmLabelWidgetClass,
                                form,
                                XmNtopAttachment,XmATTACH_FORM,      
                                XmNleftAttachment,XmATTACH_FORM,
                                NULL);

  for (i=1;i<4;i++) 
    digit[i]= XtVaCreateManagedWidget("digit", xmLabelWidgetClass,
                                form,
                                XmNtopAttachment,XmATTACH_FORM,      
                                XmNleftAttachment,XmATTACH_WIDGET,
                                XmNleftWidget,digit[i-1],      
                                NULL);
  XtRealizeWidget(toplevel);



  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,  400,  100, 0,0,0, /* main_window */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  400,  100, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   34,   17, 0,0,0, /* digit */
   CWWidth | CWHeight | CWX | CWY,   34,    0,   34,   17, 0,0,0, /* digit */
   CWWidth | CWHeight | CWX | CWY,   68,    0,   34,   17, 0,0,0, /* digit */
   CWWidth | CWHeight | CWX | CWY,  102,    0,   34,   17, 0,0,0, /* digit */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
