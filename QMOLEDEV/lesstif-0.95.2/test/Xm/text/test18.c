/* $Header: /cvsroot/lesstif/lesstif/test/Xm/text/test18.c,v 1.3 2001/06/18 14:30:00 amai Exp $
/* taken from:
 *  http://techpubs.sgi.com:80/library/dynaweb_bin/ebt-bin/0650/nph-infosrch.cgi/infosrchtpl/SGI_Developer/Motif_Port_G/@InfoSearch__BookTextView/1094;he=0?DwebQuery=motif
 */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>

 
#define APPNAME "Vertical"
 
XtAppContext AppContext;
Widget TopLevel;

int 
main(int argc, char ** argv)
{ 
#if XmVERSION > 1
    Widget vert;
#endif
    Widget form, horiz;
     
    XtSetLanguageProc(NULL, NULL, NULL);
     
    TopLevel = XtVaOpenApplication(&AppContext, 
                     APPNAME, NULL, 0, 
                     &argc, argv, 
                     NULL, 
                     sessionShellWidgetClass, 
                     NULL);
 
    form = XtVaCreateWidget("form", xmFormWidgetClass, TopLevel, NULL);
 
/* vertical text widget */    

#if XmVERSION > 1
    vert = XtVaCreateManagedWidget("vertical", 
                     xmTextWidgetClass, form,
                     XmNtopAttachment, XmATTACH_FORM,
                     XmNleftAttachment, XmATTACH_FORM,
                     XmNbottomAttachment, XmATTACH_FORM,
                     XmNlayoutDirection, 
                     XmTOP_TO_BOTTOM,
                     XmNrows, 10, NULL);    
#endif
 
/* horizontal text widget */    
 
    horiz = XtVaCreateManagedWidget("horizontal",
                     xmTextWidgetClass, form,
                     XmNtopAttachment, XmATTACH_FORM,
#if XmVERSION > 1
                     XmNleftAttachment, 
                     XmATTACH_WIDGET,
                     XmNleftWidget, vert,
#else
                     XmNleftAttachment, 
                     XmATTACH_FORM,
#endif
                     XmNrightAttachment, XmATTACH_FORM,
                     NULL);    
  
/* manage form */    
    XtManageChild(form);
  
    XtRealizeWidget(TopLevel);
  
    LessTifTestMainLoop(TopLevel);
    exit(0);
}
