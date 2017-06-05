/**
 * This program demostrates two bugs in LessTif's XmScale widget:
 *
 *   (1) Setting XmNwidth doesn't have any effect.  (However,
 *         XmscaleWidth works)
 *
 *   (2) The first time the window is drawn the first label gadget is missing.
 *                       (However, it shows up after a refresh.)
 *  
 * May 1999
 * Rick Niles <frederick.a.niles.1@gsfc.nasa.gov>
 **/

#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/LabelG.h>
#include <Xm/Label.h>
#include <Xm/Scale.h>

int 
main(int argc, char *argv[])
{
  Widget top_shell;
  Widget footer_panel;
  Widget localForm, localLabel;
  Widget rateFrame, rateSlider;
  XtAppContext app;
  XmString xm_string;

  top_shell = XtVaAppInitialize(&app, "XScaleTest",  NULL,
                                0, &argc, argv, NULL,NULL);
  
  XtVaSetValues(top_shell,XmNtitle,"Broken Lesstif Scale Test",NULL);
  
  footer_panel = XtVaCreateManagedWidget("footer_panel",
                                         xmFormWidgetClass,
                                         top_shell,
                                         NULL);

  rateFrame = XtVaCreateManagedWidget("rateFrame",
                                      xmFrameWidgetClass,
                                      footer_panel,
                                      XmNtopAttachment,   XmATTACH_FORM,
                                      XmNtopOffset,       10,
                                      XmNleftAttachment,  XmATTACH_FORM,
                                      XmNleftOffset,      25,
                                      NULL);
  
  localForm  = XtVaCreateManagedWidget("localForm",
                                       xmFormWidgetClass,
                                       rateFrame,
                                       NULL);
  
  xm_string = XmStringCreateSimple("Where is the left '1' value:");
  localLabel = XtVaCreateManagedWidget("localLabel",
                                       xmLabelWidgetClass,
                                       localForm,
                                       XmNtopAttachment,   XmATTACH_FORM,
                                       XmNleftAttachment,  XmATTACH_FORM,
                                       XmNleftOffset,      5,
                                       XmNtopOffset,       10,
                                       XmNlabelString,     xm_string,
                                       NULL);
  
  rateSlider  = XtVaCreateManagedWidget("rateSlider",
                                        xmScaleWidgetClass,
                                        localForm,
                                        XmNorientation, XmHORIZONTAL,
                                        XmNmaximum, 2250,
                                        XmNminimum,  1,
                                        XmNscaleMultiple, 1,
                                        XmNshowValue,   True,
/*   It will work "correctly" if we set scaleWidth, but not with just width */
/*                                      XmNscaleWidth,   210, */  
                                        XmNwidth,   210,
                                        XmNtopAttachment,   XmATTACH_FORM,
                                        XmNleftAttachment,  XmATTACH_WIDGET,
                                        XmNleftWidget,      localLabel,
                                        XmNleftOffset,      5,
                                        XmNvalue,           970,
                                        NULL);

  /* This next line with "1" fails to appear on first drawing of window. */
  /* after an X windows refresh it will suddenly appear! */
  XtVaCreateManagedWidget("1", xmLabelGadgetClass, rateSlider, NULL);
  XtVaCreateManagedWidget("|", xmLabelGadgetClass, rateSlider, NULL);
  XtVaCreateManagedWidget("2250", xmLabelGadgetClass, rateSlider, NULL);

  XtRealizeWidget(top_shell);
 {
    static XtWidgetGeometry Expected[] = {
	CWWidth | CWHeight 			  ,	 4   , 896 , 421 , 66, 0,0,0, /*   footer_panel */
	CWWidth | CWHeight | CWX | CWY,	 25  , 10  , 396 , 56, 0,0,0, /*      rateFrame */
	CWWidth | CWHeight | CWX | CWY,	 2   , 2   , 392 , 52, 0,0,0, /*      localForm */
	CWWidth | CWHeight | CWX | CWY,	 5   , 10  , 172 , 17, 0,0,0, /*     localLabel */
	CWWidth | CWHeight | CWX | CWY,	 182 , 0   , 210 , 52, 0,0,0, /*     rateSlider */
	CWWidth | CWHeight | CWX | CWY,	 0   , 33  , 210 , 19, 0,0,0, /*      Scrollbar */
	CWWidth | CWHeight | CWX | CWY,	 14  , 0   , 10  , 17, 0,0,0, /*             1 */
	CWWidth | CWHeight | CWX | CWY,	 100 , 0   , 10  , 17, 0,0,0, /*              | */
	CWWidth | CWHeight | CWX | CWY,	 177 , 0   , 28  , 17, 0,0,0, /*           2250 */
    };
    PrintDetails(top_shell,Expected);
 }; 
  LessTifTestMainLoop(top_shell);
  
  return 0;
}
