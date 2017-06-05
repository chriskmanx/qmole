/* $Header: /cvsroot/lesstif/lesstif/test/Xm/label/test3.c,v 1.11 2002/05/01 15:39:21 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/ArrowB.h>
#include <Xm/LabelG.h>
#include <Xm/Label.h>
#include <Xm/PushBG.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/ToggleBG.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>

#include "../../common/Test.h"



#define SCREEN_NUMBER           0
#define MAX_SWITCH              8
#define SWITCH_MASK          0x80
#define MAX_LED                10

#define LED_WIDTH              10
#define LED_HEIGHT             20
#define LED_DISTANCE            5
#define LED_HEIGHT_OFFSET       5
#define MAX_ADC              4999           /* in milli Volt */

unsigned char     ICTP_IO_switch_state = 0;  

void exit_proc(Widget w,XtPointer client_data,XtPointer call_data);
void toggled_proc(Widget w,XtPointer client_data,XtPointer call_data);
void test_proc(Widget w,XtPointer client_data,XtPointer call_data);

Widget              led, lcd, off_switches[MAX_SWITCH],on_switches[MAX_SWITCH];
Widget              adc_slider;
Widget              test_switch_box[MAX_SWITCH];
Widget              on_off_box[MAX_SWITCH];
Widget              leds[MAX_LED];
Pixel               red_color_pixel;
Pixmap              led_off_pixmap,led_on_pixmap;

static char *FallBack[] = {
		"*.geometrySlop: 2",
		NULL
};

int
main(int argc, char **argv)
{

  int               i;
  char              number_string[5];
  XtAppContext      theApp;
  Widget            toplevel,main_window,form;
  Widget            menu_bar,file_button,file_menu,quit_button;
  Widget            lcd_label,lcd_frame;
  Widget            led_label;
  Widget            switch_label[MAX_SWITCH];
  Widget            switches_label,switch_box,switch_on_label,switch_off_label;
  Widget            led_frame,led_column;
  Widget            adc_label;
  Display           *display;
  Window            root_window;
  GC                graphics_context;
  XGCValues         values;
  unsigned long     value_mask;
  Font              label_font;
  XColor            closest_color,exact_color;
  Arg               args[5];   
  XmString          label_string,dummy_string;
   
  toplevel = XtVaAppInitialize(&theApp, "ICTP_examples", NULL, 0,
			       &argc, argv, FallBack, NULL);
  XtSetArg(args[0],XtNtitle,"Simulator for Prof. Ang\'s I/O board");  
  XtSetValues(toplevel,args,1);
  display     = XtDisplay(toplevel);
  root_window = DefaultRootWindow(display);

  label_font = XLoadFont(display,
			 "-adobe-courier-medium-o-normal--10-100-*-*-m-60-*-*");

  value_mask = GCForeground | GCBackground | GCFont;
  values.foreground = BlackPixel(display,SCREEN_NUMBER);
  values.background = WhitePixel(display,SCREEN_NUMBER);
  values.font = label_font;
  graphics_context = XCreateGC(display,root_window,value_mask,&values);

  XAllocNamedColor(display,DefaultColormap(display,SCREEN_NUMBER),"red",
                   &closest_color,&exact_color);
  red_color_pixel = closest_color.pixel;

  XtSetArg(args[0],XmNshowSeparator,TRUE);
  main_window = XtCreateManagedWidget("main_window",xmMainWindowWidgetClass,
                                       toplevel,args,1);

  form = XtVaCreateManagedWidget("form",xmFormWidgetClass,
                                 main_window,
                                 XmNwidth,180,NULL);
/*
  create pulldown menu
*/

  file_menu = XmCreatePulldownMenu(main_window,
                                   "file_menu",args,(Cardinal) 0);

  label_string = XmStringCreateLtoR("Quit",XmSTRING_DEFAULT_CHARSET);
  XtSetArg(args[0],XmNlabelString,label_string);
  quit_button = XtCreateManagedWidget("quit_button",xmPushButtonWidgetClass,
				       file_menu,args,1);
  XtAddCallback(quit_button,XmNactivateCallback,exit_proc,NULL);
  XmStringFree(label_string);

  menu_bar = XmCreateMenuBar(main_window,"menu_bar",args,(Cardinal)NULL);

  label_string = XmStringCreateLtoR("File",XmSTRING_DEFAULT_CHARSET);
  XtSetArg(args[0],XmNlabelString,label_string);
  XtSetArg(args[1],XmNsubMenuId,file_menu);
  file_button =  XtCreateManagedWidget("file_button",
                                       xmCascadeButtonWidgetClass,
                                       menu_bar,args,2);
  XmStringFree(label_string);
  XtSetArg(args[0],XmNmenuBar,menu_bar);
  XtSetValues(main_window,args,1);

  XtSetArg(args[0],XmNworkWindow,form);
  XtSetValues(main_window,args,1);

  XtManageChild(menu_bar);
  label_string = XmStringCreateLtoR("LCD Panel",
				     XmSTRING_DEFAULT_CHARSET);  
  lcd_label = XtVaCreateManagedWidget("lcd_label", 
				      xmLabelGadgetClass,
                                      form,
				      XmNtopAttachment,XmATTACH_FORM,
				      XmNtopOffset,30,
				      XmNleftAttachment,XmATTACH_FORM,
				      XmNleftOffset,30,
				      XmNlabelString,label_string,
                                      NULL);
  XmStringFree(label_string);  
  lcd_frame = XtVaCreateManagedWidget("lcd_frame", xmFrameWidgetClass,form,
				       XmNtopAttachment,XmATTACH_WIDGET,
                                       XmNtopWidget,lcd_label,
				       XmNleftAttachment,XmATTACH_FORM,
				       XmNleftOffset,30,
				       XmNshadowType,XmSHADOW_IN,
				       NULL);

  label_string = XmStringCreateLtoR("Welcome        ",
				     XmSTRING_DEFAULT_CHARSET);  

  lcd =      XtVaCreateManagedWidget("lcd", 
				      xmLabelGadgetClass,
                                      lcd_frame,
				      XmNlabelString,label_string,
				     /*XmNfontList,
				      "adobe-*-*-*-*-*-24-*-*-*-*-*-*-*-",*/
                                      NULL);

  XmStringFree(label_string);

  label_string = XmStringCreateLtoR("LED Panel",
				     XmSTRING_DEFAULT_CHARSET);  
  led_label = XtVaCreateManagedWidget("led_label", 
				      xmLabelGadgetClass,
                                      form,
				      XmNtopAttachment,XmATTACH_FORM,
				      XmNtopOffset,30,
				      XmNleftAttachment,XmATTACH_WIDGET,
 				      XmNleftWidget,lcd_frame,
				      XmNleftOffset,30,
				      XmNlabelString,label_string,
                                      NULL);
  XmStringFree(label_string);  
  
  led_off_pixmap = XCreatePixmap(display,
		             root_window,
		             LED_WIDTH,
			     LED_HEIGHT+2*LED_HEIGHT_OFFSET,
		             DefaultDepth(display,SCREEN_NUMBER));
  led_on_pixmap = XCreatePixmap(display,
		             root_window,
		             LED_WIDTH,
			     LED_HEIGHT+2*LED_HEIGHT_OFFSET,
		             DefaultDepth(display,SCREEN_NUMBER));

  XSetForeground(display,graphics_context,WhitePixel(display,SCREEN_NUMBER));  
  XFillRectangle(display,led_off_pixmap,graphics_context,
		   0,0,
		   LED_WIDTH,
		   LED_HEIGHT+2*LED_HEIGHT_OFFSET);

  XSetForeground(display,graphics_context,red_color_pixel);  
  XFillRectangle(display,led_on_pixmap,graphics_context,
		   0,0,
		   LED_WIDTH,
		   LED_HEIGHT+2*LED_HEIGHT_OFFSET);

  XSetForeground(display,graphics_context,BlackPixel(display,SCREEN_NUMBER));

  led_frame = XtVaCreateManagedWidget("led_frame", xmFrameWidgetClass,form,
				       XmNtopAttachment,XmATTACH_WIDGET,
                                       XmNtopWidget,led_label,
				       XmNleftAttachment,XmATTACH_WIDGET,
				       XmNleftWidget,lcd_frame,
				       XmNleftOffset,30,
				       XmNshadowType,XmSHADOW_OUT,
				       NULL);

  led_column = XtVaCreateManagedWidget("led_clumn", xmRowColumnWidgetClass,
				       led_frame,
				       XmNorientation,XmHORIZONTAL,
				       NULL);
  for (i=0;i<MAX_LED;i++)
    leds[i] = XtVaCreateManagedWidget("led", xmLabelWidgetClass,led_column,
				       XmNlabelType,XmPIXMAP,
				       XmNlabelPixmap,led_off_pixmap,
				       XmNborderWidth,2,
				       NULL);

  label_string = XmStringCreateLtoR("Switches",
				     XmSTRING_DEFAULT_CHARSET);  

  switches_label = XtVaCreateManagedWidget("switches_label", 
				      xmLabelGadgetClass,
                                      form,
				      XmNtopAttachment,XmATTACH_WIDGET,
				      XmNtopWidget,lcd_frame,
				      XmNtopOffset,30,
				      XmNleftAttachment,XmATTACH_FORM,
				      XmNleftOffset,30,
				      XmNlabelString,label_string,
                                      NULL);
  XmStringFree(label_string);

  switch_box = XtVaCreateManagedWidget( "switch_box",
				       xmFormWidgetClass,form,
				       XmNtopAttachment,XmATTACH_WIDGET,
                                       XmNtopWidget,switches_label,
				       XmNleftAttachment,XmATTACH_FORM,
				       XmNleftWidget,lcd_frame,
				       XmNleftOffset,30,
                                       XmNorientation,XmVERTICAL,
				       NULL);

  sprintf(number_string," %1d",MAX_SWITCH);
  label_string = XmStringCreateLtoR(number_string,
				     XmSTRING_DEFAULT_CHARSET);
  dummy_string = XmStringCreateLtoR("",
				     XmSTRING_DEFAULT_CHARSET);

  switch_label[0] = XtVaCreateManagedWidget( "switch_label",
				       xmLabelGadgetClass,switch_box,
                                       XmNlabelString,label_string,
				       NULL);
  
  on_off_box[0] = XtVaCreateManagedWidget( "on_off_box",
				       xmRowColumnWidgetClass,switch_box,
				       XmNtopAttachment,XmATTACH_WIDGET,
				       XmNtopWidget,switch_label[0],
                                       XmNradioBehavior,TRUE,
                                       XmNorientation,XmVERTICAL,
				       XmNpacking,XmPACK_TIGHT,
                                       NULL);
  on_switches[0] = XtVaCreateManagedWidget( "on_switches",
				       xmToggleButtonGadgetClass,on_off_box[0],
                                       XmNlabelString,dummy_string,
				       NULL);
  XtAddCallback(on_switches[0],XmNvalueChangedCallback,test_proc,NULL);
  off_switches[0] = XtVaCreateManagedWidget( "off_switches",
				       xmToggleButtonGadgetClass,on_off_box[0],
				       XmNset,TRUE,     
                                       XmNlabelString,dummy_string,
				       NULL);
  XtAddCallback(off_switches[0],XmNvalueChangedCallback,test_proc,NULL);

  for (i=1;i<MAX_SWITCH;i++) {
    sprintf(number_string," %1d",MAX_SWITCH-i);
    label_string = XmStringCreateLtoR(number_string,
				     XmSTRING_DEFAULT_CHARSET);

    switch_label[i] = XtVaCreateManagedWidget( "switch_label",
				       xmLabelGadgetClass,switch_box,
                                       XmNlabelString,label_string,
				       XmNleftAttachment,XmATTACH_WIDGET,
				       XmNleftWidget,on_off_box[i-1],
				       NULL);

    on_off_box[i] = XtVaCreateManagedWidget( "on_off_box",
				       xmRowColumnWidgetClass,switch_box,
				       XmNtopAttachment,XmATTACH_WIDGET,
				       XmNtopWidget,switch_label[i],
				       XmNleftAttachment,XmATTACH_WIDGET,
				       XmNleftWidget,on_off_box[i-1],
                                       XmNradioBehavior,TRUE,
                                       XmNorientation,XmVERTICAL,
				       XmNpacking,XmPACK_TIGHT,
                                       NULL);

    on_switches[i] = XtVaCreateManagedWidget( "on_switches",
				       xmToggleButtonGadgetClass,on_off_box[i],
                                       XmNlabelString,dummy_string,
				       NULL);
    XtAddCallback(on_switches[i],XmNvalueChangedCallback,toggled_proc,NULL);

    off_switches[i] = XtVaCreateManagedWidget( "off_switches",
				       xmToggleButtonGadgetClass,on_off_box[i],
                                       XmNlabelString,dummy_string,
				       XmNset,TRUE,     
				       NULL);
    XtAddCallback(off_switches[i],XmNvalueChangedCallback,toggled_proc,NULL);

    XmStringFree(label_string);
  }
  XmStringFree(dummy_string);

  label_string = XmStringCreateLtoR("on",
				     XmSTRING_DEFAULT_CHARSET);
  switch_on_label = XtVaCreateManagedWidget( "switch_on_label",
				       xmLabelGadgetClass,switch_box,
                                       XmNlabelString,label_string,
				       XmNleftAttachment,XmATTACH_WIDGET,
				       XmNleftWidget,on_off_box[MAX_SWITCH-1],
				       XmNtopAttachment,XmATTACH_WIDGET,
				       XmNtopWidget,switch_label[MAX_SWITCH-1],
				       NULL);
  XmStringFree(label_string);

  label_string = XmStringCreateLtoR("off",
				     XmSTRING_DEFAULT_CHARSET);
  switch_off_label = XtVaCreateManagedWidget( "switch_off_label",
			  	      xmLabelGadgetClass,switch_box,
                          	      XmNlabelString,label_string,
			  	      XmNleftAttachment,XmATTACH_WIDGET,
			  	      XmNleftWidget,on_off_box[MAX_SWITCH-1],
			  	      XmNtopAttachment,XmATTACH_WIDGET,
			  	      XmNtopWidget,switch_on_label,
				      XmNtopOffset,5,
			  	      NULL);
  XmStringFree(label_string);

  /*
    Treat the simulated ADC
    */

  label_string = XmStringCreateLtoR("ADC input level",
				     XmSTRING_DEFAULT_CHARSET);  

  adc_label = XtVaCreateManagedWidget("adc_label", 
				      xmLabelGadgetClass,
                                      form,
				      XmNtopAttachment,XmATTACH_WIDGET,
				      XmNtopWidget,switch_box,
				      XmNtopOffset,30,
				      XmNleftAttachment,XmATTACH_FORM,
				      XmNleftOffset,30,
				      XmNlabelString,label_string,
                                      NULL);
  XmStringFree(label_string);

  adc_slider = XtVaCreateManagedWidget( "slider",
				       xmScaleWidgetClass,form,
				       XmNtopAttachment,XmATTACH_WIDGET,
                                       XmNtopWidget,adc_label,
				       XmNtopOffset,10,
				       XmNleftAttachment,XmATTACH_FORM,
				       XmNleftOffset,30,
				       XmNminimum,0,
				       XmNmaximum,MAX_ADC,
				       XmNscaleWidth,250,
				       XmNorientation,XmHORIZONTAL,
				       XmNshowValue,TRUE,
				       NULL);

  XtRealizeWidget(toplevel);

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
#if XmVERSION > 1
static XtWidgetGeometry Expected[] = {
   {CWWidth | CWHeight            ,  243,  311,  180,  306, 0,0,0, /* main_window */},
   {CWWidth | CWHeight | CWX | CWY,    0,   31,  180,    2, 0,0,0, /* Separator1 */},
   {CWWidth | CWHeight | CWX | CWY,    0,   33,  180,  273, 0,0,0, /* form */},
   {CWWidth | CWHeight | CWX | CWY,   30,   30,   58,   17, 0,0,0, /* lcd_label */},
   {CWWidth | CWHeight | CWX | CWY,   30,   47,   98,   21, 0,0,0, /* lcd_frame */},
   {CWWidth | CWHeight | CWX | CWY,    2,    2,   94,   17, 0,0,0, /* lcd */},
   {CWWidth | CWHeight | CWX | CWY,  158,   30,   58,   17, 0,0,0, /* led_label */},
   {CWWidth | CWHeight | CWX | CWY,  158,   47,  217,   48, 0,0,0, /* led_frame */},
   {CWWidth | CWHeight | CWX | CWY,    2,    2,  213,   44, 0,0,0, /* led_clumn */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,   24,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,   45,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,   66,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,   87,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,  108,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,  129,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,  150,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,  171,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,  192,    3,   14,   34, 0,0,0, /* led */},
   {CWWidth | CWHeight | CWX | CWY,   30,   98,   52,   17, 0,0,0, /* switches_label */},
   {CWWidth | CWHeight | CWX | CWY,   30,  115,  238,   66, 0,0,0, /* switch_box */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,   16,   17, 0,0,0, /* switch_label */},
   {CWWidth | CWHeight | CWX | CWY,    0,   17,   27,   49, 0,0,0, /* on_off_box */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,   21,   20, 0,0,0, /* on_switches */},
   {CWWidth | CWHeight | CWX | CWY,    3,   26,   21,   20, 0,0,0, /* off_switches */},
   {CWWidth | CWHeight | CWX | CWY,   27,    0,   16,   17, 0,0,0, /* switch_label */},
   {CWWidth | CWHeight | CWX | CWY,   27,   17,   27,   49, 0,0,0, /* on_off_box */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,   21,   20, 0,0,0, /* on_switches */},
   {CWWidth | CWHeight | CWX | CWY,    3,   26,   21,   20, 0,0,0, /* off_switches */},
   {CWWidth | CWHeight | CWX | CWY,   54,    0,   16,   17, 0,0,0, /* switch_label */},
   {CWWidth | CWHeight | CWX | CWY,   54,   17,   27,   49, 0,0,0, /* on_off_box */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,   21,   20, 0,0,0, /* on_switches */},
   {CWWidth | CWHeight | CWX | CWY,    3,   26,   21,   20, 0,0,0, /* off_switches */},
   {CWWidth | CWHeight | CWX | CWY,   81,    0,   16,   17, 0,0,0, /* switch_label */},
   {CWWidth | CWHeight | CWX | CWY,   81,   17,   27,   49, 0,0,0, /* on_off_box */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,   21,   20, 0,0,0, /* on_switches */},
   {CWWidth | CWHeight | CWX | CWY,    3,   26,   21,   20, 0,0,0, /* off_switches */},
   {CWWidth | CWHeight | CWX | CWY,  108,    0,   16,   17, 0,0,0, /* switch_label */},
   {CWWidth | CWHeight | CWX | CWY,  108,   17,   27,   49, 0,0,0, /* on_off_box */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,   21,   20, 0,0,0, /* on_switches */},
   {CWWidth | CWHeight | CWX | CWY,    3,   26,   21,   20, 0,0,0, /* off_switches */},
   {CWWidth | CWHeight | CWX | CWY,  135,    0,   16,   17, 0,0,0, /* switch_label */},
   {CWWidth | CWHeight | CWX | CWY,  135,   17,   27,   49, 0,0,0, /* on_off_box */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,   21,   20, 0,0,0, /* on_switches */},
   {CWWidth | CWHeight | CWX | CWY,    3,   26,   21,   20, 0,0,0, /* off_switches */},
   {CWWidth | CWHeight | CWX | CWY,  162,    0,   16,   17, 0,0,0, /* switch_label */},
   {CWWidth | CWHeight | CWX | CWY,  162,   17,   27,   49, 0,0,0, /* on_off_box */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,   21,   20, 0,0,0, /* on_switches */},
   {CWWidth | CWHeight | CWX | CWY,    3,   26,   21,   20, 0,0,0, /* off_switches */},
   {CWWidth | CWHeight | CWX | CWY,  189,    0,   16,   17, 0,0,0, /* switch_label */},
   {CWWidth | CWHeight | CWX | CWY,  189,   17,   27,   49, 0,0,0, /* on_off_box */},
   {CWWidth | CWHeight | CWX | CWY,    3,    3,   21,   20, 0,0,0, /* on_switches */},
   {CWWidth | CWHeight | CWX | CWY,    3,   26,   21,   20, 0,0,0, /* off_switches */},
   {CWWidth | CWHeight | CWX | CWY,  216,   17,   16,   17, 0,0,0, /* switch_on_label */},
   {CWWidth | CWHeight | CWX | CWY,  216,   39,   22,   17, 0,0,0, /* switch_off_label */},
   {CWWidth | CWHeight | CWX | CWY,   30,  211,   94,   17, 0,0,0, /* adc_label */},
   {CWWidth | CWHeight | CWX | CWY,   30,  238,  250,   35, 0,0,0, /* slider */},
   {CWWidth | CWHeight | CWX | CWY,    0,   16,  250,   19, 0,0,0, /* Scrollbar */},
   {CWWidth | CWHeight | CWX | CWY,    0,    0,  180,   31, 0,0,0, /* menu_bar */},
   {CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* file_button */},
};
#else
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  771,  192,  180,  314, 0,0,0, /* main_window */
   CWWidth | CWHeight | CWX | CWY,    0,   31,  180,    2, 0,0,0, /* Separator1 */
   CWWidth | CWHeight | CWX | CWY,    0,   33,  180,  281, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,   30,   30,   58,   17, 0,0,0, /* lcd_label */
   CWWidth | CWHeight | CWX | CWY,   30,   47,   98,   21, 0,0,0, /* lcd_frame */
   CWWidth | CWHeight | CWX | CWY,    2,    2,   94,   17, 0,0,0, /* lcd */
   CWWidth | CWHeight | CWX | CWY,  158,   30,   58,   17, 0,0,0, /* led_label */
   CWWidth | CWHeight | CWX | CWY,  158,   47,  217,   48, 0,0,0, /* led_frame */
   CWWidth | CWHeight | CWX | CWY,    2,    2,  213,   44, 0,0,0, /* led_clumn */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,   24,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,   45,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,   66,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,   87,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,  108,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,  129,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,  150,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,  171,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,  192,    3,   14,   34, 0,0,0, /* led */
   CWWidth | CWHeight | CWX | CWY,   30,   98,   52,   17, 0,0,0, /* switches_label */
   CWWidth | CWHeight | CWX | CWY,   30,  115,  270,   74, 0,0,0, /* switch_box */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   16,   17, 0,0,0, /* switch_label */
   CWWidth | CWHeight | CWX | CWY,    0,   17,   31,   57, 0,0,0, /* on_off_box */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   25,   24, 0,0,0, /* on_switches */
   CWWidth | CWHeight | CWX | CWY,    3,   30,   25,   24, 0,0,0, /* off_switches */
   CWWidth | CWHeight | CWX | CWY,   31,    0,   16,   17, 0,0,0, /* switch_label */
   CWWidth | CWHeight | CWX | CWY,   31,   17,   31,   57, 0,0,0, /* on_off_box */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   25,   24, 0,0,0, /* on_switches */
   CWWidth | CWHeight | CWX | CWY,    3,   30,   25,   24, 0,0,0, /* off_switches */
   CWWidth | CWHeight | CWX | CWY,   62,    0,   16,   17, 0,0,0, /* switch_label */
   CWWidth | CWHeight | CWX | CWY,   62,   17,   31,   57, 0,0,0, /* on_off_box */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   25,   24, 0,0,0, /* on_switches */
   CWWidth | CWHeight | CWX | CWY,    3,   30,   25,   24, 0,0,0, /* off_switches */
   CWWidth | CWHeight | CWX | CWY,   93,    0,   16,   17, 0,0,0, /* switch_label */
   CWWidth | CWHeight | CWX | CWY,   93,   17,   31,   57, 0,0,0, /* on_off_box */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   25,   24, 0,0,0, /* on_switches */
   CWWidth | CWHeight | CWX | CWY,    3,   30,   25,   24, 0,0,0, /* off_switches */
   CWWidth | CWHeight | CWX | CWY,  124,    0,   16,   17, 0,0,0, /* switch_label */
   CWWidth | CWHeight | CWX | CWY,  124,   17,   31,   57, 0,0,0, /* on_off_box */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   25,   24, 0,0,0, /* on_switches */
   CWWidth | CWHeight | CWX | CWY,    3,   30,   25,   24, 0,0,0, /* off_switches */
   CWWidth | CWHeight | CWX | CWY,  155,    0,   16,   17, 0,0,0, /* switch_label */
   CWWidth | CWHeight | CWX | CWY,  155,   17,   31,   57, 0,0,0, /* on_off_box */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   25,   24, 0,0,0, /* on_switches */
   CWWidth | CWHeight | CWX | CWY,    3,   30,   25,   24, 0,0,0, /* off_switches */
   CWWidth | CWHeight | CWX | CWY,  186,    0,   16,   17, 0,0,0, /* switch_label */
   CWWidth | CWHeight | CWX | CWY,  186,   17,   31,   57, 0,0,0, /* on_off_box */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   25,   24, 0,0,0, /* on_switches */
   CWWidth | CWHeight | CWX | CWY,    3,   30,   25,   24, 0,0,0, /* off_switches */
   CWWidth | CWHeight | CWX | CWY,  217,    0,   16,   17, 0,0,0, /* switch_label */
   CWWidth | CWHeight | CWX | CWY,  217,   17,   31,   57, 0,0,0, /* on_off_box */
   CWWidth | CWHeight | CWX | CWY,    3,    3,   25,   24, 0,0,0, /* on_switches */
   CWWidth | CWHeight | CWX | CWY,    3,   30,   25,   24, 0,0,0, /* off_switches */
   CWWidth | CWHeight | CWX | CWY,  248,   17,   16,   17, 0,0,0, /* switch_on_label */
   CWWidth | CWHeight | CWX | CWY,  248,   39,   22,   17, 0,0,0, /* switch_off_label */
   CWWidth | CWHeight | CWX | CWY,   30,  219,   94,   17, 0,0,0, /* adc_label */
   CWWidth | CWHeight | CWX | CWY,   30,  246,  250,   35, 0,0,0, /* slider */
   CWWidth | CWHeight | CWX | CWY,    0,   16,  250,   19, 0,0,0, /* Scrollbar */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  180,   31, 0,0,0, /* menu_bar */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   40,   21, 0,0,0, /* file_button */
};
#endif
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}


/*==========================================================*/
void 
exit_proc(Widget w,XtPointer client_data,XtPointer call_data)
/*==========================================================*/
{

   exit(0);
}
/*==========================================================*/
void 
test_proc(Widget w,XtPointer client_data,XtPointer call_data)
/*==========================================================*/
{

  if (strcmp(XtName(w),"on_switches") == 0) 
    XtVaSetValues(leds[0],XmNlabelPixmap,led_on_pixmap,NULL);
  else
    XtVaSetValues(leds[0],XmNlabelPixmap,led_off_pixmap,NULL);
}

/*==========================================================*/
void 
toggled_proc(Widget w,XtPointer client_data,XtPointer call_data)
/*==========================================================*/
{
  int i;
  XmToggleButtonCallbackStruct *state = 
     (XmToggleButtonCallbackStruct *) call_data;

  for (i=0;i<MAX_SWITCH;i++)
  {
    if (XtParent(w) == on_off_box[i]) 
    {
      if (strcmp(XtName(w),"on_switches") == 0) 
      {
	if (state -> set) 
	{
           ICTP_IO_switch_state |= 1<<(MAX_SWITCH-i-1);
        }
        else
        {
           ICTP_IO_switch_state &= ~(1<<(MAX_SWITCH-i-1));
        }
      }
    }
  }
printf("%x\n",ICTP_IO_switch_state);
}
