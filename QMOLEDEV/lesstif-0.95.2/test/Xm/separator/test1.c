#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <stdlib.h>

int
main(int argc, char **argv)
{
  XtAppContext theApp;
  Widget toplevel;
  Widget butt1;
  int id;
  int nid=0;
  int orientation;

  toplevel = XtVaAppInitialize(&theApp, "separator", NULL, 0,
			       &argc, argv, NULL, NULL);

  if (argc < 2){
	printf("Usage SeparTest <style> [orientation]\n\
		Where style = 0-6 \n\
		0 = XmNO_LINE \n\
		1 = XmSINGLE_LINE \n\
		2 = XmDOUBLE_LINE \n\
		3 = XmSINGLE_DASHED_LINE \n\
		4 = XmDOUBLE_DASHED_LINE \n\
		5 = XmSHADOW_ETCHED_IN \n\
		6 = XmSHADOW_ETCHED_OUT \n\
		7 = XmSHADOW_ETCHED_IN_DASH \n\
		8 = XmSHADOW_ETCHED_OUT_DASH \n\
		Where orientation = 0-1 \n\
		0 = XmHORIZONTAL \n\
		1 = XmVERTICAL \n");

  }

  if (argc>1){
  	id = atoi(argv[1]);
        if (id >= XmINVALID_SEPARATOR_TYPE || id < XmNO_LINE)
	  printf("Use Id 0-8 to define style\n");
  }
  else {
	id = 0;
  }

  if (argc>2){
        orientation = atoi(argv[2]);
        if (orientation>1 || orientation < 0 ) 
			printf("Use Id 0-1 to define orientation\n");
  }
  else {
        orientation = 0;
  }


  switch(id){
	case 0: nid = XmSINGLE_LINE; break;
	case 1: nid = XmDOUBLE_LINE; break;
	case 2: nid = XmSINGLE_DASHED_LINE; break;
	case 3: nid = XmDOUBLE_DASHED_LINE; break;
	case 4: nid = XmSHADOW_ETCHED_IN; break;
	case 5: nid = XmSHADOW_ETCHED_OUT; break;
	case 6: nid = XmNO_LINE; break;
  }

  butt1= XtVaCreateManagedWidget("Button1", xmSeparatorWidgetClass, toplevel, 
				XmNseparatorType,nid,
				XmNorientation,(orientation ? XmVERTICAL : XmHORIZONTAL),
				NULL);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,    2,    3, 0,0,0, /* Button1 */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );

  exit(0);
}

