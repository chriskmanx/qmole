/*
Subject:	Some changes to MessageBox and ResConvert
Date:		Sun, 30 Aug 1998 22:30:44 +0200
From:		Karsten Jensen <kbwj@diku.dk>

test16.c shows why the buttons in e.g MessageBox is
bigger in Motif than Lesstif. Is seems like they add
Xm3D_ENHANCE_PIXEL both to the margins and to the highlightThickness.
*/

#include <Xm/FormP.h>
#include <Xm/PushBP.h>
#include <Xm/MainWP.h>
#include <Xm/ArrowBP.h>
#include <Xm/RowColumnP.h>
#include <Xm/CascadeBP.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *
mode_to_string(int mode)
{
    Boolean first = True;
    char *return_str = XtMalloc(sizeof(char)*40);

    mode &= CWWidth | CWHeight | CWBorderWidth;

    if (mode & CWWidth)
    {
	strcpy(return_str, "CWWidth");
	first = False;
    }
    mode &= ~CWWidth;
    if (mode & CWHeight)
    {
	if (first)
	{
	    strcpy(return_str, "CWHeight");
	    first = False;
	}
	else
	{
	    strcat(return_str, "|CWHeight");
	}
    }
    mode &= ~CWHeight;
    if (mode & CWBorderWidth)
    {
	if (first)
	{
	    strcpy(return_str, "CWBorderWidth");
	    first = False;
	}
	else
	{
	    strcat(return_str, "|CWBorderWidth");
	}
    }
    mode &= ~CWBorderWidth;
    if (first)
    {
	strcpy(return_str, "none");
    }
    return return_str;
}

char *
result_to_string(XtGeometryResult r)
{
    static char *return_str[]  = 
    {"XtGeometryNo","XtGeometryYes","XtGeometryAlmost","unknown"};

    if (r ==  XtGeometryNo)
    {
	return return_str[0];
    }
    else if (r ==  XtGeometryYes)
    {
	return return_str[1];
    }
    else if (r ==  XtGeometryAlmost)
    {
	return return_str[2];
    }
    else
    {
	return return_str[3];
    }
}

void
CallQueryGeometry(Widget pb, XtPointer client_data, XtPointer call_data)
{
    Widget w = (Widget) client_data;
    XmLabelWidget lw = (XmLabelWidget) w;
    char width_string[128], height_string[128];
    XtWidgetGeometry intended, preferred;
    XtGeometryResult result;
    Dimension new_height, new_width;
    int r;
    XmString s = XmStringCreateLocalized("NEW OK");
    static char *requests[] =
    {
	"width none height none",
	"width none height none",
	NULL
    };
    static int req_nr = 0;




    fprintf(stderr, "TextRect Width %3d, Height %3d\n",
	    lw->label.TextRect.width,
	    lw->label.TextRect.height);
    fprintf(stderr, "AccTextRect Width %3d, Height %3d\n",
	    lw->label.acc_TextRect.width,
	    lw->label.acc_TextRect.height);

    fprintf(stderr, "borderWidth %3d\n",
	    lw->core.border_width);

    fprintf(stderr, "highlightThickness %3d, shadowThickness %3d\n",
	    lw->primitive.highlight_thickness,
	    lw->primitive.shadow_thickness);

    fprintf(stderr, "MarginTop %3d, MarginBottom %3d\n",
	    lw->label.margin_top,
	    lw->label.margin_bottom);

    fprintf(stderr, "MarginLeft %3d, MarginRight %3d\n",
	    lw->label.margin_left,
	    lw->label.margin_right);

    fprintf(stderr, "MarginWidth %3d, MarginHeight %3d\n",
	    lw->label.margin_width,
	    lw->label.margin_height);
    if (requests[req_nr] != NULL)
    {
	r = sscanf(requests[req_nr++], "%*s %s %*s %s ",
		   width_string, height_string);
	printf("Size before request: width = %3i, height = %3i\n",
	       XtWidth(w), XtHeight(w));
	
	intended.request_mode = 0;
	if (strcmp("none", width_string))
	{
	    new_width = atoi(width_string);
	    intended.width = new_width;
	    intended.request_mode |= CWWidth;
	}
	else
	{
	    new_width = 0;
	    intended.width = new_width;
	}
	if (strcmp("none", height_string))
	{
	    new_height = atoi(height_string);
	    intended.height = new_height;
	    intended.request_mode |= CWHeight;
	}
	else
	{
	    new_height = 0;
	    intended.height = new_height;
	}
	printf("QueryGeometry called with width = %3i, height = %3i, "
	       "request_mode = %s\n", intended.width, intended.height,
	       mode_to_string(intended.request_mode));
	result = XtQueryGeometry(w, &intended, &preferred);
	printf("The result is %s with width = %3i, height = %3i"
	       ", request_mode %s\n",
	       result_to_string(result), preferred.width, preferred.height,
	       mode_to_string(preferred.request_mode));
    }
    else
    {
	exit(0);
    }
    XtVaSetValues(w, XmNlabelString, s, NULL);
    XmStringFree(s);

}

char *fallback_resources[] =
{
    "Test1.allowShellResize: True",
    "*pb.labelString: QueryGeometry",
    "*pb.showAsDefault: True",
    NULL
};

int
main(int argc, char *argv[])
{
    Widget toplevel,  fo, la, pb;
    XtAppContext app;

    toplevel = XtVaAppInitialize(&app, "Test1",
				 NULL, 0,
				 &argc, argv,
				 fallback_resources,
				 NULL);
    fo  = XtVaCreateManagedWidget("fo",
				  xmFormWidgetClass,
				  toplevel, 
				  NULL);
    pb  = XtVaCreateManagedWidget("pb",
				  xmPushButtonWidgetClass,
				  fo,
				  XmNleftAttachment, XmATTACH_FORM, 
				  XmNtopAttachment, XmATTACH_FORM, 
				  NULL);
    la  = XtVaCreateManagedWidget("la",
				  xmLabelWidgetClass,
				  fo, 
				  XmNleftAttachment, XmATTACH_FORM, 
				  XmNtopAttachment, XmATTACH_WIDGET, 
				  XmNtopWidget, pb, 
				  XmNrightAttachment, XmATTACH_FORM, 
				  XmNbottomAttachment, XmATTACH_FORM, 
				  NULL);

    XtAddCallback(pb, XmNactivateCallback, CallQueryGeometry, pb);
    
    XtRealizeWidget(toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  102,   54, 0,0,0, /* fo */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  102,   37, 0,0,0, /* pb */
   CWWidth | CWHeight | CWX | CWY,    0,   37,  102,   17, 0,0,0, /* la */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

    /*NOTREACHED*/
    return 0;
}




