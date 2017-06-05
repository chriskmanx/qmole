/* $Id: test3.c,v 1.3 2000/08/29 21:59:20 dannybackx Exp $ */
#include <stdio.h>
#include <stdlib.h>

#include <X11/IntrinsicP.h>

#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/MainW.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>

static char *FallBack[] =
{
    "*MotifDragWindowId.alignment: XmALIGNMENT_BEGINNING",
    NULL
};

Window MotifDragWindowId = None;

static Widget
CreateMenuBar(Widget parent)
{
Widget MenuBar;
Widget Cascade;
Widget Button;
Widget Menu;

	MenuBar = XmCreateMenuBar(parent, "MenuBar", NULL, 0);

	Menu = XmCreatePulldownMenu(MenuBar, "FilePullDownMenu", NULL, 0);
	Button = XmCreatePushButton(Menu, "Quit", NULL, 0);
	XtAddCallback(Button, XmNactivateCallback, (XtCallbackProc)exit, NULL);
	XtManageChild(Button);
	Cascade = XmCreateCascadeButton(MenuBar, "File", NULL, 0);
	XtVaSetValues(Cascade,
		XmNsubMenuId, Menu,
		NULL);
	XtManageChild(Cascade);

	return(MenuBar);
}

static void
ReadMotifDragWindowId(Widget label)
{
Atom DragWindowAtom;
Atom actual_type;
int actual_format;
unsigned long nitems;
unsigned long bytes_after;
unsigned char *prop = NULL;
char buf[1024];
XmString string;

/*    printf("ReadMotifDragWindowId()\n"); */
    DragWindowAtom = XmInternAtom(XtDisplay(label), "_MOTIF_DRAG_WINDOW", False);
    if (XGetWindowProperty(XtDisplay(label), DefaultRootWindow(XtDisplay(label)), DragWindowAtom, 0L, 100000L, False, XA_WINDOW, &actual_type, &actual_format, &nitems, &bytes_after, &prop) == Success)
    {
    	if (actual_format == 32 && nitems == 1)
    	{
	    MotifDragWindowId = *((Window *)prop);
    	}
    	XFree(prop);
    }
    sprintf(buf, "_MOTIF_DRAG_WINDOW - ");
    if (MotifDragWindowId == None)
    {
    	sprintf(&buf[strlen(buf)], "None");
    }
    else
    {
    	sprintf(&buf[strlen(buf)], "%p", MotifDragWindowId);
    	/* set event handler for this window */
    }
    string = XmStringCreateSimple(buf);
    XtVaSetValues(label,
    	XmNlabelString, string,
    	NULL);
    XmStringFree(string);
}

static void
ReadDragTargets(Widget label)
{
Atom MotifDragTargets;
Atom actual_type;
int actual_format;
unsigned long nitems;
unsigned long bytes_after;
unsigned char *prop = NULL;
char buf[1024];
XmString string;
int num_target_lists;

/*    printf("ReadDragTargets(%s)\n", XtName(label)); */
    MotifDragTargets = XmInternAtom(XtDisplay(label), "_MOTIF_DRAG_TARGETS", False);
    if (XGetWindowProperty(XtDisplay(label), MotifDragWindowId, MotifDragTargets, 0L, 100000L, False, MotifDragTargets, &actual_type, &actual_format, &nitems, &bytes_after, &prop) == Success)
    {
    	if (nitems >= 8)
    	{
	    sprintf(buf, "Byte Order %s", prop[0] == 'B' ? "MSB first" : (prop[0] == 'l' ? "LSB first" : "Unknown"));
	    string = XmStringCreateSimple(buf);
	    XtVaSetValues(XtNameToWidget(label, "*ByteOrder"),
	    	XmNlabelString, string,
	    	NULL);
	    XmStringFree(string);
	    sprintf(buf, "Protocol version %i", prop[1]);
	    string = XmStringCreateSimple(buf);
	    XtVaSetValues(XtNameToWidget(label, "*Version"),
	    	XmNlabelString, string,
	    	NULL);
	    XmStringFree(string);
	    switch (prop[0])
	    {
	    case 'B':
		num_target_lists = prop[2] * 256 + prop[3];
	    	break;
	    case 'l':
		num_target_lists = prop[3] * 256 + prop[2];
	    	break;
	    default:
	    	num_target_lists = 0;
	    	break;
	    }
	    sprintf(buf, "%i target lists", num_target_lists);
	    string = XmStringCreateSimple(buf);
	    XtVaSetValues(XtNameToWidget(label, "*NumTargetLists"),
	    	XmNlabelString, string,
	    	NULL);
	    XmStringFree(string);
    	}
    	else
    	{
	    printf("ReadDragAtoms() - %i\n", nitems);
    	}
    	XFree(prop);
    }
    else
    {
	printf("ReadDragAtoms() - failed\n");
    }
}

static void
ReadDragAtoms(Widget label)
{
Atom MotifDragAtoms;
Atom actual_type;
int actual_format;
unsigned long nitems;
unsigned long bytes_after;
unsigned char *prop = NULL;
char buf[1024];
XmString string;
int num_atoms;
typedef struct {
    Atom atom;
    Time time;
} AtomTime_t;
struct {
    unsigned char format;
    unsigned char version;
    unsigned char num[2];
    AtomTime_t *AtomTime;
} DragAtomTable;

/*    printf("ReadDragAtoms(%s)\n", XtName(label)); */
    MotifDragAtoms = XmInternAtom(XtDisplay(label), "_MOTIF_DRAG_ATOMS", False);
    if (XGetWindowProperty(XtDisplay(label), 
                           MotifDragWindowId, 
                           MotifDragAtoms, 
                           0L, 100000L, 
                           False, 
                           MotifDragAtoms, 
                           &actual_type, 
                           &actual_format, 
                           &nitems, 
                           &bytes_after, 
                           &prop) == Success)
    {
    	if (nitems >= 8)
    	{
	    sprintf(buf, "Byte Order %s", prop[0] == 'B' ? "MSB first" : (prop[0] == 'l' ? "LSB first" : "Unknown"));
	    string = XmStringCreateSimple(buf);
	    XtVaSetValues(XtNameToWidget(label, "*ByteOrder"),
	    	XmNlabelString, string,
	    	NULL);
	    XmStringFree(string);
	    sprintf(buf, "Protocol version %i", prop[1]);
	    string = XmStringCreateSimple(buf);
	    XtVaSetValues(XtNameToWidget(label, "*Version"),
	    	XmNlabelString, string,
	    	NULL);
	    XmStringFree(string);
	    switch (prop[0])
	    {
	    case 'B':
		num_atoms = prop[2] * 256 + prop[3];
	    	break;
	    case 'l':
		num_atoms = prop[3] * 256 + prop[2];
	    	break;
	    default:
	    	num_atoms = 0;
	    	break;
	    }
	    sprintf(buf, "%i atoms", num_atoms);
	    string = XmStringCreateSimple(buf);
	    XtVaSetValues(XtNameToWidget(label, "*NumAtoms"),
	    	XmNlabelString, string,
	    	NULL);
	    XmStringFree(string);
    	}
    	else
    	{
	    printf("ReadDragAtoms() - %i\n", nitems);
    	}
    	XFree(prop);
    }
    else
    {
	printf("ReadDragAtoms() - failed\n");
    }
}

static void
ReadDragAtomPairs(Widget label)
{
Atom MotifDragAtomPairs;
Atom actual_type;
int actual_format;
unsigned long nitems;
unsigned long bytes_after;
unsigned char *prop = NULL;
char buf[1024];
XmString string;

/*    printf("ReadDragAtomPairss(%s)\n", XtName(label)); */
    MotifDragAtomPairs = XmInternAtom(XtDisplay(label), "_MOTIF_DRAG_ATOM_PAIRS", False);
    if (XGetWindowProperty(XtDisplay(label), MotifDragWindowId, MotifDragAtomPairs, 0L, 100000L, False, MotifDragAtomPairs, &actual_type, &actual_format, &nitems, &bytes_after, &prop) == Success)
    {
    	if (nitems >= 8)
    	{
	    sprintf(buf, "Byte Order %s", prop[0] == 'B' ? "MSB first" : (prop[0] == 'l' ? "LSB first" : "Unknown"));
	    string = XmStringCreateSimple(buf);
	    XtVaSetValues(XtNameToWidget(label, "*ByteOrder"),
	    	XmNlabelString, string,
	    	NULL);
	    XmStringFree(string);
	    sprintf(buf, "Protocol version %i", prop[1]);
	    string = XmStringCreateSimple(buf);
	    XtVaSetValues(XtNameToWidget(label, "*Version"),
	    	XmNlabelString, string,
	    	NULL);
	    XmStringFree(string);
    	}
    	else
    	{
	    printf("ReadDragAtoms() - %i\n", nitems);
    	}
    	XFree(prop);
    }
    else
    {
	printf("ReadDragAtoms() - failed\n");
    }
}

static void
ReadMotifDragWindowProperties(Widget label)
{
/*	printf("ReadMotifDragWindowProperties()\n"); */
	ReadDragTargets(XtNameToWidget(label, "*DragTargetsFrame"));
	ReadDragAtoms(XtNameToWidget(label, "*DragAtomsFrame"));
	ReadDragAtomPairs(XtNameToWidget(label, "*DragAtomPairsFrame"));
	/*
	   _MOTIF_DRAG_TARGETS
	   _MOTIF_DRAG_ATOMS
	   _MOTIF_DRAG_ATOM_PAIRS
	 */
}

static void
RootWindowPropertyNotifyHandler(Widget w, Widget label, XEvent *event)
{
Atom DragWindowAtom;

    printf("RootWindowPropertyNotifyHandler()\n");
    DragWindowAtom = XmInternAtom(XtDisplay(label), "_MOTIF_DRAG_WINDOW", False);
    if (event->xproperty.atom == DragWindowAtom)
    {
	printf("RoowWindowPropertyNotifyHandler() - %s\n",
		event->xproperty.state == PropertyNewValue ? "PropertyNewValue" : "PropertyDelete");
	ReadMotifDragWindowId(label);
    }
}

static Widget
CreateRootProperties(Widget parent)
{
Widget Frame;
Widget Label;
Widget DummyLabel;
Window DummyLabelWindow;

	Frame = XmCreateFrame(parent, "RootPropertiesFrame", NULL, 0);
	Label = XmCreateLabel(Frame, "RootWindowProperties", NULL, 0);
	XtVaSetValues(Label,
		XmNchildType, XmFRAME_TITLE_CHILD,
		NULL);
	XtManageChild(Label);
	Label = XmCreateLabel(Frame, "MotifDragWindowId", NULL, 0);
	ReadMotifDragWindowId(Label);
	XtManageChild(Label);
	XtRegisterDrawable(XtDisplay(Frame), DefaultRootWindow(XtDisplay(parent)), Frame);

	XtAddEventHandler(Frame,
		PropertyChangeMask,
		False,
		(XtEventHandler)RootWindowPropertyNotifyHandler,
		Label);

	return(Frame);
}

static void
DragWindowPropertyNotifyHandler(Widget w, Widget label, XEvent *event)
{
String AtomName = XGetAtomName(XtDisplay(w), event->xproperty.atom);

    printf("DragWindowPropertyNotifyHandler() - %s %s\n",
	    event->xproperty.state == PropertyNewValue ? "PropertyNewValue" : "PropertyDelete",
	    AtomName);
    XtFree(AtomName);
    ReadMotifDragWindowProperties(label);
}

static Widget
CreateDragTargets(Widget parent)
{
Widget Frame;
Widget Label;
Widget Form;
Widget ByteOrder;
Widget Version;
Widget NumTargetLists;
Widget TargetTableSize;

    Frame = XmCreateFrame(parent, "DragTargetsFrame", NULL, 0);
    Label = XmCreateLabel(Frame, "_MOTIF_DRAG_TARGETS", NULL, 0);
    XtVaSetValues(Label,
	    XmNchildType, XmFRAME_TITLE_CHILD,
	    NULL);
    XtManageChild(Label);
    Form = XmCreateForm(Frame, "DragTargetsForm", NULL, 0);
    ByteOrder = XmCreateLabel(Form, "ByteOrder", NULL, 0);
    XtVaSetValues(ByteOrder,
	    XmNtopAttachment, XmATTACH_FORM,
	    XmNleftAttachment, XmATTACH_FORM,
	    NULL);
    XtManageChild(ByteOrder);
    Version = XmCreateLabel(Form, "Version", NULL, 0);
    XtVaSetValues(Version,
	    XmNtopAttachment, XmATTACH_WIDGET,
	    XmNtopWidget, ByteOrder,
	    XmNleftAttachment, XmATTACH_FORM,
	    NULL);
    XtManageChild(Version);
    NumTargetLists = XmCreateLabel(Form, "NumTargetLists", NULL, 0);
    XtVaSetValues(NumTargetLists,
	    XmNtopAttachment, XmATTACH_WIDGET,
	    XmNtopWidget, Version,
	    XmNleftAttachment, XmATTACH_FORM,
	    NULL);
    XtManageChild(NumTargetLists);
    TargetTableSize = XmCreateLabel(Form, "TargetTableSize", NULL, 0);
    XtVaSetValues(TargetTableSize,
	    XmNtopAttachment, XmATTACH_WIDGET,
	    XmNtopWidget, NumTargetLists,
	    XmNleftAttachment, XmATTACH_FORM,
	    NULL);
    XtManageChild(TargetTableSize);
    /* now the actual target table */

    XtManageChild(Form);
    return(Frame);
}

static Widget
CreateDragAtoms(Widget parent)
{
Widget Frame;
Widget Label;
Widget Form;
Widget ByteOrder;
Widget Version;
Widget NumAtoms;

    Frame = XmCreateFrame(parent, "DragAtomsFrame", NULL, 0);
    Label = XmCreateLabel(Frame, "_MOTIF_DRAG_ATOMS", NULL, 0);
    XtVaSetValues(Label,
	    XmNchildType, XmFRAME_TITLE_CHILD,
	    NULL);
    XtManageChild(Label);
    Form = XmCreateForm(Frame, "DragAtomsForm", NULL, 0);
    ByteOrder = XmCreateLabel(Form, "ByteOrder", NULL, 0);
    XtVaSetValues(ByteOrder,
	    XmNtopAttachment, XmATTACH_FORM,
	    XmNleftAttachment, XmATTACH_FORM,
	    NULL);
    XtManageChild(ByteOrder);
    Version = XmCreateLabel(Form, "Version", NULL, 0);
    XtVaSetValues(Version,
	    XmNtopAttachment, XmATTACH_WIDGET,
	    XmNtopWidget, ByteOrder,
	    XmNleftAttachment, XmATTACH_FORM,
	    NULL);
    XtManageChild(Version);
    NumAtoms = XmCreateLabel(Form, "NumAtoms", NULL, 0);
    XtVaSetValues(NumAtoms,
	    XmNtopAttachment, XmATTACH_WIDGET,
	    XmNtopWidget, Version,
	    XmNleftAttachment, XmATTACH_FORM,
	    NULL);
    XtManageChild(NumAtoms);
    /* now for the atoms */

    XtManageChild(Form);
    return(Frame);
}

static Widget
CreateDragAtomPairs(Widget parent)
{
Widget Frame;
Widget Label;
Widget Form;
Widget ByteOrder;
Widget Version;

    Frame = XmCreateFrame(parent, "DragAtomPairsFrame", NULL, 0);
    Label = XmCreateLabel(Frame, "_MOTIF_DRAG_ATOM_PAIRS", NULL, 0);
    XtVaSetValues(Label,
	    XmNchildType, XmFRAME_TITLE_CHILD,
	    NULL);
    XtManageChild(Label);
    Form = XmCreateForm(Frame, "DragAtomPairsForm", NULL, 0);
    ByteOrder = XmCreateLabel(Form, "ByteOrder", NULL, 0);
    XtVaSetValues(ByteOrder,
	    XmNtopAttachment, XmATTACH_FORM,
	    XmNleftAttachment, XmATTACH_FORM,
	    NULL);
    XtManageChild(ByteOrder);
    Version = XmCreateLabel(Form, "Version", NULL, 0);
    XtVaSetValues(Version,
	    XmNtopAttachment, XmATTACH_WIDGET,
	    XmNtopWidget, ByteOrder,
	    XmNleftAttachment, XmATTACH_FORM,
	    NULL);
    XtManageChild(Version);
    /* now for the atom pairs */

    XtManageChild(Form);
    return(Frame);
}

static Widget
CreateDragProperties(Widget parent)
{
Widget Frame;
Widget Label;
Widget DummyLabel;
Widget Form;
Widget DragTargets;
Widget DragAtoms;
Widget DragAtomPairs;
Window DummyLabelWindow;

	/*
	   _MOTIF_DRAG_TARGETS
	   _MOTIF_DRAG_ATOMS
	   _MOTIF_DRAG_ATOM_PAIRS
	 */
	Frame = XmCreateFrame(parent, "DragPropertiesFrame", NULL, 0);
	Label = XmCreateLabel(Frame, "DragWindowProperties", NULL, 0);
	XtVaSetValues(Label,
		XmNchildType, XmFRAME_TITLE_CHILD,
		NULL);
	XtManageChild(Label);

	Form = XmCreateForm(Frame, "DragWindowPropertiesForm", NULL, 0);
	DragTargets = CreateDragTargets(Form);
	XtVaSetValues(DragTargets,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
	XtManageChild(DragTargets);
	DragAtoms = CreateDragAtoms(Form);
	XtVaSetValues(DragAtoms,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, DragTargets,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
	XtManageChild(DragAtoms);
	DragAtomPairs = CreateDragAtomPairs(Form);
	XtVaSetValues(DragAtomPairs,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, DragAtoms,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
	XtManageChild(DragAtomPairs);
	XtManageChild(Form);
	ReadMotifDragWindowProperties(Form);

	XSelectInput(XtDisplay(Frame), MotifDragWindowId, PropertyChangeMask);
	XtRegisterDrawable(XtDisplay(Frame), MotifDragWindowId, Frame);

	XtAddEventHandler(Frame,
		PropertyChangeMask,
		False,
		(XtEventHandler)DragWindowPropertyNotifyHandler,
		Form);

	return(Frame);
}

static Widget
CreateWorkWindow(Widget parent)
{
Widget Form;
Widget RootWindowProperties;
Widget DragWindowProperties;

	Form = XmCreateForm(parent, "WorkWindow", NULL, 0);
	RootWindowProperties = CreateRootProperties(Form);
	XtVaSetValues(RootWindowProperties,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
	XtManageChild(RootWindowProperties);

	DragWindowProperties = CreateDragProperties(Form);
	XtVaSetValues(DragWindowProperties,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, RootWindowProperties,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
	XtManageChild(DragWindowProperties);

	return(Form);
}

int
main(int argc, char **argv)
{
    XtAppContext app;
    Widget Shell;
    Widget MainWindow;
    Widget MenuBar;
    Widget WorkWindow;

    XtSetLanguageProc(NULL, NULL, NULL);

    Shell = XtVaAppInitialize(&app, "Shell",
			      NULL, 0,
			      &argc, argv,
			      FallBack,
			      NULL);
    MainWindow = XmCreateMainWindow(Shell, "MainWindow", NULL, 0);
    MenuBar = CreateMenuBar(MainWindow);
    XtManageChild(MenuBar);
    WorkWindow = CreateWorkWindow(MainWindow);
    XtManageChild(WorkWindow);

    XtManageChild(MainWindow);
    XtRealizeWidget(Shell);
    LessTifTestMainLoop(Shell);
    exit(0);
}
