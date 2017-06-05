/* $Id: test1.c,v 1.1 2002/05/14 22:49:04 dannybackx Exp $ */
/*
 * Copyright 1994 John L. Cwikla
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * Wolfram Research, Inc not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.	John L. Cwikla and Wolfram Research, Inc make no
 * representations about the suitability of this software for any
 * purpose.	It is provided "as is" without express or implied warranty.
 *
 * John L. Cwikla and Wolfram Research, Inc disclaim all warranties with
 * regard to this software, including all implied warranties of
 * merchantability and fitness, in no event shall John L. Cwikla or
 * Wolfram Research, Inc be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of
 * use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Author:
 *	John L. Cwikla
 *	X Programmer
 *	Wolfram Research Inc.
 *
 *	cwikla@wri.com
*/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Core.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/RowColumn.h>
#include <Xm/DialogS.h>

#include <stdio.h>

#include "SmartMB.h"

#define APPNAME "SmartMBTest"
#define APPCLASS "SmartMBTest"

#include <Xm/XmP.h>

#if DEBUG
static void
dump_box(XmKidGeometry box)
{
    if (!box)
	return;
    while (box->kid) {
	printf("    KID %s REQ %08x X %-5d Y %-5d W %-5d H %-5d B %-5d\n",
		XtName(box->kid), box->box.request_mode, box->box.x, box->box.y,
		box->box.width, box->box.height, box->box.border_width);
	box++;
    }
}

static void
dump_layout(XmGeoRowLayout rows, XmKidGeometry boxes)
{
    if (!rows)
	return;
    --rows;
    do {
	rows++;
	printf("ROW: %p\n", rows);
	printf("  end: %d fixup: %p even_width: %d even_height: %d\n",
		rows->end, rows->fix_up, rows->even_width, rows->even_height);
	printf("  min_height: %d stretch_height: %d uniform_border %d\n",
		rows->min_height, rows->stretch_height, rows->uniform_border);
	printf("  border: %d fill_mode: %d fit_mode: %d sticky_end: %d\n",
		rows->border, rows->fill_mode, rows->fit_mode, rows->sticky_end);
	printf("  space_above: %d space_end: %d space_between: %d\n",
		rows->space_above, rows->space_end, rows->space_between);
	printf("  max_box_height: %d boxes_width: %d fill_width %d\n",
		rows->max_box_height, rows->boxes_width, rows->fill_width);
	printf("  box_count: %d\n", rows->box_count);
	dump_box(boxes);
	boxes += rows->box_count + 1;
    } while (!rows->end);
}

static void
dump_matrix(XmGeoMatrix geo) {
    printf("MATRIX: composite: %p instigator %p boxes %p\n",
	geo->composite, geo->instigator, geo->boxes);
    printf("  layouts: %p margin_w: %d margin_h: %d stretch_boxes: %d\n",
	geo->layouts, geo->margin_w, geo->margin_h, geo->stretch_boxes);
    printf("  uniform_border: %d border: %d max_major: %d boxes_minor: %d\n",
	geo->uniform_border, geo->border, geo->max_major, geo->boxes_minor);
    printf("  fill_minor: %d width: %d height: %d set: %p\n",
	geo->fill_minor, geo->width, geo->height, geo->set_except);
    printf("  almost: %p no_geo: %p extension %p destruct: %p\n",
	geo->almost_except, geo->no_geo_request, geo->extension,
	geo->ext_destructor);
    printf("  arrange: %p major: %d\n",
	geo->arrange_boxes, geo->major_order);
    dump_layout((XmGeoRowLayout)geo->layouts, geo->boxes);
}
#endif

int
main(argc, argv)
int argc;
char *argv[];
{
	Widget row, toplevel, smb, defButton;
	XtAppContext app;
	Display *theDisplay;
	Arg warg[8];
	int n, i;

	XtToolkitInitialize();
	app = XtCreateApplicationContext();
	
	theDisplay = XtOpenDisplay (app, NULL, APPNAME, APPCLASS, 
		NULL, 0, &argc, argv);

	if (!theDisplay)
	{
		printf("%s: can't open display, exiting...", APPNAME);
		exit (0);
	}

#if 1
	toplevel = XtAppCreateShell (APPNAME, APPCLASS,
		applicationShellWidgetClass, theDisplay, NULL, 0);

	smb = XtCreateManagedWidget("SmartMessageBoxInformation", xmSmartMessageBoxWidgetClass,
		toplevel, NULL, 0);

	XtCreateManagedWidget("Separator", xmSeparatorWidgetClass, smb, NULL, 0);
	XtCreateManagedWidget("MessageLabel", xmLabelWidgetClass, smb, NULL, 0);

	n = 0;
	defButton = XtCreateManagedWidget("OK", xmPushButtonWidgetClass, smb, warg, n);
	XtCreateManagedWidget("Help", xmPushButtonWidgetClass, smb, NULL, 0);

	n = 0;
	XtSetArg(warg[n], XmNdefaultButton, defButton); n++;
	XtSetValues(smb, warg, n);

	XtRealizeWidget(toplevel);

#endif

	toplevel = XtAppCreateShell (APPNAME, APPCLASS,
		applicationShellWidgetClass, theDisplay, NULL, 0);

	smb = XtCreateManagedWidget("SmartMessageBox", xmSmartMessageBoxWidgetClass,
		toplevel, NULL, 0);

	XtCreateManagedWidget("Separator", xmSeparatorWidgetClass, smb, NULL, 0);
	row = XtCreateManagedWidget("RowCol", xmRowColumnWidgetClass, smb, NULL, 0);

	for(i=0;i<3;i++)
		XtCreateManagedWidget("Control Button", xmPushButtonWidgetClass, row, NULL, 0);

	n = 0;
	defButton = XtCreateManagedWidget("OK", xmPushButtonWidgetClass, smb, warg, n);
	XtCreateManagedWidget("Apply", xmPushButtonWidgetClass, smb, NULL, 0);
	XtCreateManagedWidget("Cancel", xmPushButtonWidgetClass, smb, NULL, 0);
	XtCreateManagedWidget("Help", xmPushButtonWidgetClass, smb, NULL, 0);

	n = 0;
	XtSetArg(warg[n], XmNdefaultButton, defButton); n++;
	XtSetValues(smb, warg, n);

	XtRealizeWidget(toplevel);

#if 1
	toplevel = XmCreateDialogShell(smb, "popup_shell", NULL, 0);

	smb = XtCreateWidget("SmartMessageBox", xmSmartMessageBoxWidgetClass,
		toplevel, NULL, 0);

	XtCreateManagedWidget("NoSeparatorLabel", xmLabelWidgetClass, smb, NULL, 0);

	n = 0;
	defButton = XtCreateManagedWidget("OK", xmPushButtonWidgetClass, smb, warg, n);
	XtCreateManagedWidget("Help", xmPushButtonWidgetClass, smb, NULL, 0);

	n = 0;
	XtSetArg(warg[n], XmNdefaultButton, defButton); n++;
	XtSetValues(smb, warg, n);

	XtManageChild(smb);
	XtPopup(toplevel, XtGrabNone);

	toplevel = XtAppCreateShell (APPNAME, APPCLASS,
		applicationShellWidgetClass, theDisplay, NULL, 0);

	smb = XtCreateManagedWidget("SmartMessageBox", xmSmartMessageBoxWidgetClass,
		toplevel, NULL, 0);

	for(i=0;i<5;i++)
		defButton = XtCreateManagedWidget("Buttons", xmPushButtonWidgetClass, smb, NULL, 0);

	XtSetArg(warg[n], XmNdefaultButton, defButton); n++;
	XtSetValues(smb, warg, n);

	XtRealizeWidget(toplevel);
#endif

	
	/* core dumps, so we can't get geometry */
	PrintDetails(toplevel,NULL);
	LessTifTestMainLoop(toplevel);
	exit(0);
}
