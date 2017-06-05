/* $Header: /cvsroot/lesstif/lesstif/test/extra/cmap/test1.c,v 1.1 2002/05/15 10:14:42 amai Exp $ */
/*
 * Copyright 1993 John L. Cwikla
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * Wolfram Research, Inc not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.  John L. Cwikla and Wolfram Research, Inc make no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
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
 *  John L. Cwikla
 *  X Programmer
 *  Wolfram Research Inc.
 *
 *  cwikla@wri.com
*/

#include <stdlib.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/time.h>

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/CoreP.h>
#include <X11/ShellP.h>

#include "Cmap.h"

#include "../../common/Test.h"

#define APPNAME "CmapTest"
#define APPCLASS "CmapTest"

static double Value(_n1, _n2, _hue)
double _n1;
double _n2;
double _hue;
{
  double val;

  if (_hue > 360.0)
	_hue -= 360.0;
  if (_hue < 0.0)
	_hue += 360.0;

  if (_hue < 60.0)
	val = _n1 + (_n2 - _n1) * _hue/60.0;
  else
  if (_hue < 180.0)
	val = _n2;
  else
  if (_hue < 240.0)
	val = _n1 + (_n2 - _n1) * (240.0 - _hue)/60.0;
  else
	val = _n1;

  return val;
}

static void HLStoRGB(_h, _l, _s, _r, _g, _b)
double *_h;
double *_l;
double *_s;
double *_r;
double *_g;
double *_b;
{
  double m1, m2;

  m2 = ((*_l < 0.5) ? (*_l) * (1.0 + *_s) : *_l + *_s - (*_l)*(*_s));

  m1 = 2.0 * (*_l) - m2;

  if  (*_s == 0)
	*_r = *_g = *_b = *_l;
  else
  {
	*_r = Value(m1, m2, (double)(*_h + 120.0));
	*_g = Value(m1, m2, (double)(*_h));
	*_b = Value(m1, m2, (double)(*_h - 120.0));
  }
}


static void QuitIt(_w, _nil, _event)
Widget _w;
void *_nil;
XEvent *_event;
{
	if (_event->type == ButtonPress)
		if (((XButtonEvent *)_event)->button == 3)
		{
			printf("Have a nice day. --JLC\n");
			exit(1);
		}
}

int
main(argc, argv)
int argc;
char *argv[];
{
	Widget cmapWidget, toplevel;
	XtAppContext app;
	Display *theDisplay;
	int theScreenNumber;
	Pixel pixels[128];
	int last;
	double h, l, s, r, g, b;
	Arg warg[3];
	int n, i;
	XColor xcolor;

	XtToolkitInitialize();
	app = XtCreateApplicationContext();

	theDisplay = XtOpenDisplay (app, NULL, APPNAME, APPCLASS, 
		NULL, 0, &argc, argv);

	if (!theDisplay)
	{
	printf("%s: can't open display, exiting...", APPNAME);
	exit (0);
	}

	theScreenNumber = DefaultScreen(theDisplay);

	toplevel = XtAppCreateShell (APPNAME, APPCLASS,
		applicationShellWidgetClass, theDisplay, NULL, 0);

	last = 0;

	l = 0.50;
	s = 0.50;
	for(i=0;i<128;i++)
	{
		h = (double)i*360.0/((double)128+1.0);
		HLStoRGB(&h, &l, &s, &r, &g, &b);
		xcolor.red = (short)(r * 65535.0);
		xcolor.green = (short)(g * 65535.0);
		xcolor.blue = (short)(b * 65535.0);

		if (XAllocColor(theDisplay, DefaultColormap(theDisplay, theScreenNumber), &xcolor))
		{
			pixels[last] = xcolor.pixel;
			last++;
		}
	}
	
	if (last == 0)
	{
		fprintf(stderr, "Unable to allocate any colors for demo. Exiting...\n");
		fprintf(stderr, "Have a nice day. -- JLC\n");
		exit (1);
	}

	n = 0;
	XtSetArg(warg[n], XtNfirstIndex, 0); n++;
	XtSetArg(warg[n], XtNlastIndex, last-1); n++;
	XtSetArg(warg[n], XtNmappingTable, (XtPointer)pixels); n++;
	cmapWidget = XtCreateManagedWidget("CW 1", cmapWidgetClass, toplevel, warg, n);

	XtRealizeWidget(toplevel);

	XtAddEventHandler(cmapWidget, ButtonPressMask, FALSE, QuitIt, NULL);

	printf("Press mouse button 3 to exit.\n");
	printf("Press mouse button 1 to select a new box.\n");

	printf("Use the <osfUp, osfDown, osfLeft, osfRight> keys to move <up, down, left, right>\n");

	
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  272,  239, 0,0,0, /* CW 1 */ 
    };
    PrintDetails(toplevel,Expected);
};
	LessTifTestMainLoop(toplevel);
	exit(0);
}
