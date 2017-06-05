/*
 * Try running this with :
 *	test12 -xrm "*sw.spacing: 15"
 *	test12 -xrm "*sw.scrollBarPlacement: top_left"
 */
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/ArrowB.h>
#include <Xm/ScrolledW.h>
#include <X11/IntrinsicP.h>
/*
#include <Xm/ScrolledWP.h>
#include <XmI/MacrosI.h>
*/

Widget toplevel, sw, ab;
Dimension st;

char *fallback[] = {
	"*sw.shadowThickness:	5",
	"*sw.background:	green",
	"*sw.topShadowColor:	red",
	"*sw.bottomShadowColor:	blue",
	"*ab.background:	yellow",
	"*ab.foreground:	black",
	NULL
};

void PrintParentChain(Widget w, int level)
{
	Widget	p;

	p = XtParent(w);

	if (p)
		PrintParentChain(p, level + 1);

	fprintf(stderr, "%s(%s)", XtName(w), XtClass(w)->core_class.class_name);

	if (level == 0)
		fprintf(stderr, "\n");
	else
		fprintf(stderr, ".");
}

void Doit(Widget w, XtPointer client, XtPointer call)
{
  Dimension	wid, ht, mw, mh, sp;
  Position	xx, yy;
  Widget	cl, vs, hs;

  XtVaGetValues(sw,
		XmNshadowThickness,			&st,
		XmNscrolledWindowMarginWidth,		&mw,
		XmNscrolledWindowMarginHeight,		&mh,
		XmNspacing,				&sp,
	NULL);
  XtVaGetValues(sw, XmNclipWindow, &cl, NULL);
  XtVaGetValues(cl, XmNx, &xx, XmNy, &yy, XmNwidth, &wid, XmNheight, &ht, NULL);

  fprintf(stderr, "ST %d, SP %d, MW %d, MH %d, CLIP GEO: %d %d %d %d\n",
	 st, sp, mw, mh,
	 xx, yy, wid, ht);

  XtVaGetValues(sw, XmNhorizontalScrollBar, &hs, NULL);
  XtVaGetValues(hs, XmNx, &xx, XmNy, &yy, XmNwidth, &wid, XmNheight, &ht, NULL);

  fprintf(stderr, "HSB GEO: %d %d %d %d ", xx, yy, wid, ht);

  XtVaGetValues(sw, XmNverticalScrollBar, &vs, NULL);
  XtVaGetValues(vs, XmNx, &xx, XmNy, &yy, XmNwidth, &wid, XmNheight, &ht, NULL);

  fprintf(stderr, "VSB GEO: %d %d %d %d\n", xx, yy, wid, ht);

  XtVaGetValues(ab, XmNx, &xx, XmNy, &yy, XmNwidth, &wid, XmNheight, &ht, NULL);

  fprintf(stderr, "Arrow GEO: %d %d %d %d\n", xx, yy, wid, ht);

  PrintParentChain(cl, 0);
  PrintParentChain(hs, 0);
  PrintParentChain(vs, 0);
  PrintParentChain(ab, 0);
}

int main(int argc, char **argv)
{
  XtAppContext app;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0,
	&argc, argv, fallback, NULL);

  sw  = XtVaCreateManagedWidget("sw", xmScrolledWindowWidgetClass, toplevel, 
		XmNscrollingPolicy,	XmAUTOMATIC,
		XmNwidth,		100,
		XmNheight,		100,
	NULL);

  ab = XtVaCreateManagedWidget("ab", xmArrowButtonWidgetClass, sw,
		XmNwidth,	300,
		XmNheight,	300,
	NULL);

  XtAddCallback(ab, XmNactivateCallback, Doit, NULL);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  508,  524,  100,  100, 0,0,0, /* sw */
   CWWidth | CWHeight | CWX | CWY,    7,    7,   63,   63, 0,0,0, /* ScrolledWindowClipWindow */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  300,  300, 0,0,0, /* ab */
   CWWidth | CWHeight | CWX | CWY,   81,    0,   19,   77, 0,0,0, /* VertScrollBar */
   CWWidth | CWHeight | CWX | CWY,    0,   81,   77,   19, 0,0,0, /* HorScrollBar */ 
    };
    PrintDetails(toplevel,Expected);
};
  LessTifTestMainLoop(toplevel);

  exit(0);
}
