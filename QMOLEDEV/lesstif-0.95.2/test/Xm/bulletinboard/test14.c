/*
From:        Frederick (Rick) A Niles <niles@axp745.gsfc.nasa.gov>
To:          lesstif@hungry.com
Subject:     allowShellResize affects window size only w/ Lesstif with XmNx/y set.
Date:        Wed, 21 Oct 98 00:10:58 -0400
cc:          niles@axp745.gsfc.nasa.gov


I have found the X parameter allowShellResize seems to allow Lesstif to
make the window as big as it would like to fit all the child widgets
even if XmNx and XmXy are set.  However, it states in the Motif man
page for XmDialogShell: "For the managed child of a DialogShell,
regardless of the value of the shell's XmNallowShellResize resource,
setting XmNx or XmNy sets the corresponding resource of the parent..."

I've included a modified BulletinBoard test1 (maybe a new test14?)
that demos the problem.  Under Motif 1.2 and 2.1 this window will show
up as 200x200, but with the lesstif-current it is 359x342.

	Thanks,
	Rick Niles.

------------------
*/
/* test for bulletinboard */
/* this is useful when checking motif */
#undef NEED_EDITRES

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#ifdef NEED_EDITRES
#include <X11/Xmu/Editres.h>
#endif
#include <stdio.h>

int
main(int argc, char **argv)
{
  Widget toplevel, one;
  XtAppContext app;
  Dimension thick = 0, wd, ht;
  Position x,y;
  Widget c,c2,c3;
  Arg args[10];
  int n=0;

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

  XtSetArg(args[n], XmNallowShellResize, True); n++;
  XtSetValues(toplevel, args, n); 

  one = XtVaCreateManagedWidget("One", 
                                xmBulletinBoardWidgetClass, 
                                toplevel,
/* 				XmNshadowThickness, 12, */
/* 				XmNshadowType, XmSHADOW_ETCHED_OUT_DASH, */
				XmNallowOverlap,	True,                 
				XmNmarginHeight, 20,
				XmNmarginWidth, 20,
				XtNheight,		200,	
				XtNwidth,		200,
				NULL);

  c = XtVaCreateManagedWidget("test1",
			      xmLabelWidgetClass,
			      one,
			       XtNx,		   20,		   
			       XtNy,		    20,		    
			       XmNwidth, 34,
			       XmNheight, 17,
			      NULL);
  c2 = XtVaCreateManagedWidget("test2",
			       xmPushButtonWidgetClass,
/* 			      xmLabelWidgetClass, */
			       one,
			       XtNx,		   200,		   
			       XtNy,		    200,		    
			       XmNwidth, 38,
			       XmNheight, 21,
			      NULL);
  c3 = XtVaCreateManagedWidget("test3",
			       xmPushButtonWidgetClass,
			       one,
			       XtNx, 300,
			       XtNy, 300,
			       XmNwidth, 38,
			       XmNheight, 21,
			       NULL);

#ifdef NEED_EDITRES
  XtAddEventHandler(toplevel, (EventMask)0, True,
                    (XtEventHandler)_XEditResCheckMessages, NULL);
#endif

  XtRealizeWidget(toplevel);

  XtVaGetValues(one, XmNshadowThickness, &thick, NULL);
  printf("shadow thickness: %d\n", thick);
  XtVaGetValues(c, XmNx, &x, XmNy, &y, XmNwidth, &wd, XmNheight, &ht, NULL);
  printf("label x,y: %d %d  wd,ht: %d %d\n", x, y, wd, ht);


/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    5,  451,  200,  200, 0,0,0, /* One */
   CWWidth | CWHeight | CWX | CWY,   20,   20,   34,   17, 0,0,0, /* test2 */
   CWWidth | CWHeight | CWX | CWY,  200,  200,   38,   21, 0,0,0, /* test1 */
   CWWidth | CWHeight | CWX | CWY,  300,  300,   38,   21, 0,0,0, /* test3 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
  exit(0);
}

