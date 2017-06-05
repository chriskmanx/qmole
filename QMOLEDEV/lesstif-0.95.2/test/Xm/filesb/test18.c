/*
 * $Id: test18.c,v 1.3 2001/05/15 13:11:23 amai Exp $
 *
 * This is test3.c, changed by brian.harrawood@duke.edu to show a problem
 * on displays with more than one depth.
 *
 * This is bug #421187 :
 *
 * Bugs item #421187, was updated on 2001-05-03 15:10
 * You can respond by visiting: 
 * http://sourceforge.net/tracker/?func=detail&atid=108596&aid=421187&group_id=8596
 * 
 * Category: Xm library
 * Summary: XmCreateFileSelectionDialog & new visual
 * 
 * Initial Comment:
 * When creating an application which does not use the
 * default visual, screen, colormap, and screen depth -
 * any push button call backs generated from the
 * FileSelectionPopup generate this:
 * 
 * X Error of failed request:  BadMatch (invalid parameter
 * attributes)
 *   Major opcode of failed request:  66 (X_PolySegment)
 * 
 * This is the call into LessTif
 *   cnt = 0;
 *   XtSetArg(args[cnt], XtNcolormap,
 * the_Xvis->colormap()); cnt++;
 *   XtSetArg(args[cnt], XtNdepth,    the_Xvis->depth());
 * cnt++;
 *   XtSetArg(args[cnt], XtNvisual,   the_Xvis->visual());
 * cnt++;
 *   
 *   file_selection_popup =
 * XmCreateFileSelectionDialog(the_Xvis->getTopWidget(),       
 * "fs_popup",                                             args, cnt);
 *   
 * The args seem to be passed through to the Xt library. 
 * If I remove them, I get a bad match and no popup at
 * all.  Is this a problem with the
 * xmFileSelectionBoxWidgetClass?
 * 
 * 
 */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/FileSB.h>
#include <Xm/PushB.h>

#include "../../common/Test.h"


static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

XtAppContext app;
Widget toplevel, box, push;

int	popdown;


/* new globals */
Display     * G_dpy;
XVisualInfo * G_visinfo;
Visual      * G_visual;
int           G_depth;
Colormap      G_colormap;

static char *
XdbXmString2String(XmString xms)
{
  char *s = NULL;
  
#ifdef LesstifVersion
  if (xms == (XmString)XmUNSPECIFIED)
    {
      return "XmUNSPECIFIED";
    }
#endif
  XmStringGetLtoR(xms, XmFONTLIST_DEFAULT_TAG, &s);
  
  if (s == NULL)
    {
      return "(null)";
    }
  
  return s;
}

void
cb(Widget w, XtPointer client, XtPointer call)
{
  XmFileSelectionBoxCallbackStruct *cbp =
    (XmFileSelectionBoxCallbackStruct *)call;
  
  /* Print stuff */
  fprintf(stderr, "Callback Structure :\n");
  fprintf(stderr, "\tReason %s, event %p\n",
	  XdbReason2String(cbp->reason),
	  cbp->event);
  fprintf(stderr, "\tvalue '%s', length %d\n",
	  XdbXmString2String(cbp->value),
	  cbp->length);
  fprintf(stderr, "\tmask '%s', length %d\n",
	  cbp->mask ? XdbXmString2String(cbp->mask) : "(null)",
	  cbp->mask_length);
  fprintf(stderr, "\tdir '%s', length %d\n",
	  cbp->dir ? XdbXmString2String(cbp->dir) : "(null)",
	  cbp->dir_length);
  fprintf(stderr, "\tpattern '%s', length %d\n",
	  cbp->pattern ? XdbXmString2String(cbp->pattern) : "(null)",
	  cbp->pattern_length);
  
  /* Trigger popping down */
  popdown = 1;
}

void
pushme(Widget w, XtPointer client, XtPointer call)
{
  Arg           args[16];
  int           cnt;
  
  cnt = 0;
  XtSetArg(args[cnt], XtNcolormap, G_colormap); cnt++;
  XtSetArg(args[cnt], XtNdepth,    G_depth); cnt++;
  XtSetArg(args[cnt], XtNvisual,   G_visual); cnt++;
  
  box = XmCreateFileSelectionDialog(toplevel, "Box", args, cnt);
  XtAddCallback(box, XmNokCallback, cb, NULL);
  
  popdown = 0;
  XtManageChild(box);
#if 0
  set_path(box);
#endif
  
  
  {
    static XtWidgetGeometry Expected[] = {
      CWWidth | CWHeight            ,    6,   22,  286,  374, 0,0,0, /* Box */
      CWWidth | CWHeight | CWX | CWY,  191,   69,   83,   17, 0,0,0, /* Items */
      CWWidth | CWHeight | CWX | CWY,  191,   86,   83,  154, 0,0,0, /* ItemsListSW */
      CWWidth | CWHeight | CWX | CWY,   68,    0,   15,  135, 0,0,0, /* VertScrollBar */
      CWWidth | CWHeight | CWX | CWY,    0,  139,   64,   15, 0,0,0, /* HorScrollBar */
      CWWidth | CWHeight | CWX | CWY,    0,    0,   64,  135, 0,0,0, /* ItemsList */
      CWWidth | CWHeight | CWX | CWY,   11,  250,  264,   17, 0,0,0, /* Selection */
      CWWidth | CWHeight | CWX | CWY,   11,  267,  264,   31, 0,0,0, /* Text */
      CWWidth | CWHeight | CWX | CWY,    0,  308,  286,    2, 0,0,0, /* Separator */
      CWWidth | CWHeight | CWX | CWY,   11,  320,   66,   43, 0,0,0, /* OK */
      CWWidth | CWHeight | CWX | CWY,   77,  320,   66,   43, 0,0,0, /* Apply */
      CWWidth | CWHeight | CWX | CWY,  143,  320,   66,   43, 0,0,0, /* Cancel */
      CWWidth | CWHeight | CWX | CWY,  209,  320,   66,   43, 0,0,0, /* Help */
      CWWidth | CWHeight | CWX | CWY,   11,   11,  264,   17, 0,0,0, /* FilterLabel */
      CWWidth | CWHeight | CWX | CWY,   11,   69,  170,   17, 0,0,0, /* Dir */
      CWWidth | CWHeight | CWX | CWY,   11,   28,  264,   31, 0,0,0, /* FilterText */
      CWWidth | CWHeight | CWX | CWY,   11,   86,  170,  154, 0,0,0, /* DirListSW */
      CWWidth | CWHeight | CWX | CWY,  155,    0,   15,  135, 0,0,0, /* VertScrollBar */
      CWWidth | CWHeight | CWX | CWY,    0,  139,  151,   15, 0,0,0, /* HorScrollBar */
      CWWidth | CWHeight | CWX | CWY,    0,    0,  151,  135, 0,0,0, /* DirList */ 
    };
#if 0
    PrintDetails(box,Expected);
#endif
  };
  /*
    while (popdown == 0) {
    XtAppProcessEvent(XtWidgetToApplicationContext(w), XtIMAll);
    }
    
    fprintf(stderr, "Destroying the FSB\n");
    XtDestroyWidget(box);
  */
}

void
select_new_vis(Widget top)
{
  XVisualInfo * vinfo   = 0;
  XVisualInfo   rvinfo;
  int           choosen = 0;
  int           numvis  = 0;
  long          flags   = 0;
  Arg           args[16];
  int           cnt;
  int           d_depth;
  int           found_one = 0;

  Display * G_dpy = XtDisplay(top);
  
  int screen = DefaultScreen(G_dpy);
  
  d_depth = DefaultDepth(G_dpy, screen);

  rvinfo.screen = screen;
  
  flags = VisualScreenMask;
  
  vinfo = XGetVisualInfo(G_dpy, flags, &rvinfo, &numvis);
  
  if (!vinfo){
    fprintf(stderr, "XGetVisualInfo failed %s:%d\n", __FILE__, __LINE__);
    exit(1);
  }  
  
  if(numvis < 2){
    fprintf(stderr, "Test requires an Xserver with more than one visual\n");
    fprintf(stderr, "This Xserver has only 1\n");
    exit(1);
  }  
  
  choosen = 0;
  
  while(choosen < numvis){
    
    /* skip default visual */
    if((vinfo[choosen].visualid == 
	XVisualIDFromVisual(DefaultVisual(G_dpy, screen)))){
      choosen++;
      continue;
    }
    
    /* skip visuals at same depth */
    if(vinfo[choosen].depth == d_depth){
      choosen++;
      continue;
    }
    
    found_one = 1;
    break;
  }
  
  if(!found_one){
    fprintf(stderr, "No depth other than default depth of the display!\n");
    exit(0);
  }

  G_visinfo = (XVisualInfo *) XtMalloc (sizeof(XVisualInfo));
  
  memcpy(G_visinfo, &vinfo[choosen], sizeof(XVisualInfo));
  
  G_visinfo->visual = (Visual *) XtMalloc( sizeof(Visual));
  
  memcpy(G_visinfo->visual, vinfo[choosen].visual, sizeof(Visual));
  
  G_visual = G_visinfo->visual;
  
  G_depth = G_visinfo->depth;
  
  XFree((char *) vinfo);
  
  /* we need our own colormap */
  
  G_colormap   = XCreateColormap(G_dpy, 
				 RootWindow(G_dpy, screen),
				 G_visinfo->visual, 
				 AllocNone); 
  
  cnt = 0;
  XtSetArg(args[cnt], XtNcolormap, G_colormap); cnt++;
  XtSetArg(args[cnt], XtNdepth,    G_depth); cnt++;
  XtSetArg(args[cnt], XtNvisual,   G_visual); cnt++;
  
  XtSetValues(top, args, cnt);
  
}

int
main(int argc, char **argv)
{
  
  Arg           args[16];
  int           cnt;
  
#if 0
  make_tmp_dir_tree();
#endif
  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
			       &argc, argv, FallBack, NULL);
  
#if 1
  /* find a new visual */
  select_new_vis(toplevel);
  
#endif
  
  cnt = 0;
  XtSetArg(args[cnt], XtNcolormap, G_colormap); cnt++;
  XtSetArg(args[cnt], XtNdepth,    G_depth); cnt++;
  XtSetArg(args[cnt], XtNvisual,   G_visual); cnt++;
  
  push = XmCreatePushButton(toplevel, "push", args, cnt);
  XtVaSetValues(push,
		XtVaTypedArg, XmNlabelString, XtRString, "Push me !", 9,
		NULL);
  
  XtAddCallback(push, XmNactivateCallback, pushme, NULL);
  
  XtManageChild(push);
  
  XtRealizeWidget(toplevel);
  
#if 0
  LessTifTestWaitForIt(toplevel);
  LessTifTestPushButton(push);
  
  
  LessTifTestMainLoop(toplevel);
  
  exit(0);
  
#endif
  
  XtAppMainLoop(app);

  return(0);
}
