/* $Header: /cvsroot/lesstif/lesstif/test/Xm/rowcolumn/test54.c,v 1.4 2002/02/02 11:43:11 amai Exp $
 *
 *
 * Test indicating problems with menu focus handling code.
 * JAC, May 10, 1999.
 *
 * Run and choose Graphics->Customize->[Any dial]->[Any option]
 * Get core dump.
 *
 */

#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/CascadeB.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/ToggleB.h>
#include <Xm/TearOffBP.h> /* for xmTearOffButtonWidgetClass */


/*************************************************************
 *
 *
 * Library/Support Routines
 *
 *
 *************************************************************/

static Widget GXMenuBar, GXMenuPane, GXTopWidget;
static int GXDopupReturn;	     /* return code of a popup menu */
static XtAppContext GXAppContext;

typedef struct {
   char*                  name;           /* name of the button */
   void                  (*func)();       /* Callback to be invoked */
   XtPointer              data;           /* Data for the callback */
   WidgetClass            class;          /* the class of the button */
   int                    indicator;      /* 1-of or N-of */
   Widget                 submenu;        /* the widgetid of the submenu */
 } GXMenuStruct;

#define GXMenuBarWidgetClass xmRowColumnWidgetClass
#define GXSeparatorWidgetClass xmSeparatorWidgetClass
#define GXPushButtonWidgetClass xmPushButtonWidgetClass
#define GXactivateCallback XmNactivateCallback
#define GXLabelWidgetClass xmLabelWidgetClass
#define GXToggleButtonWidgetClass xmToggleButtonWidgetClass
#define GXCascadeButtonWidgetClass xmCascadeButtonWidgetClass
#define GXONE_OF_MANY XmONE_OF_MANY
#define GXN_OF_MANY XmN_OF_MANY
#define GXToggleButtonGetState XmToggleButtonGetState
#define GXToggleButtonSetState XmToggleButtonSetState
#define GXvalueChangedCallback XmNvalueChangedCallback
#define GXMixedModeWidgetClass (WidgetClass) -2

#define GXWatchWidget(a,b) a
#define GX_INSENSITIVE ((XtPointer)1) 

/* add a simple pushbutton */
#define GXaddtop GXaddpush
#define GXaddpush(MENU,LABEL,HANDLER,DATA) \
                 MENU.name=LABEL; \
                 MENU.func=HANDLER; \
                 MENU.class=GXPushButtonWidgetClass;\
                 MENU.submenu=NULL;\
                 MENU.data=(XtPointer) DATA

/* add a cascade button for a sub menu */
#define GXaddsub(MENU,LABEL,HANDLER,SUBMENU) \
                 MENU.name=LABEL; \
                 MENU.func=HANDLER; \
                 MENU.class=GXCascadeButtonWidgetClass;\
                 MENU.data=NULL;\
                 MENU.submenu=SUBMENU




static void DopupCB(Widget w, XtPointer client, XtPointer call);
void GXMakeMenu(char *title, Widget menu, GXMenuStruct * menulist, int
		nitems, Boolean IsPopup, Boolean tearable);

static Boolean DopupDone;
static Boolean ExecutingPopup = FALSE;
static int DopupReturn;



void GXEventHandler(void)
/*-
  Purpose:  this routine implements a simple event handler, allowing for
  dial+button events.  It is intended to be called from within an event
  loop, and it will handle exactly one event.

  This is GXMainLoop without the loop!
-*/
{
  static XEvent event;

  XtAppNextEvent(GXAppContext, &event);
  XtDispatchEvent(&event);
}

void GXGetPointerPosition(int *x, int *y)
{
  int win_x, win_y;
  unsigned int mask;
  Window root, child;
  Display *GXDisplay=XtDisplay(GXTopWidget);

  XQueryPointer(GXDisplay, XtWindow(GXTopWidget),
		&root, &child,
		x,y,
		&win_x, &win_y,
		&mask);
}

Boolean GXIsToggle(Widget w)
{
    return (XtClass(w) == xmToggleButtonWidgetClass);
}

Widget
GXCreatePopupMenu(Widget parent, GXMenuStruct * menu_tree,
		  char *title, int cnt)
/*-
  create a popup menu child of parent, titled title.  the cnt children are
  described in menu_tree.

  The child may be created in the overlay planes if 1) USE_OVERLAYS was defined
  at compile time, and 2) overlay planes are available.

  Colormap management for menus created by this routine in the overlay
  planes is automatic.
-*/
{
    Widget popup;
    int n;
    Arg args[10];

    n = 0;

    popup = XmCreatePopupMenu(parent, title, args, n);
    GXMakeMenu(title, popup, menu_tree, cnt, TRUE, TRUE);
    return (popup);
}

Widget
GXCreatePulldownMenu(Widget parent, GXMenuStruct * menu_tree,
		     char *title, int cnt)
/*-
  create a pulldown menu child of parent, titled title.  the cnt children are
  described in menu_tree.

  The child may be created in the overlay planes if 1) USE_OVERLAYS was defined
  at compile time, and 2) overlay planes are available.

  Colormap management for menus created by this routine in the overlay
  planes is automatic.
-*/
{
    Widget pulldown;
    int n;
    Arg args[10];

    n = 0;
    pulldown = XmCreatePulldownMenu(parent, title, args, n);
    GXMakeMenu(title, pulldown, menu_tree, cnt, FALSE, TRUE);
    return (pulldown);
}

Widget
GXCreateMenuPane(Widget parent)
/*-
  create the common menu pane to hold all pulldown menus
-*/
{
    Widget pane;
    int n;
    Arg args[10];

    if (GXMenuPane)		/* there can be only one */
	return (GXMenuPane);

    n = 0;
    pane = XmCreatePulldownMenu(parent, " ", args, n);
    return (pane);
}

static Widget
 GXAddToMenuBar(Widget menu_bar, char *name, Widget pulldown)
/*-
 * add pulldown to menu_bar with button label name 
 -*/
{
    Widget cascade;
    Arg args[10];
    register int n;

    n = 0;
    XtSetArg(args[n], XmNsubMenuId, pulldown);
    n++;
    cascade = GXWatchWidget(XmCreateCascadeButton(menu_bar, name, args,
						  n),(int)NULL); 
    XtManageChild(cascade);
    return (NULL);
}

Widget
GXCreateMenuBarPulldown(Widget parent, GXMenuStruct * menu_tree,
			char *title, int cnt)
/*- 
  create a pulldown menu child of the menubar parent, titled title.
  the new menu is added to the menubar parent.
  the cnt children are described in menu_tree.

  The child may be created in the overlay planes if 1) USE_OVERLAYS was defined
  at compile time, and 2) overlay planes are available.

  Colormap management for menus created by this routine in the overlay
  planes is automatic.
-*/
{
    int n;
    Arg args[10];
    Widget pulldown;

    n = 0;
    pulldown = XmCreatePulldownMenu(parent, title, args, n);
    GXMakeMenu(NULL, pulldown, menu_tree, cnt, FALSE, TRUE);
    GXAddToMenuBar(parent, title, pulldown);
    return (pulldown);
}

Widget
GXCreateMenuBar(Widget parent, char *name)
/*-
 * create a managed menu bar child of parent with the default
 * resource values
 -*/
{
    Widget menu_bar;

    menu_bar = XmCreateMenuBar(parent, name, NULL, 0);
    XtManageChild(menu_bar);
    return (menu_bar);
}


void GXMakeMenu(char *title, Widget menu, GXMenuStruct * menulist, int
		nitems, Boolean IsPopup, Boolean tearable)
/*-
  create the menu defined by GXMenuStruct, and add callbacks.
  up to four callbacks are added for each button, in the following order:
      1) GXButtonPressHookCB, if defined
      2) menulist[i].func, if defined
      3) DopupCB

   For ToggleButtons and PushButtons with no callback, the button is
   greyed out (insensitive).

   This routine allows for RadioBox-type behavior in the same menu as
   ToggleButtons, and even more than one radio box in the same menu.
   Separators are used to delimit different RadioBoxes.

-*/
{
    Arg wargs[10];
    int GotRadio, GotCheck, MixedMode, GotMixed;
    Widget w;
    int n, i, defargcnt = 0;


    GotMixed = GotRadio = GotCheck = FALSE;
    for (i = 0; i < nitems; i++) {
	if (menulist[i].class == GXToggleButtonWidgetClass) {
	    if (menulist[i].indicator == GXONE_OF_MANY)
		GotRadio = TRUE;
	    if (menulist[i].indicator == GXN_OF_MANY)
		GotCheck = TRUE;
	}
	if (menulist[i].class == GXMixedModeWidgetClass)
	  GotMixed = TRUE;
    }
    MixedMode = ((GotRadio && GotCheck)||GotMixed);

    /* 
     * Allocate a widget list to hold all 
     * button widgets.
     */
    /* 
     * If a title is given, create Label and Separator widgets.
     */
    if (title) {
	n = 0;
	XtCreateManagedWidget(title, GXLabelWidgetClass, menu,
			      wargs, defargcnt + n);
	XtCreateManagedWidget("separator", GXSeparatorWidgetClass,
			      menu, wargs, defargcnt);
    }
    /* 
     * Create an entry for each item in the menu.
     */
    for (i = 0; i < nitems; i++) {
	/* 
	 * add in the next item according to its class
	 */

	if (menulist[i].class == GXSeparatorWidgetClass) {
	    XtCreateManagedWidget("separator",
				  menulist[i].class,
				  menu, wargs, defargcnt);
	} else if (menulist[i].class == GXPushButtonWidgetClass) {
	    w = 
	      GXWatchWidget(XtCreateManagedWidget(menulist[i].name,
					    menulist[i].class,
					    menu, wargs,
					    defargcnt),(int)NULL);
	    switch ((int) menulist[i].func) {
	    case 0:
		break;
	    case (int) GX_INSENSITIVE:
		XtSetSensitive(w, FALSE);
		break;
	    default:
		XtAddCallback(w, GXactivateCallback,
			      menulist[i].func, menulist[i].data);
	    }
	    XtAddCallback(w, GXactivateCallback,
			  DopupCB, menulist[i].data);
	} else if (menulist[i].class == GXToggleButtonWidgetClass) {
	    /* ToggleButton (Radio or Check) */
	    n = 0;
	    XtSetArg(wargs[defargcnt + n], XmNvisibleWhenOff, TRUE);  n++;
	    /* XtSetArg(wargs[defargcnt + n], XmNindicatorSize, 15);  n++;
	     */
	    XtSetArg(wargs[defargcnt + n], XmNindicatorType, 
		     menulist[i].indicator);  n++;
	    w = GXWatchWidget(XtCreateManagedWidget(menulist[i].name,
					      menulist[i].class,
					      menu, wargs,
					      defargcnt + n),(int)NULL);
	    switch ((int) menulist[i].func) {
	    case 0:
		break;
	    case (int) GX_INSENSITIVE:
		XtSetSensitive(w, FALSE);
		break;
	    default:
		XtAddCallback(w, GXvalueChangedCallback,
			      menulist[i].func, menulist[i].data);
		break;
	    }
	    XtAddCallback(w, GXvalueChangedCallback,
			  DopupCB, menulist[i].data);

	} else if (menulist[i].class == GXLabelWidgetClass) {
	    n = 0;
	    w = XtCreateManagedWidget(menulist[i].name,
					      menulist[i].class,
					      menu, wargs, 
					      defargcnt + n);
	} else if (menulist[i].class == GXCascadeButtonWidgetClass) {
	    n = 0;
	    XtSetArg(wargs[n + defargcnt], XmNsubMenuId, menulist[i].submenu);
	    n++;
	    w =
	      GXWatchWidget(XtCreateManagedWidget(menulist[i].name,
						  GXCascadeButtonWidgetClass,
						  menu, wargs, 
						  defargcnt + n),(int)NULL);
	}
    }
    if ((GotRadio) && !(MixedMode))
	XtVaSetValues(menu,
		      XmNradioBehavior, TRUE,
		      XmNradioAlwaysOne, TRUE,
		      NULL);
}

static void DopupCB(Widget w, XtPointer client, XtPointer call)
/*-
  Gets the return code for a popup posted via GXDopup.
-*/
{
    GXDopupReturn = (int) client;	/* pass return code to global
					 * variable */
    if (!ExecutingPopup)
	return;
    DopupDone = TRUE;
    DopupReturn = (int) client;
}

static void DopupDestroyCB(Widget w, XtPointer client, XtPointer call)
/*-
  Handler for GXDopup to deal with cancellation of the menu.  Returns -1
  return code.
-*/
{
    if (DopupDone)
	return;
    DopupDone = TRUE;
    DopupReturn = -1;
}


int GXdopup(Widget menu, XEvent * event)
/*-
  pops up "menu" and waits for a reply by implementing its own event loop.
  doesn't return to its caller until: 1) a menu item is selected (in which
  case GXdopup returns the client_data value of the menu item) or 2) the
  menu is canceled via escape or clicking outside the menu, in which case
  the GXdopup returns -1.
-*/
{
    unsigned char typ;
    XButtonEvent newbuttonevent;
    if (!menu)
	return (-1);
    /* enter dopup mode */
    DopupDone = FALSE;
    ExecutingPopup = TRUE;

    if (XtClass(menu)!=GXMenuBarWidgetClass)
      return(-1);
    XtVaGetValues(menu,
		  XmNrowColumnType, &typ,
		  NULL);
    if (typ!=XmMENU_POPUP)
	return(-1);
    XtAddCallback(XtParent(menu), XtNpopdownCallback,
		  (XtCallbackProc) DopupDestroyCB, NULL);

    if (!event) {
      GXGetPointerPosition((int*)&newbuttonevent.x_root,
			   (int*)&newbuttonevent.y_root);
      event = (XEvent *) & newbuttonevent;
    }
    XmMenuPosition(menu, (XButtonEvent *) event);
    XtManageChild(menu);

    while (DopupDone == FALSE) {
	GXEventHandler();
    }
    /* exit dopup mode */
    ExecutingPopup = FALSE;
    XtRemoveCallback(XtParent(menu), XtNpopdownCallback,
		     (XtCallbackProc) DopupDestroyCB, NULL);
    return (DopupReturn);
}

void GXfreepup(Widget w)
{
  XtDestroyWidget(w);
}

/*************************************************************
 *
 *
 * BEGIN MAIN CODE
 *
 *
 *************************************************************/

#define MAXDIAL 6 
static char *dial_string[]={
  "Warp level", "Antimatter flux", "Dilithium level",
  "Photon reserves", "Impeller strength", "Graviton flux",
};

static void dialbox_h(Widget w, int client, caddr_t call)
{
  Widget popup;
  int n,i;
  GXMenuStruct menu[MAXDIAL];

  n=0;
  for (i=0;i<MAXDIAL;i++) {
    GXaddpush(menu[i], dial_string[i], NULL, i);
  }
  popup=GXCreatePopupMenu(w,menu,"Silly menu",MAXDIAL);
  
  n=GXdopup(popup,NULL);
  GXfreepup(popup);
}

Widget CreateDialBoxMenu(Widget parent)
{
  Widget w;
  GXMenuStruct menu[30];
  int n;
  
  n=0;
  GXaddpush(menu[n], "Dial 0", dialbox_h, 0); n++;
  GXaddpush(menu[n], "Dial 1", dialbox_h, 1); n++;
  GXaddpush(menu[n], "Dial 2", dialbox_h, 2); n++;
  GXaddpush(menu[n], "Dial 3", dialbox_h, 3); n++;
  GXaddpush(menu[n], "Dial 4", dialbox_h, 4); n++;
  GXaddpush(menu[n], "Dial 5", dialbox_h, 5); n++;
  GXaddpush(menu[n], "Dial 6", dialbox_h, 6); n++;
  GXaddpush(menu[n], "Dial 7", dialbox_h, 7); n++;
  w=GXCreatePulldownMenu(parent,menu,"Customization menu",n);
  return(w);
}

Widget create_graphics_menu(Widget PDMenuPane)
{
  GXMenuStruct menu[50];
  Widget w;
  int n;

  n=0;
  w=CreateDialBoxMenu(PDMenuPane);
  GXaddsub(menu[n], "Customize",NULL,w); n++;
  
  w=GXCreateMenuBarPulldown(GXMenuBar, menu,"Graphics",n);
  return w; /* ? */
}

int main(int argc, char **argv)
{
  Widget toplevel, w;

  GXTopWidget= toplevel = 
    XtVaAppInitialize(&GXAppContext, "rc-test1", NULL, 0,
		      &argc, argv, NULL, NULL);

  GXMenuBar=XmCreateMenuBar(toplevel,"menubar",NULL,0);
  XtManageChild(GXMenuBar);

  /* create a shared menu pane */
  GXMenuPane=GXCreateMenuPane(GXMenuBar);
  w=create_graphics_menu(GXMenuPane);

  XtRealizeWidget(GXTopWidget);

  {
    static XtWidgetGeometry Expected[] = {
      CWWidth | CWHeight            ,    0,    0,   74,   31, 0,0,0, /*
								       menubar */
      CWWidth | CWHeight | CWX | CWY,    5,    5,   64,   21, 0,0,0, /*
								       Graphics */
    };
    /* toplevel should be replaced with to correct applicationShell */
    PrintDetails(GXTopWidget, Expected);
  }
  LessTifTestMainLoop(GXTopWidget);
  exit(0);
}
