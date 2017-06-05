/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test8.c,v 1.7 2001/06/18 14:35:54 amai Exp $
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <X11/Intrinsic.h>
#include <X11/Shell.h>

#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/List.h>


static void create_gc(Widget w);
static void create_shell(Display *display,
                  char *app_name,
                  int app_argc,
                  char **app_argv);
static void initialise_objects(Widget parent);
static XmString string_append(XmString s1, XmString s2);

Widget shell = (Widget) NULL;
Widget form = (Widget) NULL;
Widget list1_sw = (Widget) NULL;
Widget list1 = (Widget) NULL;
Widget da = (Widget) NULL;

static GC  gc = (GC)NULL;


static void
da_expose(Widget          w,
   	  XtPointer       client_data,
	  XtPointer       call_data)
{
	XExposeEvent   *e = &((XmDrawingAreaCallbackStruct *) call_data)
	->event->xexpose;
	XmString       *items, *selected_items, out_string, tmp_string;
	Boolean         underline;
	int             item_count, selected_count;
	Widget          list = (Widget) client_data;
	Dimension       width, height, extent, item_extent, string_height;
	int             i;
	XmFontList      font_list;
	XRectangle      clip;

	/* Extract items, selected items and font from XmList */
	XtVaGetValues(list,
	              XmNitems, &items,
		      XmNitemCount, &item_count,
		      XmNselectedItems, &selected_items,
		      XmNselectedItemCount, &selected_count,
		      XmNfontList, &font_list,
		      NULL);
	XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
	underline = (selected_count > 0);
	create_gc(w);
	extent = 0;
	out_string = NULL;
	/*
	 * Form list items into a single compound string, inserting
	 * separators where needed to avoid drawing outside the XmDrawingArea
	 */
	for (i = 0; i < item_count; i++) {
		item_extent = XmStringWidth(font_list, items[i]);
		if (out_string != NULL && (extent + item_extent > width)) {
			extent = 0;
			out_string = string_append(out_string,
						 XmStringSeparatorCreate());
		}
		tmp_string = XmStringConcat(out_string, items[i]);
		XmStringFree(out_string);
		out_string = tmp_string;
		extent = extent + item_extent;
	}
	string_height = XmStringHeight(font_list, out_string);
	clip.x = e->x;
	clip.y = e->y;
	clip.width = e->width;
	clip.height = e->height;
	XSetClipRectangles(XtDisplay(w), gc, 0, 0, &clip, 1, YXBanded);
	/* Draw compound string, underlining the selected item if any */
	if (underline)
		XmStringDrawUnderline(XtDisplay(w), XtWindow(w), font_list,
		     out_string, gc, 0, (height - string_height) / 2, width,
				      XmALIGNMENT_CENTER,
				      XmSTRING_DIRECTION_L_TO_R, NULL,
				      selected_items[0]);
	else
		XmStringDraw(XtDisplay(w), XtWindow(w), font_list, out_string, gc,
			     0, (height - string_height) / 2, width,
			     XmALIGNMENT_CENTER,
			     XmSTRING_DIRECTION_L_TO_R, NULL);
	XmStringFree(out_string);
}

static XmString
string_append(XmString s1, XmString s2)
{
	XmString        s3 = XmStringConcat(s1, s2);
	XmStringFree(s1);
	XmStringFree(s2);
	return (s3);
}


static void 
create_gc(Widget w)
{
	XGCValues       values;
	XmFontList      fontlist;
	XFontStruct    *font, **font_struct_list;
	XFontSet        fontset;
	char          **font_name_list, *tag;
	XtPointer       t;
	XmFontContext   context;
	XmFontListEntry first_entry, entry;
	XmFontType      font_type;
	int             font_count;

	if (gc)
		return;

	/* Assume parent is Shell or BulletinBoard */
	XtVaGetValues(XtParent(w), XmNlabelFontList, &fontlist, NULL);
	if (fontlist)
		if (XmFontListInitFontContext(&context, fontlist)) {
			/* Get first entry */
			entry = XmFontListNextEntry(context);
			first_entry = entry;
			/* Walk down list looking for "Drawing" */
			while (entry) {
				tag = XmFontListEntryGetTag(entry);
				if (strcmp(tag, "Drawing") == 0) {
					XtFree(tag);
					break;
				}
				XtFree(tag);
				entry = XmFontListNextEntry(context);
			}
			XmFontListFreeFontContext(context);
			/* If we didn't find it use first entry */
			if (!entry)
				entry = first_entry;
			/* Get the font */
			t = XmFontListEntryGetFont(entry, &font_type);
			if (font_type == XmFONT_IS_FONT)
				font = (XFontStruct *) t;
			else {
				/*
				 * It's a font set - use the first one in the
				 * set
				 */
				fontset = (XFontSet) t;
				font_count = XFontsOfFontSet(fontset, &font_struct_list,
							   &font_name_list);
				font = font_struct_list[0];
			}
			values.font = font->fid;
			values.foreground = BlackPixelOfScreen(XtScreen(w));
			gc = XCreateGC(XtDisplay(w), XtWindow(w),
				       GCFont | GCForeground, &values);
		}
}

static  XmFontList f1;

static void
initialise_objects(Widget parent)
{
	const char from_s[] = "-*-fixed-*-*-*-*-24-*-*-*-*-*-*-*=japanese,-*-lucidabright-medium-i-*-*-24-*-*-*-*-*-*-*=large_italic,-*-lucidabright-demibold-r-*-*-24-*-*-*-*-*-*-*=large_bold,-*-lucidabright-demibold-i-*-*-14-*-*-*-*-*-*-*=small_italic,-*-lucidabright-demibold-r-*-*-14-*-*-*-*-*-*-*=small_bold";
	XrmValue from, to;
        static int _xd_initialised = 0;

	if ( _xd_initialised ) return;
	_xd_initialised = 1;
	from.size = strlen(from_s)+1;
	from.addr = XtMalloc ( from.size );
	strcpy ( from.addr, from_s );
	to.addr=NULL;
	XtConvert( parent, XmRString, &from, XmRFontList, &to);
	XtFree ( from.addr );

        f1 = *(XmFontList*)to.addr;
}

static void
create_shell(Display *display,
             char *app_name,
             int app_argc,
             char **app_argv)
{
	Arg al[12];                    /* Arg List */
	int ac;           /* Arg Count */
	XmString *list_items;          /* For list items */
	int list_item;                 /* Index for list_items */
	/* XmFontList f1; */

	ac = 0;
	XtSetArg(al[ac], XmNallowShellResize, TRUE); ac++;
	XtSetArg(al[ac], XmNargc, app_argc); ac++;
	XtSetArg(al[ac], XmNargv, app_argv); ac++;
	shell = XtAppCreateShell ( app_name, "XApplication", applicationShellWidgetClass, display, al, ac );

	initialise_objects ( shell );
	
	ac = 0;
	XtSetArg(al[ac], XmNautoUnmanage, FALSE); ac++;
	XtSetArg(al[ac], XmNlabelFontList, f1); ac++;
	XtSetArg(al[ac], XmNheight, 400); ac++;
	form = XmCreateForm ( shell, "form", al, ac );
	ac = 0;
	XtSetArg(al[ac], XmNfontList, f1); ac++;
	XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT); ac++;
	XtSetArg(al[ac], XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE); ac++;
	XtSetArg(al[ac], XmNitemCount, 5); ac++;
	list_items = (XmString *) XtMalloc ( sizeof (XmString) * 5 );
	list_items[0] = XmStringCreateLtoR("The", "large_italic");
	list_items[1] = XmStringCreateLtoR("Quick", "small_bold");
	/*list_items[2] = XmStringCreateLtoR("\033$BF|\033(B\033$BK\\\033(B", "japanese" );*/
	list_items[2] = XmStringCreateLtoR("F|  K\\", "japanese" );
	list_items[3] = XmStringCreateLtoR("Brown", "small_italic");
	list_items[4] = XmStringCreateLtoR("Fox", "large_bold");
	XtSetArg(al[ac], XmNitems, list_items); ac++;
	list1 = XmCreateScrolledList ( form, "list1", al, ac );
	list1_sw = XtParent ( list1 );

	for (list_item = 0; list_item < 5; list_item++ )
		XmStringFree (list_items [list_item]);
	XtFree ( (char *) list_items ); 
	da = XmCreateDrawingArea ( form, "da", al, 0 );

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_POSITION); ac++;
	XtSetArg(al[ac], XmNbottomPosition, 65); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;

	XtSetValues ( list1_sw,al, ac );

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_POSITION); ac++;
	XtSetArg(al[ac], XmNtopPosition, 65); ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	XtSetValues ( da,al, ac );

	XtManageChild(list1);
	XtAddCallback (da, XmNexposeCallback, da_expose, list1);
	XtManageChild(da);
	XtManageChild ( form);
}



XtAppContext app_context;
Display *display;       /*  Display             */

int
main (int argc, char *argv[])
{
	XtSetLanguageProc ( (XtAppContext) NULL, (XtLanguageProc) NULL, (XtPointer) NULL );
	XtToolkitInitialize ();
	app_context = XtCreateApplicationContext ();
	display = XtOpenDisplay (app_context, NULL, argv[0], "XApplication",
				 NULL, 0, &argc, argv);
	if (!display)
	{
	    printf("%s: can't open display, exiting...\n", argv[0]);
	    exit (-1);
	}
	create_shell ( display, argv[0], argc, argv );
	XtRealizeWidget (shell);
	
/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,  361,  266,   74,  400, 0,0,0, /* form */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   74,  260, 0,0,0, /* list1SW */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   74,  260, 0,0,0, /* list1 */
   CWWidth | CWHeight | CWX | CWY,    0,  260,   74,  140, 0,0,0, /* da */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(shell, Expected);
}

	LessTifTestMainLoop(shell);
	exit (0);
}
