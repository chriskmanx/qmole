#include <Xm/CutPaste.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <stdio.h>

static void to_clipbd(), from_clipbd();

int
main(int argc, char **argv)
{
    Widget toplevel, rowcol, button;
    XtAppContext app;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "Demo", NULL, 0,
				 &argc, argv, NULL, NULL);

    rowcol = XtVaCreateWidget("rowcol", xmRowColumnWidgetClass,
			      toplevel, NULL);

    button = XtVaCreateManagedWidget("button1", xmPushButtonWidgetClass,
				     rowcol, XtVaTypedArg, XmNlabelString,
				     XmRString, "Copy To Clipboard", 18,
				     NULL);

    XtAddCallback(button, XmNactivateCallback, to_clipbd, "text");

    button = XtVaCreateManagedWidget("button2", xmPushButtonWidgetClass,
				     rowcol, XtVaTypedArg, XmNlabelString,
				     XmRString, "Retrieve From Clipboard", 24,
				     NULL);

    XtAddCallback(button, XmNactivateCallback, from_clipbd, NULL);

    XtManageChild(rowcol);
    XtRealizeWidget(toplevel);
    
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   50,   50,  156,   59, 0,0,0, /* rowcol */
   CWWidth | CWHeight | CWX | CWY,    3,    3,  150,   25, 0,0,0, /* button1 */
   CWWidth | CWHeight | CWX | CWY,    3,   31,  150,   25, 0,0,0, /* button2 */ 
    };
    PrintDetails(    toplevel ,Expected);
};
   LessTifTestMainLoop(    toplevel );
   exit(0);
}

static void
to_clipbd(Widget w, XtPointer client_data, XtPointer call_data)
{
    long item_id = 0;
    int status;
    XmString clip_label;
    char buf[40];
    static int cnt;
    Display *dpy = XtDisplayOfObject(w);
    Window window = XtWindowOfObject(w);
    char *data = (char *)client_data;

    sprintf(buf, "%s-%30d", data, ++cnt);

    clip_label = XmStringCreateLocalized("to_clipbd");

    do {
	status = XmClipboardStartCopy(dpy, window, clip_label, CurrentTime,
				      NULL, NULL, &item_id);
    } while (status == ClipboardLocked);

    XmStringFree(clip_label);

    do {
	status = XmClipboardCopy(dpy, window, item_id, "STRING",
				buf, (long)strlen(buf)+1, cnt, NULL);
    } while (status == ClipboardLocked);


    do {
	status = XmClipboardEndCopy(dpy, window, item_id);
    } while (status == ClipboardLocked);

    printf("copied: '%s' to clipboard\n", buf);
}

static void
from_clipbd(Widget w, XtPointer client_data, XtPointer call_data)
{
    int status;
    unsigned total_bytes;
    unsigned long received;
    char buf[16], *data = NULL;
    Display *dpy = XtDisplayOfObject(w);
    Window window = XtWindowOfObject(w);

    do {
	status = XmClipboardStartRetrieve(dpy, window, CurrentTime);
    } while (status == ClipboardLocked);

    data = XtMalloc(1);
    total_bytes = 1;

    do {
	status = XmClipboardRetrieve(dpy, window, "STRING",
				     buf, sizeof(buf), &received, NULL);

	if (!(data = XtRealloc(data, total_bytes + received))) {
	    XtError("Can't allocate space for data");
	    break;
	}

	strncpy(&data[total_bytes-1], buf, received);
	total_bytes += received;

    } while (status == ClipboardTruncate);

    if (data)
	data[total_bytes] = 0;

    if (status == ClipboardSuccess)
	printf("Retrieved '%s' from clipboard.\n", data);

    status = XmClipboardEndRetrieve(dpy, window);
}

