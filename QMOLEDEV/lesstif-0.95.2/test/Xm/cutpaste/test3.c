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
   CWWidth | CWHeight            ,   56,   72,  156,   59, 0,0,0, /* rowcol */
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
    char buf[32];
    static int cnt;
    Display *dpy = XtDisplayOfObject(w);
    Window window = XtWindowOfObject(w);
    char *data = (char *)client_data;

    sprintf(buf, "%s-%d", data, ++cnt);

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
    unsigned long length;
    unsigned long recvd;
    char *data;
    Display *dpy = XtDisplayOfObject(w);
    Window window = XtWindowOfObject(w);

    do {
	status = XmClipboardInquireLength(dpy, window, "STRING", &length);
    } while (status == ClipboardLocked);
    
    if (length == 0) {
	printf("no data on clipboard in spec'ed format\n");
	return;
    }

    data = XtMalloc(length + 1);

    do {
	status = XmClipboardRetrieve(dpy, window, "STRING",
				     data, length+1, &recvd, NULL);
    } while (status == ClipboardLocked);

    if (status != ClipboardSuccess || recvd != length) {
	printf("Failed to receive all clipboard data\n");
	XtFree(data);
    }
    else
	printf("Retrieved '%s' from clipboard.\n", data);
}

