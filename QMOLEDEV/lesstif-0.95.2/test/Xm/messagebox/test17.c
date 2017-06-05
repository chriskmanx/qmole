/*
Subject:	Some changes to MessageBox and ResConvert
Date:		Sun, 30 Aug 1998 22:30:44 +0200
From:		Karsten Jensen <kbwj@diku.dk>

test17.c shows the geometry of all the children in a MessageBox.

*/

#include <Xm/XmP.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/PushBGP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/MessageBP.h>
#include <stdio.h>

static XtWidgetProc old_resize = NULL;

static void
resize(Widget w)
{
    XmMessageBoxWidget mw = (XmMessageBoxWidget) w;

    (*old_resize) (w);

    fprintf(stderr, "MessageBox width %3i, height %3i\n",
	    mw->core.width, mw->core.height);

    if (mw->message_box.message_wid != NULL)
    {
	fprintf(stderr, "MessageWidget x %3i, y %3i %s\n",
		XtX(mw->message_box.message_wid),
		XtY(mw->message_box.message_wid),
		XtIsManaged(mw->message_box.message_wid) ?
		"Managed" : "Unmanaged");
	fprintf(stderr, "MessageWidget width %3i, height %3i\n",
		XtWidth(mw->message_box.message_wid),
		XtHeight(mw->message_box.message_wid));
    }
    else
    {
	fprintf(stderr, "MessageWidget is NULL\n");
    }

    if (mw->message_box.symbol_wid != NULL)
    {
	fprintf(stderr, "SymbolWidget x %3i, y %3i %s\n",
		XtX(mw->message_box.symbol_wid),
		XtY(mw->message_box.symbol_wid),
		XtIsManaged(mw->message_box.symbol_wid) ?
		"Managed" : "Unmanaged");
	fprintf(stderr, "SymbolWidget width %3i, height %3i\n",
		XtWidth(mw->message_box.symbol_wid),
		XtHeight(mw->message_box.symbol_wid));
    }
    else
    {
	fprintf(stderr, "SymbolWidget is NULL\n");
    }

    if (mw->message_box.separator != NULL)
    {
	fprintf(stderr, "Separator x %3i, y %3i %s\n",
		XtX(mw->message_box.separator),
		XtY(mw->message_box.separator),
		XtIsManaged(mw->message_box.separator) ?
		"Managed" : "Unmanaged");
	fprintf(stderr, "Separator width %3i, height %3i\n",
		XtWidth(mw->message_box.separator),
		XtHeight(mw->message_box.separator));
    }
    else
    {
	fprintf(stderr, "Separator is NULL\n");
    }

    if (mw->message_box.ok_button != NULL)
    {
	fprintf(stderr, "OkButton x %3i, y %3i %s\n",
		XtX(mw->message_box.ok_button),
		XtY(mw->message_box.ok_button),
		XtIsManaged(mw->message_box.ok_button) ?
		"Managed" : "Unmanaged");
	fprintf(stderr, "OkButton width %3i, height %3i\n",
		XtWidth(mw->message_box.ok_button),
		XtHeight(mw->message_box.ok_button));
    }
    else
    {
	fprintf(stderr, "OkButton is NULL\n");
    }

    if (mw->message_box.help_button != NULL)
    {
	fprintf(stderr, "HelpButton x %3i, y %3i %s\n",
		XtX(mw->message_box.help_button),
		XtY(mw->message_box.help_button),	
		XtIsManaged(mw->message_box.help_button) ?
		"Managed" : "Unmanaged");
fprintf(stderr, "HelpButton width %3i, height %3i\n",
		XtWidth(mw->message_box.help_button),
		XtHeight(mw->message_box.help_button));
    }
    else
    {
	fprintf(stderr, "HelpButton is NULL\n");
    }

    if (mw->bulletin_board.cancel_button != NULL)
    {
	fprintf(stderr, "CancelButton x %3i, y %3i %s\n",
		XtX(mw->bulletin_board.cancel_button),
		XtY(mw->bulletin_board.cancel_button),
		XtIsManaged(mw->bulletin_board.cancel_button) ?
		"Managed" : "Unmanaged");
	fprintf(stderr, "CancelButton width %3i, height %3i\n\n",
		XtWidth(mw->bulletin_board.cancel_button),
		XtHeight(mw->bulletin_board.cancel_button));
    }
    else
    {
	fprintf(stderr, "CancelButton is NULL\n\n");
    }
}





static void
install_wrapper(void)
{
    WidgetClass super_class;
    super_class  = (WidgetClass)&xmMessageBoxClassRec;
	old_resize = super_class->core_class.resize;
    while(old_resize == XtInheritResize)
    {
	super_class = super_class->core_class.superclass;
	old_resize = super_class->core_class.resize;
    }
    xmMessageBoxClassRec.core_class.resize = resize;
}


static char *fallbacks[] =
{
    "*mb.showSeparator: True",
    NULL
};

int
main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget shell, mb;

    shell = XtVaAppInitialize(&theApp, "Test1", NULL, 0,
				 &argc, argv, fallbacks, NULL);

    install_wrapper();

    mb = XmCreateMessageBox(shell, "mb", NULL, 0);
    XtManageChild(mb);

    XtRealizeWidget(shell);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  214,   89, 0,0,0, /* mb */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  192,    4, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,   25,  214,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   37,   64,   41, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   75,   37,   64,   41, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  139,   37,   64,   41, 0,0,0, /* Help */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(shell, Expected);
}
  LessTifTestMainLoop(shell);

    exit(0);
}

