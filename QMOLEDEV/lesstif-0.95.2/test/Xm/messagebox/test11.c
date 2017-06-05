/* test of selection boxes */

#include <Xm/Xm.h>
#include <Xm/PushBP.h>
#include <Xm/MessageB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/TextF.h>

Widget toplevel, box;

int
main(int argc, char **argv)
{
	XtAppContext	app;
	XmString	xms;
	Arg		args[3];
	int		nargs;
	Widget msgb, m_stopButton, form, m_logo, pane, url_label, m_url_value;
	Widget saving_label, m_saving_value;

	toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
			       &argc, argv, NULL, NULL);

	nargs = 0;
	msgb = XmCreateMessageBox(toplevel, "Box", args, nargs);
	XtVaSetValues(msgb,
		XmNdialogType, XmDIALOG_TEMPLATE,
		NULL);
	/*
	XtUnmanageChild(XmMessageBoxGetChild(msgb, XmDIALOG_SEPARATOR));
	*/
	XtUnmanageChild(XmMessageBoxGetChild(msgb, XmDIALOG_OK_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(msgb, XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(msgb, XmDIALOG_HELP_BUTTON));

	m_stopButton = XmCreatePushButton(msgb, "stopLoading", NULL, 0);
	XtManageChild(m_stopButton);

	form = XmCreateForm(msgb, "topArea", NULL, 0);
	XtVaSetValues(form,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_NONE,
		NULL);

	m_logo = XmCreateLabel(form, "logo", NULL, 0);
	XtManageChild(m_logo);

	pane = XmCreateForm(form, "pane", NULL, 0);
	XtManageChild(pane);

	url_label = XmCreateLabelGadget(pane, "dowloadURLLabel", NULL, 0);
	XtManageChild(url_label);

	m_url_value = XmCreateTextField(pane, "downloadURLValue", NULL, 0);
	XtManageChild(m_url_value);

	saving_label = XmCreateLabelGadget(pane, "downloadFileLabel", NULL, 0);
	XtManageChild(saving_label);

	m_saving_value = XmCreateTextField(pane, "downloadFileValue", NULL, 0);
	XtManageChild(m_saving_value);

	XtVaSetValues(url_label,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtopOffset, 10,
		XmNleftOffset, 10,
		NULL);

	XtVaSetValues(m_url_value,
		XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNtopWidget, url_label,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, url_label,
		XmNleftOffset, 10,
		XmNrightOffset, 10,
		NULL);

	XtVaSetValues(saving_label,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, m_url_value,
		XmNtopOffset, 5,
		XmNbottomOffset, 15,
		XmNleftOffset, 10,
		NULL);

	XtVaSetValues(m_saving_value,
		XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		XmNtopWidget, saving_label,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, saving_label,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftOffset, 10,
		XmNrightOffset, 10,
		NULL);

	if (XtWidth(url_label) < XtWidth(saving_label))
	{
	    XtVaSetValues(url_label,
		    XmNwidth, XtWidth(saving_label),
		    NULL);
	}
	else
	{
	    XtVaSetValues(saving_label,
		    XmNwidth, XtWidth(url_label),
		    NULL);
	}
	XtVaSetValues(url_label,
		XmNheight, XtHeight(m_url_value),
		NULL);
	XtVaSetValues(saving_label,
		XmNheight, XtHeight(m_saving_value),
		NULL);

	XtVaSetValues(pane,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, m_logo,
		NULL);

	XtVaSetValues(m_logo,
		XmNtopAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_NONE,
		XmNbottomAttachment, XmATTACH_NONE,
		XmNtopOffset, 15,
		XmNwidth, 36,
		XmNheight, 36,
		NULL);

	XtManageChild(form);
	XtManageChild(msgb);
	XtRealizeWidget(toplevel);

{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  332,  176, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,    0,    0,    4,    4, 0,0,0, /* Symbol */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  310,    4, 0,0,0, /* Message */
   CWWidth | CWHeight | CWX | CWY,    0,  112,  332,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,  119,  124,   94,   41, 0,0,0, /* stopLoading */
   CWWidth | CWHeight | CWX | CWY,   11,   25,  310,   77, 0,0,0, /* topArea */
   CWWidth | CWHeight | CWX | CWY,  274,   15,   36,   36, 0,0,0, /* logo */
   CWWidth | CWHeight | CWX | CWY,    0,    0,  274,   77, 0,0,0, /* pane */
   CWWidth | CWHeight | CWX | CWY,   10,   10,  106,   31, 0,0,0, /* dowloadURLLabel */
   CWWidth | CWHeight | CWX | CWY,  126,   10,  138,   31, 0,0,0, /* downloadURLValue */
   CWWidth | CWHeight | CWX | CWY,   10,   46,  106,   31, 0,0,0, /* downloadFileLabel */
   CWWidth | CWHeight | CWX | CWY,  126,   46,  138,   31, 0,0,0, /* downloadFileValue */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
  LessTifTestMainLoop(toplevel);

	exit(0);
}
