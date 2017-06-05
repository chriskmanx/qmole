/* $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/trait/test1.c,v 1.5 2002/05/01 15:47:32 amai Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/XmAll.h>

#include "../../common/Test.h"


int
main(int argc, char **argv)
{
#ifdef	LESSTIF_VERSION
	extern void XmeTraitReport(void);
	Widget          toplevel, w, nb, mb;
	XtAppContext    app;

	XtSetLanguageProc(NULL, NULL, NULL);

	toplevel = XtVaAppInitialize(&app, "Notebook", NULL, 0, &argc, argv, NULL, NULL);

	nb = XmCreateNotebook(toplevel, "notebook", NULL, 0);
	mb = XmCreateMenuBar(toplevel, "x", NULL, 0);
	w = XmCreateArrowButton(toplevel, "x", NULL, 0);
	w = XmCreateArrowButtonGadget(nb, "x", NULL, 0);
	w = XmCreateBulletinBoard(toplevel, "x", NULL, 0);
	w = XmCreateBulletinBoardDialog(toplevel, "x", NULL, 0);
	w = XmCreateCascadeButton(mb, "x", NULL, 0);
	w = XmCreateCascadeButtonGadget(mb, "x", NULL, 0);
	w = XmCreateCommand(toplevel, "x", NULL, 0);
	w = XmCreateCommandDialog(toplevel, "x", NULL, 0);
	w = XmCreateDialogShell(toplevel, "x", NULL, 0);
	w = XmCreateDragIcon(toplevel, "x", NULL, 0);
	w = XmCreateDrawingArea(toplevel, "x", NULL, 0);
	w = XmCreateDrawnButton(toplevel, "x", NULL, 0);
	w = XmCreateFileSelectionBox(toplevel, "x", NULL, 0);
	w = XmCreateFileSelectionDialog(toplevel, "x", NULL, 0);
	w = XmCreateForm(toplevel, "x", NULL, 0);
	w = XmCreateFormDialog(toplevel, "x", NULL, 0);
	w = XmCreateFrame(toplevel, "x", NULL, 0);
	w = XmCreateLabel(toplevel, "x", NULL, 0);
	w = XmCreateLabelGadget(nb, "x", NULL, 0);
	w = XmCreateList(toplevel, "x", NULL, 0);
	w = XmCreateScrolledList(toplevel, "x", NULL, 0);
	w = XmCreateMainWindow(toplevel, "x", NULL, 0);
	w = XmCreateMenuShell(toplevel, "x", NULL, 0);
	w = XmCreateMessageBox(toplevel, "x", NULL, 0);
	w = XmCreateErrorDialog(toplevel, "x", NULL, 0);
	w = XmCreateInformationDialog(toplevel, "x", NULL, 0);
	w = XmCreateMessageDialog(toplevel, "x", NULL, 0);
	w = XmCreateQuestionDialog(toplevel, "x", NULL, 0);
	w = XmCreateTemplateDialog(toplevel, "x", NULL, 0);
	w = XmCreateWarningDialog(toplevel, "x", NULL, 0);
	w = XmCreateWorkingDialog(toplevel, "x", NULL, 0);
	w = XmCreatePanedWindow(toplevel, "x", NULL, 0);
	w = XmCreatePushButton(toplevel, "x", NULL, 0);
	w = XmCreatePushButtonGadget(nb, "x", NULL, 0);
	w = XmCreateOptionMenu(toplevel, "x", NULL, 0);
	w = XmCreatePopupMenu(toplevel, "x", NULL, 0);
	w = XmCreatePulldownMenu(toplevel, "x", NULL, 0);
	w = XmCreateRadioBox(toplevel, "x", NULL, 0);
	w = XmCreateRowColumn(toplevel, "x", NULL, 0);
	w = XmCreateWorkArea(toplevel, "x", NULL, 0);
	w = XmCreateScale(toplevel, "x", NULL, 0);
	w = XmCreateScrollBar(toplevel, "x", NULL, 0);
	w = XmCreateScrolledWindow(toplevel, "x", NULL, 0);
	w = XmCreateSelectionBox(toplevel, "x", NULL, 0);
	w = XmCreateSelectionDialog(toplevel, "x", NULL, 0);
	w = XmCreatePromptDialog(toplevel, "x", NULL, 0);
	w = XmCreateSeparatorGadget(nb, "x", NULL, 0);
	w = XmCreateSeparator(toplevel, "x", NULL, 0);
	w = XmCreateSimpleCheckBox(toplevel, "x", NULL, 0);
	w = XmCreateSimpleMenuBar(toplevel, "x", NULL, 0);
	w = XmCreateSimpleOptionMenu(toplevel, "x", NULL, 0);
	w = XmCreateSimplePopupMenu(toplevel, "x", NULL, 0);
	w = XmCreateSimplePulldownMenu(toplevel, "x", NULL, 0);
	w = XmCreateSimpleRadioBox(toplevel, "x", NULL, 0);
	w = XmCreateText(toplevel, "x", NULL, 0);
	w = XmCreateScrolledText(toplevel, "x", NULL, 0);
	w = XmCreateTextField(toplevel, "x", NULL, 0);
	w = XmCreateToggleButton(toplevel, "x", NULL, 0);
	w = XmCreateToggleButtonGadget(nb, "x", NULL, 0);

	/* 2.0 */
#if 0
	w = XmCreateCSText(nb, "x", NULL, 0);
	w = XmCreateScrolledCSText(nb, "x", NULL, 0);
#endif
	w = XmCreateComboBox(nb, "x", NULL, 0);
	w = XmCreateDropDownComboBox(nb, "x", NULL, 0);
	w = XmCreateDropDownList(nb, "x", NULL, 0);
	w = XmCreateContainer(nb, "x", NULL, 0);
	w = XmCreateGrabShell(nb, "x", NULL, 0);
	w = XmCreateIconGadget(nb, "x", NULL, 0);
	w = XmCreateNotebook(nb, "x", NULL, 0);
	w = XmCreatePanedWindow(nb, "x", NULL, 0);
	w = XmCreateSpinBox(nb, "x", NULL, 0);

	/* 2.1 */
	w = XmCreateSimpleSpinBox(nb, "x", NULL, 0);

	_LtDebugTraitReport();
#endif
	exit(0);
}
