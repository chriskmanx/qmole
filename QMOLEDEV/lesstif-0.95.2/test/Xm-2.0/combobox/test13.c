/* Purpose: test grab handling with multiple toplevel shells.
            Do not use this for geometry testing.  (unless you really want to 8))

            To duplicate problem:
            1. Start application
            2. Click New Form button
            3. Click the arrow button on every combobox.
            4. Close the window with the comboboxes and I do not mean minimize.
            5. Click the New Form button
            6. Click the arrow button on every combobox.  You should now
               see Grab errors.
 */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/ComboBoxP.h>
#include <Xm/PushB.h>
#include <Xm/List.h>

#include "../../common/Test.h"

#if !defined(CB_List)
#define CB_List(w) \
        (((XmComboBoxWidget)(w))->combo_box.list)
#endif

#if 0
void
check_geometry(Widget w)
{
    static int result_index = 0;

static XtWidgetGeometry Expected[] = {
/* result test 0 */
};

#if 1
    PrintDetails2(w, NULL);
#else
    if (result_index <= 0)
    {
        PrintDetails2(w, Expected);
        fflush(stdout);
        result_index ++;
    }
#endif
}
#endif

void
exit_app(Widget parent, XtPointer cdata, XtPointer udata)
{
    exit(0);
}

void add_combo(Widget parent)
{
    Widget combo, list;
    XmString item;

    combo = XmCreateDropDownComboBox(parent, "combo", NULL, 0);

    /* use list routine to add items */
    list = CB_List(combo);

    item = XmStringCreateSimple("Item 1");
    XmListAddItem(list,item,0);
    item = XmStringCreateSimple("Item 2");
    XmListAddItem(list,item,0);
    item = XmStringCreateSimple("Item 3");
    XmListAddItem(list,item,0);
    item = XmStringCreateSimple("Item 4");
    XmListAddItem(list,item,0);
    item = XmStringCreateSimple("Item 5");
    XmListAddItem(list,item,0);

    XmComboBoxUpdate(combo);

    XtManageChild(combo);
}

void
new_form(Widget parent, XtPointer cdata, XtPointer udata)
{
    Widget toplevel, rc;

    toplevel = XtVaAppCreateShell(
                "maxwell",
                "maxwell",
                topLevelShellWidgetClass,
                XtDisplay(parent),
                XmNallowResize, True,
                NULL);

    rc = XmCreateRowColumn(toplevel, "rowcolumn", NULL, 0);
    XtManageChild(rc);

    add_combo(rc);
    add_combo(rc);
    add_combo(rc);

    XtRealizeWidget(toplevel);
}

int
main(int argc, char **argv)
{
    Widget toplevel, rc, button;
    XtAppContext app;

    XtSetLanguageProc(NULL, NULL, NULL);

    toplevel = XtVaAppInitialize(&app, "multiple-toplevel-test", 
                                 NULL, 0, &argc, argv, NULL, NULL);

    rc = XmCreateRowColumn(toplevel, "rowcolumn", NULL, 0);
    XtManageChild(rc);

    button = XmCreatePushButton(rc, "New Form", NULL, 0);
    XtAddCallback(button, 
                  XmNactivateCallback, 
                  (XtCallbackProc) new_form, 
                  (XtPointer) toplevel);
    XtManageChild(button);
    button = XmCreatePushButton(rc, "Exit", NULL, 0);
    XtAddCallback(button, 
                  XmNactivateCallback, 
                  (XtCallbackProc) exit_app, 
                  (XtPointer) toplevel);
    XtManageChild(button);
  
    XtRealizeWidget(toplevel);

    LessTifTestMainLoop(toplevel);

    exit(0);
}
