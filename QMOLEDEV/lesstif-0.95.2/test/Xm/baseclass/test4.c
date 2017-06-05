/*
 * test4.c -- test _XmIsStandardMotifWidgetClass()
 */
#include <stdio.h>

#include <Xm/XmP.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <X11/ShellP.h>

void Check(WidgetClass wc, String Name)
{
    printf("%s %s a standard LessTif widget class.\n",
           Name,
           _XmIsStandardMotifWidgetClass(wc) ? "is" : "IS NOT");
}

int main(int argc, char **argv)
{
    XtAppContext AppContext;
    Widget       TopLevel, w;
    
    TopLevel = XtAppInitialize(&AppContext, "XNavigator",
                               NULL, 0,
                               &argc, argv,
                               NULL,
                               NULL, 0);

    printf("BEFORE CREATING ANY WIDGET WITH THE EXCEPTION OF THE TOPLEVEL SHELL...\n");
    Check(xmManagerWidgetClass, "Manager");

    printf("\n\nAFTER CREATING A FORM WIDGET...\n");
    w = XtVaCreateManagedWidget("form", xmFormWidgetClass, TopLevel, NULL);
    XtVaCreateManagedWidget("button", xmPushButtonWidgetClass, w, NULL);

    Check(widgetClass, "Core");
    Check(constraintWidgetClass, "Constraint");
    Check(xmPrimitiveWidgetClass, "Primitive");
    Check(xmManagerWidgetClass, "Manager");
    Check(xmPushButtonWidgetClass, "PushButton");
    Check(topLevelShellWidgetClass, "TopLevelShell");

    return 0;
}
