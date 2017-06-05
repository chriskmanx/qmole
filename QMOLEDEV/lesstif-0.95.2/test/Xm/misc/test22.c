/* $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test22.c,v 1.2 2001/06/15 09:30:51 amai Exp $ */
/*

From: "Sporri, Heinz" <Heinz.Sporri@ca.com>
To: lesstif@lesstif.org
Subject: Tab problem
Date: Mon, 19 Mar 2001 08:26:34 -0000

May be this is not the way to submit a problem report, but I got lost with
sourceforge.net/bugs. I am thankful for either help with the problem,
or help with submitting it to the right place.

> "Tab" Problem
> =============
> 
> 1. Problem Description
> ----------------------
> In a widget hierarchy we unmanage some widget which has children
> that are still managed. The unmanaged widget including its
> (possibly still managed) children disappears from our view
> which is what we want.
> But if we use "tab" to move through the fields, then the
> managed children of our unmanaged widget are still being
> tabbed through.
> I tested this on 386 lesstif-0.89.9-126 and lesstif-0.92.0-1.
> The commercial X versions on AIX, Solaris and HP tab only
> through visible fields.
> 
> 2. Problem Category
> -------------------
> This problem falls into the category of behavior problems.
> It can easily be circumvented by using the pointing device
> to focus a specific field.
> 
> 3. Sample Code
> --------------
> In order to show the problem I have created a sample program
> which has a toggle widget to manage/unmanage a scrolled
> text field. The application exhibiting this problem has a
> much more complex widget hierarchy and although it would be
> easy to fix my sample code, it is much more difficult to find
> a way around in a larger widget hierarchy.

*/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>

void ChangeIndirect(Widget w, XtPointer client_data,
                    XmToggleButtonCallbackStruct *call_data);
void dumpWidgetTree(Widget w);

Widget ParmShell, b_text;

int 
main (int argc, char **argv)
{
   XtAppContext ParmApp;
   Widget PSform,
          b_indirect;
   XmString temp_string;
   int n;
   Arg args[20];

   /* Create the application */
   ParmShell = XtVaAppInitialize(&ParmApp, "Tab Test", NULL, 0,
                                &argc, argv, NULL, NULL);

   /* Create the form */
   n=0;
   XtSetArg(args[n], XmNwidth, 300);n++;
   XtSetArg(args[n], XmNheight, 150);n++;
   PSform = XmCreateForm(ParmShell, "PSform", args, n);
   XtManageChild(PSform);

   /* Create the indirect toggle button */
   temp_string=XmStringLtoRCreate("Indirect", XmSTRING_DEFAULT_CHARSET);
   n=0;
   XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM);n++;
   XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM);n++;
   XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM);n++;
   XtSetArg(args[n], XmNheight, 75);n++;
   XtSetArg(args[n], XmNlabelString, temp_string);n++;
   b_indirect = XmCreateToggleButton(PSform, "Toggle", args, n);
   XmStringFree(temp_string);
   XtAddCallback (b_indirect, XmNvalueChangedCallback,(XtCallbackProc)ChangeIndirect,
                  NULL);

   XtManageChild(b_indirect);

   /* Create the intermediate destination text field */
   temp_string=XmStringLtoRCreate("Intermediate Dest", XmSTRING_DEFAULT_CHARSET);
   n=0;
   XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET);n++;
   XtSetArg(args[n], XmNtopWidget, b_indirect);n++;
   XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM);n++;
   XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM);n++;
   XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM);n++;
   XtSetArg(args[n], XmNlabelString, temp_string);n++;
   XtSetArg(args[n], XmNscrollVertical, False);n++;
   XtSetArg(args[n], XmNscrollHorizontal, False);n++;
   b_text = XmCreateScrolledText(PSform, "LabelIndirect", args, n);
   XmStringFree(temp_string);
   n=0;
   XtSetArg(args[n], XmNeditMode, XmSINGLE_LINE_EDIT);n++;
   XtSetValues(b_text,args,n);
   XtManageChild(b_text);

   XtRealizeWidget(ParmShell);
   dumpWidgetTree(ParmShell);
#if 0
   XtAppMainLoop(ParmApp);
#else
   LessTifTestMainLoop(ParmShell);
#endif

   exit(0);
}


void ChangeIndirect(Widget w, XtPointer client_data,
                    XmToggleButtonCallbackStruct *call_data)
{
   /* The parent is chosen on purpose to exhibit the problem */
   if (XtIsManaged(XtParent(b_text)))
      XtUnmanageChild(XtParent(b_text));
   else
      XtManageChild(XtParent(b_text));
   dumpWidgetTree(ParmShell);
}

/*******************************************************************************
* dumpWidgetTree( Widget w )
*
*       This function will recursively descend throught the Widget tree
*       and print the children and their pointer addresses.
*
*       Jeremy Jameson          5/17/95
*
* Function added to debug LessTif port for Linux S/390. Found on
* http://www.essi.fr/~buffa/cours/X11_Motif/motif-faq/part9/faq-doc-35.html
* Heinz Sporri, 27 Jan 2001, modified to better print the tree.
*
*******************************************************************************/

void dumpWidgetTree( Widget w )
{
   WidgetList list = NULL;
   Cardinal num_children = 0;
   int i;
   static int n = 0;
   Widget child;
   XmNavigationType NavTyp;
   char TextNavigationType[5]="NTSEU";
   static char* indent =
"-----------------------------------------------------------------------------";
   char tmp[256];
   int w_class;

   *tmp = 0;
   if ( n == 0 )
   {
      printf ( "Widget tree dump. Abbreviations in order of appearance:\n");
      printf ( "   M: Mananged widget, m: Unmanaged widget\n");
      printf ( "   T: Traversable, t: non traversable\n");
      printf ( "   Navigation type: N NONE, T TAB, S STICKY, E EXCLUSIVE\n");
      printf ( "                    U Undefined for the widget\n");
   }

   if ( n >= strlen( indent ) +1 )
   {
      printf(
         "ERROR:Widget tree is too deep, not enough indent string ( < %d )!\n",
          n );
      n = 0;
      return;
   }

   strncpy( tmp, indent, n );
   tmp[n] = 0;

   XtVaGetValues(w,
                XmNnavigationType, &NavTyp,
                NULL);
   switch (NavTyp) {
      case XmNONE:                i=0; break;
      case XmTAB_GROUP:           i=1; break;
      case XmSTICKY_TAB_GROUP:    i=2; break;
      case XmEXCLUSIVE_TAB_GROUP: i=3; break;
      default:                    i=4; break;
   }
   w_class = (int)XtClass(w);

   printf( "%s> %s %s %c %d wdgt %s - %#x, class %d",
          tmp,
          XtIsManaged(w) ? "M" : "m",
          XmIsTraversable(w) ? "T" : "t",
          TextNavigationType[i], NavTyp,
          XtName( w ), w, w_class );

   if ( ! XtIsComposite( w ) )
   {
      printf(
         " no children\n");
      return;
   }

   XtVaGetValues( w,
         XmNchildren, &list,
         XmNnumChildren, &num_children,
         NULL );

   printf( " %d %s\n",
      num_children, num_children == 1 ? "child" : "children" );

   for ( i = 1; i <= num_children; i++ )
   {
      child = list[i-1];
      n += 3;
      dumpWidgetTree( child );

      n -= 3;
   }
}
