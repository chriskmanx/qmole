/* test of selection boxes */

#include <Xm/XmP.h>
#ifdef LESSTIF_REVISION
#include <XmI/TraversalI.h>
#endif
#include <Xm/SelectioB.h>
#include <stdio.h>

Widget toplevel;

#ifdef LESSTIF_REVISION
static void
DumpNode(int which, XmTravTreeNode node)
{
    fprintf(stderr, "  %d: addr: %p type: %d nav_type: %d tab_parent: %p\n",
	   which, node, node->type, node->nav_type, node->tab_parent.link);
    fprintf(stderr, "  %d: widget: %s rect: %d %d %d %d\n",
	   which, XtName(node->widget), node->rect.x, node->rect.y,
	   node->rect.width, node->rect.height);
    fprintf(stderr, "  %d: next: %p prev: %p up: %p down: %p\n",
	   which, node->next, node->prev, node->up, node->down);
    fprintf(stderr, "\n");
}

static void
DumpTree(XmTravTree tree) {
    int i;

    fprintf(stderr, "Tree: Widget: %s current: %p num_entries: %d\n",
	   XtName(tree->shell), tree->current, tree->num_entries);
    fprintf(stderr, "      num_alloc: %d next_alloc: %d num_excls: %d\n",
	   tree->num_alloc, tree->next_alloc, tree->num_excls);
    fprintf(stderr, "      num_tab_alloc: %d num_tab_entries: %d\n",
	   tree->num_tab_alloc, tree->num_tab_entries);

    fprintf(stderr, "Exclusive/tabs\n");
    for (i = 0; i < tree->num_tab_entries; i++)
	fprintf(stderr, "  %d: %s\n", i, XtName(tree->excl_tabs[i]));

    fprintf(stderr, "Nodes:\n");
    for (i = 0; i < tree->num_entries; i++)
	DumpNode(i, &tree->head[i]);
}

static void
DumpFocusData(XmFocusData fd) {
    fprintf(stderr, "FocusData: active_tab %p focus_item %p old_focus %p\n",
	   fd->active_tab_group, fd->focus_item, fd->old_focus_item);
    fprintf(stderr, "           pointer_item: %p old_pointer: %p\n",
	   fd->pointer_item, fd->old_pointer_item);
    fprintf(stderr, "           flush: %d focal_point: %d first_focus: %p\n",
	   fd->flush, fd->focal_point, fd->first_focus);
    fprintf(stderr, "           focus_policy: %d\n", fd->focus_policy);
    DumpTree(&fd->tree);
}
#endif

void cb(Widget w, XtPointer a, XtPointer b)
{
#ifdef LESSTIF_REVISION
  DumpFocusData(_XmGetFocusData(w));
#else
  printf("This test really only works under lesstif since it uses LT internals\n");
#endif
}

int
main(int argc, char **argv)
{
  XtAppContext app;
  Widget box;
  Arg a;

  toplevel = XtVaAppInitialize(&app, "listTest", NULL, 0,
 		               &argc, argv, NULL, NULL);

  XtSetArg(a, XmNdialogType, XmDIALOG_PROMPT);
  box = XmCreateSelectionBox(toplevel, "Box", &a, 1);

  XtAddCallback(XmSelectionBoxGetChild(box, XmDIALOG_OK_BUTTON),
		XmNactivateCallback, cb, NULL);

  XtManageChild(box);

  XtRealizeWidget(toplevel);

  
{
    static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,   56,   72,  220,  135, 0,0,0, /* Box */
   CWWidth | CWHeight | CWX | CWY,   11,   11,  198,   17, 0,0,0, /* Selection */
   CWWidth | CWHeight | CWX | CWY,   11,   28,  198,   31, 0,0,0, /* Text */
   CWWidth | CWHeight | CWX | CWY,    0,   69,  220,    2, 0,0,0, /* Separator */
   CWWidth | CWHeight | CWX | CWY,   11,   81,   66,   43, 0,0,0, /* OK */
   CWWidth | CWHeight | CWX | CWY,   77,   81,   66,   43, 0,0,0, /* Cancel */
   CWWidth | CWHeight | CWX | CWY,  143,   81,   66,   43, 0,0,0, /* Help */ 
    };
    PrintDetails(  toplevel ,Expected);
};
   LessTifTestMainLoop(  toplevel );
  exit(0);
}
