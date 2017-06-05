/* $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test5.c,v 1.4 2002/04/17 16:22:01 amai Exp $
   test for M*tif to see what the compiled Resources look like */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/ExtObjectP.h>
#include <Xm/DesktopP.h>
#include <Xm/DialogSEP.h>
#include <Xm/ProtocolsP.h>
#include <Xm/ShellEP.h>
#include <Xm/VendorSEP.h>
#ifdef LESSTIF_VERSION
#include <Xm/WorldP.h>
#endif
#include <Xm/DragCP.h>
#include <Xm/DragIconP.h>
#include <Xm/DropTransP.h>

#include <Xm/GadgetP.h>
#include <Xm/ArrowBGP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/LabelGP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/PushBGP.h>
#include <Xm/ToggleBGP.h>

#include <Xm/PrimitiveP.h>
#include <Xm/ArrowBP.h>
#include <Xm/SeparatorP.h>
#include <Xm/ListP.h>
#include <Xm/SashP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/TextFP.h>
#include <Xm/TextP.h>
#include <Xm/LabelP.h>
#include <Xm/CascadeBP.h>
#include <Xm/DrawnBP.h>
#include <Xm/PushBP.h>
#include <Xm/ToggleBP.h>
#include <Xm/TearOffBP.h>

#include <Xm/ManagerP.h>
#include <Xm/BulletinBP.h>
#include <Xm/CommandP.h>
#include <Xm/DrawingAP.h>
#include <Xm/FileSBP.h>
#include <Xm/FormP.h>
#include <Xm/FrameP.h>
#include <Xm/MainWP.h>
#include <Xm/MessageBP.h>
#include <Xm/PanedWP.h>
#include <Xm/RowColumnP.h>
#include <Xm/ScaleP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/SelectioBP.h>

#include <Xm/DialogSP.h>
#include <Xm/VendorSP.h>
#include <Xm/MenuShellP.h>

#include <Xm/ScreenP.h>
#include <Xm/DisplayP.h>
#include <X11/Xresource.h>

void
print_syns(char *class, XrmResource **res, int nres)
{
  int i;

  printf( "%s: %d\n", class, nres);
  for (i = 0; i < nres; i++) {
    printf("    /* %d */\n", i);
    printf("    {\n");
    printf("\tXmN%s, XmC%s, XmR%s,\n",
	   XrmQuarkToString((int)res[i]->xrm_name),
	   XrmQuarkToString((int)res[i]->xrm_class),
	   XrmQuarkToString((int)res[i]->xrm_type));
    printf("\tsize: %-3d offset: %-3d\n",
	   res[i]->xrm_size, res[i]->xrm_offset);
    printf("\tXmR%s, (XtPointer)%p\n",
	   XrmQuarkToString((int)res[i]->xrm_default_type), res[i]->xrm_default_addr);
    printf("    },\n");
  }
  printf("\n");
}

int
main(int argc, char **argv)
{
  XtResource *syn;
  Widget w, toplevel, one, two;
  XtAppContext app;
  int nsyn;
  Arg args[1];

  one = XtAppInitialize(&app, argv[0], NULL, 0, &argc, argv, NULL, NULL, 0);
  toplevel = XtCreateWidget("top", (WidgetClass)&xmBulletinBoardClassRec, one, NULL, 0);
  XtSetArg(args[0], XmNrowColumnType, XmMENU_BAR);
  two = XtCreateWidget("top", (WidgetClass)&xmRowColumnClassRec, one, args, 1);

  /* ODDITIES */
  /* MLM: FIXME -- Chris, this widget gives and XtVersion mismatch */
  w = XtCreateWidget("one", (WidgetClass)&xmDragContextClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDragContextClassRec.core_class.resources;
  nsyn = xmDragContextClassRec.core_class.num_resources;
  print_syns("DragContext", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmDragIconClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDragIconClassRec.rectangle_class.resources;
  nsyn = xmDragIconClassRec.rectangle_class.num_resources;
  print_syns("DragIcon", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmDropTransferClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDropTransferClassRec.object_class.resources;
  nsyn = xmDropTransferClassRec.object_class.num_resources;
  print_syns("DropTransfer", (XrmResource **)syn, nsyn);

  /* EXT OBJS */
  w = XtCreateWidget("one", (WidgetClass)&xmExtClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmExtClassRec.object_class.resources;
  nsyn = xmExtClassRec.object_class.num_resources;
  print_syns("ExtObj", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmDesktopClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDesktopClassRec.object_class.resources;
  nsyn = xmDesktopClassRec.object_class.num_resources;
  print_syns("Desktop", (XrmResource **)syn, nsyn);

#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmDialogShellExtClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDialogShellExtClassRec.object_class.resources;
  nsyn = xmDialogShellExtClassRec.object_class.num_resources;
  print_syns("DialogShellExt", (XrmResource **)syn, nsyn);
#endif
 
  w = XtCreateWidget("one", (WidgetClass)&xmProtocolClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmProtocolClassRec.object_class.resources;
  nsyn = xmProtocolClassRec.object_class.num_resources;
  print_syns("Protocols", (XrmResource **)syn, nsyn);

#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmShellExtClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmShellExtClassRec.object_class.resources;
  nsyn = xmShellExtClassRec.object_class.num_resources;
  print_syns("ShellExt", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmVendorShellExtClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmVendorShellExtClassRec.object_class.resources;
  nsyn = xmVendorShellExtClassRec.object_class.num_resources;
  print_syns("VendorShellExt", (XrmResource **)syn, nsyn);
#endif

#ifdef LESSTIF_VERSION
  w = XtCreateWidget("one", (WidgetClass)&xmWorldClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmWorldClassRec.object_class.resources;
  nsyn = xmWorldClassRec.object_class.num_resources;
  print_syns("World", (XrmResource **)syn, nsyn);
#endif

#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmSeparatorGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSeparatorGCacheObjClassRec.object_class.resources;
  nsyn = xmSeparatorGCacheObjClassRec.object_class.num_resources;
  print_syns("SeparatorGCache", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmLabelGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmLabelGCacheObjClassRec.object_class.resources;
  nsyn = xmLabelGCacheObjClassRec.object_class.num_resources;
  print_syns("LabelGCache", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmCascadeButtonGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmCascadeButtonGCacheObjClassRec.object_class.resources;
  nsyn = xmCascadeButtonGCacheObjClassRec.object_class.num_resources;
  print_syns("CascadeButtonGCache", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmPushButtonGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPushButtonGCacheObjClassRec.object_class.resources;
  nsyn = xmPushButtonGCacheObjClassRec.object_class.num_resources;
  print_syns("PushButtonGCache", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmToggleButtonGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmToggleButtonGCacheObjClassRec.object_class.resources;
  nsyn = xmToggleButtonGCacheObjClassRec.object_class.num_resources;
  print_syns("ToggleButtonGCache", (XrmResource **)syn, nsyn);
#endif

  /* GADGETS */
  w = XtCreateWidget("one", (WidgetClass)&xmGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmGadgetClassRec.rect_class.resources;
  nsyn = xmGadgetClassRec.rect_class.num_resources;
  print_syns("Gadget", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmArrowButtonGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmArrowButtonGadgetClassRec.rect_class.resources;
  nsyn = xmArrowButtonGadgetClassRec.rect_class.num_resources;
  print_syns("ArrowButtonGadget", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmSeparatorGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSeparatorGadgetClassRec.rect_class.resources;
  nsyn = xmSeparatorGadgetClassRec.rect_class.num_resources;
  print_syns("SeparatorGadget", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmLabelGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmLabelGadgetClassRec.rect_class.resources;
  nsyn = xmLabelGadgetClassRec.rect_class.num_resources;
  print_syns("LabelGadget", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmCascadeButtonGadgetClassRec, two, NULL, 0);
  XtDestroyWidget(w);
  syn = xmCascadeButtonGadgetClassRec.rect_class.resources;
  nsyn = xmCascadeButtonGadgetClassRec.rect_class.num_resources;
  print_syns("CascadeButtonGadget", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmPushButtonGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPushButtonGadgetClassRec.rect_class.resources;
  nsyn = xmPushButtonGadgetClassRec.rect_class.num_resources;
  print_syns("PushButtonGadget", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmToggleButtonGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmToggleButtonGadgetClassRec.rect_class.resources;
  nsyn = xmToggleButtonGadgetClassRec.rect_class.num_resources;
  print_syns("ToggleButtonGadget", (XrmResource **)syn, nsyn);

  /* PRIMITIVES */
  w = XtCreateWidget("one", (WidgetClass)&xmPrimitiveClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPrimitiveClassRec.core_class.resources;
  nsyn = xmPrimitiveClassRec.core_class.num_resources;
  print_syns("Primitive", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmArrowButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmArrowButtonClassRec.core_class.resources;
  nsyn = xmArrowButtonClassRec.core_class.num_resources;
  print_syns("ArrowButton", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmSeparatorClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSeparatorClassRec.core_class.resources;
  nsyn = xmSeparatorClassRec.core_class.num_resources;
  print_syns("Separator", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmListClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmListClassRec.core_class.resources;
  nsyn = xmListClassRec.core_class.num_resources;
  print_syns("List", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmSashClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSashClassRec.core_class.resources;
  nsyn = xmSashClassRec.core_class.num_resources;
  print_syns("Sash", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmScrollBarClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmScrollBarClassRec.core_class.resources;
  nsyn = xmScrollBarClassRec.core_class.num_resources;
  print_syns("ScrollBar", (XrmResource **)syn, nsyn);

#if 0
/* FIXME: Rob, this shouldn't dump core; it does though, in _XmFontListEntryFromTag */
  w = XtCreateWidget("one", (WidgetClass)&xmTextFieldClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmTextFieldClassRec.core_class.resources;
  nsyn = xmTextFieldClassRec.core_class.num_resources;
  print_syns("TextField", (XrmResource **)syn, nsyn);
#endif

  w = XtCreateWidget("one", (WidgetClass)&xmTextClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmTextClassRec.core_class.resources;
  nsyn = xmTextClassRec.core_class.num_resources;
  print_syns("Text", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmLabelClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmLabelClassRec.core_class.resources;
  nsyn = xmLabelClassRec.core_class.num_resources;
  print_syns("Label", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmCascadeButtonClassRec, two, NULL, 0);
  XtDestroyWidget(w);
  syn = xmCascadeButtonClassRec.core_class.resources;
  nsyn = xmCascadeButtonClassRec.core_class.num_resources;
  print_syns("CascadeButton", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmDrawnButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDrawnButtonClassRec.core_class.resources;
  nsyn = xmDrawnButtonClassRec.core_class.num_resources;
  print_syns("DrawnButton", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmPushButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPushButtonClassRec.core_class.resources;
  nsyn = xmPushButtonClassRec.core_class.num_resources;
  print_syns("PushButton", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmToggleButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmToggleButtonClassRec.core_class.resources;
  nsyn = xmToggleButtonClassRec.core_class.num_resources;
  print_syns("ToggleButton", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmTearOffButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmTearOffButtonClassRec.core_class.resources;
  nsyn = xmTearOffButtonClassRec.core_class.num_resources;
  print_syns("TearOffButton", (XrmResource **)syn, nsyn);

  /* MANAGERS */
  w = XtCreateWidget("one", (WidgetClass)&xmManagerClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmManagerClassRec.core_class.resources;
  nsyn = xmManagerClassRec.core_class.num_resources;
  print_syns("Manager", (XrmResource **)syn, nsyn);
  syn = xmManagerClassRec.constraint_class.resources;
  nsyn = xmManagerClassRec.constraint_class.num_resources;
  print_syns("Manager Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmBulletinBoardClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmBulletinBoardClassRec.core_class.resources;
  nsyn = xmBulletinBoardClassRec.core_class.num_resources;
  print_syns("BulletinBoard", (XrmResource **)syn, nsyn);
  syn = xmBulletinBoardClassRec.constraint_class.resources;
  nsyn = xmBulletinBoardClassRec.constraint_class.num_resources;
  print_syns("BulletinBoard Constraint", (XrmResource **)syn, nsyn);

  /* Danny, this shouldn't dump core.  It does, though in _XmFontListEntryFromTag */
  w = XtCreateWidget("one", (WidgetClass)&xmCommandClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmCommandClassRec.core_class.resources;
  nsyn = xmCommandClassRec.core_class.num_resources;
  print_syns("Command", (XrmResource **)syn, nsyn);
  syn = xmCommandClassRec.constraint_class.resources;
  nsyn = xmCommandClassRec.constraint_class.num_resources;
  print_syns("Command Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmDrawingAreaClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDrawingAreaClassRec.core_class.resources;
  nsyn = xmDrawingAreaClassRec.core_class.num_resources;
  print_syns("DrawingArea", (XrmResource **)syn, nsyn);
  syn = xmDrawingAreaClassRec.constraint_class.resources;
  nsyn = xmDrawingAreaClassRec.constraint_class.num_resources;
  print_syns("DrawingArea Constraint", (XrmResource **)syn, nsyn);

/* FIXME: Mitch, this shouldn't dump core.  It does though, in _XmFontListEntryFromTag.
 * Since I (Mitch) am writing this, I'll immediate cast blame elsewhere and say
 * this routine is broken.  I'm probably wrong, though.
 */
  w = XtCreateWidget("one", (WidgetClass)&xmFileSelectionBoxClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmFileSelectionBoxClassRec.core_class.resources;
  nsyn = xmFileSelectionBoxClassRec.core_class.num_resources;
  print_syns("FileSelectionBox", (XrmResource **)syn, nsyn);
  syn = xmFileSelectionBoxClassRec.constraint_class.resources;
  nsyn = xmFileSelectionBoxClassRec.constraint_class.num_resources;
  print_syns("FileSelectionBox Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmFormClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmFormClassRec.core_class.resources;
  nsyn = xmFormClassRec.core_class.num_resources;
  print_syns("Form", (XrmResource **)syn, nsyn);
  syn = xmFormClassRec.constraint_class.resources;
  nsyn = xmFormClassRec.constraint_class.num_resources;
  print_syns("Form Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmFrameClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmFrameClassRec.core_class.resources;
  nsyn = xmFrameClassRec.core_class.num_resources;
  print_syns("Frame", (XrmResource **)syn, nsyn);
  syn = xmFrameClassRec.constraint_class.resources;
  nsyn = xmFrameClassRec.constraint_class.num_resources;
  print_syns("Frame Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmMainWindowClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmMainWindowClassRec.core_class.resources;
  nsyn = xmMainWindowClassRec.core_class.num_resources;
  print_syns("MainWindow", (XrmResource **)syn, nsyn);
  syn = xmMainWindowClassRec.constraint_class.resources;
  nsyn = xmMainWindowClassRec.constraint_class.num_resources;
  print_syns("MainWindow Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmMessageBoxClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmMessageBoxClassRec.core_class.resources;
  nsyn = xmMessageBoxClassRec.core_class.num_resources;
  print_syns("MessageBox", (XrmResource **)syn, nsyn);
  syn = xmMessageBoxClassRec.constraint_class.resources;
  nsyn = xmMessageBoxClassRec.constraint_class.num_resources;
  print_syns("MessageBox Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmPanedWindowClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPanedWindowClassRec.core_class.resources;
  nsyn = xmPanedWindowClassRec.core_class.num_resources;
  print_syns("PanedWindow", (XrmResource **)syn, nsyn);
  syn = xmPanedWindowClassRec.constraint_class.resources;
  nsyn = xmPanedWindowClassRec.constraint_class.num_resources;
  print_syns("PanedWindow Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmRowColumnClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmRowColumnClassRec.core_class.resources;
  nsyn = xmRowColumnClassRec.core_class.num_resources;
  print_syns("RowColumn", (XrmResource **)syn, nsyn);
  syn = xmRowColumnClassRec.constraint_class.resources;
  nsyn = xmRowColumnClassRec.constraint_class.num_resources;
  print_syns("RowColumn Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmScaleClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmScaleClassRec.core_class.resources;
  nsyn = xmScaleClassRec.core_class.num_resources;
  print_syns("Scale", (XrmResource **)syn, nsyn);
  syn = xmScaleClassRec.constraint_class.resources;
  nsyn = xmScaleClassRec.constraint_class.num_resources;
  print_syns("Scale Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmScrolledWindowClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmScrolledWindowClassRec.core_class.resources;
  nsyn = xmScrolledWindowClassRec.core_class.num_resources;
  print_syns("ScrolledWindow", (XrmResource **)syn, nsyn);
  syn = xmScrolledWindowClassRec.constraint_class.resources;
  nsyn = xmScrolledWindowClassRec.constraint_class.num_resources;
  print_syns("ScrolledWindow Constraint", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmSelectionBoxClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSelectionBoxClassRec.core_class.resources;
  nsyn = xmSelectionBoxClassRec.core_class.num_resources;
  print_syns("SelectionBox", (XrmResource **)syn, nsyn);
  syn = xmSelectionBoxClassRec.constraint_class.resources;
  nsyn = xmSelectionBoxClassRec.constraint_class.num_resources;
  print_syns("SelectionBox Constraint", (XrmResource **)syn, nsyn);

  /* SHELLS */
  w = XtCreateWidget("one", (WidgetClass)&xmDialogShellClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDialogShellClassRec.core_class.resources;
  nsyn = xmDialogShellClassRec.core_class.num_resources;
  print_syns("DialogShell", (XrmResource **)syn, nsyn);

  syn = vendorShellClassRec.core_class.resources;
  nsyn = vendorShellClassRec.core_class.num_resources;
  w = XtCreateWidget("one", (WidgetClass)&vendorShellClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  print_syns("VendorShell", (XrmResource **)syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmMenuShellClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmMenuShellClassRec.core_class.resources;
  nsyn = xmMenuShellClassRec.core_class.num_resources;
  print_syns("MenuShell", (XrmResource **)syn, nsyn);

  /* OTHER */
  w = XtCreateWidget("one", (WidgetClass)&xmDisplayClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDisplayClassRec.core_class.resources;
  nsyn = xmDisplayClassRec.core_class.num_resources;
  print_syns("Display", (XrmResource **)syn, nsyn);

#if 0
/* OOPS: only one of these per screen */
  w = XtCreateWidget("one", (WidgetClass)&xmScreenClassRec, toplevel, NULL, 0);
#else
  w = XmGetXmScreen(XtScreen(toplevel));
#endif
  syn = xmScreenClassRec.core_class.resources;
  nsyn = xmScreenClassRec.core_class.num_resources;
  print_syns("Screen", (XrmResource **)syn, nsyn);

  exit(0);
}
