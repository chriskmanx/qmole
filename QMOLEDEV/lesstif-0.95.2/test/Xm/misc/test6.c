/* $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test6.c,v 1.4 2002/04/17 16:22:01 amai Exp $
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
print_syns(char *class, XmSyntheticResource *res, int nres)
{
  int i;

  printf( "%s: %d\n", class, nres);
  for (i = 0; i < nres; i++) {
    printf("    /* %d */\n", i);
    printf("    {\n");
    printf("\tXmN%s\n",
	   XrmQuarkToString((int)res[i].resource_name));
    printf("\tsize: %-3d offset: %-3d\n",
	   res[i].resource_size, res[i].resource_offset);
    printf("    },\n");
  }
  printf("\n");
}

int
main(int argc, char **argv)
{
  XmSyntheticResource *syn;
  Widget w, toplevel, one, two;
  XtAppContext app;
  int nsyn;
  Arg args[1];

  one = XtAppInitialize(&app, argv[0], NULL, 0, &argc, argv, NULL, NULL, 0);
  toplevel = XtCreateWidget("top", (WidgetClass)&xmBulletinBoardClassRec, one, NULL, 0);
  XtSetArg(args[0], XmNrowColumnType, XmMENU_BAR);
  two = XtCreateWidget("top", (WidgetClass)&xmRowColumnClassRec, one, args, 1);

  /* ODDITIES */
#if 0
/* MLM: FIXME -- Chris, this widget gives and XtVersion mismatch */
  w = XtCreateWidget("one", (WidgetClass)&xmDragContextClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDragContextClassRec.core_class.resources;
  nsyn = xmDragContextClassRec.core_class.num_resources;
  print_syns("DragContext", syn, nsyn);
#endif

#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmDragIconClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDragIconClassRec.rectangle_class.resources;
  nsyn = xmDragIconClassRec.rectangle_class.num_resources;
  print_syns("DragIcon", syn, nsyn);
#endif

#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmDropTransferClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDropTransferClassRec.ext_class.syn_resources;
  nsyn = xmDropTransferClassRec.ext_class.num_syn_resources;
  print_syns("DropTransfer", syn, nsyn);
#endif

  /* EXT OBJS */
  w = XtCreateWidget("one", (WidgetClass)&xmExtClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmExtClassRec.ext_class.syn_resources;
  nsyn = xmExtClassRec.ext_class.num_syn_resources;
  print_syns("ExtObj", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmDesktopClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDesktopClassRec.ext_class.syn_resources;
  nsyn = xmDesktopClassRec.ext_class.num_syn_resources;
  print_syns("Desktop", syn, nsyn);

#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmDialogShellExtClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDialogShellExtClassRec.ext_class.syn_resources;
  nsyn = xmDialogShellExtClassRec.ext_class.num_syn_resources;
  print_syns("DialogShellExt", syn, nsyn);
#endif
 
  w = XtCreateWidget("one", (WidgetClass)&xmProtocolClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmProtocolClassRec.ext_class.syn_resources;
  nsyn = xmProtocolClassRec.ext_class.num_syn_resources;
  print_syns("Protocols", syn, nsyn);

#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmShellExtClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmShellExtClassRec.ext_class.syn_resources;
  nsyn = xmShellExtClassRec.ext_class.num_syn_resources;
  print_syns("ShellExt", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmVendorShellExtClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmVendorShellExtClassRec.ext_class.syn_resources;
  nsyn = xmVendorShellExtClassRec.ext_class.num_syn_resources;
  print_syns("VendorShellExt", syn, nsyn);
#endif

#ifdef LESSTIF_VERSION
  w = XtCreateWidget("one", (WidgetClass)&xmWorldClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmWorldClassRec.ext_class.syn_resources;
  nsyn = xmWorldClassRec.ext_class.num_syn_resources;
  print_syns("World", syn, nsyn);
#endif

#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmSeparatorGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSeparatorGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmSeparatorGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("SeparatorGCache", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmLabelGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmLabelGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmLabelGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("LabelGCache", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmCascadeButtonGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmCascadeButtonGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmCascadeButtonGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("CascadeButtonGCache", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmPushButtonGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPushButtonGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmPushButtonGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("PushButtonGCache", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmToggleButtonGCacheObjClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmToggleButtonGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmToggleButtonGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("ToggleButtonGCache", syn, nsyn);
#endif

  /* GADGETS */
#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("Gadget", syn, nsyn);
#endif

  w = XtCreateWidget("one", (WidgetClass)&xmArrowButtonGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmArrowButtonGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmArrowButtonGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("ArrowButtonGadget", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmSeparatorGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSeparatorGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmSeparatorGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("SeparatorGadget", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmLabelGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmLabelGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmLabelGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("LabelGadget", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmCascadeButtonGadgetClassRec, two, NULL, 0);
  XtDestroyWidget(w);
  syn = xmCascadeButtonGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmCascadeButtonGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("CascadeButtonGadget", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmPushButtonGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPushButtonGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmPushButtonGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("PushButtonGadget", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmToggleButtonGadgetClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmToggleButtonGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmToggleButtonGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("ToggleButtonGadget", syn, nsyn);

  /* PRIMITIVES */
#if 0
/* not subclassed directly */
  w = XtCreateWidget("one", (WidgetClass)&xmPrimitiveClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPrimitiveClassRec.primitive_class.syn_resources;
  nsyn = xmPrimitiveClassRec.primitive_class.num_syn_resources;
  print_syns("Primitive", syn, nsyn);
#endif

  w = XtCreateWidget("one", (WidgetClass)&xmArrowButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmArrowButtonClassRec.primitive_class.syn_resources;
  nsyn = xmArrowButtonClassRec.primitive_class.num_syn_resources;
  print_syns("ArrowButton", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmSeparatorClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSeparatorClassRec.primitive_class.syn_resources;
  nsyn = xmSeparatorClassRec.primitive_class.num_syn_resources;
  print_syns("Separator", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmListClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmListClassRec.primitive_class.syn_resources;
  nsyn = xmListClassRec.primitive_class.num_syn_resources;
  print_syns("List", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmSashClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSashClassRec.primitive_class.syn_resources;
  nsyn = xmSashClassRec.primitive_class.num_syn_resources;
  print_syns("Sash", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmScrollBarClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmScrollBarClassRec.primitive_class.syn_resources;
  nsyn = xmScrollBarClassRec.primitive_class.num_syn_resources;
  print_syns("ScrollBar", syn, nsyn);

#if 0
/* FIXME: Rob, this shouldn't dump core; it does though, in _XmFontListEntryFromTag */
  w = XtCreateWidget("one", (WidgetClass)&xmTextFieldClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmTextFieldClassRec.primitive_class.syn_resources;
  nsyn = xmTextFieldClassRec.primitive_class.num_syn_resources;
  print_syns("TextField", syn, nsyn);
#endif

  w = XtCreateWidget("one", (WidgetClass)&xmTextClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmTextClassRec.primitive_class.syn_resources;
  nsyn = xmTextClassRec.primitive_class.num_syn_resources;
  print_syns("Text", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmLabelClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmLabelClassRec.primitive_class.syn_resources;
  nsyn = xmLabelClassRec.primitive_class.num_syn_resources;
  print_syns("Label", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmCascadeButtonClassRec, two, NULL, 0);
  XtDestroyWidget(w);
  syn = xmCascadeButtonClassRec.primitive_class.syn_resources;
  nsyn = xmCascadeButtonClassRec.primitive_class.num_syn_resources;
  print_syns("CascadeButton", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmDrawnButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDrawnButtonClassRec.primitive_class.syn_resources;
  nsyn = xmDrawnButtonClassRec.primitive_class.num_syn_resources;
  print_syns("DrawnButton", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmPushButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPushButtonClassRec.primitive_class.syn_resources;
  nsyn = xmPushButtonClassRec.primitive_class.num_syn_resources;
  print_syns("PushButton", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmToggleButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmToggleButtonClassRec.primitive_class.syn_resources;
  nsyn = xmToggleButtonClassRec.primitive_class.num_syn_resources;
  print_syns("ToggleButton", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmTearOffButtonClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmTearOffButtonClassRec.primitive_class.syn_resources;
  nsyn = xmTearOffButtonClassRec.primitive_class.num_syn_resources;
  print_syns("TearOffButton", syn, nsyn);

  /* MANAGERS */
#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmManagerClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmManagerClassRec.manager_class.syn_resources;
  nsyn = xmManagerClassRec.manager_class.num_syn_resources;
  print_syns("Manager", syn, nsyn);
  syn = xmManagerClassRec.manager_class.syn_constraint_resources;
  nsyn = xmManagerClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Manager Constraint", syn, nsyn);
#endif

  w = XtCreateWidget("one", (WidgetClass)&xmBulletinBoardClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmBulletinBoardClassRec.manager_class.syn_resources;
  nsyn = xmBulletinBoardClassRec.manager_class.num_syn_resources;
  print_syns("BulletinBoard", syn, nsyn);
  syn = xmBulletinBoardClassRec.manager_class.syn_constraint_resources;
  nsyn = xmBulletinBoardClassRec.manager_class.num_syn_constraint_resources;
  print_syns("BulletinBoard Constraint", syn, nsyn);

#if 0
/* Danny, this shouldn't dump core.  It does, though in _XmFontListEntryFromTag */
  w = XtCreateWidget("one", (WidgetClass)&xmCommandClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmCommandClassRec.manager_class.syn_resources;
  nsyn = xmCommandClassRec.manager_class.num_syn_resources;
  print_syns("Command", syn, nsyn);
  syn = xmCommandClassRec.manager_class.syn_constraint_resources;
  nsyn = xmCommandClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Command Constraint", syn, nsyn);
#endif

  w = XtCreateWidget("one", (WidgetClass)&xmDrawingAreaClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDrawingAreaClassRec.manager_class.syn_resources;
  nsyn = xmDrawingAreaClassRec.manager_class.num_syn_resources;
  print_syns("DrawingArea", syn, nsyn);
  syn = xmDrawingAreaClassRec.manager_class.syn_constraint_resources;
  nsyn = xmDrawingAreaClassRec.manager_class.num_syn_constraint_resources;
  print_syns("DrawingArea Constraint", syn, nsyn);

#if 0
/* FIXME: Mitch, this shouldn't dump core.  It does though, in _XmFontListEntryFromTag.
 * Since I (Mitch) am writing this, I'll immediate cast blame elsewhere and say
 * this routine is broken.  I'm probably wrong, though.
 */
  w = XtCreateWidget("one", (WidgetClass)&xmFileSelectionBoxClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmFileSelectionBoxClassRec.manager_class.syn_resources;
  nsyn = xmFileSelectionBoxClassRec.manager_class.num_syn_resources;
  print_syns("FileSelectionBox", syn, nsyn);
  syn = xmFileSelectionBoxClassRec.manager_class.syn_constraint_resources;
  nsyn = xmFileSelectionBoxClassRec.manager_class.num_syn_constraint_resources;
  print_syns("FileSelectionBox Constraint", syn, nsyn);
#endif

  w = XtCreateWidget("one", (WidgetClass)&xmFormClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmFormClassRec.manager_class.syn_resources;
  nsyn = xmFormClassRec.manager_class.num_syn_resources;
  print_syns("Form", syn, nsyn);
  syn = xmFormClassRec.manager_class.syn_constraint_resources;
  nsyn = xmFormClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Form Constraint", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmFrameClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmFrameClassRec.manager_class.syn_resources;
  nsyn = xmFrameClassRec.manager_class.num_syn_resources;
  print_syns("Frame", syn, nsyn);
  syn = xmFrameClassRec.manager_class.syn_constraint_resources;
  nsyn = xmFrameClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Frame Constraint", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmMainWindowClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmMainWindowClassRec.manager_class.syn_resources;
  nsyn = xmMainWindowClassRec.manager_class.num_syn_resources;
  print_syns("MainWindow", syn, nsyn);
  syn = xmMainWindowClassRec.manager_class.syn_constraint_resources;
  nsyn = xmMainWindowClassRec.manager_class.num_syn_constraint_resources;
  print_syns("MainWindow Constraint", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmMessageBoxClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmMessageBoxClassRec.manager_class.syn_resources;
  nsyn = xmMessageBoxClassRec.manager_class.num_syn_resources;
  print_syns("MessageBox", syn, nsyn);
  syn = xmMessageBoxClassRec.manager_class.syn_constraint_resources;
  nsyn = xmMessageBoxClassRec.manager_class.num_syn_constraint_resources;
  print_syns("MessageBox Constraint", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmPanedWindowClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmPanedWindowClassRec.manager_class.syn_resources;
  nsyn = xmPanedWindowClassRec.manager_class.num_syn_resources;
  print_syns("PanedWindow", syn, nsyn);
  syn = xmPanedWindowClassRec.manager_class.syn_constraint_resources;
  nsyn = xmPanedWindowClassRec.manager_class.num_syn_constraint_resources;
  print_syns("PanedWindow Constraint", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmRowColumnClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmRowColumnClassRec.manager_class.syn_resources;
  nsyn = xmRowColumnClassRec.manager_class.num_syn_resources;
  print_syns("RowColumn", syn, nsyn);
  syn = xmRowColumnClassRec.manager_class.syn_constraint_resources;
  nsyn = xmRowColumnClassRec.manager_class.num_syn_constraint_resources;
  print_syns("RowColumn Constraint", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmScaleClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmScaleClassRec.manager_class.syn_resources;
  nsyn = xmScaleClassRec.manager_class.num_syn_resources;
  print_syns("Scale", syn, nsyn);
  syn = xmScaleClassRec.manager_class.syn_constraint_resources;
  nsyn = xmScaleClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Scale Constraint", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmScrolledWindowClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmScrolledWindowClassRec.manager_class.syn_resources;
  nsyn = xmScrolledWindowClassRec.manager_class.num_syn_resources;
  print_syns("ScrolledWindow", syn, nsyn);
  syn = xmScrolledWindowClassRec.manager_class.syn_constraint_resources;
  nsyn = xmScrolledWindowClassRec.manager_class.num_syn_constraint_resources;
  print_syns("ScrolledWindow Constraint", syn, nsyn);

#if 0
/* Danny, this shouldn't dump core.  It does, though in _XmFontListEntryFromTag */
  w = XtCreateWidget("one", (WidgetClass)&xmSelectionBoxClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmSelectionBoxClassRec.manager_class.syn_resources;
  nsyn = xmSelectionBoxClassRec.manager_class.num_syn_resources;
  print_syns("SelectionBox", syn, nsyn);
  syn = xmSelectionBoxClassRec.manager_class.syn_constraint_resources;
  nsyn = xmSelectionBoxClassRec.manager_class.num_syn_constraint_resources;
  print_syns("SelectionBox Constraint", syn, nsyn);
#endif

  /* SHELLS */
#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmDialogShellClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDialogShellClassRec.manager_class.syn_resources;
  nsyn = xmDialogShellClassRec.manager_class.num_syn_resources;
  print_syns("DialogShell", syn, nsyn);

  syn = vendorShellClassRec.manager_class.syn_resources;
  nsyn = vendorShellClassRec.manager_class.num_syn_resources;
  w = XtCreateWidget("one", (WidgetClass)&vendorShellClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  print_syns("VendorShell", syn, nsyn);

  w = XtCreateWidget("one", (WidgetClass)&xmMenuShellClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmMenuShellClassRec.manager_class.syn_resources;
  nsyn = xmMenuShellClassRec.manager_class.num_syn_resources;
  print_syns("MenuShell", syn, nsyn);
#endif

  /* OTHER */
#if 0
  w = XtCreateWidget("one", (WidgetClass)&xmDisplayClassRec, toplevel, NULL, 0);
  XtDestroyWidget(w);
  syn = xmDisplayClassRec.manager_class.syn_resources;
  nsyn = xmDisplayClassRec.manager_class.num_syn_resources;
  print_syns("Display", syn, nsyn);

#if 0
/* OOPS: only one of these per screen */
  w = XtCreateWidget("one", (WidgetClass)&xmScreenClassRec, toplevel, NULL, 0);
#else
  w = XmGetXmScreen(XtScreen(toplevel));
#endif
  syn = xmScreenClassRec.manager_class.syn_resources;
  nsyn = xmScreenClassRec.manager_class.num_syn_resources;
  print_syns("Screen", syn, nsyn);
#endif

  exit(0);
}
