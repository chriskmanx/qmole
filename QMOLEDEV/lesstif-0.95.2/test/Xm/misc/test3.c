/* $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test3.c,v 1.3 2001/05/15 14:20:17 amai Exp $
  test for M*tif to see what the ResInd (Synth) resources look like */

#include <stdio.h>
#include <stdlib.h>

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

void
print_syns(char *class, XmSyntheticResource *syn, int nsyn)
{
  int i;

  printf( "%s: %d\n", class, nsyn);
  for (i = 0; i < nsyn; i++) {
    printf("syn_res[%d]: name: %-20s offset: %-3d size: %-3d\n\texport: %p import: %p\n", i,
	syn[i].resource_name, syn[i].resource_offset,
	syn[i].resource_size, syn[i].export_proc, syn[i].import_proc);
  }
  printf("\n");
}

int
main(int argc, char **argv)
{
  XmSyntheticResource *syn;
  int nsyn;

  /* EXT OBJS */
  syn = xmExtClassRec.ext_class.syn_resources;
  nsyn = xmExtClassRec.ext_class.num_syn_resources;
  print_syns("ExtObj", syn, nsyn);

  syn = xmDesktopClassRec.ext_class.syn_resources;
  nsyn = xmDesktopClassRec.ext_class.num_syn_resources;
  print_syns("Desktop", syn, nsyn);

  syn = xmDialogShellExtClassRec.ext_class.syn_resources;
  nsyn = xmDialogShellExtClassRec.ext_class.num_syn_resources;
  print_syns("DialogShellExt", syn, nsyn);

  syn = xmProtocolClassRec.ext_class.syn_resources;
  nsyn = xmProtocolClassRec.ext_class.num_syn_resources;
  print_syns("Protocols", syn, nsyn);

  syn = xmShellExtClassRec.ext_class.syn_resources;
  nsyn = xmShellExtClassRec.ext_class.num_syn_resources;
  print_syns("ShellExt", syn, nsyn);

  syn = xmVendorShellExtClassRec.ext_class.syn_resources;
  nsyn = xmVendorShellExtClassRec.ext_class.num_syn_resources;
  print_syns("VendorShellExt", syn, nsyn);

#ifdef LESSTIF_VERSION
  syn = xmWorldClassRec.ext_class.syn_resources;
  nsyn = xmWorldClassRec.ext_class.num_syn_resources;
  print_syns("World", syn, nsyn);
#endif

  syn = xmSeparatorGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmSeparatorGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("SeparatorGCache", syn, nsyn);

  syn = xmLabelGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmLabelGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("LabelGCache", syn, nsyn);

  syn = xmCascadeButtonGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmCascadeButtonGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("CascadeButtonGCache", syn, nsyn);

  syn = xmPushButtonGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmPushButtonGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("PushButtonGCache", syn, nsyn);

  syn = xmToggleButtonGCacheObjClassRec.ext_class.syn_resources;
  nsyn = xmToggleButtonGCacheObjClassRec.ext_class.num_syn_resources;
  print_syns("ToggleButtonGCache", syn, nsyn);

  /* GADGETS */
  syn = xmGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("Gadget", syn, nsyn);

  syn = xmArrowButtonGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmArrowButtonGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("ArrowButtonGadget", syn, nsyn);

  syn = xmSeparatorGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmSeparatorGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("SeparatorGadget", syn, nsyn);

  syn = xmLabelGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmLabelGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("LabelGadget", syn, nsyn);

  syn = xmCascadeButtonGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmCascadeButtonGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("CascadeButtonGadget", syn, nsyn);

  syn = xmPushButtonGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmPushButtonGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("PushButtonGadget", syn, nsyn);

  syn = xmToggleButtonGadgetClassRec.gadget_class.syn_resources;
  nsyn = xmToggleButtonGadgetClassRec.gadget_class.num_syn_resources;
  print_syns("ToggleButtonGadget", syn, nsyn);

  /* PRIMITIVES */
  syn = xmPrimitiveClassRec.primitive_class.syn_resources;
  nsyn = xmPrimitiveClassRec.primitive_class.num_syn_resources;
  print_syns("Primitive", syn, nsyn);

  syn = xmArrowButtonClassRec.primitive_class.syn_resources;
  nsyn = xmArrowButtonClassRec.primitive_class.num_syn_resources;
  print_syns("ArrowButton", syn, nsyn);

  syn = xmSeparatorClassRec.primitive_class.syn_resources;
  nsyn = xmSeparatorClassRec.primitive_class.num_syn_resources;
  print_syns("Separator", syn, nsyn);

  syn = xmListClassRec.primitive_class.syn_resources;
  nsyn = xmListClassRec.primitive_class.num_syn_resources;
  print_syns("List", syn, nsyn);

  syn = xmSashClassRec.primitive_class.syn_resources;
  nsyn = xmSashClassRec.primitive_class.num_syn_resources;
  print_syns("Sash", syn, nsyn);

  syn = xmScrollBarClassRec.primitive_class.syn_resources;
  nsyn = xmScrollBarClassRec.primitive_class.num_syn_resources;
  print_syns("ScrollBar", syn, nsyn);

  syn = xmTextFieldClassRec.primitive_class.syn_resources;
  nsyn = xmTextFieldClassRec.primitive_class.num_syn_resources;
  print_syns("TextField", syn, nsyn);

  syn = xmTextClassRec.primitive_class.syn_resources;
  nsyn = xmTextClassRec.primitive_class.num_syn_resources;
  print_syns("Text", syn, nsyn);

  syn = xmLabelClassRec.primitive_class.syn_resources;
  nsyn = xmLabelClassRec.primitive_class.num_syn_resources;
  print_syns("Label", syn, nsyn);

  syn = xmCascadeButtonClassRec.primitive_class.syn_resources;
  nsyn = xmCascadeButtonClassRec.primitive_class.num_syn_resources;
  print_syns("CascadeButton", syn, nsyn);

  syn = xmDrawnButtonClassRec.primitive_class.syn_resources;
  nsyn = xmDrawnButtonClassRec.primitive_class.num_syn_resources;
  print_syns("DrawnButton", syn, nsyn);

  syn = xmPushButtonClassRec.primitive_class.syn_resources;
  nsyn = xmPushButtonClassRec.primitive_class.num_syn_resources;
  print_syns("PushButton", syn, nsyn);

  syn = xmToggleButtonClassRec.primitive_class.syn_resources;
  nsyn = xmToggleButtonClassRec.primitive_class.num_syn_resources;
  print_syns("ToggleButton", syn, nsyn);

  syn = xmTearOffButtonClassRec.primitive_class.syn_resources;
  nsyn = xmTearOffButtonClassRec.primitive_class.num_syn_resources;
  print_syns("TearOffButton", syn, nsyn);

  /* MANAGERS */
  syn = xmManagerClassRec.manager_class.syn_resources;
  nsyn = xmManagerClassRec.manager_class.num_syn_resources;
  print_syns("Manager", syn, nsyn);
  syn = xmManagerClassRec.manager_class.syn_constraint_resources;
  nsyn = xmManagerClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Manager Constraint", syn, nsyn);

  syn = xmBulletinBoardClassRec.manager_class.syn_resources;
  nsyn = xmBulletinBoardClassRec.manager_class.num_syn_resources;
  print_syns("BulletinBoard", syn, nsyn);
  syn = xmBulletinBoardClassRec.manager_class.syn_constraint_resources;
  nsyn = xmBulletinBoardClassRec.manager_class.num_syn_constraint_resources;
  print_syns("BulletinBoard Constraint", syn, nsyn);

  syn = xmCommandClassRec.manager_class.syn_resources;
  nsyn = xmCommandClassRec.manager_class.num_syn_resources;
  print_syns("Command", syn, nsyn);
  syn = xmCommandClassRec.manager_class.syn_constraint_resources;
  nsyn = xmCommandClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Command Constraint", syn, nsyn);

  syn = xmDrawingAreaClassRec.manager_class.syn_resources;
  nsyn = xmDrawingAreaClassRec.manager_class.num_syn_resources;
  print_syns("DrawingArea", syn, nsyn);
  syn = xmDrawingAreaClassRec.manager_class.syn_constraint_resources;
  nsyn = xmDrawingAreaClassRec.manager_class.num_syn_constraint_resources;
  print_syns("DrawingArea Constraint", syn, nsyn);

  syn = xmFileSelectionBoxClassRec.manager_class.syn_resources;
  nsyn = xmFileSelectionBoxClassRec.manager_class.num_syn_resources;
  print_syns("FileSelectionBox", syn, nsyn);
  syn = xmFileSelectionBoxClassRec.manager_class.syn_constraint_resources;
  nsyn = xmFileSelectionBoxClassRec.manager_class.num_syn_constraint_resources;
  print_syns("FileSelectionBox Constraint", syn, nsyn);

  syn = xmFormClassRec.manager_class.syn_resources;
  nsyn = xmFormClassRec.manager_class.num_syn_resources;
  print_syns("Form", syn, nsyn);
  syn = xmFormClassRec.manager_class.syn_constraint_resources;
  nsyn = xmFormClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Form Constraint", syn, nsyn);

  syn = xmFrameClassRec.manager_class.syn_resources;
  nsyn = xmFrameClassRec.manager_class.num_syn_resources;
  print_syns("Frame", syn, nsyn);
  syn = xmFrameClassRec.manager_class.syn_constraint_resources;
  nsyn = xmFrameClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Frame Constraint", syn, nsyn);

  syn = xmMainWindowClassRec.manager_class.syn_resources;
  nsyn = xmMainWindowClassRec.manager_class.num_syn_resources;
  print_syns("MainWindow", syn, nsyn);
  syn = xmMainWindowClassRec.manager_class.syn_constraint_resources;
  nsyn = xmMainWindowClassRec.manager_class.num_syn_constraint_resources;
  print_syns("MainWindow Constraint", syn, nsyn);

  syn = xmMessageBoxClassRec.manager_class.syn_resources;
  nsyn = xmMessageBoxClassRec.manager_class.num_syn_resources;
  print_syns("MessageBox", syn, nsyn);
  syn = xmMessageBoxClassRec.manager_class.syn_constraint_resources;
  nsyn = xmMessageBoxClassRec.manager_class.num_syn_constraint_resources;
  print_syns("MessageBox Constraint", syn, nsyn);

  syn = xmPanedWindowClassRec.manager_class.syn_resources;
  nsyn = xmPanedWindowClassRec.manager_class.num_syn_resources;
  print_syns("PanedWindow", syn, nsyn);
  syn = xmPanedWindowClassRec.manager_class.syn_constraint_resources;
  nsyn = xmPanedWindowClassRec.manager_class.num_syn_constraint_resources;
  print_syns("PanedWindow Constraint", syn, nsyn);

  syn = xmRowColumnClassRec.manager_class.syn_resources;
  nsyn = xmRowColumnClassRec.manager_class.num_syn_resources;
  print_syns("RowColumn", syn, nsyn);
  syn = xmRowColumnClassRec.manager_class.syn_constraint_resources;
  nsyn = xmRowColumnClassRec.manager_class.num_syn_constraint_resources;
  print_syns("RowColumn Constraint", syn, nsyn);

  syn = xmScaleClassRec.manager_class.syn_resources;
  nsyn = xmScaleClassRec.manager_class.num_syn_resources;
  print_syns("Scale", syn, nsyn);
  syn = xmScaleClassRec.manager_class.syn_constraint_resources;
  nsyn = xmScaleClassRec.manager_class.num_syn_constraint_resources;
  print_syns("Scale Constraint", syn, nsyn);

  syn = xmScrolledWindowClassRec.manager_class.syn_resources;
  nsyn = xmScrolledWindowClassRec.manager_class.num_syn_resources;
  print_syns("ScrolledWindow", syn, nsyn);
  syn = xmScrolledWindowClassRec.manager_class.syn_constraint_resources;
  nsyn = xmScrolledWindowClassRec.manager_class.num_syn_constraint_resources;
  print_syns("ScrolledWindow Constraint", syn, nsyn);

  syn = xmSelectionBoxClassRec.manager_class.syn_resources;
  nsyn = xmSelectionBoxClassRec.manager_class.num_syn_resources;
  print_syns("SelectionBox", syn, nsyn);
  syn = xmSelectionBoxClassRec.manager_class.syn_constraint_resources;
  nsyn = xmSelectionBoxClassRec.manager_class.num_syn_constraint_resources;
  print_syns("SelectionBox Constraint", syn, nsyn);

  exit(0);
}
