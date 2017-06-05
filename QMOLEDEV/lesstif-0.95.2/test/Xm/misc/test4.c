/* test for M*tif to see what the Resources look like */
/* $Id: test4.c,v 1.3 2001/05/06 22:14:52 rwscott Exp $ */

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

void
print_syns(char *class, XtResource *res, int nres)
{
  int i;

  printf( "%s: %d\n", class, nres);
  for (i = 0; i < nres; i++) {
    printf("    /* %d */\n", i);
    printf("    {\n");
    printf("\tXmN%s, XmC%s, XmR%s,\n",
	   res[i].resource_name, res[i].resource_class, res[i].resource_type);
    printf("\tsize: %-3d offset: %-3d\n",
	   res[i].resource_size, res[i].resource_offset);
    printf("\tXmR%s, (XtPointer)%p\n",
	   res[i].default_type, res[i].default_addr);
    printf("    },\n");
  }
  printf("\n");
}

int
main(int argc, char **argv)
{
  XtResource *syn;
  int nsyn;

  /* ODDITIES */
  syn = xmDragContextClassRec.core_class.resources;
  nsyn = xmDragContextClassRec.core_class.num_resources;
  print_syns("DragContext", syn, nsyn);

  syn = xmDragIconClassRec.rectangle_class.resources;
  nsyn = xmDragIconClassRec.rectangle_class.num_resources;
  print_syns("DragIcon", syn, nsyn);

  syn = xmDropTransferClassRec.object_class.resources;
  nsyn = xmDropTransferClassRec.object_class.num_resources;
  print_syns("DropTransfer", syn, nsyn);

  /* EXT OBJS */
  syn = xmExtClassRec.object_class.resources;
  nsyn = xmExtClassRec.object_class.num_resources;
  print_syns("ExtObj", syn, nsyn);

  syn = xmDesktopClassRec.object_class.resources;
  nsyn = xmDesktopClassRec.object_class.num_resources;
  print_syns("Desktop", syn, nsyn);

  syn = xmDialogShellExtClassRec.object_class.resources;
  nsyn = xmDialogShellExtClassRec.object_class.num_resources;
  print_syns("DialogShellExt", syn, nsyn);
 
  syn = xmProtocolClassRec.object_class.resources;
  nsyn = xmProtocolClassRec.object_class.num_resources;
  print_syns("Protocols", syn, nsyn);

  syn = xmShellExtClassRec.object_class.resources;
  nsyn = xmShellExtClassRec.object_class.num_resources;
  print_syns("ShellExt", syn, nsyn);

  syn = xmVendorShellExtClassRec.object_class.resources;
  nsyn = xmVendorShellExtClassRec.object_class.num_resources;
  print_syns("VendorShellExt", syn, nsyn);

#ifdef LESSTIF_VERSION
  syn = xmWorldClassRec.object_class.resources;
  nsyn = xmWorldClassRec.object_class.num_resources;
  print_syns("World", syn, nsyn);
#endif

  syn = xmSeparatorGCacheObjClassRec.object_class.resources;
  nsyn = xmSeparatorGCacheObjClassRec.object_class.num_resources;
  print_syns("SeparatorGCache", syn, nsyn);

  syn = xmLabelGCacheObjClassRec.object_class.resources;
  nsyn = xmLabelGCacheObjClassRec.object_class.num_resources;
  print_syns("LabelGCache", syn, nsyn);

  syn = xmCascadeButtonGCacheObjClassRec.object_class.resources;
  nsyn = xmCascadeButtonGCacheObjClassRec.object_class.num_resources;
  print_syns("CascadeButtonGCache", syn, nsyn);

  syn = xmPushButtonGCacheObjClassRec.object_class.resources;
  nsyn = xmPushButtonGCacheObjClassRec.object_class.num_resources;
  print_syns("PushButtonGCache", syn, nsyn);

  syn = xmToggleButtonGCacheObjClassRec.object_class.resources;
  nsyn = xmToggleButtonGCacheObjClassRec.object_class.num_resources;
  print_syns("ToggleButtonGCache", syn, nsyn);

  /* GADGETS */
  syn = xmGadgetClassRec.rect_class.resources;
  nsyn = xmGadgetClassRec.rect_class.num_resources;
  print_syns("Gadget", syn, nsyn);

  syn = xmArrowButtonGadgetClassRec.rect_class.resources;
  nsyn = xmArrowButtonGadgetClassRec.rect_class.num_resources;
  print_syns("ArrowButtonGadget", syn, nsyn);

  syn = xmSeparatorGadgetClassRec.rect_class.resources;
  nsyn = xmSeparatorGadgetClassRec.rect_class.num_resources;
  print_syns("SeparatorGadget", syn, nsyn);

  syn = xmLabelGadgetClassRec.rect_class.resources;
  nsyn = xmLabelGadgetClassRec.rect_class.num_resources;
  print_syns("LabelGadget", syn, nsyn);

  syn = xmCascadeButtonGadgetClassRec.rect_class.resources;
  nsyn = xmCascadeButtonGadgetClassRec.rect_class.num_resources;
  print_syns("CascadeButtonGadget", syn, nsyn);

  syn = xmPushButtonGadgetClassRec.rect_class.resources;
  nsyn = xmPushButtonGadgetClassRec.rect_class.num_resources;
  print_syns("PushButtonGadget", syn, nsyn);

  syn = xmToggleButtonGadgetClassRec.rect_class.resources;
  nsyn = xmToggleButtonGadgetClassRec.rect_class.num_resources;
  print_syns("ToggleButtonGadget", syn, nsyn);

  /* PRIMITIVES */
  syn = xmPrimitiveClassRec.core_class.resources;
  nsyn = xmPrimitiveClassRec.core_class.num_resources;
  print_syns("Primitive", syn, nsyn);

  syn = xmArrowButtonClassRec.core_class.resources;
  nsyn = xmArrowButtonClassRec.core_class.num_resources;
  print_syns("ArrowButton", syn, nsyn);

  syn = xmSeparatorClassRec.core_class.resources;
  nsyn = xmSeparatorClassRec.core_class.num_resources;
  print_syns("Separator", syn, nsyn);

  syn = xmListClassRec.core_class.resources;
  nsyn = xmListClassRec.core_class.num_resources;
  print_syns("List", syn, nsyn);

  syn = xmSashClassRec.core_class.resources;
  nsyn = xmSashClassRec.core_class.num_resources;
  print_syns("Sash", syn, nsyn);

  syn = xmScrollBarClassRec.core_class.resources;
  nsyn = xmScrollBarClassRec.core_class.num_resources;
  print_syns("ScrollBar", syn, nsyn);

  syn = xmTextFieldClassRec.core_class.resources;
  nsyn = xmTextFieldClassRec.core_class.num_resources;
  print_syns("TextField", syn, nsyn);

  syn = xmTextClassRec.core_class.resources;
  nsyn = xmTextClassRec.core_class.num_resources;
  print_syns("Text", syn, nsyn);

  syn = xmLabelClassRec.core_class.resources;
  nsyn = xmLabelClassRec.core_class.num_resources;
  print_syns("Label", syn, nsyn);

  syn = xmCascadeButtonClassRec.core_class.resources;
  nsyn = xmCascadeButtonClassRec.core_class.num_resources;
  print_syns("CascadeButton", syn, nsyn);

  syn = xmDrawnButtonClassRec.core_class.resources;
  nsyn = xmDrawnButtonClassRec.core_class.num_resources;
  print_syns("DrawnButton", syn, nsyn);

  syn = xmPushButtonClassRec.core_class.resources;
  nsyn = xmPushButtonClassRec.core_class.num_resources;
  print_syns("PushButton", syn, nsyn);

  syn = xmToggleButtonClassRec.core_class.resources;
  nsyn = xmToggleButtonClassRec.core_class.num_resources;
  print_syns("ToggleButton", syn, nsyn);

  syn = xmTearOffButtonClassRec.core_class.resources;
  nsyn = xmTearOffButtonClassRec.core_class.num_resources;
  print_syns("TearOffButton", syn, nsyn);

  /* MANAGERS */
  syn = xmManagerClassRec.core_class.resources;
  nsyn = xmManagerClassRec.core_class.num_resources;
  print_syns("Manager", syn, nsyn);
  syn = xmManagerClassRec.constraint_class.resources;
  nsyn = xmManagerClassRec.constraint_class.num_resources;
  print_syns("Manager Constraint", syn, nsyn);

  syn = xmBulletinBoardClassRec.core_class.resources;
  nsyn = xmBulletinBoardClassRec.core_class.num_resources;
  print_syns("BulletinBoard", syn, nsyn);
  syn = xmBulletinBoardClassRec.constraint_class.resources;
  nsyn = xmBulletinBoardClassRec.constraint_class.num_resources;
  print_syns("BulletinBoard Constraint", syn, nsyn);

  syn = xmCommandClassRec.core_class.resources;
  nsyn = xmCommandClassRec.core_class.num_resources;
  print_syns("Command", syn, nsyn);
  syn = xmCommandClassRec.constraint_class.resources;
  nsyn = xmCommandClassRec.constraint_class.num_resources;
  print_syns("Command Constraint", syn, nsyn);

  syn = xmDrawingAreaClassRec.core_class.resources;
  nsyn = xmDrawingAreaClassRec.core_class.num_resources;
  print_syns("DrawingArea", syn, nsyn);
  syn = xmDrawingAreaClassRec.constraint_class.resources;
  nsyn = xmDrawingAreaClassRec.constraint_class.num_resources;
  print_syns("DrawingArea Constraint", syn, nsyn);

  syn = xmFileSelectionBoxClassRec.core_class.resources;
  nsyn = xmFileSelectionBoxClassRec.core_class.num_resources;
  print_syns("FileSelectionBox", syn, nsyn);
  syn = xmFileSelectionBoxClassRec.constraint_class.resources;
  nsyn = xmFileSelectionBoxClassRec.constraint_class.num_resources;
  print_syns("FileSelectionBox Constraint", syn, nsyn);

  syn = xmFormClassRec.core_class.resources;
  nsyn = xmFormClassRec.core_class.num_resources;
  print_syns("Form", syn, nsyn);
  syn = xmFormClassRec.constraint_class.resources;
  nsyn = xmFormClassRec.constraint_class.num_resources;
  print_syns("Form Constraint", syn, nsyn);

  syn = xmFrameClassRec.core_class.resources;
  nsyn = xmFrameClassRec.core_class.num_resources;
  print_syns("Frame", syn, nsyn);
  syn = xmFrameClassRec.constraint_class.resources;
  nsyn = xmFrameClassRec.constraint_class.num_resources;
  print_syns("Frame Constraint", syn, nsyn);

  syn = xmMainWindowClassRec.core_class.resources;
  nsyn = xmMainWindowClassRec.core_class.num_resources;
  print_syns("MainWindow", syn, nsyn);
  syn = xmMainWindowClassRec.constraint_class.resources;
  nsyn = xmMainWindowClassRec.constraint_class.num_resources;
  print_syns("MainWindow Constraint", syn, nsyn);

  syn = xmMessageBoxClassRec.core_class.resources;
  nsyn = xmMessageBoxClassRec.core_class.num_resources;
  print_syns("MessageBox", syn, nsyn);
  syn = xmMessageBoxClassRec.constraint_class.resources;
  nsyn = xmMessageBoxClassRec.constraint_class.num_resources;
  print_syns("MessageBox Constraint", syn, nsyn);

  syn = xmPanedWindowClassRec.core_class.resources;
  nsyn = xmPanedWindowClassRec.core_class.num_resources;
  print_syns("PanedWindow", syn, nsyn);
  syn = xmPanedWindowClassRec.constraint_class.resources;
  nsyn = xmPanedWindowClassRec.constraint_class.num_resources;
  print_syns("PanedWindow Constraint", syn, nsyn);

  syn = xmRowColumnClassRec.core_class.resources;
  nsyn = xmRowColumnClassRec.core_class.num_resources;
  print_syns("RowColumn", syn, nsyn);
  syn = xmRowColumnClassRec.constraint_class.resources;
  nsyn = xmRowColumnClassRec.constraint_class.num_resources;
  print_syns("RowColumn Constraint", syn, nsyn);

  syn = xmScaleClassRec.core_class.resources;
  nsyn = xmScaleClassRec.core_class.num_resources;
  print_syns("Scale", syn, nsyn);
  syn = xmScaleClassRec.constraint_class.resources;
  nsyn = xmScaleClassRec.constraint_class.num_resources;
  print_syns("Scale Constraint", syn, nsyn);

  syn = xmScrolledWindowClassRec.core_class.resources;
  nsyn = xmScrolledWindowClassRec.core_class.num_resources;
  print_syns("ScrolledWindow", syn, nsyn);
  syn = xmScrolledWindowClassRec.constraint_class.resources;
  nsyn = xmScrolledWindowClassRec.constraint_class.num_resources;
  print_syns("ScrolledWindow Constraint", syn, nsyn);

  syn = xmSelectionBoxClassRec.core_class.resources;
  nsyn = xmSelectionBoxClassRec.core_class.num_resources;
  print_syns("SelectionBox", syn, nsyn);
  syn = xmSelectionBoxClassRec.constraint_class.resources;
  nsyn = xmSelectionBoxClassRec.constraint_class.num_resources;
  print_syns("SelectionBox Constraint", syn, nsyn);

  /* SHELLS */
  syn = xmDialogShellClassRec.core_class.resources;
  nsyn = xmDialogShellClassRec.core_class.num_resources;
  print_syns("DialogShell", syn, nsyn);

  syn = vendorShellClassRec.core_class.resources;
  nsyn = vendorShellClassRec.core_class.num_resources;
  print_syns("VendorShell", syn, nsyn);

  syn = xmMenuShellClassRec.core_class.resources;
  nsyn = xmMenuShellClassRec.core_class.num_resources;
  print_syns("MenuShell", syn, nsyn);

  /* OTHER */
  syn = xmDisplayClassRec.core_class.resources;
  nsyn = xmDisplayClassRec.core_class.num_resources;
  print_syns("Display", syn, nsyn);

  syn = xmScreenClassRec.core_class.resources;
  nsyn = xmScreenClassRec.core_class.num_resources;
  print_syns("Screen", syn, nsyn);

  exit(0);
}
