/* test for M*tif to see what the ResInd (Synth) resources look like */

/* $Id: test7.c,v 1.3 2001/05/06 22:14:52 rwscott Exp $ */

#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/ExtObjectP.h>
#include <Xm/DesktopP.h>
#include <Xm/DialogSEP.h>
#include <Xm/ProtocolsP.h>
#include <Xm/ShellEP.h>
#include <Xm/VendorSEP.h>

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
print_actions(char *class, XtActionsRec *actions, int num_actions)
{
  int i;

  printf( "%s: %d\n", class, num_actions);
  for (i = 0; i < num_actions; i++) {
    printf("action[%d]: name: %-20s proc: %p\n", i,
	actions[i].string, actions[i].proc);
  }
  printf("\n");
}

int
main(int argc, char **argv)
{
  XtActionsRec *actions;
  int nactions;
  XtAppContext app;
  Widget one;

  one = XtAppInitialize(&app, argv[0], NULL, 0, &argc, argv, NULL, NULL, 0);

#if 0
  /* EXT OBJS */
  actions = xmExtClassRec.ext_class.actions;
  nactions = xmExtClassRec.ext_class.num_actions;
  print_actions("ExtObj", actions, nactions);

  actions = xmDesktopClassRec.ext_class.actions;
  nactions = xmDesktopClassRec.ext_class.num_actions;
  print_actions("Desktop", actions, nactions);

  actions = xmDialogShellExtClassRec.ext_class.actions;
  nactions = xmDialogShellExtClassRec.ext_class.num_actions;
  print_actions("DialogShellExt", actions, nactions);

  actions = xmProtocolClassRec.ext_class.actions;
  nactions = xmProtocolClassRec.ext_class.num_actions;
  print_actions("Protocols", actions, nactions);

  actions = xmShellExtClassRec.ext_class.actions;
  nactions = xmShellExtClassRec.ext_class.num_actions;
  print_actions("ShellExt", actions, nactions);

  actions = xmVendorShellExtClassRec.ext_class.actions;
  nactions = xmVendorShellExtClassRec.ext_class.num_actions;
  print_actions("VendorShellExt", actions, nactions);

  actions = xmWorldClassRec.ext_class.actions;
  nactions = xmWorldClassRec.ext_class.num_actions;
  print_actions("World", actions, nactions);

  actions = xmSeparatorGCacheObjClassRec.ext_class.actions;
  nactions = xmSeparatorGCacheObjClassRec.ext_class.num_actions;
  print_actions("SeparatorGCache", actions, nactions);

  actions = xmLabelGCacheObjClassRec.ext_class.actions;
  nactions = xmLabelGCacheObjClassRec.ext_class.num_actions;
  print_actions("LabelGCache", actions, nactions);

  actions = xmCascadeButtonGCacheObjClassRec.ext_class.actions;
  nactions = xmCascadeButtonGCacheObjClassRec.ext_class.num_actions;
  print_actions("CascadeButtonGCache", actions, nactions);

  actions = xmPushButtonGCacheObjClassRec.ext_class.actions;
  nactions = xmPushButtonGCacheObjClassRec.ext_class.num_actions;
  print_actions("PushButtonGCache", actions, nactions);

  actions = xmToggleButtonGCacheObjClassRec.ext_class.actions;
  nactions = xmToggleButtonGCacheObjClassRec.ext_class.num_actions;
  print_actions("ToggleButtonGCache", actions, nactions);

  /* GADGETS */
  actions = xmGadgetClassRec.gadget_class.actions;
  nactions = xmGadgetClassRec.gadget_class.num_actions;
  print_actions("Gadget", actions, nactions);

  actions = xmArrowButtonGadgetClassRec.gadget_class.actions;
  nactions = xmArrowButtonGadgetClassRec.gadget_class.num_actions;
  print_actions("ArrowButtonGadget", actions, nactions);

  actions = xmSeparatorGadgetClassRec.gadget_class.actions;
  nactions = xmSeparatorGadgetClassRec.gadget_class.num_actions;
  print_actions("SeparatorGadget", actions, nactions);

  actions = xmLabelGadgetClassRec.gadget_class.actions;
  nactions = xmLabelGadgetClassRec.gadget_class.num_actions;
  print_actions("LabelGadget", actions, nactions);

  actions = xmCascadeButtonGadgetClassRec.gadget_class.actions;
  nactions = xmCascadeButtonGadgetClassRec.gadget_class.num_actions;
  print_actions("CascadeButtonGadget", actions, nactions);

  actions = xmPushButtonGadgetClassRec.gadget_class.actions;
  nactions = xmPushButtonGadgetClassRec.gadget_class.num_actions;
  print_actions("PushButtonGadget", actions, nactions);

  actions = xmToggleButtonGadgetClassRec.gadget_class.actions;
  nactions = xmToggleButtonGadgetClassRec.gadget_class.num_actions;
  print_actions("ToggleButtonGadget", actions, nactions);
#endif

  /* PRIMITIVES */
  actions = xmPrimitiveClassRec.core_class.actions;
  nactions = xmPrimitiveClassRec.core_class.num_actions;
  print_actions("Primitive", actions, nactions);

  actions = xmArrowButtonClassRec.core_class.actions;
  nactions = xmArrowButtonClassRec.core_class.num_actions;
  print_actions("ArrowButton", actions, nactions);

  actions = xmSeparatorClassRec.core_class.actions;
  nactions = xmSeparatorClassRec.core_class.num_actions;
  print_actions("Separator", actions, nactions);

  actions = xmListClassRec.core_class.actions;
  nactions = xmListClassRec.core_class.num_actions;
  print_actions("List", actions, nactions);

  actions = xmSashClassRec.core_class.actions;
  nactions = xmSashClassRec.core_class.num_actions;
  print_actions("Sash", actions, nactions);

  actions = xmScrollBarClassRec.core_class.actions;
  nactions = xmScrollBarClassRec.core_class.num_actions;
  print_actions("ScrollBar", actions, nactions);

  actions = xmTextFieldClassRec.core_class.actions;
  nactions = xmTextFieldClassRec.core_class.num_actions;
  print_actions("TextField", actions, nactions);

  actions = xmTextClassRec.core_class.actions;
  nactions = xmTextClassRec.core_class.num_actions;
  print_actions("Text", actions, nactions);

  actions = xmLabelClassRec.core_class.actions;
  nactions = xmLabelClassRec.core_class.num_actions;
  print_actions("Label", actions, nactions);

  actions = xmCascadeButtonClassRec.core_class.actions;
  nactions = xmCascadeButtonClassRec.core_class.num_actions;
  print_actions("CascadeButton", actions, nactions);

  actions = xmDrawnButtonClassRec.core_class.actions;
  nactions = xmDrawnButtonClassRec.core_class.num_actions;
  print_actions("DrawnButton", actions, nactions);

  actions = xmPushButtonClassRec.core_class.actions;
  nactions = xmPushButtonClassRec.core_class.num_actions;
  print_actions("PushButton", actions, nactions);

  actions = xmToggleButtonClassRec.core_class.actions;
  nactions = xmToggleButtonClassRec.core_class.num_actions;
  print_actions("ToggleButton", actions, nactions);

  actions = xmTearOffButtonClassRec.core_class.actions;
  nactions = xmTearOffButtonClassRec.core_class.num_actions;
  print_actions("TearOffButton", actions, nactions);

  /* MANAGERS */
  actions = xmManagerClassRec.core_class.actions;
  nactions = xmManagerClassRec.core_class.num_actions;
  print_actions("Manager", actions, nactions);

  actions = xmBulletinBoardClassRec.core_class.actions;
  nactions = xmBulletinBoardClassRec.core_class.num_actions;
  print_actions("BulletinBoard", actions, nactions);

  actions = xmCommandClassRec.core_class.actions;
  nactions = xmCommandClassRec.core_class.num_actions;
  print_actions("Command", actions, nactions);

  actions = xmDrawingAreaClassRec.core_class.actions;
  nactions = xmDrawingAreaClassRec.core_class.num_actions;
  print_actions("DrawingArea", actions, nactions);

  actions = xmFileSelectionBoxClassRec.core_class.actions;
  nactions = xmFileSelectionBoxClassRec.core_class.num_actions;
  print_actions("FileSelectionBox", actions, nactions);

  actions = xmFormClassRec.core_class.actions;
  nactions = xmFormClassRec.core_class.num_actions;
  print_actions("Form", actions, nactions);

  actions = xmFrameClassRec.core_class.actions;
  nactions = xmFrameClassRec.core_class.num_actions;
  print_actions("Frame", actions, nactions);

  actions = xmMainWindowClassRec.core_class.actions;
  nactions = xmMainWindowClassRec.core_class.num_actions;
  print_actions("MainWindow", actions, nactions);

  actions = xmMessageBoxClassRec.core_class.actions;
  nactions = xmMessageBoxClassRec.core_class.num_actions;
  print_actions("MessageBox", actions, nactions);

  actions = xmPanedWindowClassRec.core_class.actions;
  nactions = xmPanedWindowClassRec.core_class.num_actions;
  print_actions("PanedWindow", actions, nactions);

  actions = xmRowColumnClassRec.core_class.actions;
  nactions = xmRowColumnClassRec.core_class.num_actions;
  print_actions("RowColumn", actions, nactions);

  actions = xmScaleClassRec.core_class.actions;
  nactions = xmScaleClassRec.core_class.num_actions;
  print_actions("Scale", actions, nactions);

  actions = xmScrolledWindowClassRec.core_class.actions;
  nactions = xmScrolledWindowClassRec.core_class.num_actions;
  print_actions("ScrolledWindow", actions, nactions);

  actions = xmSelectionBoxClassRec.core_class.actions;
  nactions = xmSelectionBoxClassRec.core_class.num_actions;
  print_actions("SelectionBox", actions, nactions);

  exit(0);
}
