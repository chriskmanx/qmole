/*
 * wctest.c -- Tests various aspects of the BaseClass stuff.
 *
 * Written 97/01/03 by Harald Albrecht, albrecht@igpm.rwth-aachen.de
 */
#include <Xm/Xm.h>
#include <Xm/XmP.h>
#include <X11/ShellP.h>
#include <Xm/ArrowBGP.h>
#include <Xm/ArrowBP.h>
#include <Xm/BaseClassP.h>
#include <Xm/BulletinBP.h>
#include <Xm/CacheP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/CascadeBP.h>
#include <Xm/CommandP.h>
#if XmVERSION<2
#include <Xm/CutPasteP.h>
#endif
#include <Xm/DesktopP.h>
#include <Xm/DialogSEP.h>
#include <Xm/DialogSP.h>
#include <Xm/DisplayP.h>
#include <Xm/DragCP.h>
#include <Xm/DragIconP.h>
#include <Xm/DragOverSP.h>
#include <Xm/DrawP.h>
#include <Xm/DrawingAP.h>
#include <Xm/DrawnBP.h>
#include <Xm/DropSMgrP.h>
#include <Xm/DropTransP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/FileSBP.h>
#include <Xm/FormP.h>
#include <Xm/FrameP.h>
#include <Xm/GadgetP.h>
#include <Xm/LabelGP.h>
#include <Xm/LabelP.h>
#include <Xm/ListP.h>
#include <Xm/MainWP.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/MessageBP.h>
#include <Xm/PanedWP.h>
#include <Xm/PrimitiveP.h>
#include <Xm/ProtocolsP.h>
#include <Xm/PushBGP.h>
#include <Xm/PushBP.h>
#if XmVERSION<2
#include <Xm/RCUtilsP.h>
#endif
#include <Xm/RowColumnP.h>
#include <Xm/SashP.h>
#include <Xm/ScaleP.h>
#include <Xm/ScreenP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/SelectioBP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/SeparatorP.h>
#include <Xm/ShellEP.h>
#include <Xm/TearOffBP.h>
#include <Xm/TearOffP.h>
#include <Xm/TextFP.h>
#include <Xm/TextFSelP.h>
#include <Xm/TextInP.h>
#include <Xm/TextOutP.h>
#include <Xm/TextP.h>
#include <Xm/TextSelP.h>
#include <Xm/TextStrSoP.h>
#include <Xm/ToggleBGP.h>
#include <Xm/ToggleBP.h>
#include <Xm/TransltnsP.h>
#include <Xm/VaSimpleP.h>
#include <Xm/VendorSEP.h>
#include <Xm/VendorSP.h>
#include <Xm/VirtKeysP.h>
#if XmVERSION<2
#include <Xm/WorldP.h>
#endif
#include <Xm/XmosP.h>

#include <stdio.h>
#include <string.h>

Display      *AppDsp;
XContext      VisitorCTX = (XContext) 0;

/*
 * Checks whether a widget class has already been visited.
 */
Boolean ClassHasBeenVisited(WidgetClass wc)
{
    void *Dummy;

    if ( VisitorCTX == (XContext) 0 ) {
        VisitorCTX = XUniqueContext();
    }
    if ( XFindContext(AppDsp, (XID) wc, VisitorCTX,
		      (XPointer*) &Dummy) == 0 ) {
        return True;
    }
    XSaveContext(AppDsp, (XID) wc, VisitorCTX,
		 (XPointer) NULL);
    return False;
} /* ClassHasBeenVisited */


void HasMethod(void *MethodPointer, String HookName, 
	       Boolean InheritedOverwrite)
{
    if ( MethodPointer ) {
        printf("        %s%s\n",
	       MethodPointer == (void *) _XtInherit ? "INHERIT " :
	         (InheritedOverwrite ? "***INHERITED*** " : ""),
	       HookName);
    }
} /* HasMethod */


XmBaseClassExt GetExt(WidgetClass wc, Boolean Inherited)
{
    XmBaseClassExt ext;

    ext = (XmBaseClassExt) wc->core_class.extension;
    while ( ext && (ext->record_type == Inherited ? XmQmotif : NULLQUARK) &&
	    (ext->record_size != sizeof(XmBaseClassExtRec)) ) {
        ext = ext->next_extension;
    }
    return ext;
} /* GetExt */


Boolean IsSubclass(WidgetClass wc, WidgetClass superwc)
{
    while ( wc && (wc != superwc) ) {
        wc = wc->core_class.superclass;
    }
    return wc == superwc ? True : False;
} /* IsSubclass */


Boolean CanBeInited(WidgetClass wc)
{
    while ( wc ) {
        if (
	    (wc == (WidgetClass) &xmLabelGCacheObjClassRec) ||
	    (wc == (WidgetClass) &xmVendorShellExtClassRec)
	    ) return False;
	wc = wc->core_class.superclass;
    }
    return True;
} /* CanBeInited */

void DumpBCERecord(WidgetClass wc, Boolean Inherited)
{
    XmBaseClassExt  ext;
    char            buff[512];
    char           *p;
    int             i;

    ext = GetExt(wc, Inherited);
    if ( ext == NULL ) return;

    if ( Inherited ) {
      printf("    has a ***INHERITED*** BCE record.\n");
    }
    printf("    hooks:\n");
    HasMethod(ext->initializePrehook, "initializePrehook", Inherited);
    HasMethod(ext->initializePosthook, "initializePosthook", Inherited);
    HasMethod(ext->classPartInitPrehook, "classPartInitPrehook", Inherited);
    HasMethod(ext->classPartInitPosthook, "classPartInitPosthook", Inherited);
    HasMethod(ext->getValuesPrehook, "getValuesPrehook", Inherited);
    HasMethod(ext->getValuesPosthook, "getValuesPosthook", Inherited);
    HasMethod(ext->setValuesPrehook, "setValuesPrehook", Inherited);
    HasMethod(ext->setValuesPosthook, "setValuesPosthook", Inherited);

    printf("    methods:\n");
    HasMethod(ext->getSecResData, "getSecResData", Inherited);
    HasMethod(ext->widgetNavigable, "widgetNavigable", Inherited);
    HasMethod(ext->focusChange, "focusChange", Inherited);
    
    if ( CanBeInited(wc) ) {
        XtInitializeWidgetClass(wc);
    }
    
    for (p = buff, i = 0; i <= 63; i++ ) {
        *p++ = _XmGetFlagsBit(ext->flags, i) ? '1' : '0';
    }
    *p = 0;
    printf("    fast subclass bits 0-63:\n        %s\n", buff);

    if ( ext->ext_resources || ext->num_ext_resources) {
        printf("    has %i external resources\n", 
	       ext->num_ext_resources);
    }
    if ( ext->compiled_ext_resources ) {
        printf("    has compiled external resources\n");
    }

    if ( ext->use_sub_resources ) {
        printf("    uses subresources\n");
    }
} /* DumpBCERecord */


void TraverseDownTo(WidgetClass wc)
{
    XmBaseClassExt ext;

    if ( !ClassHasBeenVisited(wc) ) {
        if ( wc->core_class.superclass ) {
	    TraverseDownTo(wc->core_class.superclass);
	}

	printf("-------------------------------------------------------------------------------\n");
	printf("%s %s:", 
	       IsSubclass(wc, (WidgetClass) &rectObjClassRec) ? 
	         "widgetclass" : "objectclass",
	       wc->core_class.class_name);
	if ( wc->core_class.superclass ) {
	    printf(" (superclass %s)", wc->core_class.superclass->core_class.class_name);
	}
	printf("\n");

	/*
	 * The widget class hasn't been initialized yet, thus the extension
	 * is identified by NULLQUARK and not by XmQmotif. We assume here
	 * that there is no other extension present...
	 */
	ext = GetExt(wc, False);
	if ( ext != NULL ) {
	    DumpBCERecord(wc, False);
	} else if ( CanBeInited(wc) ) {
	    XtInitializeWidgetClass(wc);
	    ext = GetExt(wc, True);
	    if ( ext != NULL ) {
	        DumpBCERecord(wc, True);
	    }
	}
    }
} /* TraverseDownTo */



int main(int argc, char **argv)
{
    XtAppContext AppCtx;

    /*
     * We must open the display the old way. When using XtAppInitialize()
     * a VendorShell derived widget is created as the toplevel widget. And
     * this will initialise some widget classes -- what we don't want at all.
     * At least not yet.
     */
    XtToolkitInitialize();
    AppCtx = XtCreateApplicationContext();
    AppDsp = XtOpenDisplay(AppCtx, NULL, "wctest", "Wctest",
			   NULL, 0, &argc, argv);
    printf("== BEFORE CREATING FIRST VENDORSHELL ==========================================\n");
    TraverseDownTo(coreWidgetClass);
    TraverseDownTo(vendorShellWidgetClass);
    /*
     * Now we can create a shell or we'll crash lateron when the XmMessageBox
     * calls _XmGetDefaultDisplay(). Also we can compare what happens to some
     * widget classes from the Xt Intrinsics -- especially Core.
     */
    XtAppCreateShell("wctest", "WcTest", 
		     applicationShellWidgetClass, 
		     AppDsp, 
		     NULL, 0);
    VisitorCTX = XUniqueContext();
    printf("== AFTER CREATING FIRST VENDORSHELL ===========================================\n");

    TraverseDownTo((WidgetClass) &xmCascadeButtonGCacheObjClassRec);
    TraverseDownTo((WidgetClass) &xmLabelGCacheObjClassRec);
    TraverseDownTo((WidgetClass) &xmPushButtonGCacheObjClassRec);
    TraverseDownTo((WidgetClass) &xmToggleButtonGCacheObjClassRec);
    TraverseDownTo((WidgetClass) &xmSeparatorGCacheObjClassRec);

    TraverseDownTo((WidgetClass) &xmDragContextClassRec);
    TraverseDownTo((WidgetClass) &xmDragIconClassRec);
    TraverseDownTo((WidgetClass) &xmDragOverShellClassRec);
    TraverseDownTo((WidgetClass) &xmDropSiteManagerClassRec);
    TraverseDownTo((WidgetClass) &xmDropTransferClassRec);
    TraverseDownTo((WidgetClass) &xmProtocolClassRec);
    TraverseDownTo((WidgetClass) &xmShellExtClassRec);
    TraverseDownTo((WidgetClass) &xmVendorShellExtClassRec);
#if XmVERSION<2
    TraverseDownTo((WidgetClass) &xmWorldClassRec);
#endif

    TraverseDownTo(xmArrowButtonGadgetClass);
    TraverseDownTo(xmCascadeButtonGadgetClass);
    TraverseDownTo(xmPushButtonGadgetClass);
    TraverseDownTo(xmToggleButtonGadgetClass);

    TraverseDownTo((WidgetClass) &xmScreenClassRec);

    TraverseDownTo(xmArrowButtonWidgetClass);
    TraverseDownTo(xmCascadeButtonWidgetClass);
    TraverseDownTo(xmDrawnButtonWidgetClass);
    TraverseDownTo(xmPushButtonWidgetClass);
    TraverseDownTo(xmToggleButtonWidgetClass);
    TraverseDownTo(xmListWidgetClass);
    TraverseDownTo(xmSashWidgetClass);
    TraverseDownTo(xmScrollBarWidgetClass);
    TraverseDownTo(xmSeparatorWidgetClass);
    TraverseDownTo(xmTextWidgetClass);
    TraverseDownTo(xmTextFieldWidgetClass);

    TraverseDownTo((WidgetClass) &xmTearOffButtonClassRec);

    TraverseDownTo(xmFormWidgetClass);
    TraverseDownTo(xmMessageBoxWidgetClass);
    TraverseDownTo(xmCommandWidgetClass);
    TraverseDownTo(xmFileSelectionBoxWidgetClass);
    TraverseDownTo(xmDrawingAreaWidgetClass);
    TraverseDownTo(xmFrameWidgetClass);
    TraverseDownTo(xmPanedWindowWidgetClass);
    TraverseDownTo(xmRowColumnWidgetClass);
    TraverseDownTo(xmScaleWidgetClass);
    TraverseDownTo(xmMainWindowWidgetClass);

    TraverseDownTo(xmMenuShellWidgetClass);
    TraverseDownTo((WidgetClass) &xmDisplayClassRec);
    /*    TraverseDownTo(applicationShellWidgetClass);*/
    TraverseDownTo(xmDialogShellWidgetClass);

    XtCloseDisplay(AppDsp);
    return 0;
} /* main */

/* End of wctest.c */

