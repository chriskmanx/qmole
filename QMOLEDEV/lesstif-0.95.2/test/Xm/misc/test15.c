/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm/misc/test15.c,v 1.4 2002/04/17 16:22:00 amai Exp $
 * Produce a C source file that contains the Motif private error
 * messages.
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

extern char _XmMsgBaseClass_0000[];
extern char _XmMsgBaseClass_0001[];
extern char _XmMsgBaseClass_0002[];
extern char _XmMsgBulletinB_0001[];
extern char _XmMsgCascadeB_0000[];
extern char _XmMsgCascadeB_0001[];
extern char _XmMsgCascadeB_0002[];
extern char _XmMsgCascadeB_0003[];
extern char _XmMsgCommand_0000[];
extern char _XmMsgCommand_0001[];
extern char _XmMsgCommand_0002[];
extern char _XmMsgCommand_0003[];
extern char _XmMsgCommand_0004[];
extern char _XmMsgCommand_0005[];
extern char _XmMsgCutPaste_0000[];
extern char _XmMsgCutPaste_0001[];
extern char _XmMsgCutPaste_0002[];
extern char _XmMsgCutPaste_0003[];
extern char _XmMsgCutPaste_0004[];
extern char _XmMsgCutPaste_0005[];
extern char _XmMsgCutPaste_0006[];
extern char _XmMsgCutPaste_0007[];
extern char _XmMsgCutPaste_0008[];
extern char _XmMsgCutPaste_0009[];
extern char _XmMsgDialogS_0000[];
extern char _XmMsgDisplay_0001[];
extern char _XmMsgDisplay_0002[];
extern char _XmMsgDisplay_0003[];
extern char _XmMsgDragBS_0000[];
extern char _XmMsgDragBS_0001[];
extern char _XmMsgDragBS_0002[];
extern char _XmMsgDragBS_0003[];
extern char _XmMsgDragBS_0004[];
extern char _XmMsgDragBS_0005[];
extern char _XmMsgDragBS_0006[];
extern char _XmMsgDragC_0001[];
extern char _XmMsgDragC_0002[];
extern char _XmMsgDragC_0003[];
extern char _XmMsgDragC_0004[];
extern char _XmMsgDragC_0005[];
extern char _XmMsgDragC_0006[];
extern char _XmMsgDragICC_0000[];
extern char _XmMsgDragICC_0001[];
extern char _XmMsgDragIcon_0000[];
extern char _XmMsgDragIcon_0001[];
extern char _XmMsgDragIcon_0002[];
extern char _XmMsgDragOverS_0000[];
extern char _XmMsgDragOverS_0001[];
extern char _XmMsgDragOverS_0002[];
extern char _XmMsgDragOverS_0003[];
extern char _XmMsgDragUnder_0000[];
extern char _XmMsgDragUnder_0001[];
extern char _XmMsgDropSMgrI_0001[];
extern char _XmMsgDropSMgrI_0002[];
extern char _XmMsgDropSMgrI_0003[];
extern char _XmMsgDropSMgr_0001[];
extern char _XmMsgDropSMgr_0002[];
extern char _XmMsgDropSMgr_0003[];
extern char _XmMsgDropSMgr_0004[];
extern char _XmMsgDropSMgr_0005[];
extern char _XmMsgDropSMgr_0006[];
extern char _XmMsgDropSMgr_0007[];
extern char _XmMsgDropSMgr_0008[];
extern char _XmMsgDropSMgr_0009[];
extern char _XmMsgForm_0000[];
extern char _XmMsgForm_0002[];
extern char _XmMsgForm_0003[];
extern char _XmMsgForm_0004[];
extern char _XmMsgGeoUtils_0000[];
extern char _XmMsgGetSecRes_0000[];
extern char _XmMsgLabel_0003[];
extern char _XmMsgLabel_0004[];
extern char _XmMsgList_0000[];
extern char _XmMsgList_0005[];
extern char _XmMsgList_0006[];
extern char _XmMsgList_0007[];
extern char _XmMsgList_0008[];
extern char _XmMsgList_0009[];
extern char _XmMsgList_0010[];
extern char _XmMsgList_0011[];
extern char _XmMsgList_0012[];
extern char _XmMsgList_0013[];
extern char _XmMsgMainW_0000[];
extern char _XmMsgMainW_0001[];
extern char _XmMsgManager_0000[];
extern char _XmMsgMenuShell_0000[];
extern char _XmMsgMenuShell_0001[];
extern char _XmMsgMessageB_0003[];
extern char _XmMsgMessageB_0004[];
extern char _XmMsgNavigMap_0000[];
extern char _XmMsgPanedW_0000[];
extern char _XmMsgPanedW_0001[];
extern char _XmMsgPanedW_0002[];
extern char _XmMsgPanedW_0003[];
extern char _XmMsgPanedW_0004[];
extern char _XmMsgPanedW_0005[];
extern char _XmMsgProtocols_0000[];
extern char _XmMsgProtocols_0001[];
extern char _XmMsgProtocols_0002[];
extern char _XmMsgRegion_0000[];
extern char _XmMsgRepType_0001[];
extern char _XmMsgRepType_0002[];
extern char _XmMsgResConvert_0000[];
extern char _XmMsgResConvert_0001[];
extern char _XmMsgRowColText_0024[];
extern char _XmMsgRowColumn_0000[];
extern char _XmMsgRowColumn_0001[];
extern char _XmMsgRowColumn_0002[];
extern char _XmMsgRowColumn_0003[];
extern char _XmMsgRowColumn_0004[];
extern char _XmMsgRowColumn_0005[];
extern char _XmMsgRowColumn_0007[];
extern char _XmMsgRowColumn_0008[];
extern char _XmMsgRowColumn_0015[];
extern char _XmMsgRowColumn_0016[];
extern char _XmMsgRowColumn_0017[];
extern char _XmMsgRowColumn_0018[];
extern char _XmMsgRowColumn_0019[];
extern char _XmMsgRowColumn_0020[];
extern char _XmMsgRowColumn_0022[];
extern char _XmMsgRowColumn_0023[];
extern char _XmMsgRowColumn_0025[];
extern char _XmMsgRowColumn_0026[];
extern char _XmMsgRowColumn_0027[];
extern char _XmMsgScaleScrBar_0004[];
extern char _XmMsgScale_0000[];
extern char _XmMsgScale_0001[];
extern char _XmMsgScale_0002[];
extern char _XmMsgScale_0005[];
extern char _XmMsgScale_0006[];
extern char _XmMsgScale_0007[];
extern char _XmMsgScale_0008[];
extern char _XmMsgScreen_0000[];
extern char _XmMsgScreen_0001[];
extern char _XmMsgScrollBar_0000[];
extern char _XmMsgScrollBar_0001[];
extern char _XmMsgScrollBar_0002[];
extern char _XmMsgScrollBar_0003[];
extern char _XmMsgScrollBar_0004[];
extern char _XmMsgScrollBar_0005[];
extern char _XmMsgScrollBar_0006[];
extern char _XmMsgScrollBar_0007[];
extern char _XmMsgScrollBar_0008[];
extern char _XmMsgScrollVis_0000[];
extern char _XmMsgScrolledW_0004[];
extern char _XmMsgScrolledW_0005[];
extern char _XmMsgScrolledW_0006[];
extern char _XmMsgScrolledW_0007[];
extern char _XmMsgScrolledW_0008[];
extern char _XmMsgScrolledW_0009[];
extern char _XmMsgSelectioB_0001[];
extern char _XmMsgSelectioB_0002[];
extern char _XmMsgTextFWcs_0000[];
extern char _XmMsgTextFWcs_0001[];
extern char _XmMsgTextF_0000[];
extern char _XmMsgTextF_0001[];
extern char _XmMsgTextF_0002[];
extern char _XmMsgTextF_0003[];
extern char _XmMsgTextF_0004[];
extern char _XmMsgTextF_0005[];
extern char _XmMsgTextF_0006[];
extern char _XmMsgTextIn_0000[];
extern char _XmMsgTextOut_0000[];
extern char _XmMsgText_0000[];
extern char _XmMsgText_0002[];
extern char _XmMsgVendorE_0000[];
extern char _XmMsgVendorE_0005[];
extern char _XmMsgVendor_0000[];
extern char _XmMsgVisual_0000[];
extern char _XmMsgVisual_0001[];
extern char _XmMsgVisual_0002[];
extern char _XmMsgXmIm_0000[];

struct {
	char	*n;
	char	*t;
} list[] = {
	{ "_XmMsgBaseClass_0000", _XmMsgBaseClass_0000 },
	{ "_XmMsgBaseClass_0001", _XmMsgBaseClass_0001 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgBaseClass_0002", _XmMsgBaseClass_0002 },
#endif
	{ "_XmMsgBulletinB_0001", _XmMsgBulletinB_0001 },
	{ "_XmMsgCascadeB_0000", _XmMsgCascadeB_0000 },
	{ "_XmMsgCascadeB_0001", _XmMsgCascadeB_0001 },
	{ "_XmMsgCascadeB_0002", _XmMsgCascadeB_0002 },
	{ "_XmMsgCascadeB_0003", _XmMsgCascadeB_0003 },
	{ "_XmMsgCommand_0000", _XmMsgCommand_0000 },
	{ "_XmMsgCommand_0001", _XmMsgCommand_0001 },
	{ "_XmMsgCommand_0002", _XmMsgCommand_0002 },
	{ "_XmMsgCommand_0003", _XmMsgCommand_0003 },
	{ "_XmMsgCommand_0004", _XmMsgCommand_0004 },
	{ "_XmMsgCommand_0005", _XmMsgCommand_0005 },
	{ "_XmMsgCutPaste_0000", _XmMsgCutPaste_0000 },
	{ "_XmMsgCutPaste_0001", _XmMsgCutPaste_0001 },
	{ "_XmMsgCutPaste_0002", _XmMsgCutPaste_0002 },
	{ "_XmMsgCutPaste_0003", _XmMsgCutPaste_0003 },
	{ "_XmMsgCutPaste_0004", _XmMsgCutPaste_0004 },
	{ "_XmMsgCutPaste_0005", _XmMsgCutPaste_0005 },
	{ "_XmMsgCutPaste_0006", _XmMsgCutPaste_0006 },
	{ "_XmMsgCutPaste_0007", _XmMsgCutPaste_0007 },
	{ "_XmMsgCutPaste_0008", _XmMsgCutPaste_0008 },
	{ "_XmMsgCutPaste_0009", _XmMsgCutPaste_0009 },
	{ "_XmMsgDialogS_0000", _XmMsgDialogS_0000 },
	{ "_XmMsgDisplay_0001", _XmMsgDisplay_0001 },
	{ "_XmMsgDisplay_0002", _XmMsgDisplay_0002 },
	{ "_XmMsgDisplay_0003", _XmMsgDisplay_0003 },
	{ "_XmMsgDragBS_0000", _XmMsgDragBS_0000 },
	{ "_XmMsgDragBS_0001", _XmMsgDragBS_0001 },
	{ "_XmMsgDragBS_0002", _XmMsgDragBS_0002 },
	{ "_XmMsgDragBS_0003", _XmMsgDragBS_0003 },
	{ "_XmMsgDragBS_0004", _XmMsgDragBS_0004 },
	{ "_XmMsgDragBS_0005", _XmMsgDragBS_0005 },
	{ "_XmMsgDragBS_0006", _XmMsgDragBS_0006 },
	{ "_XmMsgDragC_0001", _XmMsgDragC_0001 },
	{ "_XmMsgDragC_0002", _XmMsgDragC_0002 },
	{ "_XmMsgDragC_0003", _XmMsgDragC_0003 },
	{ "_XmMsgDragC_0004", _XmMsgDragC_0004 },
	{ "_XmMsgDragC_0005", _XmMsgDragC_0005 },
	{ "_XmMsgDragC_0006", _XmMsgDragC_0006 },
	{ "_XmMsgDragICC_0000", _XmMsgDragICC_0000 },
	{ "_XmMsgDragICC_0001", _XmMsgDragICC_0001 },
	{ "_XmMsgDragIcon_0000", _XmMsgDragIcon_0000 },
	{ "_XmMsgDragIcon_0001", _XmMsgDragIcon_0001 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgDragIcon_0002", _XmMsgDragIcon_0002 },
#endif
	{ "_XmMsgDragOverS_0000", _XmMsgDragOverS_0000 },
	{ "_XmMsgDragOverS_0001", _XmMsgDragOverS_0001 },
	{ "_XmMsgDragOverS_0002", _XmMsgDragOverS_0002 },
	{ "_XmMsgDragOverS_0003", _XmMsgDragOverS_0003 },
	{ "_XmMsgDragUnder_0000", _XmMsgDragUnder_0000 },
	{ "_XmMsgDragUnder_0001", _XmMsgDragUnder_0001 },
	{ "_XmMsgDropSMgrI_0001", _XmMsgDropSMgrI_0001 },
	{ "_XmMsgDropSMgrI_0002", _XmMsgDropSMgrI_0002 },
	{ "_XmMsgDropSMgrI_0003", _XmMsgDropSMgrI_0003 },
	{ "_XmMsgDropSMgr_0001", _XmMsgDropSMgr_0001 },
	{ "_XmMsgDropSMgr_0002", _XmMsgDropSMgr_0002 },
	{ "_XmMsgDropSMgr_0003", _XmMsgDropSMgr_0003 },
	{ "_XmMsgDropSMgr_0004", _XmMsgDropSMgr_0004 },
	{ "_XmMsgDropSMgr_0005", _XmMsgDropSMgr_0005 },
	{ "_XmMsgDropSMgr_0006", _XmMsgDropSMgr_0006 },
	{ "_XmMsgDropSMgr_0007", _XmMsgDropSMgr_0007 },
	{ "_XmMsgDropSMgr_0008", _XmMsgDropSMgr_0008 },
	{ "_XmMsgDropSMgr_0009", _XmMsgDropSMgr_0009 },
	{ "_XmMsgForm_0000", _XmMsgForm_0000 },
	{ "_XmMsgForm_0002", _XmMsgForm_0002 },
	{ "_XmMsgForm_0003", _XmMsgForm_0003 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgForm_0004", _XmMsgForm_0004 },
	{ "_XmMsgGeoUtils_0000", _XmMsgGeoUtils_0000 },
	{ "_XmMsgGetSecRes_0000", _XmMsgGetSecRes_0000 },
#endif
	{ "_XmMsgLabel_0003", _XmMsgLabel_0003 },
	{ "_XmMsgLabel_0004", _XmMsgLabel_0004 },
	{ "_XmMsgList_0000", _XmMsgList_0000 },
	{ "_XmMsgList_0005", _XmMsgList_0005 },
	{ "_XmMsgList_0006", _XmMsgList_0006 },
	{ "_XmMsgList_0007", _XmMsgList_0007 },
	{ "_XmMsgList_0008", _XmMsgList_0008 },
	{ "_XmMsgList_0009", _XmMsgList_0009 },
	{ "_XmMsgList_0010", _XmMsgList_0010 },
	{ "_XmMsgList_0011", _XmMsgList_0011 },
	{ "_XmMsgList_0012", _XmMsgList_0012 },
	{ "_XmMsgList_0013", _XmMsgList_0013 },
	{ "_XmMsgMainW_0000", _XmMsgMainW_0000 },
	{ "_XmMsgMainW_0001", _XmMsgMainW_0001 },
	{ "_XmMsgManager_0000", _XmMsgManager_0000 },
	{ "_XmMsgMenuShell_0000", _XmMsgMenuShell_0000 },
	{ "_XmMsgMenuShell_0001", _XmMsgMenuShell_0001 },
	{ "_XmMsgMessageB_0003", _XmMsgMessageB_0003 },
	{ "_XmMsgMessageB_0004", _XmMsgMessageB_0004 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgNavigMap_0000", _XmMsgNavigMap_0000 },
#endif
	{ "_XmMsgPanedW_0000", _XmMsgPanedW_0000 },
	{ "_XmMsgPanedW_0001", _XmMsgPanedW_0001 },
	{ "_XmMsgPanedW_0002", _XmMsgPanedW_0002 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgPanedW_0003", _XmMsgPanedW_0003 },
#endif
	{ "_XmMsgPanedW_0004", _XmMsgPanedW_0004 },
	{ "_XmMsgPanedW_0005", _XmMsgPanedW_0005 },
	{ "_XmMsgProtocols_0000", _XmMsgProtocols_0000 },
	{ "_XmMsgProtocols_0001", _XmMsgProtocols_0001 },
	{ "_XmMsgProtocols_0002", _XmMsgProtocols_0002 },
	{ "_XmMsgRegion_0000", _XmMsgRegion_0000 },
	{ "_XmMsgRepType_0001", _XmMsgRepType_0001 },
	{ "_XmMsgRepType_0002", _XmMsgRepType_0002 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgResConvert_0000", _XmMsgResConvert_0000 },
#endif
	{ "_XmMsgResConvert_0001", _XmMsgResConvert_0001 },
	{ "_XmMsgRowColText_0024", _XmMsgRowColText_0024 },
	{ "_XmMsgRowColumn_0000", _XmMsgRowColumn_0000 },
	{ "_XmMsgRowColumn_0001", _XmMsgRowColumn_0001 },
	{ "_XmMsgRowColumn_0002", _XmMsgRowColumn_0002 },
	{ "_XmMsgRowColumn_0003", _XmMsgRowColumn_0003 },
	{ "_XmMsgRowColumn_0004", _XmMsgRowColumn_0004 },
	{ "_XmMsgRowColumn_0005", _XmMsgRowColumn_0005 },
	{ "_XmMsgRowColumn_0007", _XmMsgRowColumn_0007 },
	{ "_XmMsgRowColumn_0008", _XmMsgRowColumn_0008 },
	{ "_XmMsgRowColumn_0015", _XmMsgRowColumn_0015 },
	{ "_XmMsgRowColumn_0016", _XmMsgRowColumn_0016 },
	{ "_XmMsgRowColumn_0017", _XmMsgRowColumn_0017 },
	{ "_XmMsgRowColumn_0018", _XmMsgRowColumn_0018 },
	{ "_XmMsgRowColumn_0019", _XmMsgRowColumn_0019 },
	{ "_XmMsgRowColumn_0020", _XmMsgRowColumn_0020 },
	{ "_XmMsgRowColumn_0022", _XmMsgRowColumn_0022 },
	{ "_XmMsgRowColumn_0023", _XmMsgRowColumn_0023 },
	{ "_XmMsgRowColumn_0025", _XmMsgRowColumn_0025 },
	{ "_XmMsgRowColumn_0026", _XmMsgRowColumn_0026 },
	{ "_XmMsgRowColumn_0027", _XmMsgRowColumn_0027 },
	{ "_XmMsgScaleScrBar_0004", _XmMsgScaleScrBar_0004 },
	{ "_XmMsgScale_0000", _XmMsgScale_0000 },
	{ "_XmMsgScale_0001", _XmMsgScale_0001 },
	{ "_XmMsgScale_0002", _XmMsgScale_0002 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgScale_0005", _XmMsgScale_0005 },
#endif
	{ "_XmMsgScale_0006", _XmMsgScale_0006 },
	{ "_XmMsgScale_0007", _XmMsgScale_0007 },
	{ "_XmMsgScale_0008", _XmMsgScale_0008 },
	{ "_XmMsgScreen_0000", _XmMsgScreen_0000 },
	{ "_XmMsgScreen_0001", _XmMsgScreen_0001 },
	{ "_XmMsgScrollBar_0000", _XmMsgScrollBar_0000 },
	{ "_XmMsgScrollBar_0001", _XmMsgScrollBar_0001 },
	{ "_XmMsgScrollBar_0002", _XmMsgScrollBar_0002 },
	{ "_XmMsgScrollBar_0003", _XmMsgScrollBar_0003 },
	{ "_XmMsgScrollBar_0004", _XmMsgScrollBar_0004 },
	{ "_XmMsgScrollBar_0005", _XmMsgScrollBar_0005 },
	{ "_XmMsgScrollBar_0006", _XmMsgScrollBar_0006 },
	{ "_XmMsgScrollBar_0007", _XmMsgScrollBar_0007 },
	{ "_XmMsgScrollBar_0008", _XmMsgScrollBar_0008 },
	{ "_XmMsgScrollVis_0000", _XmMsgScrollVis_0000 },
	{ "_XmMsgScrolledW_0004", _XmMsgScrolledW_0004 },
	{ "_XmMsgScrolledW_0005", _XmMsgScrolledW_0005 },
	{ "_XmMsgScrolledW_0006", _XmMsgScrolledW_0006 },
	{ "_XmMsgScrolledW_0007", _XmMsgScrolledW_0007 },
	{ "_XmMsgScrolledW_0008", _XmMsgScrolledW_0008 },
	{ "_XmMsgScrolledW_0009", _XmMsgScrolledW_0009 },
	{ "_XmMsgSelectioB_0001", _XmMsgSelectioB_0001 },
	{ "_XmMsgSelectioB_0002", _XmMsgSelectioB_0002 },
	{ "_XmMsgTextFWcs_0000", _XmMsgTextFWcs_0000 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgTextFWcs_0001", _XmMsgTextFWcs_0001 },
#endif
	{ "_XmMsgTextF_0000", _XmMsgTextF_0000 },
	{ "_XmMsgTextF_0001", _XmMsgTextF_0001 },
	{ "_XmMsgTextF_0002", _XmMsgTextF_0002 },
	{ "_XmMsgTextF_0003", _XmMsgTextF_0003 },
	{ "_XmMsgTextF_0004", _XmMsgTextF_0004 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgTextF_0005", _XmMsgTextF_0005 },
#endif
	{ "_XmMsgTextF_0006", _XmMsgTextF_0006 },
	{ "_XmMsgTextIn_0000", _XmMsgTextIn_0000 },
	{ "_XmMsgTextOut_0000", _XmMsgTextOut_0000 },
	{ "_XmMsgText_0000", _XmMsgText_0000 },
#ifdef LESSTIF_VERSION
	{ "_XmMsgText_0002", _XmMsgText_0002 },
	{ "_XmMsgVendorE_0000", _XmMsgVendorE_0000 },
	{ "_XmMsgVendorE_0005", _XmMsgVendorE_0005 },
#endif
	{ "_XmMsgVendor_0000", _XmMsgVendor_0000 },
	{ "_XmMsgVisual_0000", _XmMsgVisual_0000 },
	{ "_XmMsgVisual_0001", _XmMsgVisual_0001 },
	{ "_XmMsgVisual_0002", _XmMsgVisual_0002 },
	{ "_XmMsgXmIm_0000", _XmMsgXmIm_0000 },
	{ NULL, NULL }
};

int main(int argc, char *argv[])
{
	int	i;
	char	*p;

	printf("/* $Id: test15.c,v 1.4 2002/04/17 16:22:00 amai Exp $ */\n");
	printf("/* This is the output of LESSTIF/test/Xm/test15.c */\n");
	printf("\n");
	printf("\n");
	for (i=0; list[i].n; i++) {
		printf("extern _XmConst char\t%s[];\n", list[i].n);
	}
	return 0;
}
