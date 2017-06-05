/* $Header: /cvsroot/lesstif/lesstif/test/Xm/cascadebuttong/test7.c,v 1.3 2001/05/23 15:00:02 amai Exp $ */
#if 0
To:          Rick Scott <rwscott@omnisig.com>, Jamie Gritton <jamie@gritton.org>
From:        Edouard Parmelan <Edouard.Parmelan@quadratec.fr>
Subject:     Re: RowColumn, PulldownMenu and OptionMenu.
Cc:          LessTif <lesstif@hungry.com>
Date:        Thu, 29 Apr 1999 17:02:49 +0200
#endif

#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeB.h>
#include <Xm/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/RepType.h>
#include <Xm/SelectioB.h>
#include <Xm/SeparatoG.h>

#include <Xm/XmP.h>

#include "../../common/Test.h"

/* liste des char set */
char helvMRm[] = "helvMRm";         /* helvetica medium roman  medium */
static char defhelvMRm[]  = "-adobe-helvetica-medium-r-normal--*-140-*-*-*-*-*-1";

/* structure de donnees Font */
typedef struct {
  XFontStruct   *helvMRm;         /* helvetica medium roman  medium */
} AppDataTxt, *pAppDataTxt;

/* structure des ressources */
static XtResource ressources[] = {
  { helvMRm, "MessFont", XtRFontStruct, sizeof(XFontStruct*),
    XtOffset(pAppDataTxt, helvMRm), XtRString, defhelvMRm },
};

static	AppDataTxt	sres;       /* ressources X de l'application */

#define LABEL_MARGIN_WIDTH	8
XmFontList fontList;


void ZXcreateFM1(Widget top)
{
    Arg targ[16];
    Widget tw[8];

    Widget top_fm1, fm1_mb1, mb1_pdm42, pdm42_pdm43, pdm42_pdm50, pdm42_pdm3,
	mb1_cbg3,
	pdm42_cbg1,
	pdm42_cbg2,
	pdm42_spg1,
	pdm42_cbg3,
	pdm42_pbg1;
    XmString label, mnemo;

    /* TOP_FM1 */
    top_fm1 = XmCreateForm(top, "top_fm1", targ, 0);
    XtManageChildren(&top_fm1, 1);

    /* FM1_MB1 */
    XtSetArg(targ[0], XmNtopAttachment, XmATTACH_FORM);
    XtSetArg(targ[1], XmNbottomAttachment, XmATTACH_NONE);
    XtSetArg(targ[2], XmNrightAttachment, XmATTACH_FORM);
    XtSetArg(targ[3], XmNleftAttachment, XmATTACH_FORM);
    fm1_mb1 = XmCreateMenuBar(top_fm1, "fm1_mb1", targ, 4);
    XtManageChildren(&fm1_mb1, 1);

    XtSetArg(targ[0], XmNspacing, 0);
    XtSetArg(targ[1], XmNmarginHeight, 0);
    XtSetArg(targ[2], XmNmarginWidth, 0);

    /* MB1_PDM42 */
    mb1_pdm42 = XmCreatePulldownMenu(fm1_mb1, "mb1_pdm42", targ, 3);

    /* PDM42_PDM43 */
    pdm42_pdm43 = XmCreatePulldownMenu(mb1_pdm42, "pdm42_pdm43", targ, 3);

    /* PDM42_PDM50 */
    pdm42_pdm50 = XmCreatePulldownMenu(mb1_pdm42, "pdm42_pdm50", targ, 3);

    /* PDM42_PDM3 */
    pdm42_pdm3 = XmCreatePulldownMenu(mb1_pdm42, "pdm42_pdm3", targ, 3);


    /* sous Motif 1.0 et 1.1 le Charset empeche la materialisation du _ 	*/
    /* seul XmSTRING_DEFAULT_CHARSET				*/
    XtSetArg(targ[0], XmNmarginWidth, LABEL_MARGIN_WIDTH);
    XtSetArg(targ[1], XmNfontList, fontList);

    /* MB1_CBG3 */
    label = XmStringCreateSimple("Platform");
    mnemo = XmStringCreateSimple("P");
    XtSetArg(targ[2], XmNsubMenuId, mb1_pdm42);
    XtSetArg(targ[3], XmNlabelString, label);
    XtSetArg(targ[4], XmNmnemonic, mnemo);
    XtSetArg(targ[5], XmNmnemonicCharSet, helvMRm);
    mb1_cbg3 = XmCreateCascadeButtonGadget(fm1_mb1, "mb1_cbg3", targ, 6);
    XtManageChild(mb1_cbg3);


    /* PDM42_CBG1 */
    label = XmStringCreateSimple("Host");
    mnemo = XmStringCreateSimple("H");
    XtSetArg(targ[0], XmNmarginWidth, LABEL_MARGIN_WIDTH);
    XtSetArg(targ[1], XmNfontList, fontList);
    XtSetArg(targ[2], XmNlabelString, label);
    XtSetArg(targ[3], XmNmnemonic, mnemo);
    XtSetArg(targ[4], XmNmnemonicCharSet, helvMRm);
    XtSetArg(targ[5], XmNsubMenuId, pdm42_pdm43);
    pdm42_cbg1 = XmCreateCascadeButtonGadget(mb1_pdm42, "pdm42_cbg1", targ, 6);

    /* PDM42_CBG2 */
    label = XmStringCreateSimple("Application");
    mnemo = XmStringCreateSimple("A");
    XtSetArg(targ[0], XmNmarginWidth, LABEL_MARGIN_WIDTH);
    XtSetArg(targ[1], XmNfontList, fontList);
    XtSetArg(targ[2], XmNlabelString, label);
    XtSetArg(targ[3], XmNmnemonic, mnemo);
    XtSetArg(targ[4], XmNmnemonicCharSet, helvMRm);
    XtSetArg(targ[5], XmNsubMenuId, pdm42_pdm50);
    pdm42_cbg2 = XmCreateCascadeButtonGadget(mb1_pdm42, "pdm42_cbg2", targ, 6);

    /* PDM42_SPG1 */
    XtSetArg(targ[0], XmNhighlightThickness, 0);
    pdm42_spg1 = XmCreateSeparatorGadget(mb1_pdm42, "pdm42_spg1", targ, 1);

    /* PDM42_CBG3 */
    label = XmStringCreateSimple("Use");
    mnemo = XmStringCreateSimple("U");
    XtSetArg(targ[0], XmNmarginWidth, LABEL_MARGIN_WIDTH);
    XtSetArg(targ[1], XmNfontList, fontList);
    XtSetArg(targ[2], XmNlabelString, label);
    XtSetArg(targ[3], XmNmnemonic, mnemo);
    XtSetArg(targ[4], XmNmnemonicCharSet, helvMRm);
    XtSetArg(targ[5], XmNsubMenuId, pdm42_pdm3);
    pdm42_cbg3 = XmCreateCascadeButtonGadget(mb1_pdm42, "pdm42_cbg3", targ, 6);

    /* PDM42_PBG1 */
    label = XmStringCreateSimple("tina");
    mnemo = XmStringCreateSimple("t");
    XtSetArg(targ[2], XmNlabelString, label);
    XtSetArg(targ[3], XmNmnemonic, mnemo);
    XtSetArg(targ[4], XmNmnemonicCharSet, helvMRm);
    pdm42_pbg1 = XmCreatePushButtonGadget(mb1_pdm42, "pdm42_pdg1", targ, 5);

    tw[0] = pdm42_cbg1;
    tw[1] = pdm42_spg1;
    tw[2] = pdm42_cbg2;
    tw[3] = pdm42_cbg3;
    tw[4] = pdm42_pbg1;
    XtManageChildren(tw, 5);
    
}

int main(int argc, char **argv)
{
    XtAppContext theApp;
    Widget	toplevel;

    toplevel = XtVaAppInitialize(&theApp, "test7", NULL, 0, &argc, argv, NULL, NULL);

    XtGetApplicationResources(toplevel, (XtPointer)&sres,
	    ressources, XtNumber(ressources),
	    NULL, 0);

    if ((argc == 2) && (strcmp(argv[1], "-no-font-list") == 0)) {
	fontList = NULL;
    }
    else {
	fontList = XmFontListCreate(sres.helvMRm, helvMRm);
    }

    ZXcreateFM1(toplevel);
    XtRealizeWidget(toplevel);
    

/* Note: the following values are the result of
 * querying the current geometry.
 */
{
static XtWidgetGeometry Expected[] = {
   CWWidth | CWHeight            ,    0,    0,   81,   34, 0,0,0, /* top_fm1 */
   CWWidth | CWHeight | CWX | CWY,    0,    0,   81,   34, 0,0,0, /* fm1_mb1 */
   CWWidth | CWHeight | CWX | CWY,    5,    5,   71,   24, 0,0,0, /* mb1_cbg3 */
};
/* toplevel should be replaced with to correct applicationShell */
PrintDetails(toplevel, Expected);
}
LessTifTestMainLoop(toplevel);
    exit(0);
}
