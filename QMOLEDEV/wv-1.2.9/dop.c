/* wvWare
 * Copyright (C) Caolan McNamara, Dom Lachowicz, and others
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include "wv.h"

void
wvGetCOPTS (COPTS * item, wvStream * fd)
{
    U16 temp16;
    temp16 = read_16ubit (fd);

    item->fNoTabForInd = temp16 & 0x0001;
    item->fNoSpaceRaiseLower = (temp16 & 0x0002) >> 1;
    item->fSuppressSpbfAfterPageBreak = (temp16 & 0x0004) >> 2;
    item->fWrapTrailSpaces = (temp16 & 0x0008) >> 3;
    item->fMapPrintTextColor = (temp16 & 0x0010) >> 4;
    item->fNoColumnBalance = (temp16 & 0x0020) >> 5;
    item->fConvMailMergeEsc = (temp16 & 0x0040) >> 6;
    item->fSuppressTopSpacing = (temp16 & 0x0080) >> 7;
    item->fOrigWordTableRules = (temp16 & 0x0100) >> 8;
    item->fTransparentMetafiles = (temp16 & 0x0200) >> 9;
    item->fShowBreaksInFrames = (temp16 & 0x0400) >> 10;
    item->fSwapBordersFacingPgs = (temp16 & 0x800) >> 11;
    item->reserved = (temp16 & 0xf000) >> 12;
}


void
wvInitCOPTS (COPTS * item)
{
    item->fNoTabForInd = 0;
    item->fNoSpaceRaiseLower = 0;
    item->fSuppressSpbfAfterPageBreak = 0;
    item->fWrapTrailSpaces = 0;
    item->fMapPrintTextColor = 0;
    item->fNoColumnBalance = 0;
    item->fConvMailMergeEsc = 0;
    item->fSuppressTopSpacing = 0;
    item->fOrigWordTableRules = 0;
    item->fTransparentMetafiles = 0;
    item->fShowBreaksInFrames = 0;
    item->fSwapBordersFacingPgs = 0;
    item->reserved = 0;
}


void
wvGetDOP (wvVersion ver, DOP * dop, U32 fcDop, U32 lcbDop, wvStream * fd)
{
    U16 temp16;
    U32 temp32;
    int i;

    if (ver != WORD8)
	wvInitDOP (dop);

    if (lcbDop <= 0)
	return;
    wvStream_goto (fd, fcDop);

    temp16 = read_16ubit (fd);

    dop->fFacingPages = temp16 & 0x0001;
    dop->fWidowControl = (temp16 & 0x0002) >> 1;
    dop->fPMHMainDoc = (temp16 & 0x0004) >> 2;
    dop->grfSuppression = (temp16 & 0x18) >> 3;
    dop->fpc = (temp16 & 0x0060) >> 5;
    wvTrace (("fpc is %d\n", dop->fpc));
    dop->reserved1 = (temp16 & 0x0080) >> 7;
    dop->grpfIhdt = (temp16 & 0xFF00) >> 8;

    temp16 = read_16ubit (fd);

    if (ver == WORD2)
      {
	  dop->fFtnRestart = temp16 & 0x0001;
	  dop->nFtn = (temp16 & 0xFFFE) >> 1;
      }
    else
      {
	  dop->rncFtn = temp16 & 0x0003;
	  dop->nFtn = (temp16 & 0xFFFC) >> 2;
      }
    temp16 = read_16ubit (fd);

    if (ver == WORD2)
      {
	  dop->irmBar = temp16 & 0x00FF;
	  dop->irmProps = temp16 & 0x8F00 >> 8;
      }
    else
      {
	  dop->fOutlineDirtySave = temp16 & 0x0001;
	  dop->reserved2 = (temp16 & 0x00FE) >> 1;

	  dop->fOnlyMacPics = (temp16 & 0x0100) >> 8;
	  dop->fOnlyWinPics = (temp16 & 0x0200) >> 9;
	  dop->fLabelDoc = (temp16 & 0x0400) >> 10;
	  dop->fHyphCapitals = (temp16 & 0x0800) >> 11;
	  dop->fAutoHyphen = (temp16 & 0x1000) >> 12;
	  dop->fFormNoFields = (temp16 & 0x2000) >> 13;
	  dop->fLinkStyles = (temp16 & 0x4000) >> 14;
      }

    dop->fRevMarking = (temp16 & 0x8000) >> 15;

    temp16 = read_16ubit (fd);

    dop->fBackup = temp16 & 0x0001;
    dop->fExactCWords = (temp16 & 0x0002) >> 1;
    dop->fPagHidden = (temp16 & 0x0004) >> 2;
    dop->fPagResults = (temp16 & 0x0008) >> 3;
    dop->fLockAtn = (temp16 & 0x0010) >> 4;
    dop->fMirrorMargins = (temp16 & 0x0020) >> 5;

    if (ver == WORD2)
      {
	  dop->fKeepFileFormat = (temp16 & 0x0040) >> 6;
      }
    else
      {
	  dop->reserved3 = (temp16 & 0x0040) >> 6;
      }
    dop->fDfltTrueType = (temp16 & 0x0080) >> 7;
    dop->fPagSuppressTopSpacing = (temp16 & 0x0100) >> 8;

    if (ver == WORD2)
      {
	  dop->fRTLAlignment = (temp16 & 0x0200) >> 9;
      }
    else
      {
	  dop->fProtEnabled = (temp16 & 0x0200) >> 9;
      }

    /* (Nameless in W2) */
    dop->fDispFormFldSel = (temp16 & 0x0400) >> 10;
    dop->fRMView = (temp16 & 0x0800) >> 11;
    dop->fRMPrint = (temp16 & 0x1000) >> 12;
    dop->reserved4 = (temp16 & 0x2000) >> 13;
    dop->fLockRev = (temp16 & 0x4000) >> 14;
    dop->fEmbedFonts = (temp16 & 0x8000) >> 15;

    if (ver == WORD2)
      {
	  temp16 = read_16ubit (fd);
	  /* nameless to (temp16&0xFE00)>>8 */
	  dop->fSpares = read_16ubit (fd);

      }


    /*
       not used in word 8 as far as i know, but are in previous word's
       not in word 2 either 
     */
    if (ver > WORD2)
	wvGetCOPTS (&dop->copts, fd);

    dop->dxaTab = read_16ubit (fd);

    if (ver == WORD2)
	dop->ftcDefaultBi = read_16ubit (fd);

    dop->wSpare = read_16ubit (fd);
    dop->dxaHotZ = read_16ubit (fd);

    if (ver > WORD2)
	dop->cConsecHypLim = read_16ubit (fd);

    dop->wSpare2 = read_16ubit (fd);
    if (ver == WORD2)
	dop->wSpare3 = read_16ubit (fd);

    wvGetDTTM (&dop->dttmCreated, fd);
    wvGetDTTM (&dop->dttmRevised, fd);
    wvGetDTTM (&dop->dttmLastPrint, fd);

    dop->nRevision = read_16ubit (fd);
    dop->tmEdited = read_32ubit (fd);
    dop->cWords = read_32ubit (fd);
    dop->cCh = read_32ubit (fd);
    dop->cPg = read_16ubit (fd);

    if (ver == WORD2)
      {
	  dop->rgwSpareDocSum[1] = read_16ubit (fd);
	  dop->rgwSpareDocSum[2] = read_16ubit (fd);
	  return;		/* No more for Word 2 */
      }
    else
      {
	  dop->cParas = read_32ubit (fd);
      }

    temp16 = read_16ubit (fd);

    dop->rncEdn = temp16 & 0x0003;
    dop->nEdn = (temp16 & 0xFFFC) >> 2;

    temp16 = read_16ubit (fd);

    dop->epc = temp16 & 0x0003;
    dop->nfcFtnRef = (temp16 & 0x003C) >> 2;
    dop->nfcEdnRef = (temp16 & 0x03C0) >> 6;
    dop->fPrintFormData = (temp16 & 0x0400) >> 10;
    dop->fSaveFormData = (temp16 & 0x0800) >> 11;
    dop->fShadeFormData = (temp16 & 0x1000) >> 12;
    dop->reserved6 = (temp16 & 0x6000) >> 13;
    dop->fWCFtnEdn = (temp16 & 0x8000) >> 15;

    dop->cLines = read_32ubit (fd);
    dop->cWordsFtnEnd = read_32ubit (fd);
    dop->cChFtnEdn = read_32ubit (fd);
    dop->cPgFtnEdn = read_16ubit (fd);
    dop->cParasFtnEdn = read_32ubit (fd);
    dop->cLinesFtnEdn = read_32ubit (fd);
    dop->lKeyProtDoc = read_32ubit (fd);


    temp16 = read_16ubit (fd);

    dop->wvkSaved = temp16 & 0x0007;
    dop->wScaleSaved = (temp16 & 0x0FF8) >> 3;
    dop->zkSaved = (temp16 & 0x3000) >> 12;
    dop->fRotateFontW6 = (temp16 & 0x4000) >> 14;
    dop->iGutterPos = (temp16 & 0x8000) >> 15;

    if (ver == WORD6)
      {
	  dop->fNoTabForInd = dop->copts.fNoTabForInd;
	  dop->fNoSpaceRaiseLower = dop->copts.fNoSpaceRaiseLower;
	  dop->fSuppressSpbfAfterPageBreak =
	      dop->copts.fSuppressSpbfAfterPageBreak;
	  dop->fWrapTrailSpaces = dop->copts.fWrapTrailSpaces;
	  dop->fMapPrintTextColor = dop->copts.fMapPrintTextColor;
	  dop->fNoColumnBalance = dop->copts.fNoColumnBalance;
	  dop->fConvMailMergeEsc = dop->copts.fConvMailMergeEsc;
	  dop->fSuppressTopSpacing = dop->copts.fSuppressTopSpacing;
	  dop->fOrigWordTableRules = dop->copts.fOrigWordTableRules;
	  dop->fTransparentMetafiles = dop->copts.fTransparentMetafiles;
	  dop->fShowBreaksInFrames = dop->copts.fShowBreaksInFrames;
	  dop->fSwapBordersFacingPgs = dop->copts.fSwapBordersFacingPgs;
	  return;
      }



    temp32 = read_32ubit (fd);

    dop->fNoTabForInd = temp32 & 0x00000001;
    dop->fNoSpaceRaiseLower = (temp32 & 0x00000002) >> 1;
    dop->fSuppressSpbfAfterPageBreak = (temp32 & 0x00000004) >> 2;
    dop->fWrapTrailSpaces = (temp32 & 0x00000008) >> 3;
    dop->fMapPrintTextColor = (temp32 & 0x00000010) >> 4;
    dop->fNoColumnBalance = (temp32 & 0x00000020) >> 5;
    dop->fConvMailMergeEsc = (temp32 & 0x00000040) >> 6;
    dop->fSuppressTopSpacing = (temp32 & 0x00000080) >> 7;
    dop->fOrigWordTableRules = (temp32 & 0x00000100) >> 8;
    dop->fTransparentMetafiles = (temp32 & 0x00000200) >> 9;
    dop->fShowBreaksInFrames = (temp32 & 0x00000400) >> 10;
    dop->fSwapBordersFacingPgs = (temp32 & 0x00000800) >> 11;
    dop->reserved7 = (temp32 & 0x0000F000) >> 12;
    dop->fSuppressTopSpacingMac5 = (temp32 & 0x00010000) >> 16;
    dop->fTruncDxaExpand = (temp32 & 0x00020000) >> 17;
    dop->fPrintBodyBeforeHdr = (temp32 & 0x00040000) >> 18;
    dop->fNoLeading = (temp32 & 0x00080000) >> 19;
    dop->reserved8 = (temp32 & 0x00100000) >> 20;
    dop->fMWSmallCaps = (temp32 & 0x00200000) >> 21;
    dop->reserved9 = (temp32 & 0xFFC00000) >> 22;

    if (ver == WORD7)
	return;

    dop->adt = read_16ubit (fd);
    wvGetDOPTYPOGRAPHY (&dop->doptypography, fd);
    wvGetDOGRID (&dop->dogrid, fd);

    temp16 = read_16ubit (fd);

    dop->reserved10 = temp16 & 0x0001;
    dop->lvl = (temp16 & 0x001E) >> 1;
    dop->fGramAllDone = (temp16 & 0x0020) >> 5;
    dop->fGramAllClean = (temp16 & 0x0040) >> 6;
    dop->fSubsetFonts = (temp16 & 0x0080) >> 7;
    dop->fHideLastVersion = (temp16 & 0x0100) >> 8;
    dop->fHtmlDoc = (temp16 & 0x0200) >> 9;
    dop->reserved11 = (temp16 & 0x0400) >> 10;
    dop->fSnapBorder = (temp16 & 0x0800) >> 11;
    dop->fIncludeHeader = (temp16 & 0x1000) >> 12;
    dop->fIncludeFooter = (temp16 & 0x2000) >> 13;
    dop->fForcePageSizePag = (temp16 & 0x4000) >> 14;
    dop->fMinFontSizePag = (temp16 & 0x8000) >> 15;

    temp16 = read_16ubit (fd);

    dop->fHaveVersions = temp16 & 0x0001;
    dop->fAutoVersion = (temp16 & 0x0002) >> 1;
    dop->reserved11 = (temp16 & 0xFFFC) >> 2;

    wvGetASUMYI (&dop->asumyi, fd);

    dop->cChWS = read_32ubit (fd);
    dop->cChWSFtnEdn = read_32ubit (fd);
    dop->grfDocEvents = read_32ubit (fd);

    temp32 = read_32ubit (fd);

    dop->fVirusPrompted = temp32 & 0x00000001;
    dop->fVirusLoadSafe = (temp32 & 0x00000002) >> 1;
    dop->KeyVirusSession30 = (temp32 & 0xFFFFFFFC) >> 2;

    for (i = 0; i < 30; i++)
	dop->Spare[i] = read_8ubit (fd);

    dop->reserved12 = read_32ubit (fd);
    dop->reserved13 = read_32ubit (fd);
    dop->cDBC = read_32ubit (fd);
    dop->cDBCFtnEdn = read_32ubit (fd);
    dop->reserved14 = read_32ubit (fd);
    dop->new_nfcFtnRef = read_16ubit (fd);
    dop->new_nfcEdnRef = read_16ubit (fd);
    dop->hpsZoonFontPag = read_16ubit (fd);
    dop->dywDispPag = read_16ubit (fd);

    /*
       if ((dop->fLockAtn) || (dop->fLockRev))
       wvError(("doc protection key is %x\n",dop->lKeyProtDoc));
     */
}

void
wvInitDOP (DOP * dop)
{
    int i;
    dop->fFacingPages = 0;
    dop->fWidowControl = 0;
    dop->fPMHMainDoc = 0;
    dop->grfSuppression = 0;
    dop->fpc = 0;
    dop->reserved1 = 0;
    dop->grpfIhdt = 0;
    dop->rncFtn = 0;
    dop->fFtnRestart = 0;
    dop->nFtn = 0;
    dop->irmBar = 0;
    dop->irmProps = 0;
    dop->fOutlineDirtySave = 0;
    dop->reserved2 = 0;
    dop->fOnlyMacPics = 0;
    dop->fOnlyWinPics = 0;
    dop->fLabelDoc = 0;
    dop->fHyphCapitals = 0;
    dop->fAutoHyphen = 0;
    dop->fFormNoFields = 0;
    dop->fLinkStyles = 0;
    dop->fRevMarking = 0;
    dop->fBackup = 0;
    dop->fExactCWords = 0;
    dop->fPagHidden = 0;
    dop->fPagResults = 0;
    dop->fLockAtn = 0;
    dop->fMirrorMargins = 0;
    dop->fKeepFileFormat = 0;
    dop->reserved3 = 0;
    dop->fDfltTrueType = 0;
    dop->fPagSuppressTopSpacing = 0;
    dop->fRTLAlignment = 0;
    dop->fSpares = 0;
    dop->fProtEnabled = 0;
    dop->fDispFormFldSel = 0;
    dop->fRMView = 0;
    dop->fRMPrint = 0;
    dop->reserved4 = 0;
    dop->fLockRev = 0;
    dop->fEmbedFonts = 0;
    wvInitCOPTS (&dop->copts);
    dop->dxaTab = 0;
    dop->ftcDefaultBi = 0;
    dop->wSpare = 0;
    dop->dxaHotZ = 0;
    dop->cConsecHypLim = 0;
    dop->wSpare2 = 0;
    wvInitDTTM (&dop->dttmCreated);
    wvInitDTTM (&dop->dttmRevised);
    wvInitDTTM (&dop->dttmLastPrint);
    dop->nRevision = 0;
    dop->tmEdited = 0;
    dop->cWords = 0;
    dop->cCh = 0;
    dop->cPg = 0;
    dop->cParas = 0;
    dop->rgwSpareDocSum[1] = 0;
    dop->rgwSpareDocSum[2] = 0;
    dop->rncEdn = 0;
    dop->nEdn = 0;
    dop->epc = 0;
    dop->nfcFtnRef = 0;
    dop->nfcEdnRef = 0;
    dop->fPrintFormData = 0;
    dop->fSaveFormData = 0;
    dop->fShadeFormData = 0;
    dop->reserved6 = 0;
    dop->fWCFtnEdn = 0;
    dop->cLines = 0;
    dop->cWordsFtnEnd = 0;
    dop->cChFtnEdn = 0;
    dop->cPgFtnEdn = 0;
    dop->cParasFtnEdn = 0;
    dop->cLinesFtnEdn = 0;
    dop->lKeyProtDoc = 0;
    dop->wvkSaved = 0;
    dop->wScaleSaved = 0;
    dop->zkSaved = 0;
    dop->fRotateFontW6 = 0;
    dop->iGutterPos = 0;
    dop->fNoTabForInd = 0;
    dop->fNoSpaceRaiseLower = 0;
    dop->fSuppressSpbfAfterPageBreak = 0;
    dop->fWrapTrailSpaces = 0;
    dop->fMapPrintTextColor = 0;
    dop->fNoColumnBalance = 0;
    dop->fConvMailMergeEsc = 0;
    dop->fSuppressTopSpacing = 0;
    dop->fOrigWordTableRules = 0;
    dop->fTransparentMetafiles = 0;
    dop->fShowBreaksInFrames = 0;
    dop->fSwapBordersFacingPgs = 0;
    dop->reserved7 = 0;
    dop->fSuppressTopSpacingMac5 = 0;
    dop->fTruncDxaExpand = 0;
    dop->fPrintBodyBeforeHdr = 0;
    dop->fNoLeading = 0;
    dop->reserved8 = 0;
    dop->fMWSmallCaps = 0;
    dop->reserved9 = 0;
    dop->adt = 0;
    wvInitDOPTYPOGRAPHY (&dop->doptypography);
    wvInitDOGRID (&dop->dogrid);
    dop->reserved10 = 0;
    dop->lvl = 0;
    dop->fGramAllDone = 0;
    dop->fGramAllClean = 0;
    dop->fSubsetFonts = 0;
    dop->fHideLastVersion = 0;
    dop->fHtmlDoc = 0;
    dop->reserved11 = 0;
    dop->fSnapBorder = 0;
    dop->fIncludeHeader = 0;
    dop->fIncludeFooter = 0;
    dop->fForcePageSizePag = 0;
    dop->fMinFontSizePag = 0;
    dop->fHaveVersions = 0;
    dop->fAutoVersion = 0;
    dop->reserved11 = 0;
    wvInitASUMYI (&dop->asumyi);
    dop->cChWS = 0;
    dop->cChWSFtnEdn = 0;
    dop->grfDocEvents = 0;
    dop->fVirusPrompted = 0;
    dop->fVirusLoadSafe = 0;
    dop->KeyVirusSession30 = 0;
    for (i = 0; i < 30; i++)
	dop->Spare[i] = 0;
    dop->reserved12 = 0;
    dop->reserved13 = 0;
    dop->cDBC = 0;
    dop->cDBCFtnEdn = 0;
    dop->reserved14 = 0;
    dop->new_nfcFtnRef = 0;
    dop->new_nfcEdnRef = 0;
    dop->hpsZoonFontPag = 0;
    dop->dywDispPag = 0;
}
