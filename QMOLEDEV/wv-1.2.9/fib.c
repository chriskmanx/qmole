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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "wv.h"

void
wvInitFIB (FIB * item)
{
    item->wIdent = 0;
    item->nFib = 0;
    item->nProduct = 0;
    item->lid = 0;
    item->pnNext = 0;
    item->fDot = 0;
    item->fGlsy = 0;
    item->fComplex = 0;
    item->fHasPic = 0;
    item->cQuickSaves = 0;
    item->fEncrypted = 0;
    item->fWhichTblStm = 0;
    item->fReadOnlyRecommended = 0;
    item->fWriteReservation = 0;
    item->fExtChar = 0;
    item->fLoadOverride = 0;
    item->fFarEast = 0;
    item->fCrypto = 0;
    item->nFibBack = 0;
    item->lKey = 0;
    item->envr = 0;
    item->fMac = 0;
    item->fEmptySpecial = 0;
    item->fLoadOverridePage = 0;
    item->fFutureSavedUndo = 0;
    item->fWord97Saved = 0;
    item->fSpare0 = 0;
    item->chse = 0;
    item->chsTables = 0;
    item->fcMin = 0;
    item->fcMac = 0;
    item->csw = 0;
    item->wMagicCreated = 0;
    item->wMagicRevised = 0;
    item->wMagicCreatedPrivate = 0;
    item->wMagicRevisedPrivate = 0;
    item->pnFbpChpFirst_W6 = 0;
    item->pnChpFirst_W6 = 0;
    item->cpnBteChp_W6 = 0;
    item->pnFbpPapFirst_W6 = 0;
    item->pnPapFirst_W6 = 0;
    item->cpnBtePap_W6 = 0;
    item->pnFbpLvcFirst_W6 = 0;
    item->pnLvcFirst_W6 = 0;
    item->cpnBteLvc_W6 = 0;
    item->lidFE = 0;
    item->clw = 0;
    item->cbMac = 0;
    item->lProductCreated = 0;
    item->lProductRevised = 0;
    item->ccpText = 0;
    item->ccpFtn = 0;
    item->ccpHdr = 0;
    item->ccpMcr = 0;
    item->ccpAtn = 0;
    item->ccpEdn = 0;
    item->ccpTxbx = 0;
    item->ccpHdrTxbx = 0;
    item->pnFbpChpFirst = 0;
    item->pnChpFirst = 0;
    item->cpnBteChp = 0;
    item->pnFbpPapFirst = 0;
    item->pnPapFirst = 0;
    item->cpnBtePap = 0;
    item->pnFbpLvcFirst = 0;
    item->pnLvcFirst = 0;
    item->cpnBteLvc = 0;
    item->fcIslandFirst = 0;
    item->fcIslandLim = 0;
    item->cfclcb = 0;
    item->fcStshfOrig = 0;
    item->lcbStshfOrig = 0;
    item->fcStshf = 0;
    item->lcbStshf = 0;
    item->fcPlcffndRef = 0;
    item->lcbPlcffndRef = 0;
    item->fcPlcffndTxt = 0;
    item->lcbPlcffndTxt = 0;
    item->fcPlcfandRef = 0;
    item->lcbPlcfandRef = 0;
    item->fcPlcfandTxt = 0;
    item->lcbPlcfandTxt = 0;
    item->fcPlcfsed = 0;
    item->lcbPlcfsed = 0;
    item->fcPlcpad = 0;
    item->lcbPlcpad = 0;
    item->fcPlcfphe = 0;
    item->lcbPlcfphe = 0;
    item->fcSttbfglsy = 0;
    item->lcbSttbfglsy = 0;
    item->fcPlcfglsy = 0;
    item->lcbPlcfglsy = 0;
    item->fcPlcfhdd = 0;
    item->lcbPlcfhdd = 0;
    item->fcPlcfbteChpx = 0;
    item->lcbPlcfbteChpx = 0;
    item->fcPlcfbtePapx = 0;
    item->lcbPlcfbtePapx = 0;
    item->fcPlcfsea = 0;
    item->lcbPlcfsea = 0;
    item->fcSttbfffn = 0;
    item->lcbSttbfffn = 0;
    item->fcPlcffldMom = 0;
    item->lcbPlcffldMom = 0;
    item->fcPlcffldHdr = 0;
    item->lcbPlcffldHdr = 0;
    item->fcPlcffldFtn = 0;
    item->lcbPlcffldFtn = 0;
    item->fcPlcffldAtn = 0;
    item->lcbPlcffldAtn = 0;
    item->fcPlcffldMcr = 0;
    item->lcbPlcffldMcr = 0;
    item->fcSttbfbkmk = 0;
    item->lcbSttbfbkmk = 0;
    item->fcPlcfbkf = 0;
    item->lcbPlcfbkf = 0;
    item->fcPlcfbkl = 0;
    item->lcbPlcfbkl = 0;
    item->fcCmds = 0;
    item->lcbCmds = 0;
    item->fcPlcmcr = 0;
    item->lcbPlcmcr = 0;
    item->fcSttbfmcr = 0;
    item->lcbSttbfmcr = 0;
    item->fcPrDrvr = 0;
    item->lcbPrDrvr = 0;
    item->fcPrEnvPort = 0;
    item->lcbPrEnvPort = 0;
    item->fcPrEnvLand = 0;
    item->lcbPrEnvLand = 0;
    item->fcWss = 0;
    item->lcbWss = 0;
    item->fcDop = 0;
    item->lcbDop = 0;
    item->fcSttbfAssoc = 0;
    item->lcbSttbfAssoc = 0;
    item->fcClx = 0;
    item->lcbClx = 0;
    item->fcPlcfpgdFtn = 0;
    item->lcbPlcfpgdFtn = 0;
    item->fcAutosaveSource = 0;
    item->lcbAutosaveSource = 0;
    item->fcGrpXstAtnOwners = 0;
    item->lcbGrpXstAtnOwners = 0;
    item->fcSttbfAtnbkmk = 0;
    item->lcbSttbfAtnbkmk = 0;
    item->fcPlcdoaMom = 0;
    item->lcbPlcdoaMom = 0;
    item->fcPlcdoaHdr = 0;
    item->lcbPlcdoaHdr = 0;
    item->fcPlcspaMom = 0;
    item->lcbPlcspaMom = 0;
    item->fcPlcspaHdr = 0;
    item->lcbPlcspaHdr = 0;
    item->fcPlcfAtnbkf = 0;
    item->lcbPlcfAtnbkf = 0;
    item->fcPlcfAtnbkl = 0;
    item->lcbPlcfAtnbkl = 0;
    item->fcPms = 0;
    item->lcbPms = 0;
    item->fcFormFldSttbs = 0;
    item->lcbFormFldSttbs = 0;
    item->fcPlcfendRef = 0;
    item->lcbPlcfendRef = 0;
    item->fcPlcfendTxt = 0;
    item->lcbPlcfendTxt = 0;
    item->fcPlcffldEdn = 0;
    item->lcbPlcffldEdn = 0;
    item->fcPlcfpgdEdn = 0;
    item->lcbPlcfpgdEdn = 0;
    item->fcDggInfo = 0;
    item->lcbDggInfo = 0;
    item->fcSttbfRMark = 0;
    item->lcbSttbfRMark = 0;
    item->fcSttbCaption = 0;
    item->lcbSttbCaption = 0;
    item->fcSttbAutoCaption = 0;
    item->lcbSttbAutoCaption = 0;
    item->fcPlcfwkb = 0;
    item->lcbPlcfwkb = 0;
    item->fcPlcfspl = 0;
    item->lcbPlcfspl = 0;
    item->fcPlcftxbxTxt = 0;
    item->lcbPlcftxbxTxt = 0;
    item->fcPlcffldTxbx = 0;
    item->lcbPlcffldTxbx = 0;
    item->fcPlcfhdrtxbxTxt = 0;
    item->lcbPlcfhdrtxbxTxt = 0;
    item->fcPlcffldHdrTxbx = 0;
    item->lcbPlcffldHdrTxbx = 0;
    item->fcStwUser = 0;
    item->lcbStwUser = 0;
    item->fcSttbttmbd = 0;
    item->cbSttbttmbd = 0;
    item->fcUnused = 0;
    item->lcbUnused = 0;
    item->fcPgdMother = 0;
    item->lcbPgdMother = 0;
    item->fcBkdMother = 0;
    item->lcbBkdMother = 0;
    item->fcPgdFtn = 0;
    item->lcbPgdFtn = 0;
    item->fcBkdFtn = 0;
    item->lcbBkdFtn = 0;
    item->fcPgdEdn = 0;
    item->lcbPgdEdn = 0;
    item->fcBkdEdn = 0;
    item->lcbBkdEdn = 0;
    item->fcSttbfIntlFld = 0;
    item->lcbSttbfIntlFld = 0;
    item->fcRouteSlip = 0;
    item->lcbRouteSlip = 0;
    item->fcSttbSavedBy = 0;
    item->lcbSttbSavedBy = 0;
    item->fcSttbFnm = 0;
    item->lcbSttbFnm = 0;
    item->fcPlcfLst = 0;
    item->lcbPlcfLst = 0;
    item->fcPlfLfo = 0;
    item->lcbPlfLfo = 0;
    item->fcPlcftxbxBkd = 0;
    item->lcbPlcftxbxBkd = 0;
    item->fcPlcftxbxHdrBkd = 0;
    item->lcbPlcftxbxHdrBkd = 0;
    item->fcDocUndo = 0;
    item->lcbDocUndo = 0;
    item->fcRgbuse = 0;
    item->lcbRgbuse = 0;
    item->fcUsp = 0;
    item->lcbUsp = 0;
    item->fcUskf = 0;
    item->lcbUskf = 0;
    item->fcPlcupcRgbuse = 0;
    item->lcbPlcupcRgbuse = 0;
    item->fcPlcupcUsp = 0;
    item->lcbPlcupcUsp = 0;
    item->fcSttbGlsyStyle = 0;
    item->lcbSttbGlsyStyle = 0;
    item->fcPlgosl = 0;
    item->lcbPlgosl = 0;
    item->fcPlcocx = 0;
    item->lcbPlcocx = 0;
    item->fcPlcfbteLvc = 0;
    item->lcbPlcfbteLvc = 0;
    wvInitFILETIME (&item->ftModified);
    item->fcPlcflvc = 0;
    item->lcbPlcflvc = 0;
    item->fcPlcasumy = 0;
    item->lcbPlcasumy = 0;
    item->fcPlcfgram = 0;
    item->lcbPlcfgram = 0;
    item->fcSttbListNames = 0;
    item->lcbSttbListNames = 0;
    item->fcSttbfUssr = 0;
    item->lcbSttbfUssr = 0;

    /* Word 2 */
    item->Spare = 0;
    item->rgwSpare0[0] = 0;
    item->rgwSpare0[1] = 0;
    item->rgwSpare0[2] = 0;
    item->fcSpare0 = 0;
    item->fcSpare1 = 0;
    item->fcSpare2 = 0;
    item->fcSpare3 = 0;
    item->ccpSpare0 = 0;
    item->ccpSpare1 = 0;
    item->ccpSpare2 = 0;
    item->ccpSpare3 = 0;
    item->fcPlcfpgd = 0;
    item->cbPlcfpgd = 0;

    item->fcSpare5 = 0;
    item->cbSpare5 = 0;
    item->fcSpare6 = 0;
    item->cbSpare6 = 0;
    item->wSpare4 = 0;
}

void
wvGetFIB (FIB * item, wvStream * fd)
{
    U16 temp16;
    U8 temp8;

    item->fEncrypted = 0;

    wvStream_goto (fd, 0);
#ifdef PURIFY
    wvInitFIB (item);
#endif
    item->wIdent = read_16ubit (fd);
    item->nFib = read_16ubit (fd);

    if ((wvQuerySupported (item, NULL) == WORD2))
      {
	  wvInitFIB (item);
	  wvStream_offset (fd, -4);
	  wvGetFIB2 (item, fd);
	  return;
      }

    if ((wvQuerySupported (item, NULL) == WORD5)
	|| (wvQuerySupported (item, NULL) == WORD6)
	|| (wvQuerySupported (item, NULL) == WORD7))
      {
	  wvInitFIB (item);
	  wvStream_offset (fd, -4);
	  wvGetFIB6 (item, fd);
	  return;
      }

    item->nProduct = read_16ubit (fd);
    item->lid = read_16ubit (fd);
    wvTrace (("lid is %x\n", item->lid));
    item->pnNext = (S16) read_16ubit (fd);
    temp16 = read_16ubit (fd);
    item->fDot = (temp16 & 0x0001);
    item->fGlsy = (temp16 & 0x0002) >> 1;
    item->fComplex = (temp16 & 0x0004) >> 2;
    item->fHasPic = (temp16 & 0x0008) >> 3;
    item->cQuickSaves = (temp16 & 0x00F0) >> 4;
    item->fEncrypted = (temp16 & 0x0100) >> 8;
    item->fWhichTblStm = (temp16 & 0x0200) >> 9;
    item->fReadOnlyRecommended = (temp16 & 0x0400) >> 10;
    item->fWriteReservation = (temp16 & 0x0800) >> 11;
    item->fExtChar = (temp16 & 0x1000) >> 12;
    wvTrace (("fExtChar is %d\n", item->fExtChar));
    item->fLoadOverride = (temp16 & 0x2000) >> 13;
    item->fFarEast = (temp16 & 0x4000) >> 14;
    item->fCrypto = (temp16 & 0x8000) >> 15;
    item->nFibBack = read_16ubit (fd);
    item->lKey = read_32ubit (fd);
    item->envr = read_8ubit (fd);
    temp8 = read_8ubit (fd);
    item->fMac = (temp8 & 0x01);
    item->fEmptySpecial = (temp8 & 0x02) >> 1;
    item->fLoadOverridePage = (temp8 & 0x04) >> 2;
    item->fFutureSavedUndo = (temp8 & 0x08) >> 3;
    item->fWord97Saved = (temp8 & 0x10) >> 4;
    item->fSpare0 = (temp8 & 0xFE) >> 5;
    item->chse = read_16ubit (fd);
    item->chsTables = read_16ubit (fd);
    item->fcMin = read_32ubit (fd);
    item->fcMac = read_32ubit (fd);
    item->csw = read_16ubit (fd);
    item->wMagicCreated = read_16ubit (fd);
    item->wMagicRevised = read_16ubit (fd);
    item->wMagicCreatedPrivate = read_16ubit (fd);
    item->wMagicRevisedPrivate = read_16ubit (fd);
    item->pnFbpChpFirst_W6 = (S16) read_16ubit (fd);
    item->pnChpFirst_W6 = (S16) read_16ubit (fd);
    item->cpnBteChp_W6 = (S16) read_16ubit (fd);
    item->pnFbpPapFirst_W6 = (S16) read_16ubit (fd);
    item->pnPapFirst_W6 = (S16) read_16ubit (fd);
    item->cpnBtePap_W6 = (S16) read_16ubit (fd);
    item->pnFbpLvcFirst_W6 = (S16) read_16ubit (fd);
    item->pnLvcFirst_W6 = (S16) read_16ubit (fd);
    item->cpnBteLvc_W6 = (S16) read_16ubit (fd);
    item->lidFE = (S16) read_16ubit (fd);
    item->clw = read_16ubit (fd);
    item->cbMac = (S32) read_32ubit (fd);
    item->lProductCreated = read_32ubit (fd);
    item->lProductRevised = read_32ubit (fd);
    item->ccpText = read_32ubit (fd);
    item->ccpFtn = (S32) read_32ubit (fd);
    item->ccpHdr = (S32) read_32ubit (fd);
    item->ccpMcr = (S32) read_32ubit (fd);
    item->ccpAtn = (S32) read_32ubit (fd);
    item->ccpEdn = (S32) read_32ubit (fd);
    item->ccpTxbx = (S32) read_32ubit (fd);
    item->ccpHdrTxbx = (S32) read_32ubit (fd);
    item->pnFbpChpFirst = (S32) read_32ubit (fd);
    item->pnChpFirst = (S32) read_32ubit (fd);
    item->cpnBteChp = (S32) read_32ubit (fd);
    item->pnFbpPapFirst = (S32) read_32ubit (fd);
    item->pnPapFirst = (S32) read_32ubit (fd);
    item->cpnBtePap = (S32) read_32ubit (fd);
    item->pnFbpLvcFirst = (S32) read_32ubit (fd);
    item->pnLvcFirst = (S32) read_32ubit (fd);
    item->cpnBteLvc = (S32) read_32ubit (fd);
    item->fcIslandFirst = (S32) read_32ubit (fd);
    item->fcIslandLim = (S32) read_32ubit (fd);
    item->cfclcb = read_16ubit (fd);
    item->fcStshfOrig = (S32) read_32ubit (fd);
    item->lcbStshfOrig = read_32ubit (fd);
    item->fcStshf = (S32) read_32ubit (fd);
    item->lcbStshf = read_32ubit (fd);

    item->fcPlcffndRef = (S32) read_32ubit (fd);
    item->lcbPlcffndRef = read_32ubit (fd);
    item->fcPlcffndTxt = (S32) read_32ubit (fd);
    item->lcbPlcffndTxt = read_32ubit (fd);
    item->fcPlcfandRef = (S32) read_32ubit (fd);
    item->lcbPlcfandRef = read_32ubit (fd);
    item->fcPlcfandTxt = (S32) read_32ubit (fd);
    item->lcbPlcfandTxt = read_32ubit (fd);
    item->fcPlcfsed = (S32) read_32ubit (fd);
    item->lcbPlcfsed = read_32ubit (fd);
    item->fcPlcpad = (S32) read_32ubit (fd);
    item->lcbPlcpad = read_32ubit (fd);
    item->fcPlcfphe = (S32) read_32ubit (fd);
    item->lcbPlcfphe = read_32ubit (fd);
    item->fcSttbfglsy = (S32) read_32ubit (fd);
    item->lcbSttbfglsy = read_32ubit (fd);
    item->fcPlcfglsy = (S32) read_32ubit (fd);
    item->lcbPlcfglsy = read_32ubit (fd);
    item->fcPlcfhdd = (S32) read_32ubit (fd);
    item->lcbPlcfhdd = read_32ubit (fd);
    item->fcPlcfbteChpx = (S32) read_32ubit (fd);
    item->lcbPlcfbteChpx = read_32ubit (fd);
    item->fcPlcfbtePapx = (S32) read_32ubit (fd);
    item->lcbPlcfbtePapx = read_32ubit (fd);
    item->fcPlcfsea = (S32) read_32ubit (fd);
    item->lcbPlcfsea = read_32ubit (fd);
    item->fcSttbfffn = (S32) read_32ubit (fd);
    item->lcbSttbfffn = read_32ubit (fd);
    item->fcPlcffldMom = (S32) read_32ubit (fd);
    item->lcbPlcffldMom = read_32ubit (fd);
    item->fcPlcffldHdr = (S32) read_32ubit (fd);
    item->lcbPlcffldHdr = read_32ubit (fd);
    item->fcPlcffldFtn = (S32) read_32ubit (fd);
    item->lcbPlcffldFtn = read_32ubit (fd);
    item->fcPlcffldAtn = (S32) read_32ubit (fd);
    item->lcbPlcffldAtn = read_32ubit (fd);
    item->fcPlcffldMcr = (S32) read_32ubit (fd);
    item->lcbPlcffldMcr = read_32ubit (fd);
    item->fcSttbfbkmk = (S32) read_32ubit (fd);
    item->lcbSttbfbkmk = read_32ubit (fd);
    item->fcPlcfbkf = (S32) read_32ubit (fd);
    item->lcbPlcfbkf = read_32ubit (fd);
    item->fcPlcfbkl = (S32) read_32ubit (fd);
    item->lcbPlcfbkl = read_32ubit (fd);
    item->fcCmds = (S32) read_32ubit (fd);
    item->lcbCmds = read_32ubit (fd);
    item->fcPlcmcr = (S32) read_32ubit (fd);
    item->lcbPlcmcr = read_32ubit (fd);
    item->fcSttbfmcr = (S32) read_32ubit (fd);
    item->lcbSttbfmcr = read_32ubit (fd);
    item->fcPrDrvr = (S32) read_32ubit (fd);
    item->lcbPrDrvr = read_32ubit (fd);
    item->fcPrEnvPort = (S32) read_32ubit (fd);
    item->lcbPrEnvPort = read_32ubit (fd);
    item->fcPrEnvLand = (S32) read_32ubit (fd);
    item->lcbPrEnvLand = read_32ubit (fd);
    item->fcWss = (S32) read_32ubit (fd);
    item->lcbWss = read_32ubit (fd);
    item->fcDop = (S32) read_32ubit (fd);
    item->lcbDop = read_32ubit (fd);
    item->fcSttbfAssoc = (S32) read_32ubit (fd);
    item->lcbSttbfAssoc = read_32ubit (fd);
    item->fcClx = (S32) read_32ubit (fd);
    item->lcbClx = read_32ubit (fd);
    item->fcPlcfpgdFtn = (S32) read_32ubit (fd);
    item->lcbPlcfpgdFtn = read_32ubit (fd);
    item->fcAutosaveSource = (S32) read_32ubit (fd);
    item->lcbAutosaveSource = read_32ubit (fd);
    item->fcGrpXstAtnOwners = (S32) read_32ubit (fd);
    item->lcbGrpXstAtnOwners = read_32ubit (fd);
    item->fcSttbfAtnbkmk = (S32) read_32ubit (fd);
    item->lcbSttbfAtnbkmk = read_32ubit (fd);
    item->fcPlcdoaMom = (S32) read_32ubit (fd);
    item->lcbPlcdoaMom = read_32ubit (fd);
    item->fcPlcdoaHdr = (S32) read_32ubit (fd);
    item->lcbPlcdoaHdr = read_32ubit (fd);
    item->fcPlcspaMom = (S32) read_32ubit (fd);
    item->lcbPlcspaMom = read_32ubit (fd);
    item->fcPlcspaHdr = (S32) read_32ubit (fd);
    item->lcbPlcspaHdr = read_32ubit (fd);
    item->fcPlcfAtnbkf = (S32) read_32ubit (fd);
    item->lcbPlcfAtnbkf = read_32ubit (fd);
    item->fcPlcfAtnbkl = (S32) read_32ubit (fd);
    item->lcbPlcfAtnbkl = read_32ubit (fd);
    item->fcPms = (S32) read_32ubit (fd);
    item->lcbPms = read_32ubit (fd);
    item->fcFormFldSttbs = (S32) read_32ubit (fd);
    item->lcbFormFldSttbs = read_32ubit (fd);
    item->fcPlcfendRef = (S32) read_32ubit (fd);
    item->lcbPlcfendRef = read_32ubit (fd);
    item->fcPlcfendTxt = (S32) read_32ubit (fd);
    item->lcbPlcfendTxt = read_32ubit (fd);
    item->fcPlcffldEdn = (S32) read_32ubit (fd);
    item->lcbPlcffldEdn = read_32ubit (fd);
    item->fcPlcfpgdEdn = (S32) read_32ubit (fd);
    item->lcbPlcfpgdEdn = read_32ubit (fd);
    item->fcDggInfo = (S32) read_32ubit (fd);
    item->lcbDggInfo = read_32ubit (fd);
    item->fcSttbfRMark = (S32) read_32ubit (fd);
    item->lcbSttbfRMark = read_32ubit (fd);
    item->fcSttbCaption = (S32) read_32ubit (fd);
    item->lcbSttbCaption = read_32ubit (fd);
    item->fcSttbAutoCaption = (S32) read_32ubit (fd);
    item->lcbSttbAutoCaption = read_32ubit (fd);
    item->fcPlcfwkb = (S32) read_32ubit (fd);
    item->lcbPlcfwkb = read_32ubit (fd);
    item->fcPlcfspl = (S32) read_32ubit (fd);
    item->lcbPlcfspl = read_32ubit (fd);
    item->fcPlcftxbxTxt = (S32) read_32ubit (fd);
    item->lcbPlcftxbxTxt = read_32ubit (fd);
    item->fcPlcffldTxbx = (S32) read_32ubit (fd);
    item->lcbPlcffldTxbx = read_32ubit (fd);
    item->fcPlcfhdrtxbxTxt = (S32) read_32ubit (fd);
    item->lcbPlcfhdrtxbxTxt = read_32ubit (fd);
    item->fcPlcffldHdrTxbx = (S32) read_32ubit (fd);
    item->lcbPlcffldHdrTxbx = read_32ubit (fd);
    item->fcStwUser = (S32) read_32ubit (fd);
    item->lcbStwUser = read_32ubit (fd);
    item->fcSttbttmbd = (S32) read_32ubit (fd);
    item->cbSttbttmbd = read_32ubit (fd);
    item->fcUnused = (S32) read_32ubit (fd);
    item->lcbUnused = read_32ubit (fd);
    item->fcPgdMother = (S32) read_32ubit (fd);
    item->lcbPgdMother = read_32ubit (fd);
    item->fcBkdMother = (S32) read_32ubit (fd);
    item->lcbBkdMother = read_32ubit (fd);
    item->fcPgdFtn = (S32) read_32ubit (fd);
    item->lcbPgdFtn = read_32ubit (fd);
    item->fcBkdFtn = (S32) read_32ubit (fd);
    item->lcbBkdFtn = read_32ubit (fd);
    item->fcPgdEdn = (S32) read_32ubit (fd);
    item->lcbPgdEdn = read_32ubit (fd);
    item->fcBkdEdn = (S32) read_32ubit (fd);
    item->lcbBkdEdn = read_32ubit (fd);
    item->fcSttbfIntlFld = (S32) read_32ubit (fd);
    item->lcbSttbfIntlFld = read_32ubit (fd);
    item->fcRouteSlip = (S32) read_32ubit (fd);
    item->lcbRouteSlip = read_32ubit (fd);
    item->fcSttbSavedBy = (S32) read_32ubit (fd);
    item->lcbSttbSavedBy = read_32ubit (fd);
    item->fcSttbFnm = (S32) read_32ubit (fd);
    item->lcbSttbFnm = read_32ubit (fd);
    item->fcPlcfLst = (S32) read_32ubit (fd);
    item->lcbPlcfLst = read_32ubit (fd);
    item->fcPlfLfo = (S32) read_32ubit (fd);
    item->lcbPlfLfo = read_32ubit (fd);
    item->fcPlcftxbxBkd = (S32) read_32ubit (fd);
    item->lcbPlcftxbxBkd = read_32ubit (fd);
    item->fcPlcftxbxHdrBkd = (S32) read_32ubit (fd);
    item->lcbPlcftxbxHdrBkd = read_32ubit (fd);
    item->fcDocUndo = (S32) read_32ubit (fd);
    item->lcbDocUndo = read_32ubit (fd);
    item->fcRgbuse = (S32) read_32ubit (fd);
    item->lcbRgbuse = read_32ubit (fd);
    item->fcUsp = (S32) read_32ubit (fd);
    item->lcbUsp = read_32ubit (fd);
    item->fcUskf = (S32) read_32ubit (fd);
    item->lcbUskf = read_32ubit (fd);
    item->fcPlcupcRgbuse = (S32) read_32ubit (fd);
    item->lcbPlcupcRgbuse = read_32ubit (fd);
    item->fcPlcupcUsp = (S32) read_32ubit (fd);
    item->lcbPlcupcUsp = read_32ubit (fd);
    item->fcSttbGlsyStyle = (S32) read_32ubit (fd);
    item->lcbSttbGlsyStyle = read_32ubit (fd);
    item->fcPlgosl = (S32) read_32ubit (fd);
    item->lcbPlgosl = read_32ubit (fd);
    item->fcPlcocx = (S32) read_32ubit (fd);
    item->lcbPlcocx = read_32ubit (fd);
    item->fcPlcfbteLvc = (S32) read_32ubit (fd);
    item->lcbPlcfbteLvc = read_32ubit (fd);
    wvGetFILETIME (&(item->ftModified), fd);
    item->fcPlcflvc = (S32) read_32ubit (fd);
    item->lcbPlcflvc = read_32ubit (fd);
    item->fcPlcasumy = (S32) read_32ubit (fd);
    item->lcbPlcasumy = read_32ubit (fd);
    item->fcPlcfgram = (S32) read_32ubit (fd);
    item->lcbPlcfgram = read_32ubit (fd);
    item->fcSttbListNames = (S32) read_32ubit (fd);
    item->lcbSttbListNames = read_32ubit (fd);
    item->fcSttbfUssr = (S32) read_32ubit (fd);
    item->lcbSttbfUssr = read_32ubit (fd);
}

wvStream *
wvWhichTableStream (FIB * fib, wvParseStruct * ps)
{
    wvStream *ret;

    if ((wvQuerySupported (fib, NULL) & 0x7fff) == WORD8)
      {
	  if (fib->fWhichTblStm)
	    {
		wvTrace (("1Table\n"));
		ret = ps->tablefd1;
		if (ret == NULL)
		  {
		      wvError (
			       ("!!, the FIB lied to us, (told us to use the 1Table) making a heroic effort to use the other table stream, hold on tight\n"));
		      ret = ps->tablefd0;
		  }
	    }
	  else
	    {
		wvTrace (("0Table\n"));
		ret = ps->tablefd0;
		if (ret == NULL)
		  {
		      wvError (
			       ("!!, the FIB lied to us, (told us to use the 0Table) making a heroic effort to use the other table stream, hold on tight\n"));
		      ret = ps->tablefd1;
		  }
	    }
      }
    else			/* word 7- */
	ret = ps->mainfd;
    return (ret);
}


wvVersion
wvQuerySupported (FIB * fib, int *reason)
{
    int ret = WORD8;

    if (fib->wIdent == 0x37FE)
	ret = WORD5;
    else
      {
	  /*begin from microsofts kb q 40 */
	  if (fib->nFib < 101)
	    {
		if (reason)
		    *reason = 1;
		ret = WORD2;
	    }
	  else
	    {
		switch (fib->nFib)
		  {
		  case 101:
		      if (reason)
			  *reason = 2;
		      ret = WORD6;
		      break;	/* I'm pretty sure we should break here, Jamie. */
		  case 103:
		  case 104:
		      if (reason)
			  *reason = 3;
		      ret = WORD7;
		      break;	/* I'm pretty sure we should break here, Jamie. */
		  default:
		      break;
		  }
	    }
	  /*end from microsofts kb q 40 */
      }
    wvTrace (("RET is %d\n", ret));
    if (fib->fEncrypted)
      {
	  if (reason)
	      *reason = 4;
	  ret |= 0x8000;
      }
    return (ret);
}

void
wvGetFIB2 (FIB * item, wvStream * fd)
{
    U16 temp16 = 0;

    item->wIdent = read_16ubit (fd);
    item->nFib = read_16ubit (fd);

    item->nProduct = read_16ubit (fd);
    item->lid = read_16ubit (fd);
    wvTrace (("lid is %x\n", item->lid));
    item->pnNext = (S16) read_16ubit (fd);
    temp16 = read_16ubit (fd);

    item->fDot = (temp16 & 0x0001);
    item->fGlsy = (temp16 & 0x0002) >> 1;
    item->fComplex = (temp16 & 0x0004) >> 2;
    item->fHasPic = (temp16 & 0x0008) >> 3;
    item->cQuickSaves = (temp16 & 0x00F0) >> 4;
    item->fEncrypted = (temp16 & 0x0100) >> 8;
    item->fWhichTblStm = 0;	/* Unused from here on */
    item->fReadOnlyRecommended = 0;
    item->fWriteReservation = 0;
    item->fExtChar = 0;
    item->fLoadOverride = 0;
    item->fFarEast = 0;
    item->fCrypto = 0;

    item->nFibBack = read_16ubit (fd);
    wvTrace (("nFibBack is %d\n", item->nFibBack));

    item->Spare = read_32ubit (fd);	/* A spare for W2 */
    item->rgwSpare0[0] = read_16ubit (fd);
    item->rgwSpare0[1] = read_16ubit (fd);
    item->rgwSpare0[2] = read_16ubit (fd);
    item->fcMin = read_32ubit (fd);	/* These appear correct MV 29.8.2000 */
    item->fcMac = read_32ubit (fd);
    wvTrace (("fc from %d to %d\n", item->fcMin, item->fcMac));

    item->cbMac = read_32ubit (fd);	/* Last byte file position plus one. */

    item->fcSpare0 = read_32ubit (fd);
    item->fcSpare1 = read_32ubit (fd);
    item->fcSpare2 = read_32ubit (fd);
    item->fcSpare3 = read_32ubit (fd);

    item->ccpText = read_32ubit (fd);
    wvTrace (("length %d == %d\n", item->fcMac - item->fcMin, item->ccpText));

    item->ccpFtn = (S32) read_32ubit (fd);
    item->ccpHdr = (S32) read_32ubit (fd);
    item->ccpMcr = (S32) read_32ubit (fd);
    item->ccpAtn = (S32) read_32ubit (fd);
    item->ccpSpare0 = (S32) read_32ubit (fd);
    item->ccpSpare1 = (S32) read_32ubit (fd);
    item->ccpSpare2 = (S32) read_32ubit (fd);
    item->ccpSpare3 = (S32) read_32ubit (fd);

    item->fcStshfOrig = read_32ubit (fd);
    item->lcbStshfOrig = (S32) read_16ubit (fd);
    item->fcStshf = read_32ubit (fd);
    item->lcbStshf = (S32) read_16ubit (fd);
    item->fcPlcffndRef = read_32ubit (fd);
    item->lcbPlcffndRef = (S32) read_16ubit (fd);
    item->fcPlcffndTxt = read_32ubit (fd);
    item->lcbPlcffndTxt = (S32) read_16ubit (fd);
    item->fcPlcfandRef = read_32ubit (fd);
    item->lcbPlcfandRef = (S32) read_16ubit (fd);
    item->fcPlcfandTxt = read_32ubit (fd);
    item->lcbPlcfandTxt = (S32) read_16ubit (fd);
    item->fcPlcfsed = read_32ubit (fd);
    item->lcbPlcfsed = (S32) read_16ubit (fd);
    item->fcPlcfpgd = read_32ubit (fd);
    item->cbPlcfpgd = read_16ubit (fd);
    item->fcPlcfphe = read_32ubit (fd);
    item->lcbPlcfphe = (S32) read_16ubit (fd);
    item->fcPlcfglsy = read_32ubit (fd);
    item->lcbPlcfglsy = (S32) read_16ubit (fd);
    item->fcPlcfhdd = read_32ubit (fd);
    item->lcbPlcfhdd = (S32) read_16ubit (fd);
    item->fcPlcfbteChpx = read_32ubit (fd);
    item->lcbPlcfbteChpx = (S32) read_16ubit (fd);
    item->fcPlcfbtePapx = read_32ubit (fd);
    item->lcbPlcfbtePapx = (S32) read_16ubit (fd);
    item->fcPlcfsea = read_32ubit (fd);
    item->lcbPlcfsea = (S32) read_16ubit (fd);
    item->fcSttbfffn = read_32ubit (fd);
    item->lcbSttbfffn = (S32) read_16ubit (fd);
    item->fcPlcffldMom = read_32ubit (fd);
    item->lcbPlcffldMom = (S32) read_16ubit (fd);
    item->fcPlcffldHdr = read_32ubit (fd);
    item->lcbPlcffldHdr = (S32) read_16ubit (fd);
    item->fcPlcffldFtn = read_32ubit (fd);
    item->lcbPlcffldFtn = (S32) read_16ubit (fd);
    item->fcPlcffldAtn = read_32ubit (fd);
    item->lcbPlcffldAtn = (S32) read_16ubit (fd);
    item->fcPlcffldMcr = read_32ubit (fd);
    item->lcbPlcffldMcr = (S32) read_16ubit (fd);
    item->fcSttbfbkmk = read_32ubit (fd);
    item->lcbSttbfbkmk = (S32) read_16ubit (fd);
    item->fcPlcfbkf = read_32ubit (fd);
    item->lcbPlcfbkf = (S32) read_16ubit (fd);
    item->fcPlcfbkl = read_32ubit (fd);
    item->lcbPlcfbkl = (S32) read_16ubit (fd);
    item->fcCmds = read_32ubit (fd);
    item->lcbCmds = (S32) read_16ubit (fd);
    item->fcPlcmcr = read_32ubit (fd);
    item->lcbPlcmcr = (S32) read_16ubit (fd);
    item->fcSttbfmcr = read_32ubit (fd);
    item->lcbSttbfmcr = (S32) read_16ubit (fd);
    item->fcPrDrvr = read_32ubit (fd);
    item->lcbPrDrvr = (S32) read_16ubit (fd);
    item->fcPrEnvPort = read_32ubit (fd);
    item->lcbPrEnvPort = (S32) read_16ubit (fd);
    item->fcPrEnvLand = read_32ubit (fd);
    item->lcbPrEnvLand = (S32) read_16ubit (fd);
    item->fcWss = read_32ubit (fd);
    item->lcbWss = (S32) read_16ubit (fd);
    item->fcDop = read_32ubit (fd);
    item->lcbDop = (S32) read_16ubit (fd);
    item->fcSttbfAssoc = read_32ubit (fd);
    item->lcbSttbfAssoc = (S32) read_16ubit (fd);
    item->fcClx = read_32ubit (fd);
    item->lcbClx = (S32) read_16ubit (fd);
    item->fcPlcfpgdFtn = read_32ubit (fd);
    item->lcbPlcfpgdFtn = (S32) read_16ubit (fd);
    item->fcAutosaveSource = read_32ubit (fd);
    item->lcbAutosaveSource = (S32) read_16ubit (fd);
    item->fcSpare5 = read_32ubit (fd);
    item->cbSpare5 = read_16ubit (fd);
    item->fcSpare6 = read_32ubit (fd);
    item->cbSpare6 = read_16ubit (fd);
    item->wSpare4 = read_16ubit (fd);
    item->pnChpFirst = read_16ubit (fd);
    item->pnPapFirst = read_16ubit (fd);
    item->cpnBteChp = read_16ubit (fd);
    item->cpnBtePap = read_16ubit (fd);

}

void
wvGetFIB6 (FIB * item, wvStream * fd)
{
    U16 temp16;
    U8 temp8;

    item->wIdent = read_16ubit (fd);
    item->nFib = read_16ubit (fd);

    item->nProduct = read_16ubit (fd);
    item->lid = read_16ubit (fd);
    wvTrace (("lid is %x\n", item->lid));
    item->pnNext = (S16) read_16ubit (fd);
    temp16 = read_16ubit (fd);

    item->fDot = (temp16 & 0x0001);
    item->fGlsy = (temp16 & 0x0002) >> 1;
    item->fComplex = (temp16 & 0x0004) >> 2;
    item->fHasPic = (temp16 & 0x0008) >> 3;
    item->cQuickSaves = (temp16 & 0x00F0) >> 4;
    item->fEncrypted = (temp16 & 0x0100) >> 8;
    item->fWhichTblStm = 0;	/* word 6 files only have one table stream */
    item->fReadOnlyRecommended = (temp16 & 0x0400) >> 10;
    item->fWriteReservation = (temp16 & 0x0800) >> 11;
    item->fExtChar = (temp16 & 0x1000) >> 12;
    wvTrace (("fExtChar is %d\n", item->fExtChar));
    item->fLoadOverride = 0;
    item->fFarEast = 0;
    item->fCrypto = 0;
    item->nFibBack = read_16ubit (fd);
    item->lKey = read_32ubit (fd);
    item->envr = read_8ubit (fd);
    temp8 = read_8ubit (fd);
    item->fMac = 0;
    item->fEmptySpecial = 0;
    item->fLoadOverridePage = 0;
    item->fFutureSavedUndo = 0;
    item->fWord97Saved = 0;
    item->fSpare0 = 0;
    item->chse = read_16ubit (fd);
    item->chsTables = read_16ubit (fd);
    item->fcMin = read_32ubit (fd);
    item->fcMac = read_32ubit (fd);

    item->csw = 14;
    item->wMagicCreated = 0xCA0;	/*this is the unique id of the creater, so its me :-) */

    item->cbMac = read_32ubit (fd);

    read_16ubit (fd);
    read_16ubit (fd);
    read_16ubit (fd);
    read_16ubit (fd);
    read_16ubit (fd);
    read_16ubit (fd);
    read_16ubit (fd);
    read_16ubit (fd);

    item->ccpText = read_32ubit (fd);
    item->ccpFtn = (S32) read_32ubit (fd);
    item->ccpHdr = (S32) read_32ubit (fd);
    item->ccpMcr = (S32) read_32ubit (fd);
    item->ccpAtn = (S32) read_32ubit (fd);
    item->ccpEdn = (S32) read_32ubit (fd);
    item->ccpTxbx = (S32) read_32ubit (fd);
    item->ccpHdrTxbx = (S32) read_32ubit (fd);

    read_32ubit (fd);

    item->fcStshfOrig = (S32) read_32ubit (fd);
    item->lcbStshfOrig = read_32ubit (fd);
    item->fcStshf = (S32) read_32ubit (fd);
    item->lcbStshf = read_32ubit (fd);
    item->fcPlcffndRef = (S32) read_32ubit (fd);
    item->lcbPlcffndRef = read_32ubit (fd);
    item->fcPlcffndTxt = (S32) read_32ubit (fd);
    item->lcbPlcffndTxt = read_32ubit (fd);
    item->fcPlcfandRef = (S32) read_32ubit (fd);
    item->lcbPlcfandRef = read_32ubit (fd);
    item->fcPlcfandTxt = (S32) read_32ubit (fd);
    item->lcbPlcfandTxt = read_32ubit (fd);
    item->fcPlcfsed = (S32) read_32ubit (fd);
    item->lcbPlcfsed = read_32ubit (fd);
    item->fcPlcpad = (S32) read_32ubit (fd);
    item->lcbPlcpad = read_32ubit (fd);
    item->fcPlcfphe = (S32) read_32ubit (fd);
    item->lcbPlcfphe = read_32ubit (fd);
    item->fcSttbfglsy = (S32) read_32ubit (fd);
    item->lcbSttbfglsy = read_32ubit (fd);
    item->fcPlcfglsy = (S32) read_32ubit (fd);
    item->lcbPlcfglsy = read_32ubit (fd);
    item->fcPlcfhdd = (S32) read_32ubit (fd);
    item->lcbPlcfhdd = read_32ubit (fd);
    item->fcPlcfbteChpx = (S32) read_32ubit (fd);
    item->lcbPlcfbteChpx = read_32ubit (fd);
    item->fcPlcfbtePapx = (S32) read_32ubit (fd);
    item->lcbPlcfbtePapx = read_32ubit (fd);
    item->fcPlcfsea = (S32) read_32ubit (fd);
    item->lcbPlcfsea = read_32ubit (fd);
    item->fcSttbfffn = (S32) read_32ubit (fd);
    item->lcbSttbfffn = read_32ubit (fd);
    item->fcPlcffldMom = (S32) read_32ubit (fd);
    item->lcbPlcffldMom = read_32ubit (fd);
    item->fcPlcffldHdr = (S32) read_32ubit (fd);
    item->lcbPlcffldHdr = read_32ubit (fd);
    item->fcPlcffldFtn = (S32) read_32ubit (fd);
    item->lcbPlcffldFtn = read_32ubit (fd);
    item->fcPlcffldAtn = (S32) read_32ubit (fd);
    item->lcbPlcffldAtn = read_32ubit (fd);
    item->fcPlcffldMcr = (S32) read_32ubit (fd);
    item->lcbPlcffldMcr = read_32ubit (fd);
    item->fcSttbfbkmk = (S32) read_32ubit (fd);
    item->lcbSttbfbkmk = read_32ubit (fd);
    item->fcPlcfbkf = (S32) read_32ubit (fd);
    item->lcbPlcfbkf = read_32ubit (fd);
    item->fcPlcfbkl = (S32) read_32ubit (fd);
    item->lcbPlcfbkl = read_32ubit (fd);
    item->fcCmds = (S32) read_32ubit (fd);
    item->lcbCmds = read_32ubit (fd);
    item->fcPlcmcr = (S32) read_32ubit (fd);
    item->lcbPlcmcr = read_32ubit (fd);
    item->fcSttbfmcr = (S32) read_32ubit (fd);
    item->lcbSttbfmcr = read_32ubit (fd);
    item->fcPrDrvr = (S32) read_32ubit (fd);
    item->lcbPrDrvr = read_32ubit (fd);
    item->fcPrEnvPort = (S32) read_32ubit (fd);
    item->lcbPrEnvPort = read_32ubit (fd);
    item->fcPrEnvLand = (S32) read_32ubit (fd);
    item->lcbPrEnvLand = read_32ubit (fd);
    item->fcWss = (S32) read_32ubit (fd);
    item->lcbWss = read_32ubit (fd);
    item->fcDop = (S32) read_32ubit (fd);
    item->lcbDop = read_32ubit (fd);
    item->fcSttbfAssoc = (S32) read_32ubit (fd);
    item->lcbSttbfAssoc = read_32ubit (fd);
    item->fcClx = (S32) read_32ubit (fd);
    item->lcbClx = read_32ubit (fd);
    item->fcPlcfpgdFtn = (S32) read_32ubit (fd);
    item->lcbPlcfpgdFtn = read_32ubit (fd);
    item->fcAutosaveSource = (S32) read_32ubit (fd);
    item->lcbAutosaveSource = read_32ubit (fd);
    item->fcGrpXstAtnOwners = (S32) read_32ubit (fd);
    item->lcbGrpXstAtnOwners = read_32ubit (fd);
    item->fcSttbfAtnbkmk = (S32) read_32ubit (fd);
    item->lcbSttbfAtnbkmk = read_32ubit (fd);

    read_16ubit (fd);

    item->pnChpFirst = (S32) read_16ubit (fd);
    item->pnPapFirst = (S32) read_16ubit (fd);
    item->cpnBteChp = (S32) read_16ubit (fd);
    item->cpnBtePap = (S32) read_16ubit (fd);
    item->fcPlcdoaMom = (S32) read_32ubit (fd);
    item->lcbPlcdoaMom = read_32ubit (fd);
    item->fcPlcdoaHdr = (S32) read_32ubit (fd);
    item->lcbPlcdoaHdr = read_32ubit (fd);

    read_32ubit (fd);
    read_32ubit (fd);
    read_32ubit (fd);
    read_32ubit (fd);

    item->fcPlcfAtnbkf = (S32) read_32ubit (fd);
    item->lcbPlcfAtnbkf = read_32ubit (fd);
    item->fcPlcfAtnbkl = (S32) read_32ubit (fd);
    item->lcbPlcfAtnbkl = read_32ubit (fd);
    item->fcPms = (S32) read_32ubit (fd);
    item->lcbPms = read_32ubit (fd);
    item->fcFormFldSttbs = (S32) read_32ubit (fd);
    item->lcbFormFldSttbs = read_32ubit (fd);
    item->fcPlcfendRef = (S32) read_32ubit (fd);
    item->lcbPlcfendRef = read_32ubit (fd);
    item->fcPlcfendTxt = (S32) read_32ubit (fd);
    item->lcbPlcfendTxt = read_32ubit (fd);
    item->fcPlcffldEdn = (S32) read_32ubit (fd);
    item->lcbPlcffldEdn = read_32ubit (fd);
    item->fcPlcfpgdEdn = (S32) read_32ubit (fd);
    item->lcbPlcfpgdEdn = read_32ubit (fd);

    read_32ubit (fd);
    read_32ubit (fd);

    item->fcSttbfRMark = (S32) read_32ubit (fd);
    item->lcbSttbfRMark = read_32ubit (fd);
    item->fcSttbCaption = (S32) read_32ubit (fd);
    item->lcbSttbCaption = read_32ubit (fd);
    item->fcSttbAutoCaption = (S32) read_32ubit (fd);
    item->lcbSttbAutoCaption = read_32ubit (fd);
    item->fcPlcfwkb = (S32) read_32ubit (fd);
    item->lcbPlcfwkb = read_32ubit (fd);

    read_32ubit (fd);
    read_32ubit (fd);


    item->fcPlcftxbxTxt = (S32) read_32ubit (fd);
    item->lcbPlcftxbxTxt = read_32ubit (fd);
    item->fcPlcffldTxbx = (S32) read_32ubit (fd);
    item->lcbPlcffldTxbx = read_32ubit (fd);
    item->fcPlcfhdrtxbxTxt = (S32) read_32ubit (fd);
    item->lcbPlcfhdrtxbxTxt = read_32ubit (fd);
    item->fcPlcffldHdrTxbx = (S32) read_32ubit (fd);
    item->lcbPlcffldHdrTxbx = read_32ubit (fd);
    item->fcStwUser = (S32) read_32ubit (fd);
    item->lcbStwUser = read_32ubit (fd);
    item->fcSttbttmbd = (S32) read_32ubit (fd);
    item->cbSttbttmbd = read_32ubit (fd);
    item->fcUnused = (S32) read_32ubit (fd);
    item->lcbUnused = read_32ubit (fd);
    item->fcPgdMother = (S32) read_32ubit (fd);
    item->lcbPgdMother = read_32ubit (fd);
    item->fcBkdMother = (S32) read_32ubit (fd);
    item->lcbBkdMother = read_32ubit (fd);
    item->fcPgdFtn = (S32) read_32ubit (fd);
    item->lcbPgdFtn = read_32ubit (fd);
    item->fcBkdFtn = (S32) read_32ubit (fd);
    item->lcbBkdFtn = read_32ubit (fd);
    item->fcPgdEdn = (S32) read_32ubit (fd);
    item->lcbPgdEdn = read_32ubit (fd);
    item->fcBkdEdn = (S32) read_32ubit (fd);
    item->lcbBkdEdn = read_32ubit (fd);
    item->fcSttbfIntlFld = (S32) read_32ubit (fd);
    item->lcbSttbfIntlFld = read_32ubit (fd);
    item->fcRouteSlip = (S32) read_32ubit (fd);
    item->lcbRouteSlip = read_32ubit (fd);
    item->fcSttbSavedBy = (S32) read_32ubit (fd);
    item->lcbSttbSavedBy = read_32ubit (fd);
    item->fcSttbFnm = (S32) read_32ubit (fd);
    item->lcbSttbFnm = read_32ubit (fd);
}
