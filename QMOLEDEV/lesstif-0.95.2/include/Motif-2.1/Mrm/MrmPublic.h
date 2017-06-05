/**
 *
 * $Id: MrmPublic.h,v 1.1 2004/08/28 19:23:24 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2000 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

#ifndef _MRM_MRMPUBLIC_H
#define _MRM_MRMPUBLIC_H

#include <X11/Intrinsic.h>

#ifdef __cplusplus
extern "C" {
#endif

/* result codes */
enum {
    MrmSUCCESS			= 1,
    MrmCREATE_NEW		= 3,
    MrmINDEX_RETRY		= 5,
    MrmINDEX_GT			= 7,
    MrmINDEX_LT			= 9,
    MrmPARTIAL_SUCCESS		= 11,

    MrmFAILURE			= 0,
    MrmNOT_FOUND		= 2,
    MrmEXISTS			= 4,
    MrmNUL_GROUP		= 6,
    MrmNUL_TYPE			= 8,
    MrmWRONG_GROUP		= 10,
    MrmWRONG_TYPE		= 12,
    MrmOUT_OF_RANGE		= 14,
    MrmBAD_RECORD		= 16,
    MrmNULL_DATA		= 18,
    MrmBAD_DATA_INDEX		= 20,
    MrmBAD_ORDER		= 22,
    MrmBAD_CONTEXT		= 24,
    MrmNOT_VALID		= 26,
    MrmBAD_BTREE		= 28,
    MrmBAD_WIDGET_REC		= 30,
    MrmBAD_CLASS_TYPE		= 32,
    MrmNO_CLASS_NAME		= 34,
    MrmTOO_MANY			= 36,
    MrmBAD_IF_MODULE		= 38,
    MrmNULL_DESC		= 40,
    MrmOUT_OF_BOUNDS		= 42,
    MrmBAD_COMPRESS		= 44,
    MrmBAD_ARG_TYPE		= 46,
    MrmNOT_IMP			= 48,
    MrmNULL_INDEX		= 50,
    MrmBAD_KEY_TYPE		= 52,
    MrmBAD_CALLBACK		= 54,
    MrmNULL_ROUTINE		= 56,
    MrmVEC_TOO_BIG		= 58,
    MrmBAD_HIERARCHY		= 60,
    MrmBAD_CLASS_CODE		= 62,
    MrmDISPLAY_NOT_OPENED	= 63,
    MrmEOF			= 64,
    MrmUNRESOLVED_REFS		= 65
};

#define MrmNcreateCallback	"createCallback"
#define MrmCR_CREATE		XmCR_CREATE
#define MrmwcUnknown		1

/* mrm arguments */
typedef struct {
    String name;
    XtPointer value;
} MRMRegisterArg, MrmRegisterArg, *MrmRegisterArglist;

/* some Mrm types... don't know what we need here -- chris */
typedef enum {
    /* 1 */	MrmRtypeInteger		= 1,
    /* 2 */	MrmRtypeBoolean,
    /* 3 */	MrmRtypeChar8,
    /* 4 */	MrmRtypeChar8Vector,
    /* 5 */	MrmRtypeCString,
    /* 6 */	MrmRtypeCStringVector,
    /* 7 */	MrmRtypeFloat,
    /* 8 */	MrmRtype_8,		/* not used */
    /* 9 */	MrmRtypeCallback,
    /* 10 */	MrmRtypePixmapImage,
    /* 11 */	MrmRtypePixmapDDIF,
    /* 12 */	MrmRtypeResource,
    /* 13 */	MrmRtypeNull,
    /* 14 */	MrmRtypeAddrName,
    /* 15 */	MrmRtypeIconImage,
    /* 16 */	MrmRtypeFont,
    /* 17 */	MrmRtypeFontList,
    /* 18 */	MrmRtypeColor,
    /* 19 */	MrmRtypeColorTable,
    /* 20 */	MrmRtypeAny,
    /* 21 */	MrmRtypeTransTable,
    /* 22 */	MrmRtypeClassRecName,
    /* 23 */	MrmRtypeIntegerVector,
    /* 24 */	MrmRtypeXBitmapFile,
    /* 25 */	MrmRtypeCountedVector,
    /* 26 */	MrmRtypeKeysym,
    /* 27 */	MrmRtypeSingleFloat,
    /* 28 */	MrmRtypeWideCharacter,
    /* 29 */	MrmRtypeFontSet,
    /* 30 */	MrmRtypeMax
/* end */
} MrmCode;

/* Counterpart for MrmRtypeMax : */
#define	MrmRtypeMin	1

typedef unsigned char MrmSCode;
typedef unsigned short int MrmOffset;
typedef unsigned short int MrmSize;
typedef short int MrmCount;
typedef unsigned char MrmFlag;
typedef long int MrmResourceId;
typedef short int MrmGroup;

typedef enum {
    /* user defined type */
    MrmWCunknown,
    
    /* gadgets */
    MrmWCArrowButtonGadget,
    MrmWCLabelGadget,
    MrmWCCascadeButtonGadget,
    MrmWCPushButtonGadget,
    MrmWCToggleButtonGadget,
    MrmWCSeparatorGadget,

    /* primitives */
    MrmWCArrowButton,
    MrmWCLabel,
    MrmWCCascadeButton,
    MrmWCDrawnButton,
    MrmWCPushButton,
    MrmWCToggleButton,
    MrmWCList,
    MrmWCScrollbar,
    MrmWCSeparator,
    MrmWCText,
    MrmWCTextField,

    /* managers */
    MrmWCBulletinBoard,
    MrmWCForm,
    MrmWCSelectionBox,
    MrmWCCommand,
    MrmWCFileSelectionBox,
    MrmWCMessageBox,
    MrmWCDrawingArea,
    MrmWCFrame,
    MrmWCPanedWindow,
    MrmWCRowColumn,
    MrmWCScale,
    MrmScrolledWindow,
    MrmWCMainWindow,
    
    /* shells */
    MrmWCMenuShell,
    MrmWCDialogShell
} MrmType;

#define URMwcUnknown    1

typedef int *MrmOsOpenParamPtr; /* don't need this.. Motif is dumb */
typedef struct MrmHierarchyStruct *MrmHierarchy;

#ifdef __cplusplus
};
#endif

#include <Mrm/MrmDecls.h>

#endif /* _MRM_MRMPUBLIC_H */
