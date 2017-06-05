/**
 *
 * $Id: MrmTypes.h,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
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
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/

#ifndef _MRMTYPES_H
#define _MRMTYPES_H

#include "lookup.h"

typedef struct _FontSet
{
    ExpressionType theExpression;
    FontSetType fontset;
}
FontSet;

typedef struct _InheritItem
{
    ExpressionType theExpression;
    char lvalue[256];
}
InheritItem;

typedef struct _AddrName
{
    ExpressionType theExpression;
    char lvalue[256];
}
AddrName;

typedef struct _Char8
{
    ExpressionType theExpression;
    char lvalue[256];
    FontSet *theFontSet;
}
Char8;

typedef struct _Char8Element
{
    struct _Char8Element *Next;
    char lvalue[256];
}
Char8Element;

typedef struct _Char8Vector
{
    ExpressionType theExpression;
    Char8Element *CharVector;
}
Char8Vector;

typedef struct _FontM
{
    ExpressionType theExpression;
    char lvalue[256];
}
FontM;

typedef struct _FontElement
{
    struct _FontElement *Next;
    FontM *theFont;
    FontSet *theFontSet;
}
FontElement;

typedef struct _FontTable
{
    ExpressionType theExpression;
    FontElement *FontVector;
}
FontTable;

typedef struct _ColorDefinition
{
    char *name;
    int r;
    int g;
    int b;
}
ColorDefinition;

typedef struct _Color
{
    ExpressionType theExpression;
    ColorDefinition *theDefinition;
}
Color;

typedef struct _ColorElement
{
    struct _ColorElement *Next;
    char *name;
    Color *theColor;
}
ColorElement;

typedef struct _ColorTable
{
    ExpressionType theExpression;
    ColorElement *ColorVector;
}
ColorTable;

typedef struct _XBitmapFile
{
    ExpressionType theExpression;
    BitMapType bitmap;
}
XBitmapFile;

typedef struct _BooleanM
{
    ExpressionType theExpression;
}
BooleanM;

typedef struct _Integer
{
    ExpressionType theExpression;
}
Integer;

typedef struct _Keysym
{
    ExpressionType theExpression;
    char lvalue[256];
}
Keysym;

typedef struct _CStringElement
{
    struct _CStringElement *Next;
    char *theFontSet;
    char lvalue[256];
    char IsAddress;
    char IsSeparator;
}
CStringElement;

typedef struct _CString
{
    ExpressionType theExpression;
    CStringElement *StringVector;
}
CString;

typedef struct _PixmapImage
{
    ExpressionType theExpression;
    char *theData;
    char *theColorMap;
    PixmapType thePixmap;
}
PixmapImage;

Char8Vector *Char8VectorNew(void);
ColorTable *ColorTableNew(void);
InheritItem *InheritItemNew(char *name);
CString *CStringNew(char *s, char *fs, char IsAddress, char IsSeparator);
CString *CStringNew1(AddrName *add);
Char8 *Char8New(char *string);
Char8 *Char8New1(char *string, FontSet * fs);
Integer *IntegerNew(int i);
BooleanM *BooleanMNew(Bool s);
AddrName *AddrNameNew(char *name);
PixmapImage *PixmapImageNew(char *name, Char8Vector *data);
Keysym *KeysymNew(char *sym);
Color *ColorNew(char *name, int r, int g, int b);
FontM *FontMNew(char *name);
XBitmapFile *XBitmapFileNew(char *name);

void FontMEmit(FontM * this);
FontElement *FontElementNew(FontSet * theFontSet, FontM * theFont);
void ColorEmit(Color *this);
void ColorSetAddress(Color *this);
void XBitmapFileEmit(XBitmapFile *this);
void KeysymEmit(Keysym *this);
void ColorTableEmit(ColorTable *this);
void ColorTableAppend(ColorTable *this, char *Representation, Color *color);
void Char8VectorEmit(Char8Vector *this);
void Char8VectorAppend(Char8Vector *this, char *s);
void BooleanMEmit(BooleanM *this);
void IntegerEmit(Integer *this);
void AddrNameEmit(AddrName *this);
void CStringEmit(CString *this);
void Char8Emit(Char8 *this);
void PixmapImageEmit(PixmapImage *this);
void InheritItemEmit(InheritItem *this);
FontSet *FontSetNew(void);
void FontSetDirectionRtoL(FontSet * this, int d);
void FontSet16Bit(FontSet * this, int bits16);
void FontSetName(FontSet * this, char *name);
FontTable *FontTableNew(void);
void FontTableAppend(FontTable * this, FontSet * theFontSet, FontM * theFont);

CString *CStringAdd(CString *this, CString *s);

#endif /* #ifndef _MRMTYPES_H */
