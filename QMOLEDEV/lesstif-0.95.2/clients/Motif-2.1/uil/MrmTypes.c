/**
 *
 * $Id: MrmTypes.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright (C) 1995-2001 LessTif Development Team
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

#include <LTconfig.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <X11/Xlib.h>

#include "FakeWidget.h"
#include "glue.h"
#include "uil.h"
#include "main.h"

#include "MrmTypes.h"


#if 0
static Char8 *
Char8Add(Char8 *s1, Char8 *s2)
{
    Char8 *Return = s1;

    strcat(Return->lvalue, s2->lvalue);

    Return->theExpression.value = (long)Return->lvalue;

    return Return;
}

static int
AddrNameGetEvalValue(AddrName *this)
{
    ExpressionElement *t = ExpressionListFind(&LocalSymbolTable,
					  (char *)(this->theExpression.value));
    if (NULL == t)
    {
	t = ExpressionListFind(&GlobalSymbolTable,
			       (char *)this->theExpression.value);
    }

    if (NULL == t)
    {
	__MrmExit(LOC,
	     "Can't find %s in Symbol Tables\n", this->theExpression.value);
    }

    if (!ExpressionElementIsType(t, MrmRtypeInteger))
    {
	yyerror("Illegal type in expression");
    }

    return (int)ExpressionElementGetValue(t);
}
#endif

void
InheritItemEmit(InheritItem *this)
{
    fputc(this->theExpression.type, outFile);

    fwrite((char *)this->theExpression.value, 1,
	   strlen((char *)this->theExpression.value), outFile);

    fputc(0, outFile);
}

InheritItem *
InheritItemNew(char *s)
{
    InheritItem *this = (InheritItem *)malloc(sizeof(InheritItem));

    strcpy(this->lvalue, s);

    this->theExpression.value = (long)this->lvalue;
    this->theExpression.type = MrmRtypeCountedVector;
    this->theExpression.Emit = (PFI)InheritItemEmit;

    return this;
}

FontSet *
FontSetNew(void)
{
    FontSet *this = (FontSet *) malloc(sizeof(FontSet));

    this->theExpression.type = MrmRtypeFontSet;
    this->theExpression.value = (long)&this->fontset;
    this->fontset.DirectionRtoL = 0;
    this->fontset.WideChar = 0;
    this->fontset.name = NULL;

    return this;
}

#if 0
static char *
FontSetGetName(FontSet * this)
{
    return (this->fontset.name);
}
#endif

static void
FontSetEmit(FontSet * this)
{
    fputc(this->theExpression.type, outFile);

    fwrite(this->fontset.name, 1, strlen(this->fontset.name), outFile);

    fputc(0, outFile);

    fwrite(&this->fontset.DirectionRtoL, sizeof(this->fontset.DirectionRtoL),
	   1, outFile);

    fwrite(&this->fontset.WideChar, sizeof(this->fontset.WideChar), 1, outFile);
}

FontM *
FontMNew(char *font)
{
    FontM *this = (FontM *) malloc(sizeof(FontM));

    this->theExpression.value = (long)this->lvalue;

    strcpy(this->lvalue, font);

    this->theExpression.type = MrmRtypeFont;
    this->theExpression.Emit = (PFI)FontMEmit;

    return this;
}

void
FontMEmit(FontM * this)
{
    fputc(this->theExpression.type, outFile);

    fwrite(this->lvalue, 1, strlen(this->lvalue), outFile);

    fputc(0, outFile);
}

static void
FontElementEmit(FontElement * this)
{
    FontSetEmit(this->theFontSet);

    FontMEmit(this->theFont);
}

FontElement *
FontElementNew(FontSet * theFontSet, FontM * theFont)
{
    FontElement *this = (FontElement *) malloc(sizeof(FontElement));

    this->Next = NULL;
    this->theFontSet = theFontSet;
    this->theFont = theFont;

    return this;
}

void
FontTableAppend(FontTable * this, FontSet * theFontSet, FontM * theFont)
{
    FontElement **i;

    for (i = &this->FontVector; *i != NULL; i = &((*i)->Next));

    *i = FontElementNew(theFontSet, theFont);
}

static void
FontTableEmit(FontTable * this)
{
    FontElement **j;
    int size = 0;

    fputc(this->theExpression.type, outFile);

    for (j = &this->FontVector; *j != NULL; j = &((*j)->Next))
    {
	size++;
    }

    fwrite(&size, sizeof(size), 1, outFile);

    for (j = &this->FontVector; *j != NULL; j = &((*j)->Next))
    {
	FontElementEmit(*j);
    };
}

FontTable *
FontTableNew(void)
{
    FontTable *this = (FontTable *) malloc(sizeof(FontTable));

    this->theExpression.type = MrmRtypeFontList;
    this->FontVector = NULL;
    this->theExpression.value = (long)this->FontVector;
    this->theExpression.Emit = (PFI)FontTableEmit;

    return this;
}

static ColorDefinition *
ColorDefinitionNew(char *color, int r, int g, int b)
{
    ColorDefinition *this = (ColorDefinition *) malloc(sizeof(ColorDefinition));

    if (NULL != color)
    {
	this->name = __MrmStore(color);
    }
    else
    {
	this->name = NULL;
    }

    this->r = r;
    this->g = g;
    this->b = b;

    return this;
}

static void
ColorDefinitionEmit(ColorDefinition * this)
{
    if (this->name && this->name[0])
    {
	fputs(this->name, outFile);

	fputc(0, outFile);
    }
    else
    {
	fputc(0, outFile);

	fwrite(&this->r, sizeof(int), 1, outFile);
	fwrite(&this->g, sizeof(int), 1, outFile);
	fwrite(&this->b, sizeof(int), 1, outFile);
    }
}

Color *
ColorNew(char *color, int r, int g, int b)
{
    Color *this = (Color *)malloc(sizeof(Color));

    this->theDefinition = ColorDefinitionNew(color, r, g, b);
    this->theExpression.value = (long)&this->theDefinition;
    this->theExpression.type = MrmRtypeColor;
    this->theExpression.Emit = (PFI)ColorEmit;

    return this;
}

void
ColorSetAddress(Color *this)
{
    this->theExpression.type = MrmRtypeAddrName;
}

void
ColorEmit(Color *this)
{
    fputc(this->theExpression.type, outFile);

    ColorDefinitionEmit(this->theDefinition);
}

void
FontSetDirectionRtoL(FontSet * this, int d)
{
    this->fontset.DirectionRtoL = d;
}

void
FontSet16Bit(FontSet * this, int bits16)
{
    this->fontset.WideChar = bits16;
}

void
FontSetName(FontSet * this, char *name)
{
    this->fontset.name = __MrmStore(name);
}

XBitmapFile *
XBitmapFileNew(char *FileName)
{
    XBitmapFile *this = (XBitmapFile *)malloc(sizeof(XBitmapFile));

    this->theExpression.type = MrmRtypeXBitmapFile;
    this->theExpression.value = (long)&this->bitmap;

    __MrmReadBitmapFileData(FileName, &(this->bitmap.width), &(this->bitmap.height),
		       &(this->bitmap.data), &(this->bitmap.x_hot),
		       &(this->bitmap.y_hot));

    this->theExpression.Emit = (PFI)XBitmapFileEmit;

    return this;
}

void
XBitmapFileEmit(XBitmapFile *this)
{
    fputc(this->theExpression.type, outFile);

    fwrite((char *)this->theExpression.value,
	   sizeof(BitMapType) - sizeof(long), 1, outFile);

    fwrite(this->bitmap.data,
	   (this->bitmap.width * this->bitmap.height) >> 3, 1, outFile);
}

Keysym *
KeysymNew(char *s)
{
    Keysym *this = (Keysym *)malloc(sizeof(Keysym));

    strcpy(this->lvalue, s);

    this->theExpression.value = (long)this->lvalue;
    this->theExpression.type = MrmRtypeKeysym;
    this->theExpression.Emit = (PFI)KeysymEmit;

    return this;
}

void
KeysymEmit(Keysym *this)
{
    fputc(this->theExpression.type, outFile);

    fwrite(this->lvalue, 1, strlen(this->lvalue), outFile);

    fputc(0, outFile);
}

static ColorElement *
ColorElementNew(char *rep, Color *color)
{
    ColorElement *this = (ColorElement *)malloc(sizeof(ColorElement));

    this->Next = NULL;
    this->name = __MrmStore(rep);
    this->theColor = color;

    return this;
}

static void
ColorElementEmit(ColorElement *this)
{
    fputs(this->name, outFile);

    fputc(0, outFile);

    ColorEmit(this->theColor);
}

ColorTable *
ColorTableNew(void)
{
    ColorTable *this = (ColorTable *)malloc(sizeof(ColorTable));

    this->theExpression.type = MrmRtypeColorTable;
    this->ColorVector = NULL;
    this->theExpression.value = (long)this->ColorVector;
    this->theExpression.Emit = (PFI)ColorTableEmit;

    return this;
}

void
ColorTableAppend(ColorTable *this, char *Representation, Color *color)
{
    ColorElement **i;

    for (i = &this->ColorVector; *i != NULL; i = &((*i)->Next));

    *i = ColorElementNew(Representation, color);
}

void
ColorTableEmit(ColorTable *this)
{
    ColorElement **j;
    int size = 0;

    fputc(this->theExpression.type, outFile);

    for (j = &this->ColorVector; *j != NULL; j = &((*j)->Next))
    {
	size++;
    }

    fwrite(&size, sizeof(size), 1, outFile);

    for (j = &this->ColorVector; *j != NULL; j = &((*j)->Next))
    {
	ColorElementEmit(*j);
    };
}

Char8Vector *
Char8VectorNew(void)
{
    Char8Vector *this = (Char8Vector *)malloc(sizeof(Char8Vector));

    this->theExpression.type = MrmRtypeChar8Vector;
    this->CharVector = NULL;
    this->theExpression.value = (long)&this->CharVector;
    this->theExpression.Emit = (PFI)Char8VectorEmit;

    return this;
}

static Char8Element *
Char8ElementNew(char *s)
{
    Char8Element *this = (Char8Element *)malloc(sizeof(Char8Element));

    this->Next = NULL;
    if (strlen(s) > 255)	/* FIX ME */
    {
	__MrmExit(LOC, "String too long\n");
    }
    strcpy(this->lvalue, s);

    return this;
}

void
Char8VectorAppend(Char8Vector *this, char *s)
{
    Char8Element **j;

    for (j = &this->CharVector; *j != NULL; j = &((*j)->Next));

    *j = Char8ElementNew(s);
}

void
Char8VectorEmit(Char8Vector *this)
{
    Char8Element **j;

    fputc(this->theExpression.type, outFile);

    for (j = &this->CharVector; *j != NULL; j = &((*j)->Next))
    {
	fputs((*j)->lvalue, outFile);

	fputc('"', outFile);
    };

    fputc(0, outFile);
}

BooleanM *
BooleanMNew(Bool i)
{
    BooleanM *this = (BooleanM *)malloc(sizeof(BooleanM));

    this->theExpression.value = i;
    this->theExpression.type = MrmRtypeBoolean;
    this->theExpression.Emit = (PFI)BooleanMEmit;

    return this;
}

void
BooleanMEmit(BooleanM *this)
{
    fputc(this->theExpression.type, outFile);

    fwrite(&this->theExpression.value, sizeof(long), 1, outFile);
}

Integer *
IntegerNew(int i)
{
    Integer *this = (Integer *)malloc(sizeof(Integer));

    this->theExpression.value = (long)i;
    this->theExpression.type = MrmRtypeInteger;
    this->theExpression.Emit = (PFI)IntegerEmit;

    return this;
}

void
IntegerEmit(Integer *this)
{
    fputc(this->theExpression.type, outFile);

    fwrite(&this->theExpression.value, sizeof(long), 1, outFile);
}

#if 0
static int
IntegerGetEvalValue(Integer *this)
{
    return ((int)this->theExpression.value);
}
#endif

AddrName *
AddrNameNew(char *s)
{
    AddrName *this = (AddrName *)malloc(sizeof(AddrName));

    strcpy(this->lvalue, s);

    this->theExpression.value = (long)this->lvalue;
    this->theExpression.type = MrmRtypeAddrName;
    this->theExpression.Emit = (PFI)AddrNameEmit;

    return this;
}

void
AddrNameEmit(AddrName *this)
{
    fputc(this->theExpression.type, outFile);

    fputs((char *)this->theExpression.value, outFile);

    fputc(0, outFile);
}

static CStringElement *
CStringElementNew(char *s, char *fs,
		  int IsAddress, int IsSeparator)
{
    CStringElement *this = (CStringElement *) malloc(sizeof(CStringElement));

    this->Next = NULL;
    this->theFontSet = fs;
    this->IsAddress = IsAddress;
    this->IsSeparator = IsSeparator;

    strcpy(this->lvalue, s);

    return this;
}

static void
CStringElementEmit(CStringElement * this)
{
    fputs((char *)this->lvalue, outFile);

    fputc(0, outFile);

    if (this->theFontSet)
    {
	fputs((char *)this->theFontSet, outFile);
    }

    fputc(0, outFile);
    fputc(this->IsAddress, outFile);
    fputc(this->IsSeparator, outFile);
}


CString *
CStringNew(char *s, char *fs, char IsAddress, char IsSeparator)
{
    CString *this = (CString *)malloc(sizeof(CString));

    this->StringVector = CStringElementNew(s, fs, IsAddress, IsSeparator);
    this->theExpression.type = MrmRtypeCString;
    this->theExpression.value = (long)this->StringVector;
    this->theExpression.Emit = (PFI)CStringEmit;

    return this;
}

CString *
CStringNew1(AddrName *add)
{
    return CStringNew((char *)add->theExpression.value, NULL, 1, 0);
}

CString *
CStringAdd(CString *this, CString *s)
{
    CStringElement **j;

    for (j = &this->StringVector; *j != NULL; j = &((*j)->Next));

    *j = s->StringVector;

    return this;
}

void
CStringEmit(CString *this)
{
    CStringElement **j;

    fputc(this->theExpression.type, outFile);

    for (j = &this->StringVector; *j != NULL; j = &((*j)->Next))
    {
	CStringElementEmit(*j);
    };

    fputc(0, outFile);
}

Char8 *
Char8New1(char *s, FontSet * fs)
{
    Char8 *this = Char8New(s);

    this->theFontSet = fs;

    return this;
}

Char8 *
Char8New(char *s)
{
    Char8 *this = (Char8 *)malloc(sizeof(Char8));
    char *q = s;
    char *q1 = s;

    while (*q)
    {
	if ('\\' == *q)
	{
	    q++;
	    switch (*q)
	    {
	    case 'a':
		*q1++ = '\a';
		break;
	    case 'n':
		*q1++ = '\n';
		break;
	    case 'r':
		*q1++ = '\r';
		break;
	    case 't':
		*q1++ = '\t';
		break;
	    case '\\':
		*q1++ = '\\';
		break;
	    default:
		*q1++ = *q;
		break;
	    }
	    q++;
	}
	else
	{
	    *q1++ = *q++;
	}
    }

    *q1 = '\0';

    this->theExpression.type = MrmRtypeChar8;

    strcpy(this->lvalue, s);

    this->theExpression.value = (long)this->lvalue;
    this->theExpression.Emit = (PFI)Char8Emit;
    this->theFontSet = NULL;

    return this;
}

void
Char8Emit(Char8 *this)
{
    fputc(this->theExpression.type, outFile);

    fputs((char *)this->theExpression.value, outFile);

    fputc(0, outFile);

    if (this->theFontSet)
    {
	fputs((char *)this->theFontSet->fontset.name, outFile);
    }

    fputc(0, outFile);
}

PixmapImage *
PixmapImageNew(char *ColorMap, Char8Vector *pixmap)
{
    char *current = NULL, *source = NULL;
    Char8Element **i;
    PixmapImage *this = (PixmapImage *)malloc(sizeof(PixmapImage));

    this->theExpression.type = MrmRtypePixmapImage;
    this->thePixmap.height = 0;
    this->theData = NULL;

    if (ColorMap)
    {
	this->theColorMap = __MrmStore(ColorMap);
    }
    else
    {
	this->theColorMap = NULL;
    }

    this->thePixmap.width = strlen(pixmap->CharVector->lvalue);

    for (i = &pixmap->CharVector; *i != NULL; i = &((*i)->Next))
    {
	this->thePixmap.height += 1;
    }

    current = this->theData =
	(char *)malloc(this->thePixmap.height * this->thePixmap.width);

    for (i = &pixmap->CharVector; *i != NULL; i = &((*i)->Next))
    {
	source = (*i)->lvalue;
	if (NULL == source)
	{
	    __MrmExit(LOC, "Source?\n");
	}

	while (*source)
	{
	    *current++ = *source++;
	}
    }

    this->thePixmap.data = this->theData;
    this->theExpression.value = (long)&this->thePixmap;
    this->theExpression.Emit = (PFI)PixmapImageEmit;

    return this;
}

void
PixmapImageEmit(PixmapImage *this)
{
    fputc(this->theExpression.type, outFile);

    fwrite((char *)this->theExpression.value, 1, sizeof(int) * 2, outFile);

    if (this->theColorMap)
    {
	fputs(this->theColorMap, outFile);
    }

    fputc(0, outFile);

    fwrite(this->thePixmap.data, 1,
	   this->thePixmap.width * this->thePixmap.height, outFile);
}
