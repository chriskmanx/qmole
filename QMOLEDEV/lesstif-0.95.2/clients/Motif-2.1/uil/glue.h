/**
 *
 * $Id: glue.h,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
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


#ifndef _GLUE_H
#define _GLUE_H

#include "misc.h"

extern void SaveTop(char *);
extern char *body_OBJECT_object(char *, char *);
extern void features_arguments(char *, char *);
extern char *Features_NULL(void);
extern char *controllist_controllist_ID_ID(char *, char *, char *, char);
extern char *control_list_ID_features(char *, char *, char *, char);
extern void ID_ID_features(char *, char *, char *);
extern void features_controls(char *, char *);
extern void features_callbacks(char *, char *);
extern char *Store(char *);
extern char *arglist_arglist_ID_addexpr(char *, char *, char *);
extern char *expr_STRING(char *, char *, int);
extern char *prim_exp(char *);
extern char *expr_BOOL(char *);
extern char *keysym(char *s);
extern char *font(char *s);
extern char *bitmap(char *s);
extern char *pixmap(char *n, char *s);
extern char *expr_ID(char *);
extern char *compoundstring_csSTRING(char *, char *, char *);
extern char *compoundstring_csCS(char *, char *, char *);
extern char *callbacklist_callbacklist_PROCID_arglist(char *, char *, char *, char *);
char *InheritArgument(char *, char *);
char *InheritControls(char *, char *);
char *InheritCallback(char *);
void AddControlList(char *name, char *c);
void AddCallbackList(char *name, char *c);
void AddAttributeList(char *, char *);
char *InsertString(char *, char *);
void MakeTable(char *, char *, int);
char *AppendStrings(char *, char *);
char *Add(char *, char *);
char *Subtract(char *, char *);
char *Multiply(char *, char *);
char *Divide(char *, char *);
char *WidgetArgument(char *, char *, char *);
char *Parameter(char *, char *);
char *color(char *, char *, char *, char *);
char *AddColor(char *, char *, char *, int);
void MakeColorTable(char *);
char *CharSetName(char *cs, char *name);
char *CharSet16Bit(char *cs, int bits16);
char *CharSetRToL(char *cs, int d);
char *MakeNewCharSet(void);
char *expr_STRING_Compound(char *st, int Separate, int IsAddress);
char *AddFont(char *fs, char *f, char *l);

extern int yyparse(void);

char *string_push(char *);
void string_clear(void);

#endif /* #ifndef _GLUE_H */
