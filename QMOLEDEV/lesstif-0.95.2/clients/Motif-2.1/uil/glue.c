/**
 *
 * $Id: glue.c,v 1.1 2004/08/28 19:25:46 dannybackx Exp $
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
#include "MrmTypes.h"
#include "main.h"


static void Check(void);

#define StripQuotes(s) {s++;s[strlen(s)-1] = 0;}


/***
 ***        c is a child widget of the widget currently being defind in w.
 ***        c may not be already defined but space has been allocated for it
 ***        already and c points to that space.  This function must put that
 ***        pointer in w's list of children and set c's parent.
 ***/
void
features_controls(char *w, char *c)
{
    FakeWidgetList *children = (FakeWidgetList *)c;
    FakeWidgetType *wid = (FakeWidgetType *)w;
    FakeWidgetList *widChild = FakeWidgetTypeKids(wid);

    if (NULL == c)
    {
	return;
    }

    FakeWidgetListAppendLists(widChild, children);
    FakeWidgetListSetParents(widChild, wid);
}

/*
 *  a is list of attributes that describe the widget being created
 *  in w.  'a' needs to be appended to the attribute list that w
 *  has aquired so far.
 */
void
features_arguments(char *w, char *a)
{
    FakeWidgetType *wid = ((FakeWidgetType *)w);
    AttributeList *widAtts = FakeWidgetTypeAtts(wid);
    AttributeList *al = (AttributeList *)a;

    if (NULL == a)
    {
	return;
    }

    AttributeListAppendLists(widAtts, al);
}

void
features_callbacks(char *w, char *c)
{
    CallbackList *cl = (CallbackList *)c;
    FakeWidgetType *wid = ((FakeWidgetType *)w);
    CallbackList *widCalls = FakeWidgetTypeCalls(wid);

    if (NULL == cl)
    {
	cl = CallbackListNew();
    }

    CallbackListAppendLists(widCalls, cl);
}

/*
 * This is the bottom of the line,  Create space for a widget
 * to be defined as we move back up the stack.  It may turn out
 * that space the widget we are defining here has already been
 * forward referced.  In that case, this widget will be copied to
 * that one and destroyed.      
 */
char *
Features_NULL(void)
{
    FakeWidgetType *wid = FakeWidgetTypeNew();

    return (char *)wid;
}

char *
InheritControls(char *w, char *name)
{
    FakeWidgetList *wil = (FakeWidgetList *)w;
    FakeWidgetType *wid;

    wid = FakeWidgetTypeNew2(name, "Inherited", 0);
    if (NULL == wil)
    {
	wil = FakeWidgetListNew(NULL);
    }

    FakeWidgetListAppend(wil, wid);
    return (char *)wil;
}
/*
 * We are referencing a widget that might not exist yet.
 * Look up the widget in the symbol table (Widget List)
 * and return its pointer if it is found. If it doesn't exit
 * Create the symbol table entry and return a pointer to it.
 * The WidgetList being built will be the list of children
 * of a Widget being built.
 */
char *
controllist_controllist_ID_ID(char *w, char *theClass, char *name, char managed)
{
    FakeWidgetList *wil = (FakeWidgetList *)w;
    FakeWidgetType *wid;

    wid = FakeWidgetTypeNew2(name, theClass, managed);
    if (NULL == wil)
    {
	wil = FakeWidgetListNew(NULL);
    }

    FakeWidgetListUpdate(&WidgetTable, &wid);
    FakeWidgetListAppend(wil, wid);

    return (char *)wil;
}

char *
control_list_ID_features(char *w, char *theType, char *wid, char managed)
{
    FakeWidgetList *wil = (FakeWidgetList *)w;
    FakeWidgetType *widget = (FakeWidgetType *)wid;

    if (NULL == wil)
    {
	wil = FakeWidgetListNew(NULL);
    }

    FakeWidgetTypeSetName(widget, NULL, theType);
    if (!managed)
    {
	FakeWidgetTypeUnmanage(widget);
    }

    FakeWidgetListUpdate(&WidgetTable, &widget);
    FakeWidgetListAppend(wil, widget);
    return (char *)wil;
}

/*
 * We are building the attribute list for the widget currently
 * being built.  Each new attribute is created here.  Its name
 * is passed as 'n' and its value is in expr passed through 'e'.
 * The attribute should be added to the list in 'a' after its 
 * creation.
 */
char *
arglist_arglist_ID_addexpr(char *a, char *name, char *e)
{
    AttributeList *list = (AttributeList *)a;
    ExpressionType *expr = (ExpressionType *)e;

    if (NULL == list)
    {
	list = AttributeListNew();
    }
    if (expr)
    {
	AttributeListAppend(list, ExpressionElementNew1(name, expr));
    }

    return (char *)list;
}

char *
Parameter(char *pl, char *e)
{
    ParameterList *list = (ParameterList *)pl;
    ExpressionType *expr = (ExpressionType *)e;
    ParameterElement *elem = ParameterElementNew1(expr);

    if (NULL == list)
    {
	list = ParameterListNew();
    }

    if (expr)
    {
	ParameterListAppend(list, elem);
    }

    return (char *)list;
}

char *
WidgetArgument(char *a, char *name, char *widName)
{
    AttributeList *list = (AttributeList *)a;
    ExpressionType *expr = (ExpressionType *)AddrNameNew(widName);
    ExpressionElement *elem = ExpressionElementNew1(name, expr);

    if (NULL == list)
    {
	list = AttributeListNew();
    }

    AttributeListAppend(list, elem);

    return (char *)list;
}

char *
InsertString(char *sl, char *s)
{
    char temp[256];
    Char8Vector *StringList = (Char8Vector *)sl;

    if (NULL == StringList)
    {
	StringList = Char8VectorNew();
    }

    if ('"' == s[0])
    {
	StripQuotes(s);
    }
    else
    {
	strcpy(&temp[1], s);
	temp[0] = '~';
	s = temp;
    }

    Char8VectorAppend(StringList, __MrmStore(s));

    return (char *)StringList;
}

char *
InheritCallback(char *name)
{
    CallbackType *callback = CallbackTypeNew("Inherited", name, NULL);
    CallbackList *cb = CallbackListNew();

    CallbackListAppend(cb, callback);

    return (char *)cb;
}

char *
InheritArgument(char *a, char *name)
{
    AttributeList *list = (AttributeList *)a;
    ExpressionType *Expr = (ExpressionType *)InheritItemNew(name);
    ExpressionElement *Elem = ExpressionElementNew1(name, Expr);

    if (NULL == list)
    {
	list = AttributeListNew();
    }

    AttributeListAppend(list, Elem);

    return (char *)list;
}

char *
expr_STRING_Compound(char *st, int Separate, int IsAddress)
{
    if (!IsAddress)
    {
	StripQuotes(st);
    }

    return (char *)CStringNew(st, NULL, IsAddress, Separate);
}

/*
 * The rvalue of an equation is a string.  Set values and return.
 */
char *
expr_STRING(char *st, char *cs, int CompoundString)
{
    char s[256];
    int i;
    FontSet *fs = (FontSet *) cs;
    char *FontName = NULL;

    if (fs)
    {
	FontName = fs->fontset.name;
    }

    for (i = 0; *st; i++)
    {
	if ('\\' == *st)
	{
	    st++;
	    while (*st && isdigit(*st))
	    {
		s[i] = s[i] * 10 + *st++ - '0';
	    }

	    if (*st != '\\')
	    {
		__MrmExit(LOC, "String ERROR\n");
	    }
	    st++;
	}
	else
	{
	    s[i] = *st++;
	}
    }

    s[i] = 0;
    st = s;

    if (CompoundString)
    {
	return (char *)CStringNew(st, FontName, 0, 0);
    }
    else
    {
	StripQuotes(st);

	return (char *)CStringNew(st, FontName, 0, 0);
    }
}

/*
 * The rvalue of an equation is a integer.  Set values and return.
 */
char *
prim_exp(char *s)
{
    return (char *)IntegerNew(atoi(s));
}

char *
expr_BOOL(char *s)
{
    return (char *)BooleanMNew((Bool)(long)s);
}

char *
expr_ID(char *s)
{
    return (char *)AddrNameNew(s);
}

/*
 * We are building a complex string.  Just push the
 * strings passed in and return a pointer to the stack.
 */
static char StackString[1000];

char *
string_push(char *s)
{
    strcat(StackString, s);

    return StackString;
}

void
string_clear(void)
{
    StackString[0] = 0;
}

/*
 * Here we are generating the list of callbacks in 'l'
 * for the widget that is currently being built.
 */
char *
callbacklist_callbacklist_PROCID_arglist(char *l, char *id,
					 char *fn, char *parm)
{
    CallbackList *list = (CallbackList *)l;
    ParameterList *Parameters = (ParameterList *)parm;

    if (NULL == list)
    {
	list = CallbackListNew();
    }

    CallbackListAppend(list, CallbackTypeNew(id, fn, Parameters));

    return (char *)list;
}

/*
 * The widget 'w' has just been built.  Add it to the list
 * 'wl' and to the symbol table.
 */
char *
body_OBJECT_object(char *wl, char *w)
{
    FakeWidgetType *wid = (FakeWidgetType *)w;
    FakeWidgetList *list = (FakeWidgetList *)wl;

    if (NULL == list)
    {
	list = FakeWidgetListNew(NULL);
    }

    if (0 == strcmp(wid->theName, "layoutRTForm"))
    {
	fprintf(stderr, "HERE\n");
    }

    FakeWidgetListUpdate(&WidgetTable, &wid);
    FakeWidgetListAppend(list, wid);

    return (char *)list;
}

void 
AddCallbackList(char *name, char *c)
{
    CallbackList *Callbacks = (CallbackList *)c;
    FakeWidgetType *wid = FakeWidgetTypeNew1(name, "list");
    CallbackList *calls = (CallbackList *)wid->Callbacks;

    if (NULL != Callbacks)
    {
	CallbackListAppendLists(calls, Callbacks);
    }

    FakeWidgetListUpdate(&WidgetTable, &wid);
}

void 
AddControlList(char *name, char *c)
{
    FakeWidgetList *children = (FakeWidgetList *)c;
    FakeWidgetType *wid = FakeWidgetTypeNew1(name, "list");
    FakeWidgetList *kids = FakeWidgetTypeKids(wid);

    if (NULL != children)
    {
	FakeWidgetListAppendLists(kids, children);
    }

    FakeWidgetListUpdate(&WidgetTable, &wid);
}

void
AddAttributeList(char *name, char *a)
{
    AttributeList *Attributes = (AttributeList *)a;
    FakeWidgetType *wid = FakeWidgetTypeNew1(name, "list");
    AttributeList *atts = FakeWidgetTypeAtts(wid);

    AttributeListAppendLists(atts, Attributes);
    FakeWidgetListUpdate(&WidgetTable, &wid);
}

void
ID_ID_features(char *name, char *theClass, char *w)
{
    FakeWidgetType *wid = (FakeWidgetType *)w;

    FakeWidgetTypeSetName(wid, name, theClass);
}

void 
MakeTable(char *VariableName, char *Ex, int global)
{
    ExpressionType *Expression = (ExpressionType *)Ex;

    if (global)
    {
	ExpressionListAppend(&GlobalSymbolTable,
			     ExpressionElementNew1(VariableName, Expression));
    }
    else
    {
	ExpressionListAppend(&LocalSymbolTable,
			     ExpressionElementNew1(VariableName, Expression));
    }
}

char *
pixmap(char *n, char *s)
{
    Char8Vector *strings = (Char8Vector *)s;

    return (char *)PixmapImageNew(n, strings);
}

char *
AddFont(char *fs, char *f, char *l)
{
    FontM *font;
    FontSet *theFontSet = (FontSet *) fs;
    FontTable *list = (FontTable *) l;

    font = FontMNew(f);
    if (NULL == list)
    {
	list = FontTableNew();
    }

    FontTableAppend(list, theFontSet, font);

    return (char *)list;
}

char *
AddColor(char *c, char *representation, char *l, int IsAddressd)
{
    Color *color;
    ColorTable *list = (ColorTable *)l;
    int IsAddress = 1;

    if ('"' == c[0])
    {
	StripQuotes(c);
	IsAddress = 0;
    }

    StripQuotes(representation);
    color = ColorNew(c, 0, 0, 0);

    if (IsAddress)
    {
	ColorSetAddress(color);
    }

    if (NULL == list)
    {
	list = ColorTableNew();
    }

    ColorTableAppend(list, representation, color);

    return (char *)list;
}

char *
color(char *s, char *r, char *g, char *b)
{
    if (s && s[0])
    {
	if ('"' == s[0])
	{
	    StripQuotes(s);
	}

	return (char *)ColorNew(s, 0, 0, 0);
    }
    else
    {
	return (char *)ColorNew(NULL, atoi(r), atoi(g), atoi(b));
    }
}

char *
font(char *s)
{
    if ('"' == s[0])
    {
	StripQuotes(s);
    }

    return (char *)FontMNew(s);
}

char *
keysym(char *s)
{
    char temp[256];

    if ('"' == s[0])
    {
	StripQuotes(s);
    }
    else
    {
	temp[0] = '~';
	strcpy(&temp[1], s);
	s = temp;
    }

    return (char *)KeysymNew(s);
}

char *
bitmap(char *s)
{
    StripQuotes(s);

    return (char *)XBitmapFileNew(s);
}

char *
MakeNewCharSet(void)
{
    return (char *)FontSetNew();
}

char *
CharSetRToL(char *cs, int d)
{
    FontSet *CharSet = (FontSet *) cs;

    FontSetDirectionRtoL(CharSet, d);

    return cs;
}

char *
CharSet16Bit(char *cs, int bits16)
{
    FontSet *CharSet = (FontSet *) cs;

    FontSet16Bit(CharSet, bits16);

    return cs;
}

char *
CharSetName(char *cs, char *name)
{
    FontSet *CharSet;

    if (NULL == cs)
    {
	CharSet = FontSetNew();
    }
    else
    {
	CharSet = (FontSet *) cs;
    }

    FontSetName(CharSet, name);

    return (char *)CharSet;
}

char *
AppendStrings(char *p1, char *p2)
{
    CString *param1 = (CString *)p1;
    CString *param2 = (CString *)p2;

    if (param1->theExpression.type == MrmRtypeAddrName)
    {
	param1 = CStringNew1((AddrName *)p1);
    }

    if (param2->theExpression.type == MrmRtypeAddrName)
    {
	param2 = CStringNew1((AddrName *)p2);
    }

    CStringAdd(param1, param2);

    return (char *)param1;
}

char *
Add(char *p1, char *p2)
{
    ExpressionType *param1 = (ExpressionType *)p1;
    ExpressionType *param2 = (ExpressionType *)p2;

    ExpressionTypeAdd(param1, param2);
    ExpressionTypeDelete(param2);

    return (char *)param1;
}

char *
Subtract(char *p1, char *p2)
{
    ExpressionType *param1 = (ExpressionType *)p1;
    ExpressionType *param2 = (ExpressionType *)p2;

    ExpressionTypeSubtract(param1, param2);
    ExpressionTypeDelete(param2);

    return (char *)param1;
}

char *
Multiply(char *p1, char *p2)
{
    ExpressionType *param1 = (ExpressionType *)p1;
    ExpressionType *param2 = (ExpressionType *)p2;

    ExpressionTypeMultiply(param1, param2);
    ExpressionTypeDelete(param2);

    return (char *)param1;
}

char *
Divide(char *p1, char *p2)
{
    ExpressionType *param1 = (ExpressionType *)p1;
    ExpressionType *param2 = (ExpressionType *)p2;

    ExpressionTypeDivide(param1, param2);
    ExpressionTypeDelete(param2);

    return (char *)param1;

}

static void 
Check(void)
{
    int a;

    if (WidgetTable.theList)
    {
	if (WidgetTable.theList->Next)
	{
	    if (WidgetTable.theList->Next->theWidget)
	    {
		if (WidgetTable.theList->Next->theWidget->theName)
		{
		    a = strcmp("titleArgs",
			       WidgetTable.theList->Next->theWidget->theName);

		    if (a == 0)
		    {
			fprintf(stderr, "title Arg OK\n");

			return;
		    }
		}
	    }
	}
    }

    fprintf(stderr, "title Arg NOT OK\n");
}
