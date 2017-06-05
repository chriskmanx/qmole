/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Mrm-2.1/Mrm.c,v 1.2 2004/11/30 17:37:16 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2002, 2004 LessTif Development Team
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
#include <string.h>
#include <stdio.h>

#include <X11/Xlib.h>
#include <Xm/XmP.h>
#include <XmI/XpmI.h>

#include <Mrm/MrmPublic.h>

#include "Mrm.h"
#include "uil.h"
#include "lookup.h"
#include "misc.h"

#include <XmI/DebugUtil.h>

#ifndef X_LIB_PATH
#define X_LIB_PATH "/usr/X11/lib"
#endif

#ifndef LESSTIFHOME
#define LESSTIFHOME "/usr/local"
#endif

/*
 * Macros and types
 */
#define MAX_ARGS 40
#define MAX_BUFFER 2048
#define MAX_HIER 20

#define MAX_DEF 2048
#define MAX_STR 256

enum
{
    GEO, LATE, ON_CREATE
};
#define ANOTHER_WIDGET -1


typedef struct _MyHierStruct
{
    char *CreateName;
    int CreateClass;
    Arg *args;
    int argCount;
    int SizeArgs;
} MyHierStruct;

typedef struct _SymbolTable
{
    struct _SymbolTable *Next;
    int type;
    int Index;
    char *Name;
    char *value;
    char Access;
} SymbolTableType;

typedef struct _callbackType
{
    struct _callbackType *next;
    int id;
    int index;
    char *argName;
    char *functionName;
    SymbolTableType *Parameters;
}
callbackType;

typedef struct _DummyWidget
{
    char *Name;
    int wclass;
    char managed;
    int parent;
    int NumberOfInherit;
    int *inherit;
    int NumberOfChildren;
    int *children;
    callbackType *callbacks;
    SymbolTableType *attributes;
}
DummyWidget;

typedef struct _UilModuleType
{
    char **Definitions;
    char **Names;
    int NumberOfWidgets;
    Widget *WidgetArray;
    DummyWidget *DummyWidgetArray;
    SymbolTableType *SymbolTable;
}
UilModuleType;

typedef struct _GeoType
{
    int module_id;
    int this_id;
    struct _GeoType *next;
    DummyWidget *DW;
}
GeoType;

typedef struct _ColorTableType
{
    char *Reference;
    int IsAddress;
    char *Color;
    char *StringXPM;
}
ColorTableType;

typedef struct _ColorTableHeaderType
{
    int NumColors;
    ColorTableType *ColorTable;
}
ColorTableHeaderType;

typedef struct _FontTableType
{
    FontSetType Tag;
    char *FontName;
}
FontTableType;

typedef struct _FontTableHeaderType
{
    int NumFonts;
    FontTableType *FontTable;
}
FontTableHeaderType;

/*
 * Variables
 */
static Boolean init = False;
static Display *display = NULL;
static Colormap colormap;
static Screen *theScreen;
static Window window;
static Widget parent;
static char *theName;
static char *theClass;
static GeoType *GeometryList = NULL;
static SymbolTableType *GlobalSymbolTable = NULL;
static int NumberFiles;
static String *FileList;
static int MaxAttributes;
static int MaxHier;
static UilModuleType UilModule[MAX_HIER];
static int FetchCount = 0;
static SymbolTableType *InstantationSymbolTable = NULL;
static char *uidpath;


/* exported stuff */
int NumberRegisteredFunctions = 0;
MrmRegisterArg *RegisteredFunctions = NULL;

/*
 * Forward declarations of functions
 */
static int GetSymbolTableValue(XtArgVal *, SymbolTableType *, int, SymbolTableType *);
static SymbolTableType *GetSymbolTable(SymbolTableType *);
static SymbolTableType *LookupValue(SymbolTableType *SymbolTable, char *name);
static FILE *UILOpen(char *FileName, char *mode, FILE * old);
static char *Substitute(char *buffer);
static void ReadWidgetTree(UilModuleType *Module);
static void PrintWidgetTree(UilModuleType *Module);
static void PrintWidgets(UilModuleType *Module);
static void MakePixmap(Display *d, Pixmap *pixmap, PixmapType * data, Pixel fg, Pixel bg);
static void FreeSymbolTable(SymbolTableType *t);
static void GetCallbacks(int filenr, int wid_id, DummyWidget *DW);
static void AddGeometryAttributes(GeoType **GeometryList, int module_id, int this_id,
			   DummyWidget *DW);
static void SetGeometryAttributes(GeoType **GeometryList);
static void ReadList(UilModuleType *Module);
static void ReadWidgets(UilModuleType *Module);
static void PrintArgs(SymbolTableType *Next);
static void PrintControls(int count, int *ids, DummyWidget *Array);
static void PrintCallbacks(callbackType *Next);
static void PrintSymbolTable(SymbolTableType *SymbolTable);
static SymbolTableType *LookUpAddr(char *temp, int module_id);
/* static char *CreateFontListFromFontName(char *FontName); */

static SymbolTableType *ReadAttributes(void);
static void ReadCallbacks(callbackType **last);
static char *ReadType(int type);

static XmString DecodeComplexString(StringType * value, int module_id, char *ascii);
static void Xm_List(Widget parent_widget, DummyWidget *DW, int module_id);

static void GetAttributes(Display *d, Window w, int module_id,
		   int *n, Arg *args,
		   DummyWidget *DW, SymbolTableType
		   *FormSymbolTable, int OnGeometry);



extern void 
MrmInitialize(void)
{
  if (!init) {
     uidpath = getenv("UIDPATH");
     init = True;
  }
}


extern Cardinal 
MrmCloseHierarchy(MrmHierarchy hierarchy_id)
{
    return MrmSUCCESS;
}


extern Cardinal 
MrmOpenHierarchy(MrmCount num_files, String *name_list, MrmOsOpenParamPtr *os_ext_list, MrmHierarchy *hierarchy_id_return)
{

    if (display)
       return MrmOpenHierarchyPerDisplay(display, num_files, name_list, os_ext_list, hierarchy_id_return);
    else {
       __MrmExit(LOC, "MrmOpenHierarchy(): no display set yet!\n");
       return MrmFAILURE;
    }
}


extern Cardinal
MrmOpenHierarchyFromBuffer(unsigned char uid_buffer,
                           MrmHierarchy *hierarchy_id)
{
    _XmWarning(NULL, "MrmOpenHierarchyFromBuffer() is not yet implemented!");
   return MrmFAILURE;
}


extern Cardinal 
MrmOpenHierarchyPerDisplay(Display *theDisplay, MrmCount num_files, String *name_list, MrmOsOpenParamPtr *os_ext_list, MrmHierarchy *hierarchy_id_return)
{
    int fileIndex;


    /*
       MrmSUCCESS     The function executed successfully.

       MrmFAILURE     The function failed.
     */
    display   = theDisplay;
    window    = DefaultRootWindow(theDisplay);
    theScreen = XDefaultScreenOfDisplay(theDisplay);
    colormap  = DefaultColormapOfScreen(theScreen);

    XtGetApplicationNameAndClass(display, &theName, &theClass);

    NumberFiles = num_files;
    FileList = name_list;

    hierarchy_id_return = NULL;

    for (fileIndex = 0; fileIndex < num_files; fileIndex++)
    {
	if (NULL == UILOpen(FileList[fileIndex], "r", stdin))
	{
	    __MrmExit(LOC, "Can't open %s\n", FileList[fileIndex]);
	}
	ReadWidgetTree(&UilModule[fileIndex]);
	if (1)
	{
	    fprintf(stderr, "GLOBAL *******\n");
	    PrintSymbolTable(GlobalSymbolTable);
	    fprintf(stderr, "LOCAL  %d\n", fileIndex);
	    PrintSymbolTable(UilModule[fileIndex].SymbolTable);
	    PrintWidgetTree(&UilModule[fileIndex]);
	}
    }
    MaxHier = fileIndex;
    return MrmSUCCESS;
}

extern Cardinal 
MrmRegisterNames(MrmRegisterArglist reglist, MrmCount num_reg)
{
/*
   MrmSUCCESS     The function executed successfully.

   MrmFAILURE     The function failed.
 */
    NumberRegisteredFunctions = num_reg;
    RegisteredFunctions = reglist;
    return MrmSUCCESS;
}


extern Cardinal
MrmRegisterClass(MrmType class_code, String class_name, String create_name,
		 Widget (*creator) (void), WidgetClass class_record)
{
    return MrmFAILURE;
}


extern Cardinal 
MrmFetchIconLiteral(MrmHierarchy hierarchy_id, String index,
		    Screen *scrn, Display *d, Pixel fg, Pixel bg,
		    Pixmap *pixmap)
{
    SymbolTableType *symbol;

    for (symbol = GlobalSymbolTable; symbol; symbol = symbol->Next)
	if (strcmp(index, symbol->Name) == 0)
	{
	    if (MrmRtypeXBitmapFile == symbol->type)
	    {
		BitMapType *bm = (BitMapType *) symbol->value;

		*pixmap = XCreatePixmapFromBitmapData(d, window, bm->data,
						      bm->width, bm->height,
						      fg, bg,
						      DefaultDepth(display, 0));
	    }
	    else
		MakePixmap(d, pixmap, (PixmapType *) symbol->value, fg, bg);
	    return MrmSUCCESS;
	}
    return MrmNOT_FOUND;
}


extern Cardinal 
MrmFetchColorLiteral(MrmHierarchy hierarchy_id, String index,
		     Display *display, Colormap cmap,
		     Pixel *pixel_return)
{
/*
   XColor exact, def;
   SymbolTableType *value;
   MrmCode type = 3;

   if (MrmSUCCESS == MrmMrmFetchLiteral(hierarchy_id, index, *display, (XtPointer *) value, &type)) {
   XAllocNamedColor(display, cmap, value->value, &def, &exact);
   *pixel_return = (XtArgVal)def.pixel; 
   return MrmSUCCESS;
   }
 */
    return MrmFAILURE;
}


extern Cardinal 
MrmFetchLiteral(MrmHierarchy hierarchy_id, String index, Display *d, XtPointer *value, MrmCode *type)
{
    SymbolTableType *symbol;
/*
   MrmBAD_HIERARCHY
   MrmNOT_FOUND
   MrmWRONG_TYPE
   MrmFAILURE
 */
    fprintf(stderr, "HEHE\n");
    for (symbol = GlobalSymbolTable; symbol; symbol = symbol->Next)
    {
	if (strcmp(index, symbol->Name) == 0)
	{
	    *value = (XtPointer)&symbol->value;
#if 0
	    if (*type != symbol->type)
		return MrmWRONG_TYPE;
#endif
	    return MrmSUCCESS;
	}
    }
    return MrmNOT_FOUND;
}


extern Cardinal 
MrmFetchWidgetOverride(MrmHierarchy hierarchy_id, String index, Widget parent_widget, String override_name, ArgList override_args, Cardinal override_num_args, Widget *widget, MrmType *wclass)
{
    Cardinal returnValue =
         MrmFetchWidget(hierarchy_id, index, parent_widget, widget, wclass);
    XtSetValues(*widget, override_args, override_num_args);

    return returnValue;
}


extern Cardinal
MrmFetchSetValues(MrmHierarchy hierarchy_id,
				  Widget w,
				  ArgList args,
				  Cardinal num_args)
{
	/* Fix Me ! */
	_XmWarning(NULL, "MrmFetchSetValues() is not yet implemented!");
	return MrmFAILURE;
}


extern Cardinal
MrmFetchBitmapLiteral(MrmHierarchy hierarchy_id,
				      String index,
				      Screen *screen,
				      Display *display,
				      Pixmap *pixmap_return,
				      Dimension *width,
				      Dimension *height)
{
	/* Fix Me ! */
	_XmWarning(NULL, "MrmFetchBitmapLiteral() is not yet implemented!");
	return MrmFAILURE;
}
	

extern Cardinal 
MrmFetchWidget(MrmHierarchy hierarchy_id, String index, Widget parent_widget, Widget *widget, MrmType *wclass)
{
    DummyWidget *DW = NULL;
    int this_id = -1, child_id = -1;
    int i = 0;
    int k, n;
    int module_id = 0;
    Widget child;
    MrmType type;
    Bool Found = False;
    int file;
    Arg *args, ARGS[MAX_ARGS];
    int SizeArgs = 0;
    MyHierStruct *MyHier = (MyHierStruct *)hierarchy_id;
    char *CreateName;
    int CreateClass;

    FetchCount++;
    if (1 == FetchCount)
    {
	FreeSymbolTable(InstantationSymbolTable);
	InstantationSymbolTable = NULL;
    }
    parent = parent_widget;

    for (file = 0; !Found && (file < NumberFiles); file++)
    {
	for (k = 0; k < UilModule[file].NumberOfWidgets; k++)
	{
	    if (strcmp(index, UilModule[file].DummyWidgetArray[k].Name) == 0)
	    {
		module_id = file;
		Found = True;
		DW = &UilModule[file].DummyWidgetArray[k];
		this_id = k;
		break;
	    }
	}
    }
    if (NULL == DW)
	__MrmExit(LOC, "Looks like a corrupt uid file\n");

    if ((FetchCount != 1) && (NULL != hierarchy_id))
    {
	args = MyHier->args;
	n = MyHier->argCount;
	SizeArgs = MyHier->SizeArgs;
	CreateName = MyHier->CreateName;
	CreateClass = MyHier->CreateClass;
    }
    else
    {
	CreateName = DW->Name;
	CreateClass = DW->wclass;
	n = 0;
	args = ARGS;
	SizeArgs = 0;
    }

    if (!Found)
    {
	fprintf(stderr, "Fetch return Not FOUND\n");
	FetchCount--;
	return (MrmNOT_FOUND);
    }

    GetAttributes( /*display */ NULL, /*window */ 0,
		  module_id, &n, args,
		  DW, InstantationSymbolTable, ON_CREATE);

    if (DW->NumberOfInherit)
    {
	for (i = 0; i < DW->NumberOfInherit; i++)
	{
	    char *index;
	    int inherit_id;
	    MyHierStruct *Hier;

	    if (NULL == MyHier)
	    {
		Hier = (MyHierStruct *)malloc(sizeof(MyHierStruct));
		Hier->CreateName = DW->Name;
		Hier->CreateClass = DW->wclass;
		Hier->args = args;
		Hier->argCount = n;
		Hier->SizeArgs = SizeArgs;
	    }
	    else
		Hier = MyHier;
	    inherit_id = DW->inherit[i];
	    index = UilModule[module_id].DummyWidgetArray[inherit_id - 1].Name;

	    MrmFetchWidget((MrmHierarchy)Hier, index, parent_widget, widget, wclass);
	}
    }
    else
    {
	if (XM_LIST_INDEX == CreateClass)
	{
	    Xm_List(parent_widget, DW, module_id);
	    *widget = NULL;
	    FetchCount--;
	    return MrmSUCCESS;
	}

	if (NULL == CreateFunctions[CreateClass].function)
	{
	    char Name[20];
	    strcpy(Name, &CreateFunctions[CreateClass].Name[2]);
	    Name[0] = '*';
	    *widget = XtNameToWidget(parent_widget, Name);
	}
	else
	{
	    SymbolTableType *newent = (SymbolTableType *)malloc(sizeof(SymbolTableType));

	    *widget = CreateFunctions[CreateClass].function(parent_widget,
							  CreateName, args, n);
	    newent->value = (char *)*widget;
	    newent->Name = __MrmStore(CreateName);
	    newent->Next = InstantationSymbolTable;
	    InstantationSymbolTable = newent;
	    newent->type = ANOTHER_WIDGET;
	}
    }
    if (NULL == *widget)
    {
	__MrmExit(LOC, "Couldn't create widget %s", DW->Name);
    }
    UilModule[module_id].WidgetArray[this_id] = *widget;
    for (i = 0; i < DW->NumberOfChildren; i++)
    {
	Cardinal returnCode;
	DummyWidget *ChildDummy;

	child_id = DW->children[i];
	ChildDummy = &UilModule[module_id].DummyWidgetArray[child_id - 1];

	if (MrmSUCCESS !=
	    (returnCode =
	     MrmFetchWidget(NULL, ChildDummy->Name,
			    UilModule[module_id].WidgetArray[this_id],
			    &child, &type)))
	{
	    FetchCount--;
	    return (returnCode);
	}

	if ((0 == strcmp(CreateFunctions[DW->wclass].Name, "XmCascadeButton")) ||
	    (0 == strcmp(CreateFunctions[DW->wclass].Name, "XmOptionMenu")))
	{
	    Arg args[1];

	    XtSetArg(args[0], XmNsubMenuId, child);
	    XtSetValues(UilModule[module_id].WidgetArray[this_id], args, 1);
	}
	if (child && UilModule[module_id].DummyWidgetArray[child_id - 1].managed)
	{
	    XtManageChild(child);
	}
    }

    n = 0;
    GetAttributes( /*display */ NULL, /*window */ 0,
		  module_id, &n, args,
		  DW, InstantationSymbolTable, LATE);

    XtSetValues(*widget, args, n);
    GetCallbacks(module_id, this_id, DW);
    UilModule[module_id].WidgetArray[this_id] = *widget;
    AddGeometryAttributes(&GeometryList, module_id, this_id, DW);
    if (1 == FetchCount)
    {				/* Just before we return to initial caller do GEOM */
	SetGeometryAttributes(&GeometryList);
    }
    FetchCount--;
    return MrmSUCCESS;
}


extern Cardinal
MrmFetchInterfaceModule(MrmHierarchy hierarchy_id ,
					char *module_name,
					Widget parent,
					Widget *w_return)
{
	/* This is a legacy call, which is no longer documented in
         2.1 docs, but seems to be present at least in 2.0 still */
	_XmWarning(NULL, "MrmFetchInterfaceModule() is not implemented!");
	return MrmFAILURE;
}


static void 
MakePixmap(Display *d, Pixmap *pixmap, PixmapType * data, Pixel fg,
	   Pixel bg)
{
    char *temp, *value;
    char buffer[256];
    int filenr;
    ColorTableHeaderType *ColorTableHeader;
    ColorTableType *ColorTable;
    XpmAttributes attributes;
    char Header[256];
    char **andrew = NULL;
    int NumColors;
    int i, j, k, Index;
    SymbolTableType *symbol;
    GC *GCTable;
    XColor exact, def;
    char *Image;
    ColorDefType *Color;

    attributes.valuemask = 0;
    temp = data->ColorTable;
    if (NULL == (symbol = LookupValue(GlobalSymbolTable, temp)))
    {
	for (filenr = 0; filenr < NumberFiles; filenr++)
	{
	    if (NULL !=
		(symbol = LookupValue(UilModule[filenr].SymbolTable, temp)))
		break;
	}
	if (NULL == symbol)
	    __MrmExit(LOC, "Couldn't find %s in symbol tables\n", temp);
    }
    ColorTableHeader = (ColorTableHeaderType *)symbol->value;
    ColorTable = ColorTableHeader->ColorTable;
    NumColors = ColorTableHeader->NumColors;
    GCTable = (GC *)malloc(sizeof(GC) * NumColors);
    if (NULL == ColorTable[0].StringXPM)
    {
	for (i = 0; i < NumColors; i++)
	{
	    long theColor = 0;

	    value = ColorTable[i].Color;
	    if (ColorTable[i].IsAddress)
	    {
		if (NULL == (symbol = LookupValue(GlobalSymbolTable, value)))
		{
		    for (filenr = 0; filenr < MaxHier; filenr++)
		    {
			if (NULL !=
			    (symbol = LookupValue(UilModule[filenr].SymbolTable, value)))
			    break;
		    }
		    if (NULL == symbol)
			__MrmExit(LOC, "Couldn't find %s in symbol tables\n", value);
		}
		value = symbol->value;
	    }
	    Color = (ColorDefType *) value;
	    sprintf(buffer, "%s     c %s", ColorTable[i].Reference, Color->name);
	    GCTable[i] = XCreateGC(d, window, 0, NULL);
	    if (Color->name)
	    {
		if (strcmp(Color->name, "_fg") == 0)
		      theColor = fg;
		    else if (strcmp(Color->name, "_bg") == 0)
			  theColor = bg;
		    else
		    {
			XAllocNamedColor(d, colormap, Color->name, &def, &exact);
			theColor = def.pixel;
		    }
	    }
	    else
	    {
		def.red = Color->r;
		def.green = Color->g;
		def.blue = Color->b;
		if (XAllocColor(display, colormap, &def))
		    theColor = def.pixel;
		else
		    __MrmExit(LOC, "Couldn't resolve color: %s\n",
			 Color->r, Color->g, Color->b);
	    }

	    XSetForeground(d, GCTable[i], theColor);
/*      ColorTable[i].StringXPM = __MrmStore(buffer); */
	}
    }
    andrew = (char **)malloc(sizeof(char *) * (1 + NumColors + data->width));
    sprintf(Header, "%d %d %d 1 -1 -1", data->width, data->height, NumColors);
    andrew[0] = Header;
    for (i = 0; i < NumColors; i++)
	andrew[i + 1] = ColorTable[i].StringXPM;
    for (i = 0; i < (int)data->width; i++)
	andrew[i + NumColors + 1] = &data->data[i * data->width];
    Image = data->data;

    /* the following is a total hack because I want to go to bed.  The Pixel
     * value in XmGetPixmapByDepth are totally made up. */
    *pixmap = XCreatePixmap(display, window, data->width, data->height,
			    DefaultDepth(display, 0));
    for (i = 0; i < (int)data->height; i++)
    {
	for (j = 0; j < (int)data->width; j++)
	{
	    Index = -1;
	    for (k = 0; k < NumColors; k++)
	    {
		/* FIX ME  -- the following may need to compare more than one
		   byte in case each pixel of the image is wider
		 */
		if (strncmp(ColorTable[k].Reference, Image, 1) == 0)
		{
		    Index = k;
		    break;
		}
	    }
	    if (Index == -1)
		__MrmExit(LOC, "Color Error\n");
	    XDrawPoint(display, *pixmap, GCTable[Index], j, i);
	    Image += 1;
	}
    }
#if 0
    if (_LtXmXpmCreateImageFromData(d, andrew, &im, &imshape, &attributes) ==
	_LtXpmSuccess)
    {
	GC gc;

	*pixmap = XCreatePixmap(d, window, im->width, im->height, im->depth);

	gc = XCreateGC(d, window, 0, NULL);

	XPutImage(d, *pixmap, gc, im, 0, 0, 0, 0, im->width, im->height);

	XFreeGC(d, gc);

	/* this should probably be enabled */
#if 0
	XDestroyImage(im);
	if (imshape)
	    XDestroyImage(imshape);
#endif

	_LtXmXpmFreeAttributes(&attributes);
    }
#endif

    free(andrew);
    free(GCTable);
}


static void 
AddGeometryAttributes(GeoType **GeometryList, int module_id, int this_id,
		      DummyWidget *DW)
{
    GeoType *node = (GeoType *)malloc(sizeof(GeoType));
    node->module_id = module_id;
    node->this_id = this_id;
    node->next = *GeometryList;
    *GeometryList = node;
    node->DW = DW;
}

static void 
SetGeometryAttributes(GeoType **GeometryList)
{
    GeoType *node = *GeometryList;
    GeoType *next;
    Arg args[MAX_ARGS];
    int n;
    Widget widget;

    while (node)
    {
	next = node->next;
	widget = UilModule[node->module_id].WidgetArray[node->this_id];
	n = 0;
	GetAttributes( /*display */ NULL, /*window */ 0,
		      node->module_id, &n, args,
		      node->DW, InstantationSymbolTable, GEO);
	XtSetValues(widget, args, n);
	free(node);
	node = next;
    }
    *GeometryList = NULL;
}

static void
GetCallbacks(int filenr, int wid_id, DummyWidget *DW)
{
    callbackType *Next = DW->callbacks;
    XtPointer ParameterValue;
    SymbolTableType *SymbolTable = NULL;
    Widget w = UilModule[filenr].WidgetArray[wid_id];

    while (Next)
    {
	if (Next->Parameters)
	{
	    if (MrmRtypeAddrName == Next->Parameters->type)
	    {
		char *temp;

		temp = Next->Parameters->value;
		if (1 != __MrmLookUpPredefines(temp, (long *)&ParameterValue))
		    if (NULL ==
			(SymbolTable = LookupValue(UilModule[filenr].SymbolTable, temp)))
		    {
			if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp)))
			{
			    __MrmExit(LOC, "Couldn't find %s in symbol tables\n", temp);
			}
		    }
	    }
	    else
		SymbolTable = (SymbolTableType *)(Next->Parameters);
	}
	else
	    ParameterValue = NULL;
	if (SymbolTable)
	{
	    if (MrmRtypeInteger != SymbolTable->type)
		ParameterValue = (XtPointer)SymbolTable->value;
	    else
		ParameterValue = (XtPointer)&(SymbolTable->value);
	}
	if (strcmp("createCallback", Next->argName) != 0)
	    XtAddCallback(w, Next->argName,
			  (XtCallbackProc)__MrmLookUpFunction(Next->functionName),
			  ParameterValue);
	else
	    ((XtCallbackProc)
	     __MrmLookUpFunction(Next->functionName)) (w, ParameterValue, NULL);
	Next = Next->next;
    }
    return;
}

static void
GetAttributes(Display *d, Window w, int module_id,
	      int *n, Arg *args,
	      DummyWidget *DW, SymbolTableType
	      *FormSymbolTable, int State)
{
    int count = 0;
    XtArgVal value;
    SymbolTableType *Next = DW->attributes;
    int Do;

/*  *n = 0;   We will append to existing attributes - make sure n is 0 
   before calling this routine if that is intended 
 */
    while (Next)
    {
	if (Next->Access & S)
	{			/* Attribute can be set after widget creation */
	    Do = (LATE == State);
	}
	else if (Next->Access & C)
	{			/* Attribute must be set at creation time */
	    Do = (ON_CREATE == State);
	}
	else
	{			/* Special '0' case - very late attribute setting */
	    Do = (GEO == State);
	}
	if (Do)
	{
	    count = GetSymbolTableValue(&value, Next, module_id, FormSymbolTable);
	    XtSetArg(args[*n], Next->Name, value);
	    (*n)++;
	    if (*n >= MAX_ARGS)
		__MrmExit(LOC, "Too many args\n");
	    if (count > 0)
	    {			/* Certain attributes have a count associated */
		char buff[256];	/* with them.  This is a temporary(?) way to  */
		/* identify them and automatically include it */
		if (0 == strcmp(Next->Name, XmNitems))
		    strcpy(buff, XmNitemCount);
#if XmVERSION > 1
		else if (0 == strcmp(Next->Name, XmNdetail))
		    strcpy(buff, XmNdetailCount);
		else if (0 == strcmp(Next->Name, XmNdetailColumnHeading))
		    strcpy(buff, XmNdetailColumnHeadingCount);
		else if (0 == strcmp(Next->Name, XmNvalues))
		    strcpy(buff, XmNnumValues);
#endif
		else
		    __MrmWarn(LOC, "Can't find %s\n", Next->Name);
		XtSetArg(args[*n], buff, count);
		(*n)++;
		if (*n >= MAX_ARGS)
		    __MrmExit(LOC, "You guessed it - Too many args\n");
	    }
	}
	Next = Next->Next;
    }
}

static int 
GetSymbolTableValue(XtArgVal *value, SymbolTableType *Next,
		    int module_id, SymbolTableType *FormSymbolTable)
{
    int status = 0;
    char *temp;
    BitMapType *bitmap;
    SymbolTableType *SymbolTable;

    switch (Next->type)
    {
    case MrmRtypePixmapImage:
	{
	    PixmapType *data;
	    char *Image;
	    Pixel fg, bg;
	    Pixmap icon;

	    data = (PixmapType *) Next->value;
	    Image = data->data;
	    XtVaGetValues(parent, XmNforeground, &fg, XmNbackground, &bg, NULL, NULL);
	    MakePixmap(display, &icon, data, fg, bg);
	    *value = (long)icon;

	    break;
	}
    case MrmRtypeXBitmapFile:
	bitmap = (BitMapType *) Next->value;;
	*value = (long)XCreateBitmapFromData(display, window,
					     bitmap->data,
					     bitmap->width, bitmap->height);
	break;
    case MrmRtypeKeysym:
	{
	    char *sym = (char *)Next->value;
	    if ('~' == sym[0])
	    {
		temp = &sym[1];
		if (NULL ==
		    (SymbolTable = LookupValue(UilModule[module_id].SymbolTable, temp)))
		{
		    if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp)))
		    {
			if (NULL == (SymbolTable = LookupValue(FormSymbolTable, temp)))
			{
			    __MrmExit(LOC, "Could not find %s\n", temp);
			}
		    }
		}
		*value = XStringToKeysym(SymbolTable->value);
	    }
	    else
		*value = XStringToKeysym(Next->value);
	    break;
	}
    case MrmRtypeInteger:
    case MrmRtypeBoolean:
	*value = (long)Next->value;
	break;
    case MrmRtypeFont:
	{
	    XFontStruct *theFont = XLoadQueryFont(display, Next->value);
	    XmFontListEntry theEntry;

	    if (NULL == theFont)
		__MrmExit(LOC, "Can't load font %s\n", Next->value);
	    theEntry = XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG,
					     XmFONT_IS_FONT,
					     (XtPointer)theFont);
	    *value = (long)XmFontListAppendEntry(NULL, theEntry);
	    break;
	}
    case MrmRtypeFontList:
	{
	    int i;
	    FontTableHeaderType *theHeader = (FontTableHeaderType *) Next->value;
	    XmFontList theFontList = NULL;
	    XmFontListEntry theEntry;
	    char *FontName;
	    char *TagName;
	    SymbolTableType *symbolTable;

	    for (i = 0; i < theHeader->NumFonts; i++)
	    {
		FontName = theHeader->FontTable[i].FontName;
		TagName = theHeader->FontTable[i].Tag.name;
		if (0 == TagName[0])
		    TagName = XmFONTLIST_DEFAULT_TAG;
		symbolTable = LookUpAddr(FontName, module_id);
		fprintf(stderr, "LOOKUP FONT %s\n", FontName);
		if (NULL == symbolTable)
		    __MrmExit(LOC, "Can't find %s in the symbol tables\n", FontName);
		FontName = (char *)symbolTable->value;

/* RtoL = theHeader->FontTable[i].Tag.DirectionRtoL; */
/* WideChar = theHeader->FontTable[i].Tag.WideChar; */

		theEntry = XmFontListEntryLoad(display, FontName, XmFONT_IS_FONT,
					       TagName);
		theFontList = XmFontListAppendEntry(theFontList, theEntry);
		XmFontListEntryFree(&theEntry);
	    }
	    *value = (long)theFontList;
	    break;
	}
    case MrmRtypeColor:
	{
	    XColor exact, def;
	    ColorDefType *Color = (ColorDefType *) (Next->value);

	    if (Color->name)
	    {
		if (XAllocNamedColor(display, colormap, Color->name, &def, &exact))
		{
		    *value = (XtArgVal)def.pixel;
		}
		else
		    __MrmExit(LOC, "Couldn't resolve color: %s\n", Next->value);
	    }
	    else
	    {
		def.red = Color->r;
		def.green = Color->g;
		def.blue = Color->b;
		if (XAllocColor(display, colormap, &def))
		    *value = (XtArgVal)def.pixel;
		else
		    __MrmExit(LOC, "Couldn't resolve color: %s\n", Next->value);
	    }
	    break;
	}
    case MrmRtypeAddrName:
	temp = Next->value;
	if (1 == __MrmLookUpPredefines(temp, value))
	    break;
	if (NULL ==
	    (SymbolTable = LookupValue(UilModule[module_id].SymbolTable, temp)))
	{
	    if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp)))
	    {
		if (NULL == (SymbolTable = LookupValue(FormSymbolTable, temp)))
		{
		    __MrmExit(LOC, "Could not find %s\n", temp);
		}
	    }
	}
	status = GetSymbolTableValue(value, SymbolTable, module_id, FormSymbolTable);
	break;
    case MrmRtypeChar8Vector:
	{
	    XmString *strings;
	    char **array;
	    char **temparray;
	    int i = 0;

	    array = (char **)Next->value;
	    for (temparray = array; *temparray; temparray++)
		i++;
	    strings = (XmString *)malloc(i * sizeof(XmString *));
	    for (i = 0; *array; array++, i++)
	    {
		char *sym;
		sym = *array;
		if ('~' == sym[0])
		{
		    temp = &sym[1];
		    if (NULL ==
			(SymbolTable = LookupValue(UilModule[module_id].SymbolTable, temp)))
		    {
			if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp)))
			{
			    if (NULL == (SymbolTable = LookupValue(FormSymbolTable, temp)))
			    {
				__MrmExit(LOC, "Could not find %s\n", temp);
			    }
			}
		    }
		    strings[i] = XmStringCreate(SymbolTable->value, XmFONTLIST_DEFAULT_TAG);
		}
		else
		    strings[i] = XmStringCreate(*array, XmFONTLIST_DEFAULT_TAG);
	    }
	    *value = (XtArgVal)strings;
	    status = i;
	    break;
	}
    case MrmRtypeChar8:
	*value = (XtArgVal)XmStringCreateSimple(Next->value);
	break;
    case MrmRtypeCString:
	*value = (XtArgVal)
	    DecodeComplexString((StringType *) Next->value, module_id, NULL);
	break;
    case ANOTHER_WIDGET:	/* Another Widget */
	*value = (XtArgVal)(Next->value);
	break;
    default:
	__MrmExit(LOC, "UNKNOWN ATTRIBUTE: %d\n", Next->type);
    }
    return status;
}

static XmString 
DecodeComplexString(StringType * stringC, int module_id, char *ascii)
{
    SymbolTableType *st;
    StringType *tempC;
    XmString s = NULL;
    static char a[2048];

    a[0] = 0;
    for (; stringC; stringC = stringC->Next)
    {
	if (stringC->IsAddr)
	{
	    tempC = stringC;
	    st = LookUpAddr(stringC->theString, module_id);
	    if (NULL == st)
		__MrmExit(LOC, "Could not find %s\n", stringC->theString);
	    tempC = (StringType *) st->value;
	    s = XmStringConcat(s, DecodeComplexString(tempC, module_id, NULL));
	}
	else
	{
	    s = XmStringConcat(s, XmStringCreate(stringC->theString,
						 stringC->theFontSet));
	    strncat(a, stringC->theString, 2048);
	    if (stringC->IsSeparator)
	    {
		strcat(a, "\n");
		s = XmStringConcat(s, XmStringSeparatorCreate());
	    }
	}
    }
    if (ascii != NULL)
	ascii = &a[0];
    return s;
}

static SymbolTableType *
GetSymbolTable(SymbolTableType *Table)
{
    char name[256], ch;
    int i;
    SymbolTableType *NewTable = NULL;
    SymbolTableType *LastTable = NULL;

    if (Table)
	for (LastTable = Table; LastTable->Next; LastTable = LastTable->Next)
	    ;
    for (ch = getchar(); ch; ch = getchar())
    {
	for (i = 0, name[i] = ch; name[i] != '"'; i++, name[i] = getchar())
	    ;
	name[i] = 0;
	NewTable = (SymbolTableType *)malloc(sizeof(SymbolTableType));
	NewTable->Next = NULL;
	if (LastTable)
	{
	    LastTable->Next = NewTable;
	}
	else if (NULL == Table)
	    Table = NewTable;
	LastTable = NewTable;
	NewTable->Name = __MrmStore(name);
	NewTable->type = getchar();
	NewTable->value = ReadType(NewTable->type);
    }

    if (NULL == Table)
	return NewTable;
    else
	return Table;
}

static SymbolTableType *
LookupValue(SymbolTableType *SymbolTable, char *name)
{
    while (SymbolTable)
    {
	if (0 == strcmp(SymbolTable->Name, name))
	    return SymbolTable;
	else
	    SymbolTable = SymbolTable->Next;
    }
    return NULL;
}

static void 
PrintWidgetTree(UilModuleType *Module)
{
    PrintWidgets(Module);
}

/* LIST SYMBOL_TABLE(Global) SYMBOL_TABLE(local) WIDGETS */
static void 
ReadWidgetTree(UilModuleType *Module)
{
    ReadList(Module);
    GlobalSymbolTable =
	GetSymbolTable(GlobalSymbolTable);
    Module->SymbolTable =
	GetSymbolTable(NULL);
    ReadWidgets(Module);
}

/* LIST = INT(count) NAMES  CHAR(0) */
static void 
ReadList(UilModuleType *Module)
{
    int NumberOfWidgets;
    int i, c;

    fread(&NumberOfWidgets, sizeof(int), 1, stdin);	/* INT(widget count)  */
    fread(&MaxAttributes, sizeof(int), 1, stdin);	/* INT(max attributes)  */

    Module->NumberOfWidgets = NumberOfWidgets;
    Module->Definitions = (char **)malloc(sizeof(char *) * NumberOfWidgets);
    Module->WidgetArray = (Widget *)malloc(sizeof(Widget) * NumberOfWidgets);
    Module->DummyWidgetArray = (DummyWidget *)
	malloc(sizeof(DummyWidget) * NumberOfWidgets);

    for (i = 0; (((c = getc(stdin)) != EOF) && c); i++)
    {				/* NAMES CHAR(0)   */
	char string[MAX_STR];
	int j = 0;

	for (j = 0; (c != EOF) && (c != '"'); j++)
	{			/* NAMES = NAME "  */
	    string[j] = (char)c;	/* NAME = [^"]*    */
	    if (j >= MAX_STR)
		__MrmExit(LOC, "String def too long\n");
	    c = getc(stdin);
	}
	string[j] = 0;
	Module->DummyWidgetArray[i].Name = __MrmStore(string);
    }
    if (c)
	__MrmExit(LOC, "ERROR in header\n");
}

static void 
PrintWidgets(UilModuleType *Module)
{
    DummyWidget *DW;
    int i;

    for (i = 0; i < Module->NumberOfWidgets; i++)
    {
	DW = &Module->DummyWidgetArray[i];
	fprintf(stderr, "oject %s: %s {\n", DW->Name,
		CreateFunctions[DW->wclass].Name);
	PrintArgs(DW->attributes);
	PrintControls(DW->NumberOfChildren, DW->children, Module->DummyWidgetArray);
	PrintCallbacks(DW->callbacks);
	fprintf(stderr, "};\n");
    }
}

static void 
PrintArgs(SymbolTableType *Next)
{
    fprintf(stderr, "\targuments {\n");
    while (Next)
    {
	switch (Next->type)
	{
	case MrmRtypeInteger:
	case MrmRtypeBoolean:
	    fprintf(stderr, "\t\t%s = %ld;\n", Next->Name, (long)Next->value);
	    break;
	case MrmRtypeKeysym:
	case MrmRtypeAddrName:
	    fprintf(stderr, "\t\t%s = \"%s\";\n", Next->Name, Next->value);
	    break;
	case MrmRtypeChar8:
	    fprintf(stderr, "\t\t%s = \"%s\";\n", Next->Name,
		((StringType *)Next->value)->theString);
	    break;
	case MrmRtypeCString:
	    fprintf(stderr, "\t\t%s = \"%s\";\n", Next->Name,
		((StringType *)Next->value)->theString);
	    break;
	default:
	    fprintf(stderr, "!  ????  %s \n", Next->Name);
	    break;
	}
	Next = Next->Next;
    }
    fprintf(stderr, "\t};\n");
}

static void 
PrintControls(int count, int *ids, DummyWidget *Array)
{
    int i;
    DummyWidget *DW;

    fprintf(stderr, "\tcontrols {\n");
    for (i = 0; i < count; i++)
    {
	DW = &Array[ids[i] - 1];
	fprintf(stderr, "\t\t%s %s;\n", CreateFunctions[DW->wclass].Name, DW->Name);
    };
    fprintf(stderr, "\t}\n");
}

static void 
PrintCallbacks(callbackType *Next)
{
    SymbolTableType *NextParam;

    fprintf(stderr, "\tcallbacks {\n");
    while (Next)
    {
	fprintf(stderr, "\t\t%s = procedure %s(",
		Next->argName, Next->functionName);
	for (NextParam = Next->Parameters; NextParam; NextParam = NextParam->Next)
	    switch (NextParam->type)
	    {
	    case MrmRtypeInteger:
	    case MrmRtypeBoolean:
		fprintf(stderr, "%ld", (long)NextParam->value);
		break;
	    case MrmRtypeKeysym:
	    case MrmRtypeAddrName:
		fprintf(stderr, "%s", NextParam->value);
		break;
	    case MrmRtypeChar8:
		fprintf(stderr, "%s",
			((StringType *)NextParam->value)->theString);
		break;
	    case MrmRtypeCString:
		fprintf(stderr, "%s",
			((StringType *)NextParam->value)->theString);
		break;
	    default:
		break;
	    }
	fprintf(stderr, ");\n");
	Next = Next->next;
    };
    fprintf(stderr, "\t};\n");
}

/*
   // WIDGETS = WIDGET WIDGETS
   //         | NULL
 */
static void 
ReadWidgets(UilModuleType *Module)
{
    char c;
    int WidgetIndex;
    DummyWidget *DW;
    int i, index, size;
    int this_id, child_id, inherit_id;
    char temp[256];

/*
   In the following definition:

   integer size = 4 bytes

   INT(0) - an integer sized zero to mark
   the end of a list  e.g. INHERIT is a list of inherited widgets but
   if a widget id of zero is read, we know we have come to the end of the list

   CHAR(MrmTypeClassRecName) - a byte sized value of 22 (MrmTypeClassRecName) 
   to indicate the start of a widget.
 */

/* WIDGET = CHAR(MrmTypeClassRecName) NAME(widget's name) " INT(id) INT(class) 
   //           CHAR(managed) INT(parent) INHERIT INT(0) CHILDREN INT(0) 
   //           CALLBACKS TABLE(attributes)
 */
    for (c = getc(stdin), WidgetIndex = 0;
	 c == MrmRtypeClassRecName;
	 c = getc(stdin), WidgetIndex++)
    {				/* CHAR(MrmTypeClassRecName) */
	for (i = 0, c = getchar(); c != '"'; c = getchar(), i++)
	{			/* NAME " */
	    temp[i] = c;
	}
	temp[i] = 0;
	DW = &Module->DummyWidgetArray[WidgetIndex];
	DW->Name = __MrmStore(temp);
	if (strcmp(DW->Name, "templatePick") == 0)
	    fprintf(stderr, "STOP\n");
	fread(&this_id, sizeof(this_id), 1, stdin);	/*INT(id)    */
	if (this_id != (WidgetIndex + 1))
	    __MrmExit(LOC, "INDEX ERROR:widgets should be stored in file by id number\n");
	fread(&DW->wclass, sizeof(DW->wclass), 1, stdin);		/*INT(class) */
	fread(&DW->managed, sizeof(DW->managed), 1, stdin);	/*CHAR(managed) */
	fread(&DW->parent, sizeof(DW->parent), 1, stdin);	/*INT(parent) */
	DW->inherit = NULL;
	size = 0;
	index = 0;
	for (fread(&inherit_id, sizeof(inherit_id), 1, stdin);	/*INHERIT = INT INH */
	     inherit_id;	/*INT(0)  */
	     fread(&inherit_id, sizeof(inherit_id), 1, stdin))
	{
	    if (size <= index)
	    {
		size += 10;
		DW->inherit = (int *)realloc(DW->inherit, size * sizeof(inherit_id));
	    }
	    DW->inherit[index] = inherit_id;
	    index++;
	}
	DW->NumberOfInherit = index;
	DW->children = NULL, size = 0, index = 0;
	for (fread(&child_id, sizeof(child_id), 1, stdin);	/*CHILDREN=INT CHILDREN */
	     child_id;		/*INT(0) */
	     fread(&child_id, sizeof(child_id), 1, stdin))
	{
	    if (size <= index)
	    {
		size += 10;
		if (DW->children)
		    DW->children =
			(int *)realloc(DW->children, size * sizeof(child_id));
		else
		    DW->children =
			(int *)malloc(size * sizeof(child_id));
	    }
	    DW->children[index] = child_id;
	    index++;
	}
	DW->NumberOfChildren = index;
	ReadCallbacks(&(DW->callbacks));
	DW->attributes = ReadAttributes();
    }
}

/* CALLBACKS = CHAR(MrmRtypeCallback) INT(id) INT(index) NAME(argname) " 
   //               NAME(function name) " CHAR(# of params) PARAMS CALLBACKS
   //           | NULL
   // PARAMS = CHAR(type) INT(value) PARAMS      integer,bool
   //        | CHAR(type) STRING CHAR(0) PARAMS   AddrName,CString,String,keysym
 */
static void 
ReadCallbacks(callbackType **first)
{
    char c, temp[MAX_BUFFER];
    int i;
    SymbolTableType **ParamPointer;
    char NumberOfParams;
    callbackType *last;
    callbackType *newc;

    *first = last = NULL;
    c = getchar();
    for (; c == MrmRtypeCallback; c = getchar())
    {				/* CHAR(MrmRtypeCallback) */
	newc = (callbackType *)malloc(sizeof(callbackType));
	newc->Parameters = NULL;
	newc->next = NULL;
	if (last)
	    last->next = newc;
	last = newc;
	if (NULL == *first)
	    *first = newc;
	fread(&newc->id, sizeof(newc->id), 1, stdin);	/* INT(id)     */
	for (i = 0, c = getchar(); c != '"'; c = getchar())	/* NAME(argname) */
	    temp[i++] = c;
	temp[i] = 0;
	newc->argName = __MrmStore(temp);
	for (i = 0, c = getchar(); c != '"'; c = getchar())	/* NAME(function name) */
	    temp[i++] = c;
	temp[i] = 0;
	newc->functionName = __MrmStore(temp);
/* PARAMS = CHAR(type) INT(value) PARAMS      integer,bool       */
/*        | CHAR(type) STRING CHAR(0) PARAMS   AddrName,CString,String,keysym */
	NumberOfParams = getchar();
	ParamPointer = &(newc->Parameters);
	newc->Parameters = NULL;
	for (i = 0; i < NumberOfParams; i++)
	{
	    *ParamPointer = (SymbolTableType *)malloc(sizeof(SymbolTableType));
	    (*ParamPointer)->type = getchar();
	    (*ParamPointer)->value = ReadType((*ParamPointer)->type);
	    (*ParamPointer)->Next = NULL;
	    ParamPointer = &((*ParamPointer)->Next);
	}
    }
    ungetc(c, stdin);
}

/*  TABLE(attributes) = EXPRESSION TABLE(attributes)
   //                    | NULL
 */
static SymbolTableType *
ReadAttributes(void)
{
    char c;
    SymbolTableType *Return = NULL, *Next, *Last = NULL;

    fprintf(stderr, "READ ATTRIBUTES\n");
/* EXPRESSION = CHAR(MrmRtypeResource) NAME " CHAR(type) INT(value)
   //        | CHAR(MrmRtypeResource) NAME " CHAR(type) NAME(string value) CHAR(0)
   //        | CHAR(MrmRtypeResource) NAME " CHAR(type) NAME(bitmap value) CHAR(0)
   //        | CHAR(MrmRtypeResource) NAME " CHAR(type) NAME(pixmap value) CHAR(0)
 */
    for (c = getchar();
	 c == MrmRtypeResource;
	 c = getchar())
    {				/* CHAR(MrmRtypeResource) */
	Next = (SymbolTableType *)malloc(sizeof(SymbolTableType));
	Next->Next = NULL;
	if (NULL == Last)
	    Return = Next;
	else
	    Last->Next = Next;
	fread(&(Next->Index), sizeof(Next->Index), 1, stdin);
	__MrmGetArgValues(Next->Index, &Next->Name, &Next->Access);
	Next->type = getchar();	/* CHAR(type) */
	Next->value = ReadType(Next->type);
	Last = Next;
    }
    ungetc(c, stdin);
    return Return;
}

static char *
ReadType(int type)
{
    int i, j;
    char buffer[MAX_BUFFER];
    char *temp;
    char *ReturnValue;

    switch (type)
    {
    case MrmRtypeInteger:
    case MrmRtypeBoolean:
	fread(&ReturnValue, sizeof(long), 1, stdin);
	break;
    case MrmRtypeXBitmapFile:
	{
	    int SizeOfData;
	    BitMapType *bitmap;

	    bitmap = (BitMapType *) malloc(sizeof(BitMapType));
	    fread(bitmap, sizeof(BitMapType) - sizeof(long), 1, stdin);
	    SizeOfData = bitmap->width * bitmap->height >> 3;
	    bitmap->data = (char *)malloc(SizeOfData);
	    fread(bitmap->data, sizeof(char), SizeOfData, stdin);
	    ReturnValue = (char *)bitmap;
	    break;
	}
    case MrmRtypeChar8Vector:
	{
	    char *last;
	    char **array = NULL;
	    int size = 0;
	    int index = 0;
	    temp = last = &buffer[0];
	    while ((*temp = getchar()))
	    {
		if ('"' == *temp)
		{
		    *temp = 0;
		    if (size <= index)
		    {
			size += 10;
			array = (char **)realloc(array, size * sizeof(char *));
		    }
		    array[index] = __MrmStore(last);
		    index++;
		    last = temp + 1;
		}
		temp++;
	    }
	    array[index] = NULL;
	    ReturnValue = (char *)array;
	    break;
	}
    case MrmRtypeKeysym:
    case MrmRtypeAddrName:
	temp = &buffer[0];
	while ((*temp++ = getchar()));
	ReturnValue = (char *)__MrmStore(buffer);
	break;
    case MrmRtypeChar8:
	{
	    StringType *theString = (StringType *) malloc(sizeof(StringType));
	    temp = &buffer[0];
	    while ((*temp++ = getchar()));
	    fprintf(stderr, "name = %s  ", buffer);
	    theString->theString = (char *)__MrmStore(buffer);
	    temp = &buffer[0];
	    while ((*temp++ = getchar()));
	    theString->theFontSet = __MrmStore(buffer);
	    ReturnValue = (char *)theString;
	    break;
	}
    case MrmRtypeCString:
	{
	    StringType **handle;
	    fprintf(stderr, "READING COMPLEX STRING\n");

	    ReturnValue = NULL;
	    handle = (StringType **) & ReturnValue;
	    while (1)
	    {
		for (i = 0; (i < MAX_BUFFER) && (buffer[i] = getchar()); i++);
		if (0 == i)
		{
		    break;
		}
		if (MAX_BUFFER == i)
		    __MrmExit(LOC, "Static storage area exhausted\n");
		(*handle) = (StringType *) malloc(sizeof(StringType));
		(*handle)->theString = __MrmStore(buffer);

		for (i = 0; (i < MAX_BUFFER) && (buffer[i] = getchar()); i++);
		if (MAX_BUFFER == i)
		    __MrmExit(LOC, "Static storage area exhausted: %s\n", buffer);
		(*handle)->theFontSet = __MrmStore(buffer);
		(*handle)->IsAddr = getchar();
		(*handle)->IsSeparator = getchar();
		(*handle)->Next = NULL;
		handle = &((*handle)->Next);
	    }
	    fprintf(stderr, "COMPLEX DONE\n");
	    break;
	}
    case MrmRtypeFontList:
	{			/* INT(size) FONT_TABLE */
	    FontTableType *FontTable;
	    FontTableHeaderType *FontTableHeader;
	    int NumFonts;
	    fread(&NumFonts, sizeof(int), 1, stdin);
	    FontTable = (FontTableType *) malloc(sizeof(FontTableType) * NumFonts);
	    FontTableHeader = (FontTableHeaderType *) malloc(sizeof(FontTableHeader));
	    /*                     FONT_TABLE = FONT_SET FONT FONT_TABLE */
	    /*                     FONT_SET = CHAR(MrmRtypeFontSet) NAME INT(dir)
	       INT(wideChar)  */
	    FontTableHeader->FontTable = FontTable;
	    FontTableHeader->NumFonts = NumFonts;
	    for (j = 0; j < NumFonts; j++)
	    {
		char tempType;

		fread(&tempType, sizeof(char), 1, stdin);
		if (tempType != MrmRtypeFontSet)
		    __MrmExit(LOC, "There may be an internal bug with font table decoding. "
		    "%d read != %d from header.\n", tempType, MrmRtypeFontSet);
		for (i = 0; (i < MAX_BUFFER) && (buffer[i] = getchar()); i++);
		if (MAX_BUFFER == i)
		    __MrmExit(LOC, "Static storage area exhausted\n");
		FontTable[j].Tag.name = __MrmStore(buffer);
		fread(&FontTable[j].Tag.DirectionRtoL, sizeof(int), 1, stdin);
		fread(&FontTable[j].Tag.WideChar, sizeof(int), 1, stdin);

		/*                   FONT = CHAR(MrmTypeFont) NAME   */
		fread(&tempType, sizeof(char), 1, stdin);
		if (tempType != MrmRtypeFont)
		    __MrmExit(LOC, "There may be an internal bug with font table decoding\n");
		for (i = 0; (i < MAX_BUFFER) && (buffer[i] = getchar()); i++);
		if (MAX_BUFFER == i)
		    __MrmExit(LOC, "Static storage area exhausted\n");
		FontTable[j].FontName = __MrmStore(buffer);
	    }
	    ReturnValue = (char *)FontTableHeader;
	    break;
	}
    case MrmRtypeColorTable:
	{
	    ColorTableType *ColorTable;
	    ColorTableHeaderType *ColorTableHeader;
	    int NumColors;
	    char type;
	    fread(&NumColors, sizeof(int), 1, stdin);
	    ColorTable = (ColorTableType *)malloc(sizeof(ColorTableType) * NumColors);
	    ColorTableHeader = (ColorTableHeaderType *)
		malloc(sizeof(ColorTableHeaderType));
	    ColorTableHeader->ColorTable = ColorTable;
	    ColorTableHeader->NumColors = NumColors;
	    for (j = 0; j < NumColors; j++)
	    {
		for (i = 0; (i < MAX_BUFFER) && (buffer[i] = getchar()); i++);
		if (MAX_BUFFER == i)
		    __MrmExit(LOC, "Static storage area exhausted\n");
		ColorTable[j].Reference = __MrmStore(buffer);
		fread(&type, sizeof(type), 1, stdin);
		ColorTable[j].IsAddress = (MrmRtypeAddrName == type);
		ColorTable[j].StringXPM = NULL;
		ColorTable[j].Color=ReadType(type);
	    }
	    ReturnValue = (char *)ColorTableHeader;
	    break;
	}
    case MrmRtypeColor:
	{
	    ColorDefType *Color = (ColorDefType *) malloc(sizeof(ColorDefType));
	    for (i = 0; (i < MAX_BUFFER) && (buffer[i] = getchar()); i++);
	    if (MAX_BUFFER == i)
		__MrmExit(LOC, "Static storage area exhausted\n");
	    if (0 == i)
	    {
		Color->name = NULL;
		fread(&Color->r, sizeof(int), 1, stdin);
		fread(&Color->g, sizeof(int), 1, stdin);
		fread(&Color->b, sizeof(int), 1, stdin);
	    }
	    else
	    {
		Color->name = __MrmStore(buffer);
	    }
	    ReturnValue = (char *)Color;
	    break;
	}
    case MrmRtypeFont:
	for (i = 0; (i < MAX_BUFFER) && (buffer[i] = getchar()); i++);
	if (MAX_BUFFER == i)
	    __MrmExit(LOC, "Static storage area exhausted\n");
	ReturnValue = __MrmStore(buffer);
	break;
    case MrmRtypePixmapImage:
	{
	    PixmapType *pixmap;
	    int SizeOfData;
	    int i;

	    pixmap = (PixmapType *) malloc(sizeof(PixmapType));
	    fread(&pixmap->width, sizeof(int), 1, stdin);
	    fread(&pixmap->height, sizeof(int), 1, stdin);
	    SizeOfData = pixmap->width * pixmap->height;
	    for (i = 0; (i < MAX_BUFFER) && (buffer[i] = getchar()); i++);
	    if (MAX_BUFFER == i)
		__MrmExit(LOC, "Static storage area exhausted\n");
	    pixmap->ColorTable = __MrmStore(buffer);
	    pixmap->data = (char *)malloc(SizeOfData);
	    fread(pixmap->data, sizeof(char), SizeOfData, stdin);
	    ReturnValue = (char *)pixmap;
	    break;
	}
    default:
	__MrmExit(LOC, "UNKNOWN ATTRIBUTE: code = %d\n", type);
    }

    return ReturnValue;
#if 0


    char *ReturnValue;

    switch (Next->type)
    {
    case MrmRtypePixmapImage:
	{
	    int SizeOfData;
	    PixmapType *pixmap;
	    char temp[MAX_BUFFER];
	    int i;

	    pixmap = (PixmapType *) malloc(sizeof(PixmapType));
	    fread(pixmap, sizeof(PixmapType) - sizeof(long), 1, stdin);		/*NAME(pixmap) */
	    for (i = 0; (i < MAX_BUFFER) && (temp[i] = getchar()); i++);	/*NAME(colortab */
	    if (2048 == i)
		__MrmExit(LOC, "Static storage area exhausted\n");
	    pixmap->ColorTable = __MrmStore(temp);
	    SizeOfData = pixmap->width * pixmap->height;
	    pixmap->data = (char *)malloc(SizeOfData);
	    fread(pixmap->data, sizeof(char), SizeOfData, stdin);	/* image data */
	    return ((char *)pixmap);
	    break;
	}
    case MrmRtypeXBitmapFile:
	{
	    int SizeOfData;
	    BitMapType *bitmap;

	    bitmap = (BitMapType *) malloc(sizeof(BitMapType));
	    fread(bitmap, sizeof(BitMapType) - sizeof(long), 1, stdin);		/*NAME(bitmap) */
	    SizeOfData = bitmap->width * bitmap->height >> 3;
	    bitmap->data = (char *)malloc(SizeOfData);
	    fread(bitmap->data, sizeof(char), SizeOfData, stdin);
	    return ((char *)bitmap);
	    break;
	}
    case MrmRtypeInteger:
    case MrmRtypeBoolean:
	fread(&ReturnValue, sizeof(ReturnValue), 1, stdin);	/* INT(value) */
	return (ReturnValue);
	break;
    case MrmRtypeCString:
    case MrmRtypeChar8:
	i = 0;			/* NAME(value) CHAR(0) */
	while ((i < MAX_BUFFER) && (temp[i++] = getchar()));
	if (MAX_BUFFER == i)
	{
	    temp[256] = 0;
	    __MrmExit(LOC, "String too complex: %s", temp);
	}
	temp[i] = 0;
	ReturnValue = __MrmStore(temp);
	i = 0;			/* NAME(tag) CHAR(0) */
	while ((i < MAX_BUFFER) && (temp[i++] = getchar()));
	if (MAX_BUFFER == i)
	{
	    temp[256] = 0;
	    __MrmExit(LOC, "String too complex: %s", temp);
	}
	temp[i] = 0;
	fprintf(stderr, "TAG = %s\n", temp);
	return (ReturnValue);
	break;
    case MrmRtypeKeysym:
    case MrmRtypeColor:
    case MrmRtypeAddrName:
	i = 0;			/* NAME(value) CHAR(0) */
	while ((i < MAX_BUFFER) && (temp[i++] = getchar()));
	if (MAX_BUFFER == i)
	{
	    temp[256] = 0;
	    __MrmExit(LOC, "String too complex: %s", temp);
	}
	temp[i] = 0;
	return (__MrmStore(temp));
	break;
    default:
	__MrmExit(LOC, "Unknown Attribute type: %d\n", c);
    }
#endif /* #if 0 */

}

static void 
Xm_List(Widget parent_widget, DummyWidget *DW, int module_id)
{
    Arg args[10];
    int n = 0;
    int i;
    int ListCount = 0;
    SymbolTableType *Next;
    XtArgVal value;
    char *temp;
    SymbolTableType *SymbolTable;
    XmString *ListItem;


    for (Next = DW->attributes; Next; Next = Next->Next)
    {
	switch (Next->type)
	{
	case MrmRtypeInteger:
	    value = (long)Next->value;
	    break;
	case MrmRtypeAddrName:
	    temp = Next->value;
	    if (NULL == (SymbolTable = LookUpAddr(temp, module_id)))
	    {
		__MrmLookUpPredefines(temp, &value);
		break;
	    }
	    if (0 == strcmp(Next->Name, "items"))
	    {
		char **strings = (char **)SymbolTable->value;

		while (*strings++)
		    ListCount++;
		ListItem = (XmString *)malloc(ListCount * sizeof(XmString));
		strings = (char **)SymbolTable->value;
		for (i = 0; i < ListCount; i++)
		{
		    char *sym = strings[i];
		    char s[256];

		    if ('~' == sym[0])
		    {
			temp = &sym[1];
			if (NULL ==
			    (SymbolTable = LookupValue(UilModule[module_id].SymbolTable,
						       temp)))
			{
			    if (NULL == (SymbolTable = LookupValue(GlobalSymbolTable, temp)))
			    {
				__MrmExit(LOC, "Could not find %s\n", temp);
			    }
			}
			strcpy(s, SymbolTable->value);
			ListItem[i] = XmStringCreate(s, XmFONTLIST_DEFAULT_TAG);
		    }
		    else
			ListItem[i] = XmStringCreate(strings[i], XmFONTLIST_DEFAULT_TAG);
		}
		value = (XtArgVal)ListItem;
	    }
	    else
		value = (XtArgVal)SymbolTable->value;
	    break;
	default:
	    __MrmExit(LOC, "UNKNOWN ATTRIBUTE\n");
	}
	XtSetArg(args[n], Next->Name, value);
	n++;
    };
    XtSetArg(args[n], XmNitemCount, ListCount);
    n++;
    XtSetValues(XtNameToWidget(parent_widget, "*List"), args, n);
}


static FILE *
UILOpen(char *tFileName, char *mode, FILE * old)
{
    FILE *Return = NULL;
    int i, j;
    char *theBase;
    char buffer[256];
    static char Eval = 0;
    char *Base[] =
    {
	"$HOME",
	X_LIB_PATH "/lib/X11",	/* X installation path */
	LESSTIFHOME "/lib/X11"	/* LessTif installation path */
    };
    char *Paths[] =
    {
	"%L/uid/%N/%U%S",
	"%l/uid/%N/%U%S",
	"uid/%N/%U%S",
	"%L/uid/%U%S",
	"%l/uid/%U%S",
	"uid/%U%S",
	"%U%S",
	"%T",
    };
    static int BaseSize = sizeof(Base) / sizeof(Base[0]);
    static int PathSize = sizeof(Paths) / sizeof(Paths[0]);
    char *UILPath;
    char FileName[256];

    for (i = 0; *tFileName; i++)
	FileName[i] = *tFileName++;
    FileName[i] = 0;
    if ((i < 4) || (strcmp(&FileName[i - 4], ".uid") != 0))
	strcat(FileName, ".uid");

    if (NULL != (Return = freopen(FileName, mode, old)))
    {
	fprintf(stderr, "Opening %s\n", FileName);
	return Return;
    }
    if (!Eval)
    {
	Eval = 1;
	for (j = 0; j < BaseSize; j++)
	{
	    theBase = Base[j];
	    if ('$' == theBase[0])
		Base[j] = getenv(&theBase[1]);
	    fprintf(stderr, "Base %d = %s\n", j, Base[j]);
	}
    }
    UILPath = getenv("UILPATH");
    if (NULL != UILPath)
    {
	for (i = j = 0;; j++, i++)
	{
	    if ((':' == UILPath[i]) || (0 == UILPath[i]))
	    {
		buffer[j++] = '/';
		strcpy(&buffer[j], FileName);
		j = -1;
		if (NULL != (Return = freopen(Substitute(buffer), mode, old)))
		{
		    __MrmWarn(LOC, "Opening %s\n", buffer);
		    return Return;
		}
		if (0 == UILPath[i])
		    break;
	    }
	    else
		buffer[j] = UILPath[i];
	}
    }
    for (j = 1; (NULL == Return) && (j < BaseSize); j++)
	if (Base[j])
	    for (i = 0, sprintf(buffer, "%s/%s/%s", Base[j], Paths[i], FileName);
		 (NULL == (Return = freopen(Substitute(buffer), mode, old))) &&
		 (i < PathSize);
		 i++, sprintf(buffer, "%s/%s/%s", Base[j], Paths[i], FileName))
	    {
	    };
    if (NULL != Return)
	__MrmWarn(LOC, "Opening %s\n", buffer);
    return Return;
}


static char *
Substitute(char *buffer)
{
    static char buffer2[256];
    char *string = buffer2;

    while (*buffer)
    {
	if ('%' == *buffer)
	{
	    buffer++;
	    switch (*buffer++)
	    {
	    case 'N':
		strcpy(string, theName);
		string += strlen(theName);
		break;
	    case 'U':
		strcpy(string, "FROM_LIST");	/* FIX ME */
		string += 9;
		break;
	    case 'T':
		strcpy(string, "uid");
		string += 3;
		break;
	    case 'S':
		strcpy(string, ".uid");
		string += 4;
		break;
	    case 'L':
		strcpy(string, "LANG");		/* FIX ME */
		string += 4;
		break;
	    case 'l':
		strcpy(string, "lang");		/* FIX ME */
		string += 4;
		break;
	    default:
		__MrmExit(LOC, "Unknown format\n");
	    }
	}
	else
	{
	    *string++ = *buffer++;
	}
    }
    *string = 0;
    return buffer2;
}


static void 
PrintSymbolTable(SymbolTableType *SymbolTable)
{
    for (; SymbolTable; SymbolTable = SymbolTable->Next)
	fprintf(stderr, "      %s\n", SymbolTable->Name);
}


static void 
FreeSymbolTable(SymbolTableType *t)
{
    SymbolTableType *n;

    while (t)
    {
	n = t->Next;
	free(t);
	t = n;
    }
}


static SymbolTableType *
LookUpAddr(char *temp, int module_id)
{
    SymbolTableType *SymbolTable;

    SymbolTable = LookupValue(UilModule[module_id].SymbolTable, temp);
    if (NULL == SymbolTable)
    {
	SymbolTable = LookupValue(GlobalSymbolTable, temp);
    }
    return SymbolTable;
}


extern Cardinal
MrmRegisterNamesInHierarchy(MrmHierarchy hierarchy_id,
			    MrmRegisterArglist reglist,
			    MrmCount num_reg)
{
    /* FIX ME */
    _XmWarning(NULL, "MrmRegisterNamesInHierarchy() is not yet implemented!");
    return MrmFAILURE;
}
