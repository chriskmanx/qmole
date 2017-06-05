/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/MapEvent.c,v 1.2 2004/10/27 19:31:15 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004 LessTif Development Team
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

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/MapEvent.c,v 1.2 2004/10/27 19:31:15 dannybackx Exp $";

/*
 * this code borrows heavily from TMparse.c.  The copyright there is as follows:
 *
 * Copyright 1987-88 by Digital Equipment Corporation, Maynard, Massachusetts,
 * and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
 *  
 *                         All Rights Reserved
 *  
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the names of Digital or MIT not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *  
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */
#include <LTconfig.h>

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>

#define XK_MISCELLANY
#define XK_LATIN1
#include <X11/keysymdef.h>

#include <XmI/DebugUtil.h>


#define COLON	':'

/*
 * some overrides
 */
#define XP		XtPointer
#define AnyButtonMask	(1 << 27) /* must be greater than masks in X.h */

typedef int		EventType;

typedef unsigned short	Value;

typedef struct _LateBindings
{
    unsigned int knot:1;
    unsigned int pair:1;
    unsigned short ref_count;   /* garbage collection */
    KeySym keysym;
}
LateBindings, *LateBindingsPtr;
 
typedef void (*ModifierProc)(Value value, LateBindingsPtr *lateBindings,
			     Boolean notFlag, Value *valueP);

typedef struct _ModifierRec
{
    char*      name;
    XrmQuark   signature;
    ModifierProc modifierParseProc;
    Value      value;
}
ModifierRec, *ModifierKeys;

typedef struct
{
    char	*name;
    XrmQuark	signature;
    Value	value;
}
NameValueRec, *NameValueTable;

typedef struct _EventRec
{
    unsigned long modifiers;
    unsigned long modifierMask;
    LateBindingsPtr lateModifiers;
    unsigned long eventType;
    unsigned long eventCode;
    unsigned long eventCodeMask;
    Boolean standard;
}
Event;
 
typedef struct _EventSeqRec *EventSeqPtr;
typedef struct _EventSeqRec
{
    Event event;        /* X event description */
    EventSeqPtr next;   /* next event on line */
}
EventSeqRec;
 
typedef EventSeqRec EventRec;
typedef EventSeqPtr EventPtr;

typedef String (*ParseProc)(String str, XP closure,
			    EventPtr event, Boolean* error);

typedef struct _EventKey
{
    char    	*event;
    XrmQuark	signature;
    EventType	eventType;
    ParseProc	parseDetail;
    XP		closure;
}
EventKey, *EventKeys;

static void ParseModImmed(Value value, LateBindingsPtr *lateBindings,
			  Boolean notFlag, Value *valueP);
static void ParseModSym(Value value, LateBindingsPtr *lateBindings,
			Boolean notFlag, Value *valueP);

static String PanicModeRecovery(String str);
static String ParseRepeat(String str, int *reps, Boolean *plus,
			  Boolean *error);
static KeySym StringToKeySym(String str, Boolean *error);

static ModifierRec modifiers[] =
{
    {"Shift",	0,	ParseModImmed,ShiftMask},
    {"Lock",	0,	ParseModImmed,LockMask},
    {"Ctrl",	0,	ParseModImmed,ControlMask},
    {"Mod1",	0,	ParseModImmed,Mod1Mask},
    {"Mod2",	0,	ParseModImmed,Mod2Mask},
    {"Mod3",	0,	ParseModImmed,Mod3Mask},
    {"Mod4",	0,	ParseModImmed,Mod4Mask},
    {"Mod5",	0,	ParseModImmed,Mod5Mask},
    {"Meta",	0,	ParseModSym,  XK_Meta_L},
    {"m",       0,      ParseModSym,  XK_Meta_L},
    {"h",       0,      ParseModSym,  XK_Hyper_L},
    {"su",      0,      ParseModSym,  XK_Super_L},
    {"a",       0,      ParseModSym,  XK_Alt_L},
    {"Hyper",   0,      ParseModSym,  XK_Hyper_L},
    {"Super",   0,      ParseModSym,  XK_Super_L},
    {"Alt",     0,      ParseModSym,  XK_Alt_L},
    {"Button1",	0,	ParseModImmed,Button1Mask},
    {"Button2",	0,	ParseModImmed,Button2Mask},
    {"Button3",	0,	ParseModImmed,Button3Mask},
    {"Button4",	0,	ParseModImmed,Button4Mask},
    {"Button5",	0,	ParseModImmed,Button5Mask},
    {"c",	0,	ParseModImmed,ControlMask},
    {"s",	0,	ParseModImmed,ShiftMask},
    {"l",	0,	ParseModImmed,LockMask},
};

static NameValueRec buttonNames[] =
{
    {"Button1",	0,		Button1},
    {"Button2", 0,		Button2},
    {"Button3", 0,		Button3},
    {"Button4", 0,		Button4},
    {"Button5", 0,		Button5},
    {NULL,	NULLQUARK,	0},
};

static NameValueRec motionDetails[] = {
    {"Normal",		0,	NotifyNormal},
    {"Hint",		0,	NotifyHint},
    {NULL, NULLQUARK, 0},
};

static NameValueRec notifyModes[] = {
    {"Normal",		0,	NotifyNormal},
    {"Grab",		0,	NotifyGrab},
    {"Ungrab",		0,	NotifyUngrab},
    {"WhileGrabbed",    0,	NotifyWhileGrabbed},
    {NULL, NULLQUARK, 0},
};

static NameValueRec mappingNotify[] = {
    {"Modifier",	0,		MappingModifier},
    {"Keyboard",	0,		MappingKeyboard},
    {"Pointer",		0,		MappingPointer},
    {NULL,		NULLQUARK,	0},
};

static String ParseKeySym(String str, XtPointer closure, EventPtr event,
			  Boolean *error);
static String ParseKeyAndModifiers(String str, XtPointer closure,
				   EventPtr event, Boolean *error);
static String ParseTable(String str, XtPointer closure, EventPtr event,
			 Boolean *error);
static String ParseImmed(String str, XtPointer closure, EventPtr event,
			 Boolean *error);
static String ParseAddModifier(String str, XtPointer closure, EventPtr event,
			       Boolean *error);
static String ParseNone(String str, XtPointer closure, EventPtr event,
			Boolean *error);
static String ParseAtom(String str, XtPointer closure, EventPtr event
			, Boolean *error);

static EventKey events[] =
{
/* Event Name,	  Quark, Event Type,	Detail Parser, Closure */
    {"KeyPress",    NULLQUARK, KeyPress, ParseKeySym,	NULL},
    {"Key", 	    NULLQUARK, KeyPress, ParseKeySym,	NULL},
    {"KeyDown",	    NULLQUARK, KeyPress, ParseKeySym,	NULL},
    {"Ctrl",        NULLQUARK, KeyPress, ParseKeyAndModifiers,(XP)ControlMask},
    {"Shift",       NULLQUARK, KeyPress, ParseKeyAndModifiers,(XP)ShiftMask},
    {"Meta",        NULLQUARK, KeyPress, ParseKeyAndModifiers,(XP)NULL},
    {"KeyUp",	    NULLQUARK, KeyRelease,	ParseKeySym,	NULL},
    {"KeyRelease",  NULLQUARK, KeyRelease,	ParseKeySym,	NULL},

    {"ButtonPress", NULLQUARK, ButtonPress,  ParseTable,(XP)buttonNames},
    {"BtnDown",	    NULLQUARK, ButtonPress,  ParseTable,(XP)buttonNames},
    {"Btn1Down",    NULLQUARK, ButtonPress,	ParseImmed,(XP)Button1},
    {"Btn2Down",    NULLQUARK, ButtonPress,	ParseImmed,(XP)Button2},
    {"Btn3Down",    NULLQUARK, ButtonPress,	ParseImmed,(XP)Button3},
    {"Btn4Down",    NULLQUARK, ButtonPress,	ParseImmed,(XP)Button4},
    {"Btn5Down",    NULLQUARK, ButtonPress,	ParseImmed,(XP)Button5},

/* Event Name,	  Quark, Event Type,	Detail Parser, Closure */

    {"ButtonRelease",   NULLQUARK, ButtonRelease,  ParseTable,(XP)buttonNames},
    {"BtnUp", 	    NULLQUARK, ButtonRelease,  ParseTable,(XP)buttonNames},
    {"Btn1Up", 	    NULLQUARK, ButtonRelease,    ParseImmed,(XP)Button1},
    {"Btn2Up", 	    NULLQUARK, ButtonRelease,    ParseImmed,(XP)Button2},
    {"Btn3Up", 	    NULLQUARK, ButtonRelease,    ParseImmed,(XP)Button3},
    {"Btn4Up", 	    NULLQUARK, ButtonRelease,    ParseImmed,(XP)Button4},
    {"Btn5Up", 	    NULLQUARK, ButtonRelease,    ParseImmed,(XP)Button5},

    {"MotionNotify",    NULLQUARK, MotionNotify, ParseTable, (XP)motionDetails},
    {"PtrMoved",  NULLQUARK, MotionNotify, ParseTable, (XP)motionDetails},
    {"Motion", 	  NULLQUARK, MotionNotify, ParseTable, (XP)motionDetails},
    {"MouseMoved",NULLQUARK, MotionNotify, ParseTable, (XP)motionDetails},
    {"BtnMotion", NULLQUARK, MotionNotify, ParseAddModifier, (XP)AnyButtonMask},
    {"Btn1Motion",NULLQUARK, MotionNotify, ParseAddModifier, (XP)Button1Mask},
    {"Btn2Motion",NULLQUARK, MotionNotify, ParseAddModifier, (XP)Button2Mask},
    {"Btn3Motion",NULLQUARK, MotionNotify, ParseAddModifier, (XP)Button3Mask},
    {"Btn4Motion",NULLQUARK, MotionNotify, ParseAddModifier, (XP)Button4Mask},
    {"Btn5Motion",NULLQUARK, MotionNotify, ParseAddModifier, (XP)Button5Mask},

    {"EnterNotify", NULLQUARK, EnterNotify,    ParseTable,(XP)notifyModes},
    {"Enter",	    NULLQUARK, EnterNotify,    ParseTable,(XP)notifyModes},
    {"EnterWindow", NULLQUARK, EnterNotify,    ParseTable,(XP)notifyModes},

    {"LeaveNotify", NULLQUARK, LeaveNotify,    ParseTable,(XP)notifyModes},
    {"LeaveWindow", NULLQUARK, LeaveNotify,    ParseTable,(XP)notifyModes},
    {"Leave",	    NULLQUARK, LeaveNotify,    ParseTable,(XP)notifyModes},

/* Event Name,	  Quark, Event Type,	Detail Parser, Closure */

    {"FocusIn",	    NULLQUARK, FocusIn,	       ParseTable,(XP)notifyModes},
    {"FocusOut",    NULLQUARK, FocusOut,       ParseTable,(XP)notifyModes},

    {"KeymapNotify",NULLQUARK, KeymapNotify,	ParseNone,	NULL},
    {"Keymap",	    NULLQUARK, KeymapNotify,	ParseNone,	NULL},

    {"Expose", 	    NULLQUARK, Expose,		ParseNone,	NULL},

    {"GraphicsExpose",  NULLQUARK, GraphicsExpose,	ParseNone,	NULL},
    {"GrExp",	        NULLQUARK, GraphicsExpose,	ParseNone,	NULL},

    {"NoExpose",    NULLQUARK, NoExpose,	ParseNone,	NULL},
    {"NoExp",	    NULLQUARK, NoExpose,	ParseNone,	NULL},

    {"VisibilityNotify",NULLQUARK, VisibilityNotify,ParseNone,	NULL},
    {"Visible",	        NULLQUARK, VisibilityNotify,ParseNone,	NULL},

    {"CreateNotify",NULLQUARK, CreateNotify,	ParseNone,	NULL},
    {"Create",	    NULLQUARK, CreateNotify,	ParseNone,	NULL},

/* Event Name,	  Quark, Event Type,	Detail Parser, Closure */

    {"DestroyNotify",   NULLQUARK, DestroyNotify,	ParseNone,	NULL},
    {"Destroy",	        NULLQUARK, DestroyNotify,	ParseNone,	NULL},

    {"UnmapNotify", NULLQUARK, UnmapNotify,	ParseNone,	NULL},
    {"Unmap",	    NULLQUARK, UnmapNotify,	ParseNone,	NULL},

    {"MapNotify",   NULLQUARK, MapNotify,	ParseNone,	NULL},
    {"Map",	    NULLQUARK, MapNotify,	ParseNone,	NULL},

    {"MapRequest",  NULLQUARK, MapRequest,	ParseNone,	NULL},
    {"MapReq",	    NULLQUARK, MapRequest,	ParseNone,	NULL},

    {"ReparentNotify",  NULLQUARK, ReparentNotify,	ParseNone,	NULL},
    {"Reparent",        NULLQUARK, ReparentNotify,	ParseNone,	NULL},

    {"ConfigureNotify", NULLQUARK, ConfigureNotify,	ParseNone,	NULL},
    {"Configure",       NULLQUARK, ConfigureNotify,	ParseNone,	NULL},

    {"ConfigureRequest",NULLQUARK, ConfigureRequest,ParseNone,	NULL},
    {"ConfigureReq",    NULLQUARK, ConfigureRequest,ParseNone,	NULL},

/* Event Name,	  Quark, Event Type,	Detail Parser, Closure */

    {"GravityNotify",   NULLQUARK, GravityNotify,	ParseNone,	NULL},
    {"Grav",	        NULLQUARK, GravityNotify,	ParseNone,	NULL},

    {"ResizeRequest",   NULLQUARK, ResizeRequest,	ParseNone,	NULL},
    {"ResReq",	        NULLQUARK, ResizeRequest,	ParseNone,	NULL},

    {"CirculateNotify", NULLQUARK, CirculateNotify,	ParseNone,	NULL},
    {"Circ",	        NULLQUARK, CirculateNotify,	ParseNone,	NULL},

    {"CirculateRequest",NULLQUARK, CirculateRequest,ParseNone,	NULL},
    {"CircReq",	        NULLQUARK, CirculateRequest,ParseNone,	NULL},

    {"PropertyNotify",  NULLQUARK, PropertyNotify,	ParseAtom,	NULL},
    {"Prop",	        NULLQUARK, PropertyNotify,	ParseAtom,	NULL},

    {"SelectionClear",  NULLQUARK, SelectionClear,	ParseAtom,	NULL},
    {"SelClr",	        NULLQUARK, SelectionClear,	ParseAtom,	NULL},

    {"SelectionRequest",NULLQUARK, SelectionRequest,ParseAtom,	NULL},
    {"SelReq",	        NULLQUARK, SelectionRequest,ParseAtom,	NULL},

/* Event Name,	  Quark, Event Type,	Detail Parser, Closure */

    {"SelectionNotify", NULLQUARK, SelectionNotify,	ParseAtom,	NULL},
    {"Select",	        NULLQUARK, SelectionNotify,	ParseAtom,	NULL},

    {"ColormapNotify",  NULLQUARK, ColormapNotify,	ParseNone,	NULL},
    {"Clrmap",	        NULLQUARK, ColormapNotify,	ParseNone,	NULL},

    {"ClientMessage",   NULLQUARK, ClientMessage,	ParseAtom,	NULL},
    {"Message",	        NULLQUARK, ClientMessage,	ParseAtom,	NULL},

    {"MappingNotify", NULLQUARK, MappingNotify, ParseTable, (XP)mappingNotify},
    {"Mapping",	      NULLQUARK, MappingNotify, ParseTable, (XP)mappingNotify},
};


#define ScanFor(str, ch) \
    while ((*(str) != (ch)) && (*(str) != '\0') && (*(str) != '\n')) (str)++

#define ScanNumeric(str)  while ('0' <= *(str) && *(str) <= '9') (str)++

#define ScanAlphanumeric(str) \
    while (('A' <= *(str) && *(str) <= 'Z') || \
           ('a' <= *(str) && *(str) <= 'z') || \
           ('0' <= *(str) && *(str) <= '9')) (str)++

#define ScanWhitespace(str) \
    while (*(str) == ' ' || *(str) == '\t') (str)++


static Boolean initialized = FALSE;

static XrmQuark QMeta;
static XrmQuark QCtrl;
static XrmQuark QNone;
static XrmQuark QAny;

static void
FreeEventSeq(EventSeqPtr eventSeq)
{
    EventSeqPtr evs = eventSeq;
 
    evs = eventSeq;
    while (evs != NULL)
    {
        EventPtr event = evs;
	if (event->event.lateModifiers) {
		XtFree((char *)event->event.lateModifiers);
	}

        evs = evs->next;

        if (evs == event)
	{
	    evs = NULL;
	}

	XtFree((char *)event);
    }
}


static void
CompileNameValueTable(NameValueTable table)
{
    int i;

    for (i=0; table[i].name; i++)
    {
        table[i].signature = XrmPermStringToQuark(table[i].name);
    }
}


static int
OrderEvents(const void *a1, const void *b1)
{
    EventKey *a = (EventKey *)a1, *b = (EventKey *)b1;

    return ((a->signature < b->signature) ? -1 : 1);
}


static void
Compile_XtEventTable(EventKeys table, Cardinal count)
{
    int i;
    EventKeys entry = table;

    for (i=count; --i >= 0; entry++)
    {
	entry->signature = XrmPermStringToQuark(entry->event);
    }

    qsort(table, count, sizeof(EventKey), OrderEvents);
}


static int
OrderModifiers(const void *a1, const void *b1)
{
    ModifierRec *a = (ModifierRec *)a1, *b = (ModifierRec *)b1;
    return ((a->signature < b->signature) ? -1 : 1);
}


static void
Compile_XtModifierTable(ModifierKeys table, Cardinal count)
{
    int i;
    ModifierKeys entry = table;

    for (i=count; --i >= 0; entry++)
    {
	entry->signature = XrmPermStringToQuark(entry->name);
    }

    qsort(table, count, sizeof(ModifierRec), OrderModifiers);
}


static String
PanicModeRecovery(String str)
{
    ScanFor(str,'\n');

    if (*str == '\n')
    {
	str++;
    }

    return str;
}


static void
Syntax(String str, String str1)
{
    _XmWarning(NULL, "%s %s\n", str, str1);
}


static Cardinal
LookupTMEventType(String eventStr, Boolean *error)
{
    int   i, left, right;
    XrmQuark	signature;
    static int 	previous = 0;

    if ((signature = XrmStringToQuark(eventStr)) == events[previous].signature)
    {
	return (Cardinal) previous;
    }

    left = 0;
    right = XtNumber(events) - 1;

    while (left <= right)
    {
	i = (left + right) >> 1;
	if (signature < events[i].signature)
	{
	    right = i - 1;
	}
	else if (signature > events[i].signature)
	{
	    left = i + 1;
	}
	else
	{
	    previous = i;
	    return (Cardinal) i;
	}
    }

    Syntax("Unknown event type :  ", eventStr);

    *error = TRUE;
    return (Cardinal) i;
}


/***********************************************************************
 * _XtLookupTableSym
 * Given a table and string, it fills in the value if found and returns
 * status
 ***********************************************************************/
static Boolean
_XtLookupTableSym(NameValueTable table, String name, Value *valueP)
{
    int i;
    XrmQuark signature = XrmStringToQuark(name);

    for (i=0; table[i].name != NULL; i++)
    {
	if (table[i].signature == signature)
	{
	    *valueP = table[i].value;
	    return TRUE;
	}
    }

    return FALSE;
}


static void
StoreLateBindings(KeySym keysymL, Boolean notL,
		  KeySym keysymR, Boolean notR,
		  LateBindingsPtr *lateBindings)
{
    LateBindingsPtr temp;
    Boolean pair = FALSE;
    unsigned long count, number;

    if (lateBindings != NULL)
    {
        temp = *lateBindings;
        if (temp != NULL)
	{
            for (count = 0; temp[count].keysym; count++)
	    {
		/*EMPTY*/
	    }
        }
        else
	{
	    count = 0;
	}

        if (!keysymR)
	{
             number = 1;
	     pair = FALSE;
        }
	else
	{
             number = 2;
	     pair = TRUE;
        }
          
        temp = (LateBindingsPtr)XtRealloc((char *)temp,
					  (unsigned)((count + number + 1) *
						sizeof(LateBindings)));
        *lateBindings = temp;
        temp[count].knot = notL;
        temp[count].pair = pair;

	if (count == 0)
	{
	    temp[count].ref_count = 1;
	}

        temp[count++].keysym = keysymL;
        if (keysymR)
	{
            temp[count].knot = notR;
            temp[count].pair = FALSE;
            temp[count++].keysym = keysymR;
        }

        temp[count].knot = FALSE;
        temp[count].keysym = 0;
    }
} 


static void
_XtParseKeysymMod(String name, LateBindingsPtr *lateBindings,
		  Boolean notFlag, Value *valueP, Boolean *error)
{
    KeySym keySym;

    keySym = StringToKeySym(name, error);
    *valueP = 0;

    if (keySym != NoSymbol)
    {
        StoreLateBindings(keySym, notFlag, (KeySym)NULL, FALSE, lateBindings);
    }
}

static Boolean
_XtLookupModifier(XrmQuark signature, LateBindingsPtr *lateBindings,
		  Boolean notFlag, Value *valueP, Bool constMask)
{
    int i, left, right;
    static int previous = 0;
   
    if (signature == modifiers[previous].signature)
    {
	if (constMask)
	{
	    *valueP = modifiers[previous].value;
	}
        else /* if (modifiers[previous].modifierParseProc) always true */
	{
	   (*modifiers[previous].modifierParseProc)(modifiers[previous].value,
						    lateBindings, notFlag,
						    valueP);
	}
	return TRUE;
    }

    left = 0;
    right = XtNumber(modifiers) - 1;
    while (left <= right)
    {
	i = (left + right) >> 1;
	if (signature < modifiers[i].signature)
	{
	    right = i - 1;
	}
	else if (signature > modifiers[i].signature)
	{
	    left = i + 1;
	}
	else
	{
	    previous = i;
	    if (constMask)
	    {
		*valueP = modifiers[i].value;
	    }
	    else /* if (modifiers[i].modifierParseProc) always true */
	    {
	       (*modifiers[i].modifierParseProc)(modifiers[i].value,
						 lateBindings, notFlag,
						 valueP);
	    }

	    return TRUE;
	}
    }

    return FALSE;
}


static String
ScanIdent(String str)
{
    ScanAlphanumeric(str);

    while (('A' <= *str && *str <= 'Z') ||
	   ('a' <= *str && *str <= 'z') ||
	   ('0' <= *str && *str <= '9') ||
	   (*str == '-') || (*str == '_') || (*str == '$'))
    {
	str++;
    }

    return str;
}


static String
FetchModifierToken(String str, XrmQuark *token_return)
{
    String start = str;

    if (*str == '$')
    {
        *token_return = QMeta;
        str++;

        return str;
    }

    if (*str == '^')
    {
        *token_return = QCtrl;
        str++;

        return str;
    }

    str = ScanIdent(str);
    if (start != str)
    {
	char modStr[100];
	memcpy(modStr, start, str-start);
	modStr[str-start] = '\0';
	*token_return = XrmStringToQuark(modStr);

	return str;
    }

    return str;
}        
    

static String
ParseModifiers(String str, EventPtr event, Boolean *error)
{
    String start;
    Boolean notFlag, exclusive, keysymAsMod;
    Value maskBit;
    XrmQuark Qmod;
 
    ScanWhitespace(str);
    start = str;
    str = FetchModifierToken(str, &Qmod);
    exclusive = FALSE;
    if (start != str)
    {
	if (Qmod == QNone)
	{
	    event->event.modifierMask = ~0;
	    event->event.modifiers = 0;
	    ScanWhitespace(str);

	    return str;
	}
	else if (Qmod == QAny) /*backward compatability*/
	{
	    event->event.modifierMask = 0;
	    event->event.modifiers = AnyModifier;
	    ScanWhitespace(str);

	    return str;
	}
	str = start; /*if plain modifier, reset to beginning */
    }
    else while (*str == '!' || *str == COLON)
    {
        if (*str == '!')
	{
             exclusive = TRUE;
             str++;
             ScanWhitespace(str);
        }

        if (*str == COLON)
	{
             event->event.standard = TRUE;
             str++;
             ScanWhitespace(str);
        }
    }
   
    while (*str != '<')
    {
        if (*str == '~')
	{
             notFlag = TRUE;
             str++;
        }
	else 
	{
             notFlag = FALSE;
	}

        if (*str == '@')
	{
            keysymAsMod = TRUE;
            str++;
        }
        else
	{
	    keysymAsMod = FALSE;
	}

	start = str;
        str = FetchModifierToken(str, &Qmod);
        if (start == str)
	{
            Syntax("Modifier or '<' expected","");
            *error = TRUE;
            return PanicModeRecovery(str);
        }

        if (keysymAsMod)
	{
            _XtParseKeysymMod(XrmQuarkToString(Qmod),
			      &event->event.lateModifiers,
			      notFlag,&maskBit, error);
	    if (*error)
	    {
                 return PanicModeRecovery(str);
	    }
        }
	else if (!_XtLookupModifier(Qmod, &event->event.lateModifiers,
				    notFlag, &maskBit, FALSE))
	{
	     Syntax("Unknown modifier name:  ", XrmQuarkToString(Qmod));
             *error = TRUE;
             return PanicModeRecovery(str);
	}

        event->event.modifierMask |= maskBit;
	if (notFlag)
	{
	    event->event.modifiers &= ~maskBit;
	}
	else
	{
	    event->event.modifiers |= maskBit;
	}

        ScanWhitespace(str);
    }

    if (exclusive)
    {
	event->event.modifierMask = ~0;
    }

    return str;
}


static String
ParseXtEventType(String str, EventPtr event, Cardinal *tmEventP, Boolean *error)
{
    String start = str;
    char eventTypeStr[100];

    ScanAlphanumeric(str);
    memcpy(eventTypeStr, start, str-start);
    eventTypeStr[str-start] = '\0';
    *tmEventP = LookupTMEventType(eventTypeStr,error);
    if (*error) 
    {
        return PanicModeRecovery(str);
    }

    event->event.eventType = events[*tmEventP].eventType;

    return str;
}


static unsigned long
StrToHex(String str)
{
    char   c;
    unsigned long    val = 0;

    while ((c = *str) != 0)
    {
	if ('0' <= c && c <= '9') val = val*16+c-'0';
	else if ('a' <= c && c <= 'z') val = val*16+c-'a'+10;
	else if ('A' <= c && c <= 'Z') val = val*16+c-'A'+10;
	else return 0;

	str++;
    }

    return val;
}


static unsigned long
StrToOct(String str)
{
    char c;
    unsigned long  val = 0;

    while ((c = *str) != 0)
    {
        if ('0' <= c && c <= '7') val = val*8+c-'0'; else return 0;
	str++;
    }

    return val;
}


static unsigned long
StrToNum(String str)
{
    char c;
    unsigned long val = 0;

    if (*str == '0')
    {
	str++;
	if (*str == 'x' || *str == 'X') return StrToHex(++str);
	else return StrToOct(str);
    }

    while ((c = *str) != 0)
    {
	if ('0' <= c && c <= '9') val = val*10+c-'0';
	else return 0;

	str++;
    }

    return val;
}


static KeySym
StringToKeySym(String str, Boolean *error)
{
    KeySym k;

    if (str == NULL || *str == '\0')
    {
	return (KeySym)0;
    }


#ifndef NOTASCII
    /* special case single character ASCII, for speed */
    if (*(str+1) == '\0')
    {
	if (' ' <= *str && *str <= '~')
	{
	    return XK_space + (*str - ' ');
	}
    }
#endif

    if ('0' <= *str && *str <= '9')
    {
	return (KeySym) StrToNum(str);
    }

    k = XStringToKeysym(str);
    if (k != NoSymbol)
    {
	return k;
    }

#ifdef NOTASCII
    /* fall-back case to preserve backwards compatibility; no-one
     * should be relying upon this!
     */
    if (*(str+1) == '\0')
    {
	return (KeySym) *str;
    }
#endif

    Syntax("Unknown keysym name: ", str);
    *error = True;
    return NoSymbol;
}


static void
ParseModImmed(Value value, LateBindingsPtr *lateBindings,
	      Boolean notFlag, Value *valueP)
{
    *valueP = value;
}


/*
 * is only valid with keysyms that have an _L and _R in their name;
 * and ignores keysym lookup errors (i.e. assumes only valid keysyms)
 */
static void
ParseModSym(Value value, LateBindingsPtr *lateBindings,
	    Boolean notFlag, Value *valueP)
{
    KeySym keysymL = (KeySym)value;
    KeySym keysymR = keysymL + 1; /* valid for supported keysyms */

    StoreLateBindings(keysymL, notFlag, keysymR, notFlag, lateBindings);

    *valueP = 0;
}


static String
ParseImmed(String str, XP closure, EventPtr event, Boolean *error)
{
    event->event.eventCode = (unsigned long)closure;
    event->event.eventCodeMask = (unsigned long)~0L;

    return str;
}


static String
ParseAddModifier(String str, XP closure, EventPtr event, Boolean *error)
{
    unsigned long modval = (unsigned long)closure;
    event->event.modifiers |= modval;

    if (modval != AnyButtonMask) /* AnyButtonMask is don't-care mask */
    {
	event->event.modifierMask |= modval;
    }

    return str;
}


static String
ParseKeyAndModifiers(String str, XP closure, EventPtr event, Boolean *error)
{
    str = ParseKeySym(str, closure, event,error);

    if ((unsigned long) closure == 0)
    {
	Value metaMask; /* unused */
	(void) _XtLookupModifier(QMeta, &event->event.lateModifiers, False,
				 &metaMask, False);
    }
    else
    {
	event->event.modifiers |= (unsigned long) closure;
	event->event.modifierMask |= (unsigned long) closure;
    }

    return str;
}


static String
ParseKeySym(String str, XP closure, EventPtr event, Boolean *error)
{
    char *start;
    char keySymName[100];

    ScanWhitespace(str);

    if (*str == '\\')
    {
	str++;
	keySymName[0] = *str;
	if (*str != '\0' && *str != '\n')
	{
	    str++;
	}
	keySymName[1] = '\0';
	event->event.eventCode = StringToKeySym(keySymName, error);
	event->event.eventCodeMask = ~0L;
    }
    else if (*str == ',' || *str == COLON ||
             /* allow leftparen to be single char symbol,
              * for backwards compatibility
              */
             (*str == '(' && *(str+1) >= '0' && *(str+1) <= '9'))
    {
	/* no detail */
	event->event.eventCode = 0L;
        event->event.eventCodeMask = 0L;
    }
    else
    {
	start = str;
	while (*str != ',' && *str != COLON && *str != ' ' && *str != '\t' &&
	       *str != '\n' && *str != 0 &&
	       (*str != '(' || *(str+1) <= '0' || *(str+1) >= '9') &&
	       *str != '\0')
	{
	    str++;
	}

	memcpy(keySymName, start, str-start);

	keySymName[str-start] = '\0';

	event->event.eventCode = StringToKeySym(keySymName, error);
	event->event.eventCodeMask = ~0L;
    }
    if (*error)
    {
	if (keySymName[0] == '<')
	{
	    /* special case for common error */
	    _XmWarning(NULL, "missing comma in event sequence.\n");
	}

	return PanicModeRecovery(str);
    }

    return str;
}


static String
ParseTable(String str, XP closure, EventPtr event, Boolean *error)
{
    String start = str;
    char tableSymName[100];

    event->event.eventCode = 0L;
    ScanAlphanumeric(str);
    if (str == start)
    {
	event->event.eventCodeMask = 0L;
	return str;
    }

    if (str-start >= 99)
    {
	Syntax("Invalid Detail Type (string is too long).", "");
	*error = TRUE;

	return str;
    }

    memcpy(tableSymName, start, str-start);
    tableSymName[str-start] = '\0';

    if (!_XtLookupTableSym((NameValueTable)closure, tableSymName, 
			   (Value *)&event->event.eventCode))
    {
	Syntax("Unknown Detail Type:  ",tableSymName);
        *error = TRUE;

        return PanicModeRecovery(str);
    }

    event->event.eventCodeMask = ~0L;

    return str;
}


static String
ParseNone(String str, XP closure, EventPtr event, Boolean *error)
{
    event->event.eventCode = 0;
    event->event.eventCodeMask = 0;

    return str;
}


static String
ParseAtom(String str, XP closure, EventPtr event, Boolean *error)
{
    ScanWhitespace(str);

    if (*str == ',' || *str == COLON || *str == 0)
    {
	/* no detail */
	event->event.eventCode = 0L;
        event->event.eventCodeMask = 0L;
    }
    else
    {
	char *start, atomName[1000];
	start = str;

	while (*str != ',' && *str != COLON && *str != ' ' && *str != '\t' &&
	       *str != '\n' && *str != '\0')
	{
	    str++;
	}

	if (str-start >= 999)
	{
	    Syntax( "Atom name must be less than 1000 characters long.", "" );
	    *error = TRUE;
	    return str;
	}

	memcpy(atomName, start, str-start);
	atomName[str-start] = '\0';
	event->event.eventCode = XrmStringToQuark(atomName);
    }

    return str;
}


static short buttonModifierMasks[] =
{
    0, Button1Mask, Button2Mask, Button3Mask, Button4Mask, Button5Mask
};


static String
ParseEvent(String str, EventPtr event, int *reps, Boolean *plus, Boolean *error)
{
    Cardinal	tmEvent;

    str = ParseModifiers(str, event,error);

    if (*error)
    {
	return str;
    }

    if (*str != '<')
    {
         Syntax("Missing '<' while parsing event type.",""); 
         *error = TRUE;
         return PanicModeRecovery(str);
    }
    else
    {
	str++;
    }

    str = ParseXtEventType(str, event, &tmEvent,error);
    if (*error)
    {
	return str;
    }

    if (*str != '>')
    {
         Syntax("Missing '>' while parsing event type","");
         *error = TRUE;

         return PanicModeRecovery(str);
    }
    else
    {
	str++;
    }

    if (*str == '(')
    {
	str = ParseRepeat(str, reps, plus, error);
	if (*error)
	{
	    return str;
	}
    }

    str = (*(events[tmEvent].parseDetail))(str, events[tmEvent].closure,
					   event, error);
    if (*error)
    {
	return str;
    }

    /* gross hack! ||| this kludge is related to the X11 protocol deficiency
     * w.r.t. modifiers in grabs.
     */
    if ((event->event.eventType == ButtonRelease) &&
	(event->event.modifiers | event->event.modifierMask) /* any */ &&
	(event->event.modifiers != AnyModifier))
    {
	event->event.modifiers |= buttonModifierMasks[event->event.eventCode];
	/* the button that is going up will always be in the modifiers... */
    }

    return str;
}


static String
ParseQuotedStringEvent(String str, EventPtr event, Boolean *error)
{
    Value metaMask;
    char	s[2];

    if (*str=='^')
    {
	str++;
	event->event.modifiers = ControlMask;
    }
    else if (*str == '$')
    {
	str++;

	(void) _XtLookupModifier(QMeta, &event->event.lateModifiers, FALSE,
				 &metaMask, FALSE);
    }

    if (*str == '\\')
    {
	str++;
    }

    s[0] = *str;
    s[1] = '\0';
    if (*str != '\0' && *str != '\n')
    {
	str++;
    }

    event->event.eventType = KeyPress;
    event->event.eventCode = StringToKeySym(s, error);
    if (*error)
    {
	return PanicModeRecovery(str);
    }

    event->event.eventCodeMask = ~0L;
    event->event.standard = True;

    return str;
}

#define _XtEventTimerEventType ((unsigned long)~0L)

static EventSeqRec timerEventRec =
{
    {0, 0, NULL, _XtEventTimerEventType, 0L, 0L, FALSE},
    NULL
};


static void
RepeatDown(EventPtr *eventP, int reps)
{
    EventRec upEventRec;
    EventPtr event, downEvent;
    EventPtr upEvent = &upEventRec;
    int i;

    downEvent = event = *eventP;
    *upEvent = *downEvent;
    upEvent->event.eventType = ((event->event.eventType == ButtonPress)
				? ButtonRelease
				: KeyRelease);

    if ((upEvent->event.eventType == ButtonRelease) &&
	(upEvent->event.modifiers != AnyModifier) &&
	(upEvent->event.modifiers | upEvent->event.modifierMask))
    {
	upEvent->event.modifiers |= buttonModifierMasks[event->event.eventCode];
    }

    if (event->event.lateModifiers)
    {
	event->event.lateModifiers->ref_count += (reps - 1) * 2;
    }

    for (i=1; i<reps; i++)
    {
	/* up */
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = *upEvent;

	/* timer */
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = timerEventRec;

	/* down */
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = *downEvent;

    }

    event->next = NULL;
    *eventP = event;
}


static void
RepeatDownPlus(EventPtr *eventP, int reps)
{
    EventRec upEventRec;
    EventPtr event, downEvent, lastDownEvent = NULL;
    EventPtr upEvent = &upEventRec;
    int i;

    downEvent = event = *eventP;
    *upEvent = *downEvent;
    upEvent->event.eventType = ((event->event.eventType == ButtonPress)
				? ButtonRelease
				: KeyRelease);

    if ((upEvent->event.eventType == ButtonRelease) &&
	(upEvent->event.modifiers != AnyModifier) &&
	(upEvent->event.modifiers | upEvent->event.modifierMask))
    {
	upEvent->event.modifiers |= buttonModifierMasks[event->event.eventCode];
    }

    if (event->event.lateModifiers)
    {
	event->event.lateModifiers->ref_count += reps * 2 - 1;
    }

    for (i=0; i<reps; i++)
    {
	if (i > 0)
	{
	    /* down */
	    event->next = XtNew(EventSeqRec);
	    event = event->next;
	    *event = *downEvent;
	}
	lastDownEvent = event;

	/* up */
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = *upEvent;

	/* timer */
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = timerEventRec;

    }

    event->next = lastDownEvent;
    *eventP = event;
}


static void
RepeatUp(EventPtr *eventP, int reps)
{
    EventRec upEventRec;
    EventPtr event, downEvent;
    EventPtr upEvent = &upEventRec;
    int i;

    /* the event currently sitting in *eventP is an "up" event */
    /* we want to make it a "down" event followed by an "up" event, */
    /* so that sequence matching on the "state" side works correctly. */

    downEvent = event = *eventP;
    *upEvent = *downEvent;
    downEvent->event.eventType = ((event->event.eventType == ButtonRelease)
					? ButtonPress
					: KeyPress);

    if ((downEvent->event.eventType == ButtonPress) &&
	(downEvent->event.modifiers != AnyModifier) &&
	(downEvent->event.modifiers | downEvent->event.modifierMask))
    {
	downEvent->event.modifiers &=
		~buttonModifierMasks[event->event.eventCode];
    }

    if (event->event.lateModifiers)
    {
	event->event.lateModifiers->ref_count += reps * 2 - 1;
    }

    /* up */
    event->next = XtNew(EventSeqRec);
    event = event->next;
    *event = *upEvent;

    for (i=1; i<reps; i++)
    {
	/* timer */
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = timerEventRec;

	/* down */
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = *downEvent;

	/* up */
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = *upEvent;
    }

    event->next = NULL;
    *eventP = event;
}


static void
RepeatUpPlus(EventPtr *eventP, int reps)
{
    EventRec upEventRec;
    EventPtr event, downEvent, lastUpEvent = NULL;
    EventPtr upEvent = &upEventRec;
    int i;

    /* the event currently sitting in *eventP is an "up" event */
    /* we want to make it a "down" event followed by an "up" event, */
    /* so that sequence matching on the "state" side works correctly. */

    downEvent = event = *eventP;
    *upEvent = *downEvent;
    downEvent->event.eventType = ((event->event.eventType == ButtonRelease)
					? ButtonPress
					: KeyPress);

    if ((downEvent->event.eventType == ButtonPress) &&
	(downEvent->event.modifiers != AnyModifier) &&
	(downEvent->event.modifiers | downEvent->event.modifierMask))
    {
	downEvent->event.modifiers &=
		~buttonModifierMasks[event->event.eventCode];
    }

    if (event->event.lateModifiers)
    {
	event->event.lateModifiers->ref_count += reps * 2;
    }

    for (i=0; i<reps; i++)
    {
	/* up */
	event->next = XtNew(EventSeqRec);
	lastUpEvent = event = event->next;
	*event = *upEvent;

	/* timer */
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = timerEventRec;

	/* down */
	event->next = XtNew(EventSeqRec);
        event = event->next;
	*event = *downEvent;
    }

    event->next = lastUpEvent;
    *eventP = event;
}


static void
RepeatOther(EventPtr *eventP, int reps)
{
    EventPtr event, tempEvent;
    int i;

    tempEvent = event = *eventP;

    if (event->event.lateModifiers)
    {
	event->event.lateModifiers->ref_count += reps - 1;
    }

    for (i=1; i<reps; i++)
    {
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = *tempEvent;
    }

    *eventP = event;
}


static void
RepeatOtherPlus(EventPtr *eventP, int reps)
{
    EventPtr event, tempEvent;
    int i;

    tempEvent = event = *eventP;

    if (event->event.lateModifiers)
    {
	event->event.lateModifiers->ref_count += reps - 1;
    }

    for (i=1; i<reps; i++)
    {
	event->next = XtNew(EventSeqRec);
	event = event->next;
	*event = *tempEvent;
    }

    event->next = event;
    *eventP = event;
}


static void
RepeatEvent(EventPtr *eventP, int reps, Boolean plus)
{
    switch ((*eventP)->event.eventType)
    {
    case ButtonPress:
    case KeyPress:
	if (plus) RepeatDownPlus(eventP, reps);
	else RepeatDown(eventP, reps);
	break;

    case ButtonRelease:
    case KeyRelease:
	if (plus) RepeatUpPlus(eventP, reps);
	else RepeatUp(eventP, reps);
	break;

    default:
	if (plus) RepeatOtherPlus(eventP, reps);
	else RepeatOther(eventP, reps);
    }
}


static String
ParseRepeat(String str, int *reps, Boolean *plus, Boolean *error)
{

    /*** Parse the repetitions, for double click etc... ***/
    if (*str != '(' || !(isdigit(str[1]) || str[1] == '+' || str[1] == ')'))
    {
	return str;
    }

    str++;
    if (isdigit(*str))
    {
	String start = str;
	char repStr[7];
	size_t len;

	ScanNumeric(str);
	len = (str - start);
	if (len < sizeof(repStr))
	{
	    memcpy(repStr, start, len);
	    repStr[len] = '\0';
	    *reps = StrToNum(repStr);
	}
	else
	{
	    Syntax("Repeat count too large.", "");
	    *error = True;
	    return str;
	}
    }

    if (*reps == 0)
    {
	Syntax("Missing repeat count.","");
	*error = True;
	return str;
    }

    if (*str == '+')
    {
	*plus = TRUE;
	str++;
    }

    if (*str == ')')
    {
	str++;
    }
    else
    {
	Syntax("Missing ')'.","");
	*error = True;
    }

    return str;
}


/***********************************************************************
 * ParseEventSeq
 * Parses the left hand side of a translation table production
 * up to, and consuming the ":".
 * Takes a pointer to a char* (where to start parsing) and returns an
 * event seq (in a passed in variable), having updated the String 
 **********************************************************************/
static String
ParseEventSeq(String str, EventSeqPtr *eventSeqP, Boolean *error)
{
    EventSeqPtr *nextEvent = eventSeqP;

    *eventSeqP = NULL;

    while ( *str != '\0' && *str != '\n')
    {
	static Event nullEvent = {0, 0, (LateBindingsPtr)NULL, 0, 0L, 0L, FALSE};
	EventPtr     event;

	ScanWhitespace(str);

	if (*str == '"')
	{
	    str++;

	    while (*str != '"' && *str != '\0' && *str != '\n')
	    {
                event = (EventPtr)XtNew(EventRec);
                event->event = nullEvent;
                event->next = NULL;
		str = ParseQuotedStringEvent(str, event,error);
		if (*error)
		{
		    _XmWarning(NULL,
			       "non-Latin1 character in quoted string.\n");

		    return PanicModeRecovery(str);
		}

		*nextEvent = event;
		nextEvent = &event->next;
	    }
	    if (*str != '"')
	    {
                Syntax("Missing '\"'.","");
                *error = TRUE;
                return PanicModeRecovery(str);
            }
            else
	    {
		str++;
	    }
	}
	else
	{
	    int reps = 0;
	    Boolean plus = False;

            event = (EventPtr)XtNew(EventRec);
            event->event = nullEvent;
            event->next = NULL;

	    str = ParseEvent(str, event, &reps, &plus, error);
            if (*error)
	    {
		return str;
	    }

	    *nextEvent = event;

	    if (reps > 1 || plus)
	    {
		RepeatEvent(&event, reps, plus);
	    }

	    nextEvent = &event->next;
	}

	ScanWhitespace(str);
        if (*str == COLON || *str == 0)
	{
	    break;
	}
        else
	{
            if (*str != ',')
	    {
                Syntax("',' or 0 expected while parsing event sequence.","");
                *error = TRUE;

                return PanicModeRecovery(str);
	    }
	    else
	    {
		str++;
	    }
        }
    }

    if (*str != COLON && *str != 0)
    {
        Syntax("Missing 0 after event sequence.",""); 
        *error = TRUE;

        return PanicModeRecovery(str);
    }
    else
    {
	str++;
    }

    return str;
}


static void
PrintLateBindings(LateBindingsPtr late)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "LateBindings: knot: %d pair: %d, ref: %d KeySym %04x\n",
		      late->knot, late->pair, late->ref_count, late->keysym));
}


static void
PrintEvent(Event *event)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "Event: modifiers: %08x modMask: %08x late: %p\n",
		      event->modifiers, event->modifierMask,
		      event->lateModifiers));
    DEBUGOUT(_LtDebug(__FILE__, NULL,
		      "       eventType: %08x eventCode: %08x codeMask: %08x\n",
		      event->eventType, event->eventCode,
		      event->eventCodeMask));
    DEBUGOUT(_LtDebug(__FILE__, NULL, "       standard: %d\n",
		      event->standard));

    if (event->lateModifiers)
    {
	PrintLateBindings(event->lateModifiers);
    }
}


static void
PrintEventSeq(EventSeqPtr evs)
{
    EventSeqPtr tmp;

    DEBUGOUT(_LtDebug(__FILE__, NULL, "EventSequence:\n"));
    for (tmp = evs; tmp != NULL; tmp = tmp->next)
    {
	PrintEvent(&evs->event);
    }
}


static void
_XmTransParseInit(void)
{
    if (initialized)
    {
	return;
    }

    initialized = TRUE;

    QMeta = XrmPermStringToQuark("Meta");
    QCtrl = XrmPermStringToQuark("Ctrl");
    QNone = XrmPermStringToQuark("None");
    QAny  = XrmPermStringToQuark("Any");

    Compile_XtEventTable( events, XtNumber(events) );
    Compile_XtModifierTable( modifiers, XtNumber(modifiers) );

    CompileNameValueTable( buttonNames );
    CompileNameValueTable( notifyModes );
    CompileNameValueTable( motionDetails );
    CompileNameValueTable( mappingNotify );
}


/*
 * SetLateModifier -- set up the modifier masks for ALT, META, SUPER,
 * and HYPER modifier keys.  If the modifier is not bound to a
 * particular KeySym, return False; upon success, return True.
 *
 * For compatibility with the closed software foundation, we treat META
 * and ALT as synonyms of Mod1 if not bound to a particular keysym.
 */
static Boolean 
SetLateModifier(Display *display, EventSeqPtr evs)
{
    if (evs->event.lateModifiers)
    {
	unsigned long mask = 0;
	XmModifierMaskSetReference mods;

	mods = _XmGetModifierMappingsForDisplay(display);

	switch (evs->event.lateModifiers->keysym)
	{
        case XK_Meta_L:
        case XK_Meta_R:
            mask = mods[METAModifier];
	    if (mask == 0)
		mask = Mod1Mask;
            break;
 
        case XK_Alt_L:
        case XK_Alt_R:
	    mask = mods[ALTModifier];
	    if (mask == 0)
		mask = Mod1Mask;
            break;
 
        case XK_Super_L:
        case XK_Super_R:
	    mask = mods[SUPERModifier];
            break;

        case XK_Hyper_L:
        case XK_Hyper_R:
	    mask = mods[HYPERModifier];
	    break;

	case 0:
	    /* this is also valid */
	    return False;

	default:
	    _XmWarning(NULL, "Unknown modifier.\n");
	    return True;
	}

	if (mask == 0)
	{
	    /* There is no keysym associated with this modifier. */
	    /* Should we issue a warning here? -- FIXME */
	    return True;
	}
	else
	{
	    evs->event.modifiers |= mask;
	}
    }

    return False;
}


Boolean
_XmMapBtnEvent(String str, int *eventType,
	       unsigned int *button, unsigned int *modifiers)
{
    EventSeqPtr evs = NULL;
    Boolean err = False;

    _XmTransParseInit();

    DEBUGOUT(_LtDebug(__FILE__, NULL, "MAP KEY: %s\n", str));

    ParseEventSeq(str, &evs, &err);

    if (evs == NULL)
    {
	FreeEventSeq(evs);

	return False;
    }

    if (evs->next != NULL)
    {
	_XmWarning(NULL, "Multiple event sequence ignored.\n");
    }

    err = SetLateModifier(_XmGetDefaultDisplay(), evs);
    if (err)
    {
	FreeEventSeq(evs);

	return False;
    }

    PrintEventSeq(evs);

    if (evs->event.eventType != ButtonPress &&
	evs->event.eventType != ButtonRelease &&
	evs->event.eventType != MotionNotify)
    {
	FreeEventSeq(evs);

	return False;
    }

    *eventType = evs->event.eventType;
    *button = evs->event.eventCode;
    *modifiers = evs->event.modifiers;

    FreeEventSeq(evs);

    return True;
}


extern Boolean
_XmMapKeyEvent(String str, int *eventType,
	       unsigned *keysym, unsigned int *modifiers)
{
	EventSeqPtr evs = NULL;
	Boolean err = False;

	_XmTransParseInit();

	DEBUGOUT(_LtDebug(__FILE__, NULL, "MAP KEY: %s\n", str));

	ParseEventSeq(str, &evs, &err);
	if (evs == NULL) {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "MapKey got NULL sequence:\n"));
		FreeEventSeq(evs);
		return False;
	}

	if (evs->next != NULL) {
		_XmWarning(NULL, "Multiple event sequence ignored.\n");
	}

	err = SetLateModifier(_XmGetDefaultDisplay(), evs);
	if (err) {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "MapKey got err sequence:\n"));
		FreeEventSeq(evs);
		return False;
	}

	PrintEventSeq(evs);
	if (evs->event.eventType != KeyPress && evs->event.eventType != KeyRelease) {
		FreeEventSeq(evs);
		return False;
	}

	*eventType = evs->event.eventType;
	*keysym = evs->event.eventCode;
	*modifiers = evs->event.modifiers;

	FreeEventSeq(evs);
	return True;
}


/*
 * This function is documented in http://motifdeveloper.com/tips/tip15.html
 */
extern int
_XmMapKeyEvents(String str, int **eventType, KeySym **keysym, Modifiers **modifiers)
{
	EventSeqPtr evs = NULL, e;
	Boolean err = False;
	char	*r;
	int	i, cnt;

	_XmTransParseInit();

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmMapKeyEvents(%s)\n", str));

	r = ParseEventSeq(str, &evs, &err);
	if (evs == NULL) {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "MapKey got NULL sequence:\n"));
		FreeEventSeq(evs);
		return 0;
	}

	err = SetLateModifier(_XmGetDefaultDisplay(), evs);
	if (err) {
		DEBUGOUT(_LtDebug(__FILE__, NULL, "MapKey got err sequence:\n"));
		FreeEventSeq(evs);
		return 0;
	}

	PrintEventSeq(evs);
	if (evs->event.eventType != KeyPress && evs->event.eventType != KeyRelease) {
		FreeEventSeq(evs);
		return 0;
	}

	/* Count the number of elements */
	for (cnt=0, e=evs; e; e=e->next)
		cnt++;

	/* Allocate space */
	*eventType = (int *)XtCalloc(cnt+1, sizeof(int));
	*keysym = (KeySym *)XtCalloc(cnt+1, sizeof(KeySym));
	*modifiers = (Modifiers *)XtCalloc(cnt+1, sizeof(Modifiers));

	/* Copy the results in the allocated space */
	i = 0;
	e = evs;
	do {
		(*eventType)[i] = e->event.eventType;
		(*keysym)[i] = e->event.eventCode;
		(*modifiers)[i] = e->event.modifiers;
		i++;
		e = e->next;
	} while (e);

	FreeEventSeq(evs);
	return i;
}

Boolean
_XmMatchBtnEvent(XEvent *event, int eventType,
		 unsigned int button, unsigned int modifiers)
{
    return False;
}


Boolean
_XmMatchKeyEvent(XEvent *event, int eventType,
		 unsigned int key, unsigned int modifiers)
{
    return False;
}
