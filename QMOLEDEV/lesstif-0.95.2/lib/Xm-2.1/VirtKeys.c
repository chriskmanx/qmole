/**
 *
 * $Id: VirtKeys.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $
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
 **/

static const char rcsid[] = "$Id: VirtKeys.c,v 1.1 2004/08/28 19:22:46 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <X11/keysym.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/AtomMgr.h>
#include <Xm/DisplayP.h>
#include <Xm/MwmUtil.h>
#include <Xm/VirtKeysP.h>
#include <Xm/TransltnsP.h>

#include <Xm/XmosP.h>

#include <XmI/DebugUtil.h>


/*
 * Order of binding precedences:
 *  1) defaultVirtualBindings resource
 *  2) _MOTIF_BINDINGS property on root window
 *  3) _MOTIF_DEFAULT_BINDINGS property on root window
 *  4) .motifbind in $(HOME)
 *  5) xmbind.alias in $(HOME)
 *     look for "<VENDOR> <VERSION>"pathname, and load that file
 *  6) if all else fails, look for xmbind.alias in XMBINDDIR,
 *     or /usr/lib/Xm/bindings if XMBINDDIR is not set.
 *  7) otherwise, load fallback bindings.
 * For 4) till 7) put the bindings in the _MOTIF_BINDINGS property on
 * the root window.
 */

/*
 * The properties we have to deal with...
 */
#define DEFAULT_BINDINGS_PROPERTY_NAME _XA_MOTIF_DEFAULT_BINDINGS
#define BINDINGS_PROPERTY_NAME         _XA_MOTIF_BINDINGS


XmConst char _XmVirtKeys_hpFallbackBindingString[] = "\
osfAddMode    : Shift<Key>F8   \n\
osfBackSpace  : <Key>BackSpace \n\
osfBeginLine  : <Key>Home      \n\
osfCancel     : <Key>Escape    \n\
osfClear      : <Key>Clear     \n\
osfDelete     : <Key>DeleteChar\n\
osfEndLine    : <Key>F7        \n\
osfHelp       : <Key>F1        \n\
osfInsert     : <Key>InsertChar\n\
osfLeft       : <Key>Left      \n\
osfMenu       : Shift<Key>F10  \n\
osfMenuBar    : <Key>F10       \n\
osfPageDown   : <Key>Next      \n\
osfPageUp     : <Key>Prior     \n\
osfPrimaryPaste:<Key>InsertLine\n\
osfQuickPaste : <Key>DeleteLine\n\
osfRight      : <Key>Right     \n\
osfSelect     : <Key>Select    \n\
osfUndo       : <Key>Undo      \n\
osfUp         : <Key>Up        \n\
osfDown       : <Key>Down      ";


XmConst char _XmVirtKeys_ibmFallbackBindingString[] = "\
osfAddMode    : Shift<Key>F8   \n\
osfBackSpace  : <Key>BackSpace \n\
osfBeginLine  : <Key>Home      \n\
osfCancel     : <Key>Escape    \n\
osfDelete     : <Key>Delete    \n\
osfEndLine    : <Key>End       \n\
osfHelp       : <Key>F1        \n\
osfInsert     : <Key>Insert    \n\
osfLeft       : <Key>Left      \n\
osfMenu       : Shift<Key>F10  \n\
osfMenuBar    : <Key>F10       \n\
osfPageDown   : <Key>Next      \n\
osfPageUp     : <Key>Prior     \n\
osfRight      : <Key>Right     \n\
osfUp         : <Key>Up        \n\
osfDown       : <Key>Down      ";


XmConst char _XmVirtKeys_pcFallbackBindingString[] = "\
osfActivate   : <Key>KP_Enter  \n\
osfAddMode    : Shift<Key>F8   \n\
osfBackSpace  : <Key>BackSpace \n\
osfBeginLine  : <Key>Home      \n\
osfCancel     : <Key>Escape    \n\
osfClear      : <Key>Clear     \n\
osfCopy       : Ctrl<Key>Insert\n\
osfCut        : Shift<Key>Delete\n\
osfDelete     : <Key>Delete    \n\
osfEndLine    : <Key>End       \n\
osfHelp       : <Key>F1        \n\
osfInsert     : <Key>Insert    \n\
osfLeft       : <Key>Left      \n\
osfMenu       : Shift<Key>F10  \n\
osfMenuBar    : <Key>F10       \n\
osfPageDown   : <Key>Next      \n\
osfPageLeft   : Ctrl<Key>Prior \n\
osfPageRight  : Ctrl<Key>Next  \n\
osfPageUp     : <Key>Prior     \n\
osfPaste      : Shift<Key>Insert\n\
osfPrimaryPaste:Meta Ctrl<Key>Insert\n\
osfRight      : <Key>Right     \n\
osfSelect     : <Key>Select    \n\
osfUndo       : <Key>Undo      \n\
osfUp         : <Key>Up        \n\
osfDown       : <Key>Down      ";


XmConst char _XmVirtKeys_sgiFallbackBindingString[] = "\
osfActivate   : <Key>KP_Enter  \n\
osfAddMode    : Shift<Key>F8   \n\
osfBackSpace  : <Key>BackSpace \n\
osfBeginLine  : <Key>Home      \n\
osfCancel     : <Key>Escape    \n\
osfDelete     : <Key>Delete    \n\
osfEndLine    : <Key>End       \n\
osfHelp       : <Key>F1        \n\
osfInsert     : <Key>Insert    \n\
osfLeft       : <Key>Left      \n\
osfMenu       : Shift<Key>F10  \n\
osfMenuBar    : <Key>F10       \n\
osfPageDown   : <Key>Next      \n\
osfPageUp     : <Key>Prior     \n\
osfRight      : <Key>Right     \n\
osfUp         : <Key>Up        \n\
osfDown       : <Key>Down      ";


XmConst char _XmVirtKeys_sunFallbackBindingString[] = "\
osfActivate   : <Key>KP_Enter  \n\
osfAddMode    : Shift<Key>F8   \n\
osfBackSpace  : <Key>BackSpace \n\
osfBeginLine  : <Key>F27       \n\
osfCancel     : <Key>Escape    \n\
osfClear      : <Key>Clear     \n\
osfDelete     : <Key>Delete    \n\
osfEndLine    : <Key>F13       \n\
osfCopy       : <Key>F16       \n\
osfCut        : <Key>F20       \n\
osfHelp       : <Key>F1        \n\
osfInsert     : <Key>Insert    \n\
osfLeft       : <Key>Left      \n\
osfMenu       : Shift<Key>F10  \n\
osfMenuBar    : <Key>F10       \n\
osfPaste      : <Key>F18       \n\
osfPageDown   : <Key>F35       \n\
osfPageUp     : <Key>F29       \n\
osfRight      : <Key>Right     \n\
osfSelect     : <Key>Select    \n\
osfUndo       : <Key>Undo      \n\
osfUp         : <Key>Up        \n\
osfDown       : <Key>Down      ";

/*
 * List of predefined virtual key bindings for several server vendors. I
 * don't claim that the list is complete.
 */
static XmDefaultBindingStringRec defaultBindings[] =
{
    {"The XFree86 Project, Inc", _XmVirtKeys_pcFallbackBindingString},
    {"Hewlett-Packard Company", _XmVirtKeys_hpFallbackBindingString},
    {"International Business Machines",
     _XmVirtKeys_ibmFallbackBindingString},
    {"Silicon Graphics Inc.", _XmVirtKeys_sgiFallbackBindingString},
    {"Silicon Graphics", _XmVirtKeys_sgiFallbackBindingString},
    {"X11/NeWS - Sun Microsystems Inc.",
     _XmVirtKeys_sunFallbackBindingString},
};

/*
 * Stick the actual bindings string to the root window of the display. Here,
 * the CSF docu lacks the details: bindings are set on a per-display basis.
 * But displays can have more man one screen and thus more than one root
 * window. So which one to take? Seems like the CSF stores the property
 * always to the root window of screen #0, and not the default screen. And
 * in fact this makes sense.
 */
static void
StickBindingsToRootWindow(Display *Dsp, String Bindings,
			  String PropertyName)
{
    Atom BindingsProperty;

    BindingsProperty = XmInternAtom(Dsp, PropertyName, False);
    XChangeProperty(Dsp, RootWindowOfScreen(ScreenOfDisplay(Dsp, 0)),
		    BindingsProperty, XA_STRING, 8, PropModeReplace,
		    (unsigned char *)Bindings, strlen(Bindings) + 1);
}				/* StickBindingsToRootWindow */

/*
 * This is a list of all currently defined virtual csf keysyms. This allows
 * us to look up virtual keysyms in order to convert them back to their real
 * (vendor) keysyms. The accompanying table with the vendor keysyms is
 * property of a XmDisplay widget.
 */
static XmVirtualKeysymRec VirtualKeysyms[] =
{
    {"osfActivate", osfXK_Activate},
    {"osfAddMode", osfXK_AddMode},
    {"osfBackSpace", osfXK_BackSpace},
    {"osfBeginLine", osfXK_BeginLine},
    {"osfCancel", osfXK_Cancel},
    {"osfClear", osfXK_Clear},
    {"osfCopy", osfXK_Copy},
    {"osfCut", osfXK_Cut},
    {"osfDelete", osfXK_Delete},
    {"osfDown", osfXK_Down},
    {"osfEndLine", osfXK_EndLine},
    {"osfHelp", osfXK_Help},
    {"osfInsert", osfXK_Insert},
    {"osfLeft", osfXK_Left},
    {"osfMenu", osfXK_Menu},
    {"osfMenuBar", osfXK_MenuBar},
    {"osfPageDown", osfXK_PageDown},
    {"osfPageLeft", osfXK_PageLeft},
    {"osfPageRight", osfXK_PageRight},
    {"osfPageUp", osfXK_PageUp},
    {"osfPaste", osfXK_Paste},
    {"osfPrimaryPaste", osfXK_PrimaryPaste},
    {"osfQuickPaste", osfXK_QuickPaste},
    {"osfRight", osfXK_Right},
    {"osfSelect", osfXK_Select},
    {"osfUndo", osfXK_Undo},
    {"osfUp", osfXK_Up}
};				/* VirtualKeysyms */


/*
 * Armed with a virtual keysym (one of the osfXK_xxxx keysyms) return the
 * keysym and modifiers, which trigger this virtual keysym.
 */
extern void
_XmVirtualToActualKeysym(Display *Dsp,
			 KeySym VirtualKeysym,
			 KeySym *RealKeysymReturn,
			 Modifiers *ModifierReturn)
{
    XmDisplayRec *wd;
    Cardinal i;

    wd = (XmDisplayRec *)XmGetXmDisplay(Dsp);
    /*
     * Because the table is soooooo small (only 27 entries), we can afford
     * it to do a linear search.
     */
    for (i = 0; i < XtNumber(VirtualKeysyms); i++)
    {
	if (VirtualKeysym == VirtualKeysyms[i].keysym)
	{
	    *RealKeysymReturn = (wd->display.bindings)[i].keysym;
	    *ModifierReturn = (wd->display.bindings)[i].modifiers;

	    DEBUGOUT(_LtDebug(__FILE__, NULL,
		"_XmVirtualToActualKeysym %d -> 0x%X\n",
		VirtualKeysym, *RealKeysymReturn));

	    return;
	}
    }

    /*
     * Nothing found. So return no symbol at all...
     */
    *RealKeysymReturn = NoSymbol;
    *ModifierReturn = (Modifiers)0;
}				/* _XmVirtualToActualKeysym */

/*
 * GetModifierMapping -- returns the modifier masks for the ALT and META
 * modifier keys. If there is no such mapping, then a mask of zero will
 * be returned for that particular modifier.
 *
 * This trick allows us to understand the ALT and META modifiers when the
 * user specifies the virtual keysyms. In contrast to the translation
 * manager of the X intrinsics toolkit we can handle this particular
 * modifiers as true modifiers, not as keypress sequences.
 *
 * And in contrast to the closed software foundation we can handle ALT
 * not just as a fixed alias to Mod1 but as the true modifier as set up
 * with xmodmap...
 */
#define SET_MODIFIER(m_idx, mask_cnt)                                \
    if ( ModifierMasks[m_idx] == (Modifiers) 0 ) {                   \
        ModifierMasks[m_idx] = (Modifiers) (1 << mask_cnt);          \
    }

static void
GetModifierMapping(Display *Dsp,
		   XmModifierMaskSetReference ModifierMasks)
{
    XModifierKeymap *ModifierKeymap;
    KeySym ModifierKeysym;
    int ModifierSet, SetIndex, SetSize;

    /*
     * First reset the modifier masks in question to zero, that is, these
     * particular modifiers are not bound.
     */
    for (SetIndex = 0; SetIndex < MAX_MODIFIERS; SetIndex++)
    {
	ModifierMasks[SetIndex] = (Modifiers)0;
    }

    /*
     * Ask the server for the current modifier mapping and parse it for
     * the META and ALT keysyms. If one of them is bound to a modifier,
     * then remember the mask of that modifier.
     */
    ModifierKeymap = XGetModifierMapping(Dsp);
    SetSize = ModifierKeymap->max_keypermod;
    for (ModifierSet = 0; ModifierSet < 8; ModifierSet++)
    {
	for (SetIndex = 0; SetIndex < SetSize; SetIndex++)
	{
	    ModifierKeysym = XKeycodeToKeysym(Dsp,
					      ModifierKeymap->modifiermap[
					     SetIndex + ModifierSet * SetSize],
					      0);
	    switch (ModifierKeysym)
	    {
		/*
		 * If we've found one of the well known modifier keysyms,
		 * we'll remember the modifier - but only if we havn't yet
		 * seen another one.
		 */
	    case XK_Meta_L:
	    case XK_Meta_R:
		SET_MODIFIER(METAModifier, ModifierSet);
		break;

	    case XK_Alt_L:
	    case XK_Alt_R:
		SET_MODIFIER(ALTModifier, ModifierSet);
		break;

	    case XK_Super_L:
	    case XK_Super_R:
		SET_MODIFIER(SUPERModifier, ModifierSet);
		break;

	    case XK_Hyper_L:
	    case XK_Hyper_R:
		SET_MODIFIER(HYPERModifier, ModifierSet);
		break;
	    }
	}
    }

    /*
     * And don't forget to clean up, we don't want to waste memory here --
     * because we're not the Closed Software Foundation! And then a last
     * paranoic checking for missing ALT modifiers...
     */
    XFreeModifiermap(ModifierKeymap);
    SET_MODIFIER(ALTModifier, Mod1MapIndex);
}				/* GetModifierMapping */

/*
 * This implements the cache for speeding up lookups within
 * _XmGetAltModifierForDisplay(). X contexts are nice things!
 */
#define MCC_None             ((XContext) 0)
#define MCC_RID_DisplayCache ((XID) 0)

static XContext ModifierCacheContext = MCC_None;

/*
 * Ask for the modifier masks of the ALT & Co. modifiers. This internal
 * function is specific to LessTif as the csf does not provide adequate
 * functionality. The results are cached so all but the first query won't
 * cause a round trip to the server. We currently have only one problem:
 * the cache must be invalidated whenever a MappingNotify event occours
 * on the wire. So a XmDisplay widget must trigger the invalidation
 * whenever it sees the notification coming along the wire.
 */
extern XmModifierMaskSetReference
_XmGetModifierMappingsForDisplay(Display *Dsp)
{
    XmModifierMaskSetReference ModifierMasks;

    /*
     * If we haven't yet initialized the cache, create a context.
     * This context will associate displays with modifiers.
     */
    if (ModifierCacheContext == MCC_None)
    {
	ModifierCacheContext = XUniqueContext();
    }
    if (XFindContext(Dsp, MCC_RID_DisplayCache,
		     ModifierCacheContext, (XPointer *)&ModifierMasks)
	!= XCSUCCESS)
    {
	/*
	 * No modifier set yet queried for this particular display. Now,
	 * ask for the modifiers and store it in the cache.
	 */
	ModifierMasks = (XmModifierMaskSetReference)
	    XtCalloc(1, sizeof(XmModifierMaskSet));

	GetModifierMapping(Dsp, ModifierMasks);

	XSaveContext(Dsp, MCC_RID_DisplayCache,
		     ModifierCacheContext, (XPointer)ModifierMasks);
    }

    return ModifierMasks;
}				/* _XmGetModifierMappingsForDisplay */


/*
 * Invalidate any cached modifier mappings for a specific display. The
 * next lookup with _XmGetModifierMappingsForDisplay() will then ask
 * the X server for a fresh setting and cache the new results. The
 * cache should be invalidated whenever the modifier mappings got reset
 * and a broadcast was send to us. This function is intented for
 * internal use only.
 */
extern void
_XmInvalidateModifierMappingsForDisplay(Display *Dsp)
{
    XmModifierMaskSetReference ModifierMasks;

    /*
     * If there is no context yet, then there isn't a cache there yet,
     * and therefor there is nothing to invalidate yet. Return (yet).
     */
    if (ModifierCacheContext == MCC_None)
    {
	return;
    }

    /*
     * Otherwise ask for a set of modifier mappings and dispose it.
     * This way the next query on that particular display will cause
     * a fresh modifier lookup.
     */
    if (XFindContext(Dsp, MCC_RID_DisplayCache,
		     ModifierCacheContext, (XPointer *)&ModifierMasks)
	== XCSUCCESS)
    {
	XDeleteContext(Dsp, MCC_RID_DisplayCache, ModifierCacheContext);
	XtFree((char *)ModifierMasks);
    }
}				/* _XmInvalidateModifierMappingsForDisplay */


/*
 * Spit out a warning during conversion of virtual bindings.
 */
static void
DecomposeWarning(Display *Dsp, String str)
{
    char Buffer[80], *p;
    Cardinal len;

    p = str;
    while (*p && (*p != '\n'))
    {
	p++;
    }

    len = p - str;
    if (len >= sizeof(Buffer))
    {
	len = sizeof(Buffer) - 1;
    }

    strncpy(Buffer, str, len);
    Buffer[len] = '\0';
    _XmWarning(XmGetXmDisplay(Dsp),
	       "Cannot convert string \"%s\" to type VirtualBinding.",
	       Buffer);
}				/* DecomposeWarning */

/*
 * Some easy, useful macros for use with parsing...
 */
#define SKIP_WS(p)                                                      \
    while ( ((c = *p) == ' ') || (c == '\t') ) {                        \
        p++;                                                            \
    }

#define PARSE_ID(p)                                                     \
    while ( (((c = *p) >= 'A') && (c <= 'Z')) ||                        \
            ((c >= 'a') && (c <= 'z'))        ||                        \
            ((c >= '0') && (c <= '9'))        ||                        \
            (c == '_') ) {                                              \
        p++;                                                            \
    }

#define EAT_LINE_AND_RETURN(p)                                          \
 {                                                                      \
    while ( *p && (*p++ != '\n') ) { /* empty */ }                      \
    return p;                                                           \
 }

#define PARSE_IDENTIFIER(p) {                                           \
    start = p; PARSE_ID(p); end = p;                                    \
    len = end - start;                                                  \
    if ( len >= sizeof(Identifier) ) { len = sizeof(Identifier) - 1; }  \
    strncpy(Identifier, start, len);                                    \
    Identifier[len] = '\0';                                             \
}


/*
 * This proc gets a string specifying a binding and decomposes it into
 * the vendor keysym to be translated and the destination osfkeysym. The
 * binding string is terminated either by a trailing zero or a newline.
 * If this function cannot convert the binding, it returns NoSymbol as
 * *VendorKey. This way the caller can realize, that there are no useful
 * values in the other parameters returned from the call.
 */
static String
DecomposeBindingString(Display *Dsp,
		       String Binding,
		       XmModifierMaskSet ModifierMasks,
		       KeySym *VendorKey,
		       Modifiers *ModifierMask,
		       KeySym *osfKey)
{
    String start, end, p, osfStart;
    char Identifier[80];
    char c;
    int len;

    /*
     * First skip all leading white space. If we then encounter an excla-
     * mation mark, this line is a comment. So skip it and return to the
     * caller.
     */
    *VendorKey = NoSymbol;

    p = Binding;

    SKIP_WS(p);

    if (!*p || (*p == '\n'))
    {
	return *p ? ++p : p;
    }
    if (*p == '!')
    {
	EAT_LINE_AND_RETURN(p);
    }

    /*
     * Now read in the osf keysym's name to be mapped to a vendor keysym.
     * If it is not valid, then silently ignore the whole line. So much
     * for compatibility...
     */
    PARSE_IDENTIFIER(p);

    *osfKey = XStringToKeysym(Identifier);

    if (*osfKey == NoSymbol)
    {
	EAT_LINE_AND_RETURN(p);
    }

    SKIP_WS(p);
    if (*p != ':')
    {
	EAT_LINE_AND_RETURN(p);
    }

    /*

     */
    *ModifierMask = (Modifiers)0;

    p++;

    SKIP_WS(p);

    osfStart = p;

    do
    {
	SKIP_WS(p);
	if (*p == '<')
	{
	    /*
	     * This is unofficial support as this is really non-sense!
	     * No one should ever specify an unbound key in a binding file...
	     * well, despite this, I'm not sure, whether there are users...
	     */
	    if (strncmp(p, "<unbound>", 9) == 0)
	    {
		EAT_LINE_AND_RETURN(p);
	    }

	    /*
	     * This looks like the <Key> keyword. Check it. If we're
	     * wrong, print out a warning message to the terminal.
	     */
	    if (strncmp(p, "<Key>", 5) != 0)
	    {
		DecomposeWarning(Dsp, osfStart);
		EAT_LINE_AND_RETURN(p);
	    }

	    /*
	     * Skip the string <Key> and fetch the name of the keysym to
	     * be bound to the osf keysym.
	     */
	    p += 5;

	    SKIP_WS(p);

	    PARSE_IDENTIFIER(p);

	    *VendorKey = XStringToKeysym(Identifier);
	    if (*VendorKey == NoSymbol)
	    {
		DecomposeWarning(Dsp, osfStart);
		EAT_LINE_AND_RETURN(p);
	    }

	    SKIP_WS(p);
	    if (*p && (*p++ != '\n'))
	    {
		DecomposeWarning(Dsp, osfStart);
		*VendorKey = NoSymbol;
		EAT_LINE_AND_RETURN(p);
	    }

	    return p;
	}
	else
	{
	    /*
	     * At this time only names of modifiers are allowed here. De-
	     * code the name and add it to the set of required modifier
	     * flags when decoding virtual keysyms.
	     */
	    PARSE_IDENTIFIER(p);
	    if (strcmp(Identifier, "Shift") == 0)
	    {
		*ModifierMask |= ShiftMask;
	    }
	    else if (strcmp(Identifier, "Ctrl") == 0)
	    {
		*ModifierMask |= ControlMask;
	    }
	    else if (strcmp(Identifier, "Alt") == 0)
	    {
		*ModifierMask |= ModifierMasks[ALTModifier];
	    }
	    else if (strcmp(Identifier, "Meta") == 0)
	    {
		*ModifierMask |= ModifierMasks[ALTModifier];
	    }
	    else if (strcmp(Identifier, "Super") == 0)
	    {
		*ModifierMask |= ModifierMasks[ALTModifier];
	    }
	    else if (strcmp(Identifier, "Hyper") == 0)
	    {
		*ModifierMask |= ModifierMasks[ALTModifier];
	    }
	    else
	    {
		DecomposeWarning(Dsp, osfStart);
		EAT_LINE_AND_RETURN(p);
	    }
	}
    }
    while (*p && (*p != '\n'));

    EAT_LINE_AND_RETURN(p);
}


/*
 * Armed with a string full of bindings, this proc creates the binding table
 * for use with a display. The memory for storing the bindings must have
 * been (already) allocated!
 */
static void
ParseBindings(Display *Dsp, String Bindings)
{
    XmModifierMaskSet ModifierMasks;
    String p;
    KeySym VendorKey, csfKey;
    Modifiers VendorModifiers;
    XmKeyBinding BindingTable;
    int i;

    BindingTable = ((XmDisplayRec *)XmGetXmDisplay(Dsp))->display.bindings;

    GetModifierMapping(Dsp, ModifierMasks);

    p = Bindings;

    while (p && *p)
    {
	/*
	 * For every line of text parse the binding into the keysyms and
	 * modifiers. Then try to find the csfKeysym in the table of
	 * allowed virtual bindings. If it is found, then store the keysym
	 * and modifiers to be converted into the bindings table of the
	 * display widget.
	 */
	p = DecomposeBindingString(Dsp, p, ModifierMasks,
				   &VendorKey, &VendorModifiers,
				   &csfKey);
	if (VendorKey != NoSymbol)
	{
	    for (i = 0; i < XtNumber(VirtualKeysyms); i++)
	    {
		if (csfKey == VirtualKeysyms[i].keysym)
		{
		    BindingTable[i].keysym = VendorKey;
		    BindingTable[i].modifiers = VendorModifiers;
		    break;
		}
	    }
	}
    }
}


/*
 * If all fails, install the default bindings... But try to install vendor
 * specific default bindings, if possible. I don't know that the return
 * value is good for -- seems to be zero all the time.
 */
extern int
_XmVirtKeysLoadFallbackBindings(Display *Dsp, String *Bindings)
{
    String VendorString, BindingString;
    Cardinal i;

    VendorString = XServerVendor(Dsp);
    BindingString = _XmVirtKeys_fallbackBindingString;
    for (i = 0; i < XtNumber(defaultBindings); i++)
    {
	if (strcmp(VendorString, defaultBindings[i].vendorName) == 0)
	{
	    BindingString = defaultBindings[i].defaults;
	    break;
	}
    }

    *Bindings = XtNewString(BindingString);
    ParseBindings(Dsp, BindingString);
    StickBindingsToRootWindow(Dsp, BindingString,
			      DEFAULT_BINDINGS_PROPERTY_NAME);

    return 0;
}


/*
 * Read in a file containing (hopefuly) bindings for our ******* virtual
 * keysyms. The file is read in chunks of 1k to speed the reading while
 * not eating up to much memory. We're not using ftell here so we can
 * (in principle) even read the bindings from a pipe...
 */
#define QUANTUMLEAPS 1024
extern Boolean
_XmVirtKeysLoadFileBindings(String filename, String *binding)
{
    FILE *fp;
    int BindingLen, BytesRead;

    *binding = NULL;
    fp = fopen(filename, "r");
    if (fp)
    {
	BindingLen = 0;

	do
	{
	    *binding = XtRealloc(*binding, (BindingLen + QUANTUMLEAPS) *
				 sizeof(char));
	    BytesRead = fread(*binding + BindingLen, sizeof(char),
			      QUANTUMLEAPS, fp);
	    BindingLen += BytesRead;
	}
	while (BytesRead == QUANTUMLEAPS);

	fclose(fp);

	/*
	 * Shrink memory block to the exact size of the data read and add 
	 * a trailing zero to the string.
	 */
	BindingLen++;
	*binding = XtRealloc(*binding, BindingLen);
	(*binding)[BindingLen - 1] = '\0';

	return True;
    }

    return False;
}


/*
 * This is a helper proc for FindXmBindAliasBinding(). It tries to locate
 * the binding file name and then to load that file.
 */
static Boolean
LoadFileBindingsFromAlias(Display *Dsp, String dir, String r,
			  String *bindingSpec,
			  String xmbind_alias, int lineno)
{
    String re, motifbind;
    Boolean found;

    /*
     * Skip the trailing quote and all following white
     * space. Then cut off any trailing white space.
     */
    ++r;
    while ((*r == ' ') || (*r == '\t') || (*r == '\n'))
    {
	++r;
    }

    re = r + strlen(r);
    if (re > r)
    {
	--re;		/* Otherwise we would start on the final \0... */
	while ((re != r) && ((*re == ' ') || (*re == '\t') ||
			     (*re == '\n')))
	{
	    --re;
	}

	*++re = '\0';

	if (*r == '/')
	{
	    /* an absolute pathname */
	    return _XmVirtKeysLoadFileBindings(r, bindingSpec);
	}
	else
	{
	    /* it's a relative one. */
	    motifbind = XtMalloc(sizeof(char) *
				   (strlen(dir) + 1 + strlen(r) + 1));

	    sprintf(motifbind, "%s/%s", dir, r);
	    found = _XmVirtKeysLoadFileBindings(motifbind,
						bindingSpec);
	    XtFree(motifbind);
	    return found;
	}
    }
    else
    {
	_XmWarning(XmGetXmDisplay(Dsp),
		   "Malformed line in file \"%s\" (line number %d):\n"
		   "Missing binding file name.", xmbind_alias, lineno);
    }
    return False;
}

/*
 * Look for an xmbind.alias file which specifies the file to load for a
 * specific X server vendor.
 */
static Boolean
FindXmBindAliasBindings(Display *Dsp, String *bindingSpec,
			String dir)
{
    FILE *fp;
    String xmbind_alias = XtMalloc(sizeof(char) *
				   (strlen(dir) + strlen("/xmbind.alias") + 1));
    String server_vendor = XServerVendor(Dsp);
    int server_version = XVendorRelease(Dsp);
    String r;

    /*
     * Try to open the xmbind.alias in the directory specified by dir. This
     * can be either the user's home directory, $XMBINDDIR or a hardcoded
     * directory, if $XMBINDDIR isn't set.
     */
    sprintf(xmbind_alias, "%s/xmbind.alias", dir);

    fp = fopen(xmbind_alias, "r");

    if (fp)
    {
	char buf[256];
	String p;
	int lineno = 0;
	int len;

	/*
	 * Read in every line and look for a match of the vendor name and
	 * vendor release. For convenience, count the line number, so error
	 * messages can be more precise.
	 */
	while (fgets(buf, sizeof(buf), fp))
	{
	    lineno++;
	    /*
	     * Skip lines containing only comments -- these lines start
	     * with an exclamation mark. Also skip empty lines.
	     */
	    p = buf;
	    while ((*p == ' ') || (*p == '\t'))
	    {
		++p;
	    }
	    if ((*p == '!') || (*p == '\n') || (*p == '\0'))
	    {
		continue;
	    }
	    if (*p != '"')
	    {
		_XmWarning(XmGetXmDisplay(Dsp),
			   "Malformed line in file \"%s\" (line number %d):\n"
			   "Missing opening double quote. The vendor name"
			   " (and optionally the vendor\nrelease number) must"
			   " be enclosed in double quotes.",
			   xmbind_alias, lineno);
		continue;
	    }

	    /*
	     * Check to see if at least the vendor is correct...
	     */
	    len = strlen(server_vendor);
	    if (strncmp(p + 1, server_vendor, len) == 0)
	    {
		/* 
		 * Was it followed by a quote? If not, check for the release 
		 * number. If it is the right release number then try to
		 * find the file name.
		 */
		if (p[len + 1] == ' ')
		{
		    r = strchr(p + len + 2, '"');
		    if (r == NULL)
		    {
			_XmWarning(XmGetXmDisplay(Dsp),
			    "Malformed line in file \"%s\" (line number %d):\n"
			       "Missing closing double quote. The vendor name "
			     "(and optionally the vendor\nrelease number) must"
				   " be enclosed in double quotes.",
				   xmbind_alias, lineno);
			continue;
		    }
		    if (server_version != atoi(p + len + 2))
		    {
			continue;
		    }

		    if (LoadFileBindingsFromAlias(Dsp, dir, r,
						  bindingSpec,
						  xmbind_alias, lineno))
		    {
			XtFree(xmbind_alias);
			fclose(fp);
			return True;
		    }
		}
		else
		{
		    if ((r = strchr(p + len + 1, '"')) == NULL)
		    {
			_XmWarning(XmGetXmDisplay(Dsp),
			    "Malformed line in file \"%s\" (line number %d):\n"
			   "Missing closing double quote. The vendor name (and"
			     " optionally the vendor\nrelease number) must be "
				   "enclosed in double quotes.",
				   xmbind_alias, lineno);
		    }
		    else
		    {
			if (LoadFileBindingsFromAlias(Dsp, dir, r,
						      bindingSpec,
						      xmbind_alias, lineno))
			{
			    XtFree(xmbind_alias);
			    fclose(fp);
			    return True;
			}
		    }
		}
	    }
	}

	fclose(fp);
    }

    XtFree(xmbind_alias);

    return False;
}


/*
 * Initialize the virtual key bindings for a XmDisplay widget. We will
 * look in a couple of places for binding specifications. If all else fails,
 * we take some default fallbacks.
 */
static void
VirtKeysInitialize(Widget w)
{
    String HOME = _XmOSGetHomeDirName();	/* getenv("HOME"); */
    String bindingSpec = NULL;
    String motifbind, XMBINDDIR;
    XmDisplayRec *wd;
    Display *Dsp;
    char *type;
    XrmValue value;
    Atom _motif_default_bindings, _motif_bindings;
    Atom actual_type;
    int actual_format;
    unsigned long numitems;
    unsigned long bytes_left;

    wd = (XmDisplayRec *)w;
    wd->display.bindings = (XmKeyBinding)XtCalloc(sizeof(XmKeyBindingRec),
						  XtNumber(VirtualKeysyms));
    Dsp = XtDisplay(w);

    /*
     * First check for the defaultVirtualBindings resource... In order to
     * be compatible with M*tif we first check for a resource on the display
     * widget. If that's not NULL, then we'll use its contents, otherwise
     * we fall back on other strategies. But we'll never touch that resource
     * again (i.e. we won't SET it!).
     */
    if (wd->display.bindingsString != NULL)
    {
	ParseBindings(Dsp, wd->display.bindingsString);
	return;
    }

    if (XrmGetResource(XtDatabase(Dsp),
		       "defaultVirtualBindings",
		       "DefaultVirtualBindings",
		       &type, &value))
    {
	DEBUGOUT(_LtDebug(__FILE__, w, "Found resource\n"));

	/*
	 * As long as we're not modifying the resource database during
	 * parsing, it's perfectly secure to use the string stored inside
	 * the database. This way, we don't need to create a copy, which
	 * can be slow -- depending on this platform's malloc performance.
	 */
	bindingSpec = (String)value.addr;

	ParseBindings(Dsp, bindingSpec);

	return;
    }

    /* 
     * Check the _MOTIF_DEFAULT_BINDINGS and _MOTIF_BINDINGS properties on
     * the root window (of screen #0, of course. See above for an
     * explanation).
     */
    _motif_bindings = XmInternAtom(Dsp,
				   BINDINGS_PROPERTY_NAME, False);

    _motif_default_bindings = XmInternAtom(Dsp,
					   DEFAULT_BINDINGS_PROPERTY_NAME,
					   False);
    if ((XGetWindowProperty(Dsp,
			    RootWindowOfScreen(ScreenOfDisplay(Dsp, 0)),
			    _motif_bindings,
			    0, 10000 /* some really big number */ ,
			    False, XA_STRING,
			    &actual_type, &actual_format,
			    &numitems, &bytes_left,
			    (unsigned char **)&bindingSpec) == Success)
	||
	(XGetWindowProperty(Dsp,
			    RootWindowOfScreen(ScreenOfDisplay(Dsp, 0)),
			    _motif_default_bindings,
			    0, 10000 /* some really big number */ ,
			    False, XA_STRING,
			    &actual_type, &actual_format,
			    &numitems, &bytes_left,
			    (unsigned char **)&bindingSpec) == Success))
    {
	if (bindingSpec)
	{
	    DEBUGOUT(_LtDebug(__FILE__, w, "Found property\n"));

	    ParseBindings(Dsp, bindingSpec);

	    XFree(bindingSpec);

	    return;
	}
    }
    /*
     * Check for .motifbind in $(HOME). If we find one, parse it and stick
     * a copy of it to property of the root window.
     */
    if (HOME == NULL)
    {
	HOME = ".";
    }

    motifbind = XtMalloc(sizeof(char) *
			   (strlen(HOME) + strlen("/.motifbind") + 1));

    sprintf(motifbind, "%s/.motifbind", HOME);

    if (_XmVirtKeysLoadFileBindings(motifbind, &bindingSpec))
    {
	ParseBindings(Dsp, bindingSpec);

	StickBindingsToRootWindow(Dsp, bindingSpec, BINDINGS_PROPERTY_NAME);

	XtFree(bindingSpec);

	return;
    }

    /*
     * Try to find an xmbind.alias file, looking first in $(HOME) and then 
     * in XMBINDDIR/some-other-dir
     */
    if (FindXmBindAliasBindings(Dsp, &bindingSpec, HOME))
    {
	ParseBindings(Dsp, bindingSpec);

	StickBindingsToRootWindow(Dsp, bindingSpec, BINDINGS_PROPERTY_NAME);

	XtFree(bindingSpec);

	return;
    }

    XMBINDDIR = getenv("XMBINDDIR");
    if (XMBINDDIR == NULL)
    {
	XMBINDDIR = "/usr/lib/Xm/bindings";
    }
    if (FindXmBindAliasBindings(Dsp, &bindingSpec, XMBINDDIR))
    {
	ParseBindings(Dsp, bindingSpec);

	StickBindingsToRootWindow(Dsp, bindingSpec, BINDINGS_PROPERTY_NAME);

	XtFree(bindingSpec);

	return;
    }

    /*
     * If all fails, load the fallback bindings.
     */
    _XmVirtKeysLoadFallbackBindings(Dsp, &bindingSpec);
    XtFree(bindingSpec);
}


/*
 * This is completely new: refresh the virtual binding stuff on the fly
 * during run-time.
 */
extern void
_XmRefreshVirtKeys(Widget w)
{
    if (!XmIsDisplay(w))
    {
	_XmWarning(w,
	      "_XmVirtKeysInitialize(): Thou shall not try to create virtual\n"
		 "bindings on a widget which is not a subclass of XmDisplay.");
	return;
    }
    _XmInvalidateModifierMappingsForDisplay(XtDisplay(w));
    VirtKeysInitialize(w);
}


/*
 * Initialize the virtual key mechanism of a display widget. Although
 * the interface says, that w is any widget, we will handle only display
 * widgets here. Otherwise it would make no sense.
 */
extern void
_XmVirtKeysInitialize(Widget w)
{
    /*
     * It's always better to double-check the parameters...
     */
    if (!XmIsDisplay(w))
    {
	_XmWarning(w,
	      "_XmVirtKeysInitialize(): Thou shall not try to create virtual\n"
		 "bindings on a widget which is not a subclass of XmDisplay.");
	return;
    }

    VirtKeysInitialize(w);

    /*
     * Initialize some fields for compatibility reasons...
     */
    memset(((XmDisplayRec *)w)->display.keycode_tag, 0, XmKEYCODE_TAG_SIZE);

    ((XmDisplayRec *)w)->display.lastKeyEvent =
	(XKeyEvent *)XtMalloc(sizeof(XKeyEvent));
}


/*
 * Cleanup any leftovers from the binding horror when a XmDisplay is
 * getting destroyed.
 */
extern void
_XmVirtKeysDestroy(Widget w)
{
    if (!XmIsDisplay(w))
    {
	_XmWarning(w,
	     "_XmVirtKeysInitialize(): Thou shall not try to destroy virtual\n"
		 "bindings on a widget which is not a subclass of XmDisplay.");
	return;
    }

    if (((XmDisplayRec *)w)->display.lastKeyEvent)
    {
	XtFree((char *)((XmDisplayRec *)w)->display.lastKeyEvent);
    }
    if (((XmDisplayRec *)w)->display.bindings)
    {
	XtFree((char *)((XmDisplayRec *)w)->display.bindings);
    }
}


/*
 * If we get our hands on some of the dreaded vendor keysyms, we have to
 * convert them to the even more dreaded csf keysyms.
 */
static void
CheckForVirtualKey(Display *Dsp, KeyCode Keycode,
		   Modifiers CurrentModifiers,
		   Modifiers *ModifiersReturn,
		   KeySym *KeysymReturn)
{
    XmDisplayRec *wd;
    XmKeyBinding Bindings;
    KeySym Key;
    Modifiers BestMods;
    Cardinal i;

    wd = (XmDisplayRec *)XmGetXmDisplay(Dsp);
    Bindings = wd->display.bindings;
    Key = *KeysymReturn;	/* as set by the caller!!! */

    if (Key == NoSymbol)
    {
	return;
    }

#if 0
/*
 * This is a hack to make nedit support PageUp and PageDown.
 * A more permanent solution seems to have been found. It's not
 * nice though. Look in XmTranslateKey and _XmVirtualHandler.
 */
    if (Key == 0xFF56 && CurrentModifiers == 4) {	/* PageDown */
	*KeysymReturn = 0x1004FF42;			/* osfXK_PageDown */
	return;
    }
    if (Key == 0xFF55 && CurrentModifiers == 4) {	/* PageUp */
	*KeysymReturn = 0x1004FF41;			/* osfXK_PageUp */
	return;
    }
#endif

    /*
     * Because the table is soooooo small (only 27 entries), we can afford
     * it to do a linear search (well, we can afford it once again, like in
     * _XmVirtualToActualKeysym()). For every entry in the bindings table
     * of the appropiate XmDisplay widget we check for a keysym match. If
     * we get one, we must make sure, that the modifier flags are allright.
     * Also we MUST do an exhaustive search as there might be another binding
     * with "better matching" modifiers in the table.
     */
    BestMods = (Modifiers)0;
    for (i = 0; i < XtNumber(VirtualKeysyms); i++, Bindings++)
    {
	if (Key == Bindings->keysym)
	{
	    if (((CurrentModifiers & Bindings->modifiers) ==
		 Bindings->modifiers) &&
		(Bindings->modifiers >= BestMods))
	    {
		*KeysymReturn = VirtualKeysyms[i].keysym;
		BestMods = Bindings->modifiers;
	    }

	    *ModifiersReturn |= Bindings->modifiers;
	}
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL,
	"CheckForVirtualKey 0x%X, mod 0x%X -> 0x%X, mod 0x%X\n",
	Key, CurrentModifiers, *KeysymReturn, BestMods));
}


/*
 * Fortunatly, we can rely on XtTranslateKey() doing most of the work for
 * ordinary key presses: shift (lock) state, upper and lower case, worrying
 * with display locking procedures, and, and, and... We "only" have to
 * check for an outstanding conversion from a vendor keysym (ordinary
 * keysym) to a virtual keysym.
 */
extern void
XmTranslateKey(Display *Dsp, KeyCode Keycode,
	       Modifiers modifiers, Modifiers *modifiers_return,
	       KeySym *Keysym_return)
{
#if 1
    /*
     * Tie the keysym and the current modifier (from the lastKeyEvent) into
     * eachother to figure out whether this maps into a virtual keysym.
     */
    XmDisplay d = (XmDisplay)XmGetXmDisplay(Dsp);

    DEBUGOUT(_LtDebug(__FILE__, NULL,
	"XmTranslateKey (KeyCode %d, modifiers 0x%X), event %d\n",
	Keycode, modifiers, d->display.lastKeyEvent->state));
    DEBUGOUT(_LtDebug("NMEM", NULL,
	"XmTranslateKey (KeyCode %d, modifiers 0x%X), event %d\n",
	Keycode, modifiers, d->display.lastKeyEvent->state));

    XtTranslateKey(Dsp, Keycode, modifiers, modifiers_return, Keysym_return);

    CheckForVirtualKey(Dsp, Keycode, (Modifiers)d->display.lastKeyEvent->state,
		       modifiers_return, Keysym_return);
#else
    /*
     * Same as above without the funky stuff.
     * This doesn't seem to work in all cases.
     * Nedit shows that PageUp/PageDown don't work. Instead they behave as
     * PageRight and PageLeft. Which lead me to look at the modifiers, which
     * eventually got me to find all this. Sigh.
     */
    XtTranslateKey(Dsp, Keycode, modifiers, modifiers_return, Keysym_return);

    CheckForVirtualKey(Dsp, Keycode, modifiers,
		       modifiers_return, Keysym_return);
#endif
}


extern void
_XmVirtKeysStoreBindings(Widget shell, String binding)
{
}

/*
 * This handler just collects any key presses and stores the least one
 * with the appropiate display widget object. This one is in mainly for
 * compatibility reasons -- maybe someone needs the information collected
 * here lateron...
 *
 * Yes we do. This appears to be the only way to get hold of which
 * modifiers are valid right now (i.e. when the user presses this key).
 */
extern void
_XmVirtKeysHandler(Widget w, XtPointer Data, XEvent *Event,
		   Boolean *ContDispatch)
{
    XmDisplay	d;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmVirtKeysHandler\n"));
    DEBUGOUT(_LtDebug("NMEM", w, "_XmVirtKeysHandler\n"));

    if (!w->core.being_destroyed && (Event->xany.type == KeyPress))
    {
	d = (XmDisplay)XmGetXmDisplay(XtDisplay(w));

	*(d->display.lastKeyEvent) = *((XKeyEvent *)Event);

	/*
	 * Looks like Xt has a cache to prevent it from calling XmTranslateKey
	 * all the time.
	 *
	 * Disable it for now.
	 * That might be the only way to get XmTranslateKey processing (i.e.
	 * our virtual bindings) to work right: we need to tie the modifier(s)
	 * currently valid with the keysym that Xt comes up with to figure out
	 * if this is one of our virtual keysyms.
	 * Looks like this Xt caching business prevents this from working.
	 *
	 * There must be a better way to do this - FIX ME. Sigh.
	 */
	XtSetKeyTranslator(XtDisplay(w), (XtKeyProc)XmTranslateKey);
    }
}
