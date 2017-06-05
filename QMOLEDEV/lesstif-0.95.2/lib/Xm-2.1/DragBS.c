/**
 *
 * $Id: DragBS.c,v 1.2 2006/02/10 21:30:38 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995-2001, 2006 LessTif Development Team
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

static const char rcsid[] = "$Id: DragBS.c,v 1.2 2006/02/10 21:30:38 dannybackx Exp $";

#include <LTconfig.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/DisplayP.h>
#include <Xm/AtomMgr.h>
#include <XmI/AtomMgrI.h>
#include <XmI/DragDropI.h>
#include <XmI/CutPasteI.h>

#include <XmI/DebugUtil.h>

/*
 * here's a little tidbit to look at.  If you've run mwm, or a Motif
 * application, do an 'xprop' on the root window.  You'll see a property
 * _MOTIF_DRAG_WINDOW; now, do an xprop on _it_.  Interesting things running
 * around there, no?  This stuff is crucial to DnD...
 *
 * Daniel Dardailler's documentation is invaluable to actually understanding
 * this mess.
 *
 * convention: read/write refers to properties.  get/set refers to
 * XContexts.
 *
 * Don't panic when you find all the XGrabServer/XUngrabServer pairs.
 * Basically, they delineate critical sections where only the grabber should
 * have access to the X server.
 */

/* amai: Hmm, well for now it's just for convenience to help on debugging.
 * it used to be 100000L 
 */
#define PROP_LENGTH 200000L


/*
 * these are used for protected sections
 */
static Boolean error_flag, bad_window;
static int (*oldErrorHandler) (Display *, XErrorEvent *);
static int firstProtectRequest;
static Window errorWindow = None;

/*
 * these targets should always be present on the drag window
 */
static Atom noTarget = 0, stringTarget = XA_STRING;

/*
 * some contexts for the drag window, the atoms table, and the targets table.
 */
static XContext displayToDW = None;
static XContext displayToAtoms = None;
static XContext displayToTargets = None;


/*
 * basic atoms that must be agreed on
 */
static char *motifAtoms[] =
{
    _XA_MOTIF_DRAG_WINDOW,
    _XA_MOTIF_DRAG_ATOM_PAIRS,
    _XA_MOTIF_DRAG_ATOMS,
    _XA_MOTIF_ATOM_0,
    _XA_MOTIF_DRAG_TARGETS,
    _XA_MOTIF_WM_OFFSET,
    _XA_MOTIF_DRAG_PROXY_WINDOW,
    _XA_MOTIF_WM_MESSAGES,
    _XA_MOTIF_WM_HINTS,
    _XA_MOTIF_WM_MENU,
    _XA_MOTIF_WM_INFO,
    _XA_MOTIF_BINDINGS,
    _XA_ATOM_PAIR,
    _XA_AVERAGE_WIDTH,
    _XA_CLIPBOARD,
    _XA_CLIP_TEMPORARY,
    _XA_COMPOUND_TEXT,
    _XA_DELETE,
    _XA_INCR,
    _XA_INSERT_SELECTION,
    _XA_LENGTH,
    _XA_MOTIF_DESTINATION,
    _XA_MULTIPLE,
    _XA_PIXEL_SIZE,
    _XA_RESOLUTION_Y,
    _XA_TARGETS,
    _XA_TEXT,
    _XA_TIMESTAMP,
    _XA_WM_DELETE_WINDOW,
    _XA_WM_PROTOCOLS,
    _XA_WM_STATE,
    _XA_XmTRANSFER_SUCCESS,
    _XA_XmTRANSFER_FAILURE,
    _XA_MOTIF_CLIP_DATA_DELETE,
    _XA_MOTIF_CLIP_DATA_REQUEST,
    _XA_MOTIF_CLIP_HEADER,
    _XA_MOTIF_CLIP_LOCK_ACCESS_VALID,
    _XA_MOTIF_CLIP_MESSAGE,
    _XA_MOTIF_CLIP_NEXT_ID,
    _XA_MOTIF_CLIP_TIME,
    _XA_MOTIF_DROP,
    _XA_MOTIF_DRAG_INITIATOR_INFO,
    _XA_MOTIF_DRAG_RECEIVER_INFO,
    _XA_MOTIF_MESSAGE,
    _XA_XM_TEXT_I_S_PROP
};


static int
DWError(Display *display, XErrorEvent *event)
{
    error_flag = True;

    return 0;
}


/*
 * create the drag window
 */
static Window
create_drag_window(Display *display)
{
    Window win;
    XSetWindowAttributes attr;


    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:create_drag_window(%d)\n",
		      __FILE__, __LINE__));

    attr.override_redirect = True;
    attr.event_mask = PropertyChangeMask;

    win = XCreateWindow(display, DefaultRootWindow(display),
			-100, -100, 10, 10, 0, 0,
			InputOnly, CopyFromParent,
			(CWOverrideRedirect | CWEventMask),
			&attr);

    XMapWindow(display, win);

    return win;
}


/*
 * see if the drag window property can be found on the root window
 */
static Window
read_drag_window(Display *display)
{
    int (*old_handler) (Display *, XErrorEvent *);
    Atom mw, atype;
    int aformat;
    unsigned long nitems, bafter;
    unsigned char *prop = NULL;
    Window win = None;


    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_drag_window(%d)\n",
		      __FILE__, __LINE__));

    old_handler = XSetErrorHandler(DWError);

    error_flag = False;

    mw = XmInternAtom(display, _XA_MOTIF_DRAG_WINDOW, False);

    if (XGetWindowProperty(display, DefaultRootWindow(display),
			   mw, 0L, PROP_LENGTH,
			   False, AnyPropertyType,
			   &atype, &aformat, &nitems, &bafter, &prop)
	== Success && atype == XA_WINDOW && aformat == 32 && nitems == 1)
    {
	win = *((Window *)prop);
    }
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_drag_window(%d) - bafter=%i\n",
		      __FILE__, __LINE__, bafter));
    if (prop)
    {
	XFree((char *)prop);
    }

    XSetErrorHandler(old_handler);

    if (error_flag)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_drag_window(%d) - None\n",
		      __FILE__, __LINE__));

	return None;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_drag_window(%d) - %p\n",
		      __FILE__, __LINE__, win));

    return win;
}


/*
 * set the drag window property on the root window
 */
static void
write_drag_window(Display *display, Window *win)
{
    Atom mw;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:write_drag_window(%d)\n",
		      __FILE__, __LINE__));

    mw = XmInternAtom(display, _XA_MOTIF_DRAG_WINDOW, False);

    XChangeProperty(display, DefaultRootWindow(display), mw,
		    XA_WINDOW, 32, PropModeReplace, (unsigned char *)win, 1);
}


/*
 * lookup (program local) the value of the drag window
 */
static Window
get_drag_window(Display *display)
{
    Window win;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:get_drag_window(%d)\n",
		      __FILE__, __LINE__));

    if (displayToDW == None)
    {
	displayToDW = XUniqueContext();
    }

    if (XFindContext(display, DefaultRootWindow(display),
		     displayToDW, (XPointer *)&win) == XCSUCCESS)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:get_drag_window(%d) - %p\n",
		      __FILE__, __LINE__, win));

	return win;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:get_drag_window(%d) - None\n",
		      __FILE__, __LINE__));

    return None;
}


/*
 * cache (program local) the value of the drag window
 */
static void
set_drag_window(Display *display, Window win)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:set_drag_window(%d)\n",
		      __FILE__, __LINE__));

    if (displayToDW == None)
    {
	displayToDW = XUniqueContext();
    }

    if (XFindContext(display, DefaultRootWindow(display),
		     displayToDW, (XPointer *)&win) == XCSUCCESS)
    {
	XDeleteContext(display, DefaultRootWindow(display),
		       displayToDW);
    }

    XSaveContext(display, DefaultRootWindow(display),
		 displayToDW, (XPointer)win);
}


/*
 * catch window errors when fetching or setting the DRAG_WINDOW properties
 */
static int
protect_handler(Display *display, XErrorEvent *event)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:protect_handler(%d)\n",
		      __FILE__, __LINE__));

    if (event->error_code == BadWindow &&
	event->resourceid == errorWindow &&
	event->serial >= firstProtectRequest)
    {
	bad_window = True;

	return 0;
    }

    if (oldErrorHandler == NULL)
    {
	return 0;
    }

    return (*oldErrorHandler) (display, event);
}


/*
 * begin catching window errors
 */
static void
begin_protection(Display *display, Window win)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:begin_protection(%d)\n",
		      __FILE__, __LINE__));

    bad_window = False;

    oldErrorHandler = XSetErrorHandler(protect_handler);

    firstProtectRequest = NextRequest(display);

    errorWindow = win;
}


/*
 * end catching window errors
 */
static void
end_protection(Display *display)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:end_protection(%d)\n",
		      __FILE__, __LINE__));

    XSync(display, False);

    XSetErrorHandler(oldErrorHandler);

    oldErrorHandler = NULL;
}


/*
 * extracts n values (formatted as CARD32) from the drag buffer and
 * converts them to Atoms
 */
static void
read_atoms_from_drag_buffer(XmDndBufMgr bmgr, int n, Boolean swap, Atom* atoms)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_atoms_from_drag_buffer(%d) - count %d swap %s\n",
		      __FILE__, __LINE__,
		      n, swap ? "True" : "False"));

    if (sizeof(Atom) == sizeof(CARD32))
    {
        _XmReadDragBuffer(bmgr, False, (char*) atoms, sizeof(Atom) * n);
        if (swap)
        {
            int i;

            for (i = 0; i < n; i++)
            {
                SWAP4BYTES(atoms[i]);
            }
        }
    }
    else {
        CARD32 in;
        int i;

        if (swap)
        {
            for (i = 0; i < n; i++)
            {
                _XmReadDragBuffer(bmgr, False, (char*) &in, sizeof(CARD32));
                SWAP4BYTES(in);
                atoms[i] = in;
            }
        }
        else {
            for (i = 0; i < n; i++)
            {
                _XmReadDragBuffer(bmgr, False, (char*) &in, sizeof(CARD32));
                atoms[i] = in;
            }
        }
    }
}


/*
 * writes nent Atoms to the drag buffer
 */
static void
write_atoms_to_drag_buffer(XmDndBufMgr bmgr, int n, Atom* atoms)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:write_atoms_to_drag_buffer(%d) - count %d\n",
		      __FILE__, __LINE__,
		      n));

    if (sizeof(Atom) == sizeof(CARD32))
    {
        _XmWriteDragBuffer(bmgr, False, (char*) atoms, sizeof(Atom) * n);
    }
    else {
        CARD32 out;
        int i;

        for (i = 0; i < n; i++)
        {
            out = atoms[i];
            _XmWriteDragBuffer(bmgr, False, (char*) &out, sizeof(CARD32));
        }
    }
}


/*
 * read from a drag buffer (sort of)
 * probably for Motif BC (binary compatibility?!)
 */
extern int
_XmReadDragBuffer(XmDndBufMgr bmgr, Boolean read_string,
		  char *retbuf, int bufsize)
{
    int rsize, eaten;
    int size;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmReadDragBuffer(%d) - read_string %s size %i\n",
		      __FILE__, __LINE__,
		      read_string ? "True" : "False",
		      bufsize));

    if (read_string)
    {
	rsize = bmgr->name_size;
	eaten = bmgr->name_start - bmgr->names;

	if (eaten>rsize) {
	  /* amai: in my understanding this is a major problem and
	     should never happen */
	  _XmWarning(NULL, "%s(%i): eaten=%i > rsize=%i\n",
	             __FILE__, __LINE__, eaten, rsize);
          /* abort(); */
          size=0;
	}
	else {
	   size = rsize-eaten;
  	   if (size > bufsize)
              size = bufsize;
        }
	memcpy(retbuf, bmgr->name_start, size);
	bmgr->name_start += size;
	return size;
    }
    else
    {
	rsize = bmgr->atom_size;
	eaten = (int)(bmgr->atom_start - bmgr->atoms);

	if (eaten>rsize) {
	  /* amai: in my understanding this is a major problem and
	     should never happen */
	  _XmWarning(NULL, "%s(%i): eaten=%i > rsize=%i\n",
	             __FILE__, __LINE__, eaten, rsize);
          /* abort(); */
          size=0;
	}
	else {
	   size = rsize-eaten;
	   if (size > bufsize)
              size = bufsize;
        }
        /* fprintf(stderr, "rsize=%i, eaten=%i, bufsize=%i, size=%i\n",
                rsize, eaten, bufsize, size); */
	memcpy(retbuf, bmgr->atom_start, size);
	bmgr->atom_start += size;
	return size;
    }
}


/*
 * write to a drag buffer (sort of)
 * probably for Motif BC (binary compatibility?!)
 */
extern int
_XmWriteDragBuffer(XmDndBufMgr bmgr, Boolean write_string,
		   char *data, int size)
{
    int increase_needed;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmWriteDragBuffer(%d) write_string %s size %i\n",
		      __FILE__, __LINE__,
		      write_string ? "True" : "False",
		      size));

    if (write_string)
    {
	increase_needed = (bmgr->name_size + size) - bmgr->name_avail;
	if (increase_needed > 0)
	{
	    increase_needed /= 1000;
            increase_needed++;
            increase_needed *= 1000;
	    bmgr->name_avail += increase_needed;

	    if (bmgr->names == bmgr->name_ptr)
	    {
		bmgr->names = XtMalloc(bmgr->name_avail);
		memcpy(bmgr->names, bmgr->name_ptr, bmgr->name_size);
	    }
	    else
	    {
		bmgr->names = XtRealloc((char *)bmgr->names, bmgr->name_avail);
	    }
	}

	memcpy(bmgr->names + bmgr->name_size, data, size);
	bmgr->name_size += size;

	return size;
    }
    else
    {
	increase_needed = (bmgr->atom_size + size) - bmgr->atom_avail;
	if (increase_needed)
	{
	    increase_needed /= 1000;
            increase_needed++;
            increase_needed *= 1000;		
	    bmgr->atom_avail += increase_needed;
	    if (bmgr->atoms == bmgr->atom_ptr)
	    {
		bmgr->atoms = XtMalloc(bmgr->atom_avail);
		memcpy(bmgr->atoms, bmgr->atom_ptr, bmgr->atom_size);
	    }
	    else
	    {
		bmgr->atoms = XtRealloc(bmgr->atoms, bmgr->atom_avail);
	    }
	}

	memcpy(bmgr->atoms + bmgr->atom_size, data, size);
	bmgr->atom_size += size;

	return size;
    }
}


/*
 * get the ATOM_PAIRS.  They may or may not already exist
 */
static Boolean
read_atom_pairs(Display *display)
{
    Atom pa, atype;
    Window win;
    Boolean gotit = False;
    XmDndAtomPairs *pairs;
    XmDndBufMgrRec bmgr;
    int i;
    XmDndAtomPair pair;
    char buf[32];
    int aformat;
    unsigned long nitems, bafter;
    unsigned char *prop = NULL;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_atom_pairs(%d)\n",
		      __FILE__, __LINE__));

    pa = XmInternAtom(display, _XA_MOTIF_DRAG_ATOM_PAIRS, False);

    win = get_drag_window(display);

    begin_protection(display, win);

    if (XGetWindowProperty(display, win, pa, 0L, PROP_LENGTH,
			   False, pa,
			   &atype, &aformat, &nitems, &bafter,
			   (unsigned char **)&pairs) == Success &&
	nitems >= 8 && pairs != NULL)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_atom_pairs(%d) - got property, bafter=%i\n",
		      __FILE__, __LINE__, bafter));

	gotit = True;
	prop = (unsigned char *)pairs;
    }
    end_protection(display);

    if (bad_window)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "Bad window ATOM_PAIRS property on DRAG_WINDOW.");
    }

    if (!gotit)
    {
#if 0
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "No ATOM_PAIRS property on DRAG_WINDOW.");
#endif

	if (prop)
	{
	    XFree((char *)prop);
	}

	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_atom_pairs(%d) - did not get property\n",
		      __FILE__, __LINE__));

	return False;
    }

    if (pairs->protocol_version != DND_PROTOCOL_VERSION)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "DND Protocol version mismatch.");
    }

    if (pairs->byte_order != _XmByteOrder())
    {
	SWAP2BYTES(pairs->num_pairs);
	SWAP4BYTES(pairs->data_size);
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_atom_pairs(%d) - %i pairs\n",
		      __FILE__, __LINE__,
		      pairs->num_pairs));

    if (pairs->num_pairs > 0)
    {
	bmgr.atoms = (char *)pairs;
	bmgr.atom_start = (char *)(pairs + 1);
	bmgr.atom_size = pairs->data_size;
	bmgr.names = (char *)pairs + pairs->data_size;
	bmgr.name_start = (char *)pairs + pairs->data_size;
	bmgr.name_size = nitems - pairs->data_size;

	for (i = 0; i < pairs->num_pairs; i++)
	{
	    _XmReadDragBuffer(&bmgr, False, (char *)&pair,
			      sizeof(XmDndAtomPair));

	    if (pairs->byte_order != _XmByteOrder())
	    {
		SWAP4BYTES(pair.atom);
		SWAP2BYTES(pair.namelen);
	    }

	    _XmReadDragBuffer(&bmgr, True, buf, pair.namelen);

	    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_atom_pairs(%d) - intern %s\n",
		      __FILE__, __LINE__,
		      buf));

	    _XmInternAtomAndName(display, pair.atom, buf);
	}
    }

    if (prop)
    {
	XFree((char *)prop);
    }

    return gotit;
}


/*
 * if the ATOM_PAIRS didn't exist, put them on the drag window
 */
static void
write_atom_pairs(Display *display)
{
    char pairs[1000];
    char names[1000];
    XmDndAtomPairs pair_rec;
    XmDndBufMgrRec bmgr;
    XmDndAtomPair pair;
    Atom pa;
    int i;
    Window win;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:write_atom_pairs(%d)\n",
		      __FILE__, __LINE__));

    pair_rec.byte_order = _XmByteOrder();
    pair_rec.protocol_version = DND_PROTOCOL_VERSION;
    pair_rec.num_pairs = XtNumber(motifAtoms);

    bmgr.atoms = pairs;
    bmgr.atom_ptr = pairs;
    bmgr.atom_start = NULL;
    bmgr.atom_size = 0;
    bmgr.atom_avail = 1000;
    bmgr.names = names;
    bmgr.name_ptr = names;
    bmgr.name_start = NULL;
    bmgr.name_size = 0;
    bmgr.name_avail = 1000;

    _XmWriteDragBuffer(&bmgr, False, (char *)&pair_rec, sizeof(XmDndAtomPairs));

    for (i = 0; i < XtNumber(motifAtoms); i++)
    {
	pair.atom = XmInternAtom(display, motifAtoms[i], False);

	pair.namelen = strlen(motifAtoms[i]) + 1;

	_XmWriteDragBuffer(&bmgr, True, motifAtoms[i], pair.namelen);

	_XmWriteDragBuffer(&bmgr, False, (char *)&pair, sizeof(XmDndAtomPair));

    }

    ((XmDndAtomPairs *) (bmgr.atoms))->data_size = bmgr.atom_size;

    pa = XmInternAtom(display, _XA_MOTIF_DRAG_ATOM_PAIRS, False);

    win = get_drag_window(display);

    begin_protection(display, win);

    XChangeProperty(display, win, pa, pa, 8, PropModeReplace,
		    (unsigned char *)bmgr.atoms, bmgr.atom_size);

    if (bmgr.atoms != bmgr.atom_ptr)
    {
	XtFree(bmgr.atoms);
    }

    if (bmgr.name_size)
    {
	XChangeProperty(display, win, pa, pa, 8, PropModeAppend,
			(unsigned char *)bmgr.names, bmgr.name_size);

	if (bmgr.names != bmgr.name_ptr)
	{
	    XtFree(bmgr.names);
	}
    }

    end_protection(display);

    if (bad_window)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "Bad window writing ATOM_PAIRS property on DRAG_WINDOW.");
    }
}


/*
 * fetch the cached atoms table
 */
static XmDndAtomsTable
get_atoms_table(Display *display)
{
    XmDndAtomsTable tbl = NULL;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:get_atoms_table(%d)\n",
		      __FILE__, __LINE__));

    if (displayToAtoms == (XContext)0)
    {
	displayToAtoms = XUniqueContext();
    }

    if (XFindContext(display, DefaultRootWindow(display),
		     displayToAtoms, (XPointer *)&tbl) != XCSUCCESS)
    {
	return NULL;
    }

    return tbl;
}


/*
 * cache the atoms table
 */
static void
set_atoms_table(Display *display, XmDndAtomsTable table)
{
    XmDndAtomsTable atoms;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:set_atoms_table(%d)\n",
		      __FILE__, __LINE__));

    if (displayToAtoms == (XContext)0)
	displayToAtoms = XUniqueContext();

    if (XFindContext(display, DefaultRootWindow(display),
		     displayToAtoms, (XPointer *)&atoms) == XCSUCCESS)
    {
	if (atoms == table)
	{
	    return;
	}

	XDeleteContext(display, DefaultRootWindow(display),
		       displayToAtoms);
    }

    XSaveContext(display, DefaultRootWindow(display),
		 displayToAtoms, (XPointer)table);
}


static XmDndAtomsTable
create_default_atoms_table(Display *display)
{
    XmDndAtomsTable tbl;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:create_default_atoms_table(%d)\n",
		      __FILE__, __LINE__));

    tbl = (XmDndAtomsTable) XtMalloc(sizeof(XmDndAtomsTableRec));
    tbl->num_entries = 1;

    tbl->entries =
	(XmDndAtomsTableEntry) XtMalloc(sizeof(XmDndAtomsTableEntryRec));

    tbl->entries->atom = XmInternAtom(display, _XA_MOTIF_ATOM_0, False);
    tbl->entries->time = CurrentTime;

    set_atoms_table(display, tbl);

    return tbl;
}


static Boolean
read_atoms_table(Display *display, XmDndAtomsTable tbl)
{
    Atom da, atype;
    Window win;
    int aformat, i;
    unsigned long nitems, bafter;
    XmDndAtoms *atoms = NULL;
    Boolean got_it = False;
    XmDndAtomsTableEntryRec atom_ent;
    XmDndBufMgrRec bmgr;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_atoms_table(%d)\n",
		      __FILE__, __LINE__));

    da = XmInternAtom(display, _XA_MOTIF_DRAG_ATOMS, False);

    win = get_drag_window(display);

    begin_protection(display, win);

    if (XGetWindowProperty(display, win, da, 0L, PROP_LENGTH,
			   False, da,
			   &atype, &aformat, &nitems, &bafter,
			   (unsigned char **)&atoms) == Success &&
	nitems >= 8)
    {
        DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_atom_pairs(%d) - got property, bafter=%i\n",
		      __FILE__, __LINE__, bafter));    
	got_it = True;
    }

    end_protection(display);

    if (bad_window)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "Invalid DRAG WINDOW fetching DRAG_ATOMS.");
    }

    if (!got_it)
    {
	if (atoms)
	{
	    XFree((XPointer)atoms);
	}

	return False;
    }

    if (atoms->protocol_version != DND_PROTOCOL_VERSION)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "DND Protocol version mismatch.");
    }

    if (atoms->byte_order != _XmByteOrder())
    {
	SWAP2BYTES(atoms->num_atoms);
	SWAP4BYTES(atoms->data_size);
    }

    if (!tbl)
    {
	tbl = (XmDndAtomsTable) XtMalloc(sizeof(XmDndAtomsTableRec));

	tbl->num_entries = 0;
	tbl->entries = NULL;

	set_atoms_table(display, tbl);
    }

    if (tbl->num_entries < atoms->num_atoms)
    {
	tbl->entries = (XmDndAtomsTableEntry) XtRealloc((char *)tbl->entries,
							atoms->num_atoms *
					      sizeof(XmDndAtomsTableEntryRec));
    }

    if (atoms->num_atoms > 0)
    {

	bmgr.atoms = (char *)atoms;
	bmgr.atom_start = (char *)(atoms + 1);
	bmgr.atom_size = atoms->data_size;

	for (i = 0; i < atoms->num_atoms; i++)
	{
	    _XmReadDragBuffer(&bmgr, False, (char *)&atom_ent,
			      sizeof(XmDndAtomsTableEntryRec));

	    if (atoms->byte_order != _XmByteOrder())
	    {
		SWAP4BYTES(atom_ent.atom);
		SWAP4BYTES(atom_ent.time);
	    }

	    tbl->entries[i].atom = atom_ent.atom;
	    tbl->entries[i].time = atom_ent.time;
	}
    }

    return True;
}


/*
 * write the atoms table out to the drag window
 */
static void
write_atoms_table(Display *display, XmDndAtomsTable tbl)
{
    char atoms[1000];
    XmDndAtoms atoms_rec;
    XmDndBufMgrRec bmgr;
    Atom pa;
    int i;
    Window win;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:write_atoms_table(%d)\n",
		      __FILE__, __LINE__));

    if (tbl == NULL)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "No DRAG_ATOMS to write to DRAG_WINDOW.");

	return;
    }

    atoms_rec.byte_order = _XmByteOrder();
    atoms_rec.protocol_version = DND_PROTOCOL_VERSION;
    atoms_rec.num_atoms = tbl->num_entries;

    bmgr.atoms = atoms;
    bmgr.atom_ptr = atoms;
    bmgr.atom_start = NULL;
    bmgr.atom_size = 0;
    bmgr.atom_avail = 1000;

    _XmWriteDragBuffer(&bmgr, False, (char *)&atoms_rec, sizeof(XmDndAtoms));

    for (i = 0; i < tbl->num_entries; i++)
    {
	_XmWriteDragBuffer(&bmgr, False, (char *)&tbl->entries[i],
			   sizeof(XmDndAtomsTableEntryRec));

    }

    ((XmDndAtoms *) (bmgr.atoms))->data_size = bmgr.atom_size;

    pa = XmInternAtom(display, _XA_MOTIF_DRAG_ATOMS, False);

    win = get_drag_window(display);

    begin_protection(display, win);

    XChangeProperty(display, win, pa, pa, 8, PropModeReplace,
		    (unsigned char *)bmgr.atoms, bmgr.atom_size);

    if (bmgr.atoms != bmgr.atom_ptr)
    {
	XtFree(bmgr.atoms);
    }

    end_protection(display);

    if (bad_window)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "Bad window writing DRAG_ATOMS on DRAG_WINDOW.");
    }
}


/*
 * fetch the cached atoms table
 */
static XmDndTargetsTable
get_targets_table(Display *display)
{
    XmDndTargetsTable targets = NULL;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:get_targets_table(%d)\n",
		      __FILE__, __LINE__));

    if (displayToTargets == (XContext)0)
    {
	displayToTargets = XUniqueContext();
    }

    if (XFindContext(display, DefaultRootWindow(display),
		     displayToTargets, (XPointer *)&targets) != XCSUCCESS)
    {
	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:get_targets_table(%d) - NULL\n",
		      __FILE__, __LINE__));

	return NULL;
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:get_targets_table(%d) - %p\n",
		      __FILE__, __LINE__, targets));

    return targets;
}


/*
 * cache the atoms table
 */
static void
set_targets_table(Display *display, XmDndTargetsTable table)
{
    XmDndTargetsTable targets;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:set_targets_table(%d) - %p\n",
		      __FILE__, __LINE__, table));

    if (displayToTargets == (XContext)0)
    {
	displayToTargets = XUniqueContext();
    }

    if (XFindContext(display, DefaultRootWindow(display),
		     displayToTargets, (XPointer *)&targets) == XCSUCCESS)
    {

	if (targets == table)
	{
	    return;
	}

	XDeleteContext(display, DefaultRootWindow(display),
		       displayToTargets);
    }

    XSaveContext(display, DefaultRootWindow(display),
		 displayToTargets, (XPointer)table);
}


/*
 * create the default target table
 */
static XmDndTargetsTable
create_default_targets_table(Display *display)
{
    XmDndTargetsTable tbl;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:create_default_targets_table(%d)\n",
		      __FILE__, __LINE__));

    tbl = (XmDndTargetsTable)XtMalloc(sizeof(XmDndTargetsTableRec));

    tbl->num_entries = 2;
    tbl->entries =
	(XmDndTargetsTableEntry)XtMalloc(sizeof(XmDndTargetsTableEntryRec) *
					 tbl->num_entries);

    tbl->entries[0].num_targets = 1;
    tbl->entries[0].targets = &noTarget;

    tbl->entries[1].num_targets = 1;
    tbl->entries[1].targets = &stringTarget;

    set_targets_table(display, tbl);

    return tbl;
}


static Boolean
read_targets_table(Display *display, XmDndTargetsTable tbl)
{
    Atom ta, atype;
    Window win;
    int aformat, i;
    unsigned long nitems, bafter;
    XmDndTargets *targets = NULL;
    Boolean got_it = False;
    XmDndBufMgrRec bmgr;
    CARD16 nents;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_targets_table(%d)\n",
		      __FILE__, __LINE__));

    ta = XmInternAtom(display, _XA_MOTIF_DRAG_TARGETS, False);

    win = get_drag_window(display);

    begin_protection(display, win);

    if (XGetWindowProperty(display, win, ta, 0L, PROP_LENGTH,
			   False, ta,
			   &atype, &aformat, &nitems, &bafter,
			   (unsigned char **)&targets) == Success &&
	nitems >= 8)
    {
        DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_targets_table(%d) bafter=%i\n",
		      __FILE__, __LINE__, bafter));

	got_it = True;
    }

    end_protection(display);

    if (bad_window)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "Invalid DRAG WINDOW fetching DRAG_ATOMS.");
    }

    if (!got_it)
    {
	if (targets)
	{
	    XFree((XPointer)targets);
	}

	DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_targets_table(%d) - False\n",
		      __FILE__, __LINE__));

	return False;
    }

    if (targets->protocol_version != DND_PROTOCOL_VERSION)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "DND Protocol version mismatch.");
    }

    if (targets->byte_order != _XmByteOrder())
    {
	SWAP2BYTES(targets->num_target_lists);
	SWAP4BYTES(targets->data_size);
    }

    if (!tbl)
    {
	tbl = (XmDndTargetsTable)XtMalloc(sizeof(XmDndTargetsTableRec));

	tbl->num_entries = 0;
	tbl->entries = NULL;

	set_targets_table(display, tbl);
    }

    if (tbl->num_entries < targets->num_target_lists)
    {
	tbl->num_entries = targets->num_target_lists;
	tbl->entries =
	    (XmDndTargetsTableEntry)XtRealloc((char *)tbl->entries,
					      targets->num_target_lists *
					    sizeof(XmDndTargetsTableEntryRec));
    }

    if (targets->num_target_lists > 0)
    {

	bmgr.atoms = (char *)targets;
	bmgr.atom_start = (char *)(targets + 1);
	bmgr.atom_size = targets->data_size;

	for (i = 0; i < targets->num_target_lists; i++)
	{

	    _XmReadDragBuffer(&bmgr, False, (char *)&nents,
			      sizeof(CARD16));

	    if (targets->byte_order != _XmByteOrder())
	    {
		SWAP2BYTES(nents);
	    }

	    tbl->entries[i].num_targets = nents;
	    tbl->entries[i].targets = (Atom *)XtMalloc(nents * sizeof(Atom));

            read_atoms_from_drag_buffer(&bmgr, tbl->entries[i].num_targets,
                                        targets->byte_order != _XmByteOrder(),
                                        tbl->entries[i].targets);
	}
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:read_targets_table(%d) - True %p\n",
		      __FILE__, __LINE__, tbl));

    if (targets)
     {
         XFree((XPointer)targets);
     }
    return True;
}


static void
write_targets_table(Display *display, XmDndTargetsTable tbl)
{
    char targets[1000];
    XmDndTargets target_rec;
    XmDndBufMgrRec bmgr;
    Atom ta;
    int i;
    Window win;
    CARD16 nents;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:write_targets_table(%d)\n",
		      __FILE__, __LINE__));

    if (tbl == NULL)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "No DRAG_TARGETS to write to DRAG_WINDOW.");

	return;
    }

    target_rec.byte_order = _XmByteOrder();
    target_rec.protocol_version = DND_PROTOCOL_VERSION;
    target_rec.num_target_lists = tbl->num_entries;

    bmgr.atoms = targets;
    bmgr.atom_ptr = targets;
    bmgr.atom_start = NULL;
    bmgr.atom_size = 0;
    bmgr.atom_avail = 1000;

    _XmWriteDragBuffer(&bmgr, False, (char *)&target_rec, sizeof(XmDndTargets));

    for (i = 0; i < tbl->num_entries; i++)
    {
	nents = tbl->entries[i].num_targets;

	_XmWriteDragBuffer(&bmgr, False, (char *)&nents, sizeof(CARD16));

        write_atoms_to_drag_buffer(&bmgr, tbl->entries[i].num_targets,
                                   tbl->entries[i].targets);
    }

    ((XmDndTargets *)(bmgr.atoms))->data_size = bmgr.atom_size;

    ta = XmInternAtom(display, _XA_MOTIF_DRAG_TARGETS, False);

    win = get_drag_window(display);

    begin_protection(display, win);

    XChangeProperty(display, win, ta, ta, 8, PropModeReplace,
		    (unsigned char *)bmgr.atoms, bmgr.atom_size);

    if (bmgr.atoms != bmgr.atom_ptr)
    {
	XtFree(bmgr.atoms);
    }

    end_protection(display);

    if (bad_window)
    {
	_XmWarning((Widget)XmGetXmDisplay(display),
		   "Bad window writing DRAG_ATOMS on DRAG_WINDOW.");
    }
}


/*
 * fetch and/or set the ATOM_PAIRS.  This is triggered at least once
 * by the first call to XmInternAtom().
 */
extern void
_XmInitAtomPairs(Display *display)
{
    Window win;
    char *dstr;
    Display *d;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmInitAtomPairs(%d)\n",
		      __FILE__, __LINE__));

    if ((win = read_drag_window(display)) == None)
    {
	dstr = XDisplayString(display);

	d = XOpenDisplay(dstr);

	if (d == NULL)
	{
	    _XmWarning((Widget)XmGetXmDisplay(display),
		       "Where's your display?");

	    return;
	}

	XGrabServer(d);

	if ((win = read_drag_window(d)) == None)
	{
	    XSetCloseDownMode(d, RetainPermanent);

	    win = create_drag_window(d);

	    write_drag_window(d, &win);
	}

	XCloseDisplay(d);
    }

    set_drag_window(display, win);

    if (!read_atom_pairs(display))
    {
	XGrabServer(display);

	if (!read_atom_pairs(display))
	{
	    write_atom_pairs(display);
	}

	XUngrabServer(display);

	XFlush(display);
    }
}


/*
 * intialize the DRAG_TARGETS table, and the DRAG_ATOMS table
 */
extern void
_XmInitTargetsTable(Display *display)
{
    Window win;
    char *dstr;
    Display *d;
    XmDndAtomsTable atoms;
    XmDndTargetsTable targets;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmInitTargetsTable(%d)\n",
		      __FILE__, __LINE__));

    if ((win = read_drag_window(display)) == None)
    {

	dstr = XDisplayString(display);

	d = XOpenDisplay(dstr);

	if (d == NULL)
	{
	    _XmWarning((Widget)XmGetXmDisplay(display),
		       "Where's your display?");

	    return;
	}

	XGrabServer(d);

	if ((win = read_drag_window(d)) == None)
	{

	    XSetCloseDownMode(d, RetainPermanent);

	    win = create_drag_window(d);

	    write_drag_window(d, &win);
	}

	XCloseDisplay(d);
    }

    set_drag_window(display, win);

    if (!read_atom_pairs(display))
    {

	XGrabServer(display);

	if (!read_atom_pairs(display))
	{
	    write_atom_pairs(display);
	}

	XUngrabServer(display);

	XFlush(display);
    }

    if (!read_atoms_table(display, get_atoms_table(display)))
    {
	atoms = create_default_atoms_table(display);

	write_atoms_table(display, atoms);
    }

    if (!read_targets_table(display, get_targets_table(display)))
    {
	XGrabServer(display);

	if (!read_targets_table(display, get_targets_table(display)))
	{
	    targets = create_default_targets_table(display);

	    write_targets_table(display, targets);
	}

	XUngrabServer(display);

	XFlush(display);
    }
}


/*
 * called from DropSMgr's CreateInfo method.
 */
extern Cardinal
_XmIndexToTargets(Widget shell, Cardinal t_index, Atom **targetsRtn)
{
    Display *dpy = XtDisplay(shell);
    XmDndTargetsTable targets;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmIndexToTargets(%d)\n",
		      __FILE__, __LINE__));

    if ((targets = get_targets_table(dpy)) == NULL)
    {
	_XmInitTargetsTable(dpy);

	targets = get_targets_table(dpy);
    }

    if (t_index >= targets->num_entries)
    {
	if (!read_targets_table(dpy, targets))
	{
	    _XmInitTargetsTable(dpy);
	}

	targets = get_targets_table(dpy);
    }

    if (t_index >= targets->num_entries)
    {
	_XmWarning(shell, "Index out of range to _XmIndexToTargets. %i %i",
		t_index, targets->num_entries);

	*targetsRtn = NULL;

	return 0;
    }

    *targetsRtn = targets->entries[t_index].targets;

    return targets->entries[t_index].num_targets;
}


/*
 * minimize duplicates in the targets table by sorting compound entries
 */
static int
acompare(XmConst void *a, XmConst void *b)
{
    Atom *pa = (Atom *)a, *pb = (Atom *)b;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:acompare(%d)\n",
		      __FILE__, __LINE__));

    return *pa - *pb;
}

/*
 * also called from DropSMgr.  Creates target list if not already
 * there.
 */
Cardinal
_XmTargetsToIndex(Widget shell, Atom *tlist, Cardinal numTargets)
{
    Display *dpy = XtDisplay(shell);
    XmDndTargetsTable targets;
    Cardinal i;
    Atom *atoms;

    DEBUGOUT(_LtDebug(__FILE__, shell,
		      "%s:_XmTargetsToIndex(%d) - %p 0x%x targets %p\n", 
		      __FILE__, __LINE__,
		      tlist,
		      numTargets,
		      tlist ? tlist[0] : 0
		      ));
#if 0
    /* For Netscape 4.5 PR1 binary compatibility testing.
       NS seems to be calling this routine with an invalid (huge) number
       of numTargets.  I just bail out of the whole routine for testing.
       -jac Sept 22, 1998 */
    return 0;
#endif

    if ((targets = get_targets_table(dpy)) == NULL)
    {
	_XmInitTargetsTable(dpy);

	targets = get_targets_table(dpy);
    }
    DEBUGOUT(_LtDebug(__FILE__, shell,
		      "%s:_XmTargetsToIndex(%d) - targets->num_entries %i\n", 
		      __FILE__, __LINE__,
		      targets->num_entries
		      ));

#if 1
    /* 28 Jul 1999
       Navigator is still calling this with a huge value for numTargets.
       So for now just do a check for this and bail.....
     */
    for (i = 0; i < targets->num_entries; i++)
    {
	if (numTargets == targets->entries[i].num_targets)
	{
	    break;
	}
    }
    if ((int)numTargets > 1000)
    {
    	return(0);
    }
#endif

    atoms = (Atom *)XtMalloc(numTargets * sizeof(Atom));
    for	(i = 0; i < numTargets; i++)
    {
    	atoms[i] = tlist[i];
    }

    qsort(atoms, numTargets, sizeof(Atom), acompare);

    /* maybe it's already there ? */
    for (i = 0; i < targets->num_entries; i++)
    {
	if (numTargets == targets->entries[i].num_targets &&
	    memcmp(atoms, targets->entries[i].targets,
		 sizeof(Atom) * numTargets) == 0)
	{
	    XtFree((char *)atoms);

	    return i;
	}
    }

    XGrabServer(dpy);

    if (!read_targets_table(dpy, targets))
    {
	XUngrabServer(dpy);

	_XmInitTargetsTable(dpy);

	XGrabServer(dpy);

	targets = get_targets_table(dpy);
    }

    /* maybe somebody added while we've been fooling around, but before
     * we said nuhuh with the grab */
    for (i = 0; i < targets->num_entries; i++)
    {
	if (numTargets == targets->entries[i].num_targets &&
	    memcmp(atoms, targets->entries[i].targets,
		 sizeof(Atom) * numTargets) == 0)
	{
	    XtFree((char *)atoms);

	    return i;
	}
    }

    /* nope.  add them */
    i = targets->num_entries;

    targets->num_entries++;
    targets->entries =
	(XmDndTargetsTableEntry)XtRealloc((char *)targets->entries,
					  sizeof(XmDndTargetsTableEntryRec) *
					  targets->num_entries);

    targets->entries[i].num_targets = numTargets;
    targets->entries[i].targets = atoms;

    write_targets_table(dpy, targets);

    XUngrabServer(dpy);

    XFlush(dpy);

    return i;
}


/*
 * called when a drag starts, to allocate an Atom for the DragContext
 */
extern Atom
_XmAllocMotifAtom(Widget shell, Time time)
{
    XmDndAtomsTable atoms;
    Display *dpy = XtDisplay(shell);
    Atom alloc = None;
    int i;
    char buf[32];

    DEBUGOUT(_LtDebug(__FILE__, shell, "%s:_XmAllocMotifAtom(%d)\n",
		      __FILE__, __LINE__));
    DEBUGOUT(_LtDebug("DRAGSOURCE", shell, "%s:_XmAllocMotifAtom(%d)\n",
		      __FILE__, __LINE__));

    if ((atoms = get_atoms_table(dpy)) == NULL)
    {
	_XmInitTargetsTable(dpy);

	atoms = get_atoms_table(dpy);
    }

    XGrabServer(dpy);

    if (!read_atoms_table(dpy, atoms))
    {
	XUngrabServer(dpy);

	_XmInitTargetsTable(dpy);

	XGrabServer(dpy);

	atoms = get_atoms_table(dpy);
    }

    if (atoms->num_entries != 0)
    {
	for (i = 0; i < atoms->num_entries; i++)
	{
	    if (atoms->entries[i].time == CurrentTime)
	    {
		alloc = atoms->entries[i].atom;
		atoms->entries[i].time = time;

		break;
	    }
	}
    }

    if (alloc == None)
    {
	i = atoms->num_entries;

	atoms->num_entries++;

	atoms->entries =
	    (XmDndAtomsTableEntry) XtRealloc((char *)atoms->entries,
			 atoms->num_entries * sizeof(XmDndAtomsTableEntryRec));

	sprintf(buf, _XA_MOTIF_ATOM_FORMAT, i);
	alloc = XmInternAtom(dpy, buf, False);

	atoms->entries[i].atom = alloc;
	atoms->entries[i].time = time;
    }

    write_atoms_table(dpy, atoms);

    XUngrabServer(dpy);

    XFlush(dpy);

    if (_LtDebugInDebug("DRAGSOURCE", shell))
    {
    char *AtomName = XGetAtomName(dpy, alloc);

	DEBUGOUT(_LtDebug0("DRAGSOURCE", shell, "\t%s - 0x%x\n", AtomName, time));
    	XFree(AtomName);
    }

    return alloc;
}


/*
 * Free the atom.
 */
extern void
_XmFreeMotifAtom(Widget shell, Atom atom)
{
    XmDndAtomsTable atoms;
    Display *dpy = XtDisplay(shell);
    int i;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmFreeMotifAtom(%d)\n",
		      __FILE__, __LINE__));

    if (atom == None)
    {
	return;
    }

    if ((atoms = get_atoms_table(dpy)) == NULL)
    {
	_XmInitTargetsTable(dpy);

	atoms = get_atoms_table(dpy);
    }

    XGrabServer(dpy);

    if (!read_atoms_table(dpy, atoms))
    {
	XUngrabServer(dpy);

	_XmInitTargetsTable(dpy);

	XGrabServer(dpy);

	atoms = get_atoms_table(dpy);
    }

    if (atoms->num_entries != 0)
    {
	for (i = 0; i < atoms->num_entries; i++)
	{
	    if (atoms->entries[i].atom == atom)
	    {
		atoms->entries[i].time = CurrentTime;

		break;
	    }
	}
    }

    write_atoms_table(dpy, atoms);

    XUngrabServer(dpy);

    XFlush(dpy);
}


/*
 * I understand what it does, by why would you want to do it?
 */
extern void
_XmDestroyMotifWindow(Display *dpy)
{
    Atom dw;
    Window win;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmDestroyMotifWindow(%d)\n",
		      __FILE__, __LINE__));

    win = read_drag_window(dpy);
    if (win == None)
    {
	return;
    }

    dw = XmInternAtom(dpy, _XA_MOTIF_DRAG_WINDOW, False);

    XDeleteProperty(dpy, DefaultRootWindow(dpy), dw);

    XDestroyWindow(dpy, win);
}


/*
 * get the drag proxy window.  XmDisplay uses this to initialize
 * the proxy instance variable.
 */
Window
_XmGetDragProxyWindow(Display *display)
{
    Window win = None;
    Atom pw, atype;
    int aformat;
    unsigned long nitems, bafter;
    unsigned char *prop = NULL;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmGetDragProxyWindow(%d)\n",
		      __FILE__, __LINE__));

    if ((win = read_drag_window(display)) == None)
    {
	return None;
    }

    pw = XmInternAtom(display, _XA_MOTIF_DRAG_PROXY_WINDOW, False);

    begin_protection(display, win);
    if (XGetWindowProperty(display, win, pw, 0L, PROP_LENGTH,
			   False, AnyPropertyType,
			   &atype, &aformat, &nitems, &bafter, &prop)
	== Success && atype == XA_WINDOW && aformat == 32 && nitems == 1)
    {
        DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmGetDragProxyWindow(%d) - bafter=%i\n",
		      __FILE__, __LINE__, bafter));
	win = *((Window *)prop);
    }
    end_protection(display);
    if (prop)
    {
	XFree((XPointer)prop);
    }

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmGetDragProxyWindow(%d) - %p\n",
		      __FILE__, __LINE__, win));

    return win;
}


/*
 * get the drag receiver info property from the target
 *
 * check out the _XA_MOTIF_DRAG_RECEIVER_INFO property on a Motif
 * application's window.  There's a lot more there than what Daniel
 * talks about.  Must be due to PREREGISTER, yes?
 */
extern Boolean
_XmGetDragReceiverInfo(Display *display, Window win, XmDragReceiverInfo ri)
{
    Atom dri;
    XmDndReceiverProp *receiver;
    Atom type;
    int format;
    unsigned int border;
    unsigned long bafter, length;
    Window root;

    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmGetDragReceiverInfo(%d)\n",
		      __FILE__, __LINE__));
    DEBUGOUT(_LtDebug0("DRAGSOURCE", NULL, "%s:_XmGetDragReceiverInfo(%d) - %p\n",
		      __FILE__, __LINE__, win));

    dri = XmInternAtom(display, _XA_MOTIF_DRAG_RECEIVER_INFO, False);

    if (XGetWindowProperty(display, win, dri, 0L, PROP_LENGTH, False,
			   dri, &type, &format, &length, &bafter,
			   (unsigned char **)&receiver) != Success)
    {
	DEBUGOUT(_LtDebug0("DRAGSOURCE", NULL, "%s:_XmGetDragReceiverInfo(%d) - getting prop failed\n",
		      __FILE__, __LINE__));
	return False;
    }

    if (length < sizeof(XmDndReceiverProp))
    {
	ri->dragProtocolStyle = XmDRAG_NONE;
	XFree((char *)receiver);
	DEBUGOUT(_LtDebug0("DRAGSOURCE", NULL, "%s:_XmGetDragReceiverInfo(%d) - None available\n",
		      __FILE__, __LINE__));
	return False;
    }

    if (receiver->protocol_version != DND_PROTOCOL_VERSION)
    {
	_XmWarning(NULL, "Drag protocol version mismatch: %d vs %d\n",
		   DND_PROTOCOL_VERSION, receiver->protocol_version);
    }

    if (receiver->byte_order != _XmByteOrder())
    {
	SWAP4BYTES(receiver->proxy_window);
	SWAP2BYTES(receiver->num_drop_sites);
	SWAP4BYTES(receiver->total_size);
    }

    Display_ProxyWindow(XmGetXmDisplay(display)) = receiver->proxy_window;

    ri->dragProtocolStyle = receiver->protocol_style;
    ri->iccInfo = (XtPointer)XtMalloc(sizeof(XmShellDropSiteInfoRec));

    DSI_ByteOrder(ri->iccInfo) = receiver->byte_order;
    DSI_NumDropSites(ri->iccInfo) = receiver->num_drop_sites;
    DSI_Info(ri->iccInfo).atoms = (char *)receiver;
    DSI_Info(ri->iccInfo).atom_size = receiver->total_size;
    DSI_Info(ri->iccInfo).atom_start = (char *)(receiver + 1);
    DSI_Info(ri->iccInfo).names = (char *)receiver + receiver->total_size;
    DSI_Info(ri->iccInfo).name_size = length - receiver->total_size;

    XGetGeometry(display, win, &root,
		 &ri->xOrigin, &ri->yOrigin, &ri->width, &ri->height,
		 &border, &ri->depth);

    XTranslateCoordinates(display, win, root, -border, -border,
			  &ri->xOrigin, &ri->yOrigin, &root);

    return True;
}


extern void
_XmSetDragReceiverInfo(Widget w, Widget widget)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmSetDragReceiverInfo(%d)\n",
		      __FILE__, __LINE__));

}


extern void
_XmClearDragReceiverInfo(Widget shell)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmClearDragReceiverInfo(%d)\n",
		      __FILE__, __LINE__));

}

/*
 * free up the dropsite info
 */
extern void
_XmFreeDragReceiverInfo(XmShellDropSiteInfo di)
{
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmFreeDragReceiverInfo(%d)\n",
		      __FILE__, __LINE__));

    if (di)
    {
	if (di->info.atoms != NULL)
	{
	    XFree(di->info.atoms);
	}

	XtFree((char *)di);
    }
}


extern unsigned char
_XmByteOrder(void)
{
    static unsigned char byte_order = 0;

    /*
    DEBUGOUT(_LtDebug0(__FILE__, NULL, "%s:_XmByteOrder(%d)\n",
		      __FILE__, __LINE__));
    */

    if (!byte_order)
    {
	unsigned int endian = 1;

	byte_order = (*((char *)&endian)) ? 'l' : 'B';
    }

    return byte_order;
}
