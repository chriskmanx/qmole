/**
 *
 * $Id: Protocols.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $
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

static const char rcsid[] = "$Id: Protocols.c,v 1.1 2004/08/28 19:22:45 dannybackx Exp $";

#include <LTconfig.h>

#include <string.h>

#include <XmI/XmI.h>
#include <XmI/MacrosI.h>

#include <Xm/XmP.h>
#include <Xm/ProtocolsP.h>
#include <Xm/VendorSEP.h>

#include <XmI/DebugUtil.h>

static void class_initialize(void);
static void class_part_initialize(WidgetClass w_class);
static void initialize(Widget request, Widget new_w,
		       ArgList args, Cardinal *num_args);
static void destroy(Widget w);

#if 0
static Boolean set_values(Widget current, Widget request, Widget new_w,
			  ArgList args, Cardinal *num_args);
#endif

#if 0
static void _XmProtocolHandler(Widget w, XEvent *evp, String *params, Cardinal *nparams);
#else
static void _XmProtocolHandler(Widget w, XtPointer client_data, XEvent *ev, Boolean *ctd);
#endif

/* For the prototype police */
#ifdef	XmActivateWMProtocol
#undef	XmActivateWMProtocol
#endif
#ifdef	XmAddWMProtocols
#undef	XmAddWMProtocols
#endif
#ifdef	XmAddWMProtocolCallback
#undef	XmAddWMProtocolCallback
#endif
#ifdef	XmRemoveWMProtocolCallback
#undef	XmRemoveWMProtocolCallback
#endif
#ifdef	XmRemoveWMProtocols
#undef	XmRemoveWMProtocols
#endif
#ifdef	XmSetWMProtocolHooks
#undef	XmSetWMProtocolHooks
#endif

void XmActivateWMProtocol(Widget shell, Atom protocol);
void XmAddWMProtocols(Widget shell, Atom *protocols,
			     Cardinal num_protocols);
void XmAddWMProtocolCallback(Widget shell, Atom protocol,
				   XtCallbackProc callback, XtPointer closure);
void XmRemoveWMProtocolCallback(Widget shell, Atom protocol,
				   XtCallbackProc callback, XtPointer closure);
void XmRemoveWMProtocols(Widget shell, Atom *protocols,
				Cardinal num_protocols);
void XmSetWMProtocolHooks(Widget shell, Atom protocol,
				 XtCallbackProc prehook, XtPointer pre_closure,
				 XtCallbackProc posthook,
				 XtPointer post_closure);

/* undocumented, but export stuff from libXt */
extern void _XtAddCallback(XtCallbackList *, XtCallbackProc, XtPointer);
extern void _XtRemoveCallback(XtCallbackList *, XtCallbackProc, XtPointer);

/*
 * resources 
 */
#define Offset(field) XtOffsetOf(XmProtocolRec, protocol.field)
static XtResource resources[] =
{
    {
	XmNextensionType, XmCExtensionType, XmRExtensionType,
	sizeof(unsigned char), XtOffsetOf(XmProtocolRec, ext.extensionType),
	XmRImmediate, (XtPointer)XmPROTOCOL_EXTENSION
    },
    {
	XmNprotocolCallback, XmCProtocolCallback, XmRCallback,
	sizeof(XtCallbackList), Offset(callbacks),
	XmRImmediate, (XtPointer)NULL
    }
};


XmProtocolClassRec xmProtocolClassRec = {
    /* Object class part */
    {
	/* superclass            */ (WidgetClass) &xmExtClassRec,
        /* class_name            */ "XmProtocolClass",
	/* widget_size           */ sizeof(XmProtocolRec),
	/* class_initialize      */ class_initialize,
	/* class_part_initialize */ class_part_initialize,
	/* class_inited          */ False,
	/* initialize            */ initialize,
	/* initialize_hook       */ NULL,
	/* realize               */ NULL,
	/* actions               */ NULL,
	/* num_actions           */ 0,
	/* resources             */ resources,
	/* num_resources         */ XtNumber(resources),
	/* xrm_class             */ NULLQUARK,
	/* compress_motion       */ 0,
	/* compress_exposure     */ 0,
	/* compress_enterleave   */ 0,
	/* visible_interest      */ 0,
	/* destroy               */ destroy,
	/* resize                */ NULL,
	/* expose                */ NULL,
	/* set_values            */ NULL /*set_values*/,
	/* set_values_hook       */ NULL,
	/* set_values_almost     */ NULL,
	/* get_values_hook       */ NULL /*_XmExtGetValuesHook*/,
	/* accept_focus          */ NULL,
	/* version               */ XtVersion,
	/* callback offsets      */ NULL,
	/* tm_table              */ NULL,
	/* query_geometry        */ NULL,
        /* display_accelerator   */ NULL,
	/* extension             */ NULL
    },
    /* XmExtObject part */
    {
        /* syn_resources      */ NULL,
        /* num_syn_resources  */ 0,
        /* extension          */ NULL
    },
    /* Protocol Class part */
    {
        /* extension             */ NULL
    }
};


WidgetClass xmProtocolClass = (WidgetClass)&xmProtocolClassRec;


static void
class_initialize(void)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "Protocol ClassInitialize\n"));
}


static void
class_part_initialize(WidgetClass widget_class)
{
    DEBUGOUT(_LtDebug(__FILE__, NULL, "Protocol ClassPartInitialize\n"));
}


static void
initialize(Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "Protocol Initialize\n"));
}


static void
destroy(Widget w)
{
    DEBUGOUT(_LtDebug(__FILE__, w, "Protocol Destroy\n"));
}


#if 0
static Boolean
set_values(Widget current, Widget request, Widget new_w,
	   ArgList args, Cardinal *num_args)
{
    DEBUGOUT(_LtDebug(__FILE__, new_w, "Protocol SetValues\n"));

    return True;		/* FIX ME */
}
#endif


static XmAllProtocolsMgr
__XmGetAllMgr(Widget shell)
{
    XmAllProtocolsMgr newman;

    if (shell == NULL || !XmIsVendorShell(shell))
    {
	return NULL;
    }

    newman = (XmAllProtocolsMgr)_XmGetWidgetExtData(shell,
						    XmPROTOCOL_EXTENSION);

    DEBUGOUT(_LtDebug(__FILE__, shell,
		      "Get AllMgr: %08x %08x\n", shell, newman));

    return newman;
}


static XmProtocolMgr
__XmAddProperty(XmAllProtocolsMgr mgrs, Atom property)
{
    if (mgrs->max_protocol_mgrs == 0)
    {
	mgrs->max_protocol_mgrs = 8;

	mgrs->protocol_mgrs =
	    (XmProtocolMgr *)XtMalloc(mgrs->max_protocol_mgrs *
				      sizeof(XmProtocolMgr));
    }

    if ((mgrs->num_protocol_mgrs + 1) == mgrs->max_protocol_mgrs)
    {
	mgrs->max_protocol_mgrs *= 2;

	mgrs->protocol_mgrs =
	    (XmProtocolMgr *)XtRealloc((char *)mgrs->protocol_mgrs,
				       mgrs->max_protocol_mgrs *
				       sizeof(XmProtocolMgr));
    }

    mgrs->protocol_mgrs[mgrs->num_protocol_mgrs] =
	(XmProtocolMgr)XtCalloc(1, sizeof(XmProtocolMgrRec));
    mgrs->protocol_mgrs[mgrs->num_protocol_mgrs]->property = property;
    mgrs->num_protocol_mgrs++;

    return mgrs->protocol_mgrs[mgrs->num_protocol_mgrs - 1];
}


static XmProtocolMgr
__XmFindProperty(XmAllProtocolsMgr mgrs, Atom property)
{
    int i;

    for (i = 0; i < mgrs->num_protocol_mgrs; i++)
    {
	if (mgrs->protocol_mgrs[i]->property == property)
	    return mgrs->protocol_mgrs[i];
    }

    return NULL;
}


static XmProtocol
__XmAddProtocol(XmAllProtocolsMgr mgrs, Atom property, Atom protocol)
{
    XmProtocolMgr mgr;
    XmProtocol w;
    Widget ve;

    mgr = __XmFindProperty(mgrs, property);

    if (!mgr)
    {
	mgr = __XmAddProperty(mgrs, property);
    }

    if (mgr->max_protocols == 0)
    {
	mgr->max_protocols = 8;
	mgr->protocols =
	    (XmProtocolList)XtMalloc(mgrs->max_protocol_mgrs *
				     sizeof(XmProtocolMgr));
    }

    if ((mgr->num_protocols + 1) == mgr->max_protocols)
    {
	mgr->max_protocols *= 2;
	mgr->protocols =
	    (XmProtocolList)XtRealloc((char *)mgr->protocols,
				      mgrs->max_protocol_mgrs *
				      sizeof(XmProtocolMgr));
    }

    ve = _LtFindVendorExt(mgrs->shell);
    if (!ve)
    {
	_XmError(NULL, "Shell has no extension!\n");
    }

    w = (XmProtocol)_XmExtObjAlloc(xmProtocolClass->
					core_class.widget_size);
    ExtObj_LogicalParent(w) = mgrs->shell;
    ExtObj_ExtensionType(w) = XmPROTOCOL_EXTENSION;

    memset(&Protocol_PreHook(w), 0, sizeof(XtCallbackRec));
    memset(&Protocol_PostHook(w), 0, sizeof(XtCallbackRec));
    Protocol_Callbacks(w) = NULL;

    Protocol_Atom(w) = protocol;
    mgr->protocols[mgr->num_protocols] = w;
    mgr->num_protocols++;

    return w;
}


static XmProtocol
__XmFindProtocol(XmAllProtocolsMgr mgrs, Atom property, Atom protocol)
{
    XmProtocolMgr mgr;
    int i;

    mgr = __XmFindProperty(mgrs, property);
    if (!mgr)
    {
	return NULL;
    }

    for (i = 0; i < mgr->num_protocols; i++)
    {
	if (Protocol_Atom(mgr->protocols[i]) == protocol)
	{
	    return mgr->protocols[i];
	}
    }

    return NULL;
}


#if 0
static XtActionsRec actions[] =
{
    {"XmProtocolHandler", _XmProtocolHandler},
};
#endif


void
_XmInitProtocols(Widget w)
{
    XmAllProtocolsMgr newman;

    newman = (XmAllProtocolsMgr)XtCalloc(1, sizeof(XmAllProtocolsMgrRec));
    newman->shell = w;

    _XmPushWidgetExtData(w, (XmWidgetExtData)newman, XmPROTOCOL_EXTENSION);

    DEBUGOUT(_LtDebug(__FILE__, w, "Init Protocols: %08x %08x\n", w, newman));

#if 0
    XtAppAddActions(XtWidgetToApplicationContext(w),
		    actions, XtNumber(actions));
	/* We shouldn't add the same action again and again - it's not possible
	 * to remove it and the appContext's action table will grow.
	 *
	 * However, we can not do this just once either (e.g. in class_initialize)
	 * because we have no idea about how many app_contexts there are.
	 *
	 * We simply change the action for an event handler (they dont have
	 * no XmProtocolHandler action either)
	 */
#else
	XtAddEventHandler(w, 0, True, _XmProtocolHandler, NULL);
#endif
}


extern void
_XmDestroyProtocols(Widget w)
{
	XmAllProtocolsMgr allmgrs = NULL;
	int nmgr, nproto;

	_XmPopWidgetExtData(w, (XmWidgetExtData*) &allmgrs, XmPROTOCOL_EXTENSION);

	if ( ! allmgrs )
	{
	    return;
	}

	/* we could repeatedly call _XmRemoveProtocols here, but it is easier
	 * to just destroy everything thus avoiding any searching.
	 */

	/* remove all manager structures */
	for (nmgr = 0; nmgr < allmgrs->num_protocol_mgrs; nmgr++)
	{
		XmProtocolMgr mgr = allmgrs->protocol_mgrs[nmgr];
		/* destroy all protocol objects */
		for (nproto = 0; nproto < mgr->num_protocols; nproto++)
		{
		    _XmExtObjFree((XtPointer) mgr->protocols[nproto]);
		}
		XtFree((char*) mgr->protocols);
		XtFree((char*) mgr);
	}
	
	if (allmgrs->protocol_mgrs)
	{
	    XtFree((char*) allmgrs->protocol_mgrs);
	}
	
	XtFree((char*)allmgrs);
	XtRemoveEventHandler(w, XtAllEvents, True, _XmProtocolHandler, NULL);
}


/*
 * this ONLY gets called from VendorShellExt's realize callback
 */
extern void
_XmInstallProtocols(Widget w)
{
    XmAllProtocolsMgr mgrs;
    XmProtocolMgr mgr;
    Atom *protocols;
    int i, j;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmInstallProtocols\n"));

    if ((mgrs = __XmGetAllMgr(w)) == NULL)
    {
	_XmWarning(w, "No XmProtocolManager for shell %s\n", XtName(w));
	return;
    }

    for (i = 0; i < mgrs->num_protocol_mgrs; i++)
    {
	mgr = mgrs->protocol_mgrs[i];

	protocols = (Atom *)XtMalloc(sizeof(Atom) * mgr->num_protocols);

	for (j = 0; j < mgr->num_protocols; j++)
	{
	    protocols[j] = Protocol_Atom(mgr->protocols[j]);
	}

	XChangeProperty(XtDisplay(w), XtWindow(w),
			mgr->property, XA_ATOM, 32, PropModeReplace,
			(unsigned char *)protocols, mgr->num_protocols);

	XtFree((char *)protocols);
    }
}


static void
#if 0
_XmProtocolHandler(Widget w, XEvent *evp, String *params, Cardinal *nparams)
#else
_XmProtocolHandler(Widget w, XtPointer client_data, XEvent *evp, Boolean *ctd)
#endif
{
    Atom property, protocol;
    XmAllProtocolsMgr mgrs;
    XmProtocol p;
    XmAnyCallbackStruct cd;

    DEBUGOUT(_LtDebug(__FILE__, w, "_XmProtocolHandler\n"));

    cd.reason = XmCR_WM_PROTOCOLS;
    cd.event = evp;

    if (evp->type != ClientMessage)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XmProtocolHandler(not a client message)\n"));
	return;
    }

    property = evp->xclient.message_type;
    protocol = evp->xclient.data.l[0];

    if ((mgrs = __XmGetAllMgr(w)) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XmProtocolHandler(couldn't find manager struct)\n"));

	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XmProtocolHandler(couldn't find protocol)\n"));

	return;
    }

    if (Protocol_Active(p))
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XmProtocolHandler(calling callbacks)\n"));

	if (Protocol_PreHook(p).callback)
	{
	    (Protocol_PreHook(p).callback) (mgrs->shell,
					    Protocol_PreHook(p).closure,
					    (XtPointer)&cd);
	}

	XtCallCallbackList(mgrs->shell,
			   Protocol_Callbacks(p),
			   (XtPointer)&cd);

	if ((!Protocol_Callbacks(p)) && Protocol_PostHook(p).callback)
	{
	    (Protocol_PostHook(p).callback) (mgrs->shell,
					     Protocol_PostHook(p).closure,
					     (XtPointer)&cd);
	}
    }
    else
    {
	DEBUGOUT(_LtDebug(__FILE__, w,
			  "XmProtocolHandler(protocol not active)\n"));
    }
}


extern void
XmActivateProtocol(Widget shell,
		   Atom property,
		   Atom protocol)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    DEBUGOUT(_LtDebug(__FILE__, shell, "XmActivateProtocol\n"));

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
    {
	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL)
    {
	return;
    }

    Protocol_Active(p) = True;
}


extern void
XmDeactivateProtocol(Widget shell,
		     Atom property,
		     Atom protocol)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    DEBUGOUT(_LtDebug(__FILE__, shell, "XmDeactivateProtocol\n"));

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
    {
	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL)
    {
	return;
    }

    Protocol_Active(p) = False;
}


extern void
XmActivateWMProtocol(Widget shell,
		     Atom protocol)
{
    XmActivateProtocol(shell,
		       XM_WM_PROTOCOL_ATOM(shell),
		       protocol);
}


extern void
XmAddProtocols(Widget shell,
	       Atom property,
	       Atom *protocol,
	       Cardinal num_protocols)
{
#if 0
    char *tbl, *a;
    XtTranslations ctbl;
#endif
    int i;
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    DEBUGOUT(_LtDebug(__FILE__, shell, "XmAddProtocols\n"));

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, shell,
			  "XmAddProtocols: Can't find Management structure:"
			  " not a shell?\n"));
	return;
    }

	/* T. Straumann: I changed the action/translation implementation for an event
	 * 				 handler. This is easier and avoids some memory leaks.
	 */
#if 0
    a = XGetAtomName(XtDisplay(shell), property);
    tbl = (char *)XtMalloc(32 + strlen(a));
    strcpy(tbl, "<Message>");	/* len 9 */
    strcat(tbl, a);
    strcat(tbl, ": XmProtocolHandler()\n");	/* len 21 */

#ifdef NEVER_DO_THIS
	/* T. Straumann: never use XtParseTranslationTable() in a routine
	 *				 that might get called many times. The constructed
	 *               translation table is dynamically allocated and there's
	 *				 no way to free it.
	 *				 If you absolutely need this, it's better to use
	 *				 XtConvertAndStore() because, at least, identical tables 
	 *				 will be cached.
	 */
    ctbl = XtParseTranslationTable(tbl);
#else
	{   
	XrmValue from,to;
    Boolean result;
	from.size = sizeof(String);
	from.addr = tbl;
	to.size = sizeof(XtTranslations);
	to.addr = &ctbl;
	XtConvertAndStore(shell,XtRString,&from,XtRTranslationTable,&to);
	}
#endif

    XtAugmentTranslations(shell, ctbl);

    XtFree(tbl);
    XFree(a);
#endif

    for (i = 0; i < num_protocols; i++)
    {
	if ((p = __XmFindProtocol(mgrs, property, protocol[i])) == NULL)
	{
	    __XmAddProtocol(mgrs, property, protocol[i]);
	}
    }

    if (XtIsRealized(shell))
    {
	_XmInstallProtocols(shell);
    }

    /*
     * Automatically Activate
     */
    for (i = 0; i < num_protocols; i++)
    {
	XmActivateProtocol(shell, property, protocol[i]);
    }
}


extern void
XmAddWMProtocols(Widget shell,
		 Atom *protocols,
		 Cardinal num_protocols)
{
    XmAddProtocols(shell,
		   XM_WM_PROTOCOL_ATOM(shell),
		   protocols,
		   num_protocols);
}


extern void
XmAddProtocolCallback(Widget shell,
		      Atom property,
		      Atom protocol,
		      XtCallbackProc callback,
		      XtPointer closure)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    DEBUGOUT(_LtDebug(__FILE__, shell, "XmAddProtocolCallback\n"));

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, shell,
		      "XmAddProtocolCallback: Can't find Management structure:"
			  " not a shell?\n"));

	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL)
    {
	XmAddProtocols(shell, property, &protocol, 1);

	p = __XmFindProtocol(mgrs, property, protocol);
    }

    /*
     * Add Callback
     */
    _XtAddCallback(&Protocol_Callbacks(p), callback, closure);
}


extern void
XmAddWMProtocolCallback(Widget shell,
			Atom protocol,
			XtCallbackProc callback,
			XtPointer closure)
{
    XmAddProtocolCallback(shell,
			  XM_WM_PROTOCOL_ATOM(shell),
			  protocol,
			  callback,
			  closure);
}


extern void
XmRemoveProtocolCallback(Widget shell,
			 Atom property,
			 Atom protocol,
			 XtCallbackProc callback,
			 XtPointer closure)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    DEBUGOUT(_LtDebug(__FILE__, shell, "XmRemoveProtocolCallback\n"));

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, shell,
		   "XmRemoveProtocolCallback: Can't find Management structure:"
			  " not a shell?\n"));

	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL)
    {
	XmAddProtocols(shell, property, &protocol, 1);

	p = __XmFindProtocol(mgrs, property, protocol);
    }

    /*
     * Remove Callback
     */
    _XtRemoveCallback(&Protocol_Callbacks(p), callback, closure);
}


extern void
XmRemoveWMProtocolCallback(Widget shell,
			   Atom protocol,
			   XtCallbackProc callback,
			   XtPointer closure)
{
    XmRemoveProtocolCallback(shell,
			     XM_WM_PROTOCOL_ATOM(shell),
			     protocol,
			     callback,
			     closure);
}


extern void
XmRemoveProtocols(Widget shell,
		  Atom property,
		  Atom *protocols,
		  Cardinal num_protocols)
{
    XmAllProtocolsMgr mgrs;
	XmProtocolMgr	  mgr;
    int i, mgr_slot, proto_slot;

    DEBUGOUT(_LtDebug(__FILE__, shell, "XmRemoveProtocols\n"));

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
    {
	return;
    }

    for (mgr_slot = 0; mgr_slot < mgrs->num_protocol_mgrs; mgr_slot++)
    {
	if (mgrs->protocol_mgrs[mgr_slot]->property == property)
	    break;
    }

    if ( mgr_slot == mgrs->num_protocol_mgrs )
    {
	/* property not found */
	return;
    }

    mgr = mgrs->protocol_mgrs[mgr_slot];

    for (i = 0; i < num_protocols; i++)
    {
	proto_slot = 0;
	while (proto_slot < mgr->num_protocols)
        {
	    if (Protocol_Atom(mgr->protocols[proto_slot]) == protocols[i])
	    {
		int j;

		/* destroy protocol object */
	        _XmExtObjFree((XtPointer)mgr->protocols[proto_slot]);

		/* compact the remaining list */
		for (j = proto_slot; j < mgr->num_protocols - 1; j++)
		{
			mgr->protocols[j] = mgr->protocols[j+1];
		}

		/* free the list of protocols and remove the
		 * property from the list of managers
		 * if count drops to 0
		 */
		if ( --(mgr->num_protocols) == 0 )
		{
		    int j;
	
		    XtFree((char*)mgr->protocols);

		    XtFree((char*)mgrs->protocol_mgrs[mgr_slot]);

		    /* compact list of managers */
		    for ( j = mgr_slot; j < mgrs->num_protocol_mgrs - 1; j++)
		    {
			mgrs->protocol_mgrs[j] = mgrs->protocol_mgrs[j+1];
		    }
		    if ( --(mgrs->num_protocol_mgrs) == 0 )
		    {
			XtFree((char*) mgrs->protocol_mgrs);
			mgrs->protocol_mgrs = NULL;
			mgrs->max_protocol_mgrs = 0;
		    }
		    /* IMPORTANT NOTE: If we should get here, mgr is does
		     * not exist anymore and hence we must leave the loop
		     * immediately.
		     */
		    return;
		}
		break; /* and continue looking for the next protocol to
			* remove */
	    }
	    proto_slot++;
        }
    }
}


extern void
XmRemoveWMProtocols(Widget shell,
		    Atom *protocols,
		    Cardinal num_protocols)
{
    XmRemoveProtocols(shell,
		      XM_WM_PROTOCOL_ATOM(shell),
		      protocols,
		      num_protocols);
}


extern void
XmSetProtocolHooks(Widget shell,
		   Atom property,
		   Atom protocol,
		   XtCallbackProc prehook,
		   XtPointer pre_closure,
		   XtCallbackProc posthook,
		   XtPointer post_closure)
{
    XmAllProtocolsMgr mgrs;
    XmProtocol p;

    DEBUGOUT(_LtDebug(__FILE__, shell, "XmSetProtocolHooks\n"));

    if ((mgrs = __XmGetAllMgr(shell)) == NULL)
    {
	DEBUGOUT(_LtDebug(__FILE__, shell,
			  "XmSetProtocolHooks: Can't find Management structure:"
			  " not a shell?\n"));

	return;
    }

    if ((p = __XmFindProtocol(mgrs, property, protocol)) == NULL)
    {
	XmAddProtocols(shell, property, &protocol, 1);

	p = __XmFindProtocol(mgrs, property, protocol);
    }

    Protocol_PreHook(p).callback = prehook;
    Protocol_PreHook(p).closure = pre_closure;
    Protocol_PostHook(p).callback = posthook;
    Protocol_PostHook(p).closure = post_closure;
}


extern void
XmSetWMProtocolHooks(Widget shell,
		     Atom protocol,
		     XtCallbackProc prehook,
		     XtPointer pre_closure,
		     XtCallbackProc posthook,
		     XtPointer post_closure)
{
    XmSetProtocolHooks(shell,
		       XM_WM_PROTOCOL_ATOM(shell),
		       protocol,
		       prehook,
		       pre_closure,
		       posthook,
		       post_closure);
}
