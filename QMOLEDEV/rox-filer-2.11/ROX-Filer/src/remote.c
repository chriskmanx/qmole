/*
 * ROX-Filer, filer for the ROX desktop project
 * Copyright (C) 2006, Thomas Leonard and others (see changelog for details).
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 * more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* This code is used to communicate between two copies of the filer:
 * If the filer is run and the same version of the filer is already
 * running on the same machine then the new copy simply asks the old
 * one deal with it and quits.
 */

#include "config.h"

#include <string.h>

#include <gdk/gdkx.h>
#include <X11/X.h>
#include <X11/Xatom.h>
#include <gtk/gtk.h>
#include <gtk/gtkinvisible.h>
#include <libxml/parser.h>

#include "global.h"

#include "main.h"
#include "support.h"
#include "gui_support.h"
#include "run.h"
#include "remote.h"
#include "filer.h"
#include "pinboard.h"
#include "panel.h"
#include "action.h"
#include "type.h"
#include "display.h"
#include "xml.h"
#include "diritem.h"
#include "usericons.h"

static GdkAtom filer_atom;	/* _ROX_FILER_EUID_VERSION_HOST */
static GdkAtom filer_atom_any;	/* _ROX_FILER_EUID_HOST */
static GdkAtom xsoap;		/* _XSOAP */

typedef struct _SOAP_call SOAP_call;
typedef xmlNodePtr (*SOAP_func)(GList *args);

struct _SOAP_call {
	SOAP_func func;
	gchar **required_args;
	gchar **optional_args;
};

static GHashTable *rpc_calls = NULL; /* MethodName -> Function */

/* Static prototypes */
static GdkWindow *get_existing_ipc_window(void);
static gboolean get_ipc_property(GdkWindow *window, Window *r_xid);
static void soap_send(GtkWidget *from, GdkAtom prop, GdkWindow *dest);
static gboolean client_event(GtkWidget *window,
				 GdkEventClient *event,
				 gpointer data);
static void soap_done(GtkWidget *widget,
		      GdkEventProperty *event,
		      gpointer data);
static void soap_register(char *name, SOAP_func func, char *req, char *opt);
static xmlNodePtr soap_invoke(xmlNode *method);

static xmlNodePtr rpc_Version(GList *args);
static xmlNodePtr rpc_OpenDir(GList *args);
static xmlNodePtr rpc_CloseDir(GList *args);
static xmlNodePtr rpc_Examine(GList *args);
static xmlNodePtr rpc_Show(GList *args);
static xmlNodePtr rpc_Pinboard(GList *args);
static xmlNodePtr rpc_Panel(GList *args);
static xmlNodePtr rpc_Run(GList *args);
static xmlNodePtr rpc_RunURI(GList *args);
static xmlNodePtr rpc_Copy(GList *args);
static xmlNodePtr rpc_Move(GList *args);
static xmlNodePtr rpc_Link(GList *args);
static xmlNodePtr rpc_FileType(GList *args);
static xmlNodePtr rpc_Mount(GList *args);
static xmlNodePtr rpc_Unmount(GList *args);

static xmlNodePtr rpc_PanelAdd(GList *args);
static xmlNodePtr rpc_PanelRemove(GList *args);
static xmlNodePtr rpc_PinboardAdd(GList *args);
static xmlNodePtr rpc_PinboardRemove(GList *args);
static xmlNodePtr rpc_SetBackdrop(GList *args);
static xmlNodePtr rpc_SetBackdropApp(GList *args);

static xmlNodePtr rpc_SetIcon(GList *args);
static xmlNodePtr rpc_UnsetIcon(GList *args);

/****************************************************************
 *			EXTERNAL INTERFACE			*
 ****************************************************************/


/* Try to get an already-running filer to handle things (only if
 * new_copy is FALSE); TRUE if we succeed.
 * Create an IPC widget so that future filers can contact us.
 */
gboolean remote_init(xmlDocPtr rpc, gboolean new_copy)
{
	guchar		*unique_id;
	GdkWindow	*existing_ipc_window;
	GtkWidget	*ipc_window;
	Window		xwindow;

	/* xmlDocDump(stdout, rpc); */

	rpc_calls = g_hash_table_new(g_str_hash, g_str_equal);

	soap_register("Version", rpc_Version, NULL, NULL);

	soap_register("Run", rpc_Run, "Filename", NULL);
	soap_register("OpenDir", rpc_OpenDir, "Filename",
			      "Style,Details,Sort,Class,ID,Hidden,Filter");
	soap_register("CloseDir", rpc_CloseDir, "Filename", NULL);
	soap_register("Examine", rpc_Examine, "Filename", NULL);
	soap_register("Show", rpc_Show, "Directory,Leafname", NULL);
	soap_register("RunURI", rpc_RunURI, "URI", NULL);

	soap_register("Pinboard", rpc_Pinboard, NULL, "Name");
	soap_register("Panel", rpc_Panel, NULL, "Side,Name");

	soap_register("FileType", rpc_FileType, "Filename", NULL);

	soap_register("Copy", rpc_Copy, "From,To", "Leafname,Quiet");
	soap_register("Move", rpc_Move, "From,To", "Leafname,Quiet");
	soap_register("Link", rpc_Link, "From,To", "Leafname");
	soap_register("Mount", rpc_Mount, "MountPoints", "OpenDir,Quiet");
	soap_register("Unmount", rpc_Unmount, "MountPoints", "Quiet");

	soap_register("SetBackdrop", rpc_SetBackdrop, "Filename,Style", NULL);
	soap_register("SetBackdropApp", rpc_SetBackdropApp, "App", NULL);
	soap_register("PinboardAdd", rpc_PinboardAdd, "Path", "X,Y,Label,Shortcut,Args,Locked,Update");
	soap_register("PinboardRemove", rpc_PinboardRemove, "Path", "Label");
	soap_register("PanelAdd", rpc_PanelAdd, "Side,Path", "Label,After,Shortcut,Args,Locked");
	soap_register("PanelRemove", rpc_PanelRemove, "Side", "Path,Label");
 	soap_register("SetIcon", rpc_SetIcon, "Path,Icon", NULL);
 	soap_register("UnsetIcon", rpc_UnsetIcon, "Path", NULL);

	/* Look for a property on the root window giving the IPC window
	 * of an already-running copy of this version of the filer, running
	 * on the same machine and with the same euid.
	 */
	unique_id = g_strdup_printf("_ROX_FILER_%d_%s_%s",
				(int) euid, VERSION, our_host_name());
	filer_atom = gdk_atom_intern(unique_id, FALSE);
	g_free(unique_id);

	xsoap = gdk_atom_intern("_XSOAP", FALSE);

	/* If we find a running copy, we'll need a window to put the
	 * SOAP message in before sending.
	 * If there's no running copy, we'll need a window to receive
	 * future SOAP message events.
	 * Either way, we'll need a window for it...
	 */
	ipc_window = gtk_invisible_new();
	gtk_widget_realize(ipc_window);

	XGrabServer(GDK_DISPLAY());

	existing_ipc_window = new_copy ? NULL : get_existing_ipc_window();
	if (existing_ipc_window)
	{
		xmlChar *mem;
		int	size;

		XUngrabServer(GDK_DISPLAY());

		xmlDocDumpMemory(rpc, &mem, &size);
		g_return_val_if_fail(size > 0, FALSE);
		
		/* Since Gtk might have selected this already, we'd
		 * better do it BEFORE changing the property.
		 */
		gtk_widget_add_events(ipc_window, GDK_PROPERTY_CHANGE_MASK);
		g_signal_connect(ipc_window, "property-notify-event",
				G_CALLBACK(soap_done), GINT_TO_POINTER(xsoap));

		gdk_property_change(ipc_window->window, xsoap,
				gdk_x11_xatom_to_atom(XA_STRING), 8,
				GDK_PROP_MODE_REPLACE, mem, size);
		g_free(mem);

		soap_send(ipc_window, xsoap, existing_ipc_window);

		return TRUE;
	}

	xwindow = GDK_WINDOW_XWINDOW(ipc_window->window);
	
	/* Make the IPC window contain a property pointing to
	 * itself - this can then be used to check that it really
	 * is an IPC window.
	 */
	gdk_property_change(ipc_window->window, filer_atom,
			gdk_x11_xatom_to_atom(XA_WINDOW), 32,
			GDK_PROP_MODE_REPLACE,
			(void *) &xwindow, 1);

	/* Get notified when we get a message */
	g_signal_connect(ipc_window, "client-event",
			G_CALLBACK(client_event), NULL);

	/* Make the root window contain a pointer to the IPC window */
	gdk_property_change(gdk_get_default_root_window(), filer_atom,
			gdk_x11_xatom_to_atom(XA_WINDOW), 32,
			GDK_PROP_MODE_REPLACE,
			(void *) &xwindow, 1);

	XUngrabServer(GDK_DISPLAY());

	/* Also have a property without the version number, for programs
	 * that are happy to talk to any version of the filer.
	 */
	unique_id = g_strdup_printf("_ROX_FILER_%d_%s",
				(int) euid, our_host_name());
	filer_atom_any = gdk_atom_intern(unique_id, FALSE);
	g_free(unique_id);
	/* On the IPC window... */
	gdk_property_change(ipc_window->window, filer_atom_any,
			gdk_x11_xatom_to_atom(XA_WINDOW), 32,
			GDK_PROP_MODE_REPLACE,
			(void *) &xwindow, 1);
	/* ... and on the root */
	gdk_property_change(gdk_get_default_root_window(), filer_atom_any,
			gdk_x11_xatom_to_atom(XA_WINDOW), 32,
			GDK_PROP_MODE_REPLACE,
			(void *) &xwindow, 1);

	return FALSE;
}

/* Executes the RPC call(s) in the given SOAP message and returns
 * the reply.
 */
xmlDocPtr run_soap(xmlDocPtr soap)
{
	xmlNodePtr body, node, rep_body, reply;
	xmlDocPtr rep_doc = NULL;

	g_return_val_if_fail(soap != NULL, NULL);

	/* Make sure we don't quit before doing the whole list
	 * (there's a command that closes windows)
	 */
	number_of_windows++;

	node = xmlDocGetRootElement(soap);
	if (!node->ns)
		goto bad_soap;

	if (strcmp(node->ns->href, SOAP_ENV_NS) != 0 &&
	    strcmp(node->ns->href, SOAP_ENV_NS_OLD) != 0)
		goto bad_soap;

	body = get_subnode(node, SOAP_ENV_NS, "Body");
	if (!body)
		body = get_subnode(node, SOAP_ENV_NS_OLD, "Body");
	if (!body)
		goto bad_soap;

	for (node = body->xmlChildrenNode; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE)
			continue;

		if (node->ns == NULL || strcmp(node->ns->href, ROX_NS) != 0)
		{
			g_warning("Unknown namespace %s",
					node->ns ? node->ns->href
						 : (xmlChar *) "(none)");
			continue;
		}
		
		reply = soap_invoke(node);

		if (reply)
		{
			if (!rep_doc)
				rep_doc = soap_new(&rep_body);
			xmlAddChild(rep_body, reply);
		}
	}

	goto out;

bad_soap:
	g_warning("Bad SOAP message received!");

out:
	number_of_windows--;

	return rep_doc;
}


/* Parse a SOAP reply and extract any fault strings, returning them as
 * a NULL terminated list of strings (g_strfreev).
 */
gchar **extract_soap_errors(xmlDocPtr reply)
{
	gchar **errs;
	GSList *errlist=NULL, *tmp;
	int i, n;
	
	xmlNodePtr root, node;

	if(!reply)
		return NULL;

	root=xmlDocGetRootElement(reply);
	if(strcmp(root->name, "Envelope")==0) {
		node=get_subnode(root, SOAP_ENV_NS, "Body");
		if(node) {
			xmlNodePtr sub, fault;
			for(sub=node->xmlChildrenNode; sub;
			    sub=sub->next) {
				if(sub->type != XML_ELEMENT_NODE)
					continue;
				if(strcmp(sub->name, "env:Fault")!=0)
					continue;
				
				/*if (sub->ns == NULL)
				  continue;

				  if(strcmp(sub->ns->href,
				  SOAP_ENV_NS)!=0)
				  continue;*/

				fault=get_subnode(sub, NULL,
						  "faultstring");
				if(fault) {
					xmlChar *txt;
					txt=xmlNodeGetContent(fault);
					if(txt) {
						errlist=g_slist_append(errlist,
								       g_strdup(txt));
								       
						xmlFree(txt);
					}
				}
			}
		}
	}

	n=g_slist_length(errlist);
	errs=g_malloc(sizeof(gchar *)*(n+1));
	for(i=0, tmp=errlist; i<n; i++, tmp=g_slist_next(tmp))
		errs[i]=tmp->data;
	errs[n]=NULL;

	g_slist_free(errlist);

	return errs;
}

/****************************************************************
 *			INTERNAL FUNCTIONS			*
 ****************************************************************/

/* Register this function to handle SOAP calls to method 'name'.
 * 'req' and 'opt' are comma separated lists of argument names, in the order
 * that they are to be delivered to the function.
 * NULL will be passed for an opt argument if not given.
 * Otherwise, the parameter is the xmlNode from the call.
 */
static void soap_register(char *name, SOAP_func func, char *req, char *opt)
{
	SOAP_call *call;

	call = g_new(SOAP_call, 1);
	call->func = func;
	call->required_args = req ? g_strsplit(req, ",", 0) : NULL;
	call->optional_args = opt ? g_strsplit(opt, ",", 0) : NULL;

	g_hash_table_insert(rpc_calls, g_strdup(name), call);
}

/* Get the remote IPC window of the already-running filer if there
 * is one.
 */
static GdkWindow *get_existing_ipc_window(void)
{
	Window		xid, xid_confirm;
	GdkWindow	*window;

	if (!get_ipc_property(gdk_get_default_root_window(), &xid))
		return NULL;

	if (gdk_window_lookup(xid))
		return NULL;	/* Stale handle which we now own */

	window = gdk_window_foreign_new(xid);
	if (!window)
		return NULL;

	if (!get_ipc_property(window, &xid_confirm) || xid_confirm != xid)
		return NULL;

	return window;
}

/* Returns the 'rox_atom' property of 'window' */
static gboolean get_ipc_property(GdkWindow *window, Window *r_xid)
{
	guchar		*data;
	gint		format, length;
	gboolean	retval = FALSE;
	
	if (gdk_property_get(window, filer_atom,
			gdk_x11_xatom_to_atom(XA_WINDOW), 0, 4,
			FALSE, NULL, &format, &length, &data) && data)
	{
		/* Note: values with format=32 are stored as longs client-side,
		 * which may be more than 32 bits on some systems.
		 */
		if (format == 32 && length == sizeof(long))
		{
			long windowID = *((long *) data);
			retval = TRUE;
			*r_xid = (Window) windowID;
		}
		g_free(data);
	}

	return retval;
}

static char *read_property(GdkWindow *window, GdkAtom prop, gint *out_length)
{
	gint	grab_len = 4096;
	gint	length;
	guchar	*data;
	guchar	*retval = NULL;

	gdk_error_trap_push();

	while (!retval)
	{
		if (!(gdk_property_get(window, prop,
				gdk_x11_xatom_to_atom(XA_STRING), 0, grab_len,
				FALSE, NULL, NULL,
				&length, &data) && data))
			goto out;	/* Error? */

		if (length >= grab_len)
		{
			/* Didn't get all of it - try again */
			grab_len <<= 1;
			g_free(data);
			continue;
		}

		data = g_realloc(data, length + 1);
		data[length] = '\0';	/* libxml seems to need this */
		*out_length = length;

		retval = data;
	}
out:

	if (gdk_error_trap_pop() == Success)
		return retval;

	g_warning("Error reading %s property!", gdk_atom_name(prop));

	g_free(retval);
	return NULL;
}

static gboolean client_event(GtkWidget *window,
				 GdkEventClient *event,
				 gpointer user_data)
{
	GdkWindow *src_window;
	GdkAtom prop;
	xmlDocPtr reply;
	guchar	*data;
	gint	length;
	xmlDocPtr doc;

	if (event->message_type != xsoap)
		return FALSE;

	src_window = gdk_window_foreign_new(event->data.l[0]);
	if (!src_window)
	{
		g_warning("SOAP message sender window was destroyed before I \n"
			  "could read it.");
		return FALSE;
	}
	prop = gdk_x11_xatom_to_atom(event->data.l[1]);

	data = read_property(src_window, prop, &length);
	if (!data)
		return TRUE;

	doc = xmlParseMemory(g_strndup(data, length), length);
	g_free(data);

	reply = run_soap(doc);
	if (number_of_windows == 0)
		gtk_main_quit();

	xmlFreeDoc(doc);

	if (reply)
	{
		/* Send reply back... */
		xmlChar *mem;
		int	size;

		xmlDocDumpMemory(reply, &mem, &size);
		g_return_val_if_fail(size > 0, TRUE);

		gdk_error_trap_push();
		gdk_property_change(src_window, prop,
			gdk_x11_xatom_to_atom(XA_STRING), 8,
			GDK_PROP_MODE_REPLACE, mem, size);
		g_free(mem);
		gdk_flush();
		if (gdk_error_trap_pop() != Success)
			g_warning("Failed to send SOAP reply!");
	}
	else
	{
		gdk_error_trap_push();
		gdk_property_delete(src_window, prop);
		gdk_flush();
		if (gdk_error_trap_pop() != Success)
			g_warning("Failed to send SOAP reply!");
	}

	return TRUE;
}

/* Some handy functions for processing SOAP RPC arguments... */

/* Returns TRUE, FALSE, or -1 (if argument is unspecified) */
static int bool_value(xmlNode *arg)
{
	int answer;
	char *optval;

	if (!arg)
	        return -1;

	optval = xmlNodeGetContent(arg);
	answer = text_to_boolean(optval, -1);	/* XXX: Report error? */
	g_free(optval);

	return answer;
}

/* Returns the text of this arg as a string, or NULL if the (optional)
 * argument wasn't supplied. Returns "" if the arg is empty.
 * g_free() the result.
 */
static char *string_value(xmlNode *arg)
{
	char *retval;

	if (!arg)
		return NULL;
	
	retval = xmlNodeGetContent(arg);

	return retval ? retval : g_strdup("");
}

/* Returns the text of this arg as an int, or the default value if not
 * supplied or not an int.
 */
static int int_value(xmlNode *arg, int def)
{
	char *str, *end;
	int i;

	if (!arg)
		return def;
	
	str = xmlNodeGetContent(arg);
	if (!str || !str[0])
		return def;

	i = (int) strtol(str, &end, 0);

	return (end > str) ? i : def;
}

/* Return a list of strings, one for each child node of arg.
 * g_list_free the list, and g_free each string.
 */
static GList *list_value(xmlNode *arg)
{
	GList *list = NULL;
	xmlNode *node;

	for (node = arg->xmlChildrenNode; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE)
			continue;

		list = g_list_append(list, string_value(node));
	}

	return list;
}

#define ARG(n) ((xmlNode *) g_list_nth(args, n)->data)

/* The RPC handlers all work in the same way -- they get passed a list of
 * xmlNode arguments from the RPC call and they return the result node, or
 * NULL if there isn't a result.
 */

static xmlNodePtr rpc_SetIcon(GList *args)
{
	char	   *path, *icon;
	
	path = string_value(ARG(0));
	icon = string_value(ARG(1));

	add_globicon(path,icon);

	g_free(path);
	g_free(icon);

	return NULL;
}

static xmlNodePtr rpc_UnsetIcon(GList *args)
{
	char	   *path;
	
	path = string_value(ARG(0));

	delete_globicon(path);

	g_free(path);

	return NULL;
}

static xmlNodePtr rpc_Version(GList *args)
{
	xmlNodePtr reply;

	reply = xmlNewNode(NULL, "rox:VersionResponse");
	xmlNewNs(reply, SOAP_RPC_NS, "soap");
	xmlNewTextChild(reply, NULL, "soap:result", VERSION);

	return reply;
}

/* Args: Path, [Style, Details, Sort, Class, Window] */
static xmlNodePtr rpc_OpenDir(GList *args)
{
	char	   *path;
	char       *style, *details, *sort, *class, *window;
	int         hidden=FALSE;
	char       *filter_string=NULL;
	FilerWindow *fwin = NULL;

	path = string_value(ARG(0));
	class = string_value(ARG(4));
	window = string_value(ARG(5));
	
	if (window)
		fwin = filer_get_by_id(window);

	if (!fwin)
	{
		fwin = filer_opendir(path, NULL, class);
		if (window)
			filer_set_id(fwin, window); 
	}
	else
		filer_change_to(fwin, path, NULL);

	g_free(path);
	g_free(class);
	g_free(window);
	if (!fwin)
		return NULL;

	style = string_value(ARG(1));
	details = string_value(ARG(2));
	sort = string_value(ARG(3));
	hidden = bool_value(ARG(6));
	filter_string = string_value(ARG(7));

	if (style)
	{
		DisplayStyle ds;

		ds = !g_strcasecmp(style, "Large") ? LARGE_ICONS :
		     !g_strcasecmp(style, "Small") ? SMALL_ICONS :
		     !g_strcasecmp(style, "Huge")  ? HUGE_ICONS :
		     !g_strcasecmp(style, "Automatic")  ? AUTO_SIZE_ICONS :
		     				     UNKNOWN_STYLE;
		if (ds == UNKNOWN_STYLE)
			delayed_error(_("Unknown style '%s'"), style);
		else
			display_set_layout(fwin, ds, fwin->details_type, TRUE);

		g_free(style);
	}

	if (details)
	{
		DetailsType dt;
		ViewType view_type;
		
		dt = !g_strcasecmp(details, "None") ? DETAILS_NONE :
		     !g_strcasecmp(details, "ListView") ? DETAILS_NONE :
		     !g_strcasecmp(details, "Size") ? DETAILS_SIZE :
		     !g_strcasecmp(details, "Type") ? DETAILS_TYPE :
		     !g_strcasecmp(details, "Times") ? DETAILS_TIMES :
		     !g_strcasecmp(details, "Permissions")
		     				? DETAILS_PERMISSIONS :
						  DETAILS_UNKNOWN;

		if (dt == DETAILS_UNKNOWN)
			delayed_error(_("Unknown details type '%s'"), details);
		else
			display_set_layout(fwin, fwin->display_style_wanted,
					   dt, TRUE);

		if (g_strcasecmp(details, "ListView") == 0)
			view_type = VIEW_TYPE_DETAILS;
		else
			view_type = VIEW_TYPE_COLLECTION;

		if (view_type != fwin->view_type)
			filer_set_view_type(fwin, view_type);
		
		g_free(details);
	}

	if (sort)
	{
		SortType type;

		type = !g_strcasecmp(sort, "Name") ? SORT_NAME :
		       !g_strcasecmp(sort, "Type") ? SORT_TYPE :
		       !g_strcasecmp(sort, "Date") ? SORT_DATE :
 		       !g_strcasecmp(sort, "Size") ? SORT_SIZE :
 		       !g_strcasecmp(sort, "Owner") ? SORT_OWNER :
 		       !g_strcasecmp(sort, "Group") ? SORT_GROUP :
						       -1;
		if (type == -1)
			delayed_error(_("Unknown sorting type '%s'"), sort);
		else
			display_set_sort_type(fwin, type, GTK_SORT_ASCENDING);

		g_free(sort);
	}
	if (hidden!=-1)
	{
		display_set_hidden(fwin, hidden);
	}

	if (filter_string)
		display_set_filter(fwin, FILER_SHOW_GLOB, filter_string);
	else
		display_set_filter(fwin, FILER_SHOW_ALL, NULL);

	return NULL;
}

static xmlNodePtr rpc_Run(GList *args)
{
	char	   *path;

	path = string_value(ARG(0));
	run_by_path(path);
	g_free(path);

	return NULL;
}

static xmlNodePtr rpc_RunURI(GList *args)
{
	char	   *uri, *errmsg=NULL;
	xmlNodePtr  reply=NULL;

	uri = string_value(ARG(0));
	run_by_uri(uri, &errmsg);
	g_free(uri);
	
	if(errmsg)
	{
		reply = xmlNewNode(NULL, "env:Fault");
		xmlNewNs(reply, SOAP_RPC_NS, "rpc");
		xmlNewNs(reply, SOAP_ENV_NS, "env");
		xmlNewTextChild(reply, NULL, "faultcode",
						"Failed");
		xmlNewTextChild(reply, NULL, "faultstring", errmsg);
		g_free(errmsg);
	}

	return reply;
}

static xmlNodePtr rpc_CloseDir(GList *args)
{
	char	   *path;

	path = string_value(ARG(0));
	filer_close_recursive(path);
	g_free(path);

	return NULL;
}

static xmlNodePtr rpc_Examine(GList *args)
{
	char	   *path;

	path = string_value(ARG(0));
	examine(path);
	g_free(path);

	return NULL;
}

static xmlNodePtr rpc_Show(GList *args)
{
	char	   *dir, *leaf;
	
	dir = string_value(ARG(0));
	leaf = string_value(ARG(1));

	/* XXX: Seems silly to join them only to split again later... */
	open_to_show(make_path(dir, leaf));

	g_free(dir);
	g_free(leaf);

	return NULL;
}
		
static xmlNodePtr rpc_Pinboard(GList *args)
{
	char *name = NULL;

	name = string_value(ARG(0));
	pinboard_activate(name);
	g_free(name);

	return NULL;
}

/* args = Filename, Style */
static xmlNodePtr rpc_SetBackdrop(GList *args)
{
	char *file;
	char *style;
	BackdropStyle s;

	file = string_value(ARG(0));
	style = string_value(ARG(1));

	s = !g_strcasecmp(style, "Tile") ? BACKDROP_TILE :
	    !g_strcasecmp(style, "Scale") ? BACKDROP_SCALE :
	    !g_strcasecmp(style, "Stretch") ? BACKDROP_STRETCH :
	    !g_strcasecmp(style, "Centre") ? BACKDROP_CENTRE :
					     BACKDROP_NONE;

	if (s == BACKDROP_NONE)
		g_warning("Invalid style '%s' for backdrop", style);
	else
		pinboard_set_backdrop(file, s);

	g_free(file);
	g_free(style);

	return NULL;
}

/* args = App */
static xmlNodePtr rpc_SetBackdropApp(GList *args)
{
	char *app;

	app = string_value(ARG(0));

	pinboard_set_backdrop_app(app);

	g_free(app);

	return NULL;
}

/* args = Path, [X, Y, Label, Shortcut, Args, Locked, Update] */
static xmlNodePtr rpc_PinboardAdd(GList *args)
{
	char *path = NULL;
	gchar *name, *shortcut, *xargs;
	int x, y, locked, update;

	path = string_value(ARG(0));
	x = int_value(ARG(1), -1);
	y = int_value(ARG(2), -1);
	name = string_value(ARG(3));
	shortcut = string_value(ARG(4));
	xargs = string_value(ARG(5));
	locked = bool_value(ARG(6));
	update = bool_value(ARG(7));

	pinboard_pin_with_args(path, name, x, y, shortcut, xargs, 
						(locked==-1) ? FALSE : locked, 
						(update==-1) ? FALSE : update);

	g_free(path);
	g_free(name);
	g_free(shortcut);
	g_free(xargs);

	return NULL;
}

/* args = Path, [Label] */
static xmlNodePtr rpc_PinboardRemove(GList *args)
{
	char *path = NULL;
	gchar *name;

	path = string_value(ARG(0));
	name = string_value(ARG(1));
	
	pinboard_remove(path, name);

	g_free(path);
	if(name)
	        g_free(name);

	return NULL;
}

/* args = Side, [Name] */
static xmlNodePtr rpc_Panel(GList *args)
{
	PanelSide side;
	char *name, *side_name;

	side_name = string_value(ARG(0));
	if (side_name)
	{
		side = panel_name_to_side(side_name);
		g_free(side_name);
		g_return_val_if_fail(side != PANEL_NUMBER_OF_SIDES, NULL);
	}
	else
		side = PANEL_DEFAULT_SIDE;

	name = string_value(ARG(1));

	panel_new(name, side);

	g_free(name);

	return NULL;
}

/* args = Side, Path, [Label, After, Shortcut, Args, Locked] */
static xmlNodePtr rpc_PanelAdd(GList *args)
{
	PanelSide side;
	char *path, *side_name, *label, *shortcut, *arg;
	gboolean after = FALSE;
	gboolean locked = FALSE;
	int tmp;

	side_name = string_value(ARG(0));
	side = panel_name_to_side(side_name);
	g_free(side_name);
	g_return_val_if_fail(side != PANEL_NUMBER_OF_SIDES, NULL);

	path = string_value(ARG(1));
	label = string_value(ARG(2));

	tmp = bool_value(ARG(3));
	after = (tmp == -1) ? FALSE : tmp;

	shortcut = string_value(ARG(4));
	arg = string_value(ARG(5));
	locked = bool_value(ARG(6));

	panel_add(side, path, label, after, shortcut, arg, (locked== -1) ? FALSE : locked);

	g_free(path);
	g_free(label);
	g_free(shortcut);
	g_free(arg);

	return NULL;
}

static xmlNodePtr rpc_PanelRemove(GList *args)
{
	PanelSide side;
	char *path, *side_name, *label;

	side_name = string_value(ARG(0));
	side = panel_name_to_side(side_name);
	g_free(side_name);
	g_return_val_if_fail(side != PANEL_NUMBER_OF_SIDES, NULL);

	path = string_value(ARG(1));
	label = string_value(ARG(2));

	if (path || label)
		panel_remove_item(side, path, label);
	else
		g_warning("Must specify either path or label");

	g_free(path);
	g_free(label);

	return NULL;
}

static xmlNodePtr rpc_Copy(GList *args)
{
	GList *from;
	char *to;
	char *leaf;
	int quiet;

	from = list_value(ARG(0));
	to = string_value(ARG(1));
	leaf = string_value(ARG(2));
	quiet = bool_value(ARG(3));

	if (from)
		action_copy(from, to, leaf, quiet);
	else
		g_warning("No files in SOAP request list");

	destroy_glist(&from);
	g_free(to);
	g_free(leaf);

	return NULL;
}

static xmlNodePtr rpc_Move(GList *args)
{
	GList *from;
	char *to;
	char *leaf;
	int quiet;

	from = list_value(ARG(0));
	to = string_value(ARG(1));
	leaf = string_value(ARG(2));
	quiet = bool_value(ARG(3));

	if (from)
		action_move(from, to, leaf, quiet);
	else
		g_warning("No files in SOAP request list");

	destroy_glist(&from);
	g_free(to);
	g_free(leaf);

	return NULL;
}

static xmlNodePtr rpc_Link(GList *args)
{
	GList *from;
	char *to;
	char *leaf;

	from = list_value(ARG(0));
	to = string_value(ARG(1));
	leaf = string_value(ARG(2));

	if (from)
		action_link(from, to, leaf, TRUE);
	else
		g_warning("No files in SOAP request list");

	destroy_glist(&from);
	g_free(to);
	g_free(leaf);

	return NULL;
}

static xmlNodePtr rpc_FileType(GList *args)
{
	MIME_type *type;
	char *path, *tname;
	xmlNodePtr reply;

	path = string_value(ARG(0));
	type = type_get_type(path);
	g_free(path);
	
	reply = xmlNewNode(NULL, "rox:FileTypeResponse");
	tname = g_strconcat(type->media_type, "/", type->subtype, NULL);

	xmlNewNs(reply, SOAP_RPC_NS, "soap");
	xmlNewTextChild(reply, NULL, "soap:result", tname);
	g_free(tname);

	return reply;
}

static xmlNodePtr rpc_Mount(GList *args)
{
	GList *paths;
	int	open_dir, quiet;

	paths = list_value(ARG(0));
	open_dir = bool_value(ARG(1));
	quiet = bool_value(ARG(2));

	if (open_dir == -1)
		open_dir = TRUE;

	if (paths)
		action_mount(paths, open_dir, TRUE, quiet);

	destroy_glist(&paths);

	return NULL;
}

static xmlNodePtr rpc_Unmount(GList *args)
{
	GList *paths;
	int	quiet;

	paths = list_value(ARG(0));
	quiet = bool_value(ARG(1));

	if (paths)
		action_mount(paths, FALSE, FALSE, quiet);

	destroy_glist(&paths);

	return NULL;
}

/* The first time the property changes, do nothing (it's us setting the
 * property). The second time, get the result.
 * Don't call this function three times!
 */
static void soap_done(GtkWidget *widget, GdkEventProperty *event, gpointer data)
{
	static int times_called = 0;
	GdkAtom	prop = (GdkAtom) data;

	if (prop != event->atom)
		return;

	times_called++;
	g_return_if_fail(times_called < 3);

	if (times_called == 1)
		return;

	/* If we got a reply, display it here */
	if (event->state == GDK_PROPERTY_NEW_VALUE)
	{
		gint length;

		data = read_property(event->window, event->atom, &length);

		if (data)
			puts(data);
	}

	gtk_main_quit();
}

static gboolean too_slow(gpointer data)
{
	g_warning("Existing ROX-Filer process is not responding! Try with -n");
	gtk_main_quit();

	return 0;
}

/* Send the SOAP message in property 'prop' on 'from' to 'dest' */
static void soap_send(GtkWidget *from, GdkAtom prop, GdkWindow *dest)
{
	GdkEventClient event;

	event.data.l[0] = GDK_WINDOW_XWINDOW(from->window);
	event.data.l[1] = gdk_x11_atom_to_xatom(prop);
	event.data_format = 32;
	event.message_type = xsoap;
	
	gdk_event_send_client_message((GdkEvent *) &event,
				      GDK_WINDOW_XWINDOW(dest));

	g_timeout_add(10000, too_slow, NULL);
			
	gtk_main();
}

/* Lookup this method in rpc_calls and invoke it.
 * Returns the SOAP reply or fault, or NULL if this method
 * doesn't return anything.
 */
static xmlNodePtr soap_invoke(xmlNode *method)
{
	GList *args = NULL;
	SOAP_call *call;
	gchar **arg;
	xmlNodePtr retval = NULL;
	GHashTable *name_to_node;
	xmlNode	*node;

	call = g_hash_table_lookup(rpc_calls, method->name);
	if (!call)
	{
		xmlNodePtr reply;
		gchar *err;

		err = g_strdup_printf(_("Attempt to invoke unknown SOAP "
					"method '%s'"), method->name);
		reply = xmlNewNode(NULL, "env:Fault");
		xmlNewNs(reply, SOAP_RPC_NS, "rpc");
		xmlNewNs(reply, SOAP_ENV_NS, "env");
		xmlNewTextChild(reply, NULL, "faultcode",
						"rpc:ProcedureNotPresent");
		xmlNewTextChild(reply, NULL, "faultstring", err);
		g_free(err);
		return reply;
	}

	name_to_node = g_hash_table_new(g_str_hash, g_str_equal);
	for (node = method->xmlChildrenNode; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE)
			continue;

		if (node->ns == NULL || strcmp(node->ns->href, ROX_NS) != 0)
			continue;

		g_hash_table_insert(name_to_node, (gchar *) node->name, node);
	}

	if (call->required_args)
	{
		for (arg = call->required_args; *arg; arg++)
		{
			node = g_hash_table_lookup(name_to_node, *arg);
			if (!node)
			{
				g_warning("Missing required argument '%s' "
					"in call to method '%s'", *arg,
					method->name);
				goto out;
			}

			args = g_list_append(args, node);
		}
	}

	if (call->optional_args)
	{
		for (arg = call->optional_args; *arg; arg++)
			args = g_list_append(args,
				g_hash_table_lookup(name_to_node, *arg));
	}

	retval = call->func(args);

out:
	g_hash_table_destroy(name_to_node);
	g_list_free(args);

	return retval;
}
