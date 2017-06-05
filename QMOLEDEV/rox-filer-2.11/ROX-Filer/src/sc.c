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

/* sc.c - XSMP client support */

#include <stdlib.h>
#include <fcntl.h> 
#include <gtk/gtk.h>
#include <X11/Xlib.h>
#include <X11/SM/SMlib.h>
#include <pwd.h>
#include <string.h>

#include "sc.h"

static SmPropValue *make_list_of_array_value(const gchar *vals[], gint nvals)
{
	SmPropValue *values = g_new(SmPropValue, nvals);
	gint i;
	
	for(i = 0; i < nvals; i++)
	{
		values[i].value = g_strdup(vals[i]);
		values[i].length = strlen(vals[i]);
	}
	return values;
}

static SmPropValue *make_array_value(const gchar *val)
{
	SmPropValue *value = g_new(SmPropValue, 1);
	
	value->value = g_strdup(val);
	value->length = strlen(val);

	return value;
}

static SmPropValue *make_card_value(gchar val)
{
	SmPropValue *value = g_new(SmPropValue, 1);
	
	value->value = g_memdup(&val, sizeof(gchar));
	value->length = 1;
	
	return value;
}

static SmProperty *find_property(SmClient *client, const gchar *name)
{
	GSList *list = client->props;
	SmProperty *prop;

	for (; list; list = list->next)
	{
		prop = (SmProperty *)list->data;
		if (strcmp(prop->prop.name, name) == 0)
			return prop;
	}
	return NULL;		
}

static SmProperty *new_property(SmClient *client,
				const gchar *name, const gchar *type)
{
	SmProperty *prop;

	prop = g_new(SmProperty, 1);
	prop->prop.name = g_strdup(name);
	prop->prop.type = g_strdup(type);
	prop->prop.vals = NULL;
	prop->prop.num_vals = 0;
#ifdef DEBUG
	g_printerr("appending prop %s\n", prop->prop.name);
#endif
	client->props = g_slist_append(client->props, prop);

	return prop;
}

static gint close_connection(gpointer data)
{
	SmClient *client = (SmClient *)data;
	
	SmcCloseConnection(client->conn, 0, NULL);
	
	return 0;
}

/* Called when there's data to be read on the ICE file descriptor.
   Unpacks the message and triggers the correct callback... I think */

static void poll_ice_messages(gpointer data, gint source,
				GdkInputCondition condition)
{
	SmClient *client = (SmClient *)data;
	Bool ret;

	if (IceProcessMessages(client->ice_conn, NULL, &ret) ==
			IceProcessMessagesIOError)
		SmcCloseConnection(client->conn, 0, NULL);
	return;
}

/* Called whenever an ICE connection is opened or closed */

static void ice_watch_fn(IceConn conn, IcePointer client_data,
                			Bool opening, IcePointer *watch_data)
{
	SmClient *client = (SmClient *)client_data;
	
	if(opening)
	{
#ifdef DEBUG
		g_printerr("Ice connection opened for id %s\n", client->id);
#endif
       		client->fd = IceConnectionNumber(conn);
       		fcntl(client->fd, F_SETFD, FD_CLOEXEC);
		client->input_tag = gdk_input_add(client->fd, GDK_INPUT_READ, &poll_ice_messages, client);
	}
	else
	{
		if (IceConnectionNumber(conn) == client->fd)
		{
#ifdef DEBUG
			g_printerr("Ice connection closed for id %s\n", client->id);
#endif
        		gdk_input_remove(client->input_tag);
			sc_destroy(client);
		}
	}
}

/* Callbacks for different SM messages */

static void sc_save_yourself(SmcConn conn, SmPointer client_data, int save_type,
                      		Bool shutdown, int interact_style, Bool fast)
{
	SmClient *client = (SmClient *)client_data;
	gboolean success = TRUE;
#ifdef DEBUG	
	g_printerr("%s saving state\n", client->id);
#endif
	if(client->save_yourself_fn)
		success = client->save_yourself_fn(client);
	if(success)
		sc_register_properties(client);
	SmcSaveYourselfDone(client->conn, success);
}

static void sc_shutdown_cancelled(SmcConn conn, SmPointer client_data)
{
	SmClient *client = (SmClient *)client_data;
#ifdef DEBUG	
	g_printerr("%s shutdown cancelled\n", client->id);
#endif
	if(client->shutdown_cancelled_fn)
		client->shutdown_cancelled_fn(client);
}

static void sc_save_complete(SmcConn conn, SmPointer client_data)
{
	SmClient *client = (SmClient *)client_data;
#ifdef DEBUG	
	g_printerr("%s save complete\n", client->id);
#endif
	if(client->save_complete_fn)
		client->save_complete_fn(client);
}

static void sc_die(SmcConn conn, SmPointer client_data)
{
	SmClient *client = (SmClient *)client_data;
#ifdef DEBUG	
	g_printerr("%s dying\n", client->id);
#endif
	if(client->die_fn)
		client->die_fn(client);
}

gboolean sc_session_up()
{
	if(getenv("SESSION_MANAGER") == NULL)
		return FALSE;
	return TRUE;
}

SmClient *sc_new(const gchar *client_id)
{
	SmClient *client;
	
#ifdef DEBUG
	g_printerr("Creating new sm-client\n");
#endif	
	client = g_new(SmClient, 1);
	client->id = g_strdup(client_id);
	client->props = NULL;
	client->conn = NULL;
	client->ice_conn = NULL;
	client->fd = -1;
	
	return client;
}

void sc_destroy(SmClient *client)
{
	GSList *list = client->props;
	SmProperty *prop;
	
#ifdef DEBUG
	g_printerr("destroying client\n");
#endif	
	for (; list; list = list->next)
	{
		prop = (SmProperty *)list->data;
		g_free(prop->prop.vals->value);
		g_free(prop->prop.vals);
		g_free(prop->prop.name);
		g_free(prop->prop.type);
		g_free(prop);
	}
	g_slist_free(client->props);
	g_free(client->id);
	g_free(client);
}

/* Set a property with a SmLISTofARRAY8 value as in SmRestartCommand,
   SmCloneCommand and SmDiscardCommand */ 

void sc_set_list_of_array_prop(SmClient *client,
			const gchar *name, 
			const char *vals[], gint nvals)
{
	SmProperty *prop = find_property(client, name);
	SmPropValue *value = make_list_of_array_value(vals, nvals);
		
	if(prop == NULL)
		prop = new_property(client, name, SmLISTofARRAY8);
	
	else
		g_free(prop->prop.vals);
	
	prop->prop.vals = value;
	prop->prop.num_vals = nvals;
	prop->set = TRUE;
}

/* Set a prop with a SmARRAY8 value, SmProgram, SmUserID and
   SmDiscardCommand (if using XSM) are suitable victims. */

void sc_set_array_prop(SmClient *client, const gchar *name, const gchar *vals)
{
	SmProperty *prop = find_property(client, name);
	SmPropValue *value = make_array_value(vals);
		
	if(prop == NULL)
		prop = new_property(client, name, SmARRAY8);
	
	else
		g_free(prop->prop.vals);
	
	prop->prop.vals = value;
	prop->prop.num_vals = 1;
	prop->set = TRUE;
}

/* This one is for the SmRestarStyleHint */

void sc_set_card_prop(SmClient *client, const gchar *name, const gchar val)
{
	SmProperty *prop = find_property(client, name);
	SmPropValue *value = make_card_value(val);
		
	if(prop == NULL)
		prop = new_property(client, name, SmCARD8);
	
	else
		g_free(prop->prop.vals);
	
	prop->prop.vals = value;
	prop->prop.num_vals = 1;
	prop->set = TRUE;
}

/* Puts a pointer to a SmPropValue in val_ret and 
  the number of values in nvals_ret (they are in an array ).
  It looks like this:
  typedef struct {
  	int length;
	SmPointer value;
  } SmPropValue;
  The values are those stored in the SmClient struct,
  They're not fetched from the session manager */

void sc_get_prop_value(SmClient *client, const gchar *name,
			SmPropValue **val_ret, gint *nvals_ret)
{
	SmProperty *prop = find_property(client, name);
	
	if(!prop)
	{
		*val_ret = NULL;
		*nvals_ret = 0;
	}
	else
	{
		*val_ret = prop->prop.vals;
		*nvals_ret = prop->prop.num_vals;
	}
}

/* Stores set properties in the session manager */
	
void sc_register_properties(SmClient *client)
{
	GPtrArray *set_props= g_ptr_array_new();
	GSList *list = client->props;
	SmProperty *prop;
#ifdef DEBUG
	gint i;
#endif
	
	for (; list; list = list->next)
	{
		prop = (SmProperty *)list->data;
		if(prop->set == TRUE)
		{
			g_ptr_array_add(set_props, &prop->prop);
			prop->set = FALSE;
		}
	}
#ifdef DEBUG
	g_printerr("Registering props:\n");
	for(i = 0; i < set_props->len; i++)
	{
		prop = g_ptr_array_index(set_props, i);
		g_printerr("%s\n", prop->prop.name);
	}
#endif
	if(set_props->len > 0)	
		SmcSetProperties(client->conn, set_props->len, (SmProp **)set_props->pdata);
	
	g_ptr_array_free(set_props, TRUE);
}

/* Connects to the session manager */

gboolean sc_connect(SmClient *client)
{
	gchar error_str[256];
	gchar *client_id_ret = NULL;
	SmcConn conn = NULL;
	SmcCallbacks callbacks;

	callbacks.save_yourself.callback = &sc_save_yourself;
	callbacks.save_yourself.client_data = (SmPointer)client;
	callbacks.die.callback = &sc_die;
	callbacks.die.client_data = (SmPointer)client;
	callbacks.save_complete.callback = &sc_save_complete;
	callbacks.save_complete.client_data = (SmPointer)client;
	callbacks.shutdown_cancelled.callback = &sc_shutdown_cancelled;
	callbacks.shutdown_cancelled.client_data = (SmPointer)client;
	
	if(IceAddConnectionWatch(&ice_watch_fn, client) == 0)
	{
        	g_printerr("Session Manager: IceAddConnectionWatch failed\n");
        	return FALSE;
	}

	if((conn = SmcOpenConnection(NULL, /* network ids */
                                    NULL, /* context */
                                    1, 0, /* protocol major, minor */
                                    SmcSaveYourselfProcMask |
                                    SmcSaveCompleteProcMask |
                                    SmcShutdownCancelledProcMask |
                                    SmcDieProcMask,
                                    &callbacks,
                                    client->id, &client_id_ret,
                                    sizeof(error_str), error_str)) == NULL)
	{
        	g_printerr("Session Manager: Init error\n");
		return FALSE;
	}
		
	client->id = g_strdup(client_id_ret);
	client->conn = conn;
	client->ice_conn = SmcGetIceConnection(conn);
	gdk_set_sm_client_id(client->id);
	XFree(client_id_ret);
	
	gtk_quit_add(0, &close_connection, client);
	
	return TRUE;
}
	
