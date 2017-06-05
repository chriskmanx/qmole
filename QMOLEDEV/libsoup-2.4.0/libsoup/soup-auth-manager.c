/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-auth-manager.c: SoupAuth manager for SoupSession
 *
 * Copyright (C) 2007 Red Hat, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>

#include "soup-auth-manager.h"
#include "soup-headers.h"
#include "soup-message-private.h"
#include "soup-path-map.h"
#include "soup-session.h"
#include "soup-session-private.h"
#include "soup-uri.h"

static void session_request_started (SoupSession *session, SoupMessage *msg,
				     SoupSocket *socket, gpointer data);

struct SoupAuthManager {
	SoupSession *session;
	GPtrArray *auth_types;

	SoupAuth *proxy_auth;
	GHashTable *auth_hosts;
};

typedef struct {
	SoupURI     *root_uri;
	SoupPathMap *auth_realms;      /* path -> scheme:realm */
	GHashTable  *auths;            /* scheme:realm -> SoupAuth */
} SoupAuthHost;

/* temporary until we fix this to index hosts by SoupAddress */
extern guint     soup_uri_host_hash  (gconstpointer  key);
extern gboolean  soup_uri_host_equal (gconstpointer  v1,
				      gconstpointer  v2);
extern SoupURI  *soup_uri_copy_root  (SoupURI *uri);

SoupAuthManager *
soup_auth_manager_new (SoupSession *session)
{
	SoupAuthManager *manager;

	manager = g_slice_new0 (SoupAuthManager);
	manager->session = session;
	manager->auth_types = g_ptr_array_new ();
	manager->auth_hosts = g_hash_table_new (soup_uri_host_hash,
						soup_uri_host_equal);

	g_signal_connect (session, "request_started",
			  G_CALLBACK (session_request_started), manager);
	return manager;
}

static gboolean
foreach_free_host (gpointer key, gpointer value, gpointer data)
{
	SoupAuthHost *host = value;

	if (host->auth_realms)
		soup_path_map_free (host->auth_realms);
	if (host->auths)
		g_hash_table_destroy (host->auths);

	soup_uri_free (host->root_uri);
	g_slice_free (SoupAuthHost, host);

	return TRUE;
}

void
soup_auth_manager_free (SoupAuthManager *manager)
{
	int i;

	g_signal_handlers_disconnect_by_func (
		manager->session,
		G_CALLBACK (session_request_started), manager);

	for (i = 0; i < manager->auth_types->len; i++)
		g_type_class_unref (manager->auth_types->pdata[i]);
	g_ptr_array_free (manager->auth_types, TRUE);

	g_hash_table_foreach_remove (manager->auth_hosts, foreach_free_host, NULL);
	g_hash_table_destroy (manager->auth_hosts);

	if (manager->proxy_auth)
		g_object_unref (manager->proxy_auth);

	g_slice_free (SoupAuthManager, manager);
}

static int
auth_type_compare_func (gconstpointer a, gconstpointer b)
{
	SoupAuthClass **auth1 = (SoupAuthClass **)a;
	SoupAuthClass **auth2 = (SoupAuthClass **)b;

	return (*auth2)->strength - (*auth1)->strength;
}

void
soup_auth_manager_add_type (SoupAuthManager *manager, GType type)
{
	SoupAuthClass *auth_class;

	g_return_if_fail (g_type_is_a (type, SOUP_TYPE_AUTH));

	auth_class = g_type_class_ref (type);
	g_ptr_array_add (manager->auth_types, auth_class);
	g_ptr_array_sort (manager->auth_types, auth_type_compare_func);
}

void
soup_auth_manager_remove_type (SoupAuthManager *manager, GType type)
{
	SoupAuthClass *auth_class;
	int i;

	g_return_if_fail (g_type_is_a (type, SOUP_TYPE_AUTH));

	auth_class = g_type_class_peek (type);
	for (i = 0; i < manager->auth_types->len; i++) {
		if (manager->auth_types->pdata[i] == (gpointer)auth_class) {
			g_ptr_array_remove_index (manager->auth_types, i);
			g_type_class_unref (auth_class);
			return;
		}
	}
}

static inline const char *
auth_header_for_message (SoupMessage *msg)
{
	if (msg->status_code == SOUP_STATUS_PROXY_UNAUTHORIZED) {
		return soup_message_headers_get (msg->response_headers,
						 "Proxy-Authenticate");
	} else {
		return soup_message_headers_get (msg->response_headers,
						 "WWW-Authenticate");
	}
}

static char *
extract_challenge (const char *challenges, const char *scheme)
{
	GSList *items, *i;
	int schemelen = strlen (scheme);
	char *item, *space, *equals;
	GString *challenge;

	/* The relevant grammar:
	 *
	 * WWW-Authenticate   = 1#challenge
	 * Proxy-Authenticate = 1#challenge
	 * challenge          = auth-scheme 1#auth-param
	 * auth-scheme        = token
	 * auth-param         = token "=" ( token | quoted-string )
	 *
	 * The fact that quoted-strings can contain commas, equals
	 * signs, and auth scheme names makes it tricky to "cheat" on
	 * the parsing. We just use soup_header_parse_list(), and then
	 * reassemble the pieces after we find the one we want.
	 */

	items = soup_header_parse_list (challenges);

	/* First item will start with the scheme name, followed by a
	 * space and then the first auth-param.
	 */
	for (i = items; i; i = i->next) {
		item = i->data;
		if (!g_ascii_strncasecmp (item, scheme, schemelen) &&
		    g_ascii_isspace (item[schemelen]))
			break;
	}
	if (!i) {
		soup_header_free_list (items);
		return NULL;
	}

	/* The challenge extends from this item until the end, or until
	 * the next item that has a space before an equals sign.
	 */
	challenge = g_string_new (item);
	for (i = i->next; i; i = i->next) {
		item = i->data;
		space = strpbrk (item, " \t");
		equals = strchr (item, '=');
		if (!equals || (space && equals > space))
			break;

		g_string_append (challenge, ", ");
		g_string_append (challenge, item);
	}

	soup_header_free_list (items);
	return g_string_free (challenge, FALSE);
}

static SoupAuth *
create_auth (SoupAuthManager *manager, SoupMessage *msg)
{
	const char *header;
	SoupAuthClass *auth_class;
	char *challenge = NULL;
	SoupAuth *auth;
	int i;

	header = auth_header_for_message (msg);
	if (!header)
		return NULL;

	for (i = manager->auth_types->len - 1; i >= 0; i--) {
		auth_class = manager->auth_types->pdata[i];
		challenge = extract_challenge (header, auth_class->scheme_name);
		if (challenge)
			break;
	}
	if (!challenge)
		return NULL;

	auth = soup_auth_new (G_TYPE_FROM_CLASS (auth_class), msg, challenge);
	g_free (challenge);
	return auth;
}

static gboolean
check_auth (SoupAuthManager *manager, SoupMessage *msg, SoupAuth *auth)
{
	const char *header;
	char *challenge;
	gboolean ok;

	header = auth_header_for_message (msg);
	if (!header)
		return FALSE;

	challenge = extract_challenge (header, soup_auth_get_scheme_name (auth));
	if (!challenge)
		return FALSE;

	ok = soup_auth_update (auth, msg, challenge);
	g_free (challenge);
	return ok;
}

static SoupAuthHost *
get_auth_host_for_message (SoupAuthManager *manager, SoupMessage *msg)
{
	SoupAuthHost *host;
	SoupURI *source = soup_message_get_uri (msg);

	host = g_hash_table_lookup (manager->auth_hosts, source);
	if (host)
		return host;

	host = g_slice_new0 (SoupAuthHost);
	host->root_uri = soup_uri_copy_root (source);
	g_hash_table_insert (manager->auth_hosts, host->root_uri, host);

	return host;
}

static SoupAuth *
lookup_auth (SoupAuthManager *manager, SoupMessage *msg)
{
	SoupAuthHost *host;
	const char *path, *realm;

	host = get_auth_host_for_message (manager, msg);
	if (!host->auth_realms)
		return NULL;

	path = soup_message_get_uri (msg)->path;
	if (!path)
		path = "/";
	realm = soup_path_map_lookup (host->auth_realms, path);
	if (realm)
		return g_hash_table_lookup (host->auths, realm);
	else
		return NULL;
}

static gboolean
authenticate_auth (SoupAuthManager *manager, SoupAuth *auth,
		   SoupMessage *msg, gboolean prior_auth_failed,
		   gboolean proxy)
{
	SoupURI *uri;

	if (soup_auth_is_authenticated (auth))
		return TRUE;

	if (proxy) {
		g_object_get (G_OBJECT (manager->session),
			      SOUP_SESSION_PROXY_URI, &uri,
			      NULL);
	} else
		uri = soup_uri_copy (soup_message_get_uri (msg));

	if (uri->password && !prior_auth_failed) {
		soup_auth_authenticate (auth, uri->user, uri->password);
		soup_uri_free (uri);
		return TRUE;
	}
	soup_uri_free (uri);

	soup_session_emit_authenticate (manager->session,
					msg, auth, prior_auth_failed);
	return soup_auth_is_authenticated (auth);
}

static void
update_auth (SoupMessage *msg, gpointer user_data)
{
	SoupAuthManager *manager = user_data;
	SoupAuthHost *host;
	SoupAuth *auth, *prior_auth, *old_auth;
	const char *path;
	char *auth_info, *old_auth_info;
	GSList *pspace, *p;
	gboolean prior_auth_failed = FALSE;

	host = get_auth_host_for_message (manager, msg);

	/* See if we used auth last time */
	prior_auth = soup_message_get_auth (msg);
	if (prior_auth && check_auth (manager, msg, prior_auth)) {
		auth = prior_auth;
		if (!soup_auth_is_authenticated (auth))
			prior_auth_failed = TRUE;
	} else {
		auth = create_auth (manager, msg);
		if (!auth)
			return;
	}
	auth_info = soup_auth_get_info (auth);

	if (!host->auth_realms) {
		host->auth_realms = soup_path_map_new (g_free);
		host->auths = g_hash_table_new_full (g_str_hash, g_str_equal,
						     g_free, g_object_unref);
	}

	/* Record where this auth realm is used. */
	pspace = soup_auth_get_protection_space (auth, soup_message_get_uri (msg));
	for (p = pspace; p; p = p->next) {
		path = p->data;
		old_auth_info = soup_path_map_lookup (host->auth_realms, path);
		if (old_auth_info) {
			if (!strcmp (old_auth_info, auth_info))
				continue;
			soup_path_map_remove (host->auth_realms, path);
		}

		soup_path_map_add (host->auth_realms, path,
				   g_strdup (auth_info));
	}
	soup_auth_free_protection_space (auth, pspace);

	/* Now, make sure the auth is recorded. (If there's a
	 * pre-existing auth, we keep that rather than the new one,
	 * since the old one might already be authenticated.)
	 */
	old_auth = g_hash_table_lookup (host->auths, auth_info);
	if (old_auth) {
		g_free (auth_info);
		if (auth != old_auth && auth != prior_auth) {
			g_object_unref (auth);
			auth = old_auth;
		}
	} else {
		g_hash_table_insert (host->auths, auth_info, auth);
	}

	/* If we need to authenticate, try to do it. */
	authenticate_auth (manager, auth, msg,
			   prior_auth_failed, FALSE);
}

static void
requeue_if_authenticated (SoupMessage *msg, gpointer user_data)
{
	SoupAuthManager *manager = user_data;
	SoupAuth *auth = lookup_auth (manager, msg);

	if (auth && soup_auth_is_authenticated (auth))
		soup_session_requeue_message (manager->session, msg);
}

static void
update_proxy_auth (SoupMessage *msg, gpointer user_data)
{
	SoupAuthManager *manager = user_data;
	SoupAuth *prior_auth;
	gboolean prior_auth_failed = FALSE;

	/* See if we used auth last time */
	prior_auth = soup_message_get_proxy_auth (msg);
	if (prior_auth && check_auth (manager, msg, prior_auth)) {
		if (!soup_auth_is_authenticated (prior_auth))
			prior_auth_failed = TRUE;
	}

	if (!manager->proxy_auth) {
		manager->proxy_auth = create_auth (manager, msg);
		if (!manager->proxy_auth)
			return;
	}

	/* If we need to authenticate, try to do it. */
	authenticate_auth (manager, manager->proxy_auth, msg,
			   prior_auth_failed, TRUE);
}

static void
requeue_if_proxy_authenticated (SoupMessage *msg, gpointer user_data)
{
	SoupAuthManager *manager = user_data;
	SoupAuth *auth = manager->proxy_auth;

	if (auth && soup_auth_is_authenticated (auth))
		soup_session_requeue_message (manager->session, msg);
}

static void
session_request_started (SoupSession *session, SoupMessage *msg,
			 SoupSocket *socket, gpointer data)
{
	SoupAuthManager *manager = data;
	SoupAuth *auth;

	auth = lookup_auth (manager, msg);
	if (!auth || !authenticate_auth (manager, auth, msg, FALSE, FALSE))
		auth = NULL;
	soup_message_set_auth (msg, auth);
	soup_message_add_status_code_handler (
		msg, "got_headers", SOUP_STATUS_UNAUTHORIZED,
		G_CALLBACK (update_auth), manager);
	soup_message_add_status_code_handler (
		msg, "got_body", SOUP_STATUS_UNAUTHORIZED,
		G_CALLBACK (requeue_if_authenticated), manager);

	auth = manager->proxy_auth;
	if (!auth || !authenticate_auth (manager, auth, msg, FALSE, TRUE))
		auth = NULL;
	soup_message_set_proxy_auth (msg, auth);
	soup_message_add_status_code_handler (
		msg, "got_headers", SOUP_STATUS_PROXY_UNAUTHORIZED,
		G_CALLBACK (update_proxy_auth), manager);
	soup_message_add_status_code_handler (
		msg, "got_body", SOUP_STATUS_PROXY_UNAUTHORIZED,
		G_CALLBACK (requeue_if_proxy_authenticated), manager);
}
