/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * soup-session-feature.c: Miscellaneous session feature-provider interface
 *
 * Copyright (C) 2008 Red Hat, Inc.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "soup-session-feature.h"
#include "soup-message-private.h"

/**
 * SECTION:soup-session-feature
 * @short_description: Interface for miscellaneous session features
 *
 * #SoupSessionFeature is the interface used by classes that extend
 * the functionality of a #SoupSession. Some features like HTTP
 * authentication handling are implemented internally via
 * #SoupSessionFeature<!-- -->s. Other features can be added to the session
 * by the application. (Eg, #SoupLogger, #SoupCookieJar.)
 *
 * See soup_session_add_feature(), etc, to add a feature to a session.
 **/

/**
 * SoupSessionFeature:
 *
 * An object that implement some sort of optional feature for
 * #SoupSession.
 *
 * Since: 2.24
 **/

/**
 * SoupSessionFeatureInterface:
 * @parent: The parent interface.
 * @attach: Perform setup when a feature is added to a session
 * @detach: Perform cleanup when a feature is removed from a session
 * @request_queued: Proxies the session's #SoupSession::request_queued signal
 * @request_started: Proxies the session's #SoupSession::request_started signal
 * @request_unqueued: Proxies the session's #SoupSession::request_unqueued signal
 *
 * The interface implemented by #SoupSessionFeature<!-- -->s.
 *
 * Since: 2.24
 **/

static void soup_session_feature_interface_init (SoupSessionFeatureInterface *interface);

static void attach (SoupSessionFeature *feature, SoupSession *session);
static void detach (SoupSessionFeature *feature, SoupSession *session);

GType
soup_session_feature_get_type (void)
{
  static volatile gsize g_define_type_id__volatile = 0;
  if (g_once_init_enter (&g_define_type_id__volatile))
    {
      GType g_define_type_id =
        g_type_register_static_simple (G_TYPE_INTERFACE,
                                       g_intern_static_string ("SoupSessionFeature"),
                                       sizeof (SoupSessionFeatureInterface),
                                       (GClassInitFunc)soup_session_feature_interface_init,
                                       0,
                                       (GInstanceInitFunc)NULL,
                                       (GTypeFlags) 0);
      g_type_interface_add_prerequisite (g_define_type_id, G_TYPE_OBJECT);
      g_once_init_leave (&g_define_type_id__volatile, g_define_type_id);
    }
  return g_define_type_id__volatile;
}

static void
soup_session_feature_interface_init (SoupSessionFeatureInterface *interface)
{
	interface->attach = attach;
	interface->detach = detach;
}

static void
weak_notify_unref (gpointer feature, GObject *ex_object)
{
	g_object_unref (feature);
}

static void
request_queued (SoupSession *session, SoupMessage *msg, gpointer feature)
{
	if (soup_message_disables_feature (msg, feature))
		return;

	SOUP_SESSION_FEATURE_GET_CLASS (feature)->
		request_queued (feature, session, msg);
}

static void
request_started (SoupSession *session, SoupMessage *msg,
		 SoupSocket *socket, gpointer feature)
{
	if (soup_message_disables_feature (msg, feature))
		return;

	SOUP_SESSION_FEATURE_GET_CLASS (feature)->
		request_started (feature, session, msg, socket);
}

static void
request_unqueued (SoupSession *session, SoupMessage *msg, gpointer feature)
{
	if (soup_message_disables_feature (msg, feature))
		return;

	SOUP_SESSION_FEATURE_GET_CLASS (feature)->
		request_unqueued (feature, session, msg);
}

static void
attach (SoupSessionFeature *feature, SoupSession *session)
{
	g_object_weak_ref (G_OBJECT (session),
			   weak_notify_unref, g_object_ref (feature));

	if (SOUP_SESSION_FEATURE_GET_CLASS (feature)->request_queued) {
		g_signal_connect (session, "request_queued",
				  G_CALLBACK (request_queued), feature);
	}

	if (SOUP_SESSION_FEATURE_GET_CLASS (feature)->request_started) {
		g_signal_connect (session, "request_started",
				  G_CALLBACK (request_started), feature);
	}

	if (SOUP_SESSION_FEATURE_GET_CLASS (feature)->request_unqueued) {
		g_signal_connect (session, "request_unqueued",
				  G_CALLBACK (request_unqueued), feature);
	}
}

void
soup_session_feature_attach (SoupSessionFeature *feature,
			     SoupSession        *session)
{
	SOUP_SESSION_FEATURE_GET_CLASS (feature)->attach (feature, session);
}

static void
detach (SoupSessionFeature *feature, SoupSession *session)
{
	g_object_weak_unref (G_OBJECT (session), weak_notify_unref, feature);

	g_signal_handlers_disconnect_by_func (session, request_queued, feature);
	g_signal_handlers_disconnect_by_func (session, request_started, feature);
	g_signal_handlers_disconnect_by_func (session, request_unqueued, feature);

	g_object_unref (feature);
}

void
soup_session_feature_detach (SoupSessionFeature *feature,
			     SoupSession        *session)
{
	SOUP_SESSION_FEATURE_GET_CLASS (feature)->detach (feature, session);
}

gboolean
soup_session_feature_add_feature (SoupSessionFeature *feature,
				  GType               type)
{
	SoupSessionFeatureInterface *feature_iface =
              SOUP_SESSION_FEATURE_GET_CLASS (feature);

	if (feature_iface->add_feature)
		return feature_iface->add_feature (feature, type);
	else
		return FALSE;
}

gboolean
soup_session_feature_remove_feature (SoupSessionFeature *feature,
				     GType               type)
{
	SoupSessionFeatureInterface *feature_iface =
              SOUP_SESSION_FEATURE_GET_CLASS (feature);

	if (feature_iface->remove_feature)
		return feature_iface->remove_feature (feature, type);
	else
		return FALSE;
}

gboolean
soup_session_feature_has_feature (SoupSessionFeature *feature,
				  GType               type)
{
	SoupSessionFeatureInterface *feature_iface =
              SOUP_SESSION_FEATURE_GET_CLASS (feature);

	if (feature_iface->has_feature)
		return feature_iface->has_feature (feature, type);
	else
		return FALSE;
}
