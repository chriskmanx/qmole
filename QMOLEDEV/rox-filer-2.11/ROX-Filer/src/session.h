/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

/* session.h - XSMP client support */
#ifndef _SESSION_H_
#define _SESSION_H_

#include <gtk/gtk.h>
#include <X11/SM/SMlib.h>

gboolean session_auto_respawn;

void session_init(const gchar *client_id);

#endif
