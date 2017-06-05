/*
 * linc.h: This file is part of the linc library.
 *
 * Authors:
 *    Elliot Lee     (sopwith@redhat.com)
 *    Michael Meeks  (michael@ximian.com)
 *    Mark McLouglin (mark@skynet.ie) & others
 *
 * Copyright 2001, Red Hat, Inc., Ximian, Inc.,
 *                 Sun Microsystems, Inc.
 */
#ifndef _LINC_H_
#define _LINC_H_

#include <linc/linc-config.h>
#include <linc/linc-types.h>
#include <linc/linc-protocol.h>
#include <linc/linc-connection.h>
#include <linc/linc-server.h>

G_BEGIN_DECLS

extern GMainLoop *linc_loop;

GMutex    *linc_object_get_mutex (void);
gpointer   linc_object_ref       (GObject *object);
void       linc_object_unref     (GObject *object);

void       linc_set_threaded     (gboolean       threaded);
void       linc_init             (gboolean       init_threads);

LincWatch *linc_io_add_watch     (GIOChannel    *channel,
				  GIOCondition   condition,
				  GIOFunc        func,
				  gpointer       user_data);
void       linc_io_remove_watch  (LincWatch     *watch);
void       linc_main_iteration   (gboolean       block_for_reply);
gboolean   linc_main_pending     (void);
void       linc_main_loop_run    (void);
GMainLoop *linc_main_get_loop    (void);

G_END_DECLS

#endif /* _LINC_H_ */
