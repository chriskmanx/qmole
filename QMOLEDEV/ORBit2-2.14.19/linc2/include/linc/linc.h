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
#ifndef _LINK_H_
#define _LINK_H_

#include <linc/linc-config.h>
#include <linc/linc-types.h>
#include <linc/linc-protocol.h>
#include <linc/linc-connection.h>
#include <linc/linc-server.h>
#include <linc/linc-source.h>

G_BEGIN_DECLS

extern GMainLoop *link_loop;

void       link_init             (gboolean    thread_safe);
void       link_set_io_thread    (gboolean    io_in_thread);
void       link_shutdown         (void);
void       link_main_iteration   (gboolean    block_for_reply);
gboolean   link_main_pending     (void);
void       link_main_loop_run    (void);
GMainLoop *link_main_get_loop    (void);
guint      link_main_idle_add    (GSourceFunc function,
				  gpointer    data);

void       link_wait             (void);
void       link_signal           (void);

gboolean   link_thread_io        (void);
gboolean   link_thread_safe      (void);

guint      link_io_thread_add_timeout    (guint       interval,
					  GSourceFunc function,
					  gpointer    data);
void       link_io_thread_remove_timeout (guint source_id);

#ifdef G_OS_WIN32
void link_map_winsock_error_to_errno (void);
#endif

int  link_pipe (int *handles); /* Creates a pipe on Unix, a TCP socket pair on Windows */

G_END_DECLS

#endif /* _LINK_H_ */
