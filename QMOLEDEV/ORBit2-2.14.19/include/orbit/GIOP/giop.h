#ifndef GIOP_H
#define GIOP_H 1

#include <linc/linc.h>
#define ORBIT_SSL_SUPPORT LINK_SSL_SUPPORT

#include <orbit/GIOP/giop-types.h>
#include <orbit/GIOP/giop-send-buffer.h>
#include <orbit/GIOP/giop-recv-buffer.h>
#include <orbit/GIOP/giop-connection.h>
#include <orbit/GIOP/giop-server.h>

G_BEGIN_DECLS

#ifdef ORBIT2_INTERNAL_API

void        giop_init              (gboolean thread_safe,
				    gboolean blank_wire_data);
void        giop_main_run          (void);
void        giop_shutdown          (void);
gboolean    giop_thread_safe       (void);
gboolean    giop_thread_io         (void);
GIOPThread *giop_thread_self       (void);
void        giop_invoke_async      (GIOPMessageQueueEntry *ent);
void        giop_recv_set_limit    (glong limit);
glong       giop_recv_get_limit    (void);
void        giop_incoming_signal_T (GIOPThread *tdata, GIOPMsgType t);

typedef struct _GIOPQueue GIOPQueue;
GIOPThread *giop_thread_get_main  (void);
void        giop_thread_set_main_handler (gpointer    request_handler);
void        giop_thread_request_push     (GIOPThread *tdata,
					  gpointer   *poa_object,
					  gpointer   *recv_buffer);
void        giop_thread_request_push_key (gpointer    key,
					  gpointer   *poa_object,
					  gpointer   *recv_buffer);
gboolean    giop_thread_same_key         (gpointer    key,
					  gboolean    no_key_default);
void        giop_thread_key_add          (GIOPThread *tdata,
					  gpointer    key);
void        giop_thread_key_release      (gpointer    key);
void        giop_thread_new_check        (GIOPThread *opt_self);
void        giop_thread_queue_process    (GIOPThread *tdata);
gboolean    giop_thread_queue_empty_T    (GIOPThread *tdata);
void        giop_thread_queue_tail_wakeup(GIOPThread *tdata);

#endif /* ORBIT2_INTERNAL_API */

G_END_DECLS

#endif
