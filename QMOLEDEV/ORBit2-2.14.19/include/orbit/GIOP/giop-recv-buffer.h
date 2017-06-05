#ifndef GIOP_RECV_BUFFER_H
#define GIOP_RECV_BUFFER_H 1

#include <stdio.h>
#include <orbit/GIOP/giop-types.h>

G_BEGIN_DECLS

#ifdef ORBIT2_INTERNAL_API

typedef enum {
  GIOP_MSG_READING_HEADER,
  GIOP_MSG_READING_BODY,
  GIOP_MSG_AWAITING_FRAGMENTS,
  GIOP_MSG_READY
} GIOPMessageBufferState;

typedef struct _GIOPMessageQueueEntry GIOPMessageQueueEntry;

typedef void (*GIOPAsyncCallback) (GIOPMessageQueueEntry *ent);

/* Internals used to setup waiting for a reply */
struct _GIOPMessageQueueEntry {
	GIOPRecvBuffer     *buffer;
	GIOPConnection     *cnx;
	CORBA_unsigned_long msg_type;
	CORBA_unsigned_long request_id;
	GIOPThread         *src_thread;

	GIOPAsyncCallback   async_cb;
};

struct _GIOPRecvBuffer {
	GIOPMsg msg;

	guchar *message_body;
	guchar *cur;
	guchar *end;

	GIOPConnection        *connection;
	GIOPMessageBufferState state;

	GIOPVersion giop_version;
	gulong      left_to_read;
	guint       free_body : 1;
};

#define         giop_msg_conversion_needed(msg)     giop_endian_conversion_needed(GIOP_MSG(msg)->header.flags)
GIOPRecvBuffer *giop_recv_buffer_use_buf           (GIOPConnection        *cnx);
GIOPRecvBuffer *giop_recv_buffer_use_encaps_buf    (GIOPRecvBuffer        *buf);
GIOPRecvBuffer *giop_recv_buffer_use_encaps        (guchar                *mem,
						    gulong                 len);
void            giop_recv_list_destroy_queue_entry (GIOPMessageQueueEntry *ent);
void            giop_recv_list_setup_queue_entry   (GIOPMessageQueueEntry *ent,
						    GIOPConnection        *cnx,
						    CORBA_unsigned_long    msg_type,
						    CORBA_unsigned_long    request_id);
void            giop_recv_list_setup_queue_entry_async (GIOPMessageQueueEntry *ent,
							GIOPAsyncCallback      cb);

GIOPRecvBuffer *giop_recv_buffer_get               (GIOPMessageQueueEntry *ent,
						    gboolean *timeout);
void            giop_recv_buffer_unuse             (GIOPRecvBuffer        *buf);

#define giop_recv_buffer_reply_status(buf) (                                            \
	(buf)->msg.header.version [1] ==  2 ? (buf)->msg.u.reply_1_2.reply_status :     \
	(buf)->msg.header.version [1] ==  1 ? (buf)->msg.u.reply_1_1.reply_status :     \
	(buf)->msg.header.version [1] ==  0 ? (buf)->msg.u.reply_1_0.reply_status : 0   \
)

CORBA_unsigned_long         giop_recv_buffer_get_request_id (GIOPRecvBuffer *buf);
char                       *giop_recv_buffer_get_opname     (GIOPRecvBuffer *buf);
CORBA_sequence_CORBA_octet *giop_recv_buffer_get_objkey     (GIOPRecvBuffer *buf);
void                        giop_recv_list_zap              (GIOPConnection *cnx);
gboolean                    giop_connection_handle_input    (LinkConnection *lcnx);
void                        giop_connection_destroy_frags   (GIOPConnection *cnx);
extern void                 giop_timeout_add                (GIOPConnection *cnx);


#endif /* ORBIT2_INTERNAL_API */

G_END_DECLS

#endif
