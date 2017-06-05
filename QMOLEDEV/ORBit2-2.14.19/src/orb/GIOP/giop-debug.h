#ifndef __GIOP_DEBUG__
#define __GIOP_DEBUG__

#include "../orb-core/orbit-debug.h"

#ifndef G_ENABLE_DEBUG

#define do_giop_dump(fh, ptr, len, off)
#define do_giop_dump_send(buff)
#define do_giop_dump_recv(buff)

#else /* G_ENABLE_DEBUG */

/* Hooks for security / regression testing */
extern void (*giop_debug_hook_unexpected_reply) (GIOPRecvBuffer        *buf);
extern void (*giop_debug_hook_spoofed_reply)    (GIOPRecvBuffer        *buf,
						 GIOPMessageQueueEntry *ent);
extern void (*giop_debug_hook_incoming_mangler) (GIOPRecvBuffer        *buf);
extern void (*giop_debug_hook_new_connection)   (GIOPServer            *server,
						 GIOPConnection        *new_cnx);

#define do_giop_dump(fh, ptr, len, off)		G_STMT_START {	\
	if (_orbit_debug_flags & ORBIT_DEBUG_GIOP)		\
		giop_dump (fh, ptr, len, off);			\
} G_STMT_END

#define do_giop_dump_send(buff)			G_STMT_START {	\
	if (_orbit_debug_flags & ORBIT_DEBUG_GIOP)		\
		giop_dump_send (buff);				\
} G_STMT_END

#define do_giop_dump_recv(buff)			G_STMT_START {	\
	if (_orbit_debug_flags & ORBIT_DEBUG_GIOP)		\
		giop_dump_recv (buff);				\
} G_STMT_END

#endif /* G_ENABLE_DEBUG */

void giop_dump      (FILE *out, guint8 const *ptr, guint32 len, guint32 offset);
void giop_dump_send (GIOPSendBuffer *send_buffer);
void giop_dump_recv (GIOPRecvBuffer *recv_buffer);

#endif /* __GIOP_DEBUG__ */
