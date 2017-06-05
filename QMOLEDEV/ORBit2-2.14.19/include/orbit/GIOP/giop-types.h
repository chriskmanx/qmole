#ifndef GIOP_TYPES_H
#define GIOP_TYPES_H 1

#include <glib.h>
#include <glib-object.h>
#include <linc/linc.h>
#include <orbit/orbit-config.h>
#include <orbit/util/orbit-util.h>

#include <orbit/orb-core/corba-defs.h>
#include <orbit/orb-core/iop-defs.h>

#include <orbit/GIOP/giop-basics.h>

G_BEGIN_DECLS

#ifdef ORBIT2_INTERNAL_API

typedef struct _GIOPThread GIOPThread;

struct _GIOPThread {
	GMutex       *lock;
	GCond        *incoming;
	GMainContext *wake_context;

	GList        *keys;	/* ie. per POA, per Connection, per Object etc. */

	GList        *async_ents;
	GList        *request_queue;

	GQueue       *invoke_policies;

	void        (*request_handler) (gpointer poa_object,
					gpointer recv_buffer,
					gpointer dummy);
};

#define GIOP_INITIAL_MSG_SIZE_LIMIT (256*1024)

typedef enum {
	GIOP_CONNECTION_SSL
} GIOPConnectionOptions;

extern const char giop_version_ids[GIOP_NUM_VERSIONS][2];

typedef struct {
	CORBA_char magic[4];
	CORBA_char version[2];
	CORBA_octet flags;
	CORBA_octet message_type;
	CORBA_unsigned_long message_size;
} GIOPMsgHeader;

typedef enum 
{
	GIOP_REQUEST,
	GIOP_REPLY,
	GIOP_CANCELREQUEST,
	GIOP_LOCATEREQUEST,
	GIOP_LOCATEREPLY,
	GIOP_CLOSECONNECTION,
	GIOP_MESSAGEERROR,
	GIOP_FRAGMENT,
	GIOP_NUM_MSG_TYPES
} GIOPMsgType;

#if 0
typedef enum
{
  GIOP_NO_EXCEPTION,
  GIOP_USER_EXCEPTION,
  GIOP_SYSTEM_EXCEPTION,
  GIOP_LOCATION_FORWARD,
  GIOP_LOCATION_FORWARD_PERM,
  GIOP_NEEDS_ADDRESSING_MODE
} GIOPReplyStatusType_1_2;

typedef enum
{
  GIOP_UNKNOWN_OBJECT,
  GIOP_OBJECT_HERE,
  GIOP_OBJECT_FORWARD,
  GIOP_OBJECT_FORWARD_PERM,
  GIOP_LOC_SYSTEM_EXCEPTION,
  GIOP_LOC_NEEDS_ADDRESSING_MODE
} GIOPLocateStatusType_1_2;

typedef CORBA_unsigned_long IOP_ServiceId;
typedef struct {
  IOP_ServiceId context_id;
  CORBA_sequence_CORBA_octet context_data;
} IOP_ServiceContext;

typedef struct {
  CORBA_unsigned_long _length;
  IOP_ServiceContext *_buffer;
  CORBA_boolean _release : 1;
} IOP_ServiceContextList;
#endif

typedef CORBA_sequence_CORBA_octet CORBA_Principal;

typedef struct {
  IOP_ServiceContextList service_context;
  CORBA_unsigned_long request_id;
  CORBA_boolean response_expected;
  CORBA_sequence_CORBA_octet object_key;
  CORBA_char *operation;
  CORBA_Principal requesting_principal;
} GIOPMsgRequest_1_0;

typedef struct {
  IOP_ServiceContextList service_context;
  CORBA_unsigned_long request_id;
  CORBA_boolean response_expected;
  CORBA_char reserved[3];
  CORBA_sequence_CORBA_octet object_key;
  CORBA_char *operation;
  CORBA_Principal requesting_principal;
} GIOPMsgRequest_1_1;

typedef struct {
  CORBA_unsigned_long request_id;
  CORBA_octet response_flags;
  CORBA_octet reserved[3];
  GIOP_TargetAddress target;
  CORBA_char *operation;
  IOP_ServiceContextList service_context;
} GIOPMsgRequest_1_2;

typedef struct {
  IOP_ServiceContextList service_context;
  CORBA_unsigned_long request_id;
  CORBA_unsigned_long reply_status; /* lame */
} GIOPMsgReply_1_0;

typedef GIOPMsgReply_1_0 GIOPMsgReply_1_1;

typedef struct {
  CORBA_unsigned_long request_id;
  CORBA_unsigned_long reply_status;
  IOP_ServiceContextList service_context;
} GIOPMsgReply_1_2;

typedef struct {
  CORBA_unsigned_long request_id;
} GIOPMsgCancelRequest;

typedef struct {
  CORBA_unsigned_long request_id;
  CORBA_sequence_CORBA_octet object_key;
} GIOPMsgLocateRequest_1_0;

typedef GIOPMsgLocateRequest_1_0 GIOPMsgLocateRequest_1_1;

typedef struct {
  CORBA_unsigned_long request_id;
  GIOP_TargetAddress target;
} GIOPMsgLocateRequest_1_2;

typedef struct {
  CORBA_unsigned_long request_id;
  CORBA_unsigned_long locate_status; /* lame */
} GIOPMsgLocateReply_1_0;

typedef GIOPMsgLocateReply_1_0 GIOPMsgLocateReply_1_1;

typedef struct {
  CORBA_unsigned_long request_id;
  CORBA_unsigned_long locate_status;
} GIOPMsgLocateReply_1_2;

typedef struct {
  GIOPMsgHeader header;

  union {
    GIOPMsgRequest_1_0 request_1_0;
    GIOPMsgRequest_1_1 request_1_1;
    GIOPMsgRequest_1_2 request_1_2;
    GIOPMsgReply_1_0 reply_1_0;
    GIOPMsgReply_1_1 reply_1_1;
    GIOPMsgReply_1_2 reply_1_2;
    GIOPMsgCancelRequest cancel_request;
    GIOPMsgLocateRequest_1_0 locate_request_1_0;
    GIOPMsgLocateRequest_1_1 locate_request_1_1;
    GIOPMsgLocateRequest_1_2 locate_request_1_2;
    GIOPMsgLocateReply_1_0 locate_reply_1_0;
    GIOPMsgLocateReply_1_1 locate_reply_1_1;
    GIOPMsgLocateReply_1_2 locate_reply_1_2;
  } u;
} GIOPMsg;

#define GIOP_FLAG_BIG_ENDIAN 0
#define GIOP_FLAG_LITTLE_ENDIAN 1
#define GIOP_FLAG_FRAGMENTED 2

#if G_BYTE_ORDER == G_BIG_ENDIAN
#  define GIOP_FLAG_ENDIANNESS GIOP_FLAG_BIG_ENDIAN
#elif G_BYTE_ORDER == G_LITTLE_ENDIAN
#  define GIOP_FLAG_ENDIANNESS GIOP_FLAG_LITTLE_ENDIAN
#else
#  error "Unsupported endianness on this system."
#endif

#define giop_endian_conversion_needed(to_endianness) ((to_endianness&GIOP_FLAG_LITTLE_ENDIAN)!=GIOP_FLAG_ENDIANNESS)

#define GIOP_MSG(x) ((GIOPMsg *)(x))

#endif /* ORBIT2_INTERNAL_API */

G_END_DECLS

#endif
