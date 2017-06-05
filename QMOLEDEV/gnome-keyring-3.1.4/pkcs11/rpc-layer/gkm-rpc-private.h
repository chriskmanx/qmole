/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* p11-rpc-private.h - various ids and signatures for our protocol

   Copyright (C) 2008, Stef Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

#ifndef GKM_RPC_CALLS_H
#define GKM_RPC_CALLS_H

#include <stdlib.h>
#include <stdarg.h>

#include "egg/egg-buffer.h"

#include "pkcs11/pkcs11.h"


/* Whether to print debug output or not */
#define DEBUG_OUTPUT 0


/* The calls, must be in sync with array below */
enum {
	GKM_RPC_CALL_ERROR = 0,

	GKM_RPC_CALL_C_Initialize,
	GKM_RPC_CALL_C_Finalize,
	GKM_RPC_CALL_C_GetInfo,
	GKM_RPC_CALL_C_GetSlotList,
	GKM_RPC_CALL_C_GetSlotInfo,
	GKM_RPC_CALL_C_GetTokenInfo,
	GKM_RPC_CALL_C_GetMechanismList,
	GKM_RPC_CALL_C_GetMechanismInfo,
	GKM_RPC_CALL_C_InitToken,
	GKM_RPC_CALL_C_WaitForSlotEvent,

	GKM_RPC_CALL_C_OpenSession,

	GKM_RPC_CALL_C_CloseSession,
	GKM_RPC_CALL_C_CloseAllSessions,
	GKM_RPC_CALL_C_GetFunctionStatus,
	GKM_RPC_CALL_C_CancelFunction,

	GKM_RPC_CALL_C_GetSessionInfo,
	GKM_RPC_CALL_C_InitPIN,
	GKM_RPC_CALL_C_SetPIN,
	GKM_RPC_CALL_C_GetOperationState,
	GKM_RPC_CALL_C_SetOperationState,
	GKM_RPC_CALL_C_Login,
	GKM_RPC_CALL_C_Logout,
	GKM_RPC_CALL_C_CreateObject,
	GKM_RPC_CALL_C_CopyObject,
	GKM_RPC_CALL_C_DestroyObject,
	GKM_RPC_CALL_C_GetObjectSize,
	GKM_RPC_CALL_C_GetAttributeValue,
	GKM_RPC_CALL_C_SetAttributeValue,
	GKM_RPC_CALL_C_FindObjectsInit,
	GKM_RPC_CALL_C_FindObjects,
	GKM_RPC_CALL_C_FindObjectsFinal,
	GKM_RPC_CALL_C_EncryptInit,
	GKM_RPC_CALL_C_Encrypt,
	GKM_RPC_CALL_C_EncryptUpdate,
	GKM_RPC_CALL_C_EncryptFinal,
	GKM_RPC_CALL_C_DecryptInit,
	GKM_RPC_CALL_C_Decrypt,
	GKM_RPC_CALL_C_DecryptUpdate,
	GKM_RPC_CALL_C_DecryptFinal,
	GKM_RPC_CALL_C_DigestInit,
	GKM_RPC_CALL_C_Digest,
	GKM_RPC_CALL_C_DigestUpdate,
	GKM_RPC_CALL_C_DigestKey,
	GKM_RPC_CALL_C_DigestFinal,
	GKM_RPC_CALL_C_SignInit,
	GKM_RPC_CALL_C_Sign,
	GKM_RPC_CALL_C_SignUpdate,
	GKM_RPC_CALL_C_SignFinal,
	GKM_RPC_CALL_C_SignRecoverInit,
	GKM_RPC_CALL_C_SignRecover,
	GKM_RPC_CALL_C_VerifyInit,
	GKM_RPC_CALL_C_Verify,
	GKM_RPC_CALL_C_VerifyUpdate,
	GKM_RPC_CALL_C_VerifyFinal,
	GKM_RPC_CALL_C_VerifyRecoverInit,
	GKM_RPC_CALL_C_VerifyRecover,
	GKM_RPC_CALL_C_DigestEncryptUpdate,
	GKM_RPC_CALL_C_DecryptDigestUpdate,
	GKM_RPC_CALL_C_SignEncryptUpdate,
	GKM_RPC_CALL_C_DecryptVerifyUpdate,
	GKM_RPC_CALL_C_GenerateKey,
	GKM_RPC_CALL_C_GenerateKeyPair,
	GKM_RPC_CALL_C_WrapKey,
	GKM_RPC_CALL_C_UnwrapKey,
	GKM_RPC_CALL_C_DeriveKey,
	GKM_RPC_CALL_C_SeedRandom,
	GKM_RPC_CALL_C_GenerateRandom,

	GKM_RPC_CALL_MAX
};

typedef struct _GkmRpcCall {
	int call_id;
	const char* name;
	const char* request;
	const char* response;
} GkmRpcCall;

/*
 *  a_ = prefix denotes array of _
 *  A  = CK_ATTRIBUTE
 *  f_ = prefix denotes buffer for _
 *  M  = CK_MECHANISM
 *  u  = CK_ULONG
 *  s  = space padded string
 *  v  = CK_VERSION
 *  y  = CK_BYTE
 *  z  = null terminated string
 */

static const GkmRpcCall gkm_rpc_calls[] = {
	{ GKM_RPC_CALL_ERROR,                  "ERROR",                  NULL,      NULL                   },
	{ GKM_RPC_CALL_C_Initialize,           "C_Initialize",           "ay",      ""                     },
	{ GKM_RPC_CALL_C_Finalize,             "C_Finalize",             "",        ""                     },
	{ GKM_RPC_CALL_C_GetInfo,              "C_GetInfo",              "",        "vsusv"                },
	{ GKM_RPC_CALL_C_GetSlotList,          "C_GetSlotList",          "yfu",     "au"                   },
	{ GKM_RPC_CALL_C_GetSlotInfo,          "C_GetSlotInfo",          "u",       "ssuvv"                },
	{ GKM_RPC_CALL_C_GetTokenInfo,         "C_GetTokenInfo",         "u",       "ssssuuuuuuuuuuuvvs"   },
	{ GKM_RPC_CALL_C_GetMechanismList,     "C_GetMechanismList",     "ufu",     "au"                   },
	{ GKM_RPC_CALL_C_GetMechanismInfo,     "C_GetMechanismInfo",     "uu",      "uuu"                  },
	{ GKM_RPC_CALL_C_InitToken,            "C_InitToken",            "uayz",    ""                     },
	{ GKM_RPC_CALL_C_WaitForSlotEvent,     "C_WaitForSlotEvent",     "u",       "u"                    },
	{ GKM_RPC_CALL_C_OpenSession,          "C_OpenSession",          "uu",      "u"                    },
	{ GKM_RPC_CALL_C_CloseSession,         "C_CloseSession",         "u",       ""                     },
	{ GKM_RPC_CALL_C_CloseAllSessions,     "C_CloseAllSessions",     "u",       ""                     },
	{ GKM_RPC_CALL_C_GetFunctionStatus,    "C_GetFunctionStatus",    "u",       ""                     },
	{ GKM_RPC_CALL_C_CancelFunction,       "C_CancelFunction",       "u",       ""                     },
	{ GKM_RPC_CALL_C_GetSessionInfo,       "C_GetSessionInfo",       "u",       "uuuu"                 },
	{ GKM_RPC_CALL_C_InitPIN,              "C_InitPIN",              "uay",     ""                     },
	{ GKM_RPC_CALL_C_SetPIN,               "C_SetPIN",               "uayay",   ""                     },
	{ GKM_RPC_CALL_C_GetOperationState,    "C_GetOperationState",    "ufy",     "ay"                   },
	{ GKM_RPC_CALL_C_SetOperationState,    "C_SetOperationState",    "uayuu",   ""                     },
	{ GKM_RPC_CALL_C_Login,                "C_Login",                "uuay",    ""                     },
	{ GKM_RPC_CALL_C_Logout,               "C_Logout",               "u",       ""                     },
	{ GKM_RPC_CALL_C_CreateObject,         "C_CreateObject",         "uaA",     "u"                    },
	{ GKM_RPC_CALL_C_CopyObject,           "C_CopyObject",           "uuaA",    "u"                    },
	{ GKM_RPC_CALL_C_DestroyObject,        "C_DestroyObject",        "uu",      ""                     },
	{ GKM_RPC_CALL_C_GetObjectSize,        "C_GetObjectSize",        "uu",      "u"                    },
	{ GKM_RPC_CALL_C_GetAttributeValue,    "C_GetAttributeValue",    "uufA",    "aAu"                  },
	{ GKM_RPC_CALL_C_SetAttributeValue,    "C_SetAttributeValue",    "uuaA",    ""                     },
	{ GKM_RPC_CALL_C_FindObjectsInit,      "C_FindObjectsInit",      "uaA",     ""                     },
	{ GKM_RPC_CALL_C_FindObjects,          "C_FindObjects",          "ufu",     "au"                   },
	{ GKM_RPC_CALL_C_FindObjectsFinal,     "C_FindObjectsFinal",     "u",       ""                     },
	{ GKM_RPC_CALL_C_EncryptInit,          "C_EncryptInit",          "uMu",     ""                     },
	{ GKM_RPC_CALL_C_Encrypt,              "C_Encrypt",              "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_EncryptUpdate,        "C_EncryptUpdate",        "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_EncryptFinal,         "C_EncryptFinal",         "ufy",     "ay"                   },
	{ GKM_RPC_CALL_C_DecryptInit,          "C_DecryptInit",          "uMu",     ""                     },
	{ GKM_RPC_CALL_C_Decrypt,              "C_Decrypt",              "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_DecryptUpdate,        "C_DecryptUpdate",        "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_DecryptFinal,         "C_DecryptFinal",         "ufy",     "ay"                   },
	{ GKM_RPC_CALL_C_DigestInit,           "C_DigestInit",           "uM",      ""                     },
	{ GKM_RPC_CALL_C_Digest,               "C_Digest",               "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_DigestUpdate,         "C_DigestUpdate",         "uay",     ""                     },
	{ GKM_RPC_CALL_C_DigestKey,            "C_DigestKey",            "uu",      ""                     },
	{ GKM_RPC_CALL_C_DigestFinal,          "C_DigestFinal",          "ufy",     "ay"                   },
	{ GKM_RPC_CALL_C_SignInit,             "C_SignInit",             "uMu",     ""                     },
	{ GKM_RPC_CALL_C_Sign,                 "C_Sign",                 "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_SignUpdate,           "C_SignUpdate",           "uay",     ""                     },
	{ GKM_RPC_CALL_C_SignFinal,            "C_SignFinal",            "ufy",     "ay"                   },
	{ GKM_RPC_CALL_C_SignRecoverInit,      "C_SignRecoverInit",      "uMu",     ""                     },
	{ GKM_RPC_CALL_C_SignRecover,          "C_SignRecover",          "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_VerifyInit,           "C_VerifyInit",           "uMu",     ""                     },
	{ GKM_RPC_CALL_C_Verify,               "C_Verify",               "uayay",   ""                     },
	{ GKM_RPC_CALL_C_VerifyUpdate,         "C_VerifyUpdate",         "uay",     ""                     },
	{ GKM_RPC_CALL_C_VerifyFinal,          "C_VerifyFinal",          "uay",     ""                     },
	{ GKM_RPC_CALL_C_VerifyRecoverInit,    "C_VerifyRecoverInit",    "uMu",     ""                     },
	{ GKM_RPC_CALL_C_VerifyRecover,        "C_VerifyRecover",        "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_DigestEncryptUpdate,  "C_DigestEncryptUpdate",  "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_DecryptDigestUpdate,  "C_DecryptDigestUpdate",  "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_SignEncryptUpdate,    "C_SignEncryptUpdate",    "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_DecryptVerifyUpdate,  "C_DecryptVerifyUpdate",  "uayfy",   "ay"                   },
	{ GKM_RPC_CALL_C_GenerateKey,          "C_GenerateKey",          "uMaA",    "u"                    },
	{ GKM_RPC_CALL_C_GenerateKeyPair,      "C_GenerateKeyPair",      "uMaAaA",  "uu"                   },
	{ GKM_RPC_CALL_C_WrapKey,              "C_WrapKey",              "uMuufy",  "ay"                   },
	{ GKM_RPC_CALL_C_UnwrapKey,            "C_UnwrapKey",            "uMuayaA", "u"                    },
	{ GKM_RPC_CALL_C_DeriveKey,            "C_DeriveKey",            "uMuaA",   "u"                    },
	{ GKM_RPC_CALL_C_SeedRandom,           "C_SeedRandom",           "uay",     ""                     },
	{ GKM_RPC_CALL_C_GenerateRandom,       "C_GenerateRandom",       "ufy",     "ay"                   },
};

#ifdef _DEBUG
#define GKM_RPC_CHECK_CALLS() \
	{ int i; for (i = 0; i < GKM_RPC_CALL_MAX; ++i) assert (gkm_rpc_calls[i].call_id == i); }
#endif

#define GKM_RPC_HANDSHAKE \
	((unsigned char*)"PRIVATE-GNOME-KEYRING-PKCS11-PROTOCOL-V-1")
#define GKM_RPC_HANDSHAKE_LEN \
	(strlen ((char *)GKM_RPC_HANDSHAKE))

#define GKM_RPC_SOCKET_EXT 	"pkcs11"

typedef enum _GkmRpcMessageType {
	GKM_RPC_REQUEST = 1,
	GKM_RPC_RESPONSE
} GkmRpcMessageType;

typedef struct _GkmRpcMessage {
	int call_id;
	GkmRpcMessageType call_type;
	const char *signature;
	EggBuffer buffer;

	size_t parsed;
	const char *sigverify;
} GkmRpcMessage;

GkmRpcMessage*           gkm_rpc_message_new                     (EggBufferAllocator allocator);

void                     gkm_rpc_message_free                    (GkmRpcMessage *msg);

void                     gkm_rpc_message_reset                   (GkmRpcMessage *msg);

int                      gkm_rpc_message_equals                  (GkmRpcMessage *m1,
                                                                  GkmRpcMessage *m2);

#define                  gkm_rpc_message_is_verified(msg)        (!(msg)->sigverify || (msg)->sigverify[0] == 0)

#define                  gkm_rpc_message_buffer_error(msg)       (egg_buffer_has_error(&(msg)->buffer))

int                      gkm_rpc_message_prep                    (GkmRpcMessage *msg,
                                                                  int call_id,
                                                                  GkmRpcMessageType type);

int                      gkm_rpc_message_parse                   (GkmRpcMessage *msg,
                                                                  GkmRpcMessageType type);

int                      gkm_rpc_message_verify_part             (GkmRpcMessage *msg,
                                                                  const char* part);

int                      gkm_rpc_message_write_byte              (GkmRpcMessage *msg,
                                                                  CK_BYTE val);

int                      gkm_rpc_message_write_ulong             (GkmRpcMessage *msg,
                                                                  CK_ULONG val);

int                      gkm_rpc_message_write_zero_string       (GkmRpcMessage *msg,
                                                                  CK_UTF8CHAR* string);

int                      gkm_rpc_message_write_space_string      (GkmRpcMessage *msg,
                                                                  CK_UTF8CHAR* buffer,
                                                                  CK_ULONG length);

int                      gkm_rpc_message_write_byte_buffer       (GkmRpcMessage *msg,
                                                                  CK_ULONG count);

int                      gkm_rpc_message_write_byte_array        (GkmRpcMessage *msg,
                                                                  CK_BYTE_PTR arr,
                                                                  CK_ULONG num);

int                      gkm_rpc_message_write_ulong_buffer      (GkmRpcMessage *msg,
                                                                  CK_ULONG count);

int                      gkm_rpc_message_write_ulong_array       (GkmRpcMessage *msg,
                                                                  CK_ULONG_PTR arr,
                                                                  CK_ULONG num);

int                      gkm_rpc_message_write_attribute_buffer  (GkmRpcMessage *msg,
                                                                  CK_ATTRIBUTE_PTR arr,
                                                                  CK_ULONG num);

int                      gkm_rpc_message_write_attribute_array   (GkmRpcMessage *msg,
                                                                  CK_ATTRIBUTE_PTR arr,
                                                                  CK_ULONG num);

int                      gkm_rpc_message_write_version           (GkmRpcMessage *msg,
                                                                  CK_VERSION* version);


int                      gkm_rpc_message_read_byte               (GkmRpcMessage *msg,
                                                                  CK_BYTE* val);

int                      gkm_rpc_message_read_ulong              (GkmRpcMessage *msg,
                                                                  CK_ULONG* val);

int                      gkm_rpc_message_read_space_string       (GkmRpcMessage *msg,
                                                                  CK_UTF8CHAR* buffer,
                                                                  CK_ULONG length);

int                      gkm_rpc_message_read_version            (GkmRpcMessage *msg,
                                                                  CK_VERSION* version);



void                     gkm_rpc_log                             (const char *line);

void                     gkm_rpc_warn                            (const char* msg, ...);

void                     gkm_rpc_debug                           (const char* msg, ...);

#ifdef G_DISABLE_ASSERT
#define assert(x)
#else
#include <assert.h>
#endif

/*
 * PKCS#11 mechanism parameters are not easy to serialize. They're
 * completely different for so many mechanisms, they contain
 * pointers to arbitrary memory, and many callers don't initialize
 * them completely or properly.
 *
 * We only support certain mechanisms.
 *
 * Also callers do yucky things like leaving parts of the structure
 * pointing to garbage if they don't think it's going to be used.
 */

int    gkm_rpc_mechanism_is_supported        (CK_MECHANISM_TYPE mech);
void   gkm_rpc_mechanism_list_purge          (CK_MECHANISM_TYPE_PTR mechs, CK_ULONG_PTR n_mechs);
int    gkm_rpc_mechanism_has_sane_parameters (CK_MECHANISM_TYPE type);
int    gkm_rpc_mechanism_has_no_parameters   (CK_MECHANISM_TYPE mech);

#endif /* GKM_RPC_CALLS_H */
