#ifndef GKM_RPC_LAYER_H_
#define GKM_RPC_LAYER_H_

#include "pkcs11/pkcs11.h"

/* ------------------------------------------------------------------
 * DISPATCHER
 */

/* Call to initialize the module */
int                gkm_rpc_layer_initialize             (CK_FUNCTION_LIST_PTR funcs);

/* Should be called to cleanup dispatcher */
void               gkm_rpc_layer_uninitialize           (void);

/* Call to start listening, returns socket or -1 */
int                gkm_rpc_layer_startup                (const char *prefix);

/* Accept a new connection. Should be called when above fd has read */
void               gkm_rpc_layer_accept                 (void);

/* Call to shutdown socket */
void               gkm_rpc_layer_shutdown               (void);

#endif /* GKM_RPC_LAYER_H_ */
