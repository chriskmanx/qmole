#ifndef TESTGP11HELPERS_H_
#define TESTGP11HELPERS_H_

#include "gp11.h"

#define FAIL_RES(res, e) do { \
	g_assert ((res) ? FALSE : TRUE); \
	g_assert ((e) && (e)->message && "error should be set"); \
	g_clear_error (&e); \
	} while (0)

#define SUCCESS_RES(res, err) do { \
	if (!(res)) g_printerr ("error: %s\n", err && err->message ? err->message : ""); \
	g_assert ((res) ? TRUE : FALSE && "should have succeeded"); \
	g_clear_error (&err); \
	} while(0)

/* 
 * Some dumb crypto mechanisms for simple testing.
 * 
 * CKM_CAPITALIZE (encrypt/decrypt)
 *     capitalizes to encrypt
 *     lowercase to decrypt 
 *
 * CKM_PREFIX (sign/verify)
 *     sign prefixes data with key label
 *     verify unprefixes data with key label. 
 *
 * CKM_GENERATE (generate-pair)
 *     generates a pair of keys, mechanism param should be 'generate'
 *
 * CKM_WRAP (wrap key)
 *     wraps key by returning value, mechanism param should be 'wrap'
 *
 * CKM_DERIVE (derive-key)
 *     derives key by setting value to 'derived'.
 *     mechanism param should be 'derive'
 */

#define CKM_CAPITALIZE    (CKM_VENDOR_DEFINED | 1)
#define CKM_PREFIX        (CKM_VENDOR_DEFINED | 2)
#define CKM_GENERATE      (CKM_VENDOR_DEFINED | 3)
#define CKM_WRAP          (CKM_VENDOR_DEFINED | 4)
#define CKM_DERIVE        (CKM_VENDOR_DEFINED | 5)

#endif /*TESTGP11HELPERS_H_*/
