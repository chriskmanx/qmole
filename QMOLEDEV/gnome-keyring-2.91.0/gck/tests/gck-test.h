#ifndef GCK_TEST_H_
#define GCK_TEST_H_

#include "gck.h"
#include "gck-mock.h"

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

#endif /* GCK_TEST_H_ */
