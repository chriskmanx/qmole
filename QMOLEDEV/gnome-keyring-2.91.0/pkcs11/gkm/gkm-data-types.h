#ifndef GKM_DATA_H_
#define GKM_DATA_H_

#include <glib.h>

typedef enum _GkmDataResult {
	GKM_DATA_FAILURE = -2,
	GKM_DATA_LOCKED = -1,
	GKM_DATA_UNRECOGNIZED = 0,
	GKM_DATA_SUCCESS = 1
} GkmDataResult;

#define  GKM_DATA_ERROR      (g_quark_from_static_string ("gkm-data-error"))

#endif /* GKM_DATA_H_ */
