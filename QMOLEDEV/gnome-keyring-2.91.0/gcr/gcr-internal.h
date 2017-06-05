#ifndef GCR_INTERNAL_H_
#define GCR_INTERNAL_H_

#include "gcr.h"

#include <glib.h>

void              _gcr_initialize                  (void);

GList*            _gcr_get_pkcs11_modules          (void);

#endif /* GCR_INTERNAL_H_ */
