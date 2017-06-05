#ifndef __FILTER_COMMON_H
#define __FILTER_COMMON_H 1

#include "config.h"
#include "common.h"
#include "dynamic_filters.h"
#include "image.h"

EAPI void           init(struct imlib_filter_info *info);
EAPI void           deinit(void);
EAPI void          *exec(char *filter, void *im, pIFunctionParam params);

#endif /* __FILTER_COMMON_H */
