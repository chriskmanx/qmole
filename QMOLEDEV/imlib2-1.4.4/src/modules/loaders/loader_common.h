#ifndef __LOADER_COMMON_H
#define __LOADER_COMMON_H 1

#include "config.h"
#include "common.h"
#include "image.h"

EAPI char           load(ImlibImage * im, ImlibProgressFunction progress,
                         char progress_granularity, char immediate_load);
EAPI char           save(ImlibImage * im, ImlibProgressFunction progress,
                         char progress_granularity);
EAPI void           formats(ImlibLoader * l);

#endif /* __LOADER_COMMON_H */
