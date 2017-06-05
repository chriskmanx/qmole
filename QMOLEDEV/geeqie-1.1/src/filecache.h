/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: Vladimir Nadvornik
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#ifndef FILECACHE_H
#define FILECACHE_H

#include "main.h"
#include "filedata.h"

typedef struct _FileCacheData FileCacheData;
typedef void (*FileCacheReleaseFunc)(FileData *fd);


FileCacheData *file_cache_new(FileCacheReleaseFunc release, gulong max_size);
gboolean file_cache_get(FileCacheData *fc, FileData *fd);
void file_cache_put(FileCacheData *fc, FileData *fd, gulong size);
void file_cache_dump(FileCacheData *fc);
void file_cache_set_size(FileCacheData *fc, gulong size);
gulong file_cache_get_max_size(FileCacheData *fc);
gulong file_cache_get_size(FileCacheData *fc);
void file_cache_set_max_size(FileCacheData *fc, gulong size);


#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
