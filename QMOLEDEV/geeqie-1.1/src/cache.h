/*
 * Geeqie
 * (C) 2004 John Ellis
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */


#ifndef CACHE_H
#define CACHE_H


#include "similar.h"


#define GQ_CACHE_THUMB		"thumbnails"
#define GQ_CACHE_METADATA    	"metadata"

#define GQ_CACHE_LOCAL_THUMB    ".thumbnails"
#define GQ_CACHE_LOCAL_METADATA ".metadata"

#define GQ_CACHE_EXT_THUMB      ".png"
#define GQ_CACHE_EXT_SIM        ".sim"
#define GQ_CACHE_EXT_METADATA   ".meta"
#define GQ_CACHE_EXT_XMP_METADATA   ".gq.xmp"


typedef enum {
	CACHE_TYPE_THUMB,
	CACHE_TYPE_SIM,
	CACHE_TYPE_METADATA,
	CACHE_TYPE_XMP_METADATA
} CacheType;

typedef struct _CacheData CacheData;
struct _CacheData
{
	gchar *path;
	gint width;
	gint height;
	time_t date;
	glong checksum;
	guchar md5sum[16];
	ImageSimilarityData *sim;

	gboolean dimensions;
	gboolean have_date;
	gboolean have_checksum;
	gboolean have_md5sum;
	gboolean similarity;
};

gboolean cache_time_valid(const gchar *cache, const gchar *path);


CacheData *cache_sim_data_new(void);
void cache_sim_data_free(CacheData *cd);

gboolean cache_sim_data_save(CacheData *cd);
CacheData *cache_sim_data_load(const gchar *path);

void cache_sim_data_set_dimensions(CacheData *cd, gint w, gint h);
void cache_sim_data_set_date(CacheData *cd, time_t date);
void cache_sim_data_set_checksum(CacheData *cd, glong checksum);
void cache_sim_data_set_md5sum(CacheData *cd, guchar digest[16]);
void cache_sim_data_set_similarity(CacheData *cd, ImageSimilarityData *sd);
gint cache_sim_data_filled(ImageSimilarityData *sd);

gchar *cache_get_location(CacheType type, const gchar *source, gint include_name, mode_t *mode);
gchar *cache_find_location(CacheType type, const gchar *source);

const gchar *get_thumbnails_cache_dir(void);
const gchar *get_metadata_cache_dir(void);

#endif
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
