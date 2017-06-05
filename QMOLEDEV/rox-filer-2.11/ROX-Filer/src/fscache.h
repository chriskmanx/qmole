/*
 * ROX-Filer, filer for the ROX desktop project
 * Thomas Leonard, <tal197@users.sourceforge.net>
 */


#ifndef _FSCACHE_H
#define _FSCACHE_H

#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <glib.h>
#include <glib-object.h>

typedef GObject *(*GFSLoadFunc)(const char *pathname, gpointer user_data);
typedef void (*GFSUpdateFunc)(gpointer object,
			      const char *pathname,
			      gpointer user_data);
typedef enum {
	FSCACHE_LOOKUP_CREATE,	/* Load if missing. Update as needed. */
	FSCACHE_LOOKUP_ONLY_NEW,/* Return NULL if not present AND uptodate */
	FSCACHE_LOOKUP_PEEK,	/* Lookup; don't load or update */
	FSCACHE_LOOKUP_INIT,	/* Internal use */
	FSCACHE_LOOKUP_INSERT,	/* Internal use */
} FSCacheLookup;

GFSCache *g_fscache_new(GFSLoadFunc load,
			GFSUpdateFunc update,
			gpointer user_data);
void g_fscache_destroy(GFSCache *cache);
gpointer g_fscache_lookup(GFSCache *cache, const char *pathname);
gpointer g_fscache_lookup_full(GFSCache *cache, const char *pathname,
				FSCacheLookup lookup_type,
				gboolean *found);
void g_fscache_may_update(GFSCache *cache, const char *pathname);
void g_fscache_update(GFSCache *cache, const char *pathname);
void g_fscache_purge(GFSCache *cache, gint age);

void g_fscache_insert(GFSCache *cache, const char *pathname, gpointer obj,
		      gboolean update_details);

#endif /* _FSCACHE_H */
