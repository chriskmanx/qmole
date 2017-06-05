/* xmalloc.h - Header for do-or-die memory management functions.
 * 
 * Created by Kevin Locke (to accompany xmalloc.c)
 *
 * I hereby place this file in the public domain.  It may be freely reproduced,
 * distributed, used, modified, built upon, or otherwise employed by anyone
 * for any purpose without restriction.
 */

extern void *xmalloc(size_t size);
extern void *xcalloc(size_t num, size_t size);
extern void *xrealloc(void *ptr, size_t size);
