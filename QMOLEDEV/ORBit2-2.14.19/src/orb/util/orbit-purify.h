#ifndef _ORBIT_PURIFY_H
#define _ORBIT_PURIFY_H 1

#include <string.h>
#include <glib.h>

#ifdef ORBIT_PURIFY
#  define p_memwipe(m,len)	memset ((m), 0xaa, (len))
#  define p_memzero(m,len)      memset ((m), 0, (len))
#  define p_free(m,type)        G_STMT_START { \
					p_memwipe ((m), sizeof (type)); \
					g_free (m); \
				} G_STMT_END
#else
#  define p_memwipe(m,len)
#  define p_memzero(m,len)
#  define p_free(m,len)         g_free (m)
#endif

#endif /* ORBIT_PURIFY */
