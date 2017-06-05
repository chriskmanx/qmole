#ifndef _LINC_DEBUG_H
#define _LINC_DEBUG_H

/*
 * Enables debug on the Unix socket / connection
 */
#undef CONNECTION_DEBUG

#ifndef CONNECTION_DEBUG
   static inline void d_printf (const char *format, ...) { };
#  define STATE_NAME(s) ""
#else
#  include <stdio.h>
#  define d_printf(format...) fprintf (stderr, format)
#  define STATE_NAME(s) (((s) == LINC_CONNECTED) ? "Connected" : \
			 ((s) == LINC_CONNECTING) ? "Connecting" : \
			 ((s) == LINC_DISCONNECTED) ? "Disconnected" : \
			 "Invalid state")
#endif

#endif /* _LINC_DEBUG_H */
