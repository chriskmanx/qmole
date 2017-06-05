#ifndef __ORBIT_DEBUG_H__
#define __ORBIT_DEBUG_H__

typedef enum {
	ORBIT_DEBUG_NONE           = 0,
	ORBIT_DEBUG_TRACES         = 1 << 0,
	ORBIT_DEBUG_INPROC_TRACES  = 1 << 1,
	ORBIT_DEBUG_TIMINGS        = 1 << 2,
	ORBIT_DEBUG_TYPES          = 1 << 3,
	ORBIT_DEBUG_MESSAGES       = 1 << 4,
	ORBIT_DEBUG_ERRORS         = 1 << 5,
	ORBIT_DEBUG_OBJECTS        = 1 << 6,
	ORBIT_DEBUG_GIOP           = 1 << 7,
	ORBIT_DEBUG_REFS           = 1 << 8,
	ORBIT_DEBUG_FORCE_THREADED = 1 << 9
} OrbitDebugFlags;

#ifndef G_ENABLE_DEBUG

/*static inline void dprintf (OrbitDebugFlags flags, const char *format, ...) { };*/
#ifdef G_HAVE_ISO_VARARGS
#  define dprintf(...)
#  define tprintf(...)
#elif defined(G_HAVE_GNUC_VARARGS)
#  define dprintf(args...)
#  define tprintf(args...)
#else
/* This sucks for compatibility - fix your compiler */
#define MESSAGES (OrbitDebugFlags)0
#define TYPES    (OrbitDebugFlags)0
#define OBJECTS  (OrbitDebugFlags)0
#define GIOP     (OrbitDebugFlags)0
#define ERRORS   (OrbitDebugFlags)0
static inline void dprintf (OrbitDebugFlags flags, const char *format, ...)
{
}
static inline void tprintf (const char *format, ...)
{
}
#endif

#define dump_arg(a,b)

#define tprintf_header(obj,md)
#define tprintf_trace_value(a,b)
#define tprintf_timestamp()
#define tprintf_end_method()
#define tprintf_timestamp()

#else /* G_ENABLE_DEBUG */

#include <stdio.h>

extern OrbitDebugFlags _orbit_debug_flags;

#ifdef G_HAVE_ISO_VARARGS
#  define dprintf(type, ...) G_STMT_START {		\
	if (_orbit_debug_flags & ORBIT_DEBUG_##type)	\
		fprintf (stderr, __VA_ARGS__);		\
	} G_STMT_END
#elif defined(G_HAVE_GNUC_VARARGS)
#  define dprintf(type, args...) G_STMT_START {		\
	if (_orbit_debug_flags & ORBIT_DEBUG_##type)	\
		fprintf (stderr, args);			\
	} G_STMT_END
#endif

static inline void
dump_arg (const ORBit_IArg *a, CORBA_TypeCode tc)
{
	if (_orbit_debug_flags & ORBIT_DEBUG_MESSAGES)
		fprintf (stderr, " '%s' : kind - %d, %c%c",
			 a->name, tc->kind, 
			 a->flags & (ORBit_I_ARG_IN | ORBit_I_ARG_INOUT) ? 'i' : ' ',
			 a->flags & (ORBit_I_ARG_OUT | ORBit_I_ARG_INOUT) ? 'o' : ' ');
}

void     ORBit_trace_objref     (const CORBA_Object   obj);
void     ORBit_trace_any        (const CORBA_any     *any);
void     ORBit_trace_typecode   (const CORBA_TypeCode tc);
void     ORBit_trace_value      (gconstpointer       *val,
				 CORBA_TypeCode       tc);
void     ORBit_trace_header     (CORBA_Object         object,
				 ORBit_IMethod       *m_data);
void     ORBit_trace_end_method (void);
void     ORBit_trace_profiles   (CORBA_Object obj);
void     ORBit_trace_timestamp  (void);

#ifdef G_HAVE_ISO_VARARGS
#  define tprintf(...)  dprintf (TRACES, __VA_ARGS__)
#elif defined(G_HAVE_GNUC_VARARGS)
#  define tprintf(args...)  dprintf (TRACES, args)
#endif

#define tprintf_header(obj,md)		G_STMT_START {		\
	if (_orbit_debug_flags & ORBIT_DEBUG_TRACES)		\
		ORBit_trace_header (obj,md);			\
	} G_STMT_END

#define tprintf_trace_value(a,b)	G_STMT_START {		\
	if (_orbit_debug_flags & ORBIT_DEBUG_TRACES)		\
		ORBit_trace_value ((gconstpointer *)(a), (b));	\
	} G_STMT_END

#define tprintf_end_method() 		G_STMT_START {		\
	if (_orbit_debug_flags & ORBIT_DEBUG_TRACES)		\
		ORBit_trace_end_method ();			\
	} G_STMT_END

#define tprintf_timestamp()		G_STMT_START {		\
	if (_orbit_debug_flags & ORBIT_DEBUG_TIMINGS)		\
		ORBit_trace_timestamp ();			\
	} G_STMT_END

#endif /* G_ENABLE_DEBUG */

#endif /* __ORBIT_DEBUG_H__ */
