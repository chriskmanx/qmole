#ifndef __POA_MACROS_H__
#define __POA_MACROS_H__

#include <glib.h>

G_BEGIN_DECLS

#define poa_sys_exception_if_fail(expr,ex)		G_STMT_START {	\
	if (!(expr)) {							\
		CORBA_exception_set_system (ev, (ex),			\
					    CORBA_COMPLETED_NO);	\
		g_warning (						\
			"file %s: line %d: assertion `%s' failed. "	\
			"returning exception '%s'",			\
			__FILE__,					\
			__LINE__,					\
			#expr,						\
			(ex));						\
		return;							\
	}						} G_STMT_END

#define poa_sys_exception_val_if_fail(expr,ex,val)	G_STMT_START {	\
	if (!(expr)) {							\
		CORBA_exception_set_system (ev, (ex),			\
					    CORBA_COMPLETED_NO);	\
		g_warning (						\
			"file %s: line %d: assertion `%s' failed. "	\
			"returning exception '%s'",			\
			__FILE__,					\
			__LINE__,					\
			#expr,						\
			(ex));						\
		return (val);						\
	}						} G_STMT_END

#define poa_exception_if_fail(expr,ex)			G_STMT_START {	\
	if (!(expr)) {							\
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,		\
				     (ex), NULL);			\
		g_warning (						\
			"file %s: line %d: assertion `%s' failed. "	\
			"returning exception '%s'",			\
			__FILE__,					\
			__LINE__,					\
			#expr,						\
			(ex));						\
		return;							\
	}						} G_STMT_END

#define poa_exception_val_if_fail(expr,ex,val)		G_STMT_START {	\
	if (!(expr)) {							\
		CORBA_exception_set (ev, CORBA_USER_EXCEPTION,		\
				     (ex), NULL);			\
		g_warning (						\
			"file %s: line %d: assertion `%s' failed. "	\
			"returning exception '%s'",			\
			__FILE__,					\
			__LINE__,					\
			#expr,						\
			(ex));						\
		return (val);						\
	}						} G_STMT_END

#define IS_USE_ACTIVE_OBJECT_MAP_ONLY(poa)							\
		((poa)->p_request_processing == PortableServer_USE_ACTIVE_OBJECT_MAP_ONLY)

#define IS_USE_DEFAULT_SERVANT(poa)								\
		((poa)->p_request_processing == PortableServer_USE_DEFAULT_SERVANT)

#define IS_USE_USE_SERVANT_MANAGER(poa)								\
		((poa)->p_request_processing == PortableServer_USE_SERVANT_MANAGER)

#define IS_ORB_CTRL_MODEL(poa)									\
		((poa)->p_thread == PortableServer_ORB_CTRL_MODEL)

#define IS_SINGLE_THREAD_MODEL(poa) 								\
		((poa)->p_thread == PortableServer_SINGLE_THREAD_MODEL)

#define IS_TRANSIENT(poa) 									\
		((poa)->p_lifespan == PortableServer_TRANSIENT)

#define IS_PERSISTENT(poa) 									\
		((poa)->p_lifespan == PortableServer_PERSISTENT)

#define IS_UNIQUE_ID(poa) 									\
		((poa)->p_id_uniqueness == PortableServer_UNIQUE_ID)

#define IS_MULTIPLE_ID(poa) 									\
		((poa)->p_id_uniqueness == PortableServer_MULTIPLE_ID)

#define IS_USER_ID(poa) 									\
		((poa)->p_id_assignment == PortableServer_USER_ID)

#define IS_SYSTEM_ID(poa) 									\
		((poa)->p_id_assignment == PortableServer_SYSTEM_ID)

#define IS_IMPLICIT_ACTIVATION(poa) 								\
		((poa)->p_implicit_activation == PortableServer_IMPLICIT_ACTIVATION)

#define IS_NO_IMPLICIT_ACTIVATION(poa) 								\
		((poa)->p_implicit_activation == PortableServer_NO_IMPLICIT_ACTIVATION)

#define IS_RETAIN(poa) 										\
		((poa)->p_servant_retention == PortableServer_RETAIN)

#define IS_NON_RETAIN(poa) 									\
		((poa)->p_servant_retention == PortableServer_NON_RETAIN)

G_END_DECLS

#endif /* __POA_MACROS_H__ */
