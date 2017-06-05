/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * bonobo-exception.c: a generic exception -> user string converter.
 *
 * Authors:
 *   Michael Meeks (michael@helixcode.com)
 *
 * Copyright 2000 Helix Code, Inc.
 */
#ifndef _BONOBO_EXCEPTION_H_
#define _BONOBO_EXCEPTION_H_

#include <glib.h>
#include <bonobo/Bonobo.h>

#define bonobo_exception_set(opt_ev,repo_id) G_STMT_START{                  \
     if (opt_ev) {                                                          \
         CORBA_exception_set (opt_ev, CORBA_USER_EXCEPTION, repo_id, NULL); \
     } else {                                                               \
	 g_log (G_LOG_DOMAIN,						    \
		G_LOG_LEVEL_CRITICAL,					    \
		"file %s: line %d: bonobo exception: `%s'",                 \
		__FILE__,						    \
		__LINE__,						    \
		bonobo_exception_repoid_to_text (repo_id));                 \
     } }G_STMT_END

#ifdef G_DISABLE_CHECKS

#define bonobo_return_if_fail(expr,opt_ev) G_STMT_START{		\
     if (!(expr)) {							\
         if (opt_ev)                                                    \
	     CORBA_exception_set (opt_ev, CORBA_USER_EXCEPTION,         \
				  ex_Bonobo_BadArg, NULL);              \
         return;                                                        \
     };	}G_STMT_END

#define bonobo_return_val_if_fail(expr,val,opt_ev) G_STMT_START{	\
     if (!(expr)) {							\
         if (opt_ev)                                                    \
	     CORBA_exception_set (opt_ev, CORBA_USER_EXCEPTION,         \
				  ex_Bonobo_BadArg, NULL);              \
         return val;                                                    \
     };	}G_STMT_END

#else /* !G_DISABLE_CHECKS */
#define bonobo_return_if_fail(expr,opt_ev) G_STMT_START{		\
     if (!(expr)) {							\
         if (opt_ev)                                                    \
	     CORBA_exception_set (opt_ev, CORBA_USER_EXCEPTION,         \
				  ex_Bonobo_BadArg, NULL);              \
	 g_log (G_LOG_DOMAIN,						\
		G_LOG_LEVEL_CRITICAL,					\
		"file %s: line %d (%s): assertion `%s' failed.",	\
		__FILE__,						\
		__LINE__,						\
		G_GNUC_PRETTY_FUNCTION,					\
		#expr);							\
         return;                                                        \
     };	}G_STMT_END
         
#define bonobo_return_val_if_fail(expr,val,opt_ev) G_STMT_START{	\
     if (!(expr)) {							\
         if (opt_ev)                                                    \
	     CORBA_exception_set (opt_ev, CORBA_USER_EXCEPTION,         \
				  ex_Bonobo_BadArg, NULL);              \
	 g_log (G_LOG_DOMAIN,						\
		G_LOG_LEVEL_CRITICAL,					\
		"file %s: line %d (%s): assertion `%s' failed.",	\
		__FILE__,						\
		__LINE__,						\
		G_GNUC_PRETTY_FUNCTION,					\
		#expr);							\
         return val;                                                    \
     };	}G_STMT_END
#endif

#define BONOBO_EX(ev)         ((ev) && (ev)->_major != CORBA_NO_EXCEPTION)

#define BONOBO_USER_EX(ev,id) ((ev) && (ev)->_major == CORBA_USER_EXCEPTION &&	\
			       (ev)->_id != NULL && !strcmp ((ev)->_id, id))

#define BONOBO_EX_REPOID(ev)  (ev)->_id

#define BONOBO_RET_EX(ev)		\
	G_STMT_START{			\
		if (BONOBO_EX (ev))	\
			return;		\
	}G_STMT_END

#define BONOBO_RET_VAL_EX(ev,v)		\
	G_STMT_START{			\
		if (BONOBO_EX (ev))	\
			return (v);	\
	}G_STMT_END

typedef char *(*BonoboExceptionFn)     (CORBA_Environment *ev, gpointer user_data);

char *bonobo_exception_get_text        (CORBA_Environment *ev);
char *bonobo_exception_repoid_to_text  (const char *repo_id);


void  bonobo_exception_add_handler_str (const char *repo_id,
					const char *str);

void  bonobo_exception_add_handler_fn  (const char *repo_id,
					BonoboExceptionFn fn,
					gpointer          user_data,
					GDestroyNotify    destroy_fn);

void  bonobo_exception_general_error_set (CORBA_Environment *ev,
					  CORBA_TypeCode     opt_deriv,
					  const char        *format,
					  ...);

const char *bonobo_exception_general_error_get (CORBA_Environment *ev);

#endif /* _BONOBO_EXCEPTION_H_ */
