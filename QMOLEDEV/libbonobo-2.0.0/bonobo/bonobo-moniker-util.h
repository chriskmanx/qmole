/*
 * bonobo-moniker-util.h
 *
 * Copyright (C) 2000  Helix Code, Inc.
 *
 * Authors:
 *	Michael Meeks    (michael@helixcode.com)
 *	Ettore Perazzoli (ettore@helixcode.com)
 */

#ifndef _BONOBO_MONIKER_UTIL_H
#define _BONOBO_MONIKER_UTIL_H

#include <bonobo/Bonobo.h>

/* Useful client functions */
Bonobo_Unknown      bonobo_get_object                            (const CORBA_char        *name,      
								  const char              *interface_name,
								  CORBA_Environment       *opt_ev);


Bonobo_Moniker      bonobo_moniker_client_new_from_name          (const CORBA_char        *name,
								  CORBA_Environment       *opt_ev);

CORBA_char         *bonobo_moniker_client_get_name               (Bonobo_Moniker     moniker,
								  CORBA_Environment *opt_ev);

Bonobo_Unknown      bonobo_moniker_client_resolve_default        (Bonobo_Moniker     moniker,
								  const char        *interface_name,
								  CORBA_Environment *opt_ev);

gboolean            bonobo_moniker_client_equal                  (Bonobo_Moniker     moniker,
								  const CORBA_char  *name,
								  CORBA_Environment *opt_ev);

typedef void (*BonoboMonikerAsyncFn) (Bonobo_Unknown     object,
				      CORBA_Environment *ev,
				      gpointer           user_data);
/* Async equivalents */

void                bonobo_get_object_async                      (const CORBA_char        *name,
								  const char              *interface_name,
								  CORBA_Environment       *ev,
								  BonoboMonikerAsyncFn     cb,
								  gpointer                 user_data);

void                bonobo_moniker_client_new_from_name_async    (const CORBA_char        *name,
								  CORBA_Environment       *ev,
								  BonoboMonikerAsyncFn     cb,
								  gpointer                 user_data);

void                bonobo_moniker_resolve_async                 (Bonobo_Moniker           moniker,
								  Bonobo_ResolveOptions   *options,
								  const char              *interface_name,
								  CORBA_Environment       *ev,
								  BonoboMonikerAsyncFn     cb,
								  gpointer                 user_data);

void                bonobo_moniker_resolve_async_default         (Bonobo_Moniker           moniker,
								  const char              *interface_name,
								  CORBA_Environment       *ev,
								  BonoboMonikerAsyncFn     cb,
								  gpointer                 user_data);

/* Useful moniker implementation helper functions */
CORBA_char    *bonobo_moniker_util_get_parent_name      (Bonobo_Moniker     moniker,
							 CORBA_Environment *opt_ev);
Bonobo_Unknown bonobo_moniker_util_qi_return            (Bonobo_Unknown     object,
							 const CORBA_char  *requested_interface,
							 CORBA_Environment *ev);
const char    *bonobo_moniker_util_parse_name           (const char        *name, 
							 int               *plen);
int            bonobo_moniker_util_seek_std_separator   (const CORBA_char  *str,
							 int                min_idx);
char          *bonobo_moniker_util_escape               (const char        *string,
							 int                offset);
char          *bonobo_moniker_util_unescape             (const char        *string,
							 int                num_chars);

void           bonobo_url_register                      (char              *oafiid, 
							 char              *url, 
							 char              *mime_type,
							 Bonobo_Unknown     object,
							 CORBA_Environment *ev);

void           bonobo_url_unregister                    (char              *oafiid, 
							 char              *url,
							 CORBA_Environment *ev);

Bonobo_Unknown bonobo_url_lookup                        (char              *oafiid,
							 char              *url,
							 CORBA_Environment *ev);
					   
#endif /* _BONOBO_MONIKER_UTIL_H */
