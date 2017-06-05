#ifndef _BONOBO_MONIKER_STD_H_
#define _BONOBO_MONIKER_STD_H_

#include <bonobo/bonobo-moniker-simple.h>
#include <bonobo/bonobo-moniker-extender.h>

Bonobo_Unknown bonobo_moniker_item_resolve    (BonoboMoniker               *moniker,
					       const Bonobo_ResolveOptions *options,
					       const CORBA_char            *requested_interface,
					       CORBA_Environment           *ev);

Bonobo_Unknown bonobo_moniker_ior_resolve     (BonoboMoniker               *moniker,
					       const Bonobo_ResolveOptions *options,
					       const CORBA_char            *requested_interface,
					       CORBA_Environment           *ev);
Bonobo_Unknown bonobo_moniker_oaf_resolve     (BonoboMoniker               *moniker,
					       const Bonobo_ResolveOptions *options,
					       const CORBA_char            *requested_interface,
					       CORBA_Environment           *ev);

Bonobo_Unknown bonobo_moniker_cache_resolve   (BonoboMoniker               *moniker,
					       const Bonobo_ResolveOptions *options,
					       const CORBA_char            *requested_interface,
					       CORBA_Environment           *ev);

Bonobo_Unknown bonobo_moniker_query_resolve   (BonoboMoniker               *moniker,
					       const Bonobo_ResolveOptions *options,
					       const CORBA_char            *requested_interface,
					       CORBA_Environment           *ev);

Bonobo_Unknown bonobo_moniker_new_resolve     (BonoboMoniker               *moniker,
					       const Bonobo_ResolveOptions *options,
					       const CORBA_char            *requested_interface,
					       CORBA_Environment           *ev);

/* extender functions */

Bonobo_Unknown bonobo_stream_extender_resolve (BonoboMonikerExtender       *extender,
					       const Bonobo_Moniker         m,
					       const Bonobo_ResolveOptions *options,
					       const CORBA_char            *display_name,
					       const CORBA_char            *requested_interface,
					       CORBA_Environment           *ev);

#endif /* _BONOBO_MONIKER_STD_H_ */
