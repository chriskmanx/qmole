#ifndef _BONOBO_MONIKER_EXTRA_H_
#define _BONOBO_MONIKER_EXTRA_H_

#include <bonobo/bonobo-moniker-simple.h>
#include <bonobo/bonobo-moniker-extender.h>

Bonobo_Unknown bonobo_moniker_config_resolve (
	BonoboMoniker               *moniker,
	const Bonobo_ResolveOptions *options,
	const CORBA_char            *requested_interface,
	CORBA_Environment           *ev);

Bonobo_Unknown bonobo_moniker_conf_indirect_resolve (
	BonoboMoniker               *moniker,
	const Bonobo_ResolveOptions *options,
	const CORBA_char            *requested_interface,
	CORBA_Environment           *ev);

#endif
