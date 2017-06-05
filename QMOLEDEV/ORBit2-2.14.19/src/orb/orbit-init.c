#include "config.h"
#include <stdlib.h>
#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef _WIN32
#  include <process.h>
#endif
#include <orbit/orbit.h>

#include "orbit-init.h"
#include "poa/orbit-poa.h"
#include "orb-core/orb-core-private.h"

#ifdef G_OS_WIN32
#  define getuid() 0
#endif

void
ORBit_init_internals (CORBA_ORB          orb,
		      CORBA_Environment *ev)
{
	PortableServer_POA       root_poa;
	PortableServer_Current   poa_current;
	DynamicAny_DynAnyFactory dynany_factory;
	GTimeVal                 t;

	root_poa = ORBit_POA_setup_root (orb, ev);
	ORBit_set_initial_reference (orb, "RootPOA", root_poa);
	ORBit_RootObject_release (root_poa);

	poa_current = ORBit_POACurrent_new (orb);
	ORBit_set_initial_reference (orb, "POACurrent", poa_current);
	ORBit_RootObject_release (poa_current);

	dynany_factory = ORBit_DynAnyFactory_new (orb, ev);
	ORBit_set_initial_reference (orb, "DynAnyFactory", dynany_factory);
	ORBit_RootObject_release (dynany_factory);

	/* need to srand for linc's node creation */
	g_get_current_time (&t);
	srand (t.tv_sec ^ t.tv_usec ^ getpid () ^ getuid ());
}

const char  *orbit_version       = ORBIT_VERSION;
unsigned int orbit_major_version = ORBIT_MAJOR_VERSION;
unsigned int orbit_minor_version = ORBIT_MINOR_VERSION; 
unsigned int orbit_micro_version = ORBIT_MICRO_VERSION;
