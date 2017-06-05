#ifndef _POATEST_BASIC_SHELL_H_
#define _POATEST_BASIC_SHELL_H_

#include "poatest.h"
#include "poatest-exception.h"

poatest poatest_run (PortableServer_POA        rootpoa, 
		     PortableServer_POAManager rootpoa_mgr);

extern CORBA_ORB orb;
extern PortableServer_POA child_poa;

#endif /* _POATEST_BASIC_SHELL_H_ */
