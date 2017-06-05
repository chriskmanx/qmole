#include "everything.h"
#include "constants.h"
#include <stdio.h>

/* DerivedServer bits */
POA_test_DerivedServer__epv DerivedServer_epv = { NULL };
POA_test_BaseServer__epv BaseServer_epv = {
	NULL,
	NULL, /* opPolymorphic */
	NULL  /* attribPolymorphic */
};
POA_test_B1__epv B1_epv = { NULL };
POA_test_B2__epv B2_epv = { NULL };
POA_test_C1__epv C1_epv = { NULL };
PortableServer_ServantBase__epv DerivedServer_base_epv = {NULL, simple_finalize, NULL};
POA_test_DerivedServer__vepv DerivedServer_vepv = {
	&DerivedServer_base_epv,
	&BaseServer_epv,
	&B1_epv,
	&B2_epv,
	&C1_epv,
	&DerivedServer_epv
};

