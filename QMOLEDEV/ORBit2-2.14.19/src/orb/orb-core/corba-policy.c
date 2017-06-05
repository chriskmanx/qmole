#include "config.h"
#include <orbit/orbit.h>
#include "../util/orbit-purify.h"

static void
ORBit_Policy_release (ORBit_RootObject obj)
{
	struct CORBA_Policy_type *policy = (struct CORBA_Policy_type *)obj;

	p_free (policy, struct CORBA_Policy_type);
}

static ORBit_RootObject_Interface ORBit_Policy_interface = {
	ORBIT_ROT_POLICY,
	ORBit_Policy_release
};

CORBA_Policy
ORBit_Policy_new (CORBA_unsigned_long type,
		  CORBA_unsigned_long value)
{
	struct CORBA_Policy_type *policy;

	policy = g_new0 (struct CORBA_Policy_type, 1);
	ORBit_RootObject_init ((ORBit_RootObject)policy, &ORBit_Policy_interface);

	policy->type  = type;
	policy->value = value;

	return (CORBA_Policy)ORBit_RootObject_duplicate (policy);
}

CORBA_PolicyType
CORBA_Policy__get_policy_type (CORBA_Policy       p,
			       CORBA_Environment *ev)
{
	struct CORBA_Policy_type *policy = (struct CORBA_Policy_type *)p;

	return policy->type;
}

CORBA_Policy
CORBA_Policy_copy (CORBA_Policy       p,
		   CORBA_Environment *ev)
{
	struct CORBA_Policy_type *policy = (struct CORBA_Policy_type *)p;

	return ORBit_Policy_new (policy->type, policy->value);
}

void
CORBA_Policy_destroy (CORBA_Policy       p,
		      CORBA_Environment *ev)
{
}

CORBA_Policy
CORBA_DomainManager_get_domain_policy (CORBA_DomainManager     p,
				       const CORBA_PolicyType  policy_type,
				       CORBA_Environment      *ev)
{
	return CORBA_OBJECT_NIL;
}

void
CORBA_ConstructionPolicy_make_domain_manager (CORBA_ConstructionPolicy  p,
					      const CORBA_InterfaceDef  object_type,
					      const CORBA_boolean       constr_policy,
					      CORBA_Environment        *ev)
{

}
