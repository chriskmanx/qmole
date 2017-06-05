#include <stdarg.h>
#include <string.h>
#include "../util/orbit-purify.h"
#include "orbit-policy.h"
#include "orbit-debug.h"
#include "orbit-object.h"
#include "orbit/GIOP/giop.h"

GType
ORBit_policy_ex_get_type (void)
{
	return 0; /* FIXME: maybe use GObject some day ? */
}

static void
ORBit_policy_free_fn (ORBit_RootObject obj)
{
	ORBitPolicy *p = (ORBitPolicy *) obj;
	g_ptr_array_free (p->allowed_poas, TRUE);
	p_free (p, ORBitPolicy);
}

static const ORBit_RootObject_Interface ORBit_Policy_epv = {
	ORBIT_ROT_CLIENT_POLICY,
	ORBit_policy_free_fn
};

ORBitPolicy *
ORBit_policy_new (GType        type,
		  const char  *first_prop,
		  ...)
{
	va_list      args;
	const char  *name;
	ORBitPolicy *policy = g_new0 (ORBitPolicy, 1);
	ORBit_RootObject_init (&policy->parent, &ORBit_Policy_epv);

	policy->allowed_poas = g_ptr_array_sized_new (1);

	va_start (args, first_prop);
	for (name = first_prop; name; name = va_arg (args, char *)) {
		if (!strcmp (name, "allow")) {
			gpointer poa = va_arg (args, void *);
			g_ptr_array_add (policy->allowed_poas, poa);
		}
	}

	va_end (args);

	return ORBit_RootObject_duplicate_T (policy);
}

ORBitPolicy *
ORBit_policy_ref (ORBitPolicy *p)
{
	return ORBit_RootObject_duplicate (p);
}

void
ORBit_policy_unref (ORBitPolicy *p)
{
	ORBit_RootObject_release (p);
}

void
ORBit_object_set_policy (CORBA_Object obj,
			 ORBitPolicy *p)
{
	if (obj == CORBA_OBJECT_NIL)
		return;
	ORBit_policy_unref (obj->invoke_policy);
	obj->invoke_policy = ORBit_policy_ref (p);
}

ORBitPolicy *
ORBit_object_get_policy (CORBA_Object obj)
{
	if (obj == CORBA_OBJECT_NIL)
		return CORBA_OBJECT_NIL;
	else
		return ORBit_policy_ref (obj->invoke_policy);
}

void
ORBit_policy_push (ORBitPolicy *p)
{
	GIOPThread *tdata = giop_thread_self ();

	if (!tdata->invoke_policies)
		tdata->invoke_policies = g_queue_new ();
	
	g_queue_push_head (tdata->invoke_policies, ORBit_policy_ref (p));
}

void
ORBit_policy_pop (void)
{
	GIOPThread *tdata = giop_thread_self ();

	if (!tdata->invoke_policies)
		g_warning ("No policy queue to pop from");
	else {
		ORBitPolicy *p;
		p = g_queue_pop_head (tdata->invoke_policies);
		ORBit_policy_unref (p);
	}
}

gboolean
ORBit_policy_validate (ORBitPolicy *policy)
{
	return TRUE;
}
