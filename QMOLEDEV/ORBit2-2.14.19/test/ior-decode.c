#include "config.h"
#include <stdio.h>
#include <string.h>
#include <orbit/orbit.h>
#include <ctype.h>

#include "../src/orb/orb-core/iop-profiles.h"

static void
print_objkey (ORBit_ObjectKey *objkey)
{
	int i;
	GString *str = g_string_sized_new (objkey->_length * 2 + 8);

	for (i = 0; i < objkey->_length; i++)
		g_string_append_printf (str, "%02x", objkey->_buffer [i]);

	printf ("(%u) '%s'", objkey->_length, str->str);

	g_string_free (str, TRUE);
}

static void
print_components (CORBA_Object obj, GSList *components)
{
	GSList *l;

	for (l = components; l; l = l->next) {
		IOP_Component_info *c = l->data;

		switch (c->component_type) {

		case IOP_TAG_COMPLETE_OBJECT_KEY:
			printf ("    IOP_TAG_COMPLETE_OBJECT_KEY: object_key ");
			print_objkey (obj->object_key);
			printf ("\n");
			break;

		case IOP_TAG_SSL_SEC_TRANS: {
			IOP_TAG_SSL_SEC_TRANS_info *sst = l->data;
			printf ("    IOP_TAG_SSL_SEC_TRANS: %u:%u port %d\n",
				sst->target_supports, sst->target_requires,
				sst->port);
			break;
		}

		case IOP_TAG_GENERIC_SSL_SEC_TRANS: {
			IOP_TAG_GENERIC_SSL_SEC_TRANS_info *sst = l->data;
			printf ("    IOP_TAG_GENERIC_SSL_SEC_TRANS: service %s\n",
			       sst->service);
			break;
		}

		default:
			printf ("    Unknown component %#x\n", c->component_type);
			break;
		}
		printf ("\n");
	}
}

static void
print_iiop_version (GIOPVersion ver)
{
	switch (ver) {
	case GIOP_1_0:
		printf ("GIOP 1.0");
		break;
	case GIOP_1_1:
		printf ("GIOP 1.1");
		break;
	case GIOP_1_2:
		printf ("GIOP 1.2");
		break;
	default:
		g_assert_not_reached ();
		break;
	}
}

int
main (int argc, char *argv[])
{
	GSList *l;
	CORBA_ORB orb;
	const char *ior;
	const char *type_id;
	CORBA_Object obj;
	CORBA_Environment ev;

	CORBA_exception_init (&ev);

	orb = CORBA_ORB_init (&argc, argv, "orbit-local-orb", &ev);

	if (argc != 2) {
		fprintf (stderr, "Usage: ior-decode <IOR>\n");
		return 1;
	}

	ior = strstr (argv [1], "IOR:");
	if (!ior)
		g_error ("Input doesn't look like an IOR\n");

	obj = CORBA_ORB_string_to_object (orb, ior, &ev);
	if (ev._major) {
		g_error ("Couldn't do string_to_object on '%s': %s\n",
			 ior, CORBA_exception_id (&ev));
		return 2;
	}

	if (obj == CORBA_OBJECT_NIL) {
		fprintf (stderr, "Resolved to a NIL object reference\n");
		return 3;
	}

	type_id = g_quark_to_string (obj->type_qid);
	printf ("Interface Repository ID: %s\n", type_id ? type_id : "<error no type id>");

	for (l = obj->profile_list; l; l = l->next) {
		IOP_Profile_info *pi = l->data;

		switch (pi->profile_type) {
		case IOP_TAG_INTERNET_IOP: {
			IOP_TAG_INTERNET_IOP_info *iiop = l->data;

			printf ("IOP_TAG_INTERNET_IOP: ");
			print_iiop_version (iiop->iiop_version);
			printf (" %s:%d\n",
			       iiop->host, iiop->port);
			printf ("    object_key ");
			print_objkey (obj->object_key);
			printf ("\n");
			print_components (obj, iiop->components);
			break;
		}
		case IOP_TAG_GENERIC_IOP: {
			IOP_TAG_GENERIC_IOP_info *giop = l->data;
			printf ("IOP_TAG_GENERIC_IOP: ");
			print_iiop_version (giop->iiop_version);
			printf ("[%s] %s:%s\n",
			       giop->proto,
			       giop->host, giop->service);
			print_components (obj, giop->components);
			break;
		}
		case IOP_TAG_MULTIPLE_COMPONENTS: {
			IOP_TAG_MULTIPLE_COMPONENTS_info *mci = l->data;
			printf ("IOP_TAG_MULTIPLE_COMPONENTS:\n");
			print_components (obj, mci->components);
			break;
		}
		case IOP_TAG_ORBIT_SPECIFIC: {
			IOP_TAG_ORBIT_SPECIFIC_info *osi = l->data;
			printf ("IOP_TAG_ORBIT_SPECIFIC: usock %s IPv6 port %d\n",
			       osi->unix_sock_path, osi->ipv6_port);
			printf ("    object_key ");
			print_objkey (obj->object_key);
			printf ("\n");
			break;
		}
		default:
			printf ("Unknown profile type %#x\n", pi->profile_type);
			break;
		}
		printf ("\n");
	}
  
	return 0;
}
