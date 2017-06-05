#include <config.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>

#include <orbit/orbit.h>

#include "../src/orb/orb-core/orb-core-private.h"

static void
dump_tc (CORBA_TypeCode tc)
{
	CORBA_TypeCode kind = TC_CORBA_TCKind;

	printf ("Type %12s: '%s'\n",
		kind->subnames [tc->kind],
		tc->repo_id);
}

static void
dump_iface (ORBit_IInterface *iface)
{
	int i;

	printf ("Interface '%s', %u methods\n",
		iface->tc->repo_id, iface->methods._length);

	for (i = 0; i < iface->base_interfaces._length; i++) {
		int j;

		printf ("  ");
		for (j = 0; j < i + 1; j++)
			printf ("  ");

		printf ("%s\n", iface->base_interfaces._buffer [i]);
	}

	printf ("\n");

	if (iface->methods._length > 0) {
		for (i = 0; i < iface->methods._length; i++) {
			ORBit_IMethod *m = &iface->methods._buffer [i];
			
			printf ("  %s (%u args, %s) %s%s\n",
				m->name, m->arguments._length,
				m->contexts._length ? "has context," : "",
				m->ret ? "returns " : "",
				m->ret ? m->ret->repo_id : "");
		}
	} else
		printf ("No methods\n");

	printf ("\n\n");
}

static void
list_libs (void)
{
	int    i;
	char **paths;

	printf ("Installed type libraries:\n\n");

	paths = ORBit_get_typelib_paths ();

	for (i = 0; paths && paths [i]; i++) {
		DIR *dh;
		struct dirent *de;

		dh = opendir (paths [i]);

		if (!dh)
			continue;

		printf ("%s:\n\n", paths [i]);
        
		for (de = readdir (dh); de; de = readdir (dh)) {
			char *p, *str = g_strdup (de->d_name);
			if ((p = strstr (str, "_module.la"))) {
				*p = '\0';
				printf ("\t%s\n", str);
			}
			g_free (str);
		}

		closedir (dh);
	}

	g_strfreev (paths);
}

int
main (int argc, char *argv [])
{
	int                              i;
	const char                      *name;
	CORBA_sequence_CORBA_TypeCode   *tcs;
	CORBA_sequence_ORBit_IInterface *ifaces;

	if (argc < 2) {
		list_libs ();
		return 0;
	}

	name = argv [argc - 1];

	if (!ORBit_small_load_typelib (name))
		g_error ("Can't find typelib of name '%s' in path", name);

	tcs = ORBit_small_get_types (name);

	if (!tcs || tcs->_length == 0)
		printf ("No types\n");
	else {
		printf ("%u types:\n", tcs->_length);
		for (i = 0; i < tcs->_length; i++)
			dump_tc (tcs->_buffer [i]);
	}

	ifaces = ORBit_small_get_iinterfaces (name);
	if (!ifaces || ifaces->_length == 0)
		printf ("No IInterfaces\n");
	else {
		printf ("%u interfaces:\n", ifaces->_length);
		for (i = 0; i < ifaces->_length; i++)
			dump_iface (&ifaces->_buffer [i]);
	}

	return 0;
}
