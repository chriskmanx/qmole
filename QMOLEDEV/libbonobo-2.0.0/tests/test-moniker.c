#include <config.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <libbonobo.h>

static void
check_string (const char *prefix, const char *escaped, const char *unescaped)
{
	BonoboMoniker *moniker;
	const char    *const_str;
	char          *str;
	char          *s, *name;
	CORBA_Environment ev;
	CORBA_long        equal;

	moniker = bonobo_moniker_construct (
		g_object_new (bonobo_moniker_get_type (), NULL), prefix);
	
	name = g_strconcat (prefix, escaped, NULL);
	bonobo_moniker_set_name (moniker, name);

	const_str = bonobo_moniker_get_name (moniker);
	fprintf (stderr, "'%s' == '%s'\n", unescaped, const_str);
	g_assert (!strcmp (const_str, unescaped));

	CORBA_exception_init (&ev);
	equal = Bonobo_Moniker_equal (BONOBO_OBJREF (moniker), name, &ev);
	g_assert (!BONOBO_EX (&ev));
	g_assert (equal);
	CORBA_exception_free (&ev);

	s = g_strconcat (prefix, escaped, NULL);
	str = bonobo_moniker_get_name_escaped (moniker);
	fprintf (stderr, "'%s' == '%s'\n", str, s);
	g_assert (!strcmp (str, s));
	g_free (str);
	g_free (s);

	g_assert (bonobo_moniker_client_equal (
		BONOBO_OBJREF (moniker), name, NULL));

	bonobo_object_unref (BONOBO_OBJECT (moniker));

	g_free (name);
}

static void
check_parse_name (const char *name, const char *res, int plen)
{
	const char *mname;
	int l;

	mname = bonobo_moniker_util_parse_name (name, &l);

	fprintf (stderr, "result %s %s %d\n", name, mname, l);

	g_assert (!strcmp (res, mname));
	g_assert (plen == l);
}

static void
test_real_monikers (void)
{
	CORBA_Environment *ev, real_ev;
	Bonobo_Unknown     object;

	CORBA_exception_init ((ev = &real_ev));

	/* Try an impossible moniker resolve */
	object = bonobo_get_object ("OAFIID:Bonobo_Moniker_Oaf",
				    "IDL:Bonobo/PropertyBag:1.0", ev);
	g_assert (object == CORBA_OBJECT_NIL);
	if (BONOBO_EX (ev))
		printf ("%s\n", bonobo_exception_get_text (ev));

	CORBA_exception_free (ev);
}

int
main (int argc, char *argv [])
{
	free (malloc (8));

	if (!bonobo_init (NULL, NULL))
		g_error ("Can not bonobo_init");

	check_string ("a:", "\\\\", "\\");

	check_string ("a:", "\\#", "#");

	check_string ("prefix:", "\\!", "!");

	check_string ("a:",
		      "1\\!\\#\\!\\!\\#\\\\",
		      "1!#!!#\\");

	check_parse_name ("#b:", "b:", 0);

	check_parse_name ("a:#b:", "b:", 2);

	check_parse_name ("a:!b:", "!b:", 2);

	check_parse_name ("a:3456789#b:", "b:", 9);

	check_parse_name ("a:\\##b:", "b:", 4);

	check_parse_name ("a:\\#c:", "a:\\#c:", 0);

	check_parse_name ("a:\\\\##c:", "c:", 5);

	check_parse_name ("a:\\\\#b:#c:", "c:", 7);

	check_parse_name ("a:\\\\#b:\\#c:", "b:\\#c:", 4);

	check_parse_name ("a:\\\\\\#b:\\#c:", "a:\\\\\\#b:\\#c:", 0);

	test_real_monikers ();

	return bonobo_debug_shutdown ();
}
