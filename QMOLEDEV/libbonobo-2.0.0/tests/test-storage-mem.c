/* -*- mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */

#include <string.h>
#include <bonobo/bonobo-storage-memory.h>
#include <bonobo/bonobo-main.h>
#include <bonobo/bonobo-exception.h>

int
main (int argc, char *argv [])
{
	BonoboObject                 *storage;
	Bonobo_Storage                corba_storage;
	Bonobo_Storage                ret_storage;
	Bonobo_Stream                 ret_stream;
	CORBA_Environment             real_ev, *ev;
	Bonobo_StorageInfo           *info;
	Bonobo_Storage_DirectoryList *dir_list;
	int                           num_ok = 0;
	int                           num_tests = 0;
	
	ev = &real_ev;
	
	if (!bonobo_init (&argc, argv))
		g_error ("bonobo_init failed");

	storage = bonobo_storage_mem_create ();
	corba_storage = bonobo_object_corba_objref (storage);
	
	CORBA_exception_init (ev);

	printf ("creating storage:\t");
	num_tests++;
	ret_storage = Bonobo_Storage_openStorage (corba_storage,
						  "/foo",
						  Bonobo_Storage_CREATE,
						  ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\t'/foo'\n");
		num_ok++;
	} else {
		printf ("failed\t'/foo'\n");
		CORBA_exception_free (ev);
	}
	bonobo_object_release_unref (ret_storage, NULL);

	printf ("creating sub-storage:\t");
	num_tests++;
	ret_stream = Bonobo_Storage_openStorage (corba_storage,
						 "/foo/bar",
						 Bonobo_Storage_CREATE,
						 ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\t'/foo/bar'\n");
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}
	bonobo_object_release_unref (ret_stream, NULL);
	
	printf ("creating stream:\t");
	num_tests++;
	ret_stream = Bonobo_Storage_openStream (corba_storage,
						"/foo/bar/baz",
						Bonobo_Storage_CREATE,
						ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\t'/foo/bar/baz'\n");
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}
	bonobo_object_release_unref (ret_stream, NULL);

	printf ("creating stream:\t");
	num_tests++;
	ret_stream = Bonobo_Storage_openStream (corba_storage,
						"/foo/quux",
						Bonobo_Storage_CREATE,
						ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\t'/foo/quux'\n");
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}
	bonobo_object_release_unref (ret_stream, NULL);

	printf ("opening stream:\t\t");
	num_tests++;
	ret_stream = Bonobo_Storage_openStream (corba_storage,
						"/foo/quux",
						Bonobo_Storage_READ,
						ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\n");
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}
	bonobo_object_release_unref (ret_stream, NULL);

	printf ("opening missing stream:\t");
	num_tests++;
	Bonobo_Storage_openStream (corba_storage,
				   "/foo/dummy",
				   Bonobo_Storage_READ,
				   ev);
	if (BONOBO_EX (ev) &&
	    !strcmp (BONOBO_EX_REPOID (ev), ex_Bonobo_Storage_NotFound)) {
		printf ("passed\n");
		CORBA_exception_free (ev);
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}

	printf ("rename (storage):\t");
	num_tests++;
	Bonobo_Storage_rename (corba_storage,
			       "/foo", "/renamed",
			       ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\t'%s' -> '%s'\n",
			"/foo", "/renamed");
		
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}

	printf ("getInfo (storage):\t");
	num_tests++;
	info = Bonobo_Storage_getInfo (corba_storage,
				       "/renamed",
				       Bonobo_FIELD_TYPE,
				       ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\n");
		printf ("\t\t\t\tname:\t'%s'\n", info->name);
		printf ("\t\t\t\ttype:\t%s\n",
			info->type ? "storage" : "stream" );

		CORBA_free (info);
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}

	printf ("getInfo (stream):\t");
	num_tests++;
	info = Bonobo_Storage_getInfo (corba_storage,
				       "/renamed/quux",
				       Bonobo_FIELD_TYPE | Bonobo_FIELD_SIZE | Bonobo_FIELD_CONTENT_TYPE,
				       ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\n");
		printf ("\t\t\t\tname:\t'%s'\n", info->name);
		printf ("\t\t\t\ttype:\t%s\n",
			info->type ? "storage" : "stream" );
		printf ("\t\t\t\tmime:\t%s\n", info->content_type);

		CORBA_free (info);
		num_ok++;
		
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}

	printf ("getInfo (root):\t\t");
	num_tests++;
	info = Bonobo_Storage_getInfo (corba_storage,
				       "/",
				       Bonobo_FIELD_TYPE,
				       ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\n");
		printf ("\t\t\t\tname:\t'%s'\n", info->name);
		printf ("\t\t\t\ttype:\t%s\n",
			info->type ? "storage" : "stream" );

		CORBA_free (info);
		num_ok++;
		
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}
	
	printf ("listContents:\t\t");
	num_tests++;
	dir_list = Bonobo_Storage_listContents (corba_storage,
						"/renamed",
						0,
						ev);
	if (!BONOBO_EX (ev)) {
		int i;

		printf ("passed\n");

		for (i = 0; i < dir_list->_length; i++)
			printf ("\t\t\t\t%s%c\n",
				dir_list->_buffer[i].name,
				dir_list->_buffer[i].type ? '/' : ' ');
		
		CORBA_free (dir_list);
		num_ok++;
		
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}

	printf ("erase (stream):\t\t");
	num_tests++;
	Bonobo_Storage_erase (corba_storage,
			      "/renamed/bar/baz",
			      ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\n");
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}

	printf ("erase (empty storage):\t");
	num_tests++;
	Bonobo_Storage_erase (corba_storage,
			      "/renamed/bar",
			      ev);
	if (!BONOBO_EX (ev)) {
		printf ("passed\n");
		CORBA_exception_free (ev);
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}
	
	printf ("getInfo (dltd stream):\t");
	num_tests++;
	info = Bonobo_Storage_getInfo (
		corba_storage,
		"/renamed/bar/baz",
		Bonobo_FIELD_TYPE | Bonobo_FIELD_SIZE | Bonobo_FIELD_CONTENT_TYPE,
		ev);

	if (BONOBO_EX (ev) &&
	    !strcmp (BONOBO_EX_REPOID (ev), ex_Bonobo_Storage_NotFound)) {
		printf ("passed\n");
		num_ok++;
		CORBA_exception_free (ev);
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}

	printf ("getInfo (dltd storage):\t");
	num_tests++;
	info = Bonobo_Storage_getInfo (corba_storage,
				       "/renamed/bar",
				       Bonobo_FIELD_TYPE,
				       ev);
	if (BONOBO_EX (ev) &&
	    !strcmp (BONOBO_EX_REPOID (ev), ex_Bonobo_Storage_NotFound)) {
		printf ("passed\n");
		CORBA_exception_free (ev);
		num_ok++;
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}

	printf ("listContents (deleted):\t");
	num_tests++;
	dir_list = Bonobo_Storage_listContents (corba_storage,
						"/renamed/bar",
						0, ev);

	if (BONOBO_EX (ev) &&
	    !strcmp (BONOBO_EX_REPOID (ev), ex_Bonobo_Storage_NotFound)) {
		printf ("passed\n");
		num_ok++;
		CORBA_exception_free (ev);
	} else {
		printf ("failed: %s\n", BONOBO_EX_REPOID (ev));
		CORBA_exception_free (ev);
	}
	CORBA_exception_free (ev);

	bonobo_object_unref (BONOBO_OBJECT (storage));
	
	printf ("%d of %d tests passed\n", num_ok, num_tests);
	
	if (num_ok != num_tests)
		return 1;

	return bonobo_debug_shutdown ();
}
