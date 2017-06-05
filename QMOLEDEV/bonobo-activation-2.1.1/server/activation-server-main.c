/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 *  oafd: OAF CORBA dameon.
 *
 *  Copyright (C) 1999, 2000 Red Hat, Inc.
 *  Copyright (C) 1999, 2000 Eazel, Inc.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this library; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Authors: Elliot Lee <sopwith@redhat.com>,
 *
 */

#include <config.h>
#include <unistd.h>
#include <stdlib.h>

#include "server.h"
#include "bonobo-activation/bonobo-activation-i18n.h"

#include "activation-context-query.h"
#include "object-directory-config-file.h"

#include <ORBitservices/CosNaming.h>
#include <ORBitservices/CosNaming_impl.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <popt.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <locale.h>
#include <string.h>

#include <libxml/parser.h>

#ifdef BONOBO_ACTIVATION_DEBUG
static void debug_queries (void);
#endif

/* Option values */
static char *od_source_dir = NULL;
#ifdef BONOBO_ACTIVATION_DEBUG
static char *ac_evaluate = NULL;
static gboolean server_reg = FALSE;
#endif
static int server_ac = 0, ior_fd = -1;

static struct poptOption options[] = {

	{"od-source-dir", '\0', POPT_ARG_STRING, &od_source_dir, 0,
	 N_("Directory to read .server files from"), N_("DIRECTORY")},

	{"ac-activate", '\0', POPT_ARG_NONE, &server_ac, 0,
	 N_("Serve as an ActivationContext (default is as an ObjectDirectory only)"),
	 NULL},

	{"ior-output-fd", '\0', POPT_ARG_INT, &ior_fd, 0,
	 N_("File descriptor to write IOR to"), N_("FD")},

#ifdef BONOBO_ACTIVATION_DEBUG
        {"register-server", '0', POPT_ARG_NONE, &server_reg, 0,
	 "Register as the users' activation server without locking [!]",
	 NULL},

	{"evaluate", '\0', POPT_ARG_STRING, &ac_evaluate, 0,
	 N_("Query expression to evaluate"), N_("EXPRESSION")},
#endif

	POPT_AUTOHELP {NULL}
};

GMainLoop *main_loop = NULL;

static GString *
build_src_dir (void)
{
        const char *env_od_source_dir;
        const char *gnome_env_od_source_dir;
        char *config_file_od_source_dir;
        GString *gnome_od_source_dir;
        char **gnome_dirs;
        GString *real_od_source_dir;
        int i;

        real_od_source_dir = g_string_new (SERVERINFODIR);
        env_od_source_dir = g_getenv ("BONOBO_ACTIVATION_PATH");
        gnome_env_od_source_dir = g_getenv ("GNOME2_PATH");
        config_file_od_source_dir = object_directory_load_config_file ();

        if (od_source_dir) {
                g_string_append_c (real_od_source_dir, ':');
                g_string_append (real_od_source_dir, od_source_dir);
        }

        if (env_od_source_dir) {
                g_string_append_c (real_od_source_dir, ':');
                g_string_append (real_od_source_dir,
                                 env_od_source_dir);
        }

        if (config_file_od_source_dir) {
                g_string_append_c (real_od_source_dir, ':');
                g_string_append (real_od_source_dir,
                                 config_file_od_source_dir);
                g_free (config_file_od_source_dir);
        }

        if (gnome_env_od_source_dir) {
                gnome_dirs = g_strsplit (gnome_env_od_source_dir, ":", -1);
                gnome_od_source_dir = g_string_new("");
                for (i=0; gnome_dirs[i]; i++) {
                        g_string_append (gnome_od_source_dir,
                                         gnome_dirs[i]);
                        g_string_append (gnome_od_source_dir,
                                         "/lib/bonobo/servers:");
                }
                g_strfreev (gnome_dirs);
                g_string_append_c (real_od_source_dir, ':');
                g_string_append (real_od_source_dir,
                                 gnome_od_source_dir->str);
        }

        return real_od_source_dir;
}

static int
redirect_output (int ior_fd)
{
        int dev_null_fd;
        const char *debug_output;

        debug_output = g_getenv ("BONOBO_ACTIVATION_DEBUG_OUTPUT");

        dev_null_fd = -1;
        if (debug_output == NULL || strlen (debug_output) == 0) {
                dev_null_fd = open ("/dev/null", O_RDWR);
		if (ior_fd != 0)
                        dup2 (dev_null_fd, 0);
		if (ior_fd != 1)
                        dup2 (dev_null_fd, 1);
		if (ior_fd != 2)
                        dup2 (dev_null_fd, 2);
        }

        return dev_null_fd;
}

static void
nameserver_destroy (PortableServer_POA  poa,
                    const CORBA_Object  reference,
                    CORBA_Environment  *ev)
{
        PortableServer_ObjectId *oid;

        oid = PortableServer_POA_reference_to_id (poa, reference, ev);
	PortableServer_POA_deactivate_object (poa, oid, ev);
	CORBA_free (oid);
}

int
main (int argc, char *argv[])
{
        PortableServer_POAManager     poa_manager;
        PortableServer_POA            root_poa;
        CORBA_ORB                     orb;
        CORBA_Environment             real_ev, *ev;
        CORBA_Object                  naming_service;
        Bonobo_ActivationEnvironment  environment;
        Bonobo_ObjectDirectory        od;
        poptContext                   ctx;
        int                           dev_null_fd;
        char                         *ior;
        FILE                         *fh;
        struct sigaction              sa;
        GString                      *src_dir;

        /*
         *    Become process group leader, detach from controlling
         * terminal, etc.
         */
        setsid ();
        
	/* internationalization. */
	setlocale (LC_ALL, "");
        bindtextdomain (PACKAGE, SERVER_LOCALEDIR);
        textdomain (PACKAGE);

        /* Ignore sig-pipe - as normal */
	memset (&sa, 0, sizeof (sa));
	sa.sa_handler = SIG_IGN;
	sigaction (SIGPIPE, &sa, NULL);


	ctx = poptGetContext ("oafd", argc, (const char **)argv, options, 0);
	while (poptGetNextOpt (ctx) >= 0) ;
	poptFreeContext (ctx);

        LIBXML_TEST_VERSION;
	xmlKeepBlanksDefault(0);

#if 0
        while (!g_file_test ("/tmp/orbit-go", G_FILE_TEST_EXISTS))
                sleep (1);
#endif

        dev_null_fd = redirect_output (ior_fd);

	orb = bonobo_activation_init (argc, argv);
	main_loop = g_main_loop_new (NULL, FALSE);

        add_initial_locales ();
        
	CORBA_exception_init ((ev = &real_ev));

	root_poa = (PortableServer_POA)
		CORBA_ORB_resolve_initial_references (orb, "RootPOA", ev);

        src_dir = build_src_dir ();
        bonobo_object_directory_init (root_poa, src_dir->str, ev);
        g_string_free (src_dir, TRUE);

        od = bonobo_object_directory_get ();

	memset (&environment, 0, sizeof (Bonobo_ActivationEnvironment));

        naming_service = impl_CosNaming_NamingContext__create (root_poa, ev);
        if (naming_service == NULL)
                g_warning ("Failed to create naming service");
        Bonobo_ObjectDirectory_register_new 
                (od, NAMING_CONTEXT_IID, &environment, naming_service, ev);

        if (ior_fd < 0 && !server_ac)
                g_error ("\n\n-- \nThe bonobo-activation-server must be forked by\n"
                         "libbonobo-activation, and cannot be run itself.\n"
                         "This is due to us doing client side locking.\n-- \n");

        /*
         *     It is no longer useful at all to be a pure
         * ObjectDirectory we have binned that mode of
         * operation, as a bad, racy and inefficient job.
         */
        g_assert (server_ac);
        
        activation_context_init (root_poa, od, ev);

	ior = CORBA_ORB_object_to_string (orb, activation_context_get (), ev);

	fh = NULL;
	if (ior_fd >= 0)
		fh = fdopen (ior_fd, "w");
	if (fh) {
		fprintf (fh, "%s\n", ior);
		fclose (fh);
		if (ior_fd <= 2)
                        dup2 (dev_null_fd, ior_fd);
	} else {
		printf ("%s\n", ior);
		fflush (stdout);
	}
        if (dev_null_fd != -1)
                close (dev_null_fd);

#ifdef BONOBO_ACTIVATION_DEBUG
	debug_queries ();
        if (server_reg) {
                char *fname;
                fname = g_strconcat (linc_get_tmpdir (),
                                     "/bonobo-activation-server-ior", NULL);
                fh = fopen (fname, "w+");
		fprintf (fh, "%s\n", ior);
		fclose (fh);
                g_free (fname);
        }
#endif

	CORBA_free (ior);

        poa_manager = PortableServer_POA__get_the_POAManager (root_poa, ev);
	PortableServer_POAManager_activate (poa_manager, ev);

	g_main_loop_run (main_loop);

        nameserver_destroy (root_poa, naming_service, ev);
        CORBA_Object_release (naming_service, ev);

        bonobo_object_directory_shutdown (root_poa, ev);
        activation_context_shutdown (root_poa, ev);

        CORBA_Object_release ((CORBA_Object) poa_manager, ev);
        CORBA_Object_release ((CORBA_Object) root_poa, ev);

	return !bonobo_activation_debug_shutdown ();
}

#ifdef BONOBO_ACTIVATION_DEBUG
static void
debug_queries (void)
{
	if (ac_evaluate) {
		QueryExpr *exp;
		const char *err;
		QueryContext tmpctx = { NULL, 0, CORBA_OBJECT_NIL };

		err = qexp_parse (ac_evaluate, &exp);
		if (err) {
			g_print ("Parse error: %s\n", err);
		} else {
			QueryExprConst res;

			qexp_dump (exp);
			g_print ("\n");
			g_print ("Evaluation with no server record is: ");
			res = qexp_evaluate (NULL, exp, &tmpctx);
			qexp_constant_dump (&res);
			g_print ("\n");
		}
	}
}
#endif
