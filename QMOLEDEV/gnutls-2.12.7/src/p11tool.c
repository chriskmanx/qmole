/*
 * Copyright (C) 2010, 2011 Free Software Foundation, Inc.
 *
 * Author: Nikos Mavrogiannopoulos
 *
 * This file is part of GnuTLS.
 *
 * GnuTLS is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GnuTLS is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#include <config.h>

#include <gnutls/gnutls.h>
#include <gnutls/x509.h>
#include <gnutls/openpgp.h>
#include <gnutls/pkcs12.h>
#include <gnutls/pkcs11.h>
#include <gnutls/abstract.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <error.h>

/* Gnulib portability files. */
#include <read-file.h>
#include <progname.h>
#include <version-etc.h>

#include "p11tool-gaa.h"
#include "p11tool.h"
#include "certtool-common.h"

static void gaa_parser (int argc, char **argv);

static gaainfo info;
static FILE *outfile;
int batch = 0;

static void
tls_log_func (int level, const char *str)
{
  fprintf (stderr, "|<%d>| %s", level, str);
}


int
main (int argc, char **argv)
{
  set_program_name (argv[0]);
  gaa_parser (argc, argv);

  return 0;
}

static void
gaa_parser (int argc, char **argv)
{
  int ret;
  common_info_st cinfo;

  if (gaa (argc, argv, &info) != -1)
    {
      fprintf (stderr, "Try `%s --help' for more information.\n",
               program_name);
      exit (1);
    }

  gnutls_global_set_log_function (tls_log_func);
  gnutls_global_set_log_level (info.debug);
  if (info.debug > 1)
    printf ("Setting log level to %d\n", info.debug);

  if ((ret = gnutls_global_init ()) < 0)
    error (EXIT_FAILURE, 0, "global_init: %s", gnutls_strerror (ret));

  if (info.pkcs11_provider != NULL)
    {
      ret = gnutls_pkcs11_init (GNUTLS_PKCS11_FLAG_MANUAL, NULL);
      if (ret < 0)
        fprintf (stderr, "pkcs11_init: %s", gnutls_strerror (ret));
      else
        {
          ret = gnutls_pkcs11_add_provider (info.pkcs11_provider, NULL);
          if (ret < 0)
            error (EXIT_FAILURE, 0, "pkcs11_add_provider: %s",
                   gnutls_strerror (ret));
        }
    }
  else
    {
      ret = gnutls_pkcs11_init (GNUTLS_PKCS11_FLAG_AUTO, NULL);
      if (ret < 0)
        fprintf (stderr, "pkcs11_init: %s", gnutls_strerror (ret));
    }

  if (info.outfile)
    {
      outfile = safe_open_rw (info.outfile, 0);
      if (outfile == NULL)
        error (EXIT_FAILURE, errno, "%s", info.outfile);
    }
  else
    outfile = stdout;

  memset (&cinfo, 0, sizeof (cinfo));
  cinfo.secret_key = info.secret_key;
  cinfo.privkey = info.privkey;
  cinfo.pkcs8 = info.pkcs8;
  cinfo.incert_format = info.incert_format;
  cinfo.cert = info.cert;

  switch (info.action)
    {
    case ACTION_PKCS11_LIST:
      pkcs11_list (outfile, info.pkcs11_url, info.pkcs11_type,
                   info.pkcs11_login, info.pkcs11_detailed_url, &cinfo);
      break;
    case ACTION_PKCS11_TOKENS:
      pkcs11_token_list (outfile, info.pkcs11_detailed_url, &cinfo);
      break;
    case ACTION_PKCS11_MECHANISMS:
      pkcs11_mechanism_list (outfile, info.pkcs11_url, info.pkcs11_login,
                             &cinfo);
      break;
    case ACTION_PKCS11_EXPORT_URL:
      pkcs11_export (outfile, info.pkcs11_url, info.pkcs11_login, &cinfo);
      break;
    case ACTION_PKCS11_WRITE_URL:
      pkcs11_write (outfile, info.pkcs11_url, info.pkcs11_label,
                    info.pkcs11_trusted, info.pkcs11_login, &cinfo);
      break;
    case ACTION_PKCS11_TOKEN_INIT:
      pkcs11_init (outfile, info.pkcs11_url, info.pkcs11_label, &cinfo);
      break;
    case ACTION_PKCS11_DELETE_URL:
      pkcs11_delete (outfile, info.pkcs11_url, 0, info.pkcs11_login, &cinfo);
      break;
    default:
      gaa_help ();
      exit (0);
    }
  fclose (outfile);

  gnutls_pkcs11_deinit ();
  gnutls_global_deinit ();
}
