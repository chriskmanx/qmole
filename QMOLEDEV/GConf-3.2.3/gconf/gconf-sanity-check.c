/* GConf
 * Copyright (C) 2002 Red Hat Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#include <config.h>
#include "gconf.h"
#include "gconf-internals.h"
#include "gconf-sources.h"
#include "gconf-backend.h"
#include <glib/gstdio.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>

static gboolean ensure_gtk (void);
static void     show_fatal_error_dialog (const char *format,
                                         ...) G_GNUC_PRINTF (1, 2);
static gboolean offer_delete_locks (void);
static gboolean check_file_locking (void);
static gboolean check_gconf (gboolean display_errors);

int 
main (int argc, char** argv)
{
  GOptionContext *context;
  GError *error;

  g_thread_init (NULL);

  context = g_option_context_new (_("- Sanity checks for GConf"));
  g_option_context_add_group (context, gtk_get_option_group (FALSE));

  error = NULL;
  g_option_context_parse (context, &argc, &argv, &error);
  g_option_context_free (context);

  if (error)
    {
      g_printerr (_("Error while parsing options: %s.\nRun '%s --help' to see a full list of available command line options.\n"),
                  error->message,
                  argv[0]);
      g_error_free (error);
      return 1;
    }

  if (!check_file_locking ())
    return 1;

  if (!check_gconf (FALSE))
    {
      if (!offer_delete_locks ())
        return 1;
  
      if (!check_gconf (TRUE))
        return 1;
    }
  
  return 0;
}

#ifdef F_SETLK
/* Your basic Stevens cut-and-paste */
static int
lock_reg (int fd, int cmd, int type, off_t offset, int whence, off_t len)
{
  struct flock lock;

  lock.l_type = type; /* F_RDLCK, F_WRLCK, F_UNLCK */
  lock.l_start = offset; /* byte offset relative to whence */
  lock.l_whence = whence; /* SEEK_SET, SEEK_CUR, SEEK_END */
  lock.l_len = len; /* #bytes, 0 for eof */

  return fcntl (fd, cmd, &lock);
}
#endif

#ifdef F_SETLK
#define lock_entire_file(fd) \
  lock_reg ((fd), F_SETLK, F_WRLCK, 0, SEEK_SET, 0)
#define unlock_entire_file(fd) \
  lock_reg ((fd), F_SETLK, F_UNLCK, 0, SEEK_SET, 0)
#else
#warning Please implement proper locking
#define lock_entire_file(fd) 0
#define unlock_entire_file(fd) 0
#endif

static gboolean
check_file_locking (void)
{
  char *testfile;
  int fd;
  gboolean retval;

  retval = FALSE;
  testfile = NULL;
  fd = -1;
  
  if (gconf_use_local_locks ())
    {
      GError *err;

      err = NULL;
      if (g_getenv ("GCONF_TMPDIR")) {
	testfile = g_build_filename(g_getenv ("GCONF_TMPDIR"), "gconf-test-locking-file-XXXXXX", NULL);
	fd = g_mkstemp (testfile);
	if (fd == -1)
	    {
	      g_set_error (&err,
        	           G_FILE_ERROR,
                	   g_file_error_from_errno (errno),
	                   "Failed to create file '%s': %s",
			   testfile, g_strerror (errno));
	    }
    	}
      else {
	      fd = g_file_open_tmp ("gconf-test-locking-file-XXXXXX",
        	                    &testfile,
                	            &err);
      }

      if (err != NULL)
        {
          show_fatal_error_dialog (_("Please contact your system administrator to resolve the following problem:\n"
                                     "Could not open or create the file \"%s\"; this indicates "
                                     "that there may be a problem with your configuration, "
                                     "as many programs will need to create files in your "
                                     "home directory. The error was \"%s\" (errno = %d)."),
                                   testfile, err->message, errno);

          g_error_free (err);

          goto out;
        }
    }
  else
    {
      testfile = g_build_filename (g_get_home_dir (),
                                   ".gconf-test-locking-file",
                                   NULL);
      
      /* keep the open from failing due to non-writable old file or something */
      g_unlink (testfile);
  
      fd = g_open (testfile, O_WRONLY | O_CREAT, 0700);

      if (fd < 0)
        {      
          show_fatal_error_dialog (_("Please contact your system administrator to resolve the following problem:\n"
                                     "Could not open or create the file \"%s\"; this indicates "
                                     "that there may be a problem with your configuration, "
                                     "as many programs will need to create files in your "
                                     "home directory. The error was \"%s\" (errno = %d)."),
                                   testfile, g_strerror (errno), errno);
          
          goto out;
        }
    }
      

  if (lock_entire_file (fd) < 0)
    {      
      show_fatal_error_dialog (_("Please contact your system administrator to resolve the following problem:\n"
                                 "Could not lock the file \"%s\"; this indicates "
                                 "that there may be a problem with your operating system "
                                 "configuration. If you have an NFS-mounted home directory, "
                                 "either the client or the server may be set up incorrectly. "
                                 "See the rpc.statd and rpc.lockd documentation. "
                                 "A common cause of this error is that the \"nfslock\" service has been disabled."
                                 "The error was \"%s\" (errno = %d)."),
                               testfile, g_strerror (errno), errno); 
      goto out;
    }

  retval = TRUE;

 out:
  close (fd);
  if (g_unlink (testfile) < 0)
    g_printerr (_("Can't remove file %s: %s\n"), testfile, g_strerror (errno));
  g_free (testfile);
  
  return retval;
}

static gboolean
check_gconf (gboolean display_errors)
{
  GSList* addresses;
  GSList* tmp;
  gchar* conffile;
  GError* error;
  gboolean retval;

  retval = FALSE;
  conffile = NULL;
  
  /* If gconfd is already running, it's expected that we won't be able
   * to get locks etc., and everything is already fine.
   * Plus we can skip the slow sanity checks like resolve_address.
   */
  if (gconf_ping_daemon ())
    {
      retval = TRUE;
      goto out;
    }
  
  conffile = g_build_filename (GCONF_CONFDIR, "path", NULL);

  error = NULL;
  addresses = gconf_load_source_path (conffile, &error);

  if (addresses == NULL)
    {
      if (display_errors)
        show_fatal_error_dialog (_("Please contact your system administrator to resolve the following problem:\n"
                                   "No configuration sources in the configuration file \"%s\"; this means that preferences and other settings can't be saved. %s%s"),
                                 conffile,
                                 error ? _("Error reading the file: ") : "",
                                 error ? error->message : "");

      if (error)
        g_error_free (error);

      goto out;
    }

  tmp = addresses;
  while (tmp != NULL)
    {
      GConfSource *source;
      const char *address;

      address = tmp->data;
      
      error = NULL;      
      source = gconf_resolve_address (address, &error);

      if (error)
        {
          if (display_errors)
            show_fatal_error_dialog (_("Please contact your system administrator to resolve the following problem:\n"
                                       "Could not resolve the address \"%s\" in the configuration file \"%s\": %s"),
                                     address, conffile, error->message);
          g_error_free (error);
          goto out;
        }

      gconf_source_free (source);
      
      g_free (tmp->data);
      
      tmp = tmp->next;
    }

  g_slist_free (addresses);
  
  retval = TRUE;
  
 out:
  g_free (conffile);

  return retval;
}

static void
show_fatal_error_dialog (const char *format,
                         ...)
{
  GtkWidget *d;
  char *str;
  va_list args;

  va_start (args, format);
  str = g_strdup_vprintf (format, args);
  va_end (args);

  if (!ensure_gtk ())
    {
      g_printerr ("%s\n", str);
      return;
    }
  
  d = gtk_message_dialog_new (NULL, 0,
                              GTK_MESSAGE_ERROR,
                              GTK_BUTTONS_CLOSE,
                              "%s", str);

  g_free (str);
  
  gtk_dialog_run (GTK_DIALOG (d));

  gtk_widget_destroy (d);
}

static gboolean
offer_delete_locks (void)
{
  gboolean delete_locks;
  const char *question;

  delete_locks = FALSE;
  question = _("The files that contain your preference settings are "
               "currently in use.\n\n"
               "You might be logged in to a session from another computer, "
               "and the other login session is using your preference "
               "settings files.\n\n"
               "You can continue to use the current session, but this "
               "might cause temporary problems with the preference "
               "settings in the other session.\n\n" 
               "Do you want to continue?");
      
  if (ensure_gtk ())
    {
      GtkWidget *d;
      int response;
      
      d = gtk_message_dialog_new (NULL, 0,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_NONE,
                                  "%s", question);

      gtk_dialog_add_buttons (GTK_DIALOG (d),
                              _("_Log Out"),
                              GTK_RESPONSE_REJECT,
                              _("_Continue"),
                              GTK_RESPONSE_ACCEPT,
                              NULL);
      
      response = gtk_dialog_run (GTK_DIALOG (d));

      gtk_widget_destroy (d);

      if (response == GTK_RESPONSE_ACCEPT)
        delete_locks = TRUE;
    }
  else
    {
      g_print (_("%s Continue (y/n)?"), question);
      switch (getchar ())
        {
        case 'y':
        case 'Y':
          delete_locks = TRUE;
          break;
        }
    }

  if (delete_locks)
    {
      GSList* addresses;
      GSList* tmp;
      char *conffile;
      
      conffile = g_build_filename (GCONF_CONFDIR, "path", NULL);
      
      addresses = gconf_load_source_path (conffile, NULL);

      g_free (conffile);
      
      if (addresses == NULL)
        g_printerr ("Failed to load addresses to delete locks\n");

      tmp = addresses;
      while (tmp != NULL)
        {
          const char *address;
          
          address = tmp->data;
          
          gconf_blow_away_locks (address);

          g_free (tmp->data);
          
          tmp = tmp->next;
        }

      g_slist_free (addresses);
      
      return TRUE;
    }

  return FALSE;
}

/* this is because setting up gtk is kind of slow, no point doing it
 * if we don't need an error dialog.
 */
static gboolean
ensure_gtk (void)
{
  static gboolean done_init = FALSE;  
  static gboolean ok = FALSE;
  
  if (!done_init)
    {
      ok = gtk_init_check (NULL, NULL);
      done_init = TRUE;
    }
  
  return ok;
}
