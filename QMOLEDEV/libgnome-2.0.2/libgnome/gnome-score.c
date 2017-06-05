/*
 * Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation
 * All rights reserved.
 *
 * This file is part of the Gnome Library.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
/*
  @NOTATION@
 */

/*
 * gnome-score.c
 * originally by Elliot Lee, subsequently bashed around by Nathan Bryant
 */

#include <config.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <locale.h>
#ifdef HAVE_SYS_FSUID_H
#ifdef HAVE_SETFSGID
#include <sys/fsuid.h>
#endif
#endif

#include "gnome-i18nP.h"

#include "gnome-score.h"
#include "gnome-util.h"

#ifndef NSCORES
#define NSCORES 10
#endif

#define SCORE_PATH LIBGNOME_LOCALSTATEDIR "/games"

struct command
{
   gfloat score;
   int level;			/* length of level arg to gnome_score_log
				 * including null term */
   gboolean ordering;
};

struct ascore_t
{
   gchar *username;
   time_t scoretime;
   gfloat score;
};

static int outfd = -1, infd = -1;
static const gchar *defgamename;

/********************** internal functions ***********************************/

static gchar *
gnome_get_score_file_name (const gchar * progname, const gchar * level)
{
   if (level)
     return g_strconcat (SCORE_PATH "/",
			    progname, ".", level, ".scores", NULL);
   else
     return g_strconcat (SCORE_PATH "/", progname, ".scores", NULL);
}

/* This must be wrapped in push_c_locale on the caller */
static void
print_ascore (struct ascore_t *ascore, FILE * outfile)
{
	/* make sure we write values to files in a consistent manner */
	fprintf (outfile, "%f %ld %s\n", ascore->score,
		 (long int) ascore->scoretime, ascore->username);
}

static void
free_ascore(struct ascore_t *data)
{
   g_free(data->username);
   g_free(data);
}

/**** log_score
      Inputs: 'progname' - the program to log a score for
              'level' - the freeform level identifier
	      'username' - the username that this score is logged under
	      'score' - the game score
	      'ordering' - whether lower scores are better.
      Outputs: 'retval' - Whether the score got onto the highscore list

      Description: Loads all the existing scores into the 'scores'
                   list.  Goes through and finds out whether there's a
                   place for the new score on the high-score list, and
                   if so, inserts it. Writes out the new high-score list.
 */
static gint
log_score (const gchar * progname, const gchar * level, gchar * username,
	   gfloat score, gboolean ordering)
{
   FILE *infile;
   FILE *outfile;
   gchar buf[512], *buf2;
   GList *scores = NULL, *anode;
   gchar *game_score_file;
   gfloat ascore;
   struct ascore_t *anitem, *curscore;
   int i;
   gint retval = 1;
   gint pos;

   game_score_file = gnome_get_score_file_name (progname, level);

   infile = fopen (game_score_file, "r");
   if (infile)
     {
       /* make sure we read values from files in a consistent manner */
       gnome_i18n_push_c_numeric_locale ();
       while(fgets(buf, sizeof(buf), infile))
	 {
	     long ltime;
	     i = strlen (buf) - 1;	/* Chomp */
	     while (g_ascii_isspace (buf[i]))
	       buf[i--] = '\0';

  	     {
	        char *tokp;

	        if((buf2 = strtok_r (buf, " ", &tokp)) == NULL)
		  break;
	        ascore = atof (buf2);
	        if((buf2 = strtok_r (NULL, " ", &tokp)) == NULL)
		  break;
	        ltime = atoi (buf2);
	        if((buf2 = strtok_r (NULL, "\n", &tokp)) == NULL)
		  break;
	     }

	     anitem = g_new(struct ascore_t, 1);
	     anitem->score = ascore;
	     anitem->username = g_strdup (buf2);
	     anitem->scoretime = (time_t)ltime;
	     scores = g_list_append (scores, (gpointer) anitem);
	  }
        gnome_i18n_pop_c_numeric_locale ();

	fclose (infile);
     }
   
   anitem = g_new(struct ascore_t, 1);
   anitem->score = score;
   anitem->username = g_strdup (username);
   anitem->scoretime = time (NULL);

   for (pos = 0, anode = scores;
	pos < NSCORES && anode;
	pos++, anode = anode->next)
     {
	curscore = anode->data;
	if (ordering)
	  {
	     if (curscore->score < anitem->score)
	       break;
	  }
	else
	  {
	     if (curscore->score > anitem->score)
	       break;
	  }
     }
   
   if (pos < NSCORES)
     {
	scores = g_list_insert (scores, anitem, pos);
	if ((anode = g_list_nth (scores, NSCORES)))
	  {
	     free_ascore (anode->data);
	     scores =
	       g_list_remove_link (scores, g_list_nth (scores, NSCORES));
	  }
	retval = pos + 1;
     }
   else
     retval = 0;
   
   /* we dont create the file; it must already exist */
   truncate (game_score_file, 0);
   outfile = fopen (game_score_file, "r+");
   
   if (outfile)
     {
	gnome_i18n_push_c_numeric_locale ();
	g_list_foreach (scores, (GFunc) print_ascore, outfile);
	gnome_i18n_pop_c_numeric_locale ();
	fclose (outfile);
     }
   else
     perror (game_score_file);

   g_free (game_score_file);
   
   g_list_foreach (scores, (GFunc) free_ascore, NULL);
   g_list_free (scores);
   
   return retval;
}

static int
gnome_score_child (void)
{
   struct command cmd;
   gchar *level;
   gchar *realname;
   gint retval;
#ifdef HAVE_SETFSGID
   gid_t gid;
   
   gid = getegid ();
   setgid (getgid ());
   setfsgid (gid);
#endif
   realname = g_strdup (g_get_real_name ());
   if (strlen (realname) == 0)
     realname = g_strdup (g_get_user_name ());

   while (read (STDIN_FILENO, &cmd, sizeof cmd) == sizeof(cmd))
     {
	level = g_new (char, cmd.level);
	if (read (STDIN_FILENO, level, cmd.level) != cmd.level)
	  return EXIT_FAILURE;
	if (!*level) {
	   g_free(level);
	   level = NULL;
	}
	retval = log_score (defgamename, level, realname, cmd.score,
			    cmd.ordering);
	if (write(STDOUT_FILENO, &retval, sizeof retval) != sizeof retval)
	  return EXIT_FAILURE;
	if (level)
	  g_free(level);
     }
   if (realname)
     g_free (realname);
   return EXIT_SUCCESS;
}

static void 
drop_perms (void)
{
   gid_t gid = getegid ();
   
   setregid (getgid (), getgid ());	/* on some os'es (eg linux) this
					 * incantation will also drop the
					 * saved gid */
   /* see if we can set it back -- if we can, saved id wasnt dropped */
   if (gid != getgid() && !setgid (gid))
     {
	if (getuid())
	  g_warning ("losing saved gid implementation detected, "
		     "get a real OS :)\n");
	setgid (getgid ());
     }
}

/*********************** external functions **********************************/

/**
 * gnome_score_init:
 * @gamename: Identifies the game name.
 *
 * GNOME games should call this routine as the first statement
 * in main() if they have been installed setgid to the "games" group. It
 * performs the intialization required to later record information in the
 * scores table and then drops the groups privileges.
 * 
 * Returns: %0 on success, returns %-1 on failure.
 */

gint
gnome_score_init (const gchar * gamename)
{
   int inpipe[2], outpipe[2];
   
   /*
    *creates a child process with which we communicate through a pair of pipes,
    * then drops privileges.
    */
   if (!gamename)
     gamename = "";
   if (!(defgamename = g_strdup (gamename)) ||
       pipe(inpipe))
     {
	drop_perms();
	return -1;
     }
   if (pipe (outpipe))
     {
	close (inpipe[0]);
	close (inpipe[1]);
	drop_perms ();
	return -1;
     }
   outfd = outpipe[1];
   infd = inpipe[0];
   switch (fork ())
     {
      case 0:
	if (dup2 (outpipe[0], STDIN_FILENO) == -1 ||
	    dup2 (inpipe[1], STDOUT_FILENO) == -1)
	  exit (EXIT_FAILURE);
	close(inpipe[0]);
	close(inpipe[1]);
	close(outpipe[0]);
	close(outpipe[1]);
	exit (gnome_score_child ());
      case -1:
	close (inpipe[0]);
	close (inpipe[1]);
	close (outpipe[0]);
	close (outpipe[1]);
	infd = outfd = -1;
	drop_perms ();
	return -1;
     }
   close(outpipe[0]);
   close(inpipe[1]);
   drop_perms ();
   return 0;
}

/**
 * gnome_score_log:
 * @score: The score achieved by the user in this game
 * @level: The level on which the score was obtained
 * @higher_to_lower_score_order: Set to %TRUE if high scores are better than
 * low scores.
 *
 * Logs a score entry for the user. You should call this every time a game
 * ends.  This function takes care of working out whether the user's score made
 * it into the ten best scores and, if so, records it in the table.
 *
 * Returns: %0 on failure or the status from the gnome-score helper
 * program.
 */
gint
gnome_score_log (gfloat score,
		 const gchar * level,
		 gboolean higher_to_lower_score_order)
{
   struct command cmd;
   gint retval;
   
   if (getgid () != getegid ())
     {
	g_error ("gnome_score_init must be called first thing in main()\n");
	abort ();
     }
   if (infd == -1 || outfd == -1)
     return 0;
   
   cmd.score = score;
   if (!level)
     level = "";
   cmd.level = strlen (level) + 1;
   cmd.ordering = higher_to_lower_score_order;
   
   if (write (outfd, &cmd, sizeof cmd) != sizeof(cmd) ||
       write (outfd, level, cmd.level) != cmd.level ||
       read (infd, &retval, sizeof retval) != sizeof(retval))
     {
	close (outfd);
	close (infd);
	infd = outfd = -1;
	return 0;
     }
   return retval;
}

/**
 * gnome_score_get_notable:
 * @gamename:   The name of the game we want to fetch information from.
 * @level:      The level for which we want to pull information.
 * @names:      An array of strings is returned at the address pointed here
 * @scores:     An array of gfloats is returned at the address pointed here
 * @scoretimes: An array of time_t is returned at the address pointed here
 *
 * Fetches the most notable players on @gamename at level @level.
 *
 * Returns: The number of scores returned.  The @names, @scores and @scoretime
 * pointers point to regions that were allocated with g_malloc() with the
 * contents.
 */
gint
gnome_score_get_notable (const gchar * gamename,
			 const gchar * level,
			 gchar *** names,
			 gfloat ** scores,
			 time_t ** scoretimes)
{
   const gchar *realname;
   gchar buf[512], *buf2;
   gchar *infile_name;
   FILE *infile;
   gint retval;
   
   g_return_val_if_fail (names != NULL, 0);
   g_return_val_if_fail (scores != NULL, 0);
   
   if (gamename == NULL)
     realname = defgamename;
   else
     realname = gamename;
   
   infile_name = gnome_get_score_file_name (realname, level);
   
   infile = fopen (infile_name, "r");
   g_free (infile_name);
   
   if (infile)
     {
	*names = g_malloc ((NSCORES + 1) * sizeof (gchar *));
	*scores = g_malloc ((NSCORES + 1) * sizeof (gfloat));
	*scoretimes = g_malloc ((NSCORES + 1) * sizeof (time_t));
	
        gnome_i18n_push_c_numeric_locale ();

	for (retval = 0;
	     fgets (buf, sizeof (buf), infile) && retval < NSCORES;
	     retval++)
	  {
	     char *tokp;

	     buf[strlen (buf) - 1] = 0;
	     buf2 = strtok_r (buf, " ", &tokp);
	     (*scores)[retval] = atof (buf2);
	     buf2 = strtok_r (NULL, " ", &tokp);
	     (*scoretimes)[retval] = atoi (buf2);
	     buf2 = strtok_r (NULL, "\n", &tokp);
	     (*names)[retval] = g_strdup (buf2);
	  }
	(*names)[retval] = NULL;
	(*scores)[retval] = 0.0;

        gnome_i18n_pop_c_numeric_locale ();

	fclose (infile);
     }
   else
     {
	*names = NULL;
	*scores = NULL;
	retval = 0;
     }
   return retval;
}
