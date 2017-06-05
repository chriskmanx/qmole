/*
 * Copyright (C) 1997, 1998 Elliot Lee
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

#include "config.h"

/* By Elliot Lee */

#include "gnome-triggers.h"
#include "gnome-triggersP.h"
#include "gnome-config.h"
#include "gnome-util.h"
#include "gnome-sound.h"
#ifdef HAVE_ESD
 #include <esd.h>
#endif

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <glib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>

/* TYPE DECLARATIONS */

typedef void (*GnomeTriggerTypeFunction)(GnomeTrigger *t, char *msg, char *level, char *supinfo[]);

/* PROTOTYPES */
static GnomeTrigger* gnome_trigger_dup(GnomeTrigger *dupme);
static GnomeTriggerList* gnome_triggerlist_new(char *nodename);
static void gnome_triggerlist_free(GnomeTriggerList* t);
static void gnome_trigger_free(GnomeTrigger* t);
static void gnome_trigger_do(GnomeTrigger* t, const char *msg, const char *level,
			     const char *supinfo[]);
static void gnome_trigger_do_function(GnomeTrigger* t,
				      const char *msg, const char *level,
				      const char *supinfo[]);
static void gnome_trigger_do_command(GnomeTrigger* t,
				     const char *msg, const char *level,
				     const char *supinfo[]);
static void gnome_trigger_do_mediaplay(GnomeTrigger* t,
				       const char *msg,
				       const char *level,
				       const char *supinfo[]);

/* FILEWIDE VARIABLES */

static GnomeTriggerList* gnome_triggerlist_topnode = NULL;

static const GnomeTriggerTypeFunction actiontypes[] =
/* This list should have entries for all the trigger types in
   gnome-triggers.h */
{
  (GnomeTriggerTypeFunction)NULL,
  (GnomeTriggerTypeFunction)gnome_trigger_do_function,
  (GnomeTriggerTypeFunction)gnome_trigger_do_command,
  (GnomeTriggerTypeFunction)gnome_trigger_do_mediaplay,
  (GnomeTriggerTypeFunction)NULL
};

/* IMPLEMENTATIONS */
#if 0
void
gnome_triggers_init(void)
{
}
#endif

#if 0
/* snarfed almost directly from sound-properties. */
static gint
gnome_triggers_read_path(const char *config_path)
{
  DIR *dirh;
  char *sample_name, *sample_file, *ctmp;
  gpointer event_iter;
  struct dirent *dent;
  GnomeTrigger nt;
  GString *tmpstr;

  nt.type = GTRIG_MEDIAPLAY;
  nt.level = NULL;
  nt.u.media.cache_id = -1;

  dirh = opendir(config_path);
  if(!dirh)
    return -1;

  tmpstr = g_string_new(NULL);

  while((dent = readdir(dirh))) {
    /* ignore no-good dir entries.
       We ignore "gnome" because the system sounds are listed in there.
    */
    if (!strcmp(dent->d_name, ".")
	|| !strcmp(dent->d_name, "..")
	|| !strcmp(dent->d_name, "gnome")
	|| !strcmp(dent->d_name, "gnome.soundlist"))
      continue;

    g_string_printf(tmpstr, "=%s/%s=", config_path, dent->d_name);

    gnome_config_push_prefix(tmpstr->str);

    event_iter = gnome_config_init_iterator_sections(tmpstr->str);
    while((event_iter = gnome_config_iterator_next(event_iter,
						   &sample_name, NULL))) {
      if(!strcmp(sample_name, "__section_info__"))
	goto continue_loop;

      g_string_printf(tmpstr, "%s/file", sample_name);
      sample_file = gnome_config_get_string(tmpstr->str);

      if(!sample_file || !*sample_file) {
	g_free(sample_name);
	continue;
      }

      if(*sample_file != '/') {
	char *tmp = gnome_sound_file(sample_file);
	g_free(sample_file);
	sample_file = tmp;
      }

      ctmp = g_strdup(dent->d_name);
      if(strstr(ctmp, ".soundlist"))
	*strstr(ctmp, ".soundlist") = '\0';

      nt.u.media.file = sample_file;
      gnome_triggers_add_trigger(&nt, ctmp, sample_name, NULL);
      g_print ("Added: %s\n", ctmp);
      
      g_free(ctmp);

    continue_loop:
      g_free(sample_name);
    }

    gnome_config_pop_prefix();
  }
  closedir(dirh);

  g_string_free(tmpstr, TRUE);

  return 0;
}
/**
 * gnome_triggers_readfile:
 * @infilename: A file listing triggers to install in the currently
 * running program.
 *
 * The file should be of the format:
 *
 *    level section type params
 *
 * Where 'level' indicates the message severity at which this trigger
 * should be activated, 'section' is a colon-separated list indicating
 * which part of the "message classification tree" this trigger will
 * be activated for, 'type' is either "command" (run the command
 * specified in 'params') or 'play' (play the esd sound sample named
 * 'params').
 *
 * Returns 0 on success.  1 otherwise.
 *
 */
static gint
gnome_triggers_readfile(const char *infilename)
{
  GnomeTrigger* nt;
  char aline[512];
  char **subnames = NULL;
  char **parts = NULL;
  FILE *infile;
  int i;

  infile = fopen(infilename, "r");
  if(infile == NULL)
    return 1;

  nt = gnome_trigger_dup(NULL);
  while(fgets(aline, sizeof(aline), infile)) {
    i = strlen(aline) - 1;
    while(isspace(aline[i])) aline[i--] = '\0';

    if(aline[0] == '\0' || aline[0] == '#')
      continue;

    parts = g_strsplit(aline, " ", 4);
    if(!parts || !parts[0] || !parts[1] || !parts[2] || !parts[3]) {
      g_strfreev(parts);
      g_warning("Invalid triggers line \'%s\'\n", aline);
      continue;
    }

    if(!strcmp(parts[1], "NULL")) {
      subnames = g_malloc(sizeof(gchar *));
      subnames[0] = NULL;
    } else
      subnames = g_strsplit(parts[1], ":", -1);

    if(!strcmp(parts[2], "command"))
      nt->type = GTRIG_COMMAND;
    else if(!strcmp(parts[2], "play"))
      nt->type = GTRIG_MEDIAPLAY;
    nt->u.command = parts[3];
    if(!strcmp(parts[0], "NULL"))
      nt->level = NULL;
    else
      nt->level = parts[0];
    gnome_triggers_vadd_trigger(nt, subnames);

    g_strfreev(subnames);
    g_strfreev(parts);
  }
  g_free(nt);
  fclose(infile);

  return 0;
}
#endif

/**
 * gnome_triggers_add_trigger:
 * @nt: Information on the new trigger to be added.
 * @...: The section to add the trigger under.
 *
 * Adds a new #GnomeTrigger instance to the event hierarchy.
 */
void gnome_triggers_add_trigger(GnomeTrigger* nt, ...)
{
  va_list l;
  gint nstrings, i;
  gchar **strings;
  
  /* Count number of strings */
  
  va_start(l, nt);
  for (nstrings = 0; va_arg(l, gchar *); nstrings++);
  va_end(l);
  
  /* Build list */
  
  strings = g_new(gchar *, nstrings + 1);
  
  va_start(l, nt);
  
  for (i = 0; i < nstrings; i++)
    strings[i] = va_arg(l, gchar *);
  strings[i] = NULL;
  
  va_end(l);
  
  /* And pass them to the real function */
  
  gnome_triggers_vadd_trigger(nt, strings);

  g_free (strings);
}

static GnomeTrigger*
gnome_trigger_dup(GnomeTrigger* dupme)
{
  GnomeTrigger* retval;
  retval = g_malloc(sizeof(struct _GnomeTrigger));
  if(dupme) {
    *retval = *dupme;
    if(dupme->level)
      retval->level = g_strdup(dupme->level);
    else
      retval->level = NULL;
    switch(retval->type) {
    case GTRIG_COMMAND:
      retval->u.command = g_strdup(dupme->u.command);
      break;
    default:
      break;
    }
  } else {
    retval->level = NULL;
    retval->type = GTRIG_NONE;
    memset(&retval->u, 0, sizeof(retval->u));
  }
  return retval;
}

static GnomeTriggerList*
gnome_triggerlist_new(char *nodename)
{
  GnomeTriggerList* retval;
  retval = g_malloc0(sizeof(GnomeTriggerList));
  retval->nodename = g_strdup(nodename);
  return retval;
}

/**
 * gnome_triggers_vadd_trigger:
 * @nt: Information on the new trigger to be added.
 * @supinfo: The section to add the trigger under.
 *
 * This does the same as gnome_triggers_add_trigger(), except the section is
 * stored in the %NULL terminated array @supinfo instead of as a variable
 * length argument list.
 */
void gnome_triggers_vadd_trigger(GnomeTrigger* nt,
				 char *supinfo[])
{
  g_return_if_fail(nt != NULL);
  if(!gnome_triggerlist_topnode)
    gnome_triggerlist_topnode = gnome_triggerlist_new(NULL);

  if(supinfo == NULL || supinfo[0] == NULL) {
    gnome_triggerlist_topnode->actions = g_realloc(gnome_triggerlist_topnode->actions, ++gnome_triggerlist_topnode->numactions);
    gnome_triggerlist_topnode->actions[gnome_triggerlist_topnode->numactions - 1] = gnome_trigger_dup(nt);
  } else {
    int i, j;
    GnomeTriggerList* curnode;

    for(i = 0, curnode = gnome_triggerlist_topnode;
	supinfo[i]; i++) {
      for(j = 0;
	  j < curnode->numsubtrees
	    && strcmp(curnode->subtrees[j]->nodename, supinfo[i]);
	  j++) /* Do nothing */ ;

      if(j < curnode->numsubtrees) {
	curnode = curnode->subtrees[j];
      } else {
	curnode->subtrees = g_realloc(curnode->subtrees,
				      ++curnode->numsubtrees
				      * sizeof(GnomeTriggerList*));
	curnode->subtrees[curnode->numsubtrees - 1] =
	  gnome_triggerlist_new(supinfo[i]);
	curnode = curnode->subtrees[curnode->numsubtrees - 1];
      } /* end for j */
    } /* end for i */

    curnode->actions = g_realloc(curnode->actions,
				 ++curnode->numactions
				 * sizeof(GnomeTrigger));
    curnode->actions[curnode->numactions - 1] = gnome_trigger_dup(nt);
  } /* end if */
}

/**
 * gnome_triggers_do:
 * @msg: The human-readable message describing the event (can be %NULL).
 * @level: The level of severity of the event, or %NULL.
 * @...: The classification of the event.
 *
 * Notifies GNOME about an event happening, so that any appropriate handlers
 * can be run.
 */
void
gnome_triggers_do(const char *msg, const char *level, ...)
{
  va_list l;
  gint nstrings, i;
  gchar **strings;
  
  /* Count number of strings */
  va_start(l, level);
  for (nstrings = 0; va_arg(l, gchar *); nstrings++);
  va_end(l);
  
  /* Build list */
  
  strings = g_new (gchar *, nstrings + 1);
  
  va_start(l, level);
  
  for (i = 0; i < nstrings; i++)
    strings[i] = va_arg(l, gchar *);
  strings[i] = NULL;
  
  va_end(l);
  
  /* And pass them to the real function */
  
  gnome_triggers_vdo(msg, level, (const char **)strings);

  g_free (strings);
}

/* The "add one to the sample ID" is because sample ID's start at 0,
   and we need a way to distinguish between "not found in sound_ids"
   and "sample #0" */
static void
gnome_triggers_play_sound(const char *sndname)
{
#ifdef HAVE_ESD
  int sid;
  static GHashTable *sound_ids = NULL;

  if(gnome_sound_connection_get () < 0) return;

  if(!sound_ids)
    sound_ids = g_hash_table_new(g_str_hash, g_str_equal);

  sid = GPOINTER_TO_INT(g_hash_table_lookup(sound_ids, sndname));

  if(!sid) {
    sid = esd_sample_getid(gnome_sound_connection_get (), sndname);
    if(sid >= 0) sid++;
    g_hash_table_insert(sound_ids, g_strdup(sndname), GINT_TO_POINTER(sid));
  }

  if(sid < 0) return;
  sid--;
  esd_sample_play(gnome_sound_connection_get (), sid);
#endif
  /* If there's no esound, this is just a no-op */
}

/**
 * gnome_triggers_vdo:
 * @msg: The human-readable message describing the event (can be %NULL).
 * @level: The level of severity of the event, or %NULL.
 * @supinfo: The classification of the event (%NULL terminated array).
 *
 * Notifies GNOME about an event happening, so that any appropriate handlers
 * can be run. This does the same as gnome_trigger_do() except that it takes a
 * %NULL terminated array instead of a varargs list.
 */
void
gnome_triggers_vdo(const char *msg, const char *level, const char *supinfo[])
{
  GnomeTriggerList* curnode = gnome_triggerlist_topnode;
  int i, j;
  char buf[256], *ctmp;

  if(level) {
    g_snprintf(buf, sizeof(buf), "gnome/%s", level);
    gnome_triggers_play_sound(buf);
  }

  if(!supinfo)
    return;

  ctmp = g_strjoinv("/", (char **)supinfo);
  gnome_triggers_play_sound(ctmp);
  g_free(ctmp);

  for(i = 0; curnode && supinfo[i]; i++)
    {

    for(j = 0; j < curnode->numactions; j++)
      {
	if(!curnode->actions[j]->level
	   || !level
	   || !strcmp(level, curnode->actions[j]->level))
	  gnome_trigger_do(curnode->actions[j], msg, level, supinfo);
      }
    
    for(j = 0;
	j < curnode->numsubtrees
	  && strcmp(curnode->subtrees[j]->nodename,supinfo[i]);
	j++)
      /* Do nothing */ ;
    if(j < curnode->numsubtrees)
      curnode = curnode->subtrees[j];
    else
      curnode = NULL;
  }
  if(curnode)
    {
      for(j = 0; j < curnode->numactions; j++)
	{
	  if(!curnode->actions[j]->level
	     || !level
	     || !strcmp(level, curnode->actions[j]->level))
	    gnome_trigger_do(curnode->actions[j], msg, level, supinfo);
	}
    }
}

static void
gnome_trigger_free(GnomeTrigger* t)
{
  if(t->level)
    g_free(t->level);
  switch(t->type) {
  case GTRIG_COMMAND:
    g_free(t->u.command); break;
  case GTRIG_MEDIAPLAY:
    g_free(t->u.media.file); break;
  default:
    break;
  }
  g_free(t);
}

static void
gnome_triggerlist_free(GnomeTriggerList* t)
{
  int i;

  g_free(t->nodename);

  for(i = 0; i < t->numsubtrees; i++) {
    gnome_triggerlist_free(t->subtrees[i]);
  }
  g_free(t->subtrees);

  for(i = 0; i < t->numactions; i++) {
    gnome_trigger_free(t->actions[i]);
  }
  g_free(t->actions);

  g_free(t);
}

static void
gnome_trigger_do(GnomeTrigger* t,
		 const char *msg,
		 const char * level,
		 const char *supinfo[])
{
  g_return_if_fail(t != NULL);

  actiontypes[t->type](t, (char *)msg, (char *)level, (char **)supinfo);
}

static void
gnome_trigger_do_function(GnomeTrigger* t,
			  const char *msg,
			  const char *level,
			  const char *supinfo[])
{
  t->u.function((char *)msg, (char *)level, (char **)supinfo);
}

static void
gnome_trigger_do_command(GnomeTrigger* t,
			 const char *msg,
			 const char *level,
			 const char *supinfo[])
{
  char **argv;
  int nsupinfos, i;

  for(nsupinfos = 0; supinfo[nsupinfos]; nsupinfos++);

  argv = g_malloc(sizeof(char *) * (nsupinfos + 4));
  argv[0] = (char *)t->u.command;
  argv[1] = (char *)msg;
  argv[2] = (char *)level;

  for(i = 0; supinfo[i]; i++) {
    argv[i + 3] = (char *)supinfo[i];
  }
  argv[i + 3] = NULL;

  /* We're all set, let's do it */
  {
    pid_t childpid;
    int status;
    childpid = fork();
    if(childpid)
      waitpid(childpid, &status, 0);
    else
      execv(t->u.command, argv);
  }
  
  g_free(argv);
}

static void
gnome_trigger_do_mediaplay(GnomeTrigger* t,
			   const char *msg,
			   const char *level,
			   const char *supinfo[])
{
#if defined(HAVE_ESD)
  if(gnome_sound_connection_get () == -1)
    return;

  if(t->u.media.cache_id >= 0)
    esd_sample_play(gnome_sound_connection_get (), t->u.media.cache_id);
  else if(t->u.media.cache_id == -1)
    gnome_sound_play(t->u.media.file);
#endif
}

