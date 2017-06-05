/*
 * Configuration-File Functions.
 *
 *  Copyright 1993, 1994, 1997 The Free Software Foundation
 *
 * Authors: Miguel de Icaza

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License
   as published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.
   
   The Gnome Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */
/*
  @NOTATION@
 */

#include <config.h>
#include <glib.h>

#include <string.h>
#include <stdio.h>
#include <time.h>
#include <string.h>
#include <unistd.h>	/* unlink() */
#include <stdlib.h>	/* atoi() */
#include <sys/types.h>
#include <sys/stat.h>

#include "gnome-i18nP.h"
#include "gnome-util.h"
#include "gnome-config.h"


#if !defined getc_unlocked && !defined HAVE_GETC_UNLOCKED
# define getc_unlocked(fp) getc (fp)
#endif

#define STRSIZE 4096
#define overflow (next == &CharBuffer [STRSIZE-1])

enum {
	FirstBrace,
	OnSecHeader,
	IgnoreToEOL,
	IgnoreToEOLFirst,
	KeyDef,
	KeyDefOnKey,
	KeyValue
};

typedef struct {
	int type;
	void *value;
} iterator_type;

typedef enum {
	LOOKUP,
	SET
} access_type;

typedef struct TKeys {
	char *key_name;
	char *value;
	struct TKeys *link;
} TKeys;

typedef struct TSecHeader {
	char *section_name;
	TKeys *keys;
	struct TSecHeader *link;
} TSecHeader;

typedef struct TProfile {
	char *filename;
	TSecHeader *section;
	struct TProfile *link;
	time_t last_checked;
	time_t mtime;
	gboolean written_to;
	gboolean to_be_deleted;
} TProfile;

/*
 * Prefix for all the configuration operations
 * iff the path does not begin with / or with #
 */

#define prefix (prefix_list ? prefix_list->data : NULL)

static GSList *prefix_list = NULL;

static TProfile *Current = 0;

/*
 * This one keeps track of all of the opened files
 */
static TProfile *Base = 0;

static char *
config_concat_dir_and_key (const char *dir, const char *key)
{
	g_return_val_if_fail (dir != NULL, NULL);
	g_return_val_if_fail (key != NULL, NULL);

        /* If the directory name doesn't have a / on the end, we need
	   to add one so we get a proper path to the file */
	if (dir[0] != '\0' && dir [strlen(dir) - 1] != '/')
		return g_strconcat (dir, "/", key, NULL);
	else
		return g_strconcat (dir, key, NULL);
}

/* The `release_path' and `parsed_path' routines are inside the
   following file.  It is in a separate file to allow the test-suite
   to get at it, without needing to export special symbols.

   typedef struct {
	char *file, *section, *key, *def;
	char *path, *opath;
   } ParsedPath;

   static void release_path (ParsedPath *p);
   static ParsedPath *parse_path (const char *path, gboolean priv); */
#include "parse-path.cP"

static void 
free_keys (TKeys *p)
{
	if (!p)
		return;
	free_keys (p->link);
	g_free (p->key_name);
	g_free (p->value);
	g_free (p);
}

static void 
free_sections (TSecHeader *p)
{
	if (!p)
		return;
	free_sections (p->link);
	free_keys (p->keys);
	g_free (p->section_name);
	p->link = 0;
	p->keys = 0;
	g_free (p);
}

static void 
free_profile (TProfile *p)
{
	if (!p)
		return;
	if(Current == p)
		Current = NULL;
	free_profile (p->link);
	free_sections (p->section);
	g_free (p->filename);
	g_free (p);
}

static int 
is_loaded (const char *filename, TSecHeader **section)
{
	TProfile *p = Base;
	TProfile *lastp = NULL;
	struct stat st;
	
	/*
	 * if the last one we accessed was this one we don't want to
	 * search
	 */
	if (Current && strcasecmp (filename, Current->filename) == 0){
		if (Current->last_checked != time (NULL)){
			if (stat (filename, &st) == -1)
				st.st_mtime = 0;
			if (Current->mtime != st.st_mtime) {
				free_sections (Current->section);
				Current->section = NULL;
				Current->filename[0] = '\0';
				Current->written_to = TRUE;
				Current->to_be_deleted = FALSE;
				Current = NULL;
				return 0;
			}
			Current->last_checked = time (NULL);
		}
		*section = Current->section;
		return 1;
	}
	
	while (p){
		/*search and destroy empty nodes*/
		if (p->filename[0]=='\0') {
			TProfile *next = p->link;
			if(lastp)
				lastp->link = next;
			else /*the next one is the first one actually*/
				Base = next;
			g_free(p->filename);
			g_free(p);
			p = next;
		} else if (strcasecmp (filename, p->filename) == 0){
			if (p->last_checked != time (NULL)){
				if (stat (filename, &st) == -1)
					st.st_mtime = 0;
				if (p->mtime != st.st_mtime) {
					if(p == Current)
						Current = NULL;
					free_sections (p->section);
					p->section = NULL;
					p->filename[0] = '\0';
					p->written_to = TRUE;
					p->to_be_deleted = FALSE;
					return 0;
				}
				p->last_checked = time (NULL);
			}
			Current = p;
			*section = p->section;
			return 1;
		} else {
			lastp = p;
			p = p->link;
		}
	}
	return 0;
}

static char *
decode_string_and_dup (char *s)
{
	char *p = g_malloc (strlen (s) + 1);
	char *q = p;

	do {
		if (*s == '\\'){
			switch (*(++s)){
			case 'n':
				*p++ = '\n';
				break;
			case '\\':
				*p++ = '\\';
				break;
			case 'r':
				*p++ = '\r';
				break;
			default:
				*p++ = '\\';
				*p++ = *s;
			}
		} else
			*p++ = *s;
	} while (*s++);
	return q;
}

static char *
escape_string_and_dup (char *s)
{
	char *return_value, *p = s;
	int len = 0;

	if(!s)
		return g_strdup("");
	
	while (*p){
		len++;
		if (*p == '\n' || *p == '\\' || *p == '\r' || *p == '\0')
			len++;
		p++;
	}
	return_value = p = (char *) g_malloc (len + 1);
	if (!return_value)
		return 0;
	do {
		switch (*s){
		case '\n':
			*p++ = '\\';
			*p++ = 'n';
			break;
		case '\r':
			*p++ = '\\';
			*p++ = 'r';
			break;
		case '\\':
			*p++ = '\\';
			*p++ = '\\';
			break;
		default:
			*p++ = *s;
		}
	} while (*s++);
	return return_value;
}

static TSecHeader *
load (const char *file)
{
	FILE *f;
	int state;
	TSecHeader *SecHeader = 0;
	char CharBuffer [STRSIZE];
	char *next = "";		/* Not needed */
	int c;
	
	if ((f = fopen (file, "r"))==NULL)
		return NULL;
	
	state = FirstBrace;
	while ((c = getc_unlocked (f)) != EOF){
		if (c == '\r')		/* Ignore Carriage Return */
			continue;
		
		switch (state){
			
		case OnSecHeader:
			if (c == ']' || overflow){
				*next = '\0';
				next = CharBuffer;
				SecHeader->section_name = g_strdup (CharBuffer);
				state = IgnoreToEOL;
			} else
				*next++ = c;
			break;

		case IgnoreToEOL:
		case IgnoreToEOLFirst:
			if (c == '\n'){
				if (state == IgnoreToEOLFirst)
					state = FirstBrace;
				else
					state = KeyDef;
				next = CharBuffer;
			}
			break;

		case FirstBrace:
		case KeyDef:
		case KeyDefOnKey:
			if (c == '#') {
				if (state == FirstBrace)
					state = IgnoreToEOLFirst;
				else
					state = IgnoreToEOL;
				break;
			}

			if (c == '[' && state != KeyDefOnKey){
				TSecHeader *temp;
		
				temp = SecHeader;
				SecHeader = (TSecHeader *) g_malloc (sizeof (TSecHeader));
				SecHeader->link = temp;
				SecHeader->keys = 0;
				state = OnSecHeader;
				next = CharBuffer;
				break;
			}
			/* On first pass, don't allow dangling keys */
			if (state == FirstBrace)
				break;
	    
			if ((c == ' ' && state != KeyDefOnKey) || c == '\t')
				break;
	    
			if (c == '\n' || overflow) { /* Abort Definition */
				next = CharBuffer;
				state = KeyDef;
                                break;
                        }
	    
			if (c == '=' || overflow){
				TKeys *temp;

				temp = SecHeader->keys;
				*next = '\0';
				SecHeader->keys = (TKeys *) g_malloc (sizeof (TKeys));
				SecHeader->keys->link = temp;
				SecHeader->keys->key_name = g_strdup (CharBuffer);
				state = KeyValue;
				next = CharBuffer;
			} else {
				*next++ = c;
				state = KeyDefOnKey;
			}
			break;

		case KeyValue:
			if (overflow || c == '\n'){
				*next = '\0';
				SecHeader->keys->value = decode_string_and_dup (CharBuffer);
				state = c == '\n' ? KeyDef : IgnoreToEOL;
				next = CharBuffer;
#ifdef GNOME_ENABLE_DEBUG
#endif
			} else
				*next++ = c;
			break;
	    
		} /* switch */
	
	} /* while ((c = getc_unlocked (f)) != EOF) */
	if (c == EOF && state == KeyValue){
		*next = '\0';
		SecHeader->keys->value = decode_string_and_dup (CharBuffer);
	}
	fclose (f);
	return SecHeader;
}

static void 
new_key (TSecHeader *section, const char *key_name, const char *value)
{
	TKeys *key;
    
	key = (TKeys *) g_malloc (sizeof (TKeys));
	key->key_name = g_strdup (key_name);
	key->value   = g_strdup (value);
	key->link = section->keys;
	section->keys = key;
}

static const char *
access_config (access_type mode, const char *section_name,
	       const char *key_name, const char *def, const char *filename,
	       gboolean *def_used)
{
    
	TProfile   *New;
	TSecHeader *section;
	TKeys      *key;

	if (def_used)
		*def_used = FALSE;
	if (!is_loaded (filename, &section)){
		struct stat st;
		
		if (stat (filename, &st) == -1)
			st.st_mtime = 0;

		New = (TProfile *) g_malloc (sizeof (TProfile));
		New->link = Base;
		New->filename = g_strdup (filename);
		New->section = load (filename);
		New->mtime = st.st_mtime;
		New->written_to = FALSE;
		New->to_be_deleted = FALSE;
		New->last_checked = time (NULL);
		Base = New;
		section = New->section;
		Current = New;
	}
    
	/* Start search */
	for (; section; section = section->link){
		/*if section name empty or deleted or not the one we're
		  looking for, then search on*/
		if (!section->section_name ||
		    !*section->section_name ||
		    strcasecmp (section->section_name, section_name))
			continue;
		
		for (key = section->keys; key; key = key->link){
			if (strcasecmp (key->key_name, key_name))
				continue;
			if (mode == SET){
				g_free (key->value);
				key->value = g_strdup (def);
				Current->written_to = TRUE;
			}
			return key->value;
		}

		/* No key found */
		if (mode == SET){
			new_key (section, key_name, def);
			Current->written_to = TRUE;
			return 0;
		}
	}
    
	/* Non existent section */
	if ((mode == SET) && def){
		section = (TSecHeader *) g_malloc (sizeof (TSecHeader));
		section->section_name = g_strdup (section_name);
		section->keys = 0;
		new_key (section, key_name, def);
		section->link = Current->section;
		Current->section = section;
		Current->written_to = TRUE;
	} 
	if (def_used)
		*def_used = TRUE;
	return def;
}

/* an extended version of access_config for looking up values in ~/.gnome2.
 * For writes it falls through to the standard behaviour.
 * For lookups, it first checks for the value in
 * $(datadir)/gnome/config-override, and if it isn't there in ~/.gnome2,
 * then checks $(datadir)/gnome/config, and as a last fallback uses def.
 * This gives system administrators high level control over the default
 * configuration values for GNOME
 *
 * Note that it doesn't really make sense to have system wide defaults for
 * ~/.gnome_private data (IMHO), so I haven't addressed it in this
 * interface. It probably isn't suitable for absolute config file names */
static const char *
access_config_extended (access_type mode, const char *section_name,
			const char *key_name, const char *def,
			const char *rel_file, gboolean *def_used)
{
	char *tmp, *filename;
	const char *ret_val;
	gboolean internal_def;

	static time_t cache_time = 0;
	static char *cache_filename = NULL;
	static char *cache_overrride_filename = NULL;
	static char *cache_global_filename = NULL;
	gboolean cache_valid;
	time_t now;

	switch (mode) {
	case SET:
		/* fall through to normal behaviour */
		filename = gnome_util_home_file (rel_file);
		ret_val = access_config (mode, section_name, key_name, def,
					 filename, def_used);
		g_free(filename);
 		cache_time = 0;  /* Invalidate cache.  */
		return ret_val;
	case LOOKUP:
 		now = time (NULL);
 		cache_valid = (cache_filename &&
 			       strcmp (cache_filename, rel_file) == 0 &&
 			       now - cache_time <= 2);
 		if (!cache_valid) {
 			if (cache_filename) 
				g_free (cache_filename);

 			cache_filename = g_strdup (rel_file);
 			cache_time = now;

 			if (cache_overrride_filename)
				g_free (cache_overrride_filename);

 			tmp = config_concat_dir_and_key ("gnome/config-override",rel_file);
 			filename = gnome_program_locate_file
			    (gnome_program_get (), GNOME_FILE_DOMAIN_CONFIG,
			     tmp, TRUE, NULL);
 			g_free (tmp);
 			cache_overrride_filename = filename ? g_strdup (filename) : NULL;
			
 			if (cache_global_filename)
				g_free (cache_global_filename);

			tmp = config_concat_dir_and_key ("gnome/config", rel_file);
 			filename = gnome_program_locate_file
			    (gnome_program_get (), GNOME_FILE_DOMAIN_CONFIG,
			     tmp, TRUE, NULL);
 			g_free (tmp);
			cache_global_filename = filename ? g_strdup (filename) : NULL;
 		}

		if (cache_overrride_filename) {
			/* the required config file exists */
			ret_val = access_config (mode, section_name, key_name,
						 NULL,
						 cache_overrride_filename,
						 &internal_def);
			if (!internal_def) {
				if (def_used)
					*def_used = FALSE;
				return ret_val;
			}
			g_assert (ret_val == NULL);
		}

		/* fall through to the user config section */
		filename = gnome_util_home_file (rel_file);
		ret_val = access_config (mode, section_name, key_name, NULL,
					 filename, &internal_def);
		g_free (filename);
		if (!internal_def) {
			if (def_used) 
				*def_used = FALSE;
			return ret_val;
		}
		g_assert (ret_val == NULL);

		/* fall through to the system wide config default tree */
		if (cache_global_filename) {
			/* the file exists */
			ret_val = access_config (mode, section_name, key_name,
						 def,
						 cache_global_filename,
						 def_used);
			return ret_val;
		} else {
			/* it doesn't -- use the default value */
			if (def_used) 
				*def_used = TRUE;
			return def;
		}
	}
	g_assert_not_reached ();

	/* keep the compiler happy */
	if (def_used) 
		*def_used = TRUE;
	return def;
}

static void 
dump_keys (FILE *profile, TKeys *p)
{
	if (!p)
		return;
	dump_keys (profile, p->link);
	if (*p->key_name) {
		char *t = escape_string_and_dup (p->value);
		fprintf (profile, "%s=%s\n", p->key_name, t);
		g_free (t);
	}
}

static void 
dump_sections (FILE *profile, TSecHeader *p)
{
	if (!p)
		return;
	dump_sections (profile, p->link);
	if (p->section_name && p->section_name [0]){
		fprintf (profile, "\n[%s]\n", p->section_name);
		dump_keys (profile, p->keys);
	}
}

/*check the path and if we need to create directories create them with
  mode newmode, it needs an absolute path name or it will fail, it
  needs to be passed the dir and the filename since it will take the
  filename off*/
static gint
check_path(char *path, mode_t newmode)
{
	gchar *dirpath;
	gchar *p, *cur;
	GString *newpath;
	struct stat s;

	g_return_val_if_fail (path != NULL, FALSE);

	if(strchr(path,'/')==NULL)
		return FALSE;

	dirpath = strcpy (g_alloca (strlen (path) + 1), path);
	g_return_val_if_fail (dirpath != NULL, FALSE);

	if (*dirpath == '\0')
		return FALSE;

	/*not absolute, we refuse to work*/
	if (dirpath[0] != '/')
		return FALSE;

	p = strrchr(dirpath,'/');
		*p='\0';

	/*special case if directory exists, this is probably gonna happen
	  a lot so we don't want to go though checking it part by part*/
	if (stat(dirpath, &s) == 0) {
		/*check if a directory*/
		if (!S_ISDIR(s.st_mode))
			return FALSE;
		else
			return TRUE;
	}


	/*skip leading '/'*/
	p = dirpath;
	while(*p == '/')
		p++;

	cur = p;
	newpath = g_string_new("");
	while ((p = cur)) {
	       	cur = strchr (cur, '/');
		if (cur) {
			*cur = '\0';
			cur++;
		}
		
		newpath = g_string_append_c(newpath,'/');
		newpath = g_string_append(newpath,p);
		if(stat(newpath->str,&s)==0) {
			/*check if a directory*/
			if(!S_ISDIR(s.st_mode)) {
				g_string_free(newpath,TRUE);
				return FALSE;
			}
		} else {
			/*we couldn't stat it .. let's try making the
			  directory*/
			if(mkdir(newpath->str,newmode)!=0) {
				/*error, return false*/
				g_string_free(newpath,TRUE);
				return FALSE;
			}
		}
	}

	g_string_free(newpath,TRUE);

	return TRUE;
}



static gboolean 
dump_profile (TProfile *p, gboolean one_only)
{
	gboolean ret = TRUE;
	FILE *profile;
    
	if (!p)
		return ret;
	if(!one_only) {
		if(!dump_profile (p->link, FALSE))
			ret = FALSE;
	}
	
	/*
	 * was this profile written to?, if not it's not necessary to dump
	 * it to disk
	 */
	if (!p->to_be_deleted && !p->written_to)
		return ret;

	/* .ado: p->filename can be empty, it's better to jump over */
	if (p->filename[0] != '\0') {

		/*
		 * this file was added to after it was cleaned so it doesn't
		 * want to be deleted
		 */
		if(p->to_be_deleted && p->section)
			p->to_be_deleted = FALSE;
		if(p->to_be_deleted) {
			/*remove the file and remove all it's ramaints
			  from memory*/
			unlink(p->filename);
			/* this already must have been true */
			/*p->section = 0;*/
			p->filename [0] = '\0';
			p->written_to = TRUE;
			p->to_be_deleted = FALSE;
			if(p==Current)
				Current = NULL;
		} else if (check_path(p->filename,0755) &&
		    (profile = fopen (p->filename, "w")) != NULL){
			dump_sections (profile, p->section);
			fclose (profile);
		} else {
			/* we failed at actually writing to the file */
			ret = FALSE;
		}
	}
	
	/*mark this to not be dumped any more*/
	p->written_to = FALSE;

	return ret;
}

/**
 * gnome_config_sync:
 *
 * Writes all of the information modified by gnome-config to the
 * disk.
 *
 * Note: the gnome-config code does not write anything to the
 * configuration files until this routine is actually invoked.
 *
 * Returns: %TRUE if everything went well. %FALSE if any file
 * could not be written to disk.
 */
gboolean 
gnome_config_sync (void)
{
	gboolean ret;
	ret = dump_profile (Base, FALSE);
	gnome_config_drop_all();
	return ret;
}

/**
 * gnome_config_sync_file:
 * @path: A gnome-config path
 *
 * Writes all of the information modified by gnome-config to the
 * disk for the given file.
 *
 * Note: the gnome-config code does not write anything to the
 * configuration files until this routine or gnome_config_sync()
 * is actually invoked.
 *
 * Returns: %TRUE if everything went well, %FALSE if the file
 * could not be written to for some reason.  %FALSE is only returned
 * when a write was actually attempted and failed.
 */
/**
 * gnome_config_private_sync_file:
 * @path: A gnome-config path
 *
 * Writes all of the information modified by gnome-config to the
 * disk for the given private file.
 *
 * Note: the gnome-config code does not write anything to the
 * configuration files until this routine or gnome_config_sync()
 * is actually invoked.
 *
 * Returns: %TRUE if everything went well, %FALSE if the file
 * could not be written to for some reason.  %FALSE is only returned
 * when a write was actually attempted and failed.
 */
gboolean 
gnome_config_sync_file_ (char *path, gboolean priv)
{
	gboolean ret = TRUE;
	TProfile *p;
	ParsedPath *pp;
	char *fake_path;
	
	if (!path)
		return ret;

	fake_path = config_concat_dir_and_key (path, "section/key");
	pp = parse_path (fake_path, priv);
	g_free (fake_path);

	for (p = Base; p; p = p->link){
		if (strcmp (pp->file, p->filename) != 0)
			continue;
		if(!p->written_to)
			break;
		if(!dump_profile (p, TRUE))
			ret = FALSE;
		gnome_config_drop_file(path);
		break;
	}
	release_path (pp);

	return ret;
}

/**
 * gnome_config_clean_file: 
 * @path: A gnome-config path
 *
 * Cleans up the configuration file specified by @path from any
 * configuration information.
 *
 * Changes will take place after gnome_config_sync() has been invoked.
 */
/**
 * gnome_config_private_clean_file:
 * @path: A gnome-config path
 *
 * Cleans up the private configuration file specified by @path from
 * any configuration information.
 *
 * Changes will take place after gnome_config_sync() has been invoked.
 */
void 
gnome_config_clean_file_ (const char *path, gboolean priv)
{
	TProfile *p;
	ParsedPath *pp;
	char *fake_path;
	
	if (!path)
		return;

	fake_path = config_concat_dir_and_key (path, "section/key");
	pp = parse_path (fake_path, priv);
	g_free (fake_path);

	Current = NULL;
	
	for (p = Base; p; p = p->link){
		if (strcmp (pp->file, p->filename) != 0)
			continue;
		
		free_sections (p->section);
		p->section = NULL;
		p->written_to = TRUE;
		p->to_be_deleted = TRUE;
		release_path (pp);
		return;
	}
	release_path (pp);
}

/**
 * gnome_config_drop_file: 
 * @path: A gnome-config path
 *
 * Releases any memory resources that were allocated from accessing
 * the configuration file in @path.  Changes will take place after
 * gnome_config_sync() has been invoked
 */
/**
 * gnome_config_private_drop_file:
 * @path: A gnome-config path
 *
 * Releases any memory resources that were allocated from accessing the
 * private configuration file in @path.
 */
void 
gnome_config_drop_file_ (const char *path, gboolean priv)
{
	TProfile *p;
	TProfile *last;
	ParsedPath *pp;
	char *fake_path;
	
	if (!path)
		return;

	fake_path = config_concat_dir_and_key (path, "section/key");
	pp = parse_path (fake_path, priv);
	g_free (fake_path);

	Current = NULL;
	
	for (last = NULL,p = Base; p; last = p, p = p->link){
		if (strcmp (pp->file, p->filename) != 0)
			continue;
		
		if(last)
			last->link = p->link;
		else
			Base = p->link;
		
		free_sections (p->section);
		g_free(p->filename);
		g_free(p);
		release_path (pp);
		return;
	}
	release_path (pp);
}

/**
 * gnome_config_init_iterator:
 * @path: A gnome configuration path for a section.
 *
 * Creates an iterator handle that can be used to
 * iterate over the keys in a section in a gnome configuration
 * file.  @path must refer to a section.  The returned value
 * can be used as an iterator for gnome_config_iterator_next().
 *
 * Returns: The iterator handle.
 */
/**
 * gnome_config_private_init_iterator:
 * @path: A gnome configuration path for a section.
 *
 * Creates an iterator handle that can be used to
 * iterate over the keys in a section in a private gnome configuration
 * file.  @path must refer to a section.  The returned value
 * can be used as an iterator for gnome_config_iterator_next().
 *
 * Returns: The iterator handle.
 */
void *
gnome_config_init_iterator_ (const char *path, gboolean priv)
{
	TProfile   *New;
	TSecHeader *section;
	ParsedPath *pp;
	char *fake_path;
	iterator_type *iter;


	fake_path = config_concat_dir_and_key (path, "key");
	pp = parse_path (fake_path, priv);
	g_free (fake_path);
	
	if (!is_loaded (pp->file, &section)){
		struct stat st;
		
		if (stat (pp->file, &st) == -1){
			st.st_mtime = 0;
		}

		New = (TProfile *) g_malloc (sizeof (TProfile));
		New->link = Base;
		New->filename = g_strdup (pp->file);
		New->section = load (pp->file);
		New->mtime = st.st_mtime;
		New->last_checked = time (NULL);
		New->written_to = FALSE;
		New->to_be_deleted = FALSE;
		Base = New;
		section = New->section;
		Current = New;
	}
	for (; section; section = section->link){
		if (strcasecmp (section->section_name, pp->section))
			continue;
		iter = g_new (iterator_type, 1);
		iter->type = 0;
		iter->value = section->keys;
		release_path (pp);
		return iter;
	}
	release_path (pp);
	return 0;
}


/**
 * gnome_config_init_iterator_sections:
 * @path: A gnome configuration path for a file.
 *
 * Creates an iterator handle that can be used to iterate over the
 * sections in a gnome configuration file.  @path must refer to a
 * gnome configuration file.  The returned value can be used as an
 * iterator for gnome_config_iterator_next().
 *
 * Returns: The iterator handle.
 */
/**
 * gnome_config_private_init_iterator_sections:
 * @path: A gnome configuration path for a file
 *
 * Creates an iterator handle that can be used to iterate over the
 * sections in a private gnome configuration file.  @path must refer to a
 * gnome configuration file.  The returned value can be used as an
 * iterator for gnome_config_iterator_next().
 *
 * Returns: The iterator handle.
 */
void *
gnome_config_init_iterator_sections_ (const char *path, gboolean priv)
{
	TProfile   *New;
	TSecHeader *section;
	ParsedPath *pp;
	char *fake_path;
	iterator_type *iter;


	fake_path = config_concat_dir_and_key (path, "section/key");
	pp = parse_path (fake_path, priv);
	g_free (fake_path);
	
	if (!is_loaded (pp->file, &section)){
		struct stat st;
		
		if (stat (pp->file, &st) == -1)
			st.st_mtime = 0;

		New = (TProfile *) g_malloc (sizeof (TProfile));
		New->link = Base;
		New->filename = g_strdup (pp->file);
		New->section = load (pp->file);
		New->mtime = st.st_mtime;
		New->last_checked = time (NULL);
		New->written_to = FALSE;
		New->to_be_deleted = FALSE;
		Base = New;
		section = New->section;
		Current = New;
	}
	iter = g_new (iterator_type, 1);
	iter->type = 1;
	iter->value = section;
	release_path (pp);
	return iter;
}

/**
 * gnome_config_iterator_next:
 * @iterator_handle: A gnome configu iterator handle, returned from any
 *                   iteration start routine or this routine.
 * @key:   Address where the key gets stored.
 * @value: Address where the value gets stored.
 *
 * If @key is non-NULL, then @key will point to a g_malloc()ed region that
 * holds the key.
 *
 * If @value is non-NULL, then @value will point to a g_malloc()ed region that
 * holds the key.
 */
void *
gnome_config_iterator_next (void *iterator_handle, char **key, char **value)
{
	iterator_type *iter = iterator_handle;

        /*
	 * g_return_if_fail is not appropriate since this is not
	 * really a failure, but passing in an "empty" iterator (we
	 * return NULL at times)
	 */
	if(!iterator_handle)
		return NULL; 

	if (key)
		*key = NULL;
	if (value)
		*value = NULL;
	
	if (iter->type == 0){
		TKeys *keys;
		keys = iter->value;
		if (keys){
			if (key)
				*key   = g_strdup (keys->key_name);
			if (value)
				*value = g_strdup (keys->value);
			keys   = keys->link;
			iter->value = keys;
			return iter;
		} else {
			g_free (iter);
			return 0;
		}
	} else {
		TSecHeader *section;
		section = iter->value;

		if (section){
			if (key)
				*key = g_strdup (section->section_name);
			section = section->link;
			iter->value = section;
			return iter;
		} else {
			g_free (iter);
			return 0;
		}
	}
}

/**
 * gnome_config_clean_section:
 * @path: A gnome configuration path to a section.
 *
 * Cleans up the section specified by @path from any
 * configuration information.  Changes will only take place
 * after gnome_config_sync() has been invoked.
 */
/**
 * gnome_config_private_clean_section:
 * @path: A gnome configuration path to a section.
 *
 * Cleans up the section specified by @path in a private file from any
 * configuration information.  Changes will only take place after
 * gnome_config_sync() has been invoked.
 */
void 
gnome_config_clean_section_ (const char *path, gboolean priv)
{
	TProfile   *New;
	TSecHeader *section;
	ParsedPath *pp;
	char *fake_path;
	
	fake_path = config_concat_dir_and_key (path, "key");
	pp = parse_path (fake_path, priv);
	g_free (fake_path);
	
	if (!is_loaded (pp->file, &section)){
		struct stat st;

		if (stat (pp->file, &st) == -1)
			st.st_mtime = 0;

		New = (TProfile *) g_malloc (sizeof (TProfile));
		New->link = Base;
		New->filename = g_strdup (pp->file);
		New->section = load (pp->file);
		New->mtime = st.st_mtime;
		New->last_checked = time (NULL);
		New->written_to = FALSE;
		New->to_be_deleted = FALSE;
		Base = New;
		section = New->section;
		Current = New;
	}
	/* We only disable the section, so it will still be g_freed, but it */
	/* won't be found by further walks of the structure */

	for (; section; section = section->link){
		if (strcasecmp (section->section_name, pp->section))
			continue;
		section->section_name [0] = '\0';
		Current->written_to = TRUE;
	}
	release_path (pp);
}

/**
 * gnome_config_clean_key:
 * @path: A gnome configuration path to a key.
 *
 * Removes the definition for the key on a gnome configuration file.
 *
 * Changes will take place after gnome_config_sync() has been invoked.
 */
/**
 * gnome_config_private_clean_key:
 * @path: A gnome configuration path to a key.
 *
 * Removes the definition for the key on a private gnome configuration
 * file.
 *
 * Changes will take place after gnome_config_sync() has been invoked.
 */
void 
gnome_config_clean_key_ (const char *path, gboolean priv)
	/* *section_name, char *file */
{
	TProfile   *New;
	TSecHeader *section;
	TKeys *key;
	ParsedPath *pp;
	
	pp = parse_path (path, priv);
	
	if (!is_loaded (pp->file, &section)){
		struct stat st;
		
		if (stat (pp->file, &st) == -1)
			st.st_mtime = 0;

		New = (TProfile *) g_malloc (sizeof (TProfile));
		New->link = Base;
		New->filename = g_strdup (pp->file);
		New->section = load (pp->file);
		New->mtime = st.st_mtime;
		New->written_to = FALSE;
		New->last_checked = time (NULL);
		New->to_be_deleted = FALSE;
		Base = New;
		section = New->section;
		Current = New;
	}
	for (; section; section = section->link){
	        if (strcasecmp (section->section_name, pp->section))
		        continue;
		for (key = section->keys; key; key = key->link){
			if (strcasecmp (key->key_name, pp->key))
				continue;
			key->key_name [0] = 0;
			Current->written_to = TRUE;
		}
	}
	release_path (pp);
}

/**
 * gnome_config_has_section:
 * @path: A gnome configuration path to a section
 *
 * Queries the gnome configuration file for the presence
 * of the section specified in @path.
 *
 * Returns: %TRUE if the section exists, %FALSE otherwise.
 */
/**
 * gnome_config_private_has_section:
 * @path: A gnome configuration path to a section
 *
 * Queries the private gnome configuration file for the presence
 * of the section specified in @path.
 *
 * Returns: %TRUE if the section exists, %FALSE otherwise.
 */
gboolean 
gnome_config_has_section_ (const char *path, gboolean priv)
	/* char *section_name, char *profile */
{
	TProfile   *New;
	TSecHeader *section;
	ParsedPath *pp;
	char *fake_path;

	fake_path = config_concat_dir_and_key (path, "key");
	pp = parse_path (fake_path,priv);
	g_free (fake_path);
	
	if (!is_loaded (pp->file, &section)){
		struct stat st;
		
		if (stat (pp->file, &st) == -1)
			st.st_mtime = 0;

		New = (TProfile *) g_malloc (sizeof (TProfile));
		New->link = Base;
		New->filename = g_strdup (pp->file);
		New->section = load (pp->file);
		New->mtime = st.st_mtime;
		New->written_to = FALSE;
		New->last_checked = time (NULL);
		New->to_be_deleted = FALSE;
		Base = New;
		section = New->section;
		Current = New;
	}
	for (; section; section = section->link){
		if (strcasecmp (section->section_name, pp->section))
			continue;
		release_path (pp);
		return 1;
	}
	release_path (pp);
	return 0;
}

/**
 * gnome_config_drop_all:
 *
 * Drops any information cached in memory that was fetched with
 * gnome config. Any pending information that has not been
 * written to disk is discarded.
 */
void 
gnome_config_drop_all (void)
{
	free_profile (Base);
	Base = NULL;
	Current = NULL;
}

/**
 * gnome_config_get_int:
 * @path: A gnome configuration path to an item.
 *
 * Retrieves an integer value configuration item.
 *
 * Returns: The value of a configuration item.
 */
/**
 * gnome_config_private_get_int:
 * @path: A gnome configuration path to an item in the user-private namespace.
 *
 * Retrieves a configuration item as an int from the user's private
 * configuration storage area.
 *
 * Returns: The value of a configuration item as an integer.
 */
/**
 * gnome_config_get_int_with_default:
 * @path: A gnome configuration path to an item.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves an integer value configuration item.
 *
 * Returns: The value of a configuration item as an integer or @def if the
 * configuration item does not exist.
 */
/**
 * gnome_config_private_get_int_with_default:
 * @path: A gnome configuration path to an item in the user-private namespace.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves a configuration item as an int from the user's private
 * configuration storage area.
 *
 * Returns: The value of a configuration item as an integer or @def if the
 * configuration item does not exist.
 */
gint
gnome_config_get_int_with_default_ (const char *path, gboolean *def, gboolean priv)
{
	ParsedPath *pp;
	const char *r;
	int  v;
	
	pp = parse_path (path, priv);
	/*is there a better way to check if an absolute path has been given?*/
	if (!priv && pp->opath[0] != '=')
		r = access_config_extended (LOOKUP, pp->section, pp->key,
					    pp->def, pp->path, def);
	else
		r = access_config (LOOKUP, pp->section, pp->key, pp->def,
				   pp->file, def);

	/* It isn't an error if the key is not found.  */
	if (r == NULL) {
		release_path (pp);
		return 0;
	}

	v = atoi (r);
	release_path (pp);
	return v;
}

/**
 * gnome_config_get_float:
 * @path: A gnome configuration path to an item.
 *
 * Retrieves a floating-point valued configuration item.
 *
 * Returns: The value of a configuration item.
 */
/**
 * gnome_config_private_get_float:
 * @path: A gnome configuration path to an item in the user-private namespace.
 *
 * Retrieves a configuration item from the user's private configuration storage
 * area.
 *
 * Returns: The value of a configuration item as a floating-point
 * number.
 */
/**
 * gnome_config_get_float_with_default:
 * @path: A gnome configuration path to an item.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves a floating-point valued configuration item.
 *
 * Returns: The value of a configuration item as a floating-point
 * number or @def if the configuration item does not exist.
 */
/**
 * gnome_config_private_get_float_with_default:
 * @path: A gnome configuration path to an item in the user-private namespace.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves a configuration item from the user's private configuration storage
 * area.
 *
 * Returns: The value of a configuration item as a floating-point
 * number or @def if the configuration item does not exist.
 */
gdouble
gnome_config_get_float_with_default_ (const char *path, gboolean *def, gboolean priv)
{
	ParsedPath *pp;
	const char *r;
	gdouble v;
	
	pp = parse_path (path, priv);
	if (!priv && pp->opath[0] != '=')
		r = access_config_extended (LOOKUP, pp->section, pp->key,
					    pp->def, pp->path, def);
	else
		r = access_config (LOOKUP, pp->section, pp->key, pp->def,
				   pp->file, def);

	/* It isn't an error if the key is not found.  */
	if (r == NULL) {
		release_path (pp);
		return 0;
	}

        /* make sure we read values in a consistent manner */
	gnome_i18n_push_c_numeric_locale ();
	v = strtod(r, NULL);
	gnome_i18n_pop_c_numeric_locale ();

	release_path (pp);
	return v;
}

/*
 * same as gnome_config_get_string_with_default_, but using (ParsedPath *)
 */
static char *
get_string_with_default_from_pp (ParsedPath *pp, gboolean *def, gboolean priv)
{
	const char *r;
	char *ret = NULL;
	
	if (!priv && pp->opath[0] != '=')
		r = access_config_extended (LOOKUP, pp->section, pp->key,
					    pp->def, pp->path, def);
	else
		r = access_config (LOOKUP, pp->section, pp->key, pp->def,
				   pp->file, def);
	if (r)
		ret = g_strdup (r);
	return ret;
}

/*
 * like get_string_with_default_from_pp but with language
 * This is because we must work on the parsed path to add the language
 * thingie.
 */
static char *
get_string_with_default_from_pp_with_lang (ParsedPath *pp,
					   const char *lang,
					   gboolean *def,
					   gboolean priv)
{
	char *value;
	char *oldkey;

	/* switch the key in the key from underneath it, then
	 * return it back */
	oldkey = pp->key;
	pp->key = g_strconcat (oldkey, "[", lang, "]", NULL);
	value = get_string_with_default_from_pp (pp, def, priv);
	g_free (pp->key);
	pp->key = oldkey;

	return value;
}

/**
 * gnome_config_get_translated_string:
 * @path: A gnome configuration path to an item.
 *
 * Retrieves the value of a configuration item as a string appropriate for the
 * current language. The returned value should be freed with g_free() when no
 * longer needed.
 *
 * Returns: The value of the configuration item.
 */
/**
 * gnome_config_private_get_translated_string:
 * @path: A gnome configuration path to an item in the user-private namespace.
 *
 * Retrieves the value of a configuration item as a string appropriate for the
 * current language. The returned value should be freed with g_free() when no
 * longer needed.  The item is retrieved from the user's private configuration
 * storage area.
 *
 * Returns: The value of the configuration item.
 */
/**
 * gnome_config_get_translated_string_with_default:
 * @path: A gnome configuration path to an item.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves the value of a configuration item as a string appropriate for the
 * current language. The returned value should be freed with g_free() when no
 * longer needed.
 *
 * Returns: The value of the configuration item or @def if the configuration
 * item does not exist.
 */
/**
 * gnome_config_private_get_translated_string_with_default:
 * @path: A gnome configuration path to an item in the user-private namespace.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves the value of a configuration item as a string appropriate for the
 * current language. The returned value should be freed with g_free() when no
 * longer needed.  The item is retrieved from the user's private configuration
 * storage area.
 *
 * Returns: The value of the configuration item or @def if the configuration
 * item does not exist.
 */
char *
gnome_config_get_translated_string_with_default_ (const char *path,
						  gboolean *def,
						  gboolean priv)
{
	ParsedPath *pp;
	const GList *language_list;
	gboolean local_def = FALSE;

	char *value= NULL;

	language_list = gnome_i18n_get_language_list ("LC_MESSAGES");

	pp = parse_path (path, priv);

	while (!value && language_list) {
		const char *lang= language_list->data;

		value = get_string_with_default_from_pp_with_lang
			(pp, lang, &local_def, priv);

		if (local_def || !value || *value == '\0') {
			size_t n;

			g_free (value);
			value= NULL;

			/* Sometimes the locale info looks
			   like `pt_PT@verbose'.  In this case
			   we want to try `pt' as a backup.  */
			n = strcspn (lang, "@_");
			if (lang[n]) {
				char *copy = g_strndup (lang, n);

				value = get_string_with_default_from_pp_with_lang (pp, copy, &local_def, priv);
				g_free (copy);
				if (local_def || ! value || *value == '\0') {
					g_free (value);
					value = NULL;
				}
			}
		}
		language_list = language_list->next;
	}

	if (def != NULL) {
		*def = local_def;
	}

	if (!value){
		value = get_string_with_default_from_pp (pp, def, priv);

		if (!value || *value == '\0'){
			g_free (value);
			value = NULL;
		}
	}

	release_path (pp);

	return value;
}

/**
 * gnome_config_get_string:
 * @path: A gnome configuration path to an item.
 *
 * Retrieves the value of a configuration item as a string. This value should
 * be freed with g_free() when no longer needed.
 *
 * Returns: The value of the configuration item as a string.
 */
/**
 * gnome_config_private_get_string:
 * @path: A gnome configuration path to an item in the user-private namespace.
 *
 * Retrieves the value of a configuration item from the user's private
 * configuration directory as a string. This value should be freed with
 * g_free() when no longer needed.
 *
 * Returns: The value of the configuration item as a string.
 */
/**
 * gnome_config_get_string_with_default:
 * @path: A gnome configuration path to an item.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves the value of a configuration item as a string. This value should
 * be freed with g_free() when no longer needed.
 *
 * Returns: The value of the configuration item as a string, or @def if the
 * configuration key does not exist.
 */
/**
 * gnome_config_private_get_string_with_default:
 * @path: A gnome configuration path to an item in the user-private namespace.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves the value of a configuration item from the user's private
 * configuration directory as a string. This value should be freed with
 * g_free() when no longer needed.
 *
 * Returns: The value of the configuration item as a string, or @def if the
 * configuration key does not exist.
 */
char *
gnome_config_get_string_with_default_ (const char *path, gboolean *def,
				       gboolean priv)
{
	ParsedPath *pp;
	char *ret;
	
	pp = parse_path (path, priv);
	ret = get_string_with_default_from_pp (pp, def, priv);
	release_path (pp);

	return ret;
}

/**
 * gnome_config_get_bool:
 * @path: A gnome configuration path to an item.
 *
 * Retrieves a boolean configuration value.
 *
 * Returns: The value of a configuration item.
 */
/**
 * gnome_config_private_get_bool:
 * @path: A gnome configuration path to an item in the user-private namespace.
 *
 * Retrieves the item from the user's private configuration storage area.
 *
 * Returns: The value of a configuration item as a boolean.
 */
/**
 * gnome_config_get_bool_with_default:
 * @path: A gnome configuration path to an item.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves a boolean configuration value.
 *
 * Returns: The value of a configuration item, or @def if the
 * configuration item does not exist.
 */
/**
 * gnome_config_private_get_bool_with_default:
 * @path: A gnome configuration path to an item in the user-private namespace.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves the item from the user's private configuration storage area.
 *
 * Returns: The value of a configuration item as a boolean, or @def if the
 * configuration item does not exist.
 */
gboolean
gnome_config_get_bool_with_default_ (const char *path, gboolean *def,
				     gboolean priv)
{
	ParsedPath *pp;
	const char *r;
	int  v;
	
	pp = parse_path (path, priv);
	if (!priv && pp->opath[0] != '=')
		r = access_config_extended (LOOKUP, pp->section, pp->key,
					    pp->def, pp->path, def);
	else
		r = access_config (LOOKUP, pp->section, pp->key, pp->def,
				   pp->file, def);

	/* It isn't an error if the key is not found.  */
	if (r == NULL) {
		release_path (pp);
		return 0;
	}

	if (g_ascii_tolower(*r) == 't' || g_ascii_tolower(*r) == 'y' || atoi(r)) {
	  v = 1;
	} else {
	  /* If it's not true it has to be false :) */
	  v = 0;
	}
	release_path (pp);
	return v;
}

/**
 * gnome_config_make_vector:
 * @string: The stringified vector to decode into 'argcp' and 'argvp'
 * @argcp: Returns the number of elements in @string.
 * @argvp: Returns the array of strings found in @string.
 *
 * Creates a new vector from a string as it stored in the config file,
 * breaks the string on spaces except if the space is escaped with a
 * backslash.
 *
 */
void
gnome_config_make_vector (const char *string, int *argcp, char ***argvp)
{
	char *p;
	int count, esc_spcs;
	int space_seen;

	/* Figure out how large to make return vector.  Start at 2
	 * because we want to make NULL-terminated array, and because
	 * the loop doesn't count the final element.
	 */
	count = 2;
	space_seen = 0;
	for (p = (char *) string; *p; ++p) {
	        if (*p == '\\' && *(p+1)) {
			++p;
			if (space_seen){
				count++;
				space_seen = 0;
			}
		} else if (*p == ' ') {
			space_seen = 1;
		} else if (space_seen){
			count++;
			space_seen = 0;
		}
	}

	*argcp = count - 1;
	*argvp = (char **) g_malloc0 (count * sizeof (char *));

	p = (char *) string;
	count = 0;
	do {
		char *s, *tmp = p;

		esc_spcs = 0;
		while (*p && (esc_spcs ? 1 : (*p != ' '))){
			esc_spcs = 0;
			if (*p == '\\')
				esc_spcs = 1;
			p++;
		}

 		s = (char *) g_strndup (tmp, p - tmp);

		(*argvp)[count++] = tmp = s;

		while (*s) {
			if (*s == '\\') 
				s++;				
			if (!*s) break;
			*tmp++ = *s++;
		}
		*tmp = '\0';

		while (*p && *p == ' ')
			p++;
	} while (*p);
}

/**
 * gnome_config_get_vector:
 * @path: A gnome configuration path to an item.
 * @argcp: Number of elements in the vector.
 * @argvp: Vector of strings.
 *
 * Retrieves the value of a configuration item as a string array.
 * The returned vector should be freed with g_free() when no longer needed.
 */
/**
 * gnome_config_private_get_vector:
 * @path: A gnome configuration path to an item in the user-private namespace.
 * @argcp: Number of elements in the vector.
 * @argvp: Vector of strings.
 *
 * Retrieves the value of a configuration item as a string array.
 * The returned vector should be freed with g_free() when no longer needed. The
 * configuration value is retrieved from the user's private configuration
 * storage area.
 */
/**
 * gnome_config_get_vector_with_default:
 * @path: A gnome configuration path to an item.
 * @argcp: Number of elements in the vector.
 * @argvp: Vector of strings.
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves the value of a configuration item as a string array.
 * The returned vector should be freed with g_free() when no longer needed.
 *
 */
/**
 * gnome_config_private_get_vector_with_default:
 * @path: A gnome configuration path to an item in the user-private namespace.
 * @argcp: Number of elements in the vector
 * @argvp: Vector of strings
 * @def: A pointer to a flag that will be set if the default value for the item
 * is returned.
 *
 * Retrieves the value of a configuration item as a string array.
 * The returned vector should be freed with g_free() when no longer needed. The
 * configuration value is retrieved from the user's private configuration
 * storage area.
 */
void
gnome_config_get_vector_with_default_ (const char *path, int *argcp,
				       char ***argvp, gboolean *def, gboolean priv)
{
	ParsedPath *pp;
	const char *rr;
	
	pp = parse_path (path, priv);
	if (!priv && pp->opath[0] != '=')
		rr = access_config_extended (LOOKUP, pp->section, pp->key,
					     pp->def, pp->path, def);
	else
		rr = access_config (LOOKUP, pp->section, pp->key, pp->def,
				    pp->file, def);

	if (rr == NULL) {
		*argvp = NULL;
		*argcp = 0;
	} else
		gnome_config_make_vector (rr, argcp, argvp);
	release_path (pp);
}

/**
 * gnome_config_set_translated_string:
 * @path: A gnome configuration path to a key.
 * @value: A string value to set.
 * 
 * Stores the string value @value in the file/section/key defined
 * by the @path on the proper section for the current language set by
 * by the user.
 */
/**
 * gnome_config_private_set_translated_string:
 * @path: A gnome configuration path to a key.
 * @new_value: A string value to set.
 * 
 * Stores the string value @new_value in the file/section/key defined by the
 * @path on the proper section for the current language set by by the user.
 * The configuration value is stored in the user's private storage area.
 */
void
gnome_config_set_translated_string_ (const char *path, const char *value,
				     gboolean priv)
{
	const GList *language_list;
	const char *lang;
	char *tkey;

	language_list = gnome_i18n_get_language_list("LC_MESSAGES");

	lang= language_list ? language_list->data : NULL;

	if (lang && (strcmp (lang, "C") != 0)) {
		tkey = g_strconcat (path, "[", lang, "]", NULL);
		gnome_config_set_string_(tkey, value, priv);
		g_free (tkey);
	} else
		gnome_config_set_string_ (path, value, priv);
}

/**
 * gnome_config_set_string:
 * @path: A gnome configuration path to a key.
 * @new_value: A string value to set.
 *
 * Stores the string value @new_value in the file/section/key
 * defined by the @path.
 */
/**
 * gnome_config_private_set_string:
 * @path: A gnome configuration path to a key.
 * @new_value: A string value to set.
 *
 * Stores the string value @new_value in the file/section/key
 * defined by the @path. The configuration value is stored in the user's
 * private storage area.
 */
void
gnome_config_set_string_ (const char *path, const char *new_value, gboolean priv)
{
	ParsedPath *pp;
	const char *r;
	
	pp = parse_path (path, priv);
	r = access_config (SET, pp->section, pp->key, new_value, pp->file,
			   NULL);
	release_path (pp);
}

/**
 * gnome_config_set_int:
 * @path: A gnome configuration path to a key.
 * @new_value: A int value to set.
 *
 * Stores the integer value @new_value in the file/section/key
 * defined by the @path.
 */
/**
 * gnome_config_private_set_int:
 * @path: A gnome configuration path to a key.
 * @new_value: A int value to set.
 *
 * Stores the integer value @new_value in the file/section/key
 * defined by the @path. The value is stored in the user's private
 * configuration storage area.
 */
void
gnome_config_set_int_ (const char *path, int new_value, gboolean priv)
{
	ParsedPath *pp;
	char intbuf [40];
	const char *r;
	
	pp = parse_path (path, priv);
	g_snprintf (intbuf, sizeof(intbuf), "%d", new_value);
	r = access_config (SET, pp->section, pp->key, intbuf, pp->file,
			   NULL);
	release_path (pp);
}

/**
 * gnome_config_set_float:
 * @path: A gnome configuration path to a key.
 * @new_value: A double value to set.
 *
 * Stores the double value @new_value in the file/section/key
 * defined by the @path.
 */
/**
 * gnome_config_private_set_float:
 * @path: A gnome configuration path to a key.
 * @new_value: A double value to set.
 *
 * Stores the double value @new_value in the file/section/key
 * defined by the @path. The value is stored in the user's private
 * configuration storage area.
 */
void
gnome_config_set_float_ (const char *path, gdouble new_value, gboolean priv)
{
	ParsedPath *pp;
	char floatbuf [40];
	const char *r;
	
	pp = parse_path (path, priv);

        /* make sure we write values in a consistent manner */
	gnome_i18n_push_c_numeric_locale ();
	g_snprintf (floatbuf, sizeof(floatbuf), "%.17g", new_value);
	gnome_i18n_pop_c_numeric_locale ();

	r = access_config (SET, pp->section, pp->key, floatbuf, pp->file,
			   NULL);
	release_path (pp);
}

/**
 * gnome_config_set_bool:
 * @path: A gnome configuration path to a key.
 * @new_value: A boolean value to set.
 *
 * Stores boolean value @new_value in the file/section/key defined by
 * @path.
 */
/**
 * gnome_config_private_set_bool:
 * @path: A gnome configuration path to a key.
 * @new_value: A boolean value to set.
 *
 * Stores boolean value @new_value in the file/section/key defined by @path.
 * The value is stored in the user's private configuration storage area.
 */
void
gnome_config_set_bool_ (const char *path, gboolean new_value, gboolean priv)
{
	ParsedPath *pp;
	const char *r;
	
	pp = parse_path (path, priv);
	r = access_config (SET, pp->section, pp->key,
			   new_value ? "true" : "false", pp->file, NULL);
	release_path (pp);
}

/**
 * gnome_config_assemble_vector:
 * @argc: Number of elements in the @argv string array.
 * @argv: An array of strings.
 *
 * This routine returns the the strings in the array contactenated by
 * spaces. The return value should be freed with g_free() when it is no longer
 * required.
 *
 * Returns: A string with the concatenation results.
 */
char *
gnome_config_assemble_vector (int argc, const char *const argv [])
{
	char *value, *p;
	const char *s;
	int i;
	size_t len;

	/*
	 * Compute length of quoted string.  We cheat and just use
	 * twice the sum of the lengths of all the strings.  
	 */
	len = 1;
	for (i = 0; i < argc; ++i)
		len += 2 * strlen (argv [i]) + 1 + argc;

	p = value = g_malloc (len);
	for (i = 0; i < argc; ++i) {
		for (s = argv [i]; *s; ++s) {
			if (*s == ' ' || *s == '\\')
				*p++ = '\\';
			*p++ = *s;
		}
		*p++ = ' ';
	}
	*p = '\0';

	return value;
}

/**
 * gnome_config_set_vector:
 * @path: A gnome configuration path to a key.
 * @argc: The number of elements in @argv.
 * @argv: A string array holding the data to store.
 *
 * Stores vector @argv in the file/section/key defined by
 * @path.
 */
/**
 * gnome_config_private_set_vector:
 * @path: A gnome configuration path to a key.
 * @argc: The number of elements in @argv.
 * @argv: A string array holding the data to store.
 *
 * Stores vector @argv in the file/section/key defined by @path. The
 * configuration value is set in the user's private storage area.
 */
void
gnome_config_set_vector_ (const char *path, int argc,
			  const char *const argv[],
			  gboolean priv)
{
	ParsedPath *pp;
	char *s;

	pp = parse_path (path, priv);
	s = gnome_config_assemble_vector (argc, argv);
	access_config (SET, pp->section, pp->key, s, pp->file, NULL);
	g_free (s);
	release_path (pp);
}

/**
 * gnome_config_push_prefix:
 * @path: A gnome configuration path prefix.
 *
 * @path is a prefix that will be prepended automatically to any
 * non-absolute configuration path in gnome config.
 *
 * This is used to simplify application loading code.
 *
 * Library code will usually have to set the prefix before doing
 * any gnome-configuration access, since the application might
 * be using their own prefix. 
 */
void
gnome_config_push_prefix (const char *path)
{
	prefix_list = g_slist_prepend(prefix_list, g_strdup(path));
}

/**
 * gnome_config_pop_prefix:
 *
 * Call this routine to remove the current configuration prefix from the stack.
 */
void
gnome_config_pop_prefix (void)
{
	if(prefix_list) {
		GSList *plist = prefix_list;
		g_free(prefix_list->data);
		prefix_list = prefix_list->next;
		g_slist_free_1(plist);
	}
}

/**
 * gnome_config_set_set_handler
 * @func: Obsolete
 * @data: Obsolete
 *
 * Internal Obsolete.
 */
void
gnome_config_set_set_handler(void (*func)(void *),void *data)
{
	g_warning("gnome_config_set_set_handler is obscolete and has no replacement");
}

/**
 * gnome_config_set_sync_handler
 * @func: obsolete
 * @data: obsolete
 *
 * Internal routine
 */
void
gnome_config_set_sync_handler(void (*func)(void *),void *data)
{
	g_warning("gnome_config_set_sync_handler is obscolete and has no replacement");
}

#ifdef TEST

static
x (char *str, char *file, char *sec, char *key, char *val)
{
	ParsedPath *pp;

	printf ("%s\n", str);
	pp = parse_path (str, FALSE);
	printf ("   file: %s [%s]\n", pp->file, file);
	printf ("   sect: %s [%s]\n", pp->section, sec);
	printf ("   key:  %s [%s]\n", pp->key, key);
	printf ("   def:  %s [%s]\n", pp->def, val);
}


main ()
{
	gnome_user_dir = "USERDIR";
	x ("=/tmp/file=seccion/llave=valor", "/tmp/file", "seccion", "llave", "valor");
	x ("=/tmp/file=seccion/llave", "/tmp/file", "seccion", "llave", NULL);
	x ("/file/seccion/llave=valor", "USERDIR/file", "seccion", "llave", "valor");
	x ("/file/seccion/llave", "USERDIR/file", "seccion", "llave", NULL);
	x ("/file/archivo/archivo/seccion/llave", "USERDIR/file/archivo/archivo", "seccion", "llave", NULL);
	x ("/file/archivo/archivo/seccion/llave=valor", "USERDIR/file/archivo/archivo", "seccion", "llave", "valor");
	
}
#endif
