/* $Id: e2_alias.c 2672 2013-08-10 22:15:10Z tpgww $

Copyright (C) 2004-2013 tooar <tooar@emelfm2.net>

This file is part of emelfm2.
emelfm2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelfm2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file src/command/e2_alias.c
@brief functions for handling command aliases

'match' strings are compiled as extended regex.
If they do not start with '^', then one is prepended.
The whole alias list is scanned in order, so that aliases can be
chained, unless and until a match with its 'stop' flag set
'replace' strings may contain:
\\1 = substitute the matched alias
\\2 = substitute that part of the input command string which is AFTER the matched alias
In theory,  \\0 = substitute that part of the parsed string which is BEFORE
the matched alias, but actually the alias matching always starts from the start
*/

#include "emelfm2.h"
#include <string.h>
#include "e2_alias.h"

/**
@brief cleanup data struct for @a alias
@param alias pointer to compiled-alias data struct
@return
*/
static void _e2_alias_free (E2_Alias *alias)
{
	g_free (alias->match);
	g_free (alias->replace);
	regfree (&(alias->preg));
	DEALLOCATE (E2_Alias, alias);
}
/**
@brief create data struct for an alias entry
Back-reference flag is set if @a replace includes \1 or \2
\0 is now irrelevant
@param match string to be replaced
@param replace string to use as a replacement
@param preg pointer to regex struct
@param stop TRUE if the scan is to stop at the first match
@return pointer to alias struct with the matching data, or NULL if error occurred
*/
static E2_Alias *_e2_alias_get (gchar *match, gchar *replace, regex_t preg,
	gboolean stop)
{
	E2_Alias *alias = ALLOCATE (E2_Alias);
	CHECKALLOCATEDWARN (alias, return NULL;)
	alias->match = match;
	alias->replace = replace;
	alias->preg = preg;
	alias->stop = stop;

	//test if the replace string has a back reference
	alias->has_back_reference = FALSE;
	gchar *test = NULL;
	if ((test = strchr (replace, '\\')) != NULL)	//if always ascii \, don't need g_utf8_strchr()
	{
		if (
		//	test[1] == '0' ||
		test[1] == '1' || test[1] == '2')
			alias->has_back_reference = TRUE;
	}
	return alias;
}
/**
@brief create replacement for @a str with alias replaced
This tries to match from start of string, as compiled aliases start
with ^ and were not compiled with flag REG_NOTBOL
@param alias pointer to compiled-alias data struct
@param str utf command string, the start of which might match an alias
@param replaced location for storing the string with the alias substituted
@return TRUE if a match was found, and if so, a newly allocated string in @a replaced
*/
static gboolean _e2_alias_match (E2_Alias *alias, gchar *str, gchar **replaced)
{
	regmatch_t pmatch;
	//CHECKME does this support utf ?
	gint ret = regexec (&alias->preg, str, 1, &pmatch, 0);
	gchar *tmp = NULL, *tmp2 = NULL;
	if (ret == REG_NOMATCH)
		return FALSE;

	else
		if (ret == 0)
	{
		if ((pmatch.rm_eo == -1) || (pmatch.rm_so == -1))
		//the compiled string wasn't acually used ??
			return FALSE;
		//make sure the whole of the command matches (ie avoid e.g. x matches xpdf)
//		if (*(str+pmatch.rm_eo) != ' ' && *(str+pmatch.rm_eo) != '\0')
//			return FALSE;
		//so we have a match, replace it
		if (alias->has_back_reference)
		{
			tmp = g_strdup (alias->replace);
//			printd (DEBUG, "match alias ended at offset %d", pmatch.rm_eo);
			//carve off the end of the source string
			if (pmatch.rm_eo > 0)
			{
				gchar *real_start = str + pmatch.rm_eo;
//				if (*real_start != '\0')
//					real_start--;	//backup so we grab the (last) separator too
				tmp2 = e2_utils_str_replace (tmp, "\\2", real_start);
			}
			else
				tmp2 = e2_utils_str_replace (tmp, "\\2", "");

			g_free (tmp);
			tmp = tmp2;

/*			//carve off any prior part of the source string
			//(can never happen while ^ at start)
			if (pmatch.rm_so > 0)
			{
				gchar *match = g_strndup (str, pmatch.rm_so);
				tmp2 = e2_utils_str_replace (tmp, "\\0", match);
				g_free (match);
			}
			else
				tmp2 = e2_utils_str_replace (tmp, "\\0", "");

			g_free (tmp);
			tmp = tmp2;
*/
			//build the replacement string with the alias in place
			gchar *match = g_strndup (str + pmatch.rm_so, pmatch.rm_eo - pmatch.rm_so);
			*replaced = e2_utils_str_replace (tmp, "\\1", match);

			g_free (tmp);
			g_free (match);
		}
		else	//no back-ref
		{
			gchar *last = str + pmatch.rm_eo;
			//the compiled pattern included whitespace (if any) after the command.
			//we need to keep some of that, to separate any following parameter
			//so if not at end of pattern, backup to grab the last separator too
			if (*last != '\0')
				last--;

/*			if (pmatch.rm_so > 0)   //see above - never pass this test
			{
				gchar *first = g_strndup (str, pmatch.rm_so);
				*replaced = g_strconcat (first, alias->replace, last, NULL);
				g_free (first);
			} else
*/				*replaced = g_strconcat (alias->replace, last, NULL);
		}
		return TRUE;
	} else
	{
		printd (DEBUG, "regex error");
		return FALSE;
	}
}
/**
@brief convert all alias data into a form ready for use
This adds a '^' to the beginning of the alias if not there already
(causing matching from the start) so that the config data doesn't
need to show a '^'. This means that aliases can only work for
a command, not for a part of a command after its start.
To avoid matches like x <=> xpdf, a separator is added to
the end of each alias. (This means that checks are needed
to reinstate a separator in the replacement command string.)
The match strings are 'compiled' as extended regex.
Data structs are put into a list.
@param rt pointer to alias data struct
@param iter pointer to tree iter, used for option tree traversal
@return
*/
static void _e2_alias_add_all (E2_AliasRuntime *rt, GtkTreeIter *iter)
{
	do
	{
		gchar *match, *replace, *builder;
		gboolean stop;
		gtk_tree_model_get (rt->set->ex.tree.model, iter,
			0, &match, 1, &stop, 2, &replace, -1);

		if ((match != NULL) && (*match != '\0'))
		{
			if (*match != '^')
			{
				builder = g_strconcat ("^", match, NULL);
				g_free (match);
				match = builder;
			}
			//add separator
			builder = g_strconcat (match, "([ \t]+|$)", NULL);
			g_free (match);
			match = builder;
			regex_t preg;
			if (!regcomp (&preg, match, REG_EXTENDED))
			{
				E2_Alias *alias = _e2_alias_get (match, replace, preg, stop);
				rt->aliases = g_list_append (rt->aliases, alias);
			}
			else
				printd (WARN, "error compiling regexp pattern '%s'", match);
		}
	} while (gtk_tree_model_iter_next (rt->set->ex.tree.model, iter));
}

  /******************/
 /***** public *****/
/******************/

/**
@brief find a matching alias for @a str
If the aliases have not been enlisted, then that is done first.
The whole alias list is scanned in order, so that aliases can be chained,
unless and until a match has its 'stop' flag set
@param str utf string starting with a command for which there may be an alias
@param rt pointer to aliases runtime data struct
@return a newly-allocated string, either the matched alias, or the same as @a str if there is no match
*/
gchar *e2_alias_apply (gchar *str, E2_AliasRuntime *rt)
{
	gchar *strcpy = g_strdup (str);	//never touch the original string
	if (!rt->sync)
	{
		e2_alias_sync (rt);
		if (!rt->sync)
			return strcpy;
	}
	printd (DEBUG, "find an alias for %s", str);
//	gint cmdlen = strlen (str);
	gchar *ret = NULL;  //no-match signal

	GList *member;
	for (member = rt->aliases; member != NULL; member = member->next)
	{
		E2_Alias *alias = (E2_Alias *)member->data;
		if (_e2_alias_match (alias, str, &ret))   //cmdlen, &ret))
		{
			g_free (strcpy);
			strcpy = ret;
			if (alias->stop)
				break;
			else
				printd (DEBUG, "not stopping - search for another matched alias");
		}
	}

	if (ret == NULL)
		return strcpy;
	else
		return ret;
}
/**
@brief refresh alias data
Any aliases in the list are cleared, a replacement list is built
from config data if possible, and the 'sync' flag is set accordingly
@param rt pointer to aliases runtime data struct
@return
*/
void e2_alias_sync (E2_AliasRuntime *rt)
{
	printd (DEBUG, "e2_alias_sync (rt:)");
	if (rt->aliases != NULL)
	{
		g_list_foreach (rt->aliases, (GFunc) _e2_alias_free, NULL);
		g_list_free (rt->aliases);
		rt->aliases = NULL;
	}
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_first (rt->set->ex.tree.model, &iter))
	{
		_e2_alias_add_all (rt, &iter);
		rt->sync = TRUE;
	}
	else
	{
		printd (NOTICE, "no aliases defined");
		rt->sync = FALSE;
	}
}
/**
@brief install default tree options for aliases
This function is called only if the default is missing from the config file
@param set pointer to set data
@return
*/
static void _e2_alias_tree_defaults (E2_OptionSet *set)
{
	gchar *local = DOC_DIR G_DIR_SEPARATOR_S MAIN_HELP;
	gchar *utf = F_FILENAME_FROM_LOCALE (local);
	e2_option_tree_setup_defaults (set,
	g_strdup("command-aliases=<"),  //internal name
// NOW IN COMMAND INTERPRETER
//	g_strdup(" *[^>].*[<>\\|].*||>\\1"),	//make sure re-directions and pipes are run in a new shell

//	g_strdup(";\\|\\\\|\\|>\\|<\\|\\*\\|=||%\\0\\1\\2"),
	//some terminals (eg gnome-terminal) don't support these
	//NB $TERM doesn't always exist, or might be invalid
//	g_strconcat(_("x"),"|true|$[command-xterm] -e sh -c '\\2'",NULL),
//	g_strconcat(_("xx"),"|true|$[command-xterm] -e sh -c '\\2;echo -n \"",_("Done. Press enter "),"\";read'",NULL),
//	g_strconcat(_("su"),"|true|$[command-xterm] -e sh -c 'su -c \\2;echo -n \"",_("Done. Press enter "),"\";read'",NULL),
	g_strconcat(_("x"),"|true|xterm -e sh -c '\\2'",NULL),
	g_strconcat(_("xx"),"|true|xterm -e sh -c '\\2;echo -n \"",_("Done. Press enter "),"\";read'",NULL),
#ifdef E2_POLKIT
	//see also _e2p_upgrade_su()
	g_strdup("su|true|pkexec \\2"),
#else
	g_strconcat("su|true|xterm -e sh -c 'su -c \"\\2\";echo -n \"",_("Done. Press enter "),"\";read'",NULL),
#endif
	g_strdup("cp|true|cp -i \\2"),
	g_strdup("rm|true|rm -i \\2"),
	g_strconcat(_("clear"),"|true|",_A(10),".",_A(36),NULL),	//<
	g_strconcat(_("quit"),"|true|",_A(1),".",_A(75),NULL),	//command.quit 	//<
	g_strconcat(_("help"),"|true|",_A(6),".",_A(111)," ",utf," [commands]",NULL),  //file.view_at _I( NOTE helpfile titles are not (yet?) translated
	g_strconcat(_("keys"),"|true|",_A(10),".",_A(57)," ",_("keys")," \\2",NULL),
#ifdef E2_MOUSECUSTOM
	g_strconcat(_("buttons"),"|true|",_A(10),".",_A(57)," ",_("buttons")," \\2",NULL),
#endif
	g_strconcat(_("e2ps"),"|true|",_A(2),".",_A(61),NULL),
#ifdef E2_VFS
	g_strconcat(_("cns"),"|true|",_A(13),".",_A(125),NULL),	//pane.namespace
#endif
	g_strdup("wget|true|\\1 --progress=bar:force \\2"),
	g_strdup(">"),
	NULL);
	F_FREE (utf, local);
}
/**
@brief initialize alias-related options, and init some alias flags
@return
*/
void e2_alias_init (void)
{
	E2_AliasRuntime *rt = &app.aliases;
	rt->aliases = NULL;  //moved up from end
	rt->sync = FALSE;
	//this option does not require a rebuild after config change
	gchar *group_name = g_strconcat(_C(6),".",_C(0),NULL); //_("commands.aliases"
	printd (DEBUG, "e2_alias_init (rt:,group:%s)", group_name);
	rt->set = e2_option_tree_register ("command-aliases", group_name, _C(0),
		NULL, NULL, NULL, E2_OPTION_TREE_UP_DOWN | E2_OPTION_TREE_ADD_DEL |
		E2_OPTION_TREE_LIST,
		E2_OPTION_FLAG_ADVANCED | E2_OPTION_FLAG_FREEGROUP | E2_OPTION_FLAG_BUILDALIAS);
	e2_option_tree_add_column (rt->set, _("Match"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	//this option stops alias-scanning when 1st match found
	e2_option_tree_add_column (rt->set, _("Stop"), E2_OPTION_TREE_TYPE_BOOL, FALSE, "false",
		0, NULL, NULL);
	e2_option_tree_add_column (rt->set, _("Replace"), E2_OPTION_TREE_TYPE_STR, 0, "",
		0, NULL, NULL);
	e2_option_tree_create_store (rt->set);

	e2_option_tree_prepare_defaults (rt->set, _e2_alias_tree_defaults);
}
/**
@brief cleanup alias-related data
@return
*/
void e2_alias_clean (void)
{
	E2_AliasRuntime *rt = &app.aliases;
	if (rt->aliases != NULL)
	{
		g_list_foreach (rt->aliases, (GFunc) _e2_alias_free, NULL);
		g_list_free (rt->aliases);
		rt->aliases = NULL;
	}
}
