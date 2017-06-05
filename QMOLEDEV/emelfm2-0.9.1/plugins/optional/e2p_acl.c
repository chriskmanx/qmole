/* $Id: e2p_acl.c 2927 2013-11-14 07:28:19Z tpgww $

Copyright (C) 2007-2013 tooar <tooar@emelfm2.net>

This file is part of emelFM2.
emelFM2 is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

emelFM2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with emelFM2; see the file GPL. If not, see http://www.gnu.org/licenses.
*/

/**
@file plugins/optional/e2p_acl.c
@brief plugin for updating item access-control, and related functions
*/

/*TODO
support E2ACL_POSIX_RESTRICTED probably with string form acl converted to walkable array
 _e2p_acl_find_entry
 _e2p_acl_create_entry
 _e2p_acl_get_for_mode
 _e2p_acl_apply_shown
 _e2p_acl_apply_modified
 _e2p_acl_fill_store
 _e2p_acl_reset_mode_fields

debugging
*/

//signature component, must match 'core' of this file name and likewise for corresponding icon file name 
#define ANAME "acl"

//allow non-standard store-filling
//#define FAKEACLSTORE
//prevent any actual change to the filesystem
//#define DISABLEACLCHANGE

#define ACLEXTRA_ACTIONS

#include "emelfm2.h"
#include <string.h>
#include <pwd.h>
#include <grp.h>
#include <pthread.h>
#include "e2_plugins.h"
#include "e2_dialog.h"
#include "e2_permissions_dialog.h"
#include "e2_ownership_dialog.h"
#include "e2_task.h"
#include "e2_icons.h"
#include "e2_filelist.h"

//define the scope of libacl functions which are available for use here
#ifdef __linux__
# define E2ACL_POSIX_EXTENDED
#else
//pick one of these
//# define E2ACL_POSIX_EXTENDED
# define E2ACL_POSIX_FULL
//# define E2ACL_POSIX_RESTRICTED	not yet supported
#endif

#ifdef E2ACL_POSIX_EXTENDED
# include <acl/libacl.h>
# define ACL_GET_PERM acl_get_perm
#else
# include <sys/acl.h>
# ifdef HAVE_ACL_GET_PERM
#  define ACL_GET_PERM acl_get_perm
# else
#  ifdef HAVE_ACL_GET_PERM_NP
#    define ACL_GET_PERM acl_get_perm_np
#  else
#    warning in the absence of anything else more obviously suitable, a linux-derived
#    warning version of function acl_get_perm() will be used. You MUST CHECK that''s ok
#    define ACL_GET_PERM _e2p_get_perm
#  endif
# endif
#endif

//limit on entries per ACL
#ifdef _POSIX_ACL
#define MAX_ACL_ENTRIES _POSIX_ACL_MAX
#else
#define MAX_ACL_ENTRIES 16
#endif

//acl-array enumerator
enum { AXSNOW, DEFNOW, MAXACLS };
//liststore columns enumerator
enum { CLASS, QUAL, READ, WRITE, EXEC, WHOLE, SORTKEY, MAXACLCOLS };
//change-process flags
typedef enum
{
	//types of "base" data
	E2_ACL_SHOWN  = 1, //changes based only on the data entered into the treeview(s)
	E2_ACL_SYSTEM = 1 << 1, //changes based only on converted form of normal mode permissions
	E2_ACL_SYSMOD = 1 << 2, //changes based on converted form of normal mode permissions,
							//as modified by data in the treeview(s)
	//types of entry-change
	E2_ACL_NUKE   = 1 << 3,	//revert to minimum ACL
	E2_ACL_SET    = 1 << 4,	//replace current, or add if missing from current (this may be the only valid one on some systems)
	E2_ACL_ADD    = 1 << 5, //add whole if appropriate or add entry permissions
	E2_ACL_REMOVE = 1 << 6, //delete whole if appropriate or delete entry permissions
	//only for remembering button-state between dialogs
	E2_ACL_WHOLE  = 1 << 7,
	//types of recursion
	E2_ACL_NODOWN = 1 << 8, //no recursion into any directory
	E2_ACL_DIR    = 1 << 9, //recurse dirs, apply things in accord with E2_ACL_DIRAXS, E2_ACL_DIRDEF
	E2_ACL_OTHER  = 1 << 10, //recurse dirs, apply acesss-ACL only to non-dirs
	E2_ACL_DIRAXS = 1 << 11, //apply access-ACL to any relevant dir
	E2_ACL_DIRDEF = 1 << 12 //default-ACL to any relevant dir
} E2_ACLTask;

//data for recursive changes
typedef struct _E2_ChACLData
{
	//pointers to some dialog data
	E2_ACLTask task;
	GPtrArray *axs_changes;
	GPtrArray *def_changes;
	GList *dirdata;	//list of E2P_DirEnt's for visited dirs, to be processed later
	gboolean continued_after_problem;
} E2_ChACLData;

typedef struct _E2_CopyACLData
{
	E2_ACLTask task;
	gint oldroot_len;	//length of original localpath, for filtering descendants
	VPATH *otherdir;	//localised, with trailer
	GList *dirdata;	//list of E2P_DirEnt's for visited dirs, to be processed later
	gboolean continued_after_problem;
} E2_CopyACLData;

//data for an entry in a changes-array
typedef struct _E2_EntryChangeData
{
	acl_tag_t tag;	//= type
	id_t id;	//uid_t or gid_t
	acl_perm_t perms;	//acl_permset_t points to one of these
	gboolean whole;	//TRUE = apply change to whole entry, FALSE = apply to perms only
} E2_EntryChangeData;

typedef struct _E2_ACLDlgRuntime
{
	GtkWidget *dialog;
	GtkWidget *axs_view;	//permissions presentation
	GtkWidget *def_view;
	GtkWidget *treeview;	//= axs_view or def_view for current use
	GtkListStore *axs_store;
	GtkListStore *def_store;
	GtkListStore *store;	//= axs_store or def_store for current use
	GtkListStore *classes;	//valid names for cells in the CLASS column of the main store
	GtkListStore *users;	//valid names for cells in the QUAL column of the main store
	//radio buttons which decide what to show
	GtkWidget *use_axsacl_btn;
	GtkWidget *use_defacl_btn;
	//toggle and radio buttons which decide what to do
	GtkWidget *shown_perms_btn;
	GtkWidget *system_perms_btn;
	GtkWidget *sysmod_perms_btn;
	GtkWidget *dir_axs_btn;
	GtkWidget *dir_def_btn;

	GtkWidget *remove_all_btn;
	GtkWidget *set_data_btn;
	GtkWidget *add_data_btn;
	GtkWidget *remove_data_btn;
	//toggle to setup automatic whole-field completion when row is added etc
	GtkWidget *change_whole_btn;

	GtkWidget *recurse_btn;
	GtkWidget *recurse_dirs_btn;
	GtkWidget *recurse_other_btn;
	//some of the action-area widgets
	GtkWidget *add_row_btn;
	GtkWidget *remove_row_btn;

	gchar *itempath;	//localised path of item being processed
	gboolean thisis_dir;	//whether itempath is a directory
	gboolean permission;	//whether the user is authorised to change permission(s) of itempath

	acl_t acls [MAXACLS];	//existing acl(s) of itempath, or NULL

	E2_ACLTask task;
	GPtrArray *axs_changes;
	GPtrArray *def_changes;
} E2_ACLDlgRuntime;

static void _e2p_acl_cell_edit_start_cb (GtkCellRenderer *renderer,
	GtkCellEditable *editable, gchar *path_string, E2_ACLDlgRuntime *rt);
static void _e2p_acl_cell_edit_stop_cb (GtkCellRenderer *renderer,
	E2_ACLDlgRuntime *rt);
static void _e2p_acl_cell_edited_cb (GtkCellRendererText *cell,
	gchar *path_string, gchar *new_text, E2_ACLDlgRuntime *rt);
static void _e2p_acl_toggle_cb (GtkCellRendererToggle *cell,
	const gchar *path_str, E2_ACLDlgRuntime *rt);
static gint _e2p_acl_view_sort (GtkTreeModel *model,
		GtkTreeIter *a, GtkTreeIter *b, gpointer user_data);
static void _e2p_acl_fill_sortkey (GtkTreeModel *model, GtkTreeIter *iter);

extern pthread_mutex_t task_mutex;
extern volatile gpointer copyaclfunc;	//stored in task_backend file, a pointer to func used for preserving acl's when copying

#ifdef ACLEXTRA_ACTIONS
static gboolean _e2p_task_aclcopyQ (E2_ActionTaskData *qed);
#endif
static gboolean _e2p_task_aclQ (E2_ActionTaskData *qed);

//no. of visible columns in associated treeview
#define VISCOLS SORTKEY

static PluginIface iface;

static gchar *colnames[VISCOLS] =
{
	"",	//unused
	"UID/GID",	//untranslated
	N_("Read"), N_("Write"), N_("Exec"), N_("Whole")
};

static E2_ACLTask saved_task = E2_ACL_NODOWN | E2_ACL_SET | E2_ACL_SHOWN;	//for reinstating settings from last dialog

#define CLASSCOUNT 4
static gchar *classorder [CLASSCOUNT] = { "1", "2", "3", "4" }; //for sorting store-rows
static gchar *classnames [CLASSCOUNT] = { N_("User"), N_("Group"), N_("Mask"), N_("Other") };
static gchar *classinames [CLASSCOUNT];	//translated classnames, for runtime comparisons

/**
@brief get the state of permission flag @a perm in @a permset_d
This is an adaptation of a linux libacl function, courtesy of Andreas Gruenbacher
@param permset_d
@param perm one or more of ACL_READ | ACL_WRITE | ACL_EXECUTE

@return 1 if (any flag in) @a perm is set in @param permset_d, 0 if not set, -1 on error
*/
#ifndef E2ACL_POSIX_EXTENDED
# ifndef HAVE_ACL_GET_PERM
#  ifndef HAVE_ACL_GET_PERM_NP
static gint _e2p_get_perm (acl_permset_t permset, acl_perm_t perm)
{
	if (permset == NULL)
		return -1;
	//initial (and only) member of the struct is an acl_perm_t
	return (*((acl_perm_t *)permset) & perm) ? 1 : 0;
}
#  endif
# endif
#endif

/**
@brief apply the ACL(s) of @a src to @a dest
This func is called during copy-tasks or move-tasks that have reverted to a
between-device copy
VPATH's are used, though not needed until the acl lib supports non-local items
@param src absolute path string, localised, of item whose ACL's are to be copied
@param statprt pointer to stafbuf for @a src, filled by lstat()
@param dest absolute path string, localised, of item whose ACL's are to be set

@return TRUE if the process was completed successfully
*/
static gboolean _e2p_acl_copyacls (VPATH *src, const struct stat *statptr,
	VPATH *dest)
{
#ifdef E2_VFSTMP
//FIXME	decide if only local items can have acls managed
	if (src->spacedata != NULL || src->spacedata != NULL)
		return FALSE;
#endif
	gpointer workspace = acl_init (1);	//working heapspace for retrieved access and default ACLs
	if (workspace == NULL)
		//FIXME warn user
		return FALSE;

	gboolean retval;
	acl_t acl = acl_get_file (VPSTR(src), ACL_TYPE_ACCESS);
	retval = (acl != NULL) ?
		(acl_set_file (VPSTR(dest), ACL_TYPE_ACCESS, acl) == 0) : TRUE;
	if (S_ISDIR (statptr->st_mode))
	{
		acl_t acl = acl_get_file (VPSTR(src), ACL_TYPE_DEFAULT);
		if (acl != NULL)
			retval = retval && (acl_set_file (VPSTR(dest), ACL_TYPE_DEFAULT, acl) == 0);
	}
	acl_free (workspace);
	return retval;
}
/**
@brief callback function for recursive directory ACL-copying

Tree is being walked breadth-first, not physical.
Dirs are made accessible and writable if not already so and it's permitted,
dirs are added to a list to be processed after all the tree has been traversed,
other items are changed as requested (if possible). No recursion-flags are checked,
as all available ACL's are to be copied, here
Error messages assume BGL is open/off

@param localpath absolute path of item to change, localised string
@param statptr pointer to struct stat with info about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data pointer to user-specified data

@return E2TW_CONTINUE on success, others as appropriate
*/
static E2_TwResult _e2p_acl_twcb_copyacl (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, E2_CopyACLData *user_data)
{
	E2_TwResult retval = E2TW_CONTINUE;	//default error code = none
#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = user_data->otherdir->spacedata;
#endif
	gchar *dest;
	if (status != E2TW_DP)
	{
		struct stat othersb;
		//end-portion of localpath appended to user_data->otherdir
		dest = e2_utils_strcat (VPSTR (user_data->otherdir),
			VPSTR(localpath) + user_data->oldroot_len);
#ifdef E2_VFS
		ddata.path = dest;
		if (e2_fs_lstat (&ddata, &othersb E2_ERR_NONE())
#else
		if (e2_fs_lstat (dest, &othersb E2_ERR_NONE())
#endif
			|| ((othersb.st_mode & S_IFMT) != (statptr->st_mode & S_IFMT)))
		{
			g_free (dest);
			return retval;
		}
	}
	else
		dest = NULL;	//warning prevention

	switch (status)
	{
		E2P_DirEnt *dirfix;
		GList *member;

		case E2TW_DP:
			//for a matched dir, chacl its new mode, cleanup
			for (member = g_list_last (user_data->dirdata); member != NULL; member = member->prev)
			{
				dirfix = member->data;
				if (dirfix != NULL)
				{
					if (!strcmp (dirfix->path, localpath))
					{
//						if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
//						{
#ifdef E2_VFS
							ddata.path = dirfix->path;
							if (!_e2p_acl_copyacls (localpath, statptr, &ddata))
#else
							if (!_e2p_acl_copyacls (localpath, statptr, dest))
#endif
								retval = E2TW_FIXME;
/*						}
						else
						{
							//just reinstate original mode
							E2_ERR_DECLARE
#ifdef E2_VFS
							ddata.path = dirfix->path;
							if (e2_fs_chmod (&ddata, dirfix->mode E2_ERR_PTR())
#else
							if (e2_fs_chmod (dirfix->path, dirfix->mode E2_ERR_PTR())
#endif
								&& E2_ERR_ISNOT (ENOENT)) //FIXME vfs
								user_data->continued_after_problem = TRUE;
							E2_ERR_CLEAR
						}
*/
						g_free (dirfix->path);
						DEMALLOCATE (E2P_DirEnt, dirfix);
						user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
						break;
					}
				}
//				else //should never happen CHECKME ok when walking list ?
//					user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
			}
			break;
		case E2TW_DRR:	//directory now readable
//			if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
				retval |= E2TW_DRKEEP;	//no permission reversion in walker
		case E2TW_D:
			//ensure dir is writable, if we can
			if (e2_fs_tw_adjust_dirmode (localpath, statptr, (W_OK | X_OK)) == 0)
			{
//				if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
//				{
					//failed to set missing W and/or X perm
					//take a shot at doing any change, anyhow, probably fails
#ifdef E2_VFS
					_e2p_acl_copyacls (localpath, statptr, &ddata);
#else
					_e2p_acl_copyacls (localpath, statptr, dest);
#endif
					//FIXME warn user if either change fails
					retval |= E2TW_SKIPSUB;	//don't try to do any descendant
//				}
			}
			else	//dir can be processed
			{
				//add this dir to list of items to chacl afterwards
				dirfix = MALLOCATE (E2P_DirEnt);
				CHECKALLOCATEDWARNT (dirfix, retval=E2TW_STOP;break;)
				dirfix->path = g_strdup (VPSTR (localpath));
//				if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
//					dirfix->mode = ?;
//				else
//					dirfix->mode = statptr->st_mode & ALLPERMS;	//want to restore the original value
				user_data->dirdata = g_list_append (user_data->dirdata, dirfix);
			}
			break;
		case E2TW_DM:	//dir not opened (reported upstream)
		case E2TW_DL:	//ditto
		case E2TW_DNR:	//unreadable directory (for which, error is reported upstream)
						//touch for this will probably fail, but try anyhow
			//ensure dir is writable, if we can, don't need X permission
//			if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
//			{
				if (e2_fs_tw_adjust_dirmode (localpath, statptr, W_OK) == 0)
				{
					//failed to set missing W perm
					//take a shot at doing any change, anyhow, probably fails
#ifdef E2_VFS
					_e2p_acl_copyacls (localpath, statptr, &ddata);
#else
					_e2p_acl_copyacls (localpath, statptr, dest);
#endif
					//FIXME warn user about failure
					retval = E2TW_FIXME;
				}
				else	//dir can be processed
				{
#ifdef E2_VFS
					if (!_e2p_acl_copyacls (localpath, statptr, &ddata))
#else
					if (!_e2p_acl_copyacls (localpath, statptr, dest))
#endif
					//FIXME warn user about failure
						retval = E2TW_FIXME;
				}
//			}
			break;
		case E2TW_SL:	//no mode changes for links
		case E2TW_SLN:
			break;
		case E2TW_F:
//			if (user_data->task & E2_ACL_OTHER)
//			{
#ifdef E2_VFS
				if (!_e2p_acl_copyacls (localpath, statptr, &ddata))
#else
				if (!_e2p_acl_copyacls (localpath, statptr, dest))
#endif
				//FIXME warn user about failure
					retval = E2TW_FIXME;
//			}
			break;
		case E2TW_NS:	//un-statable item (for which, error is reported upstream)
			retval = E2TW_FIXME;
			break;
		default:
			retval = E2TW_STOP;
			break;
	}

	if (dest != NULL)
		g_free (dest);
	if (retval & E2TW_SKIPSUB)
		user_data->continued_after_problem = TRUE;
	if (retval & E2TW_FIXME)
	{
		user_data->continued_after_problem = TRUE;
		retval &= ~E2TW_FIXME;	//continue after bad item
	}
	return retval;
}

  /***************************/
 /* execution-related stuff */
/***************************/

/**
@brief convert data in @a acl to a shortform acl-string for later use

@param acl the data to process, may be NULL

@return newly-allocated string, or NULL if acl is NULL or upon error
*/
static gchar *_e2p_acl_create_mode_string_for_acl (acl_t acl)
{
	gchar *acl_text, *result;
	if (acl == NULL)
		return NULL;
#ifdef E2ACL_POSIX_EXTENDED
	acl_text = acl_to_any_text (acl, NULL, ',', TEXT_ABBREVIATE);
#else
	acl_text = acl_to_text (acl, NULL);
#endif
	if (acl_text == NULL)
		result = NULL;
	else
	{
#ifdef E2ACL_POSIX_EXTENDED
		result = g_strdup (acl_text);
#else
/*	condense the string like
	user::rw-
	user:lisa:rw-         #effective:r--
	group::r--
	group:444:rw-     #effective:r--
	mask::r--
	other::r-- */
		gchar *freeme = e2_utils_str_replace (acl_text, "user:", "u:");
		result = freeme;
		freeme = e2_utils_str_replace (result, "group:", "g:");
		g_free (result);
		result = freeme;
		freeme = e2_utils_str_replace (result, "mask:", "m:");
		g_free (result);
		result = freeme;
		freeme = e2_utils_str_replace (result, "other:", "o:");
		g_free (result);
		result = freeme;
		gchar *s, *p;
		//eliminate whitespace
		p = result;
		while ((s = e2_utils_find_whitespace (p)) != NULL)
		{
			p = e2_utils_pass_whitespace (s);
			if (p == NULL)
				break;
			memmove (s, p, strlen(p) + 1);
			p = s;
		}
		//eliminate comments
		p = result;
		while ((s = strchr (p, '#')) != NULL)
		{
			p++;
			while (p[0] != '\n' && p[0] != '\0')
				p++;
			memmove (s, p, strlen(p) + 1);
			p = s;
		}
		//eliminate newlines
		p = result;
		while ((s = strchr (p, '\n')) != NULL)
		{
			*p = ',';
			p++;
		}
#endif
		acl_free (acl_text);
	}
	acl_free (acl);
	return result;
}
/**
@brief convert ACL data in @a liststore to an array for later use

@param store pointer to liststore to process, may be NULL
@param rt pointer to dialog data struct

@return new array of changes data, or NULL if none found
*/
static GPtrArray *_e2p_acl_convert_store (GtkListStore *store, E2_ACLDlgRuntime *rt)
{
	GPtrArray *array = NULL;
	GtkTreeIter iter;

	if (store != NULL && gtk_tree_model_get_iter_first (GTK_TREE_MODEL (store), &iter))
	{
		E2_EntryChangeData *changes;
		gboolean rd, wr, ex, wh;
//		gboolean bigwhole = GTK_TOGGLE_BUTTON (rt->change_whole_btn)->active; BAD, can be set in invalid contexts
		gchar *qualifier, *key;
		acl_perm_t perms;
		array = g_ptr_array_sized_new (8);
		do
		{
			changes = ALLOCATE (E2_EntryChangeData);
			CHECKALLOCATEDWARN (changes, return array;);
			gtk_tree_model_get (GTK_TREE_MODEL (store), &iter,
				QUAL, &qualifier, READ, &rd, WRITE, &wr, EXEC, &ex,
				WHOLE, &wh, SORTKEY, &key, -1);
			//convert data
			switch (key[0])
			{
				case '1':	//user
					if (key[1] == '\0')
						changes->tag = ACL_USER_OBJ;
					else
						changes->tag = ACL_USER;
					break;
				case '2':	//group
					if (key[1] == '\0')
						changes->tag = ACL_GROUP_OBJ;
					else
						changes->tag = ACL_GROUP;
					break;
				case '3':	//mask
					if (key[1] == '\0')
						changes->tag = ACL_MASK;
					else
						changes->tag = ACL_UNDEFINED_TAG; //igonre this bad entry
					break;
				case '4':	//other
					if (key[1] == '\0')
						changes->tag = ACL_OTHER;
					else
						changes->tag = ACL_UNDEFINED_TAG; //igonre this bad entry
					break;
				default:
						changes->tag = ACL_UNDEFINED_TAG; //igonre this bad entry
					break;
			}

			if (changes->tag != ACL_UNDEFINED_TAG)
			{
				if (qualifier == NULL || *qualifier == '\0')
					changes->id = ACL_UNDEFINED_ID;
				else
				{
					gchar *idstring = e2_utf8_to_locale (qualifier);
					if (idstring != NULL)
					{
						if (changes->tag == ACL_USER)
						{
							struct passwd *pw_buf;
							if ((pw_buf = getpwnam (idstring)) != NULL)
								changes->id = (id_t) pw_buf->pw_uid;
							else
							{
								changes->id = (id_t) strtol (idstring, NULL, 10);
								if (errno == EINVAL)
								{
									//FIXME user 0 probably not relevant
								}
							}
						}
						else
						{
							struct group *grp_buf;
							if ((grp_buf = getgrnam (idstring)) != NULL)
								changes->id = (id_t) grp_buf->gr_gid;
							else
							{
								changes->id = (id_t) strtol (idstring, NULL, 10);
								if (errno == EINVAL)
								{
									//FIXME group 0 probably not relevant
								}
							}
						}
						g_free (idstring);
					}
				}

				perms = 0;
				if (rd)
					perms |= ACL_READ;
				if (wr)
					perms |= ACL_WRITE;
				if (ex)
					perms |= ACL_EXECUTE;
				changes->perms = perms;

				changes->whole = wh;	//|| bigwhole;	invalid in many cases
				g_ptr_array_add (array, changes);

			}

			if (qualifier != NULL)
				g_free (qualifier);
			g_free (key);
		} while (gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter));
	}
	return array;
}
/**
@brief verify some ACL data in @a liststore if we can
This checks types of entries, but not permissions
@param store pointer to liststore to process, may be NULL
@param task flags describing the type of change

@return TRUE if content is valid, FALSE if invalid NULL store or empty store, or if error in store
*/
static gboolean _e2p_acl_verify_store (GtkListStore *store, E2_ACLTask task)
{
/*entry-related rules:
MUST HAVES
user_obj unless the task is set+system or nuke (those use existing system data)
group_obj unless the task is set+system or nuke
other unless the task is set+system or nuke
PROHIBITED CHANGES
user_obj whole addition if there's already one (key "1")
user_obj (key "1") whole removal
user whole addition if there's one with the same qualifier (= keys "1+..")
group_obj whole addition if there's already one (key "2")
group_obj (key "2") whole removal
group whole addition if there's one with the same qualifier (= keys "2+..")
mask whole addition if there's already one (key "3")
mask whole (key "3") removal if there's any user (key "1+..") or group (key "2+..")
other whole addition if there's already one (key "4")
other (key "4") whole removal
*/
	GtkTreeIter iter;
	if (store != NULL && gtk_tree_model_get_iter_first (GTK_TREE_MODEL (store), &iter))
	{
		gboolean retval = TRUE;
		gboolean user, group, mask, other;	//the unique entries
		gboolean whole;
		gchar *qualifier, *key;
		GtkTreeIter srchiter;

		user = group = mask = other = FALSE;
		do
		{
			gtk_tree_model_get (GTK_TREE_MODEL (store), &iter, QUAL, &qualifier,
				WHOLE, &whole, SORTKEY, &key, -1);
			if (key != NULL)
			{
				switch (key[0])
				{
					case '1':	//user
						if (key[1] == '\0')	//unqualified entry
						{
							if (user)
								retval = FALSE;	//duplicate entry found
							else
							{
								retval = (!whole || (task & (E2_ACL_SET | E2_ACL_NUKE)));
								user = TRUE;
							}
						}
						else	//qualified entry
							if (whole && (task & (E2_ACL_SET | E2_ACL_ADD)))
						{
							//check for no duplicated user/qualifier
							srchiter = iter;	//starting here finds itself
							if (gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &srchiter)
								&& e2_tree_find_iter_from_str_same
								(GTK_TREE_MODEL (store), SORTKEY, key, &srchiter))
								retval = FALSE;
							//CHECKME does the store order have an impact here ?
						}
						break;
					case '2':	//group
						if (key[1] == '\0')
						{
							if (group)
								retval = FALSE;
							else
							{
								retval = (!whole || (task & (E2_ACL_SET | E2_ACL_NUKE)));
								group = TRUE;
							}
						}
						else
							if (whole && (task & (E2_ACL_SET | E2_ACL_ADD)))
						{
							//check for no duplicated group/qualifier
							srchiter = iter;	//starting here finds itself
							if (gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &srchiter)
								&& e2_tree_find_iter_from_str_same
								(GTK_TREE_MODEL (store), SORTKEY, key, &srchiter))
								retval = FALSE;
						}
						break;
					case '3':	//mask
						if (key[1] == '\0')
						{
							if (mask)
								retval = FALSE;
							else
							{
								retval = (!whole || (task & (E2_ACL_SET | E2_ACL_ADD | E2_ACL_REMOVE)));
								mask = TRUE;
							}
						}
						else
						{
							retval = FALSE;
						}
						break;
					case '4':	//other
						if (key[1] == '\0')
						{
							if (other)
								retval = FALSE;
							else
							{
								retval = (!whole || (task & (E2_ACL_SET | E2_ACL_NUKE)));
								other = TRUE;
							}
						}
						else
						{
							retval = FALSE;
						}
						break;
					default:
						retval = FALSE;
						break;
				}
				g_free (key);
			}
			else
				retval = FALSE;

			if (qualifier != NULL)
				g_free (qualifier);

			if (!retval)
				return FALSE;

		} while (gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter));

		if (user && group && other)
			return TRUE;
	}
	return (((task & E2_ACL_NUKE) || (task & (E2_ACL_SET | E2_ACL_SYSTEM))));
}
/**
@brief find the first entry in @a acl which matches @a type and @a id

@param acl the data to scan
@param type the type of entry that is wanted
@param id UID or GID for a qualified entry, or ACL_UNDEFINED_ID if unqualified

@return the matching entry in @a acl, or NULL if no match
*/
static acl_entry_t _e2p_acl_find_entry (acl_t acl, acl_tag_t type, id_t id)
{
#if defined (E2ACL_POSIX_EXTENDED) || defined (E2ACL_POSIX_FULL)
	acl_entry_t entry;

# ifdef FOREACH_ACL_ENTRY
	FOREACH_ACL_ENTRY (entry, acl)
# else
	gint result = acl_get_entry (acl, ACL_FIRST_ENTRY, &entry);
	while (result == 1)
# endif
	{
		acl_tag_t thistype;

		acl_get_tag_type (entry, &thistype);
		if (type == thistype)
		{
			if (id == ACL_UNDEFINED_ID)
				return entry;
			else
			{
				id_t *this_id;
				this_id = acl_get_qualifier (entry);
				if (this_id != NULL)
				{
					if (*this_id == id)
					{
						acl_free (this_id);
						return entry;
					}
					acl_free (this_id);
				}
			}
		}
# ifndef FOREACH_ACL_ENTRY
		result = acl_get_entry (acl, ACL_NEXT_ENTRY, &entry);
# endif
	}
#endif //defined () ....
#ifdef E2ACL_POSIX_RESTRICTED
# warning _e2p_acl_find_entry() is inoperative
#endif
	return NULL;
}
/**
@brief create an entry in @a acl with parameters in accord with supplied arguments

@param aclptr address to store acl which is being updated
@param entryptr address to store created entry
@param tag the type of entry that is wanted
@param id UID or GID for a qualified entry, or ACL_UNDEFINED_ID if unqualified
@param perms flags for permissions in the new entry

@return TRUE if creation succeeded
*/
static gboolean _e2p_acl_create_entry (acl_t *aclptr, acl_entry_t *entryptr,
	acl_tag_t tag, id_t id, acl_perm_t perms)
{
#if defined (E2ACL_POSIX_EXTENDED) || defined (E2ACL_POSIX_FULL)
	if (acl_create_entry (aclptr, entryptr))
	{
		//FIXME warn user, clean entry if appropriate
		return FALSE;
	}
	else
	{
		gpointer qualifier;
		acl_permset_t permset;
		acl_set_tag_type (*entryptr, tag);
		printd (DEBUG, "_e2p_acl_create_entry - tag %d", tag);
		if (tag == ACL_USER || tag == ACL_GROUP)
		{
			qualifier = acl_get_qualifier (*entryptr);
			if (qualifier != NULL)
			{
				*((id_t *)qualifier) = id;	//ok ??
				acl_set_qualifier (*entryptr, qualifier);
				acl_free (qualifier);	//??
			}
			else
			{
				acl_free (entryptr);
				return FALSE;
			}
		}
		acl_get_permset (*entryptr, &permset);
//		if (permset != NULL)
//		{
			acl_clear_perms (permset);
			acl_add_perm (permset, perms);
			acl_set_permset (*entryptr, permset);
			acl_free (permset); //??
//		}
//		else
//		{ WARNING; acl_free (entryptr); return FALSE; }
		return TRUE;
	}
#endif //defined () ....
#ifdef E2ACL_POSIX_RESTRICTED
# warning _e2p_acl_create_entry() is inoperative
	return FALSE;
#endif
}
/* *
@brief revert an access-ACL to mode-equivalent values or delete default-ACL if one exists for @a localpath

@param localpath localised absolute path string of item to process
@param type enumerator of type of ACL to be cleared

@return 0 if successful, -1 on error
*/
//CHECKME do both ACL's if item is a dir ??
/*static gint _e2p_acl_delete_acl (gchar *localpath, acl_type_t type)
{
	gint result;
	if (type == ACL_TYPE_ACCESS)
	{	//convert access-ACL to mode-equivalent form
		//CHECKME get current mode and explicitly set corresponding perms in retained entries
		acl_t acl = acl_get_file (localpath, ACL_TYPE_ACCESS);
		if (acl == NULL)
			return -1;

		acl_entry_t entry;
		acl_tag_t tag;

#ifdef FOREACH_ACL_ENTRY
		FOREACH_ACL_ENTRY (entry, acl)
#else
		result = acl_get_entry (acl, ACL_FIRST_ENTRY, &entry);
		while (result == 1)
#endif
		{
			acl_get_tag_type (entry, &tag);
			switch (tag)
			{
				case ACL_USER:
				case ACL_MASK:
				case ACL_GROUP:
					acl_delete_entry (acl, entry);
				default:
					break;
			}
#ifndef FOREACH_ACL_ENTRY
			result = acl_get_entry (acl, ACL_NEXT_ENTRY, &entry);
#endif
		}
		result = acl_set_file (localpath, ACL_TYPE_ACCESS, acl);
	}
	else	//want ACL_TYPE_DEFAULT
		result = acl_delete_def_file (localpath);
	return result;
}
*/
/**
@brief create an acl which conforms to @a mode

@param mode mode_t flags as returned by stat() or ~umask()
@param external TRUE to migrate the created acl to user-land before returning it

@return the created acl or NULL upon error
*/
static acl_t _e2p_acl_get_for_mode (mode_t mode)	//, gboolean external)
{
#if defined (E2ACL_POSIX_EXTENDED) || defined (E2ACL_POSIX_FULL)
# ifdef E2ACL_POSIX_EXTENDED
	return (acl_from_mode (mode));
/*	acl_t acl = acl_from_mode (mode);
	if (acl == NULL)
		return NULL;
	if (!external)
		return acl;

	ssize_t bsize = acl_size (acl);
	gpointer buf = g_try_malloc (bsize * 2);	//CHECKME this extra space needed for manipulation ?
#if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (buf, acl_free(acl);return NULL;)
#else
	if (buf == NULL)
	{
		acl_free (acl);
		return NULL;
	}
#endif
	acl_copy_ext (buf, acl, bsize);
	acl_free (acl);
	return (acl_t)buf;
*/
# else
	acl_t acl;	//pointer
	acl_entry_t entry;	//pointer
	acl_permset_t permset;	//pointer

	acl = acl_init (1);
	if (acl == NULL)
		return NULL;

	if (acl_create_entry (&acl, &entry))
		goto failed3;
	if (acl_get_permset (entry, &permset))
		goto failed2;
	if (mode & S_IRUSR)
		acl_add_perm (permset, ACL_READ);
	if (mode & S_IWUSR)
		acl_add_perm (permset, ACL_WRITE);
	if (mode & S_IXUSR)
		acl_add_perm (permset, ACL_EXECUTE);
	acl_set_permset (entry, permset);
	acl_set_tag_type (entry, ACL_USER_OBJ);
	if (acl_create_entry (&acl, &entry))
		goto failed;
	acl_free (permset);
	acl_free (entry);

	if (acl_create_entry (&acl, &entry))
		goto failed3;
	if (acl_get_permset (entry, &permset))
		goto failed2;
	if (mode & S_IRGRP)
		acl_add_perm (permset, ACL_READ);
	if (mode & S_IWGRP)
		acl_add_perm (permset, ACL_WRITE);
	if (mode & S_IXGRP)
		acl_add_perm (permset, ACL_EXECUTE);
	acl_set_permset (entry, permset);
	acl_set_tag_type (entry, ACL_GROUP_OBJ);
	if (acl_create_entry (&acl, &entry))
		goto failed;
	acl_free (permset);
	acl_free (entry);


	if (acl_create_entry (&acl, &entry))
		goto failed3;
	if (acl_get_permset (entry, &permset))
		goto failed2;
	if (mode & S_IROTH)
		acl_add_perm (permset, ACL_READ);
	if (mode & S_IWOTH)
		acl_add_perm (permset, ACL_WRITE);
	if (mode & S_IXOTH)
		acl_add_perm (permset, ACL_EXECUTE);
	acl_set_permset (entry, permset);
	acl_set_tag_type (entry, ACL_OTHER);
	if (acl_create_entry (&acl, &entry))
		goto failed;
	acl_free (permset);
	acl_free (entry);

	return acl;
/*	if (!external)
		return acl;

	ssize_t bsize = acl_size (acl);
	gpointer buf = g_try_malloc (bsize * 2);	//extra space needed for manipulation ?
#if (CHECKALLOCATEDWARN)
	CHECKALLOCATEDWARN (buf, acl_free(acl);return NULL;)
#else
	if (buf == NULL)
	{
		acl_free (acl);
		return NULL;
	}
#endif
	acl_copy_ext (buf, acl, bsize);
	acl_free (acl);
	return (acl_t)buf;
*/
failed:
	acl_free (permset);
failed2:
	acl_free (entry);
failed3:
	acl_free (acl);
	return NULL;
# endif
#endif //defined () ....
#ifdef E2ACL_POSIX_RESTRICTED
# warning _e2p_acl_get_for_mode() is inoperative
	return NULL;
#endif
}
/**
@brief set ACL of type @a type for @a localpath
This assumes BGL is on/closed
@param localpath localised absolute path string of item to process
@param type enumerator of type of ACL to be applied
@param acl the acl to apply

@return TRUE if successful
*/
static gboolean _e2p_acl_apply (gchar *localpath, acl_type_t type, acl_t acl)
{
#ifdef DISABLEACLCHANGE
	printd (DEBUG, "application of ACL changes is disabled");
	return FALSE;
#else
	if (acl_set_file (localpath, type, acl))
	{
		gchar *typename = (type == ACL_TYPE_ACCESS) ? _("General ACL") : _("Directory ACL");
		gchar *stracl = _e2p_acl_create_mode_string_for_acl (acl);
		if (stracl == NULL)
			stracl = "";	//this causes hiccup in message
		gchar *utf = F_DISPLAYNAME_FROM_LOCALE (localpath);
		gchar *msg = g_strdup_printf (_("Cannot apply %s '%s' for %s"),
			typename, stracl, utf);
		F_FREE (utf, localpath);
		e2_output_print_error (msg, TRUE);
		if (*stracl != '\0')
			g_free (stracl);
		printd (DEBUG, "_e2p_acl_apply FAILED");
		return FALSE;
	}
	printd (DEBUG, "_e2p_acl_apply succeeded");
	return TRUE;
#endif
}
/**
@brief check that @a acl is ok to apply
This is always used immediately before tha application function, and prevents
that if the acl is invalid
This assumes BGL is on/closed
@param localpath item path, localised string, for any error message
@param type enumerator of type of ACL
@param acl the acl to test

@return TRUE if test is passed
*/
static gboolean _e2p_acl_validate (gchar *localpath, acl_type_t type, acl_t acl)
{
	if (acl_valid (acl))
	{
		gchar *typename = (type == ACL_TYPE_ACCESS) ? _("General ACL") : _("Directory ACL");
		gchar *stracl = _e2p_acl_create_mode_string_for_acl (acl);
		if (stracl == NULL)
			stracl = "";
		gchar *utf = F_DISPLAYNAME_FROM_LOCALE (localpath);
		gchar *longmsg = g_strdup_printf (_("Cannot apply %s '%s' for %s - Invalid"),
			typename, stracl, utf);
		e2_output_print_error (longmsg, TRUE);
		if (*stracl != '\0')
			g_free (stracl);
		F_FREE (utf, localpath);
		return FALSE;
	}
	return TRUE;
}
/**
@brief in accord with @a task, apply to @a localpath an acl based only on its system permissions
This is called only for nuke task or set/system task when there is no
existing acl for @a localpath
@param localpath localised absolute path string of item to process
@param statptr pointer to stafbuf for @a localpath, filled by lstat()
@param type enumerator of the type of acl to use
@param task flags dictating type of change to be done

@return TRUE if successful
*/
static gboolean _e2p_acl_apply_basic (gchar *localpath, const struct stat *statptr,
	acl_type_t type, E2_ACLTask task)
{
	//only call here with (task & E2_ACL_NUKE) OR (task & E2_ACL_SYSTEM)
	printd (DEBUG, "_e2p_acl_apply_basic: type %d, task %d", type, task);
	gboolean retval = TRUE;
	if (!S_ISLNK (statptr->st_mode))	//ignore links
	{
		acl_t acl;
		if (type == ACL_TYPE_ACCESS)
		{
			if (!S_ISDIR (statptr->st_mode) || (task & E2_ACL_DIRAXS))
			{
				acl = acl_get_file (localpath, ACL_TYPE_ACCESS);
				if (acl != NULL || (task & (E2_ACL_SET | E2_ACL_SYSTEM)))
				{
					if (acl != NULL)
						acl_free (acl);
					acl = _e2p_acl_get_for_mode (statptr->st_mode);
					if (acl != NULL)
					{
						//don't bother with validation ??
						//retval = (_e2p_acl_validate (localpath, ACL_TYPE_ACCESS, acl)
						//			&& _e2p_acl_apply (localpath, ACL_TYPE_ACCESS, acl));
						if (!_e2p_acl_apply (localpath, ACL_TYPE_ACCESS, acl))
							retval = FALSE;
						acl_free (acl);
					}
					else
						retval = FALSE;
				}
			}
		}

		else if (type == ACL_TYPE_DEFAULT	//should never fail
				&& S_ISDIR (statptr->st_mode)	//ditto
				&& (task & E2_ACL_DIRDEF))
		{
			if (task & E2_ACL_NUKE)
			{
				acl = acl_get_file (localpath, ACL_TYPE_DEFAULT);
				if (acl != NULL)
				{
					acl_free (acl);
					if (acl_delete_def_file (localpath))
						retval = FALSE;
				}
			}
			else	//not a nuke
			{
				mode_t mode = umask (0);
				umask (mode);
				acl = _e2p_acl_get_for_mode (~mode);
				if (acl != NULL)
				{
					//don't bother with validation ??
					//retval = (_e2p_acl_validate (localpath, ACL_TYPE_DEFAULT, acl)
					//			&& _e2p_acl_apply (localpath, ACL_TYPE_DEFAULT, acl));
					if (!_e2p_acl_apply (localpath, ACL_TYPE_DEFAULT, acl))
						retval = FALSE;
					acl_free (acl);
				}
				else
					retval = FALSE;
			}
		}
	}
	return retval;
}
/**
@brief in accord with @a task, apply to @a localpath an acl based only on @a changedata

@param localpath absolute path string, localised, of item to update
@param statptr pointer to stafbuf for @a localpath, filled by lstat()
@param type enumerator to determine access- or extended-ACL
@param task flags dictating the type of change to make
@param changedata pointer-array of change-data structs for @a type (possibly NULL)

@return TRUE if the procedure was completed successfully
*/
static gboolean _e2p_acl_apply_shown (gchar *localpath, const struct stat *statptr,
	acl_type_t type, E2_ACLTask task, GPtrArray *changedata)
{
/* if type IS ACL_TYPE_ACCESS AND (non-dir OR (dir AND applying axs))
	OR
   if type IS ACL_TYPE_DEFAULT AND dir AND applying def

	if no existing acl
   if remove ... do nothing
   if add or set ... try for new acl, if ok, populate it with array data

    if existing acl
	foreach member of array
	seek match in acl
   if set ... if found, replace perms (whole is irrelevant)
   if set ... if NOT found, AND whole-flag, add it whole per change data (can't just add perms)
   if add ... if found, add perms (whole is irrelevant)
   if add ... if NOT found, AND whole-flag, add it whole per change data (can't just add perms)
   if remove ... if found, remove whole or perms
   if remove ... if NOT found, do nothing

	if any change, verify acl
	if ok, apply return TRUE
	report error return FALSE
*/
	gboolean retval = TRUE;
	if (!S_ISLNK (statptr->st_mode) && changedata != NULL)
	{
		if (
			(type == ACL_TYPE_ACCESS && (!S_ISDIR (statptr->st_mode) || (task & E2_ACL_DIRAXS)))
			||
			(type == ACL_TYPE_DEFAULT && S_ISDIR (statptr->st_mode) && (task & E2_ACL_DIRDEF))
 			)
		{
			guint count;
			acl_entry_t entry;
			E2_EntryChangeData **iterator;
			gboolean set, add, remove, dirty;
			add = (task & E2_ACL_ADD);
			remove = (add) ? FALSE : (task & E2_ACL_REMOVE);
			set = !(add || remove) || (task & E2_ACL_SET);
			acl_t acl = acl_get_file (localpath, type);
			if (acl == NULL)
			{
				printd (DEBUG, "_e2p_acl_apply_shown - NO acl exists");
				if (remove)
				{
					retval = TRUE;
				}
				else
				{
					acl = acl_init (1);
					if (acl == NULL)
						//MESSAGE ??
						retval = FALSE;
					else
					{
						dirty = FALSE;
						//populate acl from array
						iterator = (E2_EntryChangeData **) changedata->pdata;
						for (count = 0; count < changedata->len; count++, iterator++)
						{
							if ((*iterator)->whole)
							{
								if (_e2p_acl_create_entry (&acl, &entry,
										(*iterator)->tag, (*iterator)->id,
										(*iterator)->perms))
									dirty = TRUE;
								else
									retval = FALSE;
							}
						}
						if (dirty && retval)
							retval = (_e2p_acl_validate (localpath, type, acl)
									&& _e2p_acl_apply (localpath, type, acl));
						//FIXME acl_free (entry); somewhere
						acl_free (acl);
					}
				}
			}
			else //acl exists
			{
				dirty = FALSE;
				acl_permset_t permset;
				iterator = (E2_EntryChangeData **) changedata->pdata;
				for (count = 0; count < changedata->len; count++, iterator++)
				{
					entry = _e2p_acl_find_entry (acl, (*iterator)->tag, (*iterator)->id);
					if (entry != NULL)
					{
#if defined (E2ACL_POSIX_EXTENDED) || defined (E2ACL_POSIX_FULL)
						//whole is irrelevant for set, add to existing entry
						if (set || add || (remove && !(*iterator)->whole))
						{
							acl_get_permset (entry, &permset);
							if (permset != NULL)
							{
								if (set) //replace perms
									acl_clear_perms (permset);
								if (set || add)
									acl_add_perm (permset, (*iterator)->perms);
								else
									acl_delete_perm (permset, (*iterator)->perms);
								acl_set_permset (entry, permset);
								acl_free (permset);
								dirty = TRUE;
							}
							else
							{
								retval = FALSE;
							}
						}
						else //remove whole
						{
							if (!acl_delete_entry (acl, entry))
								dirty = TRUE;
							else
								retval = FALSE;
						}
#endif //defined () ....
#ifdef E2ACL_POSIX_RESTRICTED
# warning _e2p_acl_apply_shown() is disfunctional
#endif
					}
					else	//no matching entry now
					{
					   if ((set || add) && (*iterator)->whole)
					   {
							if (_e2p_acl_create_entry (&acl, &entry,
									(*iterator)->tag, (*iterator)->id,
									(*iterator)->perms))
							{
								dirty = TRUE;
							}
							else
							{
								retval = FALSE;
							}
					   }
					}
				}
				if (dirty && retval)
					retval = (_e2p_acl_validate (localpath, type, acl)
								&& _e2p_acl_apply (localpath, type, acl));
				//FIXME acl_free (entry); somewhere
				acl_free (acl);
			}
		}
	}
	return retval;
}
/**
@brief in accord with @a task, apply to @a localpath an acl based system data modified by @a changedata

@param localpath absolute path string, localised, of item to update
@param statprt pointer to stafbuf for @a localpath, filled by lstat()
@param type enumerator to determine access- or extended-ACL
@param task flags dictating the type of change to make
@param changedata pointer-array of change-data structs for @a type, to be applied (possibly NULL)

@return TRUE if the procedure was completed successfully
*/
static gboolean _e2p_acl_apply_modified (gchar *localpath, const struct stat *statptr,
	acl_type_t type, E2_ACLTask task, GPtrArray *changedata)
{
/* if type IS ACL_TYPE_ACCESS AND (non-dir OR (dir AND applying axs)) AND item's axsacl exists or, if adding, a mode-derived acl can be created
	OR
   if type IS ACL_TYPE_DEFAULT AND (dir AND applying def) AND its defacl exists or, if adding, a umask-derived acl can be created

	get existing acl or, if adding, a new one based on mode|umask
	foreach member of array
	seek match in acl
   if set ... if found, replace perms (whole-flag is irrelevant)
   if set ... if NOT found, AND whole-flag, add it whole per change data
   if set ... if NOT found, AND NOT whole-flag, do nothing
   if add/remove ... if found, change it in part or whole per change data (add perms, remove whole or perms)
   if add/remove ... if NOT found, do nothing

	if any change, verify acl
	if ok, apply return TRUE
	report error return FALSE
*/
	gboolean retval = TRUE;
	if (!S_ISLNK (statptr->st_mode) && changedata != NULL)
	{
		mode_t mode;
		gboolean set, add, remove;
		add = (task & E2_ACL_ADD);
		remove = (add) ? FALSE : (task & E2_ACL_REMOVE);
		set = !(add || remove) || (task & E2_ACL_SET);

		acl_t acl = acl_get_file (localpath, type);
		if (acl == NULL)
		{
			if (add)
			{
				if (type == ACL_TYPE_ACCESS
					|| !S_ISDIR (statptr->st_mode))
					mode = statptr->st_mode;
				else
				{
					mode = umask (0);
					umask (mode);
					mode = ~mode & ALLPERMS;
				}
				acl = _e2p_acl_get_for_mode (mode);
				retval = (acl != NULL);
			}
		}
		if (acl != NULL)
		{
			if (
				(type == ACL_TYPE_ACCESS &&
				(!S_ISDIR (statptr->st_mode) || (task & E2_ACL_DIRAXS)))
				||
				(type == ACL_TYPE_DEFAULT &&
				S_ISDIR (statptr->st_mode) && (task & E2_ACL_DIRDEF))
	 			)
			{
				guint count;
				acl_entry_t entry;
				acl_permset_t permset;
				E2_EntryChangeData **iterator;
				gboolean dirty = FALSE;
				iterator = (E2_EntryChangeData **) changedata->pdata;
				for (count = 0; count < changedata->len; count++, iterator++)
				{
					entry = _e2p_acl_find_entry (acl, (*iterator)->tag, (*iterator)->id);
					if (entry != NULL)
					{
#if defined (E2ACL_POSIX_EXTENDED) || defined (E2ACL_POSIX_FULL)
						//whole is irrelevant for set, add to existing entry
						//CHECKME ignore set for sysmod ?
   						if (set || add || (remove && !(*iterator)->whole))
						{
   							//replace perms (whole is irrelevant)
							acl_get_permset (entry, &permset);
							if (permset != NULL)
							{
//modifying, not replacing		if (set)
//									acl_clear_perms (permset);
								if (set || add)
									acl_add_perm (permset, (*iterator)->perms);
								else
									acl_delete_perm (permset, (*iterator)->perms);
								acl_set_permset (entry, permset);
								acl_free (permset);
								dirty = TRUE;
							}
							else
							{
								retval = FALSE;
							}
						}
						else //remove whole
						{
							if (!acl_delete_entry (acl, entry))
								dirty = TRUE;
							else
								retval = FALSE;
						}
#endif //defined () ....
#ifdef E2ACL_POSIX_RESTRICTED
# warning _e2p_acl_apply_modified() is disfunctional
#endif
					}
					else	//no existing entry
					{
						if (set && (*iterator)->whole)
						{
							if (_e2p_acl_create_entry (&acl, &entry,
									(*iterator)->tag, (*iterator)->id,
									(*iterator)->perms))
								dirty = TRUE;
							else
								retval = FALSE;
						}
					}
				}
				if (dirty && retval)
					retval = (_e2p_acl_validate (localpath, type, acl)
								&& _e2p_acl_apply (localpath, type, acl));
				//FIXME acl_free (entry); somewhere
			}
			acl_free (acl);
		}
	}
	return retval;
}
/**
@brief apply changes expressed in @a task and @a changes to @a localpath

This is used when changing a single item, or recursively changing items.
Possible errors:
ENOTSUP this ACL operation isn't supported

@param localpath absolute path string, localised, of item to update
@param statprt pointer to stafbuf for @a localpath, filled by lstat()
@param type enumerator to determine access- or extended-ACL
@param task flags dictating the type of change to make
@param array pointer-array of change-data structs to be applied, possibly NULL

@return TRUE if the procedure was completed successfully
*/
static gboolean _e2p_acl_change1 (gchar *localpath, const struct stat *statptr,
	acl_type_t type, E2_ACLTask task, GPtrArray *array)
{

/*	mode_t mode = umask (0);
	umask (mode);
	acl_t debug = _e2p_acl_get_for_mode (~mode & ALLPERMS, TRUE);
	if (debug == NULL)
		printd (DEBUG, "external acl creation failed");
	else
	{
		printd (DEBUG, "external acl creation succeeded");
		g_free (debug);
	}
	debug = _e2p_acl_get_for_mode (~mode, FALSE);
	if (debug == NULL)
		printd (DEBUG, "internal acl creation failed");
	else
	{
		printd (DEBUG, "internal acl creation succeeded");
		acl_free (debug);
	}
*/
	gboolean retval;

	//some tasks use basic data only
	if (task & E2_ACL_NUKE)
 		retval = _e2p_acl_apply_basic (localpath, statptr, type, task);
	//add/system and remove/system are invalid options
	//(should never happen, and ignored if they do)
//	else if (task & (E2_ACL_SET | E2_ACL_SYSTEM))
	else if (task & E2_ACL_SYSTEM)
	{
/*		acl_t acl = acl_get_file (localpath, type);
		if (acl == NULL)
		{
*/
	 		retval = _e2p_acl_apply_basic (localpath, statptr, type, task);
/*		}
		else
		{	//no point in setting things that are already set
			acl_free (acl);
			retval = TRUE;
		}
*/
	}

	else if (task & E2_ACL_SHOWN)
		retval = _e2p_acl_apply_shown (localpath, statptr, type, task, array);

	else if (task & E2_ACL_SYSMOD)	//should never fail
		retval = _e2p_acl_apply_modified (localpath, statptr, type, task, array);

	else
		retval = TRUE;

	return retval;
}
/**
@brief callback function for recursive directory ACL-change

Tree is being walked breadth-first, not physical.
Dirs are made accessible and writable if not already so and it's permitted,
dirs are added to a list to be processed after all the tree has been traversed,
other items are changed as requested (if possible)
Error messages assume BGL is open/off

@param localpath absolute path of item to change, localised string
@param statptr pointer to struct stat with info about @a localpath
@param status code from the walker, indicating what type of report it is
@param user_data pointer to user-specified data

@return E2TW_CONTINUE on success, others as appropriate
*/
static E2_TwResult _e2p_acl_twcb_chacl (VPATH *localpath,
	const struct stat *statptr, E2_TwStatus status, E2_ChACLData *user_data)
{
	E2_TwResult retval = E2TW_CONTINUE;	//default error code = none
	switch (status)
	{
		E2P_DirEnt *dirfix;
#ifdef E2_VFS
		VPATH ddata;
		ddata.spacedata = localpath->spacedata;
#endif
		GList *member;

		case E2TW_DP:
			//chacl dir for its new mode, cleanup
			for (member = g_list_last (user_data->dirdata); member != NULL; member = member->prev)
			{
				dirfix = member->data;
				if (dirfix != NULL)
				{
					if (!strcmp (dirfix->path, localpath))
					{
						if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
						{
							gboolean dirsuccess;
							if (user_data->task & E2_ACL_DIRAXS)
								dirsuccess = _e2p_acl_change1 (dirfix->path,
									statptr, ACL_TYPE_ACCESS, user_data->task,
									user_data->axs_changes);
							else
								 dirsuccess = TRUE;
							if (user_data->task & E2_ACL_DIRDEF)
								dirsuccess = dirsuccess && _e2p_acl_change1 (dirfix->path,
									statptr, ACL_TYPE_DEFAULT, user_data->task,
									user_data->def_changes);
							if (!dirsuccess)
								user_data->continued_after_problem = TRUE;
						}
						else
						{
							//just reinstate original mode
							E2_ERR_DECLARE
#ifdef E2_VFS
							ddata.path = dirfix->path;
							if (e2_fs_chmod (&ddata, dirfix->mode E2_ERR_PTR())
#else
							if (e2_fs_chmod (dirfix->path, dirfix->mode E2_ERR_PTR())
#endif
								&& E2_ERR_ISNOT (ENOENT)) //FIXME vfs
								user_data->continued_after_problem = TRUE;
							E2_ERR_CLEAR
						}
						g_free (dirfix->path);
						DEMALLOCATE (E2P_DirEnt, dirfix);
						user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
						break;
					}
				}
//				else //should never happen CHECKME ok when walking list ?
//					user_data->dirdata = g_list_delete_link (user_data->dirdata, member);
			}
			break;
		case E2TW_DRR:	//directory now readable
			if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
				retval |= E2TW_DRKEEP;	//no permission reversion in walker
		case E2TW_D:
			//ensure dir is writable, if we can
			if (e2_fs_tw_adjust_dirmode (localpath, statptr, (W_OK | X_OK)) == 0)
			{
				if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
				{
					//failed to set missing W and/or X perm
					//take a shot at doing any change, anyhow, probably fails
					if (user_data->task & E2_ACL_DIRAXS)
						_e2p_acl_change1 ((gchar *)localpath, statptr, ACL_TYPE_ACCESS,
							user_data->task, user_data->axs_changes);
					if (user_data->task & E2_ACL_DIRDEF)
						_e2p_acl_change1 ((gchar *)localpath, statptr, ACL_TYPE_DEFAULT,
							user_data->task, user_data->def_changes);
					//FIXME warn user if either change fails
					retval |= E2TW_SKIPSUB;	//don't try to do any descendant
				}
			}
			else	//dir can be processed
			{
				//add this dir to list of items to chacl afterwards
				dirfix = MALLOCATE (E2P_DirEnt);
				CHECKALLOCATEDWARNT (dirfix, retval=E2TW_STOP;break;)
				dirfix->path = g_strdup (VPSTR (localpath));
//				if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
//					dirfix->mode = ?;
//				else
					dirfix->mode = statptr->st_mode & ALLPERMS;	//want to restore the original value
				user_data->dirdata = g_list_append (user_data->dirdata, dirfix);
			}
			break;
		case E2TW_DM:	//dir not opened (reported upstream)
		case E2TW_DL:	//ditto
		case E2TW_DNR:	//unreadable directory (for which, error is reported upstream)
						//touch for this will probably fail, but try anyhow
			if (user_data->task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
			{
				//ensure dir is writable, if we can, don't need X permission
				if (e2_fs_tw_adjust_dirmode (localpath, statptr, W_OK) == 0)
				{
					//failed to set missing W perm
					//take a shot at doing any change, anyhow, probably fails
					if (user_data->task & E2_ACL_DIRAXS)
						_e2p_acl_change1 ((gchar *)localpath, statptr, ACL_TYPE_ACCESS,
							user_data->task, user_data->axs_changes);
					if (user_data->task & E2_ACL_DIRDEF)
						_e2p_acl_change1 ((gchar *)localpath, statptr, ACL_TYPE_DEFAULT,
							user_data->task, user_data->def_changes);
					//FIXME warn user about failure
					retval = E2TW_FIXME;
				}
				else	//dir can be processed
				{
					gboolean dirsuccess;
					if (user_data->task & E2_ACL_DIRAXS)
						dirsuccess = _e2p_acl_change1 ((gchar *)localpath,
							statptr, ACL_TYPE_ACCESS, user_data->task,
							user_data->axs_changes);
					else
						dirsuccess = TRUE;
					if (user_data->task & E2_ACL_DIRDEF)
						dirsuccess = dirsuccess && _e2p_acl_change1 ((gchar *)localpath,
							statptr, ACL_TYPE_DEFAULT, user_data->task,
							user_data->def_changes);
					if (!dirsuccess)
					//FIXME warn user about failure
						retval = E2TW_FIXME;
				}
			}
			break;
		case E2TW_SL:	//no mode changes for links
		case E2TW_SLN:
			break;
		case E2TW_F:
			if (user_data->task & E2_ACL_OTHER)
			{
				if (!_e2p_acl_change1 ((gchar *)localpath, statptr, ACL_TYPE_ACCESS,
						user_data->task, user_data->axs_changes))
				//FIXME warn user about failure
					retval = E2TW_FIXME;
			}
			break;
		case E2TW_NS:	//un-statable item (for which, error is reported upstream)
			retval = E2TW_FIXME;
			break;
		default:
			retval = E2TW_STOP;
			break;
	}

	if (retval & E2TW_SKIPSUB)
		user_data->continued_after_problem = TRUE;
	if (retval & E2TW_FIXME)
	{
		user_data->continued_after_problem = TRUE;
		retval &= ~E2TW_FIXME;	//continue after bad item
	}
	return retval;
}
/**
@brief change extended permissions of item @a path to @a mode

Only an item's owner (as judged by the effective uid of the process)
or a privileged user, can change item permissions.
If invoked on a non-dir, or a dir without recursion, it is processed
here. If recursion is invoked on a dir, a ntfw funtion is invoked.
By that, all nested dir access permissons will be set to include x,
non-dir items will be have their new permissions set when
they are 'reported'. Finally, here, dirs will be set to their proper
permissions (bottom-up order) at the end of the process
Recursive chmod works on the host filesystem only.
Links are not affected, their target is processed. (OK ??)
Assumes BGL is open
@param localpath absolute path of item to process, localised string
@param axs_changes array of change-data for access-ACL, or NULL
@param def_changes array of change-data for default-ACL, or NULL
@param task flags indicating the way the operation is to be done
@return TRUE if operation succeeds
*/
static gboolean _e2p_acl_change (VPATH *localpath, GPtrArray *axs_changes,
	GPtrArray *def_changes, E2_ACLTask task)
{
	gboolean retval;
	E2_ChACLData data;
	struct stat statbuf;
	E2_ERR_DECLARE

	if (!(task & E2_ACL_NODOWN))
	{
		//decide whether src is a dir or not
		if (e2_fs_stat (localpath, &statbuf E2_ERR_PTR()))  //looks _through_ links
		{
			//abort if we can't find the item
			e2_fs_error_local (_("Cannot get information about %s"), localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
		if (S_ISDIR (statbuf.st_mode))
		{	//recursive chacl
			//park parameters where they can be accessed by helpers
			data.continued_after_problem = FALSE;
			data.task = task;
			data.axs_changes = axs_changes;
			data.def_changes = def_changes;
			data.dirdata = NULL;	//no processed dirs yet

			retval = e2_fs_tw (localpath, _e2p_acl_twcb_chacl, &data, -1,
				//flags for: no thru-links, this filesystem only, breadth-first
				E2TW_MOUNT | E2TW_PHYS E2_ERR_NONE());

			if (data.dirdata != NULL)
			{	//change/revert dir permissions, LIFO (=bottom-up) order
				GList *member;
				for (member = g_list_last (data.dirdata); member!= NULL; member = member->prev)
				{
					gboolean dirsuccess;
					E2P_DirEnt *dirfix = member->data;
					if (task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF))
					{
						if (task & E2_ACL_DIRAXS)
							dirsuccess = _e2p_acl_change1 (dirfix->path, &statbuf,
								ACL_TYPE_ACCESS, task, axs_changes);
						else
							dirsuccess = TRUE;
						if (task & E2_ACL_DIRDEF)
							dirsuccess = dirsuccess && _e2p_acl_change1 (dirfix->path,
									&statbuf, ACL_TYPE_DEFAULT, task, def_changes);
						if (!dirsuccess)
							data.continued_after_problem = TRUE;
					}
					else
					{
						//just reinstate original mode
#ifdef E2_VFS
						VPATH ddata = { dirfix->path, localpath->spacedata };
						if (e2_fs_chmod (&ddata, dirfix->mode E2_ERR_PTR())
#else
						if (e2_fs_chmod (dirfix->path, dirfix->mode E2_ERR_PTR())
#endif
							&& E2_ERR_ISNOT (ENOENT)) //FIXME vfs
							data.continued_after_problem = TRUE;
					}

					g_free (dirfix->path);
					DEMALLOCATE (E2P_DirEnt, dirfix);
				}
				g_list_free (data.dirdata);
			}
			if (!retval)
			{
				//FIXME handle error
				E2_ERR_CLEAR
			}
			if (data.continued_after_problem)
			{
				e2_fs_error_simple (_("Cannot change permission(s) of all of %s"), localpath);
				retval = FALSE;
			}
			return retval;
		}
		else //not dir, handle without recurse
			task |= E2_ACL_NODOWN;
	}
	if (task & E2_ACL_NODOWN)
	{
		if (e2_fs_lstat (localpath, &statbuf E2_ERR_PTR()))  //no link pass-thru
		{
			e2_fs_error_local (_("Cannot get information about %s"), localpath E2_ERR_MSGL());
			E2_ERR_CLEAR
			return FALSE;
		}
		if (S_ISLNK (statbuf.st_mode))
		{
			return FALSE;	//link permissions can't be changed
		}
		retval = _e2p_acl_change1 (VPSTR (localpath), &statbuf, ACL_TYPE_ACCESS, task, axs_changes);
		if (S_ISDIR (statbuf.st_mode) && def_changes != NULL)
			retval = retval && _e2p_acl_change1 (VPSTR (localpath), &statbuf, ACL_TYPE_DEFAULT, task, def_changes);
		return retval;
	}
	return FALSE; //warning prevention only
}
/**
@brief cleanup item in array of change-data

@param data pointer to array element
@param user_data UNUSED pointer to user-specified data
@return
*/
static void _e2p_acl_clean_changes (E2_EntryChangeData *data, gpointer user_data)
{
	DEALLOCATE (E2_EntryChangeData, data);
	data = NULL;
}

  /**********************/
 /** UI-related stuff **/
/**********************/

/**
@brief ensure row for @a iter is visible in the displayed part of @a tvw

@param tvw pointer to treeview
@param iter pointer to data for row to be focused, in @a tvw

@return TRUE if the view was scrolled
*/
static gboolean _e2p_acl_show_row (GtkTreeView *tvw, GtkTreeIter *iter)
{
#ifdef USE_GTK2_8
	gboolean scroll = FALSE;
	GtkTreeModel *model = gtk_tree_view_get_model (tvw);
	GtkTreePath *tpath, *start_path, *end_path;
	if (gtk_tree_view_get_visible_range (tvw, &start_path, &end_path))
	{
		tpath = gtk_tree_model_get_path (model, iter);
		if (tpath != NULL)
		{
			if (gtk_tree_path_compare (tpath, start_path) == -1
				|| gtk_tree_path_compare (tpath, end_path) >= 0)
			{
				scroll = TRUE;
				gtk_tree_view_scroll_to_cell (tvw, tpath, NULL, FALSE, 0, 0);
				gtk_tree_view_set_cursor (tvw, tpath, NULL, FALSE);
			}
			gtk_tree_path_free (tpath);
		}
		gtk_tree_path_free (start_path);
		gtk_tree_path_free (end_path);
	}
#else
	GtkTreeModel *model = gtk_tree_view_get_model (tvw);
	GtkTreePath *tpath, *check_path;
	tpath = gtk_tree_model_get_path (model, iter);
	if (tpath == NULL)
		return FALSE;
	GdkRectangle vis_rect;
	gtk_tree_view_get_visible_rect (tvw, &vis_rect);
	gboolean scroll = FALSE;
	gint wx, wy;
//#ifdef USE_GTK2_12 useless if not USE_GTK2_12
//	gtk_tree_view_convert_tree_to_widget_coords
//#else
	gtk_tree_view_tree_to_widget_coords
//#endif
		 (tvw, vis_rect.x, vis_rect.y, &wx, &wy);
	gtk_tree_view_get_path_at_pos (tvw, wx, wy, &check_path, NULL, NULL, NULL);
	if (check_path != NULL)
	{
		if (gtk_tree_path_compare (tpath, check_path) == -1)
			scroll = TRUE;
		gtk_tree_path_free (check_path);
	}
	if (!scroll)
	{
/*#ifdef USE_GTK2_12
		gtk_tree_view_convert_tree_to_widget_coords
#else
		gtk_tree_view_tree_to_widget_coords
#endif
		(tvw, vis_rect.x, vis_rect.y + vis_rect.height, &wx, &wy);
*/
		wy += vis_rect.height;
		gtk_tree_view_get_path_at_pos (tvw, wx, wy, &check_path, NULL, NULL, NULL);
		if (check_path != NULL)
		{
			if (gtk_tree_path_compare (tpath, check_path) >= 0)
				scroll = TRUE;
			gtk_tree_path_free (check_path);
		}
	}
	if (scroll)
	{
		gtk_tree_view_scroll_to_cell (tvw, tpath, NULL, FALSE, 0, 0);
		gtk_tree_view_set_cursor (tvw, tpath, NULL, FALSE);
	}
	gtk_tree_path_free (tpath);
#endif
	return scroll;
}
/**
@brief get class, user/group names for combo renderers in the qaalifier column of the treeview

@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_fill_combo_models (E2_ACLDlgRuntime *rt)
{
	GtkTreeIter iter;
	if (rt->classes == NULL)
	{
		rt->classes = gtk_list_store_new (1, G_TYPE_STRING, -1);
		guint i;
		for (i = 0; i < CLASSCOUNT; i++)
		{
			gtk_list_store_insert_with_values (rt->classes, &iter, -1,
				0, (classinames [i]), -1);
		}
	}

	if (rt->users == NULL)
	{
		rt->users = gtk_list_store_new (1, G_TYPE_STRING, -1);

		gchar *utf;
		struct passwd *pw_buf;
		struct group *grp_buf;

		gint myuid = getuid ();

		setpwent ();
		while ((pw_buf = getpwent ()) != NULL)
		{	//screen out superfluous names
			if (
				(myuid > 0) //user is not root
				&& ((guint) pw_buf->pw_uid > 0)  //buffer entry is not root's
				&& ((guint) pw_buf->pw_uid < UID_LOWLIMIT)  //but is less than the system-user threshold
			)
				continue;	//ignore it
			utf = e2_utf8_from_locale (pw_buf->pw_name);
			if (utf == NULL)
				utf = g_strdup_printf ("%d", (guint) pw_buf->pw_uid);
			gtk_list_store_insert_with_values (rt->users, &iter, -1, 0, utf, -1);
			g_free (utf);
		}
		endpwent ();

		setgrent ();
		if (myuid == 0)
		{	//root user sees all groups
			while ((grp_buf = getgrent ()) != NULL)
			{	//FIXME duplicates
				utf = e2_utf8_from_locale (grp_buf->gr_name);
				if (utf == NULL)
					utf = g_strdup_printf ("%d", (guint) grp_buf->gr_gid);
				gtk_list_store_insert_with_values (rt->users, &iter, -1, 0, utf, -1);
				g_free (utf);
			}
		}
		else //not root
		{
			gid_t grp_ids[REPORTABLE_GROUPS];
			gint n = getgroups (REPORTABLE_GROUPS, grp_ids);
			gint ctr;
			for (ctr = 0; ctr < n; ctr++)
			{
				if ((grp_buf = getgrgid (grp_ids[ctr])) != NULL)
				{
					utf = e2_utf8_from_locale (grp_buf->gr_name);
					if (utf == NULL)
						utf = g_strdup_printf ("%d", (guint) grp_buf->gr_gid);
					gtk_list_store_insert_with_values (rt->users, &iter, -1, 0,
						utf, -1);
					g_free (utf);
				}
			}
		}
		endgrent ();
	}
}
/**
@brief fill the dialog liststore with ACL data, if any, of the current item

@param store pointer to liststore to be populated
@param acl ACL holding data to be parsed (may be NULL)

@return
*/
static void _e2p_acl_fill_store (GtkListStore *store, acl_t acl)
{
#ifdef FAKEACLSTORE
	GtkTreeModel *mdl = GTK_TREE_MODEL (store);
	GtkTreeIter iter;
	gtk_list_store_insert_with_values (store, &iter, -1,
		CLASS, _("User"), READ, TRUE, WRITE, TRUE, EXEC, FALSE, -1);
	_e2p_acl_fill_sortkey (mdl, &iter);
	gtk_list_store_insert_with_values (store, &iter, -1,
		CLASS, _("Group"),  READ, TRUE, WRITE, FALSE, EXEC, FALSE, -1);
	_e2p_acl_fill_sortkey (mdl, &iter);
	gtk_list_store_insert_with_values (store, &iter, -1,
		CLASS, _("Mask"),  READ, FALSE, WRITE, FALSE, EXEC, FALSE, -1);
	_e2p_acl_fill_sortkey (mdl, &iter);
	gtk_list_store_insert_with_values (store, &iter, -1,
		CLASS, _("Other"),  READ, FALSE, WRITE, TRUE, EXEC, FALSE, -1);
	_e2p_acl_fill_sortkey (mdl, &iter);
#else
	if (acl != NULL)
	{
		//NOTE this approach may not be greatly portable ...
		acl_entry_t entry;
		acl_permset_t permset;
		acl_tag_t tag;
		gint rd, wr, ex;	//NOTE linux function may return TRUE, not necessarily +1, when flag is set
		gchar *class, *name;
		struct passwd *pw;
		struct group *grp_buf;
		GtkTreeIter iter;
		GtkTreeModel *mdl = GTK_TREE_MODEL (store);

# if defined (E2ACL_POSIX_EXTENDED) || defined (E2ACL_POSIX_FULL)
#  ifdef FOREACH_ACL_ENTRY
		FOREACH_ACL_ENTRY (entry, acl)
#  else
		gint result = acl_get_entry (acl, ACL_FIRST_ENTRY, &entry);
		while (result == 1)
#  endif
		{
			acl_get_tag_type (entry, &tag);
			switch (tag)
			{
				case ACL_USER_OBJ:
					class = classinames[0];
					name = "";
					break;
				case ACL_USER:
					class = classinames[0];
					//get the qualifier
					uid_t *UID = (uid_t *) acl_get_qualifier (entry);
					if ((pw = getpwuid (*UID)) != NULL)
						name = e2_utf8_from_locale (pw->pw_name);
					else
						name = NULL;
					if (name == NULL)
						//ascii number, no need to convert to utf-8
						name = g_strdup_printf ("%d", (guint) *UID);
					acl_free (UID);
					break;
				case ACL_GROUP_OBJ:
					class = classinames[1];
					name = "";
					break;
				case ACL_GROUP:
					class = classinames[1];
					//get the qualifier
					gid_t *GID = (gid_t *) acl_get_qualifier (entry);
					if ((grp_buf = getgrgid (*GID)) != NULL)
						name = e2_utf8_from_locale (grp_buf->gr_name);
					else
						name = NULL;
					if (name == NULL)
						//ascii number, no need to convert to utf-8
						name = g_strdup_printf ("%d", (guint) *GID);
					acl_free (GID);
					break;
				case ACL_MASK:
					class = classinames[2];
					name = "";
					break;
				case ACL_OTHER:
					class = classinames[3];
					name = "";
					break;
				default:
					class = NULL;	//prevent inclusion
					name = NULL;	//warning prevention
					break;
			}

			if (class != NULL)
			{
				acl_get_permset (entry, &permset);
				rd = ACL_GET_PERM (permset, ACL_READ);
				if (rd == -1)	//watch out if the func returns TRUE!!
				{
					//FIXME handle error
					rd = 0;
				}
				wr = ACL_GET_PERM (permset, ACL_WRITE);
				if (wr == -1)
				{
					//FIXME handle error
					wr = 0;
				}
				ex = ACL_GET_PERM (permset, ACL_EXECUTE);
				if (ex == -1)
				{
					//FIXME handle error
					ex = 0;
				}
				//store them ...
				gtk_list_store_insert_with_values (store, &iter, -1,
					CLASS, class, QUAL, name, READ, rd, WRITE, wr, EXEC, ex, -1);
				_e2p_acl_fill_sortkey (mdl, &iter);

				if (*name != '\0')
					g_free (name);
			}
#  ifndef FOREACH_ACL_ENTRY
			result = acl_get_entry (acl, ACL_NEXT_ENTRY, &entry);
#  endif
		}
# endif //defined () ....
# ifdef E2ACL_POSIX_RESTRICTED
#  warning _e2p_acl_fill_store() is inoperative
# endif
	}
#endif //def FAKEACLSTORE
}
/**
@brief setup scrolled window with treeview and empty liststore

@param type enumerator for general or default data
@param rt pointer to dialog data struct

@return the scrolled window widget
*/
static GtkWidget *_e2p_acl_create_view (acl_type_t type, E2_ACLDlgRuntime *rt)
{
	GtkTreeSortable *sortable;

	if (type == ACL_TYPE_ACCESS || rt->thisis_dir)
	{
		rt->store = gtk_list_store_new (MAXACLCOLS,
			G_TYPE_STRING,	//CLASS
			G_TYPE_STRING,	//QUAL
			G_TYPE_BOOLEAN,	//READ
			G_TYPE_BOOLEAN,	//WRITE
			G_TYPE_BOOLEAN,	//EXEC
			G_TYPE_BOOLEAN,	//WHOLE
			G_TYPE_STRING,	//SORTKEY (not displayed)
			-1);
		sortable = GTK_TREE_SORTABLE (rt->store);
		//start without sorting
//		gtk_tree_sortable_set_sort_column_id (sortable,
//			GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
		gtk_tree_sortable_set_sort_func (sortable, SORTKEY,
			(GtkTreeIterCompareFunc) _e2p_acl_view_sort, NULL, NULL);
		gtk_tree_sortable_set_sort_column_id (sortable,
			SORTKEY, GTK_SORT_ASCENDING);

		if (type == ACL_TYPE_ACCESS)
			rt->axs_store = rt->store;
		else
			rt->def_store = rt->store;
	}
	else	//should never happen
		return NULL;

	//get class, user/group names for combo renderers (only works once)
	_e2p_acl_fill_combo_models (rt);

	rt->treeview = gtk_tree_view_new ();

	if (type == ACL_TYPE_ACCESS)
		rt->axs_view = rt->treeview;
	else
		rt->def_view = rt->treeview;

	//set general treeview properties
	gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (rt->treeview), TRUE);
	GtkTreeSelection *sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->treeview));
	gtk_tree_selection_set_mode (sel, GTK_SELECTION_BROWSE);

	//set columns' properties
	guint i;
	gchar *name;
	gchar *fontstr = (e2_option_bool_get ("custom-list-font")) ?
		e2_option_str_get ("list-font") : NULL;	//NULL will cause default font
	GtkCellRenderer *renderer;
	GtkTreeViewColumn *column;

	for (i = CLASS; i < VISCOLS; i++)
	{
		if (i < READ)
		{
			renderer = gtk_cell_renderer_combo_new ();
			if (i == CLASS)
				g_object_set (G_OBJECT (renderer),
					"model", rt->classes,
					"has-entry", FALSE,
					NULL);
			else
				g_object_set (G_OBJECT (renderer),
					"model", rt->users,
					NULL);
			g_object_set (G_OBJECT (renderer),
				"text-column", 0,
				"editable", TRUE,
				"yalign", 0.0,
				"font", fontstr,
				NULL);
			if (rt->permission)
			{
				g_signal_connect (G_OBJECT (renderer), "editing-started",
					G_CALLBACK (_e2p_acl_cell_edit_start_cb), rt);
				g_signal_connect (G_OBJECT (renderer), "editing-canceled",
					G_CALLBACK (_e2p_acl_cell_edit_stop_cb), rt);
				g_signal_connect (G_OBJECT (renderer), "edited",
					G_CALLBACK (_e2p_acl_cell_edited_cb), rt);
			}

			column = gtk_tree_view_column_new_with_attributes (
				colnames[i],	//name
				renderer,
				"text", i,
				NULL);
		}
		else
		{
			name = gettext (colnames[i]);
			renderer = gtk_cell_renderer_toggle_new ();
			g_object_set (G_OBJECT (renderer),
				"activatable", TRUE,
//				"indicator-size", 15,
				"xalign", 0.5,
				"yalign", 0.0,
				NULL);
			if (rt->permission)
				g_signal_connect (G_OBJECT (renderer), "toggled",
					G_CALLBACK (_e2p_acl_toggle_cb), rt);

			column = gtk_tree_view_column_new_with_attributes (
				name,
				renderer,
//				"alignment", 0.5, do this later, here it crashes
				"active", i,
				NULL);
		}
		g_object_set_data (G_OBJECT (renderer), "column", GUINT_TO_POINTER (i));
		gtk_tree_view_column_set_resizable (column, TRUE);
		gtk_tree_view_column_set_sizing (column, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
//		if (i < READ)
			gtk_tree_view_column_set_expand (column, TRUE);
//		else
		if (i >= READ)
			gtk_tree_view_column_set_alignment (column, 0.5);
		gtk_tree_view_append_column (GTK_TREE_VIEW (rt->treeview), column);
	}

//	gtk_tree_sortable_set_sort_column_id (sortable, SORTKEY, GTK_SORT_ASCENDING);

	gtk_tree_view_set_model (GTK_TREE_VIEW (rt->treeview), GTK_TREE_MODEL (rt->store));
	g_object_unref (G_OBJECT (rt->store));
	GtkWidget *sw = e2_widget_get_sw (GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC,
		GTK_SHADOW_ETCHED_IN);
	gtk_widget_set_size_request (sw, -1, 95);	//minimum height about enough for 3 rows + headers
	gtk_container_add (GTK_CONTAINER (sw), rt->treeview);
	return sw;
}
/**
@brief change or set fields in @a store to reflect @a mode or @a acl
wrx fields match settings in @a mode or @a acl, whole field = FALSE,
sortkey field consistent with class and qualifier
@param store the liststore to be updated
@param basic TRUE if the permissions are to be strictly in accord with @a mode
@param mode flags as returned by stat() or ~umask()
@param acl pointer (maybe NULL) to data struct to replicate in @a store, if @a basic is FALSE

@return
*/
static void _e2p_acl_reset_mode_fields (GtkListStore *store, gboolean basic,
	mode_t mode, acl_t acl)
{
	gboolean rd, wr, ex;
	gboolean user, grp, other;
	gchar *key;
	GtkTreeModel *model;
	GtkTreeIter iter;

	if (store == NULL)	//should never happen
		return;

	model = GTK_TREE_MODEL (store);
	if (gtk_tree_model_get_iter_first (model, &iter))
	{
		user = grp = other = FALSE;	//flags to handle duplicates
		if (basic || acl == NULL)
		{
			do
			{
reloop:
				gtk_tree_model_get (model, &iter, SORTKEY, &key, -1);
				//revert to mode-compatible settings
				if (key[0] == '1' && key[1] == '\0')	//not an extended user entry
				{
					if (user)
					{
						g_free (key);
						if (gtk_list_store_remove (store, &iter))
							goto reloop;	//no need to get next iter
						else
							break;
					}
					else
					{
						user = TRUE;
						rd = ((mode & S_IRUSR) > 0);
						wr = ((mode & S_IWUSR) > 0);
						ex = ((mode & S_IXUSR) > 0);
					}
				}
				else if (key[0] == '2' && key[1] == '\0')	//group
				{
					if (grp)
					{
						g_free (key);
						if (gtk_list_store_remove (store, &iter))
							goto reloop;	//no need to get next iter
						else
							break;
					}
					else
					{
						grp = TRUE;
						rd = ((mode & S_IRGRP) > 0);
						wr = ((mode & S_IWGRP) > 0);
						ex = ((mode & S_IXGRP) > 0);
					}
				}
				else if (key[0] == '4' && key[1] == '\0')	//other (should never be qualfied, but user may have made mistake)
				{
					if (other)
					{
						g_free (key);
						if (gtk_list_store_remove (store, &iter))
							goto reloop;	//no need to get next iter
						else
							break;
					}
					else
					{
						other = TRUE;
						rd = ((mode & S_IROTH) > 0);
						wr = ((mode & S_IWOTH) > 0);
						ex = ((mode & S_IXOTH) > 0);
					}
				}
				else
				{
					g_free (key);
					if (gtk_list_store_remove (store, &iter))
						goto reloop;	//no need to get next iter
					else
						break;
				}
				gtk_list_store_set (store, &iter,
					READ, rd, WRITE, wr, EXEC, ex, WHOLE, FALSE, -1);
				g_free (key);
			} while (gtk_tree_model_iter_next (model, &iter));
		}
		else //use ACL values
		{
			//revert to current values
			gtk_list_store_clear (store);
			_e2p_acl_fill_store (store, acl);
			//confirm that user, grp, other entries are present
			acl_entry_t entry;
			acl_tag_t tag;
#if defined (E2ACL_POSIX_EXTENDED) || defined (E2ACL_POSIX_FULL)
# ifdef FOREACH_ACL_ENTRY
			FOREACH_ACL_ENTRY (entry, acl)
# else
			gint result = acl_get_entry (acl, ACL_FIRST_ENTRY, &entry);
			while (result == 1)
# endif
			{
				acl_get_tag_type (entry, &tag);
				switch (tag)
				{
					case ACL_USER_OBJ:
						user = TRUE;
						break;
					case ACL_GROUP_OBJ:
						grp = TRUE;
						break;
					case ACL_OTHER:
						other = TRUE;
					default:
						break;
				}
# ifndef FOREACH_ACL_ENTRY
				result = acl_get_entry (acl, ACL_NEXT_ENTRY, &entry);
# endif
			}
#endif //defined () ....
# ifdef E2ACL_POSIX_RESTRICTED
#  warning _e2p_acl_reset_mode_fields() is disfunctional
# endif
		}
		//ensure no gaps
		if (!user)
		{
			rd = ((mode & S_IRUSR) > 0);
			wr = ((mode & S_IWUSR) > 0);
			ex = ((mode & S_IXUSR) > 0);
			gtk_list_store_insert_with_values (store, &iter, -1,
				CLASS, classinames [0], QUAL, "",
				READ, rd, WRITE, wr, EXEC, ex, WHOLE, TRUE, SORTKEY, "1", -1);
		}
		if (!grp)
		{
			rd = ((mode & S_IRGRP) > 0);
			wr = ((mode & S_IWGRP) > 0);
			ex = ((mode & S_IXGRP) > 0);
			gtk_list_store_insert_with_values (store, &iter, -1,
				CLASS, classinames [1], QUAL, "",
				READ, rd, WRITE, wr, EXEC, ex, WHOLE, TRUE, SORTKEY, "2", -1);
		}
		if (!other)
		{
			rd = ((mode & S_IROTH) > 0);
			wr = ((mode & S_IWOTH) > 0);
			ex = ((mode & S_IXOTH) > 0);
			gtk_list_store_insert_with_values (store, &iter, -1,
				CLASS, classinames [3], QUAL, "",
				READ, rd, WRITE, wr, EXEC, ex, WHOLE, TRUE, SORTKEY, "4", -1);
		}
	}
	else	//store is empty
	{	//populate it
		//CHECKME manage sorting during additions
		if (basic)
		{
			rd = ((mode & S_IRUSR) > 0);
			wr = ((mode & S_IWUSR) > 0);
			ex = ((mode & S_IXUSR) > 0);
			gtk_list_store_insert_with_values (store, &iter, -1,
				CLASS, classinames [0], QUAL, "",
				READ, rd, WRITE, wr, EXEC, ex, WHOLE, FALSE, SORTKEY, "1", -1);

			rd = ((mode & S_IRGRP) > 0);
			wr = ((mode & S_IWGRP) > 0);
			ex = ((mode & S_IXGRP) > 0);
			gtk_list_store_insert_with_values (store, &iter, -1,
				CLASS, classinames [1], QUAL, "",
				READ, rd, WRITE, wr, EXEC, ex, WHOLE, FALSE, SORTKEY, "2", -1);

			rd = ((mode & S_IROTH) > 0);
			wr = ((mode & S_IWOTH) > 0);
			ex = ((mode & S_IXOTH) > 0);
			gtk_list_store_insert_with_values (store, &iter, -1,
				CLASS, classinames [3], QUAL, "",
				READ, rd, WRITE, wr, EXEC, ex, WHOLE, FALSE, SORTKEY, "4", -1);
		}
		else if (acl != NULL)
		{
			_e2p_acl_fill_store (store, acl);
		}
	}
}
/**
@brief set or clear the 'whole' field of all rows in @a store

@param store pointer to liststore to be updated
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_reset_whole_fields (GtkListStore *store, E2_ACLDlgRuntime *rt)
{
	GtkTreeIter iter;
	if (store != NULL &&
		gtk_tree_model_get_iter_first (GTK_TREE_MODEL (store), &iter))
	{
		gchar *key;
		//some whole changes are valid for set, not for add or remove
		gboolean valid;
#ifdef USE_GTK2_14
		gboolean setaction =
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->set_data_btn))
		 || gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_all_btn));
#else
		gboolean setaction = GTK_TOGGLE_BUTTON (rt->set_data_btn)->active
				|| GTK_TOGGLE_BUTTON (rt->remove_all_btn)->active;
#endif
		gboolean remaction =
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_data_btn));
#else
			GTK_TOGGLE_BUTTON (rt->remove_data_btn)->active;
#endif
		do
		{
			gtk_tree_model_get (GTK_TREE_MODEL (store), &iter, SORTKEY, &key, -1);
			if (key != NULL)
			{
				switch (key[0])
				{
					case '1':	//user
					//can always add/remove qualified user entries
						valid = setaction || key[1] != '\0';
						break;
					case '2':	//group
					//can always add/remove qualified group entries
						valid = setaction || key[1] != '\0';
						break;
					case '3':	//mask
					//can remove mask as well as set
						valid = setaction || remaction;
						break;
					case '4':	//other
					//can only set other
						valid = setaction;
						break;
					default:
						valid = FALSE;
						break;
				}
				g_free (key);
				gtk_list_store_set (store, &iter, WHOLE, valid, -1);
			}
		} while (gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter));
	}
}
/**
@brief construct and store a sortkey string for the row represented by @a iter;
This interprets the contents of CLASS field and uses corresponding classorder[]
value to produce the expected order. Any appended qualifier from the QUAL field
may be (ascii) number or (utf-8) name
@param model the gtktreemodel to which @a iter applies
@param iter pointer to liststoer iter of row to update

@return
*/
static void _e2p_acl_fill_sortkey (GtkTreeModel *model, GtkTreeIter *iter)
{
	gchar *class, *qualifier, *key;
	gtk_tree_model_get (model, iter, CLASS, &class, QUAL, &qualifier, -1);
	if (class != NULL)
	{
		guint i;
		for (i = 0; i < CLASSCOUNT; i++)
		{
			if (!strcmp (classinames[i], class))
			{
				if (qualifier == NULL || *qualifier == '\0')
					key = g_strdup (classorder[i]);
				else
					key = g_strconcat (classorder[i], qualifier, NULL);
				gtk_list_store_set (GTK_LIST_STORE (model), iter, SORTKEY, key, -1);
				g_free (key);
				break;
			}
		}
		g_free (class);
	}
	if (qualifier != NULL)
		g_free (qualifier);
}
/**
@brief sort-order comparison function
This sorts liststore items in ACL-consistent order (user>group>mask>other)
by analysing SORTKEY field data in both rows.
@param model the data model to be interrogated
@param a pointer to model iter for the first row to be compared
@param b pointer to model iter for the second row to be compared
@param user_data UNUSED data specified when the sort function was assigned

@return  integer <0, 0 or >0 according to whether a belongs before, = or after b
*/
static gint _e2p_acl_view_sort
	(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, gpointer user_data)
{
	gint result;
	gchar *keya;
	gchar *keyb;
	gtk_tree_model_get (model, a, SORTKEY, &keya, -1);
	gtk_tree_model_get (model, b, SORTKEY, &keyb, -1);
	if (keya == NULL)
		result = (keyb == NULL) ? 0 : -1;
	else if (keyb == NULL)
		result = (keya == NULL) ? 0 : 1;
	else
//		result = strcmp (keya, keyb); any username appended to the field will be utf-8
		result = g_utf8_collate (keya, keyb);
	if (keya != NULL)
		g_free (keya);
	if (keyb != NULL)
		g_free (keyb);
	return result;
}
/**
@brief create shortform @a type ACL-string for @a localpath

@param localpath path of item to process, localised string
@param type enumerator dictating whether access or default ACL is wanted

@return newly-allocated short-form ACL or NULL upon error
*/
static gchar *_e2p_acl_create_mode_string (gchar *localpath, acl_type_t type)
{
	acl_t acl = acl_get_file (localpath, type);
	return (_e2p_acl_create_mode_string_for_acl (acl));
}

  /*******************/
 /**** callbacks ****/
/*******************/

/* radio and toggle button callbacks may update widget sensitivities and/or
   liststore(s) content, but they do not change any action-type flags
   Those are all interpreted when OK or Apply-to-all is clicked */

/**
@brief "toggled" signal callback for use-shown-data radio button

@param widget the activated button widget
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_set_shown_changes_cb (GtkWidget *widget, E2_ACLDlgRuntime *rt)
{
/*	gboolean flag =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
#else
		GTK_TOGGLE_BUTTON (widget)->active;
#endif
	NEEDCLOSEBGL
	if (flag)
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
	else
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
	NEEDOPENBGL
*/
}
/**
@brief "toggled" signal callback for use-system-data radio button

@param widget the activated button widget
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_set_system_changes_cb (GtkWidget *widget, E2_ACLDlgRuntime *rt)
{
	gboolean flag =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
#else
		GTK_TOGGLE_BUTTON (widget)->active;
#endif
	NEEDCLOSEBGL
	if (flag)
	{
#ifdef USE_GTK2_14
		if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->add_data_btn))
		 || gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_data_btn)))
#else
		if (GTK_TOGGLE_BUTTON (rt->add_data_btn)->active
		 || GTK_TOGGLE_BUTTON (rt->remove_data_btn)->active)
#endif
			gtk_toggle_button_set_active
				(GTK_TOGGLE_BUTTON (rt->set_data_btn), TRUE);
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
/*	else
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
*/
	NEEDOPENBGL
}
/**
@brief "toggled" signal callback for use-modified-system-data radio button

@param widget the activated button widget
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_set_systemplus_changes_cb (GtkWidget *widget, E2_ACLDlgRuntime *rt)
{
/*	gboolean flag =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
#else
		GTK_TOGGLE_BUTTON (widget)->active;
#endif
	NEEDCLOSEBGL
	if (flag)
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
	else
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
	NEEDOPENBGL
*/
}
/**
@brief "toggled" signal callback for set_data_btn and remove_all_btn
This sets all permission-field states to conform to current mode
@param widget the activated button widget, may be NULL at dialog start
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_reset_mode_fields_cb (GtkWidget *widget, E2_ACLDlgRuntime *rt)
{
	gboolean flag = (widget == NULL ||
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))
#else
		GTK_TOGGLE_BUTTON (widget)->active
#endif
	);
	gboolean nuke = (widget != NULL && widget == rt->remove_all_btn);
	NEEDCLOSEBGL
	if (flag)
	{
		if (nuke)
		{
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->system_perms_btn), TRUE);
			gtk_widget_set_sensitive (rt->shown_perms_btn, FALSE);
			gtk_widget_set_sensitive (rt->sysmod_perms_btn, FALSE);
			gtk_widget_set_sensitive (rt->change_whole_btn, FALSE);
			gtk_widget_set_sensitive (rt->add_row_btn, FALSE);
			gtk_widget_set_sensitive (rt->remove_row_btn, FALSE);
		}
		GtkTreeSelection *sel;
		GtkTreePath *tpath;
#ifdef E2_VFS
		VPATH ddata = { rt->itempath, NULL }; //only do ACL's on local items
#endif
		struct stat sb;
		E2_ERR_DECLARE

		if (nuke && rt->acls[AXSNOW] == NULL)
			gtk_list_store_clear (rt->axs_store);
		//CHECKME view de/re-attachment during store update
#ifdef E2_VFS
		else if (!e2_fs_stat (&ddata, &sb E2_ERR_PTR()))
#else
		else if (!e2_fs_stat (rt->itempath, &sb E2_ERR_PTR()))
#endif
		{
			if (gtk_tree_model_iter_n_children (GTK_TREE_MODEL (rt->axs_store), NULL) == 0
				&& rt->acls[AXSNOW] == NULL)
					flag = TRUE;	//force filling with a basic set of data
			else
				flag = nuke;
			_e2p_acl_reset_mode_fields (rt->axs_store, flag, sb.st_mode,
					rt->acls[AXSNOW]);
			if (
#ifdef USE_GTK2_14
				gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->change_whole_btn))
#else
				GTK_TOGGLE_BUTTON (rt->change_whole_btn)->active
#endif
			)
				_e2p_acl_reset_whole_fields (rt->axs_store, rt);
			sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->axs_view));
			if (gtk_tree_selection_count_selected_rows (sel) == 0)
			{
				tpath = gtk_tree_path_new_first ();
				gtk_tree_selection_select_path (sel, tpath);
				gtk_tree_path_free (tpath);
			}
		}
		else	//stat failed
		{
			OPENBGL
			e2_fs_error_local (_("Cannot get current data for %s"),
#ifdef E2_VFS
				&ddata E2_ERR_MSGL());
#else
				rt->itempath E2_ERR_MSGL());
#endif
			CLOSEBGL
			E2_ERR_CLEAR
			//FIXME handle error
		}

		if (rt->def_store != NULL)
		{
			if (nuke)
			{
				gtk_list_store_clear (rt->def_store);
			}
			else
			{
				if (gtk_tree_model_iter_n_children (GTK_TREE_MODEL (rt->def_store), NULL) == 0
					&& rt->acls[DEFNOW] == NULL)
					flag = TRUE;
				else
					flag = nuke;
				mode_t mode = umask (0);
				umask (mode);
				_e2p_acl_reset_mode_fields (rt->def_store, flag, ~mode & ALLPERMS,
					rt->acls[DEFNOW]);
				if (
#ifdef USE_GTK2_14
					gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->change_whole_btn))
#else
					GTK_TOGGLE_BUTTON (rt->change_whole_btn)->active
#endif
				)
					_e2p_acl_reset_whole_fields (rt->def_store, rt);
				sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt->def_view));
				if (gtk_tree_selection_count_selected_rows (sel) == 0)
				{
					tpath = gtk_tree_path_new_first ();
					gtk_tree_selection_select_path (sel, tpath);
					gtk_tree_path_free (tpath);
				}
			}
		}
	}
	else	//turning off
		if (nuke)
	{
		gtk_widget_set_sensitive (rt->shown_perms_btn, TRUE);
		gtk_widget_set_sensitive (rt->sysmod_perms_btn, TRUE);
		gtk_widget_set_sensitive (rt->change_whole_btn, TRUE);
		gint count = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (rt->store), NULL);
		if (count > 0)
			gtk_widget_set_sensitive (rt->remove_row_btn, TRUE);
		if (count < MAX_ACL_ENTRIES)
			gtk_widget_set_sensitive (rt->add_row_btn, TRUE);
	}
	NEEDOPENBGL
}
/**
@brief "toggled" signal callback for add_perms_button and remove_perms_button
This sets all permission-field states to FALSE
@param widget the activated button widget, may be NULL at dialog start
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_clear_mode_fields_cb (GtkWidget *widget, E2_ACLDlgRuntime *rt)
{
	gboolean flag = (widget == NULL ||
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))
#else
		GTK_TOGGLE_BUTTON (widget)->active
#endif
	);
	NEEDCLOSEBGL
	if (flag)
	{	//	gtk_widget_set_sensitive (rt->?, flag);
		//	gtk_widget_set_sensitive (rt->?, !flag);}
		if (rt->system_perms_btn != NULL	//not running at dialog start
#ifdef USE_GTK2_14
				&& gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->system_perms_btn))
#else
				&& GTK_TOGGLE_BUTTON (rt->system_perms_btn)->active
#endif
		)
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->sysmod_perms_btn), TRUE);

		GtkTreeIter iter;
		if (rt->axs_store != NULL &&
			gtk_tree_model_get_iter_first (GTK_TREE_MODEL (rt->axs_store), &iter))
		{
			do
			{
				gtk_list_store_set (rt->axs_store, &iter,
					READ, FALSE, WRITE, FALSE, EXEC, FALSE, WHOLE, FALSE, -1);
			} while (gtk_tree_model_iter_next (GTK_TREE_MODEL (rt->axs_store), &iter));
		}
		if (rt->def_store != NULL &&
			gtk_tree_model_get_iter_first (GTK_TREE_MODEL (rt->def_store), &iter))
		{
			do
			{
				gtk_list_store_set (rt->def_store, &iter,
					READ, FALSE, WRITE, FALSE, EXEC, FALSE, WHOLE, FALSE, -1);
			} while (gtk_tree_model_iter_next (GTK_TREE_MODEL (rt->def_store), &iter));
		}
	}
/*	else
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
*/
	NEEDOPENBGL
}
/**
@brief "toggled" signal callback for recurse_btn

@param widget the activated button widget
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_toggle_recurse_btn_cb (GtkWidget *widget,
	E2_ACLDlgRuntime *rt)
{
	gboolean flag =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
#else
		GTK_TOGGLE_BUTTON (widget)->active;
#endif
	NEEDCLOSEBGL
	gtk_widget_set_sensitive (rt->recurse_dirs_btn, flag);
	gtk_widget_set_sensitive (rt->recurse_other_btn, flag);
/*	if (flag)
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
	else
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
*/
	NEEDOPENBGL
}
/**
@brief "toggled" signal callback for recurse_dirs and recurs_others buttons

@param widget the activated button widget
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_toggle_recurse_type_cb (GtkWidget *widget,
	E2_ACLDlgRuntime *rt)
{
/*	gboolean flag =
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
#else
		GTK_TOGGLE_BUTTON (widget)->active;
#endif
	NEEDCLOSEBGL
	gtk_widget_set_sensitive (rt->?, flag);
	gtk_widget_set_sensitive (rt->?, !flag);
	if (flag)
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
	else
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
	}
	NEEDOPENBGL
*/
	if (
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget))
#else
		GTK_TOGGLE_BUTTON (widget)->active
#endif
	)
		return;	//don't care about choice to turn recursion on
	NEEDCLOSEBGL
	if (widget == rt->recurse_dirs_btn)
	{
		if (!
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_other_btn))
#else
			GTK_TOGGLE_BUTTON (rt->recurse_other_btn)->active
#endif
		)
		{
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->recurse_other_btn),TRUE);
		}
	}
	else //widget == rt->recurse_other_btn
	{
		if (!
#ifdef USE_GTK2_14
			gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_dirs_btn))
#else
			GTK_TOGGLE_BUTTON (rt->recurse_dirs_btn)->active
#endif
		)
		{
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (rt->recurse_dirs_btn),TRUE);
		}
	}
	NEEDOPENBGL
}
/**
@brief "toggled" signal callback for global "whole" button

@param widget the activated button widget
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_default_whole_fields_cb (GtkWidget *widget,
	E2_ACLDlgRuntime *rt)
{
	gboolean flag =
#ifdef USE_GTK2_14
	gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));
#else
	GTK_TOGGLE_BUTTON (widget)->active;
#endif
	NEEDCLOSEBGL
	if (flag)
	{
//		gtk_widget_set_sensitive (rt->?, flag);
//		gtk_widget_set_sensitive (rt->?, !flag);
		//for cosmetic purposes only, set each row's 'whole' field
		_e2p_acl_reset_whole_fields (rt->axs_store, rt);
		_e2p_acl_reset_whole_fields (rt->def_store, rt);
	}
/*	else
	{
		gtk_widget_set_sensitive (rt->?, flag);
		gtk_widget_set_sensitive (rt->?, !flag);
	}
*/
	NEEDOPENBGL
}
/**
@brief callback for notebook page-switched signal

@param notebook UNUSED the book whose page has changed
@param page UNUSED the new page
@param page_num the index of the new page
@param rt pointer to dialog data struct
@return
*/
static void _e2p_acl_tabchange_cb (GtkNotebook *notebook,
#ifdef USE_GTK3_0
	GtkWidget *page,
#else
	GtkNotebookPage *page,
#endif
	guint page_num, E2_ACLDlgRuntime *rt)
{
	if (page_num == 0)	//general page CHECKME if order dragged
	{
		rt->store = rt->axs_store;
		rt->treeview = rt->axs_view;
	}
	else	//defaults page
	{
		rt->store = rt->def_store;
		rt->treeview = rt->def_view;
	}
	//adjust button sensitivies if needed
	if (!
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_all_btn))
#else
		GTK_TOGGLE_BUTTON (rt->remove_all_btn)->active
#endif
	)
	{
		gint count = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (rt->store), NULL);
		NEEDCLOSEBGL
		if (count == 0)
			gtk_widget_set_sensitive (rt->remove_row_btn, FALSE);
		else if (count >= MAX_ACL_ENTRIES)
			gtk_widget_set_sensitive (rt->add_row_btn, FALSE);
		NEEDOPENBGL
	}
}
/**
@brief dialog treeview-selection changed callback

@param treeselection selection object for the clicked treeview widget
@param rt pointer to data struct for the dialog

@return
*/
static void _e2p_acl_selection_change_cb (GtkTreeSelection *treeselection,
	E2_ACLDlgRuntime *rt)
{
	if (!
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_all_btn))
#else
		GTK_TOGGLE_BUTTON (rt->remove_all_btn)->active
#endif
	)
	{
		NEEDCLOSEBGL
		guint count = gtk_tree_selection_count_selected_rows (treeselection);
		gtk_widget_set_sensitive (rt->remove_row_btn, (count > 0));
		NEEDOPENBGL
	}
}
/**
@brief change <Esc> key handling when text-cell editing starts

@param renderer UNUSED the renderer for the cell
@param editable UNUSED the interface to @a cell
@param path_string UNUSED string form of gtk tree path to the row to be amended
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_cell_edit_start_cb (GtkCellRenderer *renderer,
	GtkCellEditable *editable, gchar *path_string, E2_ACLDlgRuntime *rt)
{
//	printd (DEBUG, "start cell edit cb");
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	g_signal_handlers_block_by_func (G_OBJECT (rt->dialog),
		e2_dialog_key_neg_cb, rt->dialog);
}
/**
@brief revert <Esc> key handling when text-cell editing is finished

@param renderer the renderer for the cell
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_cell_edit_stop_cb (GtkCellRenderer *renderer,
	E2_ACLDlgRuntime *rt)
{
//	printd (DEBUG, "cancel cell edit cb");
//	NEEDCLOSEBGL
//	NEEDOPENBGL
	g_signal_handlers_unblock_by_func (G_OBJECT (rt->dialog),
		e2_dialog_key_neg_cb, rt->dialog);
}
/**
@brief if permitted, save edited text value in the underlying liststore, with related sort key

@param renderer the renderer for the cell
@param path_string string form of gtk tree path to the row to be amended
@param new_text replacement text string for the cell
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_cell_edited_cb (GtkCellRendererText *cell,
	gchar *path_string, gchar *new_text, E2_ACLDlgRuntime *rt)
{
	if (new_text == NULL)	//this probably can't happen
		return;
	if (
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_all_btn))
#else
		GTK_TOGGLE_BUTTON (rt->remove_all_btn)->active
#endif
	)
		return;	//no mode changes when in nuke mode
	printd (DEBUG, "acl view edited cb, new text is %s", new_text);
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL (rt->store), &iter,
		path_string))
	{
		//no sorting until the key is added FIXME make this work
//		GtkTreeSortable *sort = GTK_TREE_SORTABLE (rt->store);
//		NEEDCLOSEBGL
//		gtk_tree_sortable_set_sort_column_id (sort,
//			GTK_TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID, GTK_SORT_ASCENDING);
//		NEEDOPENBGL
		gboolean update;
		gint col = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column"));
		gchar *qualifier, *key;
		gtk_tree_model_get (GTK_TREE_MODEL (rt->store), &iter,
			QUAL, &qualifier, SORTKEY, &key, -1);
		if (col == CLASS)
		{	//empty or user or group entries are ok to update
			update = (key == NULL || *key == '\0'
					|| qualifier == NULL || *qualifier == '\0'
					|| !strcmp (new_text, classinames[0])	//user always ok
					|| !strcmp (new_text, classinames[1]));	//ditto group
		}
		else if (col == QUAL)
		{	//empty or user or group entries are ok to update
			update = (key == NULL || *key == '\0' || key[0] == '1' || key[0] == '2');
		}
		else
			update = TRUE;
		if (qualifier != NULL)
			g_free (qualifier);
		if (key != NULL)
			g_free (key);

		if (update)
		{
			NEEDCLOSEBGL
			gtk_list_store_set (rt->store, &iter, col, new_text, -1);
			_e2p_acl_fill_sortkey (GTK_TREE_MODEL (rt->store), &iter);
			if (
#ifdef USE_GTK2_14
				gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->change_whole_btn))
#else
				GTK_TOGGLE_BUTTON (rt->change_whole_btn)->active
#endif
			)
				_e2p_acl_reset_whole_fields (rt->store, rt);

//			gtk_tree_sortable_set_sort_column_id (sort, SORTKEY, GTK_SORT_ASCENDING);
			//ensure the resorted iter is still visible in window
			_e2p_acl_show_row (GTK_TREE_VIEW (rt->treeview), &iter);
			//revert focus to edited row
			gtk_widget_grab_focus (rt->treeview);
			NEEDOPENBGL
		}
	}
}
/**
@brief if permitted, save edited boolean value in the underlying liststore

@param renderer the renderer for the cell
@param path_string string form of gtk tree path to the row to be amended
@param new_text replacement text string for the cell
@param rt pointer to dialog data struct

@return
*/
static void _e2p_acl_toggle_cb (GtkCellRendererToggle *cell,
	const gchar *path_str, E2_ACLDlgRuntime *rt)
{
	if (
#ifdef USE_GTK2_14
		gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_all_btn))
#else
		GTK_TOGGLE_BUTTON (rt->remove_all_btn)->active
#endif
	)
		return;	//no mode changes when in nuke mode
	//find out where
	GtkTreePath *tpath = gtk_tree_path_new_from_string (path_str);
	GtkTreeIter iter;
	if (gtk_tree_model_get_iter (GTK_TREE_MODEL (rt->store), &iter, tpath))
	{
		gint col = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (cell), "column"));
		gboolean update, value, setaction;
		gchar *key;
		//find out if appropriate
		gtk_tree_model_get (GTK_TREE_MODEL (rt->store), &iter, SORTKEY, &key, col, &value, -1);
		if (col == WHOLE)
		{
/*			if (
#ifdef USE_GTK2_14
				gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->change_whole_btn));
#else
				GTK_TOGGLE_BUTTON (rt->change_whole_btn)->active
#endif
			)
				update = FALSE;
*/
			//whole fields may be changed in 'set' mode
			//or sometimes in 'add' or 'remove'
			setaction =
#ifdef USE_GTK2_14
				gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->set_data_btn));
#else
				GTK_TOGGLE_BUTTON (rt->set_data_btn)->active;
#endif
			switch (key[0])
			{
				case '1':	//user
				//can always add/remove qualified user entries
					update = setaction || key[1] != '\0';
					break;
				case '2':	//group
				//can always add/remove qualified group entries
					update = setaction || key[1] != '\0';
					break;
				case '3':	//mask
				//can also remove mask
					update = setaction ||
#ifdef USE_GTK2_14
					gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_data_btn));
#else
					GTK_TOGGLE_BUTTON (rt->remove_data_btn)->active;
#endif
					break;
				case '4':	//other
				//can only set mask
					update = setaction;
					break;
				default:
					update = FALSE;
					break;
			}
		}
		else
			update = TRUE;

		if (update)
		{
			value ^= 1;
			NEEDCLOSEBGL
			//change model
			gtk_list_store_set (rt->store, &iter, col, value, -1);
			NEEDOPENBGL
		}
		g_free (key);
	}
	//clean up
	gtk_tree_path_free (tpath);
}
/**
@brief dialog response callback

@param dialog the acl-dialog
@param response the response for the clicked button or other event
@param rt pointer to dialog data struct (on stack, not heap)

@return
*/
static void _e2p_acl_dialog_response_cb (GtkDialog *dialog, gint response,
	E2_ACLDlgRuntime *rt)
{
	NEEDCLOSEBGL
	switch (response)
	{
		case E2_RESPONSE_USER1:	//help-button press
			e2_utils_show_help ("access control list plugin"); //no translation unless help doc is translated
			gtk_widget_grab_focus (rt->dialog);
			break;
		case E2_RESPONSE_USER2:	//add-button press
		{
			gint count = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (rt->store), NULL);
			if (count < MAX_ACL_ENTRIES)
			{
				GtkTreeIter iter;
				GtkTreeSelection *sel = gtk_tree_view_get_selection
					(GTK_TREE_VIEW (rt->treeview));
				if (gtk_tree_selection_get_selected (sel, NULL, &iter))
				{
					if (iter.user_data != NULL)
					{
						gchar *curkey;
						GtkTreeIter iter2;
						gtk_tree_model_get (GTK_TREE_MODEL (rt->store), &iter, SORTKEY, &curkey, -1);
						gtk_list_store_insert_after (rt->store, &iter2, &iter);
						//set "whole" flag, and sortkey to prevent the new row from being relocated by the sort func
						gtk_list_store_set (rt->store, &iter2, WHOLE, TRUE, SORTKEY, curkey, -1);
						gtk_tree_model_iter_next (GTK_TREE_MODEL (rt->store), &iter);
						g_free (curkey);
					}
				}
				else
					gtk_list_store_append (rt->store, &iter);

				_e2p_acl_show_row (GTK_TREE_VIEW (rt->treeview), &iter);	//ensure it's on-screen

				if (count == 0)
					gtk_widget_set_sensitive (rt->remove_row_btn, TRUE);
				else if (count == MAX_ACL_ENTRIES - 1)
					gtk_widget_set_sensitive (rt->add_row_btn, FALSE);

				gtk_tree_selection_select_iter (sel, &iter);

				gtk_widget_grab_focus (rt->treeview);

				//CHECKME consider a warning to user about adding a mask entry, when relevant
				//or maybe just add such an entry
			}
		}
			break;
		case E2_RESPONSE_REMOVE:	//remove-button press
		{
			GtkTreeIter iter;
			GtkTreeModel *mdl;
			GtkTreeSelection *sel = gtk_tree_view_get_selection
				(GTK_TREE_VIEW (rt->treeview));
			if (gtk_tree_selection_get_selected (sel, &mdl, &iter))
			{
				//CHECKME some validity testing here. instead of upon initiation
				//e.g. a warning to user about removing a mask entry, when relevant
				//or maybe just remove such an entry
				gtk_list_store_remove (rt->store, &iter);
				gint count = gtk_tree_model_iter_n_children (mdl, NULL);
//				if (count == 0) deletion always kills the selection
					gtk_widget_set_sensitive (rt->remove_row_btn, FALSE);
//				else
				if (count == MAX_ACL_ENTRIES - 1)
					gtk_widget_set_sensitive (rt->add_row_btn, TRUE);
			}
		}
			break;
		default:
			if (rt->permission)
			{
				//interpret all button-states into task flags
				rt->task = 0;
				//radio E2_ACL_SHOWN or E2_ACL_SYSTEM or E2_ACL_SYSMOD
				if (
#ifdef USE_GTK2_14
					gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->shown_perms_btn))
#else
					GTK_TOGGLE_BUTTON (rt->shown_perms_btn)->active
#endif
				)
					rt->task |= E2_ACL_SHOWN;
				else if (
#ifdef USE_GTK2_14
					gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->system_perms_btn))
#else
					GTK_TOGGLE_BUTTON (rt->system_perms_btn)->active
#endif
				)
					rt->task |= E2_ACL_SYSTEM;
				else //if (GTK_TOGGLE_BUTTON (rt->sysmod_perms_btn)->active)
					rt->task |= E2_ACL_SYSMOD;

				//radio E2_ACL_SET or E2_ACL_ADD or E2_ACL_REMOVE or E2_ACL_NUKE
				if (
#ifdef USE_GTK2_14
					gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->set_data_btn))
#else
					GTK_TOGGLE_BUTTON (rt->set_data_btn)->active
#endif
				)
					rt->task |= E2_ACL_SET;
				else if (
#ifdef USE_GTK2_14
						gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->add_data_btn))
#else
						GTK_TOGGLE_BUTTON (rt->add_data_btn)->active
#endif
				)
					rt->task |= E2_ACL_ADD;
				else if (
#ifdef USE_GTK2_14
						gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->remove_data_btn))
#else
						GTK_TOGGLE_BUTTON (rt->remove_data_btn)->active
#endif
				)
					rt->task |= E2_ACL_REMOVE;
				else //if (GTK_TOGGLE_BUTTON (rt->remove_all_btn)->active)
					rt->task |= E2_ACL_NUKE;

				if (
#ifdef USE_GTK2_14
					gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->change_whole_btn))
#else
					GTK_TOGGLE_BUTTON (rt->change_whole_btn)->active
#endif
				)
					rt->task |= E2_ACL_WHOLE;

				//effective radio E2_ACL_NODOWN or any of E2_ACL_DIRAXS or E2_ACL_DIRDEF or E2_ACL_OTHER
				if (rt->thisis_dir)	//means the recurse buttons have been added to the dialog
				{
					if (
#ifdef USE_GTK2_14
						gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->dir_axs_btn))
#else
						GTK_TOGGLE_BUTTON (rt->dir_axs_btn)->active
#endif
					)
						rt->task |= E2_ACL_DIRAXS;
					if (
#ifdef USE_GTK2_14
						gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->dir_def_btn))
#else
						GTK_TOGGLE_BUTTON (rt->dir_def_btn)->active
#endif
					)
						rt->task |= E2_ACL_DIRDEF;
					if (
#ifdef USE_GTK2_14
						gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_btn))
#else
						GTK_TOGGLE_BUTTON (rt->recurse_btn)->active
#endif
					)
					{
						if (
#ifdef USE_GTK2_14
							gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_dirs_btn))
#else
							GTK_TOGGLE_BUTTON (rt->recurse_dirs_btn)->active
#endif
						)
							rt->task |= E2_ACL_DIR;
						if (
#ifdef USE_GTK2_14
							gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (rt->recurse_other_btn))
#else
							GTK_TOGGLE_BUTTON (rt->recurse_other_btn)->active
#endif
						)
							rt->task |= E2_ACL_OTHER;
					}
				}
				if (!(rt->task & (E2_ACL_DIR | E2_ACL_OTHER)))
					rt->task |= E2_ACL_NODOWN;

				//the user actually wants to do something
				if (response == E2_RESPONSE_APPLY || response == E2_RESPONSE_APPLYTOALL)
				{
					DialogButtons choice;
					if (rt->thisis_dir)
					{	//check that something relevant has been selected
						if (!(rt->task & (E2_ACL_DIR | E2_ACL_OTHER | E2_ACL_DIRAXS | E2_ACL_DIRDEF)))
						{
							choice = e2_dialog_warning (_("No directory-changes have been selected"), _("_Proceed"));
							if (choice != OK)
							{
								NEEDOPENBGL
								return;
							}
						}
					}
					//verify, if not using just system data and not removing
					if (!((rt->task & E2_ACL_NUKE) || (rt->task & (E2_ACL_SET | E2_ACL_SYSTEM))
						|| (rt->task & E2_ACL_REMOVE)))
					{
						gchar *prompt;
						gchar *template = _("The specified %s is likely to ba a problem");
						if (!_e2p_acl_verify_store (rt->axs_store, rt->task))
						{
							prompt = g_strdup_printf (template, _("General ACL"));
							choice = e2_dialog_warning (prompt, NULL);
							g_free (prompt);
							if (choice != OK)
							{
								NEEDOPENBGL
								return;
							}
						}
						if (rt->thisis_dir)
						{
							if (!_e2p_acl_verify_store (rt->def_store, rt->task))
							{
								prompt = g_strdup_printf (template, _("Directory ACL"));
								choice = e2_dialog_warning (prompt, NULL);
								g_free (prompt);
								if (choice != OK)
								{
									NEEDOPENBGL
									return;
								}
							}
						}
					}
					//convert store data to array for later use
					rt->axs_changes = _e2p_acl_convert_store (rt->axs_store, rt);
					rt->def_changes = (rt->thisis_dir) ?
						 _e2p_acl_convert_store (rt->def_store, rt) : NULL;
				}
/*redundant		else
				{
					rt->axs_changes = NULL;
					rt->def_changes = NULL;
				}
*/
				saved_task = rt->task;	//ready for next time
			}
			//CHECKME acl_t cleanups ? other cleanups ? see also end of dialog func
			break;
	}
	NEEDOPENBGL
}
/**
@brief create and run an ACL-change dialog
Expects BGL open/off
@param localpath path of item to be processed
@param axs_ret store for pointer to array of changes to access-ACL
@param def_ret store for pointer to array of changes to default-ACL
@param scope_ret store for returning whether to recurse the changes
@param permission_ret store for returning whether a change is authorised
@param multi TRUE if this dialog is part of a series for multiple items

@return the enumerator of the clicked dialog button
*/
static DialogButtons _e2p_acl_dialog_run (VPATH *localpath, GPtrArray **axs_ret,
	GPtrArray **def_ret, E2_ACLTask *task_ret, gboolean *permission_ret, gboolean multi)
{
	GtkWidget *dialog_vbox, *sub_vbox;
	GtkWidget *hbox;
	GtkWidget *frame;
	GtkWidget *table;
	GtkWidget *nbook = NULL;	//assignment for warning prevention only
	GtkWidget *sw;
	GtkWidget *btn;

	gboolean freelabel;
	gchar *label, *name;
	struct stat statbuf;
	struct passwd *pw_buf;
	struct group *grp_buf;
	E2_ACLDlgRuntime rt;
	GtkTreeSelection *sel;

	if (e2_fs_lstat (localpath, &statbuf E2_ERR_NONE()))
		return CANCEL;

	GString *label_text = g_string_sized_new (NAME_MAX+20);
	memset (&rt, 0, sizeof(E2_ACLDlgRuntime));
	rt.permission = e2_fs_check_write_permission (localpath E2_ERR_NONE());
	rt.itempath = VPSTR (localpath);
	rt.thisis_dir = e2_fs_is_dir3 (localpath E2_ERR_NONE());	//CHECKME abort on error ?

	CLOSEBGL
	rt.dialog = e2_dialog_create (NULL, NULL, _("extended permissions"),
		(ResponseFunc)_e2p_acl_dialog_response_cb, &rt);
	OPENBGL
	dialog_vbox =
#ifdef USE_GTK2_14
		gtk_dialog_get_content_area (GTK_DIALOG (rt.dialog));
#else
		GTK_DIALOG (rt.dialog)->vbox;
#endif
	gtk_container_set_border_width (GTK_CONTAINER (dialog_vbox), E2_PADDING);
#ifndef USE_GTK3_0
	gtk_dialog_set_has_separator (GTK_DIALOG (rt.dialog), FALSE);  //all info in frames, no need
#endif
	label = (rt.thisis_dir) ? _("Directory name") : _("Filename") ;
	name = g_filename_display_basename (VPSTR(localpath));
	g_string_printf (label_text, "%s: <b>%s</b>", label, name);
	g_free (name);
#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, FALSE, FALSE, E2_PADDING); //top, bottom padding
	e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING); //L, R padding

	label = _("User");
	if ((pw_buf = getpwuid (statbuf.st_uid)) != NULL)
		name = e2_utf8_from_locale (pw_buf->pw_name);
	else
		name = NULL;

	if (name != NULL)
	{
		g_string_printf (label_text, "%s: %s", label, name);
		g_free (name);
	}
	else
		g_string_printf (label_text, "%s: %d", label, (guint) statbuf.st_uid);
#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING);
	gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, FALSE, FALSE, 0);

	label = _("Group");
	if ((grp_buf = getgrgid (statbuf.st_gid)) != NULL)
		name = e2_utf8_from_locale (grp_buf->gr_name);
	else
		name = NULL;

	if (name != NULL)
	{
		g_string_printf (label_text, "%s: %s", label, name);
		g_free (name);
	}
	else
		g_string_printf (label_text, "%s: %d", label, (guint) statbuf.st_gid);
#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING);
	gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, FALSE, FALSE, 0);

	gchar *errstr = _("none");
	gchar *errstr2 = _("unable to display");
	name = _("General ACL");
	//CHECKME these desciption stings may have way too amy lines for this context !!
	rt.acls[AXSNOW] = acl_get_file (VPSTR(localpath), ACL_TYPE_ACCESS);
	if (rt.acls[AXSNOW] != NULL)
	{
		label = _e2p_acl_create_mode_string (VPSTR(localpath), ACL_TYPE_ACCESS);
		if (label != NULL)
			freelabel = TRUE;
		else
		{
			label = errstr2;
			freelabel = FALSE;
		}
	}
	else
	{
		label = errstr;
		freelabel = FALSE;
	}
	g_string_printf (label_text, "%s: %s", name, label);
	if (freelabel)
		g_free (label);

#ifdef USE_GTK3_0
	hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
	hbox = gtk_hbox_new (FALSE, 0);
#endif
	e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING);
	gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, FALSE, FALSE, 0);

	if (rt.thisis_dir)
	{
		name = _("Directory ACL");
		rt.acls[DEFNOW] = acl_get_file (VPSTR(localpath), ACL_TYPE_DEFAULT);
		if (rt.acls[DEFNOW] != NULL)
		{
			label = _e2p_acl_create_mode_string (VPSTR(localpath), ACL_TYPE_DEFAULT);
			if (label != NULL)
				freelabel = TRUE;
			else
			{
				label = errstr2;
				freelabel = FALSE;
			}
		}
		else
		{
			label = errstr;
			freelabel = FALSE;
		}
		g_string_printf (label_text, "%s: %s", name, label);
		if (freelabel)
			g_free (label);

#ifdef USE_GTK3_0
		hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
		hbox = gtk_hbox_new (FALSE, 0);
#endif
		e2_widget_add_mid_label (hbox, label_text->str, 0, TRUE, E2_PADDING);
		gtk_box_pack_start (GTK_BOX(dialog_vbox), hbox, FALSE, FALSE, 0);
	}

	frame = gtk_frame_new (_("Permissions"));
	gtk_box_pack_start (GTK_BOX (dialog_vbox), frame, TRUE, TRUE, E2_PADDING);

#ifdef USE_GTK3_0
	sub_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
	sub_vbox = gtk_vbox_new (FALSE, 0);
#endif
	gtk_container_add (GTK_CONTAINER (frame), sub_vbox);

/* since the initial states of radio/toggle button are set at runtime to match
   the last session, toggling of buttons during dialog initialisation can't be
   allowed, until all widgets that may be affected by a "toggled" callback have
   been created. So all button-creations have a NULL callback, and explict
   callbacks are attached afterwards
*/
	if (rt.thisis_dir)
	{
		nbook = e2_widget_add_notebook (sub_vbox, TRUE, 0, NULL, NULL);	//no callback until after treeviews are created
		GtkNotebook *book = GTK_NOTEBOOK (nbook);
#ifdef E2_TABS_DETACH
		//enable tab dragging to/from new window
		//FIXME next is crasher
	//	gtk_drag_source_set (nbook, GDK_BUTTON1_MASK, target_table2, n_targets2,
	//		GDK_ACTION_PRIVATE);
	/*FIXME re-dragging, including drag-back, does not work
		gtk_drag_dest_set (nbook, GTK_DEST_DEFAULT_DROP, target_table2, n_targets2,
			GDK_ACTION_PRIVATE);
		g_signal_connect (G_OBJECT (nbook), "drag-data-received",
			G_CALLBACK (_e2_output_tabdrag_data_received_cb), NULL);	//CHECKME user_data
	*/
//		gtk_notebook_set_window_creation_hook
//			((GtkNotebookWindowCreationFunc) _e2_output_tab_drop_new,
//			NULL, //FIXME gpointer data
//			NULL);	//(GDestroyNotify) destroy
#endif
		//iterate backward so that we end with the first (General) tab
		sw = _e2p_acl_create_view (ACL_TYPE_DEFAULT, &rt);
		GtkWidget *tablbl = gtk_label_new (_("Directory"));
//USELESS ? e2_widget_set_safetip (tablbl,
//			_("Show the \"default\" ACL, which may apply to directories only"));
		gtk_notebook_append_page (book, sw, tablbl);
#ifdef USE_GTK2_10
		gtk_notebook_set_tab_reorderable (book, sw, TRUE);
#ifdef E2_TABS_DETACH
//		gtk_notebook_set_tab_detachable (book, sw, TRUE);
#endif
#endif
		sw = _e2p_acl_create_view (ACL_TYPE_ACCESS, &rt);
		tablbl = gtk_label_new (_("General"));
//USELESS ?	e2_widget_set_safetip (tablbl,
//			_("Show the \"access\" ACL, which may apply to any type of item"));
		gtk_notebook_prepend_page (book, sw, tablbl);
#ifdef USE_GTK2_10
		gtk_notebook_set_tab_reorderable (book, sw, TRUE);
#ifdef E2_TABS_DETACH
		gtk_notebook_set_tab_detachable (book, sw, TRUE);
#endif
#endif
		gtk_notebook_set_current_page (book, 0);
	}
	else	//not a dir
	{
		sw = _e2p_acl_create_view (ACL_TYPE_ACCESS, &rt);	//create the permissions treeview
		gtk_box_pack_start (GTK_BOX (sub_vbox), sw, TRUE, TRUE, 0);
	}

	rt.treeview = rt.axs_view;	//dialog starts with the access ACL displayed
	rt.store = rt.axs_store;
//	guint count = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (rt.store), NULL);

	if (!rt.permission) //no permission = no widgets
	{
		GtkTreePath *tpath = gtk_tree_path_new_first ();
		if (rt.acls[AXSNOW] != NULL)
		{
			_e2p_acl_fill_store (rt.axs_store, rt.acls[AXSNOW]);
			sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt.axs_view));
			gtk_tree_selection_select_path (sel, tpath);
		}
		if (rt.acls[DEFNOW] != NULL)
		{
			_e2p_acl_fill_store (rt.def_store, rt.acls[DEFNOW]);
			sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt.def_view));
			gtk_tree_selection_select_path (sel, tpath);
		}
		gtk_tree_path_free (tpath);

		e2_dialog_setup_auth (dialog_vbox);	//show message
	}
	else	//must wait until task-widgets are in place before filling store
	{
		frame = gtk_frame_new (_("Data"));
		gtk_box_pack_start (GTK_BOX (dialog_vbox), frame, FALSE, FALSE, E2_PADDING);

#ifdef USE_GTK3_0
		sub_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
		sub_vbox = gtk_vbox_new (FALSE, 0);
#endif
		gtk_container_add (GTK_CONTAINER (frame), sub_vbox);

		table = e2_widget_add_table (sub_vbox, 1, 5, TRUE, FALSE, 0);  //1 row, 5 cols, homogen, fill, no pad
		rt.shown_perms_btn = e2_button_add_radio_to_table (table, _("S_hown"),
			NULL, (saved_task & E2_ACL_SHOWN), NULL, NULL, 0,1,0,1); //gint left, gint right, gint top, gint bottom
		e2_widget_set_safetip (rt.shown_perms_btn,
			_("Changes will be based only on the data shown above"));
		rt.sysmod_perms_btn = e2_button_add_radio_to_table (table, _("_Varied"),
			gtk_radio_button_get_group (GTK_RADIO_BUTTON (rt.shown_perms_btn)),
			(saved_task & E2_ACL_SYSMOD), NULL, NULL, 1,2,0,1);
		e2_widget_set_safetip (rt.sysmod_perms_btn,
			_("Changes will be based on the standard permissions of the affected"
				" item as modified by the data shown above"));
		rt.system_perms_btn = e2_button_add_radio_to_table (table, _("S_ystem"),
			gtk_radio_button_get_group (GTK_RADIO_BUTTON (rt.shown_perms_btn)),
			(saved_task & E2_ACL_SYSTEM), NULL, NULL, 2,3,0,1);
		e2_widget_set_safetip (rt.system_perms_btn,
			_("Changes will be based only on the standard (non-ACL) permissions"
				" of the affected item"));

		frame = gtk_frame_new (_("Action"));
		gtk_box_pack_start (GTK_BOX (dialog_vbox), frame, FALSE, FALSE, E2_PADDING);

#ifdef USE_GTK3_0
		sub_vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
		sub_vbox = gtk_vbox_new (FALSE, 0);
#endif
		gtk_container_add (GTK_CONTAINER (frame), sub_vbox);

		table = e2_widget_add_table (sub_vbox, 1, 5, TRUE, FALSE, 0);  //1 row, 5 cols, homogen, fill, no pad
		rt.remove_all_btn = e2_button_add_radio_to_table (table, _("_Nuke"),
			NULL, (saved_task & E2_ACL_NUKE), NULL, NULL, 0,1,0,1); //gint left, gint right, gint top, gint bottom
		e2_widget_set_safetip (rt.remove_all_btn,
			_("Clear as much of the item's ACL as possible"));
		rt.set_data_btn = e2_button_add_radio_to_table (table, _("_Set"),
			gtk_radio_button_get_group (GTK_RADIO_BUTTON (rt.remove_all_btn)),
			(saved_task & E2_ACL_SET), NULL, NULL, 1,2,0,1 );
		rt.add_data_btn = e2_button_add_radio_to_table (table, _("_Add"),
			gtk_radio_button_get_group (GTK_RADIO_BUTTON (rt.remove_all_btn)),
			(saved_task & E2_ACL_ADD), NULL, NULL, 2,3,0,1);
		rt.remove_data_btn = e2_button_add_radio_to_table (table, _("_Remove"),
			gtk_radio_button_get_group (GTK_RADIO_BUTTON (rt.remove_all_btn)),
			(saved_task & E2_ACL_REMOVE), NULL, NULL, 3,4,0,1);

		rt.change_whole_btn = e2_button_add_toggle_to_table (table, _("_Whole"),
			(saved_task & E2_ACL_WHOLE), NULL, NULL, 4,5,0,1);
		e2_widget_set_safetip (rt.change_whole_btn,
			_("Conveniently sets all allowed 'whole' values. For those entries,"
				" the action will apply to the whole of the entry, Otherwise,"
				" the action affects only the permissions of that entry"));
		if (rt.thisis_dir)
		{
			table = e2_widget_add_table (sub_vbox, 1, 5, TRUE, FALSE, 0);  //1 row, 5 cols, homogen, fill, no pad
			rt.recurse_btn = e2_button_add_toggle_to_table (table, _("R_ecurse:"),
				!(saved_task & E2_ACL_NODOWN), NULL, NULL, 0,1,0,1);
			e2_widget_set_safetip (rt.recurse_btn,
				_("If activated, changes will be applied to selected items and"
					" also their descendents, if such items match your choice of"
					" \"directories\" and/or \"others\" (anything not a directory)"));
			rt.recurse_dirs_btn = e2_button_add_toggle_to_table (table, _("d_irectories"),
			   (saved_task & (E2_ACL_DIRAXS | E2_ACL_DIRDEF)), NULL, NULL, 1,2,0,1);
			gtk_widget_set_sensitive (rt.recurse_dirs_btn, !(saved_task & E2_ACL_NODOWN));
			rt.recurse_other_btn = e2_button_add_toggle_to_table (table, _("o_thers"),
			   (saved_task & E2_ACL_OTHER), NULL, NULL, 2,3,0,1);
			gtk_widget_set_sensitive (rt.recurse_other_btn, !(saved_task & E2_ACL_NODOWN));

			rt.dir_axs_btn = e2_button_add_toggle_to_table (table, _("dirs-_general"),
			   (saved_task & E2_ACL_DIRAXS), NULL, NULL, 3,4,0,1);
			e2_widget_set_safetip (rt.dir_axs_btn,
				_("if activated, specified changes to the \"general\" ACL"
					" will be applied to any affected directory"));
			rt.dir_def_btn = e2_button_add_toggle_to_table (table, _("dirs-_default"),
			   (saved_task & E2_ACL_DIRDEF), NULL, NULL, 4,5,0,1);
			e2_widget_set_safetip (rt.dir_def_btn,
				_("if activated, specified changes to the \"directories-only\" ACL"
					" will be applied to any affected directory"));
		}
	}

	g_string_free (label_text, TRUE);

	// add buttons in the order that they will appear
	e2_dialog_add_simple_button
		(rt.dialog, STOCK_NAME_HELP, _("_Help"), E2_RESPONSE_USER1);
	if (rt.permission)
	{
		rt.add_row_btn = e2_dialog_add_simple_button
			(rt.dialog, STOCK_NAME_ADD, _("_Add"), E2_RESPONSE_USER2);
		e2_widget_set_safetip (rt.add_row_btn, _("Insert a row in the ACL"));
//		if (count >= MAX_ACL_ENTRIES)
//			gtk_widget_set_sensitive (rt.add_row_btn, FALSE);
		rt.remove_row_btn = e2_dialog_add_simple_button
			(rt.dialog, STOCK_NAME_REMOVE, _("De_lete"), E2_RESPONSE_REMOVE);
		e2_widget_set_safetip (rt.remove_row_btn,
			_("Delete the selected row from the ACL"));
//		if (count == 0)
//			gtk_widget_set_sensitive (rt.remove_row_btn, FALSE);
	}

	E2_Button no_btn;
	if (multi)
	{
		e2_dialog_set_negative_response (rt.dialog, E2_RESPONSE_NOTOALL);
		e2_dialog_add_defined_button (rt.dialog, &E2_BUTTON_CANCEL);
		btn = e2_dialog_add_defined_button (rt.dialog, &E2_BUTTON_APPLYTOALL);
		if (!rt.permission)
			gtk_widget_set_sensitive (btn, FALSE);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_SKIP);
	}
	else
	{
		e2_dialog_set_negative_response (rt.dialog, GTK_RESPONSE_NO);
		e2_button_derive (&no_btn, &E2_BUTTON_NO, BTN_NO_CANCEL);
	}

	e2_dialog_add_defined_button (rt.dialog, &no_btn);
	E2_BUTTON_APPLY.showflags |= E2_BTN_DEFAULT;	//CHECKME local copy ?
	btn = e2_dialog_add_defined_button (rt.dialog, &E2_BUTTON_APPLY);

	//now we can apply the potential-crasher callbacks
	if (rt.permission)
	{
		g_signal_connect (G_OBJECT (rt.remove_all_btn), "toggled",
			G_CALLBACK (_e2p_acl_reset_mode_fields_cb), &rt);
		g_signal_connect (G_OBJECT (rt.shown_perms_btn), "toggled",
			G_CALLBACK (_e2p_acl_set_shown_changes_cb), &rt);
		g_signal_connect (G_OBJECT (rt.sysmod_perms_btn), "toggled",
			G_CALLBACK (_e2p_acl_set_systemplus_changes_cb), &rt);
		g_signal_connect (G_OBJECT (rt.system_perms_btn), "toggled",
			G_CALLBACK (_e2p_acl_set_system_changes_cb), &rt);

		g_signal_connect (G_OBJECT (rt.set_data_btn), "toggled",
			G_CALLBACK (_e2p_acl_reset_mode_fields_cb), &rt);
		g_signal_connect (G_OBJECT (rt.add_data_btn), "toggled",
			G_CALLBACK (_e2p_acl_clear_mode_fields_cb), &rt);
		g_signal_connect (G_OBJECT (rt.remove_data_btn), "toggled",
			G_CALLBACK (_e2p_acl_clear_mode_fields_cb), &rt);

		g_signal_connect (G_OBJECT (rt.change_whole_btn), "toggled",
			G_CALLBACK (_e2p_acl_default_whole_fields_cb), &rt);

		sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt.axs_view));
		g_signal_connect (G_OBJECT (sel), "changed",
			G_CALLBACK (_e2p_acl_selection_change_cb), &rt);

		if (rt.thisis_dir)
		{
			g_signal_connect (G_OBJECT (rt.dir_axs_btn), "toggled",
				G_CALLBACK (_e2p_acl_toggle_recurse_type_cb), &rt);
			g_signal_connect (G_OBJECT (rt.dir_def_btn), "toggled",
				G_CALLBACK (_e2p_acl_toggle_recurse_type_cb), &rt);
			g_signal_connect (G_OBJECT (rt.recurse_btn), "toggled",
				G_CALLBACK (_e2p_acl_toggle_recurse_btn_cb), &rt);
			g_signal_connect (G_OBJECT (rt.recurse_dirs_btn), "toggled",
				G_CALLBACK (_e2p_acl_toggle_recurse_type_cb), &rt);
			g_signal_connect (G_OBJECT (rt.recurse_other_btn), "toggled",
				G_CALLBACK (_e2p_acl_toggle_recurse_type_cb), &rt);
			sel = gtk_tree_view_get_selection (GTK_TREE_VIEW (rt.def_view));
			g_signal_connect (G_OBJECT (sel), "changed",
				G_CALLBACK (_e2p_acl_selection_change_cb), &rt);
			g_signal_connect (G_OBJECT (nbook), "switch-page",
				G_CALLBACK (_e2p_acl_tabchange_cb), &rt);
		}
		//with widgets and callbacks in place, we can fill the store(s)
		if (saved_task & E2_ACL_NUKE)
			_e2p_acl_reset_mode_fields_cb (rt.remove_all_btn, &rt);
		else if (saved_task & E2_ACL_SET)
		{
			_e2p_acl_reset_mode_fields_cb (rt.set_data_btn, &rt);	//fill as for set task
			_e2p_acl_set_system_changes_cb (rt.set_data_btn, &rt);	//adjust some buttons
		}
		else //add or remove
		{
			btn = (saved_task & E2_ACL_ADD) ? rt.add_data_btn : rt.remove_data_btn;
			_e2p_acl_reset_mode_fields_cb (btn, &rt);	//fill as for set task
			_e2p_acl_clear_mode_fields_cb (btn, &rt);	//all buttons same
		}
	}
	else //!rt.permission
		gtk_widget_set_sensitive (btn, FALSE);

	guint count = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (rt.store), NULL);
	if (rt.permission)
	{
		if (count >= MAX_ACL_ENTRIES)
			gtk_widget_set_sensitive (rt.add_row_btn, FALSE);
		if (count == 0)
			gtk_widget_set_sensitive (rt.remove_row_btn, FALSE);
	}

	//setup to increase window vertical size (the header counts as 1)
	guint shown = (count > 10) ? 11 : count + 1;
	gint ht;
	e2_widget_get_font_pixels (rt.treeview, NULL, &ht);
	ht = (ht + 10) * shown;	//+10 fudge factor for interline-spacing

	CLOSEBGL
	e2_dialog_setup (rt.dialog, app.main_window);
	gtk_widget_show_all (rt.dialog);

#ifdef USE_GTK2_18
	GtkAllocation alloc;
	gtk_widget_get_allocation (rt.treeview, &alloc);
	if (ht > alloc.height)
#else
	if (ht > rt.treeview->allocation.height)
#endif
	{
#ifdef USE_GTK2_18
		ht -= alloc.height;
		gtk_widget_get_allocation (rt.dialog, &alloc);
		ht += alloc.height;
		gtk_window_resize (GTK_WINDOW (rt.dialog), alloc.width, ht);
#else
		ht = ht + rt.dialog->allocation.height - rt.treeview->allocation.height;
		gtk_window_resize (GTK_WINDOW (rt.dialog), rt.dialog->allocation.width, ht);
#endif
	}
	DialogButtons choice = e2_dialog_wait (rt.dialog, TRUE, FALSE, multi, TRUE); //CHECKME TRUE maincontext
	if (GTK_IS_DIALOG (rt.dialog)) //not explicitly closed by the user
		gtk_widget_destroy (rt.dialog);	//must do this after the wait i.e. outside main callback
	OPENBGL

	if (rt.acls[AXSNOW] != NULL)
		acl_free (rt.acls[AXSNOW]);
	if (rt.acls[DEFNOW] != NULL)
		acl_free (rt.acls[DEFNOW]);

	*permission_ret = rt.permission;
	*task_ret = rt.task;
	if (choice == OK || choice == YES_TO_ALL)
	{
		*axs_ret = rt.axs_changes;	//possibly NULL
		*def_ret = rt.def_changes;	//possibly NULL
	}
	else
	{
		*axs_ret = NULL;
		*def_ret = NULL;
	}
	return choice;
}

  /*****************/
 /**** actions ****/
/*****************/

#ifdef ACLEXTRA_ACTIONS
/**
@brief transfer ACL's of selected item(s) to matching items, if any, in inactive pane
For any directory, coping is always recursive
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_task_aclcopy (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_CHACL, art, from,
		_e2p_task_aclcopyQ, e2_task_refresh_lists));
}
static gboolean _e2p_task_aclcopyQ (E2_ActionTaskData *qed)
{
#ifdef E2_VFS
	if (qed->currspace != NULL)	//only do ACL's for local items
		return FALSE;
#endif
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
	gchar *other_local = qed->othrdir;
	GString *src = g_string_sized_new (PATH_MAX);
	GString *dest = g_string_sized_new (PATH_MAX);
#ifdef E2_VFS
	VPATH sdata;
	VPATH ddata;
	sdata.spacedata = qed->currspace;
	ddata.spacedata = qed->currspace;
#endif
	mode_t thismode;
	guint count;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	struct stat statbuf;

/* out-of-loop setup = FIXME
	GtkWidget *dialog;
	dialog = e2_permissions_dialog_setup (info, &recurse);
	e2_dialog_add_buttons_simple (dialog, buttons ..., NULL);
*/

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, acl task");
#endif
	e2_filelist_disable_refresh ();
	e2_task_advise ();

	for (count = 0; count < names->len; count++, iterator++)
	{
		g_string_printf (dest, "%s%s", other_local, (*iterator)->filename);
#ifdef E2_VFS
		ddata.path = dest->str;
		if (e2_fs_lstat (&ddata, &statbuf E2_ERR_NONE()))
#else
		if (e2_fs_lstat (dest->str, &statbuf E2_ERR_NONE()))
#endif
			continue;

		thismode = statbuf.st_mode;

		g_string_printf (src, "%s%s", curr_local, (*iterator)->filename);  //separator comes with dir
#ifdef E2_VFS
		sdata.path = src->str;
		if (e2_fs_lstat (&sdata, &statbuf E2_ERR_NONE()))
#else
		if (e2_fs_lstat (src->str, &statbuf E2_ERR_NONE()))
#endif
			continue;

		if ((statbuf.st_mode & S_IFMT) != (thismode & S_IFMT))
			continue;

		if (S_ISDIR (statbuf.st_mode))
		{
			E2_CopyACLData data = { 0 };
			data.task = E2_ACL_DIRDEF | E2_ACL_DIRAXS | E2_ACL_OTHER | E2_ACL_DIR
 				| E2_ACL_SET | E2_ACL_SYSTEM;
			data.oldroot_len = strlen (src->str);
#ifdef E2_VFS
			VPATH localother = { qed->othrdir, qed->othrspace };
			data.otherdir = &localother;
			e2_fs_tw (&sdata,
#else
			data.otherdir = qed->othrdir;
			e2_fs_tw (src->str,
#endif
				_e2p_acl_twcb_copyacl, &data, -1,
			//flags for: no thru-links, this filesystem only, breadth-first
				E2TW_MOUNT | E2TW_PHYS E2_ERR_NONE());
		}
		else
		{
#ifdef E2_INCLIST
# ifdef E2_VFS
			if (_e2p_acl_copyacls (&sdata, &statbuf, &ddata))
# else
			if (_e2p_acl_copyacls (src->str, &statbuf, dest->str))
# endif
			{
				//FIXME update line in treeview
			}
#else
# ifdef E2_FAM
#  ifdef E2_VFS
			_e2p_acl_copyacls (&sdata, &statbuf, &ddata);
#  else
			_e2p_acl_copyacls (src->str, &statbuf, dest->str);
#  endif
# else
#  ifdef E2_VFS
			if (_e2p_acl_copyacls (&sdata, &statbuf, &ddata))
			{
				if (ddata.spacedata == NULL)
				{
					ddata.path = qed->othrdir;
					e2_fs_touchnow (&ddata E2_ERR_NONE());
				}
			}
#  else
			if (_e2p_acl_copyacls (src->str, &statbuf, dest->str))
				//make the file-list refresher notice successful change
				e2_fs_touchnow (qed->othrdir E2_ERR_NONE());
#  endif
# endif
#endif
		}
	}
	g_string_free (src, TRUE);
	g_string_free (dest, TRUE);
	e2_window_clear_status_message ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, acl task");
#endif
	e2_filelist_enable_refresh ();

	return TRUE;
}
#endif //def ACLEXTRA_ACTIONS

/**
@brief show/change extended permissions of selected item(s) in active pane
If > 1 item is selected, a dialog is created for each such item in turn (or
until the user chooses stop or apply-to-all)
@param from the button, menu item etc which was activated
@param art action runtime data

@return TRUE if action completed successfully, else FALSE
*/
static gboolean _e2p_task_acl (gpointer from, E2_ActionRuntime *art)
{
	return (e2_task_enqueue_task (E2_TASK_CHACL, art, from,
		_e2p_task_aclQ, e2_task_refresh_lists));
}
static gboolean _e2p_task_aclQ (E2_ActionTaskData *qed)
{
	//printd (DEBUG, "task: acl");
#ifdef E2_VFS
	if (qed->currspace != NULL)
		return FALSE;	//only manage ACL's for local items
#endif
	gpointer workspace = acl_init (2);	//working heapspace for retrieved access and default ACLs
	if (workspace == NULL)
		//FIXME warn user
		return FALSE;

//	printd (DEBUG, "task: acl");
	GPtrArray *names = qed->names;
	gchar *curr_local = qed->currdir;
	GPtrArray *axs_changes = NULL;
	GPtrArray *def_changes = NULL;
	guint count;
	gboolean multisrc = names->len > 1;
	gboolean all = FALSE;
	E2_SelectedItemInfo **iterator = (E2_SelectedItemInfo **) names->pdata;
	GString *path = g_string_sized_new (PATH_MAX+NAME_MAX);
#ifdef E2_VFS
	VPATH ddata;
	ddata.spacedata = qed->currspace;
#endif
/* out-of-loop setup = FIXME
	GtkWidget *dialog;
	dialog = e2_permissions_dialog_setup (info, &recurse);
	e2_dialog_add_buttons_simple (dialog, buttons ..., NULL);
*/

#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "disable refresh, acl task");
#endif
	e2_filelist_disable_refresh ();
	e2_task_advise ();

	for (count=0; count < names->len; count++, iterator++)
	{
		DialogButtons choice;
		E2_ACLTask task = 0;	//assignment for warning prevention
		gboolean permission;
		//".." entries filtered when names compiled
//FIXME for single-setup: instead of the following, adjust file details in dialog, reset default button
		g_string_printf (path, "%s%s", curr_local, (*iterator)->filename); //separator comes with dir
#ifdef E2_VFS
		ddata.path = path->str;
#endif
		if (all)
		{
			//check if we have permission to change this item
			//NB this tests _all_ w permissions, chmod is not so sophisticated ...
#ifdef E2_VFS
 			permission = e2_fs_check_write_permission (&ddata E2_ERR_NONE());
#else
 			permission = e2_fs_check_write_permission (path->str E2_ERR_NONE());
#endif
			if (permission)
				choice = OK;
			else
			{
#ifdef E2_VFS
				ddata.path = (*iterator)->filename;
#endif
				e2_fs_error_simple (
					_("You do not have authority to change permission(s) of %s"),
#ifdef E2_VFS
					&ddata);
#else
					(*iterator)->filename);
#endif
				choice = CANCEL;
			}
		}
		else
		{
#ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "enable refresh, acl dialog");
#endif
			e2_filelist_enable_refresh ();  //allow updates while we wait
			*qed->status = E2_TASK_PAUSED;
#ifdef E2_VFS
			choice = _e2p_acl_dialog_run (&ddata, &axs_changes, &def_changes,
#else
			choice = _e2p_acl_dialog_run (path->str, &axs_changes, &def_changes,
#endif
				&task, &permission, multisrc);
			*qed->status = E2_TASK_RUNNING;
#ifdef E2_REFRESH_DEBUG
			printd (DEBUG, "disable refresh, permissions dialog");
#endif
			e2_filelist_disable_refresh ();
		}

		switch (choice)
		{
		  case YES_TO_ALL:
			all = TRUE;
			choice = OK;
		  case OK:
			if (permission && (axs_changes != NULL || def_changes != NULL))
			{
#ifdef E2_INCLIST
				if (_e2p_acl_change (path->str, axs_changes, def_changes, task))
				{
					//FIXME update line in treeview
				}
#else
# ifdef E2_FAM
#  ifdef E2_VFS
				_e2p_acl_change (&ddata, axs_changes, def_changes, task);
#  else
				_e2p_acl_change (path->str, axs_changes, def_changes, task);
#  endif
# else
#  ifdef E2_VFS
			if (_e2p_acl_change (&ddata, axs_changes, def_changes, task))
			{
				if (ddata.spacedata == NULL)
				{
					ddata.path = qed->currdir;
					e2_fs_touchnow (&ddata E2_ERR_NONE());
				}
			}
#  else
			if (_e2p_acl_change (path->str, axs_changes, def_changes, task))
				//make the file-list refresher notice successful change
				e2_fs_touchnow (qed->currdir E2_ERR_NONE());
#  endif
# endif
#endif
				if (!all)
				{
					if (axs_changes != NULL)
					{
//#ifndef USE_GLIB2_22
						g_ptr_array_foreach (axs_changes,
							(GFunc)_e2p_acl_clean_changes, NULL);
//#endif
						g_ptr_array_free (axs_changes, TRUE);
						axs_changes = NULL;
					}
					if (def_changes != NULL)
					{
//#ifndef USE_GLIB2_22
						g_ptr_array_foreach (def_changes,
							(GFunc)_e2p_acl_clean_changes, NULL);
//#endif
						g_ptr_array_free (def_changes, TRUE);
						def_changes = NULL;
					}
				}
			}
		  case CANCEL:
			break;
		  default:
			choice = NO_TO_ALL;  // break flag;
			break;
		}
		if (choice == NO_TO_ALL)
			break;
	}

	if (axs_changes != NULL)
	{
//#ifndef USE_GLIB2_22
		g_ptr_array_foreach (axs_changes, (GFunc)_e2p_acl_clean_changes, NULL);
//#endif
		g_ptr_array_free (axs_changes, TRUE);
		axs_changes = NULL;
	}
	if (def_changes != NULL)
	{
//#ifndef USE_GLIB2_22
		g_ptr_array_foreach (def_changes, (GFunc)_e2p_acl_clean_changes, NULL);
//#endif
		g_ptr_array_free (def_changes, TRUE);
		def_changes = NULL;
	}

	acl_free (workspace);

	g_string_free (path, TRUE);
	e2_window_clear_status_message ();
#ifdef E2_REFRESH_DEBUG
	printd (DEBUG, "enable refresh, acl task");
#endif
	e2_filelist_enable_refresh ();

	return TRUE;
}

/**
@brief plugin initialization function, called by main program

@param mode flags enumerating what sort of init to perform

@return Plugin*, with refcount 1 if @a mode included runtime setup and that succeeded
*/
Plugin *init_plugin (E2PInit mode)
{
	PLUGINIT_INTRO
	PLUGINIT_NUMBERED_ALLOCATE(2)

	PLUGINIT_NUMBERED_ACTION(1,_A(6),_("acl"),_e2p_task_acl,
		_("Change _ACLs.."),
		_("Change extended permissions of selected items"),
		"plugin_"ANAME E2ICONTB)
	PLUGINIT_NUMBERED_ACTION(2,_A(6),_("copy_acl"),_e2p_task_aclcopy,
		_("_Replicate"),
		_("Recursively apply ACLs of selected items to matching items in the other pane"),
		NULL)

	//more setup if the above succeeded
	if (/*(mode & E2P_SETUP) == 0 || */iface.refcount == 1)
	{
		guint i;
		for (i = 0; i < CLASSCOUNT; i++)
			classinames[i] = gettext (classnames[i]);

		//setup to preserve acl's during copying
		//but if some acl-using file-operations are in progress, defer
		pthread_mutex_lock (&task_mutex);
		while (1)
		{
			GList *member = app.taskhistory;
			while (member != NULL)
			{
				E2_TaskRuntime *rt = (E2_TaskRuntime *) member->data;
recheck:
				if (rt != NULL
					&& (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED))
				{
					if (rt->action)
					{
						E2_TaskType tt = rt->ex.action.tasktype;
						switch (tt)
						{
							case E2_TASK_COPY:
							case E2_TASK_COPYAS:
							case E2_TASK_MOVE:	//may revert to a copy
							case E2_TASK_MOVEAS:
							case E2_TASK_TRASH:
								usleep (200000);
								goto recheck;
								break;
							default:
								member = NULL;
								break;
						}
						if (member == NULL)
							break;
					}
				}
				member = member->next;
			}
			if (member == NULL)
			{
				g_atomic_pointer_set (&copyaclfunc, _e2p_acl_copyacls);	//turn on the acl callback
				break;
			}
		}
		pthread_mutex_unlock (&task_mutex);
	}
	
	PLUGINIT_NUMBERED_END
}
/**
@brief cleanup transient things for this plugin

@param p pointer to data struct for the plugin

@return TRUE if all cleanups were completed
*/
gboolean clean_plugin (Plugin *p)
{
	//if some acl-using file-operations are in progress, defer the unload
	pthread_mutex_lock (&task_mutex);
	while (1)
	{
		GList *member = app.taskhistory;
		while (member != NULL)
		{
			E2_TaskRuntime *rt = (E2_TaskRuntime *) member->data;
recheck:
			if (rt != NULL
				&& (rt->status == E2_TASK_RUNNING || rt->status == E2_TASK_PAUSED))
			{
				if (rt->action)
				{
					E2_TaskType tt = rt->ex.action.tasktype;
					switch (tt)
					{
						case E2_TASK_COPY:
						case E2_TASK_COPYAS:
						case E2_TASK_MOVE:	//may revert to a copy
						case E2_TASK_MOVEAS:
						case E2_TASK_TRASH:
						case E2_TASK_CHACL:
							usleep (200000);
							goto recheck;
							break;
						default:
							member = NULL;
							break;
					}
					if (member == NULL)
						break;
				}
			}
			member = member->next;
		}
		if (member == NULL)
		{
			g_atomic_pointer_set (&copyaclfunc, NULL);	//ok to turn-off the acl callback
			break;
		}
	}
	pthread_mutex_unlock (&task_mutex);

	PLUGIN_CLEAR_ACTIONS (p)
	return ret;
}

#ifdef FAKEACLSTORE
# undef FAKEACLSTORE
#endif
