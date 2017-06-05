/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Hiroyuki Yamamoto and the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

#ifndef __FOLDER_H__
#define __FOLDER_H__

#include <glib.h>
#include <time.h>

typedef struct _Folder		Folder;
typedef struct _FolderClass	FolderClass;

typedef struct _FolderItem	FolderItem;
typedef struct _FolderUpdateData	FolderUpdateData;
typedef struct _FolderItemUpdateData	FolderItemUpdateData;
typedef struct _PersistPrefs		PersistPrefs;

#define FOLDER(obj)		((Folder *)obj)
#define FOLDER_CLASS(obj)	(FOLDER(obj)->klass)
#define FOLDER_TYPE(obj)	(FOLDER(obj)->klass->type)

#define FOLDER_IS_LOCAL(obj)	(FOLDER_TYPE(obj) == F_MH      || \
				 FOLDER_TYPE(obj) == F_MBOX    || \
				 FOLDER_TYPE(obj) == F_MAILDIR)

#define FOLDER_ITEM(obj)	((FolderItem *)obj)

#define FOLDER_UPDATE_HOOKLIST "folder_update"
#define FOLDER_ITEM_UPDATE_HOOKLIST "folder_item_update"

typedef enum
{
	F_MH,
	F_MBOX,
	F_MAILDIR,
	F_IMAP,
	F_NEWS,
	F_UNKNOWN
} FolderType;

typedef enum
{
	F_NORMAL,
	F_INBOX,
	F_OUTBOX,
	F_DRAFT,
	F_QUEUE,
	F_TRASH
} SpecialFolderItemType;

typedef enum
{
	SORT_BY_NONE,
	SORT_BY_NUMBER,
	SORT_BY_SIZE,
	SORT_BY_DATE,
	SORT_BY_FROM,
	SORT_BY_SUBJECT,
	SORT_BY_SCORE,
	SORT_BY_LABEL,
	SORT_BY_MARK,
	SORT_BY_STATUS,
	SORT_BY_MIME,
	SORT_BY_TO,
	SORT_BY_LOCKED,
	SORT_BY_TAGS,
	SORT_BY_THREAD_DATE
} FolderSortKey;

typedef enum
{
	SORT_ASCENDING,
	SORT_DESCENDING
} FolderSortType;

typedef enum
{
	F_MOVE_OK = 0,
	F_MOVE_FAILED_DEST_IS_PARENT = -1,
	F_MOVE_FAILED_DEST_IS_CHILD = -2,
	F_MOVE_FAILED_DEST_OUTSIDE_MAILBOX = -3,
	F_MOVE_FAILED = -4
} FolderItemMoveResult;

typedef enum
{
	FOLDER_ADD_FOLDER 		= 1 << 0,
	FOLDER_REMOVE_FOLDER 		= 1 << 1,
	FOLDER_TREE_CHANGED 		= 1 << 2,
	FOLDER_ADD_FOLDERITEM 		= 1 << 3,
	FOLDER_REMOVE_FOLDERITEM 	= 1 << 4,
	FOLDER_RENAME_FOLDERITEM	= 1 << 5
} FolderUpdateFlags;

typedef enum
{
	F_ITEM_UPDATE_MSGCNT = 1 << 0,
	F_ITEM_UPDATE_CONTENT = 1 << 1,
	F_ITEM_UPDATE_ADDMSG = 1 << 2,
	F_ITEM_UPDATE_REMOVEMSG = 1 << 3,
	F_ITEM_UPDATE_NAME = 1 << 4
} FolderItemUpdateFlags;

typedef void (*FolderUIFunc)		(Folder		*folder,
					 FolderItem	*item,
					 gpointer	 data);
typedef void (*FolderDestroyNotify)	(Folder		*folder,
					 FolderItem	*item,
					 gpointer	 data);
typedef void (*FolderItemFunc)	(FolderItem	*item,
					 gpointer	 data);


#include "proctypes.h"
#include "xml.h"
#include "prefs_account.h"
#include "matchertypes.h"

struct _MsgCache;

struct _Folder
{
	FolderClass *klass;

	gchar *name;
	PrefsAccount *account;
	guint sort;

	FolderItem *inbox;
	FolderItem *outbox;
	FolderItem *draft;
	FolderItem *queue;
	FolderItem *trash;

	FolderUIFunc ui_func;
	gpointer     ui_func_data;

	GNode *node;

	gpointer data;

	GHashTable *newsart;
};

/**
 * Callback used to convey progress information of a specific search.
 *
 * \param data User-provided data
 * \param on_server Whether or not the current progress information originated from the
 *                  server
 * \param at Number of the last message processed
 * \param matched Number of messages with definitive matches found so far
 * \param total Number of messages to be processed
 *
 * \note
 * Even if the mailserver does not support progress reports, an instance of this type
 * should be invoked when serverside search starts and ends, with \c at set to \c 0 and
 * \c total, respectively.
 */
typedef gboolean (*SearchProgressNotify)(gpointer data, gboolean on_server, guint at, guint matched, guint total);

struct _FolderClass
{
	/**
	 * A numeric identifier for the FolderClass. Will be removed in the future
	 */
	FolderType  type;
	/**
	 * A string identifier for the FolderClass. Currently used in folderlist.xml.
	 * Should be lowercase.
	 */
	gchar 	   *idstr;
	/**
	 * A string for the User Interface that identifies the FolderClass to the
	 * user. Can be upper and lowercase unlike the idstr.
	 */
	gchar	   *uistr;
	/**
	* A boolean to indicate whether or not the FolderClass supports search on the
	* server. If \c TRUE, setting \c on_server in \c search_msgs offloads search to
	* the server.
	*/
	gboolean    supports_server_search;

	
	/* virtual functions */

	/* Folder funtions */
	/**
	 * Create a new \c Folder of this \c FolderClass.
	 *
	 * \param name The name of the new Folder
	 * \param path The path of the new Folder
	 * \return The new \c Folder, or \c NULL when creating the \c Folder 
	 *         failed
	 */
	Folder 		*(*new_folder)		(const gchar	*name,
						 const gchar	*path);
	/**
	 * Destroy a \c Folder of this \c FolderClass, frees all resources
	 * allocated by the Folder
	 *
	 * \param folder The \c Folder that should be destroyed.
	 */
	void     	(*destroy_folder)	(Folder		*folder);
	/**
	 * Set the Folder's internal attributes from an \c XMLTag. Also sets the
	 * parameters of the root-FolderItem of the \c Folder. If \c NULL
	 * the default function of the basic \¢ FolderClass is used, so it
	 * must not be \c NULL if one of the parent \c FolderClasses has a \c set_xml
	 * function. In that case the parent \c FolderClass' \c set_xml function
	 * can be used or it has to be called with the \c folder and \c tag by
	 * the implementation.
	 *
	 * \param folder The \c Folder which's attributes should be updated
	 * \param tag The \c XMLTag containing the \c XMLAttrs for the attributes
	 */
	void		 (*set_xml)		(Folder		*folder,
						 XMLTag		*tag);
	/**
	 * Get an \c XMLTag for the attributes of the \c Folder and the root-FolderItem
	 * of the \c Folder. If \c NULL the default implementation for the basic
	 * FolderClass will be used, so it must not be \c NULL if one of the
	 * parent \c FolderClasses has it's own implementation for \c get_xml.
	 * In that case the parent FolderClass' \c get_xml function can be
	 * used or the \c XMLTag has to be fetched from the parent's \c get_xml
	 * function and then the \c FolderClass specific attributes can be
	 * added to it.
	 *
	 * \param Folder The \c Folder which's attributes should be set in the
	 *               \c XMLTag's \c XMLAttrs
	 * \return XMLTag An \c XMLTag with \c XMLAttrs containing the \c Folder's
	 *                attributes.
	 */
	XMLTag		*(*get_xml)		(Folder		*folder);
	/**
	 * Rebuild the folder tree from the folder's data
	 * \todo New implementations of MH and IMAP are actually syncronizing
	 *       the tree with the folder by reusing the old \c FolderItems.
	 *       Claws still destroys the old tree before calling this function.
	 *
	 * \param folder The folder which's tree should be rebuild
	 * \return 0 on success, a negative number otherwise
	 */
	gint     	(*scan_tree)		(Folder		*folder);

	gint     	(*create_tree)		(Folder		*folder);

	/* FolderItem functions */
	/**
	 * Create a new \c FolderItem structure for the \c FolderClass.
	 * \c FolderClasses can have their own \c FolderItem structure with
	 * extra attributes.
	 *
	 * \param folder The \c Folder for that a \c FolderItem should be
	 *               created
	 * \return The new \c FolderItem or NULL in case of an error
	 */
	FolderItem	*(*item_new)		(Folder		*folder);
	/**
	 * Destroy a \c FolderItem from this \c FolderClass. The \c FolderClass
	 * has to free all private resources used by the \c FolderItem.
	 *
	 * \param folder The \c Folder of the \c FolderItem
	 * \param item The \c FolderItem that should be destroyed
	 */
	void	 	 (*item_destroy)	(Folder		*folder,
						 FolderItem	*item);
	/**
	 * Set the \c FolderItem's internal attributes from an \c XMLTag. If
	 * \c NULL the default function of the basic \c FolderClass is used, so it
	 * must not be \c NULL if one of the parent \c FolderClasses has a \c item_set_xml
	 * function. In that case the parent \c FolderClass' \c item_set_xml function
	 * can be used or it has to be called with the \c folder, \c item and \c tag by
	 * the implementation.
	 *
	 * \param folder The \c Folder of the \c FolderItem
	 * \param item The \c FolderItems which's attributes should be set
	 * \param tag The \c XMLTag with \c XMLAttrs for the \c FolderItem's
	 *            attributes
	 */
	void		 (*item_set_xml)	(Folder		*folder,
						 FolderItem	*item,
						 XMLTag		*tag);
	/**
	 * Get an \c XMLTag for the attributes of the \c FolderItem If \c NULL
	 * the default implementation for the basic \c FolderClass will be used,
	 * so it must not be \c NULL if one of the parent \c FolderClasses has
	 * it's own implementation for \c item_get_xml. In that case the parent 
	 * FolderClass' \c item_get_xml function can be used or the \c XMLTag
	 * has to be fetched from the parent's \c item_get_xml function and 
	 * then the \c FolderClass specific attributes can be added to it.
	 *
	 * \param folder The \c Folder of the \c FolderItem
	 * \parem item The \c FolderItem which's attributes should be set in
	 *             the \c XMLTag's \c XMLAttrs
	 * \return An \c XMLTag with \c XMLAttrs containing the \c FolderItem's
	 *         attributes.
	 */
	XMLTag		*(*item_get_xml)	(Folder		*folder,
						 FolderItem	*item);
	/**
	 * Get a local path for the \c FolderItem where Sylpheed can save
	 * it's cache data. For local directory based folders this can be the
	 * real path. For other folders it can be the local cache directory.
	 *
	 * \param folder The \c Folder of the \c FolderItem
	 * \param item The \c FolderItem for that a path should be returned
	 * \return A path for the \c FolderItem
	 */
	gchar		*(*item_get_path)	(Folder		*folder,
						 FolderItem	*item);
	/**
	 * Create a new \c FolderItem. The function must use folder_item_append
	 * to add the new \c FolderItem to the folder tree
	 *
	 * \param folder The \c Folder in which a new \c FolderItem should be
	 *               created
	 * \param parent \c The parent \c FolderItem for the new \c FolderItem
	 * \parem name The name for the new \c FolderItem
	 * \return The new \c FolderItem
	 */
	FolderItem 	*(*create_folder)	(Folder		*folder,
						 FolderItem	*parent,
						 const gchar	*name);
	/**
	 * Rename a \c FolderItem
	 *
	 * \param folder The \c Folder of the \c FolderItem that should be
	 *               renamed
	 * \param item The \c FolderItem that should be renamed
	 * \param name The new name of the \c FolderItem
	 * \return 0 on success, a negative number otherwise
	 */
	gint     	 (*rename_folder)	(Folder		*folder,
						 FolderItem	*item,
						 const gchar	*name);
	/**
	 * Remove a \c FolderItem from the \c Folder
	 *
	 * \param folder The \c Folder that contains the \c FolderItem
	 * \param item The \c FolderItem that should be removed
	 * \return 0 on sucess, a negative number otherwise
	 */
	gint     	 (*remove_folder)	(Folder		*folder,
						 FolderItem	*item);
	/**
	 * Close a \c FolderItem. Called when the user deselects a
	 * \c FolderItem.
	 * 
	 * \attention In Sylpheed-Main operations can only be done on the
	 *            \c FolderItem that is opened in the SummaryView. This
	 *            \c FolderItem will be closed when you select a new
	 *            \c FolderItem in the FolderView. In Claws operations can
	 *            be done any time on any folder and you should not expect
	 *            that all \c FolderItems get closed after operations
	 *
	 * \param folder The \c Folder that contains the \c FolderItem
	 * \param item The \c FolderItem that should be closed
	 * \return 0 on success, a negative number otherwise
	 */
	gint		 (*close)		(Folder		*folder,
						 FolderItem	*item);
	/**
	 * Get the list of message numbers for the messages in the \c FolderItem
	 *
	 * \param folder The \c Folder that contains the \c FolderItem
	 * \param item The \c FolderItem for which the message numbers should
	 *             be fetched
	 * \param list Pointer to a GSList where message numbers have to be
	 *             added. Because of the implementation of the GSList that
	 *             changes the pointer of the GSList itself when the first
	 *             item is added this is a pointer to a pointer to a
	 *             GSList structure. Use *item = g_slist_...(*item, ...)
	 *             operations to modify the list.
	 * \param old_uids_valid In some \c Folders the old UIDs can be invalid.
	 *                       Set this pointer to a gboolean to TRUE if the
	 *                       old UIDs are still valid, otherwise set it to
	 *                       FALSE and the folder system will discard it's
	 *                       cache data of the previously know UIDs
	 * \return The number of message numbers add to the list on success,
	 *         a negative number otherwise.
	 */
	gint	 	 (*get_num_list)	(Folder		*folder,
						 FolderItem	*item,
						 GSList	       **list,
						 gboolean	*old_uids_valid);
	/**
	 * Tell the folder system if a \c FolderItem should be scanned
	 * (cache data syncronized with the folder content) when it is required
	 * because the \c FolderItem's content changed. If NULL the folder
	 * system will not do automatic scanning of \c FolderItems
	 *
	 * \param folder The \c Folder that contains the \c FolderItem
	 * \param item The \c FolderItem which's content should be checked
	 * \return TRUE if the \c FolderItem should be scanned, FALSE otherwise
	 */
	gboolean	(*scan_required)	(Folder 	*folder,
						 FolderItem 	*item);

	/**
	 * Updates the known mtime of a folder
	 */
	void		(*set_mtime)		(Folder 	*folder,
						 FolderItem 	*item);

	/* Message functions */
	/**
	 * Get a MsgInfo for a message in a \c FolderItem
	 *
	 * \param folder The \c Folder containing the message
	 * \param item The \c FolderItem containing the message
	 * \param num The message number of the message
	 * \return A pointer to a \c MsgInfo decribing the message or \c 
	 *         NULL in case of an error
	 */
	MsgInfo 	*(*get_msginfo)		(Folder		*folder,
						 FolderItem	*item,
						 gint		 num);
	/**
	 * Get \c MsgInfos for a list of message numbers
	 *
	 * \param folder The \c Folder containing the message
	 * \param item The \c FolderItem containing the message
	 * \param msgnum_list A list of message numbers for which the
	 *                    \c MsgInfos should be fetched
	 * \return A list of \c MsgInfos for the messages in the \c msgnum_list
	 *         that really exist. Messages that are not found can simply
	 *         be left out.
	 */
	MsgInfoList  	*(*get_msginfos)	(Folder		*folder,
						 FolderItem	*item,
						 MsgNumberList	*msgnum_list);
	/**
	 * Get the filename for a message. This can either be the real message
	 * file for local folders or a temporary file for remote folders.
	 *
	 * \param folder The \c Folder containing the message
	 * \param item The \c FolderItem containing the message
	 * \param num The message number of the message
	 * \return A string with the filename of the message file. The returned
	 *         string has to be freed with \c g_free(). If message is not
	 *         available return NULL.
	 */
	gchar 		*(*fetch_msg)		(Folder		*folder,
						 FolderItem	*item,
						 gint		 num);
	gchar 		*(*fetch_msg_full)	(Folder		*folder,
						 FolderItem	*item,
						 gint		 num,
						 gboolean	 headers,
						 gboolean	 body);
	/**
	 * Add a single message file to a folder with the given flags (if
	 * flag handling is supported by the folder)
	 *
	 * \param folder The target \c Folder for the message
	 * \param dest the target \c FolderItem for the message
	 * \param file The file that contains the message
	 * \param flags The flags the new message should have in the folder
	 * \return 0 on success, a negative number otherwise
	 */
	gint     	(*add_msg)		(Folder		*folder,
						 FolderItem	*dest,
						 const gchar	*file,
						 MsgFlags	*flags);
	/**
	 * Add multiple messages to a \c FolderItem. If NULL the folder
	 * system will add messages with \c add_msg one by one
	 *
	 * \param folder The target \c Folder for the messages
	 * \param dest the target \c FolderItem for the messages
	 * \param file_list A list of \c MsgFileInfos which contain the
	 *                  filenames and flags for the new messages
	 * \param relation Insert tuples of (MsgFileInfo, new message number) to
	 *                 provide feedback for the folder system which new
	 *                 message number a \c MsgFileInfo got in dest. Insert
	 *                 0 if the new message number is unknown.
	 */
	gint     	(*add_msgs)             (Folder         *folder,
                                    		 FolderItem     *dest,
                                    		 GSList         *file_list,
                                    		 GHashTable	*relation);
	/**
	 * Copy a message to a FolderItem
	 *
	 * \param folder The \c Folder of the destination FolderItem
	 * \param dest The destination \c FolderItem for the message
	 * \param msginfo The message that should be copied
	 * \return The message number the copied message got, 0 if it is
	 *         unknown because message numbers are assigned by an external
	 *         system and not available after copying or a negative number
	 *         if an error occuried
	 */
	gint    	(*copy_msg)		(Folder		*folder,
						 FolderItem	*dest,
						 MsgInfo	*msginfo);
	/**
	 * Copy multiple messages to a \c FolderItem. If \c NULL the folder
	 * system will use \c copy_msg to copy messages one by one.
	 *
	 * \param folder The \c Folder of the destination FolderItem
	 * \param dest The destination \c FolderItem for the message
	 * \param msglist A list of \c MsgInfos which should be copied to dest
	 * \param relation Insert tuples of (MsgInfo, new message number) to
	 *                 provide feedback for the folder system which new
	 *                 message number a \c MsgInfo got in dest. Insert
	 *                 0 if the new message number is unknown.
	 * \return 0 on success, a negative number otherwise
	 */
	gint    	(*copy_msgs)		(Folder		*folder,
						 FolderItem	*dest,
						 MsgInfoList	*msglist,
                                    		 GHashTable	*relation);

	/**
	 * Search the given FolderItem for messages matching \c predicate.
	 * The search may be offloaded to the server if the \c folder
	 * supports server side search, as indicated by \c supports_server_search.
	 *
	 * \param folder The \c Folder of the container FolderItem
	 * \param container The \c FolderItem containing the messages to be searched
	 * \param msgs The \c MsgNumberList results will be saved to.
	 *             If <tt>*msgs != NULL</tt>, the search will be restricted to
	 *             messages whose numbers are contained therein.
	 *             If \c on_server is considered \c FALSE, messages are guaranteed to
	 *             be processed in the order they are listed in \c msgs.
	 *             On error, \c msgs will not be changed.
	 * \param on_server Whether or not the search should be offloaded to the server.
	 *                  If \c on_server is not \c NULL and points to a \c TRUE value,
	 *                  search will be done on the server. If \c predicate contains
	 *                  one or more atoms the server does not support, the value
	 *                  pointed to by \c on_server will be set to \c FALSE upon return.
	 *                  In this case, \c msgs must still contain a valid superset of
	 *                  messages actually matched by \c predicate, or this method must
	 *                  return an error.
	 *                  \c on_server may only point to a \c TRUE value if
	 *                  \c supports_server_search is also \c TRUE.
	 *                  \c NULL and pointer to \c FALSE are considered equivalent and
	 *                  will start a client-only search.
	 * \param predicate The \c MatcherList to use in the search
	 * \param progress_cb Called for every message searched.
	 *                    When search is offloaded to the server, this function
	 *                    may or may not be called, depending on the implementation.
	 *                    The second argument of this function will be the number of
	 *                    messages already processed.
	 *                    Return \c FALSE from this function to end the search.
	 *                    May be \c NULL, no calls will be made in this case.
	 * \param progress_data First argument value for \c progress_cb
	 * \return Number of messages that matched \c predicate on success, a negative
	 *         number otherwise.
	 *
	 * \note
	 * When search is stopped by returning \c FALSE from \c progress_cb, \c msgs will
	 * contain all messages found until the point of cancellation. The number of
	 * messages found will be returned as indicated above.
	 */
	gint		(*search_msgs)		(Folder			*folder,
						 FolderItem		*container,
						 MsgNumberList		**msgs,
						 gboolean		*on_server,
						 MatcherList		*predicate,
						 SearchProgressNotify	progress_cb,
						 gpointer		progress_data);


	/**
	 * Remove a message from a \c FolderItem.
	 *
	 * \param folder The \c Folder of the message
	 * \param item The \c FolderItem containing the message
	 * \param num The message number of the message
	 * \return 0 on success, a negative number otherwise
	 */
	gint    	(*remove_msg)		(Folder		*folder,
						 FolderItem	*item,
						 gint		 num);
	gint    	(*remove_msgs)		(Folder		*folder,
						 FolderItem	*item,
						 MsgInfoList    *msglist,
						 GHashTable	*relation);
	gint    	(*expunge)		(Folder		*folder,
						 FolderItem	*item);
	/**
	 * Remove all messages in a \ c FolderItem
	 *
	 * \param folder The \c Folder of the \c FolderItem
	 * \param item The \FolderItem which's messages should be deleted
	 * \return 0 on succes, a negative number otherwise
	 */
	gint    	(*remove_all_msg)	(Folder		*folder,
						 FolderItem	*item);
	/**
	 * Check if a message has been modified by someone else
	 *
	 * \param folder The \c Folder of the message
	 * \param item The \c FolderItem containing the message
	 * \param msginfo The \c MsgInfo for the message that should be checked
	 * \return \c TRUE if the message was modified, \c FALSE otherwise
	 */
	gboolean	(*is_msg_changed)	(Folder		*folder,
						 FolderItem	*item,
						 MsgInfo	*msginfo);
	/**
	 * Update a message's flags in the folder data. If NULL only the
	 * internal flag management will be used. The function has to set
	 * \c msginfo->flags.perm_flags. It does not have to set the flags
	 * that it got as \c newflags. If a flag can not be set in this
	 * \c FolderClass the function can refuse to set it. Flags that are not
	 * supported by the \c FolderClass should not be refused. They will be
	 * managed by the internal cache in this case.
	 *
	 * \param folder The \c Folder of the message
	 * \param item The \c FolderItem of the message
	 * \param msginfo The \c MsgInfo for the message which's flags should be
	 *                updated
	 * \param newflags The flags the message should get
	 */
	void    	(*change_flags)		(Folder		*folder,
						 FolderItem	*item,
						 MsgInfo        *msginfo,
						 MsgPermFlags	 newflags);
	/**
	 * Get the flags for a list of messages. Flags that are not supported
	 * by the folder should be preserved. They can be copied from
	 * \c msginfo->flags.perm_flags
	 *
	 * \param folder The \c Folder of the messages
	 * \param item The \c FolderItem of the messages
	 * \param msglist The list of \c MsgInfos for which the flags should
	 *                   be returned
	 * \param msgflags A \c GRelation for tuples of (MsgInfo, new permanent
         *        flags for MsgInfo). Add tuples for the messages in msglist
	 * \return 0 on success, a negative number otherwise
	 */
	gint		(*get_flags)		(Folder		*folder,
						 FolderItem	*item,
						 MsgInfoList	*msglist,
						 GHashTable	*msgflags);
	
	/* Sets batch mode for a FolderItem. It means that numerous flags updates
	 * could follow, and the FolderClass implementation can cache them in order
	 * to process them later when set_false will be called again with the
	 * batch parameter set to FALSE. 
	 */
	void		(*set_batch)		(Folder		*folder,
						 FolderItem	*item,
						 gboolean	 batch);
	/* Called when switching offline or asking for synchronisation. the imple
	 * mentation should do what's necessary to be able to read mails present
	 * in the FolderItem at this time with no network connectivity. 
	 * Days: max number of days of mail to fetch.
	 */
	void		(*synchronise)		(FolderItem	*item,
						 gint		 days);
	
	/* Passed from claws-mail --subscribe scheme://uri. Implementations
	 * should check if they handle this type of URI, and return TRUE in this
	 * case after having subscribed it.
	 */
	gboolean	(*subscribe)		(Folder 	*folder,
						 const gchar	*uri);
	
	/* Gets the preferred sort key and type for a folderclass. */
	void		(*get_sort_type)	(Folder		*folder,
						 FolderSortKey	*sort_key,
						 FolderSortType	*sort_type);
	
	/* Copies internal FolderItem data from one folderItem to another. Used
	 * when moving folders (this move is in reality a folder creation, content
	 * move, folder delettion).
	 */
	void		(*copy_private_data)	(Folder		*folder,
						 FolderItem	*src,
						 FolderItem	*dest);

	void		(*remove_cached_msg)	(Folder		*folder,
						 FolderItem	*item,
						 MsgInfo 	*msginfo);
	void		(*commit_tags)		(FolderItem	*item,
						 MsgInfo 	*msginfo,
						 GSList		*tags_set,
						 GSList		*tags_unset);
	void		(*item_opened)		(FolderItem	*item);
	void		(*item_closed)		(FolderItem	*item);
};

enum {
	ITEM_NOT_SCANNING,
	ITEM_SCANNING_WITH_FLAGS,
	ITEM_SCANNING
};

struct _FolderItemPrefs;

struct _FolderItem
{
	SpecialFolderItemType stype;

	gchar *name; /* UTF-8 */
	gchar *path; /* UTF-8 */

	time_t mtime;

	gint new_msgs;
	gint unread_msgs;
	gint total_msgs;
	gint unreadmarked_msgs;
	gint marked_msgs;
	gint replied_msgs;
	gint forwarded_msgs;
	gint locked_msgs;
	gint ignored_msgs;
	gint watched_msgs;

	gint order;

	gint last_num;

	struct _MsgCache *cache;
	gboolean cache_dirty;
	gboolean mark_dirty;
	gboolean tags_dirty;

	/* special flags */
	guint no_sub         : 1; /* no child allowed?    */
	guint no_select      : 1; /* not selectable?      */
	guint collapsed      : 1; /* collapsed item       */
	guint thread_collapsed      : 1; /* collapsed item       */
	guint threaded       : 1; /* threaded folder view */
	guint hide_read_msgs : 1; /* hide read messages   */
	guint ret_rcpt       : 1; /* return receipt       */
	guint search_match   : 1;
	guint hide_del_msgs : 1; /* hide deleted messages   */
	guint hide_read_threads : 1; /* hide threads with only read messages   */

	gint op_count;
	guint opened         : 1; /* opened by summary view */
	FolderItemUpdateFlags update_flags; /* folderview for this folder should be updated */

	FolderSortKey sort_key;
	FolderSortType sort_type;

	GNode *node;

	Folder *folder;

	PrefsAccount *account;

	gboolean apply_sub;
	
	GSList *mark_queue;

	gpointer data;

	struct _FolderItemPrefs * prefs;
	
	/* for faster search of special parents */
	SpecialFolderItemType parent_stype;
	gboolean processing_pending;
	gint scanning;
	guint last_seen;
};

struct _PersistPrefs
{
	FolderSortKey	sort_key;
	FolderSortType	sort_type;
	guint		collapsed	: 1;
	guint		thread_collapsed	: 1;
	guint		threaded	: 1;
	guint		hide_read_msgs	: 1; /* CLAWS */
	guint		ret_rcpt	: 1; /* CLAWS */
	guint		hide_del_msgs	: 1; /* CLAWS */
	guint		hide_read_threads	: 1;
};

struct _FolderUpdateData
{
	Folder			*folder;
	FolderUpdateFlags	 update_flags;
	FolderItem		*item;
};

struct _FolderItemUpdateData
{
	FolderItem		*item;
	FolderItemUpdateFlags	 update_flags;
	MsgInfo			*msg;
};

void	    folder_system_init		(void);
void	    folder_register_class	(FolderClass	*klass);
void	    folder_unregister_class	(FolderClass	*klass);
Folder     *folder_new			(FolderClass	*type,
					 const gchar	*name,
					 const gchar	*path);
void 	    folder_init			(Folder		*folder,
					 const gchar	*name);

void        folder_destroy		(Folder		*folder);

void 	    folder_set_xml		(Folder		 *folder,
					 XMLTag		 *tag);
XMLTag 	   *folder_get_xml		(Folder		 *folder);

FolderItem *folder_item_new		(Folder		*folder,
				 	 const gchar	*name,
				 	 const gchar	*path);
void        folder_item_append		(FolderItem	*parent,
				 	 FolderItem	*item);
void        folder_item_remove		(FolderItem	*item);
void        folder_item_remove_children	(FolderItem	*item);
void        folder_item_destroy		(FolderItem	*item);
FolderItem *folder_item_parent		(FolderItem	*item);

void 	    folder_item_set_xml		(Folder		 *folder,
					 FolderItem	 *item,
					 XMLTag		 *tag);
XMLTag 	   *folder_item_get_xml		(Folder		 *folder,
					 FolderItem	 *item);

void        folder_set_ui_func	(Folder		*folder,
				 FolderUIFunc	 func,
				 gpointer	 data);
void        folder_set_name	(Folder		*folder,
				 const gchar	*name);
void	    folder_set_sort	(Folder		*folder,
				 guint		 sort);
void        folder_tree_destroy	(Folder		*folder);

void   folder_add		(Folder		*folder);
void   folder_remove		(Folder 	*folder);

GList *folder_get_list		(void);
gint   folder_read_list		(void);
void   folder_write_list	(void);
void   folder_scan_tree		(Folder *folder, gboolean rebuild);
void   folder_fast_scan_tree	(Folder *folder);
FolderItem *folder_create_folder(FolderItem	*parent, const gchar *name);
gint   folder_item_rename	(FolderItem *item, gchar *newname);
void   folder_update_op_count		(void);
void   folder_func_to_all_folders	(FolderItemFunc function,
					 gpointer data);
void folder_count_total_msgs(guint *new_msgs, guint *unread_msgs, 
			     guint *unreadmarked_msgs, guint *marked_msgs,
			     guint *total_msgs, guint *replied_msgs,
			     guint *forwarded_msgs, guint *locked_msgs,
			     guint *ignored_msgs, guint *watched_msgs);
gchar *folder_get_status	(GPtrArray	*folders,
				 gboolean	 full);

Folder 	   *folder_find_from_identifier		(const gchar *identifier);
Folder     *folder_find_from_path		(const gchar	*path);
Folder     *folder_find_from_name		(const gchar	*name,
						 FolderClass	*klass);
FolderItem *folder_find_item_from_path		(const gchar	*path);
FolderItem *folder_find_item_from_real_path	(const gchar 	*path);
FolderClass *folder_get_class_from_string	(const gchar 	*str);
FolderItem *folder_find_child_item_by_name	(FolderItem	*item,
						 const gchar	*name);
/* return value is locale charset */
gchar 	   *folder_get_identifier		(Folder *folder);
/* return value is locale charset */
gchar      *folder_item_get_identifier		(FolderItem	*item);
FolderItem *folder_find_item_from_identifier	(const gchar	*identifier);
FolderItem *folder_get_item_from_identifier	(const gchar	*identifier);
gchar 	   *folder_item_get_name		(FolderItem 	*item);

FolderItem *folder_get_default_inbox	(void);
FolderItem *folder_get_default_inbox_for_class(FolderType type);
FolderItem *folder_get_default_outbox	(void);
FolderItem *folder_get_default_outbox_for_class(FolderType type);
FolderItem *folder_get_default_draft	(void);
FolderItem *folder_get_default_draft_for_class(FolderType type);
FolderItem *folder_get_default_queue	(void);
FolderItem *folder_get_default_queue_for_class(FolderType type);
FolderItem *folder_get_default_trash	(void);
FolderItem *folder_get_default_trash_for_class(FolderType type);
FolderItem *folder_get_default_processing (void);
void folder_set_missing_folders		(void);
void folder_unref_account_all		(PrefsAccount	*account);

/* return value is locale encoded file name */
gchar *folder_item_get_path		(FolderItem	*item);

gint   folder_item_open			(FolderItem	*item);
gint   folder_item_close		(FolderItem	*item);
gint   folder_item_scan			(FolderItem	*item);
gint   folder_item_scan_full		(FolderItem 	*item, 
					 gboolean 	 filtering);
MsgInfo *folder_item_get_msginfo	(FolderItem 	*item,
					 gint		 num);
MsgInfo *folder_item_get_msginfo_by_msgid(FolderItem 	*item,
					 const gchar 	*msgid);
GSList *folder_item_get_msg_list	(FolderItem 	*item);
MsgNumberList *folder_item_get_number_list(FolderItem *item);

/* return value is locale charset */
gchar *folder_item_fetch_msg		(FolderItem	*item,
					 gint		 num);
gchar *folder_item_fetch_msg_full	(FolderItem	*item,
					 gint		 num, 
					 gboolean 	 get_headers,
					 gboolean	 get_body);
gint   folder_item_add_msg		(FolderItem	*dest,
					 const gchar	*file,
					 MsgFlags	*flags,
					 gboolean	 remove_source);
gint   folder_item_add_msgs             (FolderItem     *dest,
                                         GSList         *file_list,
                                         gboolean        remove_source);
gint   folder_item_move_to		(FolderItem	*src,
					 FolderItem	*dest,
					 FolderItem    **new_item,
					 gboolean	 copy);
gint   folder_item_move_msg		(FolderItem	*dest,
					 MsgInfo	*msginfo);
gint   folder_item_move_msgs		(FolderItem	*dest,
					 GSList		*msglist);
gint   folder_item_copy_msg		(FolderItem	*dest,
					 MsgInfo	*msginfo);
gint   folder_item_copy_msgs		(FolderItem	*dest,
					 GSList		*msglist);
gint   folder_item_search_msgs		(Folder			*folder,
					 FolderItem		*container,
					 MsgNumberList		**msgs,
					 gboolean		*on_server,
					 MatcherList		*predicate,
					 SearchProgressNotify	progress_cb,
					 gpointer		progress_data);
gint   folder_item_remove_msg		(FolderItem	*item,
					 gint		 num);
gint   folder_item_remove_msgs		(FolderItem	*item,
					 GSList		*msglist);
gint   folder_item_expunge		(FolderItem	*item);
gint   folder_item_remove_all_msg	(FolderItem	*item);
void 	folder_item_change_msg_flags	(FolderItem 	*item,
					 MsgInfo 	*msginfo,
					 MsgPermFlags 	 newflags);
gboolean folder_item_is_msg_changed	(FolderItem	*item,
					 MsgInfo	*msginfo);

void folder_clean_cache_memory		(FolderItem *protected_item);
void folder_clean_cache_memory_force	(void);
void folder_item_write_cache		(FolderItem *item);

void folder_item_apply_processing	(FolderItem *item);

void folder_item_update			(FolderItem *item,
					 FolderItemUpdateFlags update_flags);
void folder_item_update_recursive	(FolderItem *item,
					 FolderItemUpdateFlags update_flags);
void folder_item_update_freeze		(void);
void folder_item_update_thaw		(void);
void folder_item_set_batch		(FolderItem *item, gboolean batch);
gboolean folder_has_parent_of_type	(FolderItem *item, SpecialFolderItemType type);
gboolean folder_is_child_of		(FolderItem *item, FolderItem *possibleChild);
void folder_synchronise			(Folder *folder);
gboolean folder_want_synchronise	(Folder *folder);
gboolean folder_subscribe		(const gchar *uri);
gboolean folder_have_mailbox 		(void);
gboolean folder_item_free_cache		(FolderItem *item, gboolean force);
void folder_item_change_type		(FolderItem *item,
					 SpecialFolderItemType newtype);
gboolean folder_get_sort_type		(Folder		*folder,
					 FolderSortKey	*sort_key,
					 FolderSortType	*sort_type);
void folder_item_synchronise		(FolderItem *item);
void folder_item_discard_cache		(FolderItem *item);
void folder_item_commit_tags(FolderItem *item, MsgInfo *msginfo, GSList *tags_set, GSList *tags_unset);



gint folder_item_search_msgs_local	(Folder			*folder,
					 FolderItem		*container,
					 MsgNumberList		**msgs,
					 gboolean		*on_server,
					 MatcherList		*predicate,
					 SearchProgressNotify	progress_cb,
					 gpointer		progress_data);

#endif /* __FOLDER_H__ */
