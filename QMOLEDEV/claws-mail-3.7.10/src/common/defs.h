/*
 * Sylpheed -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Hiroyuki Yamamoto and the Claws Mail team
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

#ifndef __DEFS_H__
#define __DEFS_H__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <glibconfig.h>

#ifdef G_OS_WIN32
#  include <glib/gwin32.h>
#endif

#if HAVE_PATHS_H
#  include <paths.h>
#endif

#if HAVE_SYS_PARAM_H
#  include <sys/param.h>
#endif

#define INBOX_DIR		"inbox"
#define OUTBOX_DIR		"sent"
#define QUEUE_DIR		"queue"
#define DRAFT_DIR		"draft"
#define TRASH_DIR		"trash"
#define RC_DIR			CFG_RC_DIR
#define OLD_GTK2_RC_DIR		".sylpheed-claws"
#define OLDER_GTK2_RC_DIR	".sylpheed-gtk2"
#define OLD_GTK1_RC_DIR		".sylpheed"
#define SYLPHEED_RC_DIR		".sylpheed-2.0"
#define NEWS_CACHE_DIR		"newscache"
#define IMAP_CACHE_DIR		"imapcache"
#define MBOX_CACHE_DIR		"mboxcache"
#define HEADER_CACHE_DIR        "headercache" 
#define MIME_TMP_DIR		"mimetmp"
#define COMMON_RC		"clawsrc"
#define OLD_COMMON_RC		"sylpheedrc"
#define ACCOUNT_RC		"accountrc"
#define CUSTOM_HEADER_RC	"customheaderrc"
#define DISPLAY_HEADER_RC	"dispheaderrc"
#define FOLDERITEM_RC           "folderitemrc"
#define SCORING_RC              "scoringrc"
#define FILTERING_RC		"filteringrc"
#define MATCHER_RC		"matcherrc"
#define MENU_RC			"menurc"
#define RENDERER_RC		"rendererrc"
#define TAGS_RC			"tagsrc"
#define QUICKSEARCH_HISTORY	"quicksearch_history"
#define SUMMARY_SEARCH_FROM_HISTORY	"summarysearch_from_history"
#define SUMMARY_SEARCH_TO_HISTORY	"summarysearch_to_history"
#define SUMMARY_SEARCH_SUBJECT_HISTORY	"summarysearch_subject_history"
#define SUMMARY_SEARCH_BODY_HISTORY	"summary_searchbody_history"
#define SUMMARY_SEARCH_ADV_CONDITION_HISTORY	"summarysearch_adv_history"
#define MESSAGE_SEARCH_HISTORY	"messagesearch_history"
#define COMPOSE_SAVE_TO_HISTORY	"compose_save_to_history"
#define ADDRESSBOOK_CUSTOM_ATTRIBUTES "attributesrc"
#define TEMPLATE_DIR		"templates"
#define TMP_DIR			"tmp"
#define UIDL_DIR		"uidl"
#define NEWSGROUP_LIST		".newsgroup_list"
#define ADDRESS_BOOK		"addressbook.xml"
#define ADDRBOOK_DIR		"addrbook"
#define MANUAL_HTML_INDEX	"claws-mail-manual.html"
#define HOMEPAGE_URI		"http://www.claws-mail.org/"
#define MANUAL_URI		"http://www.claws-mail.org/documentation.php"
#define FAQ_URI			"http://www.claws-mail.org/faq/index.php"
#define PLUGINS_URI		"http://www.claws-mail.org/plugins.php"
#define DICTS_URI		"http://www.claws-mail.org/win32/dictionaries.php"
#define BUGZILLA_URI		"http://www.thewildbeast.co.uk/claws-mail/bugzilla/enter_bug.cgi"
#define THEMES_URI		"http://www.claws-mail.org/themes.php"
#define TOOLS_URI		"http://www.claws-mail.org/tools.php"
#define MAILING_LIST_URI	"http://www.claws-mail.org/MLs.php"
#define USERS_ML_ADDR		"claws-mail-users@dotsrc.org"
#define GPL_URI			"http://www.gnu.org/licenses/gpl.html"
#define DONATE_URI		"http://www.claws-mail.org/sponsors.php"
#define RELEASE_NOTES_FILE	"RELEASE_NOTES"
#define THEMEINFO_FILE		".claws_themeinfo"
#define FOLDER_LIST		"folderlist.xml"
#define OLD_CACHE_FILE		".sylpheed_claws_cache"
#define CACHE_FILE		".claws_cache"
#define OLD_MARK_FILE		".sylpheed_mark"
#define MARK_FILE		".claws_mark"
#define TAGS_FILE		".claws_tags"
#define PRINTING_PAGE_SETUP_STORAGE_FILE "print_page_setup"
#define CACHE_VERSION		24
#define MARK_VERSION		2
#define TAGS_VERSION		1

#ifdef MAEMO
#define MMC1_PATH "/media/mmc1"
#define MMC2_PATH "/media/mmc2"
#endif

#ifdef G_OS_WIN32
#  define ACTIONS_RC		"actionswinrc"
#  define COMMAND_HISTORY	"command_history_win"
#  define DEFAULT_SIGNATURE	"signature.txt"
#else
#  define ACTIONS_RC		"actionsrc"
#  define COMMAND_HISTORY	"command_history"
# ifndef MAEMO
#  define DEFAULT_SIGNATURE	".signature"
# else
#  define DEFAULT_SIGNATURE	"MyDocs/signature.txt"
# endif
#endif

#define DEFAULT_INC_PATH	"/usr/bin/mh/inc"
#define DEFAULT_INC_PROGRAM	"inc"
/* #define DEFAULT_INC_PATH	"/usr/bin/imget" */
/* #define DEFAULT_INC_PROGRAM	"imget" */
#define DEFAULT_SENDMAIL_CMD	"/usr/sbin/sendmail -t -i"
#define DEFAULT_BROWSER_CMD	"firefox '%s'"
#ifndef MAEMO
#define DEFAULT_EDITOR_CMD	"gedit '%s'"
#else
#define DEFAULT_EDITOR_CMD	"leafpad '%s'"
#endif
#define DEFAULT_MIME_CMD	"metamail -d -b -x -c %s '%s'"
#define DEFAULT_IMAGE_VIEWER_CMD "display '%s'"
#define DEFAULT_AUDIO_PLAYER_CMD "play '%s'"

#ifdef _PATH_MAILDIR
#  define DEFAULT_SPOOL_PATH	_PATH_MAILDIR
#else
#  define DEFAULT_SPOOL_PATH	"/var/spool/mail"
#endif

#define BUFFSIZE			8192

#ifndef MAXPATHLEN
#  define MAXPATHLEN			4095
#endif

#define DEFAULT_HEIGHT			460
#define DEFAULT_FOLDERVIEW_WIDTH	179
#define DEFAULT_MAINVIEW_WIDTH		600
#define DEFAULT_SUMMARY_HEIGHT		140
#define DEFAULT_HEADERVIEW_HEIGHT	40
#define DEFAULT_COMPOSE_HEIGHT		560
#define BORDER_WIDTH			2
#define CTREE_INDENT			18
#define FOLDER_SPACING			4
#define MAX_ENTRY_LENGTH		8191
#define COLOR_DIM			35000
#define UI_REFRESH_INTERVAL		50000	/* usec */
#define FOLDER_UPDATE_INTERVAL		1500	/* msec */
#define PROGRESS_UPDATE_INTERVAL	200	/* msec */
#define SESSION_TIMEOUT_INTERVAL	60	/* sec */
#define MAX_HISTORY_SIZE		32

#define BOLD_FONT prefs_common.boldfont
#define NORMAL_FONT prefs_common.normalfont
#define SMALL_FONT	prefs_common.smallfont

#define DEFAULT_PIXMAP_THEME	"INTERNAL_DEFAULT"
#define PIXMAP_THEME_DIR		"themes"

#endif /* __DEFS_H__ */
