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

#include "defs.h"
#include <glib.h>
#include <gtk/gtk.h>
#include <string.h>
#include <dirent.h>

#include "config.h"
#include "stock_pixmap.h"
#include "gtkutils.h"
#include "utils.h"
#include "prefs_common.h"

#include "pixmaps/addr_one.xpm"
#include "pixmaps/addr_two.xpm"
#include "pixmaps/address.xpm"
#include "pixmaps/anonymous.xpm"
#include "pixmaps/book.xpm"
#include "pixmaps/category.xpm"
#include "pixmaps/checkbox_off.xpm"
#include "pixmaps/checkbox_on.xpm"
#include "pixmaps/clip.xpm"
#include "pixmaps/clipkey.xpm"
#include "pixmaps/complete.xpm"
#include "pixmaps/continue.xpm"
#include "pixmaps/deleted.xpm"
#include "pixmaps/error.xpm"
#include "pixmaps/edit_extern.xpm"
#include "pixmaps/forwarded.xpm"
#include "pixmaps/group.xpm"
#include "pixmaps/insert_file.xpm"
#include "pixmaps/interface.xpm"
#include "pixmaps/jpilot.xpm"
#include "pixmaps/key.xpm"
#include "pixmaps/key_gpg_signed.xpm"
#include "pixmaps/ldap.xpm"
#include "pixmaps/linewrap.xpm"
#include "pixmaps/linewrapcurrent.xpm"
#include "pixmaps/mark.xpm"
#include "pixmaps/locked.xpm"
#include "pixmaps/new.xpm"
#include "pixmaps/replied.xpm"
#include "pixmaps/replied_and_forwarded.xpm"
#include "pixmaps/close.xpm"
#include "pixmaps/down_arrow.xpm"
#include "pixmaps/up_arrow.xpm"
#include "pixmaps/exec.xpm"
#include "pixmaps/mail.xpm"
#include "pixmaps/mail_attach.xpm"
#include "pixmaps/mail_compose.xpm"
#include "pixmaps/mail_forward.xpm"
#include "pixmaps/mail_receive.xpm"
#include "pixmaps/mail_receive_all.xpm"
#include "pixmaps/mail_reply.xpm"
#include "pixmaps/mail_reply_to_all.xpm"
#include "pixmaps/mail_reply_to_author.xpm"
#include "pixmaps/mail_reply_to_list.xpm"
#include "pixmaps/mail_send.xpm"
#include "pixmaps/mail_send_queue.xpm"
#include "pixmaps/mail_sign.xpm"
#include "pixmaps/open_mail.xpm"
#include "pixmaps/news_compose.xpm"
#include "pixmaps/paste.xpm"
#include "pixmaps/preferences.xpm"
#include "pixmaps/properties.xpm"
#include "pixmaps/queue_close.xpm"
#include "pixmaps/queue_close_hrm.xpm"
#include "pixmaps/queue_open.xpm"
#include "pixmaps/queue_open_hrm.xpm"
#include "pixmaps/claws-mail_icon.xpm"
#ifndef GENERIC_UMPC
#include "pixmaps/claws-mail_logo.xpm"
#else
#include "pixmaps/claws-mail_logo-small.xpm"
#endif
#include "pixmaps/address_book.xpm"
#include "pixmaps/unread.xpm"
#include "pixmaps/read.xpm"
#include "pixmaps/vcard.xpm"
#include "pixmaps/ignorethread.xpm"
#include "pixmaps/online.xpm"
#include "pixmaps/offline.xpm"
#include "pixmaps/notice_warn.xpm"
#include "pixmaps/notice_error.xpm"
#include "pixmaps/notice_note.xpm"
#include "pixmaps/quicksearch.xpm"
#include "pixmaps/clip_gpg_signed.xpm"
#include "pixmaps/gpg_signed.xpm"
#include "pixmaps/go_folders.xpm"
#include "pixmaps/mime_text_plain.xpm"
#include "pixmaps/mime_text_html.xpm"
#include "pixmaps/mime_text_patch.xpm"
#include "pixmaps/mime_application.xpm"
#include "pixmaps/mime_image.xpm"
#include "pixmaps/mime_audio.xpm"
#include "pixmaps/mime_text_enriched.xpm"
#include "pixmaps/mime_unknown.xpm"
#include "pixmaps/mime_pdf.xpm"
#include "pixmaps/mime_ps.xpm"
#include "pixmaps/mime_calendar.xpm"
#include "pixmaps/mime_pgpsig.xpm"
#include "pixmaps/printer.xpm"
#include "pixmaps/privacy_signed.xpm"
#include "pixmaps/privacy_passed.xpm"
#include "pixmaps/privacy_failed.xpm"
#include "pixmaps/privacy_unknown.xpm"
#include "pixmaps/privacy_expired.xpm"
#include "pixmaps/privacy_warn.xpm"
#include "pixmaps/privacy_emblem_encrypted.xpm"
#include "pixmaps/privacy_emblem_signed.xpm"
#include "pixmaps/privacy_emblem_passed.xpm"
#include "pixmaps/privacy_emblem_failed.xpm"
#include "pixmaps/privacy_emblem_warn.xpm"
#include "pixmaps/mime_message.xpm"
#include "pixmaps/address_search.xpm"
#include "pixmaps/check_spelling.xpm"

#include "pixmaps/dir_close.xpm"
#include "pixmaps/dir_close_hrm.xpm"
#include "pixmaps/dir_open.xpm"
#include "pixmaps/dir_open_hrm.xpm"
#include "pixmaps/inbox_open.xpm"
#include "pixmaps/inbox_open_hrm.xpm"
#include "pixmaps/inbox_close.xpm"
#include "pixmaps/inbox_close_hrm.xpm"
#include "pixmaps/outbox_open.xpm"
#include "pixmaps/outbox_open_hrm.xpm"
#include "pixmaps/outbox_close.xpm"
#include "pixmaps/outbox_close_hrm.xpm"
#include "pixmaps/trash.xpm"
#include "pixmaps/delete_btn.xpm"
#include "pixmaps/cancel.xpm"
#include "pixmaps/trash_btn.xpm"
#include "pixmaps/trash_hrm.xpm"
#include "pixmaps/drafts_close.xpm"
#include "pixmaps/drafts_open.xpm"
#include "pixmaps/dir_close_mark.xpm"
#include "pixmaps/dir_close_hrm_mark.xpm"
#include "pixmaps/dir_open_mark.xpm"
#include "pixmaps/dir_open_hrm_mark.xpm"
#include "pixmaps/inbox_open_mark.xpm"
#include "pixmaps/inbox_open_hrm_mark.xpm"
#include "pixmaps/inbox_close_mark.xpm"
#include "pixmaps/inbox_close_hrm_mark.xpm"
#include "pixmaps/outbox_open_mark.xpm"
#include "pixmaps/outbox_open_hrm_mark.xpm"
#include "pixmaps/outbox_close_mark.xpm"
#include "pixmaps/outbox_close_hrm_mark.xpm"
#include "pixmaps/trash_mark.xpm"
#include "pixmaps/queue_close_mark.xpm"
#include "pixmaps/queue_close_hrm_mark.xpm"
#include "pixmaps/queue_open_mark.xpm"
#include "pixmaps/queue_open_hrm_mark.xpm"
#include "pixmaps/trash_hrm_mark.xpm"
#include "pixmaps/drafts_close_mark.xpm"
#include "pixmaps/drafts_open_mark.xpm"
#include "pixmaps/dir_noselect.xpm"
#include "pixmaps/spam.xpm"
#include "pixmaps/spam_btn.xpm"
#include "pixmaps/ham_btn.xpm"
#include "pixmaps/moved.xpm"
#include "pixmaps/copied.xpm"
#include "pixmaps/selection.xpm"
#include "pixmaps/watchthread.xpm"
#include "pixmaps/empty.xpm"
#include "pixmaps/tray_newmail.offline.xpm"
#include "pixmaps/tray_newmail.xpm"
#include "pixmaps/tray_newmarkedmail.offline.xpm"
#include "pixmaps/tray_newmarkedmail.xpm"
#include "pixmaps/tray_nomail.offline.xpm"
#include "pixmaps/tray_nomail.xpm"
#include "pixmaps/tray_unreadmail.offline.xpm"
#include "pixmaps/tray_unreadmail.xpm"
#include "pixmaps/tray_unreadmarkedmail.offline.xpm"
#include "pixmaps/tray_unreadmarkedmail.xpm"

typedef struct _StockPixmapData	StockPixmapData;

struct _StockPixmapData
{
	gchar **data;
	cairo_surface_t *pixmap;
	cairo_pattern_t *mask;
	gchar *file;
	gchar *icon_path;
	GdkPixbuf *pixbuf;
};

typedef struct _OverlayData OverlayData;

struct _OverlayData
{
	gboolean is_pixmap;
	cairo_surface_t *base_pixmap;
	cairo_surface_t *overlay_pixmap;
	
	GdkPixbuf *base_pixbuf;
	GdkPixbuf *overlay_pixbuf;
	
	guint base_height;
	guint base_width;
	guint overlay_height;
	guint overlay_width;
	OverlayPosition position;
	gint border_x;
	gint border_y;
	gboolean highlight;
};

static void stock_pixmap_find_themes_in_dir(GList **list, const gchar *dirname);

static StockPixmapData pixmaps[] =
{
	{addr_one_xpm				, NULL, NULL, "addr_one", NULL},
	{addr_two_xpm				, NULL, NULL, "addr_two", NULL},
	{address_xpm				, NULL, NULL, "address", NULL},
	{address_book_xpm			, NULL, NULL, "address_book", NULL},
	{address_search_xpm			, NULL, NULL, "address_search", NULL},
	{anonymous_xpm				, NULL, NULL, "anonymous", NULL},
	{book_xpm				, NULL, NULL, "book", NULL},
	{category_xpm				, NULL, NULL, "category", NULL},
	{checkbox_off_xpm			, NULL, NULL, "checkbox_off", NULL},
	{checkbox_on_xpm			, NULL, NULL, "checkbox_on", NULL},
	{check_spelling_xpm                     , NULL, NULL, "check_spelling", NULL},
	{clip_xpm				, NULL, NULL, "clip", NULL},
	{clipkey_xpm				, NULL, NULL, "clipkey", NULL},
	{clip_gpg_signed_xpm			, NULL, NULL, "clip_gpg_signed", NULL},
	{close_xpm				, NULL, NULL, "close", NULL},
	{complete_xpm				, NULL, NULL, "complete", NULL},
	{continue_xpm				, NULL, NULL, "continue", NULL},
	{deleted_xpm				, NULL, NULL, "deleted", NULL},
	{dir_close_xpm				, NULL, NULL, "dir_close", NULL},
	{dir_close_hrm_xpm			, NULL, NULL, "dir_close_hrm", NULL},
	{dir_open_xpm				, NULL, NULL, "dir_open", NULL},
	{dir_open_hrm_xpm			, NULL, NULL, "dir_open_hrm", NULL},
	{dir_close_mark_xpm			, NULL, NULL, "dir_close_mark", NULL},
	{dir_close_hrm_mark_xpm			, NULL, NULL, "dir_close_hrm_mark", NULL},
	{dir_open_mark_xpm			, NULL, NULL, "dir_open_mark", NULL},
	{dir_open_hrm_mark_xpm			, NULL, NULL, "dir_open_hrm_mark", NULL},
	{down_arrow_xpm				, NULL, NULL, "down_arrow", NULL},
	{up_arrow_xpm				, NULL, NULL, "up_arrow", NULL},
	{edit_extern_xpm			, NULL, NULL, "edit_extern", NULL},
	{error_xpm				, NULL, NULL, "error", NULL},
	{exec_xpm				, NULL, NULL, "exec", NULL},
	{forwarded_xpm				, NULL, NULL, "forwarded", NULL},
	{group_xpm				, NULL, NULL, "group", NULL},
	{ignorethread_xpm			, NULL, NULL, "ignorethread", NULL},
	{inbox_close_xpm			, NULL, NULL, "inbox_close", NULL},
	{inbox_close_hrm_xpm			, NULL, NULL, "inbox_close_hrm", NULL},
	{inbox_open_xpm				, NULL, NULL, "inbox_open", NULL},
	{inbox_open_hrm_xpm			, NULL, NULL, "inbox_open_hrm", NULL},
	{inbox_close_mark_xpm			, NULL, NULL, "inbox_close_mark", NULL},
	{inbox_close_hrm_mark_xpm		, NULL, NULL, "inbox_close_hrm_mark", NULL},
	{inbox_open_mark_xpm			, NULL, NULL, "inbox_open_mark", NULL},
	{inbox_open_hrm_mark_xpm		, NULL, NULL, "inbox_open_hrm_mark", NULL},
	{insert_file_xpm			, NULL, NULL, "insert_file", NULL},
	{interface_xpm				, NULL, NULL, "interface", NULL},
	{jpilot_xpm				, NULL, NULL, "jpilot", NULL},
	{key_xpm				, NULL, NULL, "key", NULL},
	{key_gpg_signed_xpm			, NULL, NULL, "key_gpg_signed_xpm", NULL},
	{ldap_xpm				, NULL, NULL, "ldap", NULL},
	{linewrapcurrent_xpm			, NULL, NULL, "linewrapcurrent", NULL},
	{linewrap_xpm				, NULL, NULL, "linewrap", NULL},
	{locked_xpm				, NULL, NULL, "locked", NULL},
	{mail_xpm				, NULL, NULL, "mail", NULL},
	{mail_attach_xpm			, NULL, NULL, "mail_attach", NULL},
	{mail_compose_xpm			, NULL, NULL, "mail_compose", NULL},
	{mail_forward_xpm			, NULL, NULL, "mail_forward", NULL},
	{mail_receive_xpm			, NULL, NULL, "mail_receive", NULL},
	{mail_receive_all_xpm			, NULL, NULL, "mail_receive_all", NULL},
	{mail_reply_xpm				, NULL, NULL, "mail_reply", NULL},
	{mail_reply_to_all_xpm			, NULL, NULL, "mail_reply_to_all", NULL},
	{mail_reply_to_author_xpm		, NULL, NULL, "mail_reply_to_author", NULL},
	{mail_reply_to_list_xpm			, NULL, NULL, "mail_reply_to_list", NULL},
	{mail_send_xpm				, NULL, NULL, "mail_send", NULL},
	{mail_send_queue_xpm			, NULL, NULL, "mail_send_queue", NULL},
	{mail_sign_xpm				, NULL, NULL, "mail_sign", NULL},
	{open_mail_xpm				, NULL, NULL, "open_mail", NULL},
	{mark_xpm				, NULL, NULL, "mark", NULL},
	{new_xpm				, NULL, NULL, "new", NULL},
	{news_compose_xpm			, NULL, NULL, "news_compose", NULL},
	{outbox_close_xpm			, NULL, NULL, "outbox_close", NULL},
	{outbox_close_hrm_xpm			, NULL, NULL, "outbox_close_hrm", NULL},
	{outbox_open_xpm			, NULL, NULL, "outbox_open", NULL},
	{outbox_open_hrm_xpm			, NULL, NULL, "outbox_open_hrm", NULL},
	{outbox_close_mark_xpm			, NULL, NULL, "outbox_close_mark", NULL},
	{outbox_close_hrm_mark_xpm		, NULL, NULL, "outbox_close_hrm_mark", NULL},
	{outbox_open_mark_xpm			, NULL, NULL, "outbox_open_mark", NULL},
	{outbox_open_hrm_mark_xpm		, NULL, NULL, "outbox_open_hrm_mark", NULL},
	{replied_xpm				, NULL, NULL, "replied", NULL},
	{replied_and_forwarded_xpm	, NULL, NULL, "replied_and_forwarded", NULL},
	{paste_xpm				, NULL, NULL, "paste", NULL},
	{preferences_xpm			, NULL, NULL, "preferences", NULL},
	{properties_xpm				, NULL, NULL, "properties", NULL},
	{queue_close_xpm			, NULL, NULL, "queue_close", NULL},
	{queue_close_hrm_xpm			, NULL, NULL, "queue_close_hrm", NULL},
	{queue_open_xpm				, NULL, NULL, "queue_open", NULL},
	{queue_open_hrm_xpm			, NULL, NULL, "queue_open_hrm", NULL},
	{trash_xpm				, NULL, NULL, "trash_open", NULL},
	{trash_hrm_xpm				, NULL, NULL, "trash_open_hrm", NULL},
	{trash_xpm				, NULL, NULL, "trash_close", NULL},
	{trash_hrm_xpm				, NULL, NULL, "trash_close_hrm", NULL},
	{queue_close_mark_xpm			, NULL, NULL, "queue_close_mark", NULL},
	{queue_close_hrm_mark_xpm		, NULL, NULL, "queue_close_hrm_mark", NULL},
	{queue_open_mark_xpm			, NULL, NULL, "queue_open_mark", NULL},
	{queue_open_hrm_mark_xpm		, NULL, NULL, "queue_open_hrm_mark", NULL},
	{trash_mark_xpm				, NULL, NULL, "trash_open_mark", NULL},
	{trash_hrm_mark_xpm			, NULL, NULL, "trash_open_hrm_mark", NULL},
	{trash_mark_xpm				, NULL, NULL, "trash_close_mark", NULL},
	{trash_hrm_mark_xpm			, NULL, NULL, "trash_close_hrm_mark", NULL},
	{unread_xpm				, NULL, NULL, "unread", NULL},
	{vcard_xpm				, NULL, NULL, "vcard", NULL},
	{online_xpm				, NULL, NULL, "online", NULL},
	{offline_xpm				, NULL, NULL, "offline", NULL},
	{notice_warn_xpm			, NULL, NULL, "notice_warn",  NULL},
	{notice_error_xpm			, NULL, NULL, "notice_error",  NULL},
	{notice_note_xpm			, NULL, NULL, "notice_note",  NULL},
	{quicksearch_xpm			, NULL, NULL, "quicksearch",  NULL},
	{gpg_signed_xpm				, NULL, NULL, "gpg_signed", NULL},
	{go_folders_xpm				, NULL, NULL, "go_folders", NULL},
	{drafts_close_xpm			, NULL, NULL, "drafts_close", NULL},
	{drafts_open_xpm			, NULL, NULL, "drafts_open", NULL},
	{drafts_close_mark_xpm			, NULL, NULL, "drafts_close_mark", NULL},
	{drafts_open_mark_xpm			, NULL, NULL, "drafts_open_mark", NULL},
	{mime_text_plain_xpm			, NULL, NULL, "mime_text_plain", NULL},
	{mime_text_html_xpm			, NULL, NULL, "mime_text_html", NULL},
	{mime_text_patch_xpm			, NULL, NULL, "mime_text_patch", NULL},
	{mime_application_xpm			, NULL, NULL, "mime_application", NULL},
	{mime_image_xpm				, NULL, NULL, "mime_image", NULL},
	{mime_audio_xpm				, NULL, NULL, "mime_audio", NULL},
	{mime_text_enriched_xpm			, NULL, NULL, "mime_text_enriched", NULL},
	{mime_unknown_xpm			, NULL, NULL, "mime_unknown", NULL},	
	{mime_pdf_xpm				, NULL, NULL, "mime_pdf", NULL},	
	{mime_ps_xpm				, NULL, NULL, "mime_ps", NULL},	
	{mime_calendar_xpm			, NULL, NULL, "mime_calendar", NULL},	
	{mime_pgpsig_xpm			, NULL, NULL, "mime_pgpsig", NULL},	
	{printer_xpm				, NULL, NULL, "printer", NULL},
	{privacy_signed_xpm			, NULL, NULL, "privacy_signed", NULL},
	{privacy_passed_xpm			, NULL, NULL, "privacy_passed", NULL},
	{privacy_failed_xpm			, NULL, NULL, "privacy_failed", NULL},	
	{privacy_unknown_xpm			, NULL, NULL, "privacy_unknown", NULL},
	{privacy_expired_xpm			, NULL, NULL, "privacy_expired", NULL},
	{privacy_warn_xpm			, NULL, NULL, "privacy_warn", NULL},
	{privacy_emblem_encrypted_xpm		, NULL, NULL, "privacy_emblem_encrypted", NULL},
	{privacy_emblem_signed_xpm		, NULL, NULL, "privacy_emblem_signed", NULL},
	{privacy_emblem_passed_xpm		, NULL, NULL, "privacy_emblem_passed", NULL},
	{privacy_emblem_failed_xpm		, NULL, NULL, "privacy_emblem_failed", NULL},	
	{privacy_emblem_warn_xpm		, NULL, NULL, "privacy_emblem_warn", NULL},
	{mime_message_xpm			, NULL, NULL, "mime_message", NULL},
	{claws_mail_icon_xpm			, NULL, NULL, "claws_mail_icon", NULL},
	{read_xpm				, NULL, NULL, "read", NULL},
	{delete_btn_xpm				, NULL, NULL, "delete_btn", NULL},
	{cancel_xpm				, NULL, NULL, "cancel", NULL},
	{trash_btn_xpm				, NULL, NULL, "trash_btn", NULL},
#ifndef GENERIC_UMPC
	{claws_mail_logo_xpm			, NULL, NULL, "claws_mail_logo", NULL},
#else
	{claws_mail_logo_small_xpm		, NULL, NULL, "claws_mail_logo_small", NULL},
#endif
        {dir_noselect_xpm                       , NULL, NULL, "dir_noselect" , NULL},
        {spam_xpm                               , NULL, NULL, "spam" , NULL},
        {spam_btn_xpm                           , NULL, NULL, "spam_btn" , NULL},
        {ham_btn_xpm                            , NULL, NULL, "ham_btn" , NULL},
	{moved_xpm				, NULL, NULL, "moved", NULL},
	{copied_xpm				, NULL, NULL, "copied", NULL},
	{selection_xpm				, NULL, NULL, "selection", NULL},
	{watchthread_xpm			, NULL, NULL, "watchthread", NULL},
	{tray_newmail_offline_xpm		, NULL, NULL, "tray_newmail.offline", NULL},
	{tray_newmail_xpm			, NULL, NULL, "tray_newmail", NULL},
	{tray_newmarkedmail_offline_xpm		, NULL, NULL, "tray_newmarkedmail.offline", NULL},
	{tray_newmarkedmail_xpm			, NULL, NULL, "tray_newmarkedmail", NULL},
	{tray_nomail_offline_xpm		, NULL, NULL, "tray_nomail.offline", NULL},
	{tray_nomail_xpm			, NULL, NULL, "tray_nomail", NULL},
	{tray_unreadmail_offline_xpm		, NULL, NULL, "tray_unreadmail.offline", NULL},
	{tray_unreadmail_xpm			, NULL, NULL, "tray_unreadmail", NULL},
	{tray_unreadmarkedmail_offline_xpm	, NULL, NULL, "tray_unreadmarkedmail.offline", NULL},
	{tray_unreadmarkedmail_xpm		, NULL, NULL, "tray_unreadmarkedmail", NULL},
        {empty_xpm                              , NULL, NULL, "empty" , NULL}
};

/* return newly constructed GtkPixmap from GdkPixmap */
GtkWidget *stock_pixmap_widget(GtkWidget *window, StockPixmap icon)
{
	GdkPixbuf *pixbuf;

	cm_return_val_if_fail(window != NULL, NULL);
	cm_return_val_if_fail(icon >= 0 && icon < N_STOCK_PIXMAPS, NULL);

	if (stock_pixbuf_gdk(window, icon, &pixbuf) != -1)
		return gtk_image_new_from_pixbuf(pixbuf);
	
	return NULL;
}

/*!
 *\brief	
 */
gint stock_pixbuf_gdk(GtkWidget *window, StockPixmap icon, GdkPixbuf **pixbuf)
{
	StockPixmapData *pix_d;
	static const char *extension[]={".png", ".xpm", NULL};
	int i = 0;
	gboolean theme_changed = FALSE;

	if (pixbuf)
		*pixbuf = NULL;
		
	cm_return_val_if_fail(icon >= 0 && icon < N_STOCK_PIXMAPS, -1);

	pix_d = &pixmaps[icon];

	theme_changed = (strcmp2(pix_d->icon_path, prefs_common.pixmap_theme_path) != 0);
	if (!pix_d->pixbuf || theme_changed) {
		GdkPixbuf *pix = NULL;
		
		if (theme_changed && pix_d->pixmap) {
			g_object_unref(pix_d->pixmap);
			pix_d->pixmap = NULL;
		}

		if (strcmp(prefs_common.pixmap_theme_path, DEFAULT_PIXMAP_THEME) != 0) {
			if (is_dir_exist(prefs_common.pixmap_theme_path)) {
				char *icon_file_name; 
try_next_extension:				
				icon_file_name = g_strconcat(prefs_common.pixmap_theme_path,
							     G_DIR_SEPARATOR_S,
							     pix_d->file,
							     extension[i],
							     NULL);
				if (is_file_exist(icon_file_name)) {
					GError *err = NULL;
					pix = gdk_pixbuf_new_from_file(icon_file_name, &err);	
					if (err) g_error_free(err);
				}					
				if (pix) {
					g_free(pix_d->icon_path);
					pix_d->icon_path = g_strdup(prefs_common.pixmap_theme_path);
				}
				g_free(icon_file_name);
				if (!pix) {
					i++;
					if (extension[i])
						goto try_next_extension;
				}
			} else {
				/* even the path does not exist (deleted between two sessions), so
				set the preferences to the internal theme */
				prefs_common.pixmap_theme_path = g_strdup(DEFAULT_PIXMAP_THEME);
			}
		}
		pix_d->pixbuf = pix;
	}

	if (!pix_d->pixbuf) {
		pix_d->pixbuf = gdk_pixbuf_new_from_xpm_data((const gchar **) pix_d->data);
		if (pix_d->pixbuf) {
			g_free(pix_d->icon_path);
			pix_d->icon_path = g_strdup(DEFAULT_PIXMAP_THEME);	
		}
	}

	cm_return_val_if_fail(pix_d->pixbuf != NULL, -1);

	if (pixbuf)
		*pixbuf = pix_d->pixbuf;

	/* pixbuf should have one ref outstanding */		

	return 0;
}

static void stock_pixmap_find_themes_in_dir(GList **list, const gchar *dirname)
{
	struct dirent *d;
	DIR *dp;
	static const char *extension[]={".png", ".xpm", NULL};
	
	if ((dp = opendir(dirname)) == NULL) {
		debug_print("dir %s not found, skipping theme scan\n", dirname?dirname:"(null)");
		return;
	}
	
	while ((d = readdir(dp)) != NULL) {
		gchar *entry;
		gchar *fullentry;

		entry     = d->d_name;
		fullentry = g_strconcat(dirname, G_DIR_SEPARATOR_S, entry, NULL);
		
		if (strcmp(entry, ".") != 0 && strcmp(entry, "..") != 0 && is_dir_exist(fullentry)) {
			gchar *filetoexist;
			gboolean found = FALSE;
			int i;
			int j;
			for (i = 0; i < N_STOCK_PIXMAPS && !found; i++) {
				for (j = 0; extension[j] && !found; j++) {
					filetoexist = g_strconcat(fullentry, G_DIR_SEPARATOR_S, pixmaps[i].file, extension[j], NULL);
					if (is_file_exist(filetoexist)) {
						*list = g_list_append(*list, fullentry);
						found = TRUE;
					}
					g_free(filetoexist);
				}
			}
			if (i == N_STOCK_PIXMAPS) 
				g_free(fullentry);
		} else 
			g_free(fullentry);
	}
	closedir(dp);
}

gchar *stock_pixmap_get_system_theme_dir_for_theme(const gchar *theme)
{
	const gchar *sep = NULL;
	if (theme && *theme)
		sep = G_DIR_SEPARATOR_S;
#ifndef G_OS_WIN32
	return g_strconcat(PACKAGE_DATA_DIR, G_DIR_SEPARATOR_S,
	        	   PIXMAP_THEME_DIR, sep, theme, NULL);
#else
	return g_strconcat(get_themes_dir(), sep, theme, NULL);
#endif
}

GList *stock_pixmap_themes_list_new(void)
{
	gchar *defaulttheme;
	gchar *userthemes;
	gchar *systemthemes;
	GList *list = NULL;
	
	defaulttheme = g_strdup(DEFAULT_PIXMAP_THEME);
	userthemes   = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, 
				   PIXMAP_THEME_DIR, NULL);
	systemthemes = stock_pixmap_get_system_theme_dir_for_theme(NULL);

	list = g_list_append(list, defaulttheme);
	stock_pixmap_find_themes_in_dir(&list, userthemes);
	stock_pixmap_find_themes_in_dir(&list, systemthemes);

	g_free(userthemes);
	g_free(systemthemes);
	return list;
}

void stock_pixmap_themes_list_free(GList *list)
{
	GList *ptr;

	for (ptr = g_list_first(list); ptr != NULL; ptr = g_list_next(ptr)) 
		g_free(ptr->data);
	g_list_free(list);		
}

gchar *stock_pixmap_get_name (StockPixmap icon)
{
	if (icon < 0 || icon >= N_STOCK_PIXMAPS)
		return NULL;
	
	return pixmaps[icon].file;

}

StockPixmap stock_pixmap_get_icon (gchar *file)
{
	gint i;
	
	for (i = 0; i < N_STOCK_PIXMAPS; i++) {
		if (strcmp (pixmaps[i].file, file) == 0)
			return i;
	}
	return -1;
}

static gboolean do_pix_draw(GtkWidget *widget, cairo_t *cr,
			    OverlayData *data) 
{
	GdkWindow *drawable = gtk_widget_get_window(widget);	
	gint left = 0;
	gint top = 0;

	if (data->is_pixmap) {
		cm_return_val_if_fail(data->base_pixmap != NULL, FALSE);
	} else {
		cm_return_val_if_fail(data->base_pixbuf != NULL, FALSE);
	}

	if (data->highlight) {
		MainWindow *mw = NULL;

		mw = mainwindow_get_mainwindow();
		if (mw != NULL && mw->menubar != NULL) {
			cairo_t *cr;
			GdkColor color = gtk_widget_get_style(mw->menubar)->base[GTK_STATE_SELECTED];

			cr = gdk_cairo_create(drawable);
			gdk_cairo_set_source_color(cr, &color);
			cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
			cairo_set_line_width(cr, 1.);
			cairo_set_line_cap(cr, CAIRO_LINE_CAP_BUTT);
			cairo_set_line_join(cr, CAIRO_LINE_JOIN_BEVEL);
			cairo_rectangle(cr, data->border_x-2, data->border_y-2,
			    data->base_width+3, data->base_height+3);
			cairo_stroke(cr);
			cairo_destroy(cr);
		}
	}

	if (data->is_pixmap) {
		cairo_set_source_surface(cr, data->base_pixmap, data->border_x, data->border_y);
		cairo_pattern_set_extend(cairo_get_source(cr), CAIRO_EXTEND_REPEAT);
		cairo_rectangle(cr, data->border_x, data->border_y, data->base_width, data->base_height);
		cairo_fill(cr);
	} else {
		gdk_cairo_set_source_pixbuf(cr, data->base_pixbuf, data->border_x, data->border_y);
		cairo_paint(cr);
	}

	if (data->position != OVERLAY_NONE) {

		switch (data->position) {
			case OVERLAY_TOP_LEFT:
			case OVERLAY_MID_LEFT:
			case OVERLAY_BOTTOM_LEFT:
				left = 0;
				break;

			case OVERLAY_TOP_CENTER:
			case OVERLAY_MID_CENTER:
			case OVERLAY_BOTTOM_CENTER:
				left = (data->base_width + data->border_x * 2 - data->overlay_width)/2;
				break;

			case OVERLAY_TOP_RIGHT:
			case OVERLAY_MID_RIGHT:
			case OVERLAY_BOTTOM_RIGHT:
				left = data->base_width + data->border_x * 2 - data->overlay_width;
				break;

			default:
				break;
		}
		switch (data->position) {
			case OVERLAY_TOP_LEFT:
			case OVERLAY_TOP_CENTER:
			case OVERLAY_TOP_RIGHT:
				top = 0;
				break;

			case OVERLAY_MID_LEFT:
			case OVERLAY_MID_CENTER:
			case OVERLAY_MID_RIGHT:
				top = (data->base_height + data->border_y * 2 - data->overlay_height)/2;
				break;
					
			case OVERLAY_BOTTOM_LEFT:
			case OVERLAY_BOTTOM_CENTER:
			case OVERLAY_BOTTOM_RIGHT:
				top = data->base_height + data->border_y * 2 - data->overlay_height;
				break;

			default:
				break;
		}
	}

	if (data->position != OVERLAY_NONE) {
		if (data->is_pixmap) {
			cm_return_val_if_fail(data->overlay_pixmap != NULL, FALSE);
			cairo_set_source_surface(cr, data->overlay_pixmap, left, top);
			cairo_pattern_set_extend (cairo_get_source (cr), CAIRO_EXTEND_REPEAT);
			cairo_rectangle (cr, left, top, data->overlay_width, data->overlay_height);
			cairo_fill(cr);
		} else {
      cm_return_val_if_fail(data->overlay_pixbuf != NULL, FALSE);
      gdk_cairo_set_source_pixbuf(cr, data->overlay_pixbuf, left, top);
      cairo_paint(cr);
		}
	}

	return TRUE;
}

#if !GTK_CHECK_VERSION(3,0,0)
static gboolean pixmap_with_overlay_expose_event_cb(GtkWidget *widget, GdkEventExpose *expose,
                                                   OverlayData *data) 
#else
static gboolean pixmap_with_overlay_expose_event_cb(GtkWidget *widget, cairo_t *cr,
						    OverlayData *data) 
#endif
{
#if !GTK_CHECK_VERSION(3,0,0)
	cairo_t *cr;
	GdkWindow *drawable = gtk_widget_get_window(widget);	
	gboolean result;

	cr = gdk_cairo_create(drawable);
	gdk_window_clear_area (drawable, expose->area.x, expose->area.y,
                               expose->area.width, expose->area.height);

	result = do_pix_draw(widget, cr, data);
	cairo_destroy(cr);
	return result;
#else
	return do_pix_draw(widget, cr, data);
#endif
}

static void pixmap_with_overlay_destroy_cb(GtkWidget *object, OverlayData *data) 
{
	if (data->is_pixmap) {
		cairo_surface_destroy(data->base_pixmap);
		if (data->position != OVERLAY_NONE) {
			cairo_surface_destroy(data->overlay_pixmap);
		}
	} else {
		g_object_unref(data->base_pixbuf);
		if (data->position != OVERLAY_NONE) {
			g_object_unref(data->overlay_pixbuf);
		}
	}
	g_free(data);
}

/**
 * \brief Get a widget showing one icon with another overlaid on top of it.
 *
 * The base icon is always centralised, the other icon can be positioned.
 * The overlay icon is ignored if pos=OVERLAY_NONE is used
 *
 * \param window   top-level window widget
 * \param icon	   the base icon
 * \param overlay  the icon to overlay
 * \param pos      how to align the overlay widget, or OVERLAY_NONE for no overlay
 * \param border_x size of the border around the base icon (left and right)
 * \param border_y size of the border around the base icon (top and bottom)
 */
GtkWidget *stock_pixmap_widget_with_overlay(GtkWidget *window, StockPixmap icon,
					    StockPixmap overlay, OverlayPosition pos,
					    gint border_x, gint border_y)
{
	cairo_surface_t *stock_pixmap = NULL;
	GdkPixbuf *stock_pixbuf = NULL;
	GtkWidget *widget = NULL;
	GtkWidget *stock_wid = NULL;
	GtkRequisition requisition;
	OverlayData *data = NULL;
	
	data = g_new0(OverlayData, 1);

	stock_wid = stock_pixmap_widget(window, icon);
	gtk_widget_get_requisition(stock_wid, &requisition);

	if (gtk_image_get_storage_type(GTK_IMAGE(stock_wid)) == GTK_IMAGE_PIXMAP)
		data->is_pixmap = TRUE;
	else
		data->is_pixmap = FALSE;

	if (data->is_pixmap) {
		cairo_t *cr = gdk_cairo_create(gtk_widget_get_window(stock_wid));
		stock_pixmap = cairo_get_target(cr);
		cairo_surface_reference(stock_pixmap);
		cairo_destroy(cr);
		data->base_pixmap = stock_pixmap;
		data->base_height = requisition.height;
		data->base_width  = requisition.width;
		gtk_widget_destroy(stock_wid);

		if (pos == OVERLAY_NONE) {
			data->overlay_pixmap = NULL;
		} else {
			stock_wid = stock_pixmap_widget(window, overlay);
			cr = gdk_cairo_create(gtk_widget_get_window(stock_wid));
			stock_pixmap = cairo_get_target(cr);
			cairo_surface_reference(stock_pixmap);
			cairo_destroy(cr);
			data->overlay_pixmap = stock_pixmap;
			data->overlay_height = requisition.height;
			data->overlay_width  = requisition.width;

			gtk_widget_destroy(stock_wid);
		}
	} else {
		data->is_pixmap = FALSE;
		stock_pixbuf = gtk_image_get_pixbuf(GTK_IMAGE(stock_wid));
		g_object_ref(stock_pixbuf);
		data->base_pixbuf = stock_pixbuf;
		data->base_height = requisition.height;
		data->base_width  = requisition.width;
		gtk_widget_destroy(stock_wid);
		if (pos == OVERLAY_NONE) {
			data->overlay_pixmap = NULL;
		} else {
			stock_wid = stock_pixmap_widget(window, overlay);
			stock_pixbuf = gtk_image_get_pixbuf(GTK_IMAGE(stock_wid));
			g_object_ref(stock_pixbuf);
			data->overlay_pixbuf = stock_pixbuf;
			data->overlay_height = requisition.height;
			data->overlay_width  = requisition.width;

			gtk_widget_destroy(stock_wid);
		}
	}
	data->position = pos;
	data->border_x = border_x;
	data->border_y = border_y;
	data->highlight = FALSE;

	widget = gtk_drawing_area_new();
	gtk_widget_set_size_request(widget, data->base_width + border_x * 2, 
			      data->base_height + border_y * 2);
#if !GTK_CHECK_VERSION(3, 0, 0)
	g_signal_connect(G_OBJECT(widget), "expose_event", 
			 G_CALLBACK(pixmap_with_overlay_expose_event_cb), data);
#else
	g_signal_connect(G_OBJECT(widget), "draw", 
			 G_CALLBACK(pixmap_with_overlay_expose_event_cb), data);
#endif
	g_signal_connect(G_OBJECT(widget), "destroy",
			 G_CALLBACK(pixmap_with_overlay_destroy_cb), data);
	g_object_set_data(G_OBJECT(widget), "highlight", &(data->highlight));
	return widget;

}
