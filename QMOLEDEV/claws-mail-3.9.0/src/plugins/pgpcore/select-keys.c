/* select-keys.c - GTK+ based key selection
 *      Copyright (C) 2001-2012 Werner Koch (dd9jn) and the Claws Mail team
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#ifdef USE_GPGME
#include <stdio.h>
#include <stdlib.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include "select-keys.h"
#include "utils.h"
#include "gtkutils.h"
#include "inputdialog.h"
#include "manage_window.h"
#include "alertpanel.h"

#define DIM(v) (sizeof(v)/sizeof((v)[0]))
#define DIMof(type,member)   DIM(((type *)0)->member)


enum col_titles { 
    COL_ALGO,
    COL_KEYID,
    COL_NAME,
    COL_EMAIL,
    COL_VALIDITY,

    N_COL_TITLES
};

struct select_keys_s {
    int okay;
    GtkWidget *window;
    GtkLabel *toplabel;
    GtkCMCList *clist;
    const char *pattern;
    unsigned int num_keys;
    gpgme_key_t *kset;
    gpgme_ctx_t select_ctx;
    gpgme_protocol_t proto;
    GtkSortType sort_type;
    enum col_titles sort_column;
    SelectionResult result;
};


static void set_row (GtkCMCList *clist, gpgme_key_t key, gpgme_protocol_t proto);
static gpgme_key_t fill_clist (struct select_keys_s *sk, const char *pattern,
			gpgme_protocol_t proto);
static void create_dialog (struct select_keys_s *sk);
static void open_dialog (struct select_keys_s *sk);
static void close_dialog (struct select_keys_s *sk);
static gint delete_event_cb (GtkWidget *widget,
                             GdkEventAny *event, gpointer data);
static gboolean key_pressed_cb (GtkWidget *widget,
                                GdkEventKey *event, gpointer data);
static void select_btn_cb (GtkWidget *widget, gpointer data);
static void cancel_btn_cb (GtkWidget *widget, gpointer data);
static void dont_encrypt_btn_cb (GtkWidget *widget, gpointer data);
static void other_btn_cb (GtkWidget *widget, gpointer data);
static void sort_keys (struct select_keys_s *sk, enum col_titles column);
static void sort_keys_name (GtkWidget *widget, gpointer data);
static void sort_keys_email (GtkWidget *widget, gpointer data);

static gboolean use_untrusted (gpgme_key_t, gpgme_user_id_t uid, gpgme_protocol_t proto);

static void
update_progress (struct select_keys_s *sk, int running, const char *pattern)
{
    static int windmill[] = { '-', '\\', '|', '/' };
    char *buf;

    if (!running)
        buf = g_strdup_printf (_("No exact match for '%s'; please select the key."),
                               pattern);
    else 
        buf = g_strdup_printf (_("Collecting info for '%s' ... %c"),
                               pattern,
                               windmill[running%DIM(windmill)]);
    gtk_label_set_text (sk->toplabel, buf);
    g_free (buf);
}


/**
 * gpgmegtk_recipient_selection:
 * @recp_names: A list of email addresses
 * 
 * Select a list of recipients from a given list of email addresses.
 * This may pop up a window to present the user a choice, it will also
 * check that the recipients key are all valid.
 * 
 * Return value: NULL on error or a list of list of recipients.
 **/
gpgme_key_t *
gpgmegtk_recipient_selection (GSList *recp_names, SelectionResult *result,
				gpgme_protocol_t proto)
{
    struct select_keys_s sk;
    gpgme_key_t key = NULL;
    memset (&sk, 0, sizeof sk);

    open_dialog (&sk);

    do {
        sk.pattern = recp_names? recp_names->data:NULL;
	sk.proto = proto;
        gtk_cmclist_clear (sk.clist);
        key = fill_clist (&sk, sk.pattern, proto);
        update_progress (&sk, 0, sk.pattern);
	if (!key) {
    		gtk_widget_show_all (sk.window);
	        gtk_main ();
	} else {
     		gtk_widget_hide (sk.window);
	       	sk.kset = g_realloc(sk.kset,
                	sizeof(gpgme_key_t) * (sk.num_keys + 1));
        	gpgme_key_ref(key);
        	sk.kset[sk.num_keys] = key;
        	sk.num_keys++;
        	sk.okay = 1;
		sk.result = KEY_SELECTION_OK;
		gpgme_release (sk.select_ctx);
		sk.select_ctx = NULL;
		debug_print("used %s\n", key->uids->email);
	}
	key = NULL;
        if (recp_names)
            recp_names = recp_names->next;
    } while (sk.okay && recp_names);

    close_dialog (&sk);

    if (!sk.okay) {
        g_free(sk.kset);
        sk.kset = NULL;
    } else {
        sk.kset = g_realloc(sk.kset, sizeof(gpgme_key_t) * (sk.num_keys + 1));
        sk.kset[sk.num_keys] = NULL;
    }
    if (result)
	    *result = sk.result;
    return sk.kset;
} 

static void
destroy_key (gpointer data)
{
    gpgme_key_t key = data;
    gpgme_key_release (key);
}

static void
set_row (GtkCMCList *clist, gpgme_key_t key, gpgme_protocol_t proto)
{
    const char *s;
    const char *text[N_COL_TITLES];
    char *algo_buf;
    int row;
    gsize by_read = 0, by_written = 0;
    gchar *ret_str = NULL;

    /* first check whether the key is capable of encryption which is not
     * the case for revoked, expired or sign-only keys */
    if (!key->can_encrypt || key->revoked || key->expired || key->disabled)
        return;

    algo_buf = g_strdup_printf ("%du/%s", 
         key->subkeys->length,
         gpgme_pubkey_algo_name(key->subkeys->pubkey_algo) );
    text[COL_ALGO] = algo_buf;

    s = key->subkeys->keyid;
    if (strlen (s) == 16)
        s += 8; /* show only the short keyID */
    text[COL_KEYID] = s;


    s = key->uids->name;
    if (!s || !*s)
        s = key->uids->uid;
    if (proto == GPGME_PROTOCOL_CMS) {
	if (strstr(s, ",CN="))
		s = strstr(s, ",CN=")+4;
	else if (strstr(s, "CN="))
		s = strstr(s, "CN=")+3;
    } 
    
    ret_str = NULL;
    if (!g_utf8_validate(s, -1, NULL))
	    ret_str = g_locale_to_utf8 (s, strlen(s), &by_read, &by_written, NULL);
    if (ret_str && by_written) {
        s = ret_str;
    }
    text[COL_NAME] = s;

    if (proto == GPGME_PROTOCOL_CMS && (!key->uids->email || !*key->uids->email)) {
	gpgme_user_id_t uid = key->uids->next;
	if (uid)
		s = uid->email;
	else
		s = key->uids->email;
    } else {
        s = key->uids->email;
    }
    
    ret_str = NULL;
    if (!g_utf8_validate(s, -1, NULL))
	    ret_str = g_locale_to_utf8 (s, strlen(s), &by_read, &by_written, NULL);
    if (ret_str && by_written) {
        s = ret_str;
    }
    text[COL_EMAIL] = s;

    switch (key->uids->validity)
      {
      case GPGME_VALIDITY_UNDEFINED:
        s = _("Undefined");
        break;
      case GPGME_VALIDITY_NEVER:
        s = _("Never");
        break;
      case GPGME_VALIDITY_MARGINAL:
        s = _("Marginal");
        break;
      case GPGME_VALIDITY_FULL:
        s = _("Full");
        break;
      case GPGME_VALIDITY_ULTIMATE:
        s = _("Ultimate");
        break;
      case GPGME_VALIDITY_UNKNOWN:
      default:
        s = _("Unknown");
        break;
      }
    text[COL_VALIDITY] = s;

    row = gtk_cmclist_append (clist, (gchar**)text);
    g_free (algo_buf);

    gtk_cmclist_set_row_data_full (clist, row, key, destroy_key);
}

static gpgme_key_t 
fill_clist (struct select_keys_s *sk, const char *pattern, gpgme_protocol_t proto)
{
    GtkCMCList *clist;
    gpgme_ctx_t ctx;
    gpgme_error_t err;
    gpgme_key_t key;
    int running=0;
    int num_results = 0;
    gboolean exact_match = FALSE;
    gpgme_key_t last_key = NULL;
    gpgme_user_id_t last_uid = NULL;
    cm_return_val_if_fail (sk, NULL);
    clist = sk->clist;
    cm_return_val_if_fail (clist, NULL);

    debug_print ("select_keys:fill_clist:  pattern '%s' proto %d\n", pattern, proto);

    /*gtk_cmclist_freeze (select_keys.clist);*/
    err = gpgme_new (&ctx);
    g_assert (!err);

    gpgme_set_protocol(ctx, proto);
    sk->select_ctx = ctx;

    update_progress (sk, ++running, pattern);
    while (gtk_events_pending ())
        gtk_main_iteration ();

    err = gpgme_op_keylist_start (ctx, pattern, 0);
    if (err) {
        debug_print ("** gpgme_op_keylist_start(%s) failed: %s",
                     pattern, gpgme_strerror (err));
        sk->select_ctx = NULL;
        gpgme_release(ctx);
        return NULL;
    }
    update_progress (sk, ++running, pattern);
    while ( !(err = gpgme_op_keylist_next ( ctx, &key )) ) {
	gpgme_user_id_t uid = key->uids;
	if (!key->can_encrypt || key->revoked || key->expired || key->disabled)
		continue;
        debug_print ("%% %s:%d:  insert\n", __FILE__ ,__LINE__ );
        set_row (clist, key, proto ); 
	for (; uid; uid = uid->next) {
		gchar *raw_mail = NULL;

		if (!uid->email)
			continue;
		raw_mail = g_strdup(uid->email);
		extract_address(raw_mail);
		if (!strcasecmp(pattern, raw_mail)) {
			exact_match = TRUE;
			last_uid = uid;
			g_free(raw_mail);
			break;
		}
		g_free(raw_mail);
	}
	num_results++;
	last_key = key;
	key = NULL;
        update_progress (sk, ++running, pattern);
        while (gtk_events_pending ())
            gtk_main_iteration ();
    }
 
    if (exact_match == TRUE && num_results == 1) {
	    if (last_key->uids->validity < GPGME_VALIDITY_FULL && 
		!use_untrusted(last_key, last_uid, proto))
		    exact_match = FALSE;
    }

    debug_print ("%% %s:%d:  ready\n", __FILE__ ,__LINE__ );
    if (gpgme_err_code(err) != GPG_ERR_EOF) {
        debug_print ("** gpgme_op_keylist_next failed: %s",
                     gpgme_strerror (err));
        gpgme_op_keylist_end(ctx);
    }
    if (!exact_match || num_results != 1) {
	    sk->select_ctx = NULL;
	    gpgme_release (ctx);
    }
    /*gtk_cmclist_thaw (select_keys.clist);*/
    return (exact_match == TRUE && num_results == 1 ? last_key:NULL);
}


static void 
create_dialog (struct select_keys_s *sk)
{
    GtkWidget *window;
    GtkWidget *vbox, *vbox2, *hbox;
    GtkWidget *bbox;
    GtkWidget *scrolledwin;
    GtkWidget *clist;
    GtkWidget *label;
    GtkWidget *select_btn, *cancel_btn, *dont_encrypt_btn, *other_btn;
    const char *titles[N_COL_TITLES];

    g_assert (!sk->window);
    window = gtkut_window_new (GTK_WINDOW_TOPLEVEL, "select-keys");
    gtk_widget_set_size_request (window, 520, 280);
    gtk_container_set_border_width (GTK_CONTAINER (window), 8);
    gtk_window_set_title (GTK_WINDOW (window), _("Select Keys"));
    gtk_window_set_modal (GTK_WINDOW (window), TRUE);
    g_signal_connect (G_OBJECT (window), "delete_event",
                      G_CALLBACK (delete_event_cb), sk);
    g_signal_connect (G_OBJECT (window), "key_press_event",
                      G_CALLBACK (key_pressed_cb), sk);
    MANAGE_WINDOW_SIGNALS_CONNECT (window);

    vbox = gtk_vbox_new (FALSE, 8);
    gtk_container_add (GTK_CONTAINER (window), vbox);

    hbox  = gtk_hbox_new(FALSE, 4);
    gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
    label = gtk_label_new ( "" );
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

    hbox = gtk_hbox_new (FALSE, 8);
    gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);
    gtk_container_set_border_width (GTK_CONTAINER (hbox), 2);

    scrolledwin = gtk_scrolled_window_new (NULL, NULL);
    gtk_box_pack_start (GTK_BOX (hbox), scrolledwin, TRUE, TRUE, 0);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwin),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);

    titles[COL_ALGO]     = _("Size");
    titles[COL_KEYID]    = _("Key ID");
    titles[COL_NAME]     = _("Name");
    titles[COL_EMAIL]    = _("Address");
    titles[COL_VALIDITY] = _("Trust");

    clist = gtk_cmclist_new_with_titles (N_COL_TITLES, (char**)titles);
    gtk_container_add (GTK_CONTAINER (scrolledwin), clist);
    gtk_cmclist_set_column_width (GTK_CMCLIST(clist), COL_ALGO,      72);
    gtk_cmclist_set_column_width (GTK_CMCLIST(clist), COL_KEYID,     76);
    gtk_cmclist_set_column_width (GTK_CMCLIST(clist), COL_NAME,     130);
    gtk_cmclist_set_column_width (GTK_CMCLIST(clist), COL_EMAIL,    130);
    gtk_cmclist_set_column_width (GTK_CMCLIST(clist), COL_VALIDITY,  20);
    gtk_cmclist_set_selection_mode (GTK_CMCLIST(clist), GTK_SELECTION_BROWSE);
    g_signal_connect (G_OBJECT(GTK_CMCLIST(clist)->column[COL_NAME].button),
		      "clicked",
                      G_CALLBACK(sort_keys_name), sk);
    g_signal_connect (G_OBJECT(GTK_CMCLIST(clist)->column[COL_EMAIL].button),
		      "clicked",
                      G_CALLBACK(sort_keys_email), sk);

    hbox = gtk_hbox_new (FALSE, 8);
    gtk_box_pack_end (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);

    /* TRANSLATORS: check that the accelerators in _Select, _Other and
     * Do_n't encrypt are different than the one in the stock Cancel
     * button */
    gtkut_stock_button_set_create (&bbox, 
                                   &select_btn, _("_Select"),
		   		   &other_btn, _("_Other"),
		    		   &dont_encrypt_btn, _("Do_n't encrypt"));
    
    cancel_btn = gtk_button_new_from_stock(GTK_STOCK_CANCEL);
    gtkut_widget_set_can_default(cancel_btn, TRUE);
    gtk_box_pack_start(GTK_BOX(bbox), cancel_btn, TRUE, TRUE, 0);
    gtk_widget_show(cancel_btn);
    gtk_box_pack_end (GTK_BOX (hbox), bbox, FALSE, FALSE, 0);
    gtk_widget_grab_default (select_btn);

    g_signal_connect (G_OBJECT (select_btn), "clicked",
                      G_CALLBACK (select_btn_cb), sk);
    g_signal_connect (G_OBJECT(cancel_btn), "clicked",
                      G_CALLBACK (cancel_btn_cb), sk);
    g_signal_connect (G_OBJECT(dont_encrypt_btn), "clicked",
                      G_CALLBACK (dont_encrypt_btn_cb), sk);
    g_signal_connect (G_OBJECT (other_btn), "clicked",
                      G_CALLBACK (other_btn_cb), sk);

    vbox2 = gtk_vbox_new (FALSE, 4);
    gtk_box_pack_start (GTK_BOX (hbox), vbox2, FALSE, FALSE, 0);

    sk->window = window;
    sk->toplabel = GTK_LABEL (label);
    sk->clist  = GTK_CMCLIST (clist);
}


static void
open_dialog (struct select_keys_s *sk)
{
    if (!sk->window)
        create_dialog (sk);
    manage_window_set_transient (GTK_WINDOW (sk->window));
    sk->okay = 0;
    sk->sort_column = N_COL_TITLES; /* use an invalid value */
    sk->sort_type = GTK_SORT_ASCENDING;
}


static void
close_dialog (struct select_keys_s *sk)
{
    cm_return_if_fail (sk);
    gtk_widget_destroy (sk->window);
    sk->window = NULL;
}


static gint
delete_event_cb (GtkWidget *widget, GdkEventAny *event, gpointer data)
{
    struct select_keys_s *sk = data;

    sk->okay = 0;
    gtk_main_quit ();

    return TRUE;
}


static gboolean
key_pressed_cb (GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    struct select_keys_s *sk = data;

    cm_return_val_if_fail (sk, FALSE);
    if (event && event->keyval == GDK_KEY_Escape) {
        sk->okay = 0;
        gtk_main_quit ();
    }
    return FALSE;
}


static void 
select_btn_cb (GtkWidget *widget, gpointer data)
{
    struct select_keys_s *sk = data;
    int row;
    gboolean use_key;
    gpgme_key_t key;

    cm_return_if_fail (sk);
    if (!sk->clist->selection) {
        debug_print ("** nothing selected");
        return;
    }
    row = GPOINTER_TO_INT(sk->clist->selection->data);
    key = gtk_cmclist_get_row_data(sk->clist, row);
    if (key) {
        gpgme_user_id_t uid;
	for (uid = key->uids; uid; uid = uid->next) {
		gchar *raw_mail = NULL;

		if (!uid->email)
			continue;
		raw_mail = g_strdup(uid->email);
		extract_address(raw_mail);
		if (sk->pattern && !strcasecmp(sk->pattern, raw_mail)) {
			g_free(raw_mail);
			break;
		}
		g_free(raw_mail);
	}
	if (!uid)
		uid = key->uids;

        if ( uid->validity < GPGME_VALIDITY_FULL ) {
            use_key = use_untrusted(key, uid, sk->proto);
            if (!use_key) {
                debug_print ("** Key untrusted, will not encrypt");
                return;
            }
        }
        sk->kset = g_realloc(sk->kset,
                sizeof(gpgme_key_t) * (sk->num_keys + 1));
        gpgme_key_ref(key);
        sk->kset[sk->num_keys] = key;
        sk->num_keys++;
        sk->okay = 1;
	sk->result = KEY_SELECTION_OK;
        gtk_main_quit ();
    }
}


static void 
cancel_btn_cb (GtkWidget *widget, gpointer data)
{
    struct select_keys_s *sk = data;

    cm_return_if_fail (sk);
    sk->okay = 0;
    sk->result = KEY_SELECTION_CANCEL;
    if (sk->select_ctx)
        gpgme_cancel (sk->select_ctx);
    gtk_main_quit ();
}

static void 
dont_encrypt_btn_cb (GtkWidget *widget, gpointer data)
{
    struct select_keys_s *sk = data;

    cm_return_if_fail (sk);
    sk->okay = 0;
    sk->result = KEY_SELECTION_DONT;
    if (sk->select_ctx)
        gpgme_cancel (sk->select_ctx);
    gtk_main_quit ();
}

static void
other_btn_cb (GtkWidget *widget, gpointer data)
{
    struct select_keys_s *sk = data;
    char *uid;

    cm_return_if_fail (sk);
    uid = input_dialog ( _("Add key"),
                         _("Enter another user or key ID:"),
                         NULL );
    if (!uid)
        return;
    if (fill_clist (sk, uid, sk->proto) != NULL) {
	    gpgme_release(sk->select_ctx);
	    sk->select_ctx = NULL;
    }
    update_progress (sk, 0, sk->pattern);
    g_free (uid);
}


static gboolean
use_untrusted (gpgme_key_t key, gpgme_user_id_t uid, gpgme_protocol_t proto)
{
    AlertValue aval;
    gchar *buf = NULL;
    gchar *title = NULL;
    if (proto != GPGME_PROTOCOL_OpenPGP)
    	return TRUE;

    title = g_strdup_printf(_("Encrypt to %s <%s>"), uid->name, uid->email);
    buf = g_strdup_printf(_("This encryption key is not fully trusted.\n"
	       "If you choose to encrypt the message with this key, you don't\n"
	       "know for sure that it will go to the person you mean it to.\n\n"
	       "Key details: ID %s, primary identity %s &lt;%s&gt;\n\n"
	       "Do you trust this key enough to use it anyway?"), 
	       key->subkeys->keyid, key->uids->name, key->uids->email);
    aval = alertpanel
	    (title, buf,
	     GTK_STOCK_NO, GTK_STOCK_YES, NULL);
    g_free(buf);
    g_free(title);
    if (aval == G_ALERTALTERNATE)
	return TRUE;
    else
	return FALSE;
}


static gint 
cmp_name (GtkCMCList *clist, gconstpointer pa, gconstpointer pb)
{
    gpgme_key_t a = ((GtkCMCListRow *)pa)->data;
    gpgme_key_t b = ((GtkCMCListRow *)pb)->data;
    const char *sa, *sb;
    
    sa = a? a->uids->name : NULL;
    sb = b? b->uids->name : NULL;
    if (!sa)
        return !!sb;
    if (!sb)
        return -1;
    return g_ascii_strcasecmp(sa, sb);
}

static gint 
cmp_email (GtkCMCList *clist, gconstpointer pa, gconstpointer pb)
{
    gpgme_key_t a = ((GtkCMCListRow *)pa)->data;
    gpgme_key_t b = ((GtkCMCListRow *)pb)->data;
    const char *sa, *sb;
    
    sa = a? a->uids->email : NULL;
    sb = b? b->uids->email : NULL;
    if (!sa)
        return !!sb;
    if (!sb)
        return -1;
    return g_ascii_strcasecmp(sa, sb);
}

static void
sort_keys ( struct select_keys_s *sk, enum col_titles column)
{
    GtkCMCList *clist = sk->clist;

    switch (column) {
      case COL_NAME:
        gtk_cmclist_set_compare_func (clist, cmp_name);
        break;
      case COL_EMAIL:
        gtk_cmclist_set_compare_func (clist, cmp_email);
        break;
      default:
        return;
    }

    /* column clicked again: toggle as-/decending */
    if ( sk->sort_column == column) {
        sk->sort_type = sk->sort_type == GTK_SORT_ASCENDING ?
                        GTK_SORT_DESCENDING : GTK_SORT_ASCENDING;
    }
    else
        sk->sort_type = GTK_SORT_ASCENDING;

    sk->sort_column = column;
    gtk_cmclist_set_sort_type (clist, sk->sort_type);
    gtk_cmclist_sort (clist);
}

static void
sort_keys_name (GtkWidget *widget, gpointer data)
{
    sort_keys ((struct select_keys_s*)data, COL_NAME);
}

static void
sort_keys_email (GtkWidget *widget, gpointer data)
{
    sort_keys ((struct select_keys_s*)data, COL_EMAIL);
}

#endif /*USE_GPGME*/
