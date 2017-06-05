/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Colin Leroy <colin@colino.net> 
 * and the Claws Mail team
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
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_GNUTLS

#include <gnutls/gnutls.h>
#include <gnutls/x509.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>

#include "prefs_common.h"
#include "defs.h"
#include "ssl_certificate.h"
#include "utils.h"
#include "alertpanel.h"
#include "hooks.h"

static gboolean sslcertwindow_ask_new_cert(SSLCertificate *cert);
static gboolean sslcertwindow_ask_expired_cert(SSLCertificate *cert);
static gboolean sslcertwindow_ask_changed_cert(SSLCertificate *old_cert, SSLCertificate *new_cert);

static GtkWidget *cert_presenter(SSLCertificate *cert)
{
	GtkWidget *vbox = NULL;
	GtkWidget *hbox = NULL;
	GtkWidget *frame_owner = NULL;
	GtkWidget *frame_signer = NULL;
	GtkWidget *frame_status = NULL;
	GtkTable *owner_table = NULL;
	GtkTable *signer_table = NULL;
	GtkTable *status_table = NULL;
	GtkWidget *label = NULL;
	
	char *issuer_commonname, *issuer_location, *issuer_organization;
	char *subject_commonname, *subject_location, *subject_organization;
	char *sig_status, *exp_date;
	char *md5_fingerprint, *sha1_fingerprint, *fingerprint;
	size_t n;
	char buf[100];
	unsigned char md[128];	
	char *tmp;
	time_t exp_time_t;
	struct tm lt;

	/* issuer */	
	issuer_commonname = g_malloc(BUFFSIZE);
	issuer_location = g_malloc(BUFFSIZE);
	issuer_organization = g_malloc(BUFFSIZE);
	subject_commonname = g_malloc(BUFFSIZE);
	subject_location = g_malloc(BUFFSIZE);
	subject_organization = g_malloc(BUFFSIZE);

	n = BUFFSIZE;
	if (gnutls_x509_crt_get_issuer_dn_by_oid(cert->x509_cert, 
		GNUTLS_OID_X520_COMMON_NAME, 0, 0, issuer_commonname, &n))
		strncpy(issuer_commonname, _("<not in certificate>"), BUFFSIZE);
	n = BUFFSIZE;

	if (gnutls_x509_crt_get_issuer_dn_by_oid(cert->x509_cert, 
		GNUTLS_OID_X520_LOCALITY_NAME, 0, 0, issuer_location, &n)) {
		if (gnutls_x509_crt_get_issuer_dn_by_oid(cert->x509_cert, 
			GNUTLS_OID_X520_COUNTRY_NAME, 0, 0, issuer_location, &n)) {
			strncpy(issuer_location, _("<not in certificate>"), BUFFSIZE);
		}
	} else {
		tmp = g_malloc(BUFFSIZE);
		if (gnutls_x509_crt_get_issuer_dn_by_oid(cert->x509_cert, 
			GNUTLS_OID_X520_COUNTRY_NAME, 0, 0, tmp, &n) == 0) {
			strncat(issuer_location, ", ", BUFFSIZE-strlen(issuer_location)-1);
			strncat(issuer_location, tmp, BUFFSIZE-strlen(issuer_location)-1);
		}
		g_free(tmp);
	}

	n = BUFFSIZE;
	if (gnutls_x509_crt_get_issuer_dn_by_oid(cert->x509_cert, 
		GNUTLS_OID_X520_ORGANIZATION_NAME, 0, 0, issuer_organization, &n))
		strncpy(issuer_organization, _("<not in certificate>"), BUFFSIZE);

	n = BUFFSIZE;
	if (gnutls_x509_crt_get_dn_by_oid(cert->x509_cert, 
		GNUTLS_OID_X520_COMMON_NAME, 0, 0, subject_commonname, &n))
		strncpy(subject_commonname, _("<not in certificate>"), BUFFSIZE);
	n = BUFFSIZE;

	if (gnutls_x509_crt_get_dn_by_oid(cert->x509_cert, 
		GNUTLS_OID_X520_LOCALITY_NAME, 0, 0, subject_location, &n)) {
		if (gnutls_x509_crt_get_dn_by_oid(cert->x509_cert, 
			GNUTLS_OID_X520_COUNTRY_NAME, 0, 0, subject_location, &n)) {
			strncpy(subject_location, _("<not in certificate>"), BUFFSIZE);
		}
	} else {
		tmp = g_malloc(BUFFSIZE);
		if (gnutls_x509_crt_get_dn_by_oid(cert->x509_cert, 
			GNUTLS_OID_X520_COUNTRY_NAME, 0, 0, tmp, &n) == 0) {
			strncat(subject_location, ", ", BUFFSIZE-strlen(subject_location)-1);
			strncat(subject_location, tmp, BUFFSIZE-strlen(subject_location)-1);
		}
		g_free(tmp);
	}

	n = BUFFSIZE;
	if (gnutls_x509_crt_get_dn_by_oid(cert->x509_cert, 
		GNUTLS_OID_X520_ORGANIZATION_NAME, 0, 0, subject_organization, &n))
		strncpy(subject_organization, _("<not in certificate>"), BUFFSIZE);
		
	exp_time_t = gnutls_x509_crt_get_expiration_time(cert->x509_cert);

	memset(buf, 0, sizeof(buf));
	if (exp_time_t > 0) {
		fast_strftime(buf, sizeof(buf)-1, prefs_common.date_format, localtime_r(&exp_time_t, &lt));
		exp_date = (*buf) ? g_strdup(buf):g_strdup("?");
	} else
		exp_date = g_strdup("");

	/* fingerprint */
	n = 128;
	gnutls_x509_crt_get_fingerprint(cert->x509_cert, GNUTLS_DIG_MD5, md, &n);
	md5_fingerprint = readable_fingerprint(md, (int)n);
	n = 128;
	gnutls_x509_crt_get_fingerprint(cert->x509_cert, GNUTLS_DIG_SHA1, md, &n);
	sha1_fingerprint = readable_fingerprint(md, (int)n);

	/* signature */
	sig_status = ssl_certificate_check_signer(cert->x509_cert, cert->status);

	if (sig_status==NULL)
		sig_status = g_strdup(_("Correct"));

	vbox = gtk_vbox_new(FALSE, 5);
	hbox = gtk_hbox_new(FALSE, 5);
	
	frame_owner  = gtk_frame_new(_("Owner"));
	frame_signer = gtk_frame_new(_("Signer"));
	frame_status = gtk_frame_new(_("Status"));
	
	owner_table = GTK_TABLE(gtk_table_new(3, 2, FALSE));
	signer_table = GTK_TABLE(gtk_table_new(3, 2, FALSE));
	status_table = GTK_TABLE(gtk_table_new(3, 2, FALSE));
	
	label = gtk_label_new(_("Name: "));
	gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
	gtk_table_attach(owner_table, label, 0, 1, 0, 1, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(subject_commonname);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_table_attach(owner_table, label, 1, 2, 0, 1, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	
	label = gtk_label_new(_("Organization: "));
	gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
	gtk_table_attach(owner_table, label, 0, 1, 1, 2, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(subject_organization);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_table_attach(owner_table, label, 1, 2, 1, 2, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	
	label = gtk_label_new(_("Location: "));
	gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
	gtk_table_attach(owner_table, label, 0, 1, 2, 3, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(subject_location);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_table_attach(owner_table, label, 1, 2, 2, 3, GTK_EXPAND|GTK_FILL, 0, 0, 0);

	label = gtk_label_new(_("Name: "));
	gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
	gtk_table_attach(signer_table, label, 0, 1, 0, 1, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(issuer_commonname);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_table_attach(signer_table, label, 1, 2, 0, 1, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	
	label = gtk_label_new(_("Organization: "));
	gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
	gtk_table_attach(signer_table, label, 0, 1, 1, 2, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(issuer_organization);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_table_attach(signer_table, label, 1, 2, 1, 2, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	
	label = gtk_label_new(_("Location: "));
	gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
	gtk_table_attach(signer_table, label, 0, 1, 2, 3, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(issuer_location);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_table_attach(signer_table, label, 1, 2, 2, 3, GTK_EXPAND|GTK_FILL, 0, 0, 0);

	label = gtk_label_new(_("Fingerprint: \n"));
	gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
	gtk_table_attach(status_table, label, 0, 1, 0, 1, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	fingerprint = g_strdup_printf("MD5: %s\nSHA1: %s", 
			md5_fingerprint, sha1_fingerprint);
	label = gtk_label_new(fingerprint);
	g_free(fingerprint);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_table_attach(status_table, label, 1, 2, 0, 1, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(_("Signature status: "));
	gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
	gtk_table_attach(status_table, label, 0, 1, 1, 2, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(sig_status);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_table_attach(status_table, label, 1, 2, 1, 2, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(_("Expires on: "));
	gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
	gtk_table_attach(status_table, label, 0, 1, 2, 3, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	label = gtk_label_new(exp_date);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_table_attach(status_table, label, 1, 2, 2, 3, GTK_EXPAND|GTK_FILL, 0, 0, 0);
	
	gtk_container_add(GTK_CONTAINER(frame_owner), GTK_WIDGET(owner_table));
	gtk_container_add(GTK_CONTAINER(frame_signer), GTK_WIDGET(signer_table));
	gtk_container_add(GTK_CONTAINER(frame_status), GTK_WIDGET(status_table));
	
	gtk_box_pack_end(GTK_BOX(hbox), frame_signer, TRUE, TRUE, 0);
	gtk_box_pack_end(GTK_BOX(hbox), frame_owner, TRUE, TRUE, 0);
	gtk_box_pack_end(GTK_BOX(vbox), frame_status, TRUE, TRUE, 0);
	gtk_box_pack_end(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);
	
	gtk_widget_show_all(vbox);
	
	g_free(issuer_commonname);
	g_free(issuer_location);
	g_free(issuer_organization);
	g_free(subject_commonname);
	g_free(subject_location);
	g_free(subject_organization);
	g_free(md5_fingerprint);
	g_free(sha1_fingerprint);
	g_free(sig_status);
	g_free(exp_date);
	return vbox;
}

static gboolean sslcert_ask_hook(gpointer source, gpointer data)
{
	SSLCertHookData *hookdata = (SSLCertHookData *)source;

	if (hookdata == NULL) {
		return FALSE;
	}
	
	if (prefs_common.skip_ssl_cert_check) {
		hookdata->accept = TRUE;
		return TRUE;
	}

	if (hookdata->old_cert == NULL) {
		if (hookdata->expired)
			hookdata->accept = sslcertwindow_ask_expired_cert(hookdata->cert);
		else
			hookdata->accept = sslcertwindow_ask_new_cert(hookdata->cert);
	} else {
		hookdata->accept = sslcertwindow_ask_changed_cert(hookdata->old_cert, hookdata->cert);
	}

	return TRUE;
}

void sslcertwindow_register_hook(void)
{
	hooks_register_hook(SSLCERT_ASK_HOOKLIST, sslcert_ask_hook, NULL);
}

void sslcertwindow_show_cert(SSLCertificate *cert)
{
	GtkWidget *cert_widget = cert_presenter(cert);
	gchar *buf;
	
	buf = g_strdup_printf(_("SSL certificate for %s"), cert->host);
	alertpanel_full(buf, NULL, GTK_STOCK_CLOSE, NULL, NULL,
	 		FALSE, cert_widget, ALERT_NOTICE, G_ALERTDEFAULT);
	g_free(buf);
}

static gchar *sslcertwindow_get_invalid_str(SSLCertificate *cert)
{
	gchar *subject_cn = NULL;
	gchar *str = NULL;

	if (ssl_certificate_check_subject_cn(cert))
		return g_strdup("");
	
	subject_cn = ssl_certificate_get_subject_cn(cert);
	
	str = g_strdup_printf(_("Certificate is for %s, but connection is to %s.\n"
				"You may be connecting to a rogue server.\n\n"), 
				subject_cn, cert->host);
	g_free(subject_cn);
	
	return str;
}

static gboolean sslcertwindow_ask_new_cert(SSLCertificate *cert)
{
	gchar *buf, *sig_status;
	AlertValue val;
	GtkWidget *vbox;
	GtkWidget *label;
	GtkWidget *button;
	GtkWidget *cert_widget;
	gchar *invalid_str = sslcertwindow_get_invalid_str(cert);
	const gchar *title;

	vbox = gtk_vbox_new(FALSE, 5);
	buf = g_strdup_printf(_("Certificate for %s is unknown.\n%sDo you want to accept it?"), cert->host, invalid_str);
	g_free(invalid_str);

	label = gtk_label_new(buf);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
	g_free(buf);
	
	sig_status = ssl_certificate_check_signer(cert->x509_cert, cert->status);
	if (sig_status==NULL)
		sig_status = g_strdup(_("Correct"));

	buf = g_strdup_printf(_("Signature status: %s"), sig_status);
	label = gtk_label_new(buf);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
	g_free(buf);
	g_free(sig_status);
	
	button = gtk_expander_new_with_mnemonic(_("_View certificate"));
	gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
	cert_widget = cert_presenter(cert);
	gtk_container_add(GTK_CONTAINER(button), cert_widget);

	if (!ssl_certificate_check_subject_cn(cert))
		title = _("SSL certificate is invalid");
	else
		title = _("SSL Certificate is unknown");

	val = alertpanel_full(title, NULL,
			      _("_Cancel connection"), _("_Accept and save"), NULL,
	 		      FALSE, vbox, ALERT_QUESTION, G_ALERTDEFAULT);
	
	return (val == G_ALERTALTERNATE);
}

static gboolean sslcertwindow_ask_expired_cert(SSLCertificate *cert)
{
	gchar *buf, *sig_status;
	AlertValue val;
	GtkWidget *vbox;
	GtkWidget *label;
	GtkWidget *button;
	GtkWidget *cert_widget;
	gchar *invalid_str = sslcertwindow_get_invalid_str(cert);
	const gchar *title;

	vbox = gtk_vbox_new(FALSE, 5);
	buf = g_strdup_printf(_("Certificate for %s is expired.\n%sDo you want to continue?"), cert->host, invalid_str);
	g_free(invalid_str);

	label = gtk_label_new(buf);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
	g_free(buf);
	
	sig_status = ssl_certificate_check_signer(cert->x509_cert, cert->status);

	if (sig_status==NULL)
		sig_status = g_strdup(_("Correct"));

	buf = g_strdup_printf(_("Signature status: %s"), sig_status);
	label = gtk_label_new(buf);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
	g_free(buf);
	g_free(sig_status);
	
	button = gtk_expander_new_with_mnemonic(_("_View certificate"));
	gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, FALSE, 0);
	cert_widget = cert_presenter(cert);
	gtk_container_add(GTK_CONTAINER(button), cert_widget);

	if (!ssl_certificate_check_subject_cn(cert))
		title = _("SSL certificate is invalid and expired");
	else
		title = _("SSL certificate is expired");

	val = alertpanel_full(title, NULL,
			      _("_Cancel connection"), _("_Accept"), NULL,
	 		      FALSE, vbox, ALERT_QUESTION, G_ALERTDEFAULT);
	
	return (val == G_ALERTALTERNATE);
}

static gboolean sslcertwindow_ask_changed_cert(SSLCertificate *old_cert, SSLCertificate *new_cert)
{
	GtkWidget *old_cert_widget = cert_presenter(old_cert);
	GtkWidget *new_cert_widget = cert_presenter(new_cert);
	GtkWidget *vbox;
	gchar *buf, *sig_status;
	GtkWidget *vbox2;
	GtkWidget *label;
	GtkWidget *button;
	AlertValue val;
	gchar *invalid_str = sslcertwindow_get_invalid_str(new_cert);
	const gchar *title;

	vbox = gtk_vbox_new(FALSE, 5);
	label = gtk_label_new(_("New certificate:"));
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_box_pack_end(GTK_BOX(vbox), new_cert_widget, TRUE, TRUE, 0);
	gtk_box_pack_end(GTK_BOX(vbox), label, TRUE, TRUE, 0);
	gtk_box_pack_end(GTK_BOX(vbox), gtk_hseparator_new(), TRUE, TRUE, 0);
	label = gtk_label_new(_("Known certificate:"));
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_box_pack_end(GTK_BOX(vbox), old_cert_widget, TRUE, TRUE, 0);
	gtk_box_pack_end(GTK_BOX(vbox), label, TRUE, TRUE, 0);
	gtk_widget_show_all(vbox);
	
	vbox2 = gtk_vbox_new(FALSE, 5);
	buf = g_strdup_printf(_("Certificate for %s has changed.\n%sDo you want to accept it?"), new_cert->host, invalid_str);
	g_free(invalid_str);

	label = gtk_label_new(buf);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_box_pack_start(GTK_BOX(vbox2), label, TRUE, TRUE, 0);
	g_free(buf);
	
	sig_status = ssl_certificate_check_signer(new_cert->x509_cert, new_cert->status);

	if (sig_status==NULL)
		sig_status = g_strdup(_("Correct"));

	buf = g_strdup_printf(_("Signature status: %s"), sig_status);
	label = gtk_label_new(buf);
	gtk_label_set_selectable(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
	gtk_box_pack_start(GTK_BOX(vbox2), label, TRUE, TRUE, 0);
	g_free(buf);
	g_free(sig_status);
	
	button = gtk_expander_new_with_mnemonic(_("_View certificates"));
	gtk_box_pack_start(GTK_BOX(vbox2), button, FALSE, FALSE, 0);
	gtk_container_add(GTK_CONTAINER(button), vbox);

	if (!ssl_certificate_check_subject_cn(new_cert))
		title = _("SSL certificate changed and is invalid");
	else
		title = _("SSL certificate changed");
	val = alertpanel_full(title, NULL,
			      _("_Cancel connection"), _("_Accept and save"), NULL,
	 		      FALSE, vbox2, ALERT_WARNING, G_ALERTDEFAULT);
	
	return (val == G_ALERTALTERNATE);
}
#endif
