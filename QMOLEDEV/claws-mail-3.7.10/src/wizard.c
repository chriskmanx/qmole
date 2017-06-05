/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 Colin Leroy <colin@colino.net> 
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
#endif

#include "defs.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "utils.h"
#include "gtk/menu.h"
#include "plugin.h"
#include "account.h"
#include "prefs_account.h"
#include "mainwindow.h"
#include "stock_pixmap.h"
#include "setup.h"
#include "folder.h"
#include "alertpanel.h"
#include "filesel.h"
#ifdef USE_GNUTLS
#include "ssl.h"
#endif
#include "prefs_common.h"
#include "combobox.h"

#ifdef MAEMO
#include <libgnomevfs/gnome-vfs-volume.h>
#include <libgnomevfs/gnome-vfs-volume-monitor.h>
#include <libgnomevfs/gnome-vfs-utils.h>
#endif

typedef enum
{
	GO_BACK,
	GO_FORWARD,
	CANCEL,
	FINISHED
} PageNavigation;

int WELCOME_PAGE = -1;
int USER_PAGE = -1;
int SMTP_PAGE = -1;
int RECV_PAGE = -1;
int MAILBOX_PAGE = -1;
int DONE_PAGE = -1;

typedef struct
{
	GtkWidget *window;
	GSList    *pages;
	GtkWidget *notebook;

	MainWindow *mainwin;
	
	GtkWidget *email;
	GtkWidget *full_name;
	GtkWidget *organization;

	GtkWidget *mailbox_name;
	GtkWidget *mailbox_label;
	
	GtkWidget *smtp_server;
	GtkWidget *smtp_auth;
	GtkWidget *smtp_username;
	GtkWidget *smtp_password;
	GtkWidget *smtp_username_label;
	GtkWidget *smtp_password_label;

	GtkWidget *recv_type;
	GtkWidget *recv_label;
	GtkWidget *recv_server;
	GtkWidget *recv_username;
	GtkWidget *recv_password;
	GtkWidget *recv_username_label;
	GtkWidget *recv_password_label;
	GtkWidget *recv_imap_label;
	GtkWidget *recv_imap_subdir;
	GtkWidget *subsonly_checkbtn;
	GtkWidget *no_imap_warning;
#ifdef USE_GNUTLS
	GtkWidget *smtp_use_ssl;
	GtkWidget *recv_use_ssl;
	GtkWidget *smtp_use_tls;
	GtkWidget *recv_use_tls;
	GtkWidget *smtp_ssl_cert_file;
	GtkWidget *recv_ssl_cert_file;
	GtkWidget *smtp_ssl_cert_pass;
	GtkWidget *recv_ssl_cert_pass;
	GtkWidget *smtp_cert_table;
	GtkWidget *recv_cert_table;
#endif

#ifdef MAEMO
	GtkWidget *data_root_nokia_radiobtn;
	GtkWidget *data_root_mmc1_radiobtn;
	GtkWidget *data_root_mmc2_radiobtn;
	GnomeVFSVolumeMonitor *volmon;
	gulong volmon_mount_sigid;
	gulong volmon_unmount_sigid;
	GnomeVFSVolume *vol_mmc1;
	GnomeVFSVolume *vol_mmc2;
#endif	
	gboolean create_mailbox;
	gboolean finished;
	gboolean result;

} WizardWindow;

typedef struct _AccountTemplate {
	gchar *name;
	gchar *domain;
	gchar *email;
	gchar *organization;
	gchar *smtpserver;
	gboolean smtpauth;
	gchar *smtpuser;
	gchar *smtppass;
	RecvProtocol recvtype;
	gchar *recvserver;
	gchar *recvuser;
	gchar *recvpass;
	gchar *imapdir;
	gboolean subsonly;
	gchar *mboxfile;
	gchar *mailbox;
	gboolean smtpssl;
	gboolean recvssl;
	gchar *smtpssl_cert;
	gchar *recvssl_cert;
	gchar *smtpssl_cert_pass;
	gchar *recvssl_cert_pass;
} AccountTemplate;

static AccountTemplate tmpl;

static PrefParam template_params[] = {
	{"name", "$USERNAME",
	 &tmpl.name, P_STRING, NULL, NULL, NULL},
	{"domain", "$DEFAULTDOMAIN",
	 &tmpl.domain, P_STRING, NULL, NULL, NULL},
	{"email", "$NAME_MAIL@$DOMAIN",
	 &tmpl.email, P_STRING, NULL, NULL, NULL},
	{"organization", "",
	 &tmpl.organization, P_STRING, NULL, NULL, NULL},
	{"smtpserver", "smtp.$DOMAIN",
	 &tmpl.smtpserver, P_STRING, NULL, NULL, NULL},
	{"smtpauth", "FALSE",
	 &tmpl.smtpauth, P_BOOL, NULL, NULL, NULL},
	{"smtpuser", "",
	 &tmpl.smtpuser, P_STRING, NULL, NULL, NULL},
	{"smtppass", "",
	 &tmpl.smtppass, P_STRING, NULL, NULL, NULL},
	{"recvtype", A_POP3,
	 &tmpl.recvtype, P_INT, NULL, NULL, NULL},
	{"recvserver", "pop.$DOMAIN",
	 &tmpl.recvserver, P_STRING, NULL, NULL, NULL},
	{"recvuser", "$LOGIN",
	 &tmpl.recvuser, P_STRING, NULL, NULL, NULL},
	{"recvpass", "",
	 &tmpl.recvpass, P_STRING, NULL, NULL, NULL},
	{"imapdir", "",
	 &tmpl.imapdir, P_STRING, NULL, NULL, NULL},
	{"subsonly", "TRUE",
	 &tmpl.subsonly, P_BOOL, NULL, NULL, NULL},
	{"mboxfile", "/var/mail/$LOGIN",
	 &tmpl.mboxfile, P_STRING, NULL, NULL, NULL},
	{"mailbox", "Mail",
	 &tmpl.mailbox, P_STRING, NULL, NULL, NULL},
	{"smtpssl", "0",
	 &tmpl.smtpssl, P_INT, NULL, NULL, NULL},
	{"recvssl", "0",
	 &tmpl.recvssl, P_INT, NULL, NULL, NULL},
	{"smtpssl_cert", "",
	 &tmpl.smtpssl_cert, P_STRING, NULL, NULL, NULL},
	{"recvssl_cert", "",
	 &tmpl.recvssl_cert, P_STRING, NULL, NULL, NULL},
	{"smtpssl_cert_pass", "",
	 &tmpl.smtpssl_cert, P_STRING, NULL, NULL, NULL},
	{"recvssl_cert_pass", "",
	 &tmpl.recvssl_cert, P_STRING, NULL, NULL, NULL},
	{NULL, NULL, NULL, P_INT, NULL, NULL, NULL}
};


static gchar *accountrc_tmpl =
	"[AccountTemplate]\n"
	"#you can use $DEFAULTDOMAIN here\n"
	"#domain must be defined before the variables that use it\n"
	"#by default, domain is extracted from the hostname\n"
	"#domain=\n"
	"\n"
	"#you can use $USERNAME for name (this is the default)\n"
	"#name=\n"
	"\n"
	"#you can use $LOGIN, $NAME_MAIL and $DOMAIN here \n"
	"#$NAME_MAIL is the name without uppercase and with dots instead\n"
	"#of spaces\n"
	"#the default is $NAME_MAIL@$DOMAIN\n"
	"#email=\n"
	"\n"
	"#you can use $DOMAIN here\n"
	"#the default organization is empty\n"
	"#organization=\n"
	"\n"
	"#you can use $DOMAIN here \n"
	"#the default is smtp.$DOMAIN\n"
	"#smtpserver=\n"
	"\n"
	"#Whether to use smtp authentication\n"
	"#the default is 0 (no)\n"
	"#smtpauth=\n"
	"\n"
	"#SMTP username\n"
	"#you can use $LOGIN, $NAME_MAIL, $DOMAIN or $EMAIL here\n"
	"#the default is empty (same as reception username)\n"
	"#smtpuser=\n"
	"\n"
	"#SMTP password\n"
	"#the default is empty (same as reception password)\n"
	"#smtppass=\n"
	"\n"
	"#recvtype can be:\n"
	"#0 for pop3\n"
	"#3  for imap\n"
	"#5  for a local mbox file\n"
	"#recvtype=\n"
	"\n"
	"#you can use $DOMAIN here \n"
	"#the default is {pop,imap}.$DOMAIN\n"
	"#recvserver=\n"
	"\n"
	"#you can use $LOGIN, $NAME_MAIL, $DOMAIN or $EMAIL here\n"
	"#default is $LOGIN\n"
	"#recvuser=\n"
	"\n"
	"#default is empty\n"
	"#recvpass=\n"
	"\n"
	"#imap dir if imap (relative to the home on the server)\n"
	"#default is empty\n"
	"#imapdir=\n"
	"\n"
	"#show subscribed folders only, if imap\n"
	"#default is TRUE\n"
	"#subsonly=\n"
	"\n"
	"#mbox file if local\n"
	"#you can use $LOGIN here\n"
	"#default is /var/mail/$LOGIN\n"
	"#mboxfile=\n"
	"\n"
	"#mailbox name if pop3 or local\n"
	"#relative path from the user's home\n"
	"#default is \"Mail\"\n"
	"#mailbox=\n"
	"\n"
	"#whether to use ssl on smtp connections\n"
	"#default is 0, 1 is ssl, 2 is starttls\n"
	"#smtpssl=\n"
	"\n"
	"#whether to use ssl on pop or imap connections\n"
	"#default is 0, 1 is ssl, 2 is starttls\n"
	"#recvssl=\n"
	"\n"
	"#SSL client certificate path for SMTP\n"
	"#default is empty (no certificate)\n"
	"#smtpssl_cert=\n"
	"\n"
	"#SSL client certificate path for POP/IMAP\n"
	"#default is empty (no certificate)\n"
	"#recvssl_cert=\n"
	"\n"
	"#SSL client certificate password for SMTP\n"
	"#default is empty (no password)\n"
	"#smtpssl_cert_pass=\n"
	"\n"
	"#SSL client certificate password for POP/IMAP\n"
	"#default is empty (no password)\n"
	"#recvssl_cert_pass=\n"
	;

static gchar *wizard_get_default_domain_name(void)
{
	static gchar *domain_name = NULL;
	
	if (domain_name == NULL) {
		domain_name = g_strdup(get_domain_name());
		if (strchr(domain_name, '.') != NULL 
		&& strchr(domain_name, '.') != strrchr(domain_name, '.')
		&& strlen(strchr(domain_name, '.')) > 6) {
			gchar *tmp = g_strdup(strchr(domain_name, '.')+1);
			g_free(domain_name);
			domain_name = tmp;
		}
	}
	return domain_name;
}

static gchar *get_name_for_mail(void)
{
	gchar *name = NULL;
	if (tmpl.name == NULL)
		return NULL;
	name = g_utf8_strdown(tmpl.name, -1);
	while(strstr(name, " "))
		*strstr(name, " ")='.';
	
	return name;
}

#define PARSE_DEFAULT(str) {	\
	gchar *tmp = NULL, *new = NULL;	\
	if (str != NULL) {	\
		tmp = g_strdup(str);	\
		if (strstr(str, "$USERNAME")) {	\
			tmp = g_strdup(str);	\
			*strstr(tmp, "$USERNAME") = '\0';	\
			new = g_strconcat(tmp, g_get_real_name(), 	\
				strstr(str, "$USERNAME")+strlen("$USERNAME"), 	\
				NULL);	\
			g_free(tmp);	\
			g_free(str);	\
			str = new;	\
			new = NULL;	\
		}	\
		if (strstr(str, "$LOGIN")) {	\
			tmp = g_strdup(str);	\
			*strstr(tmp, "$LOGIN") = '\0';	\
			new = g_strconcat(tmp, g_get_user_name(), 	\
				strstr(str, "$LOGIN")+strlen("$LOGIN"), 	\
				NULL);	\
			g_free(tmp);	\
			g_free(str);	\
			str = new;	\
			new = NULL;	\
		}	\
		if (strstr(str, "$EMAIL")) {	\
			tmp = g_strdup(str);	\
			*strstr(tmp, "$EMAIL") = '\0';	\
			new = g_strconcat(tmp, tmpl.email, 	\
				strstr(str, "$EMAIL")+strlen("$EMAIL"), 	\
				NULL);	\
			g_free(tmp);	\
			g_free(str);	\
			str = new;	\
			new = NULL;	\
		}	\
		if (strstr(str, "$NAME_MAIL")) {	\
			tmp = g_strdup(str);	\
			*strstr(tmp, "$NAME_MAIL") = '\0';	\
			new = g_strconcat(tmp, get_name_for_mail(), 	\
				strstr(str, "$NAME_MAIL")+strlen("$NAME_MAIL"), 	\
				NULL);	\
			g_free(tmp);	\
			g_free(str);	\
			str = new;	\
			new = NULL;	\
		}	\
		if (strstr(str, "$DEFAULTDOMAIN")) {	\
			tmp = g_strdup(str);	\
			*strstr(tmp, "$DEFAULTDOMAIN") = '\0';	\
			new = g_strconcat(tmp, wizard_get_default_domain_name(), 	\
				strstr(str, "$DEFAULTDOMAIN")+strlen("$DEFAULTDOMAIN"), 	\
				NULL);	\
			g_free(tmp);	\
			g_free(str);	\
			str = new;	\
			new = NULL;	\
		}	\
		if (strstr(str, "$DOMAIN")) {	\
			tmp = g_strdup(str);	\
			*strstr(tmp, "$DOMAIN") = '\0';	\
			new = g_strconcat(tmp, tmpl.domain, 	\
				strstr(str, "$DOMAIN")+strlen("$DOMAIN"), 	\
				NULL);	\
			g_free(tmp);	\
			g_free(str);	\
			str = new;	\
			new = NULL;	\
		}	\
	}	\
}
static void wizard_read_defaults(void)
{
	gchar *rcpath;

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, "accountrc.tmpl", NULL);
	if (!is_file_exist(rcpath)) {
		str_write_to_file(accountrc_tmpl, rcpath);
	}

	prefs_read_config(template_params, "AccountTemplate", rcpath, NULL);

	PARSE_DEFAULT(tmpl.domain);
	PARSE_DEFAULT(tmpl.name);
	PARSE_DEFAULT(tmpl.email);
	PARSE_DEFAULT(tmpl.organization);
	PARSE_DEFAULT(tmpl.smtpserver);
	PARSE_DEFAULT(tmpl.smtpuser);
	PARSE_DEFAULT(tmpl.smtppass);
	PARSE_DEFAULT(tmpl.recvserver);
	PARSE_DEFAULT(tmpl.recvuser);
	PARSE_DEFAULT(tmpl.recvpass);
	PARSE_DEFAULT(tmpl.imapdir);
	PARSE_DEFAULT(tmpl.mboxfile);
	PARSE_DEFAULT(tmpl.mailbox);
/*
	g_print("defaults:"
	"%s, %s, %s, %s, %s, %d, %s, %s, %s, %s, %s, %s, %d, %d\n",
	tmpl.name,tmpl.domain,tmpl.email,tmpl.organization,tmpl.smtpserver,
	tmpl.recvtype,tmpl.recvserver,tmpl.recvuser,tmpl.recvpass,
	tmpl.imapdir,tmpl.mboxfile,tmpl.mailbox,tmpl.smtpssl,tmpl.recvssl);
*/
	g_free(rcpath);
}

static void initialize_fonts(WizardWindow *wizard)
{
	GtkWidget *widget = wizard->email;
	gint size = pango_font_description_get_size(
			widget->style->font_desc)
		      /PANGO_SCALE;
	gchar *tmp, *new;
#ifdef G_OS_WIN32
	PangoFontDescription *bold_desc;
	gchar *curfont = pango_font_description_to_string(widget->style->font_desc);
	g_free(prefs_common.smallfont);
	g_free(prefs_common.normalfont);
	g_free(prefs_common.boldfont);
	prefs_common.smallfont = g_strdup(curfont);
	prefs_common.normalfont = g_strdup(curfont);
	bold_desc = pango_font_description_from_string(curfont);
	pango_font_description_set_weight(bold_desc, PANGO_WEIGHT_BOLD);
	prefs_common.boldfont = pango_font_description_to_string(bold_desc);
	pango_font_description_free(bold_desc);
	g_free(curfont);
#endif	
	tmp = g_strdup(prefs_common.textfont);
	if (strrchr(tmp, ' ')) {
		*(strrchr(tmp, ' ')) = '\0';
		new = g_strdup_printf("%s %d", tmp, size);
		g_free(prefs_common.textfont);
		prefs_common.textfont = new;
	}
	g_free(tmp);
	
	tmp = g_strdup(prefs_common.smallfont);
	if (strrchr(tmp, ' ')) {
		*(strrchr(tmp, ' ')) = '\0';
		new = g_strdup_printf("%s %d", tmp, size);
		g_free(prefs_common.smallfont);
		prefs_common.smallfont = new;
	}
	g_free(tmp);
	
	tmp = g_strdup(prefs_common.normalfont);
	if (strrchr(tmp, ' ')) {
		*(strrchr(tmp, ' ')) = '\0';
		new = g_strdup_printf("%s %d", tmp, size);
		g_free(prefs_common.normalfont);
		prefs_common.normalfont = new;
	}
	g_free(tmp);

	tmp = g_strdup(prefs_common.boldfont);
	if (strrchr(tmp, ' ')) {
		*(strrchr(tmp, ' ')) = '\0';
		new = g_strdup_printf("%s %d", tmp, size);
		g_free(prefs_common.boldfont);
		prefs_common.boldfont = new;
	}
	g_free(tmp);
}

#define XFACE "+}Axz@~a,-Yx?0Ysa|q}CLRH=89Y]\"')DSX^<6p\"d)'81yx5%G#u^o*7JG&[aPU0h1Ux.vb2yIjH83{5`/bVo|~nn/i83vE^E)qk-4W)_E.4Y=D*qvf/,Ci_=P<iY<M6"
#define FACE "iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAM1BMVEUAAAAcJCI\n\
 ONl1JUi0+Z4daY2NZciPabV9ykS5kj6Wsl2ybmZOBsMjZxK2wzN3Pzczs7OsCAGN0AAAAAXRSTlM\n\
 AQObYZgAAAAFiS0dEAIgFHUgAAAIVSURBVEjH1ZbtlqMgDIaFECoGhPu/2s0b0Lais/NzN6d1OJ7\n\
 3yReQzrL8B5Zy3rvl9At52Pf2tv1vSMjtYj8jaa8XUyI/yn3YD6sigj/2Tf5Bn069MIsTPHvx/t5\n\
 /3rU/6JCIY3YwGe26r/cwUfE3cFe5T28L0K5rJAUHEeYAQxs8DHojjk3M9wECU4xxjXisI8RA0gy\n\
 oczJZJOjxiTFZVTchAnIRJrgdmEGDyFfAI3UuG5FmYTkR9RDrIC4H0SqV4pzpEcUp0HNLjwBv+jA\n\
 dikUE5g9iBvzmu3sH2oDk4lnHd829+2Q9gj6kDqDPg7hsGwBzH02fE3ZCt6ZHmlNKIMjMeRwra5I\n\
 ecgNoBnLGPmzaHPJIwLY8Sq2M/tLUJfj0QcqmfVXAdLSStIYF8dzWjBBb2VgvDa4mO9oc651OiUo\n\
 BEKbZokdPATF9E9oKAjQJcJOniaPXrVZRAnVWaqIyqRoNC8ZJvgCcW8XN39RqxVP1rS8Yd4WnCdN\n\
 aRTo2jJRDbg3vtCpEUGffgDPhqKDaSuVqYtOplFIvIcx3HUI5/MuIWl6vKyBjNlqEru8hbFXqBPA\n\
 5TpHGIUZOePeaIyzfQ/g9Xg0opU1AvdfXM9floYhv92pPAE96OZtkPV8eivgQi9RTfwPUU36I26n\n\
 Hy+WuCJzAT7efMSeA1TNf2/VugDz+dN139xfA5ffxGZDD+MvcP/uvyB80wzZ76wbz8gAAAABJRU5\n\
 ErkJggg=="

static void write_welcome_email(WizardWindow *wizard)
{
	gchar buf_date[64];
	gchar *head=NULL;
	gchar *body=NULL;
	gchar *msg=NULL;
	const gchar *mailbox = gtk_entry_get_text(GTK_ENTRY(wizard->mailbox_name));
	Folder *folder = folder_find_from_path(mailbox);
	FolderItem *inbox = folder ? folder->inbox:NULL;
	gchar *file = get_tmp_file();
	gchar enc_from_name[BUFFSIZE], enc_to_name[BUFFSIZE], enc_subject[BUFFSIZE];
	
	get_rfc822_date(buf_date, sizeof(buf_date));

	conv_encode_header_full(enc_subject, sizeof(enc_subject), 
			Q_("Welcome Mail Subject|Welcome to Claws Mail"),
			strlen("Subject: "), FALSE, CS_INTERNAL);
	conv_encode_header_full(enc_to_name, sizeof(enc_to_name), 
			gtk_entry_get_text(GTK_ENTRY(wizard->full_name)),
			strlen("To: "), TRUE, CS_INTERNAL);
	conv_encode_header_full(enc_from_name, sizeof(enc_from_name), 
			_("The Claws Mail Team"),
			strlen("From: "), TRUE, CS_INTERNAL);

	head = g_strdup_printf(
		"From: %s <%s>\n"
		"To: %s <%s>\n"
		"Date: %s\n"
		"Subject: %s\n"
		"X-Face: %s\n"
		"Face: %s\n"
		"Content-Type: text/plain; charset=UTF-8\n",
		enc_from_name,
		USERS_ML_ADDR,
		enc_to_name,
		gtk_entry_get_text(GTK_ENTRY(wizard->email)),
		buf_date, enc_subject, XFACE, FACE);
	body = g_strdup_printf(
		_("\n"
		"Welcome to Claws Mail\n"
		"---------------------\n"
		"\n"
		"Now that you have set up your account you can fetch your\n"
		"mail by clicking the 'Get Mail' button at the left of the\n"
		"toolbar.\n"
		"\n"
		"Claws Mail has lots of extra features accessible via plugins,\n"
		"like anti-spam filtering and learning (via the Bogofilter or\n"
		"SpamAssassin plugins), privacy protection (via PGP/Mime), an RSS\n"
		"aggregator, a calendar, and much more. You can load them from\n"
		"the menu entry '/Configuration/Plugins'.\n"
		"\n"
		"You can change your Account Preferences by using the menu\n"
		"entry '/Configuration/Preferences for current account'\n"
		"and change the general Preferences by using\n"
		"'/Configuration/Preferences'.\n"
		"\n"
		"You can find further information in the Claws Mail manual,\n"
		"which can be accessed by using the menu entry '/Help/Manual'\n"
		"or online at the URL given below.\n"
		"\n"
		"Useful URLs\n"
		"-----------\n"
		"Homepage:      <%s>\n"
		"Manual:        <%s>\n"
		"FAQ:	       <%s>\n"
		"Themes:        <%s>\n"
		"Mailing Lists: <%s>\n"
		"\n"
		"LICENSE\n"
		"-------\n"
		"Claws Mail is free software, released under the terms\n"
		"of the GNU General Public License, version 3 or later, as\n"
		"published by the Free Software Foundation, 51 Franklin Street,\n"
		"Fifth Floor, Boston, MA 02110-1301, USA. The license can be\n"
		"found at <%s>.\n"
		"\n"
		"DONATIONS\n"
		"---------\n"
		"If you wish to donate to the Claws Mail project you can do\n"
		"so at <%s>.\n\n"),
		HOMEPAGE_URI, MANUAL_URI, FAQ_URI, THEMES_URI, MAILING_LIST_URI,
		GPL_URI, DONATE_URI);
	
	msg = g_strconcat(head, body, NULL);

	if (inbox && inbox->total_msgs == 0
	 && str_write_to_file(msg, file) >= 0) {
		MsgFlags flags = { MSG_UNREAD|MSG_NEW, 0};
		folder_item_add_msg(inbox, file, &flags, FALSE);
	}
	g_free(head);
	g_free(body);
	g_free(msg);
	claws_unlink(file);
}
#undef XFACE

static gboolean wizard_write_config(WizardWindow *wizard)
{
	static gboolean mailbox_ok = FALSE;
	PrefsAccount *prefs_account = prefs_account_new();
	GList *account_list = NULL;
	gchar *smtp_server, *recv_server;
	gint smtp_port, recv_port;
#ifdef USE_GNUTLS
	SSLType smtp_ssl_type, recv_ssl_type;
#endif

	prefs_account->protocol = combobox_get_active_data(
					GTK_COMBO_BOX(wizard->recv_type));
	
	if (wizard->create_mailbox && prefs_account->protocol != A_IMAP4 && 
	    !strlen(gtk_entry_get_text(GTK_ENTRY(wizard->mailbox_name)))) {
		alertpanel_error(_("Please enter the mailbox name."));
		g_free(prefs_account);
		gtk_notebook_set_current_page (
			GTK_NOTEBOOK(wizard->notebook), 
			MAILBOX_PAGE);
		return FALSE;
	}

#ifdef MAEMO
	if (wizard->create_mailbox) {
		g_free(prefs_common.data_root);
		if (gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON(wizard->data_root_nokia_radiobtn)))
			prefs_common.data_root = NULL;
		else if (gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON(wizard->data_root_mmc1_radiobtn)))
			prefs_common.data_root = g_strdup(MMC1_PATH);
		else if (gtk_toggle_button_get_active(
			GTK_TOGGLE_BUTTON(wizard->data_root_mmc2_radiobtn)))
			prefs_common.data_root = g_strdup(MMC2_PATH);
	}
#endif

	if (!mailbox_ok) {
		if (wizard->create_mailbox && prefs_account->protocol != A_IMAP4) {
			mailbox_ok = setup_write_mailbox_path(wizard->mainwin, 
					gtk_entry_get_text(
						GTK_ENTRY(wizard->mailbox_name)));
		} else
			mailbox_ok = TRUE;
	}

	if (!mailbox_ok) {
		/* alertpanel done by setup_write_mailbox_path */
		g_free(prefs_account);
		gtk_notebook_set_current_page (
			GTK_NOTEBOOK(wizard->notebook), 
			MAILBOX_PAGE);
		return FALSE;
	}
	
	if (!strlen(gtk_entry_get_text(GTK_ENTRY(wizard->full_name)))
	||  !strlen(gtk_entry_get_text(GTK_ENTRY(wizard->email)))) {
		alertpanel_error(_("Please enter your name and email address."));
		g_free(prefs_account);
		gtk_notebook_set_current_page (
			GTK_NOTEBOOK(wizard->notebook), 
			USER_PAGE);
		return FALSE;
	}
	
	if (prefs_account->protocol != A_LOCAL) {
		if (!strlen(gtk_entry_get_text(GTK_ENTRY(wizard->recv_username)))
		||  !strlen(gtk_entry_get_text(GTK_ENTRY(wizard->recv_server)))) {
			alertpanel_error(_("Please enter your receiving server "
					   "and username."));
			g_free(prefs_account);
			gtk_notebook_set_current_page (
				GTK_NOTEBOOK(wizard->notebook), 
				RECV_PAGE);
			return FALSE;
		}
	} else {
		if (!strlen(gtk_entry_get_text(GTK_ENTRY(wizard->recv_server)))) {
			alertpanel_error(_("Please enter your username."));
			g_free(prefs_account);
			gtk_notebook_set_current_page (
				GTK_NOTEBOOK(wizard->notebook), 
				RECV_PAGE);
			return FALSE;
		}
	}
	
	if (!strlen(gtk_entry_get_text(GTK_ENTRY(wizard->smtp_server)))) {
		alertpanel_error(_("Please enter your SMTP server."));
		g_free(prefs_account);
		gtk_notebook_set_current_page (
			GTK_NOTEBOOK(wizard->notebook), 
			SMTP_PAGE);
		return FALSE;
	}

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wizard->smtp_auth))) {
		if (prefs_account->protocol == A_LOCAL
		&&  !strlen(gtk_entry_get_text(GTK_ENTRY(wizard->smtp_username)))) {
			alertpanel_error(_("Please enter your SMTP username."));
			g_free(prefs_account);
			gtk_notebook_set_current_page (
				GTK_NOTEBOOK(wizard->notebook), 
				SMTP_PAGE);
			return FALSE;		
		} /* if it's not local we'll use the reception server */
	}

	if (prefs_account->protocol != A_LOCAL)
		prefs_account->account_name = g_strdup_printf("%s@%s",
				gtk_entry_get_text(GTK_ENTRY(wizard->recv_username)),
				gtk_entry_get_text(GTK_ENTRY(wizard->recv_server)));
	else
		prefs_account->account_name = g_strdup_printf("%s",
				gtk_entry_get_text(GTK_ENTRY(wizard->recv_server)));

	recv_server = g_strdup(gtk_entry_get_text(GTK_ENTRY(wizard->recv_server)));
	smtp_server = g_strdup(gtk_entry_get_text(GTK_ENTRY(wizard->smtp_server)));

	if (prefs_account->protocol != A_LOCAL && strstr(recv_server, ":")) {
		recv_port = atoi(strstr(recv_server, ":")+1);
		*(strstr(recv_server, ":")) = '\0';
		if (prefs_account->protocol == A_IMAP4) {
			prefs_account->set_imapport = TRUE;
			prefs_account->imapport = recv_port;
		} else if (prefs_account->protocol == A_POP3) {
			prefs_account->set_popport = TRUE;
			prefs_account->popport = recv_port;
		}
	}
	if (strstr(smtp_server, ":")) {
		smtp_port = atoi(strstr(smtp_server, ":")+1);
		*(strstr(smtp_server, ":")) = '\0';
		prefs_account->set_smtpport = TRUE;
		prefs_account->smtpport = smtp_port;
	}
	
	prefs_account->name = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->full_name)));
	prefs_account->address = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->email)));
	prefs_account->organization = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->organization)));
	prefs_account->smtp_server = g_strdup(smtp_server);

	if (wizard->create_mailbox && prefs_account->protocol != A_IMAP4) {
		gchar *tmp;
		tmp = g_path_get_basename(gtk_entry_get_text(GTK_ENTRY(wizard->mailbox_name)));
		prefs_account->inbox = g_strdup_printf("#mh/%s/inbox",
			(!strcmp("Mail", gtk_entry_get_text(GTK_ENTRY(wizard->mailbox_name))))
				?_("Mailbox"):tmp);
		g_free(tmp);
		prefs_account->local_inbox = g_strdup(prefs_account->inbox);
	} else if (prefs_account->protocol != A_IMAP4) {
		if (folder_get_default_inbox())
			prefs_account->local_inbox = 
				folder_item_get_identifier(folder_get_default_inbox());
	}

	if (prefs_account->protocol != A_LOCAL)
		prefs_account->recv_server = g_strdup(recv_server);
	else
		prefs_account->local_mbox = g_strdup(recv_server);

	g_free(recv_server);
	g_free(smtp_server);

	prefs_account->userid = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->recv_username)));
	prefs_account->passwd = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->recv_password)));

	prefs_account->smtp_userid = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->smtp_username)));
	prefs_account->smtp_passwd = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->smtp_password)));
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wizard->smtp_auth))) {
		prefs_account->use_smtp_auth = TRUE;
	}

#ifdef USE_GNUTLS
	smtp_ssl_type = SSL_NONE;
	recv_ssl_type = SSL_NONE;	

	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wizard->smtp_use_ssl))) {
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wizard->smtp_use_tls)))
			smtp_ssl_type = SSL_STARTTLS;
		else
			smtp_ssl_type = SSL_TUNNEL;
	}
	if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wizard->recv_use_ssl))) {
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(wizard->recv_use_tls)))
			recv_ssl_type = SSL_STARTTLS;
		else
			recv_ssl_type = SSL_TUNNEL;
	}

	prefs_account->ssl_smtp = smtp_ssl_type;

	if (prefs_account->protocol == A_IMAP4)
		prefs_account->ssl_imap = recv_ssl_type;
	else
		prefs_account->ssl_pop = recv_ssl_type;

	prefs_account->out_ssl_client_cert_file = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->smtp_ssl_cert_file)));
	prefs_account->out_ssl_client_cert_pass = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->smtp_ssl_cert_pass)));
	prefs_account->in_ssl_client_cert_file = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->recv_ssl_cert_file)));
	prefs_account->in_ssl_client_cert_pass = g_strdup(
				gtk_entry_get_text(GTK_ENTRY(wizard->recv_ssl_cert_pass)));
#endif
	if (prefs_account->protocol == A_IMAP4) {
		gchar *directory = gtk_editable_get_chars(
			GTK_EDITABLE(wizard->recv_imap_subdir), 0, -1);
		if (directory && strlen(directory)) {
			prefs_account->imap_dir = g_strdup(directory);
		}
		prefs_account->imap_subsonly = 
			gtk_toggle_button_get_active(
				GTK_TOGGLE_BUTTON(wizard->subsonly_checkbtn));
		g_free(directory);
	}

	account_list = g_list_append(account_list, prefs_account);
	prefs_account_write_config_all(account_list);
	prefs_account_free(prefs_account);
	account_read_config_all();

	initialize_fonts(wizard);
	if (wizard->create_mailbox && prefs_account->protocol != A_IMAP4)
		write_welcome_email(wizard);

#ifdef MAEMO
	if (wizard->volmon_mount_sigid)
		g_signal_handler_disconnect(
					G_OBJECT(wizard->volmon),
					wizard->volmon_mount_sigid);
	if (wizard->volmon_unmount_sigid)
		g_signal_handler_disconnect(
					G_OBJECT(wizard->volmon),
					wizard->volmon_unmount_sigid);
#endif

#ifndef G_OS_WIN32 
	plugin_load_standard_plugins();
#endif
	return TRUE;
}

static GtkWidget* create_page (WizardWindow *wizard, const char * title)
{
	GtkWidget *w;
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *image;
	char *title_string;

	vbox = gtk_vbox_new (FALSE, 6);
	gtk_container_set_border_width  (GTK_CONTAINER(vbox), 10);

	/* create the titlebar */
	hbox = gtk_hbox_new (FALSE, 12);
	image = stock_pixmap_widget(wizard->window, 
			  	STOCK_PIXMAP_CLAWS_MAIL_ICON);
	gtk_box_pack_start (GTK_BOX(hbox), image, FALSE, FALSE, 0);
     	title_string = g_strconcat ("<span size=\"xx-large\" weight=\"ultrabold\">", title ? title : "", "</span>", NULL);
	w = gtk_label_new (title_string);
	gtk_label_set_use_markup (GTK_LABEL(w), TRUE);
	g_free (title_string);
	gtk_box_pack_start (GTK_BOX(hbox), w, FALSE, FALSE, 0);

	/* pack the titlebar */
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	/* pack the separator */
	gtk_box_pack_start (GTK_BOX(vbox), gtk_hseparator_new(), FALSE, FALSE, 0);

	/* pack space */
	w = gtk_alignment_new (0, 0, 0, 0);
	gtk_widget_set_size_request (w, 0, 6);
	gtk_box_pack_start (GTK_BOX(vbox), w, FALSE, FALSE, 0);

	return vbox;
}

#define PACK_BOX(hbox,text,entry) {					\
	GtkWidget *label = gtk_label_new(text);				\
	gtk_label_set_use_markup(GTK_LABEL(label), TRUE);		\
	if (GTK_IS_MISC(label))						\
		gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);	\
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);	\
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);	\
}

static gchar *get_default_server(WizardWindow * wizard, const gchar *type)
{
	if (!strcmp(type, "smtp")) {
		if (!tmpl.smtpserver || !strlen(tmpl.smtpserver))
			return g_strconcat(type, ".", tmpl.domain, NULL);
		else 
			return g_strdup(tmpl.smtpserver);
	} else {
		if (!tmpl.recvserver || !strlen(tmpl.recvserver))
			return g_strconcat(type, ".", tmpl.domain, NULL);
		else 
			return g_strdup(tmpl.recvserver);
	}
}

static gchar *get_default_account(WizardWindow * wizard)
{
	gchar *result = NULL;
	
	if (!tmpl.recvuser || !strlen(tmpl.recvuser)) {
		result = gtk_editable_get_chars(
				GTK_EDITABLE(wizard->email), 0, -1);

		if (strstr(result, "@")) {
			*(strstr(result,"@")) = '\0';
		} 
	} else {
		result = g_strdup(tmpl.recvuser);
	}
	return result;
}

static gchar *get_default_smtp_account(WizardWindow * wizard)
{
	gchar *result = NULL;
	
	if (!tmpl.smtpuser || !strlen(tmpl.smtpuser)) {
		return g_strdup("");
	} else {
		result = g_strdup(tmpl.smtpuser);
	}
	return result;
}

static void wizard_email_changed(GtkWidget *widget, gpointer data)
{
	WizardWindow *wizard = (WizardWindow *)data;
	RecvProtocol protocol;
	gchar *text;
	protocol = combobox_get_active_data(GTK_COMBO_BOX(wizard->recv_type));
	
	text = get_default_server(wizard, "smtp");
	gtk_entry_set_text(GTK_ENTRY(wizard->smtp_server), text);
	g_free(text);

	text = get_default_account(wizard);
	gtk_entry_set_text(GTK_ENTRY(wizard->recv_username), text);
	g_free(text);

	if (protocol == A_POP3) {
		text = get_default_server(wizard, "pop");
		gtk_entry_set_text(GTK_ENTRY(wizard->recv_server), text);
		g_free(text);
	} else if (protocol == A_IMAP4) {
		text = get_default_server(wizard, "imap");
		gtk_entry_set_text(GTK_ENTRY(wizard->recv_server), text);
		g_free(text);
	} else if (protocol == A_LOCAL) {
		gtk_entry_set_text(GTK_ENTRY(wizard->recv_server), tmpl.mboxfile?tmpl.mboxfile:"");
	}
	
}

static GtkWidget* user_page (WizardWindow * wizard)
{
	GtkWidget *table = gtk_table_new(1,1, FALSE);
	GtkWidget *vbox;
	GtkWidget *label;
	GtkWidget *user_table;
	
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	vbox = gtk_vbox_new(FALSE, VSPACING_NARROW);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), VSPACING_NARROW_2);

	gtk_table_attach(GTK_TABLE(table), vbox, 0,1,1,2, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	user_table = gtk_table_new(3, 2, FALSE);
	gtk_table_set_row_spacings(GTK_TABLE(user_table), VSPACING_NARROW);
	gtk_box_pack_start(GTK_BOX(vbox), user_table, FALSE, FALSE, 0);

	label = gtk_label_new(_("<span weight=\"bold\">Your name:</span>"));
	gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
	gtk_table_attach(GTK_TABLE(user_table), label, 0,1,0,1, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->full_name = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(wizard->full_name), tmpl.name?tmpl.name:"");
	gtk_table_attach(GTK_TABLE(user_table), wizard->full_name, 1,2,0,1, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	label = gtk_label_new(_("<span weight=\"bold\">Your email address:</span>"));
	gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
	gtk_table_attach(GTK_TABLE(user_table), label, 0,1,1,2, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->email = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(wizard->email), tmpl.email?tmpl.email:"");
	gtk_table_attach(GTK_TABLE(user_table), wizard->email, 1,2,1,2, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	label = gtk_label_new(_("Your organization:"));
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
	gtk_table_attach(GTK_TABLE(user_table), label, 0,1,2,3, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->organization = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(wizard->organization), tmpl.organization?tmpl.organization:"");
	gtk_table_attach(GTK_TABLE(user_table), wizard->organization, 1,2,2,3, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);
	
	g_signal_connect(G_OBJECT(wizard->email), "changed",
			 G_CALLBACK(wizard_email_changed),
			 wizard);
	return table;
}

#ifdef MAEMO
static void wizard_vol_mount_cb(GnomeVFSVolumeMonitor *vfs, GnomeVFSVolume *vol, WizardWindow *wizard)
{
	gchar *uri = gnome_vfs_volume_get_activation_uri (vol);
	gchar *mount_path = uri?gnome_vfs_get_local_path_from_uri (uri):NULL;
	g_free (uri);
	if (mount_path) {
		if(!strcmp(mount_path, MMC1_PATH)) {
			gtk_widget_set_sensitive(wizard->data_root_mmc1_radiobtn, TRUE);
		}
		if(!strcmp(mount_path, MMC2_PATH)) {
			gtk_widget_set_sensitive(wizard->data_root_mmc2_radiobtn, TRUE);
		}
	}
	g_free(mount_path);
}
static void wizard_vol_unmount_cb(GnomeVFSVolumeMonitor *vfs, GnomeVFSVolume *vol, WizardWindow *wizard)
{
	gchar *uri = gnome_vfs_volume_get_activation_uri (vol);
	gchar *mount_path = uri?gnome_vfs_get_local_path_from_uri (uri):NULL;
	g_free (uri);
	if (mount_path) {
		if(!strcmp(mount_path, MMC1_PATH)) {
			gtk_widget_set_sensitive(wizard->data_root_mmc1_radiobtn, FALSE);
			if (gtk_toggle_button_get_active(
				GTK_TOGGLE_BUTTON(wizard->data_root_mmc1_radiobtn))) {
				gtk_toggle_button_set_active(
					GTK_TOGGLE_BUTTON(wizard->data_root_nokia_radiobtn), TRUE);
			}
		}
		if(!strcmp(mount_path, MMC2_PATH)) {
			gtk_widget_set_sensitive(wizard->data_root_mmc2_radiobtn, FALSE);
			if (gtk_toggle_button_get_active(
				GTK_TOGGLE_BUTTON(wizard->data_root_mmc2_radiobtn))) {
				gtk_toggle_button_set_active(
					GTK_TOGGLE_BUTTON(wizard->data_root_nokia_radiobtn), TRUE);
			}
		}
	}
	g_free(mount_path);
}

void data_root_changed		(GtkToggleButton	*toggle_btn,
				 WizardWindow *wizard)
{
	gchar *name = g_path_get_basename(gtk_entry_get_text(GTK_ENTRY(wizard->mailbox_name)));
	gchar *path = NULL;
	if (gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(wizard->data_root_nokia_radiobtn)))
		gtk_entry_set_text(GTK_ENTRY(wizard->mailbox_name), name);
	else if (gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(wizard->data_root_mmc1_radiobtn))) {
		path = g_strconcat(MMC1_PATH, G_DIR_SEPARATOR_S, 
				  "Claws", G_DIR_SEPARATOR_S, 
				  g_get_user_name(), G_DIR_SEPARATOR_S,
				  name, NULL);
		gtk_entry_set_text(GTK_ENTRY(wizard->mailbox_name), path);
		g_free(path);
	} else if (gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(wizard->data_root_mmc2_radiobtn))) {
		path = g_strconcat(MMC2_PATH, G_DIR_SEPARATOR_S, 
				  "Claws", G_DIR_SEPARATOR_S, 
				  g_get_user_name(), G_DIR_SEPARATOR_S,
				  name, NULL);
		gtk_entry_set_text(GTK_ENTRY(wizard->mailbox_name), path);
		g_free(path);
	}
	g_free(name);
}
#endif

static GtkWidget* mailbox_page (WizardWindow * wizard)
{
	GtkWidget *table = gtk_table_new(1,1, FALSE);
	GtkWidget *vbox;
#ifdef MAEMO
	GtkWidget *vbox2;
	gchar *uri, *mount_path;
#endif
	GtkWidget *hbox;
	CLAWS_TIP_DECL();

	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	vbox = gtk_vbox_new(FALSE, VSPACING_NARROW);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), VSPACING_NARROW_2);

	gtk_table_attach(GTK_TABLE(table), vbox, 0,1,1,2, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);

	wizard->mailbox_label = gtk_label_new(_("<span weight=\"bold\">Mailbox name:</span>"));
	gtk_label_set_use_markup(GTK_LABEL(wizard->mailbox_label), TRUE);
	if (GTK_IS_MISC(wizard->mailbox_label))						      
		gtk_misc_set_alignment(GTK_MISC(wizard->mailbox_label), 1, 0.5);	      
	wizard->mailbox_name = gtk_entry_new();

	gtk_entry_set_text(GTK_ENTRY(wizard->mailbox_name), tmpl.mailbox?tmpl.mailbox:"");

	CLAWS_SET_TIP(wizard->mailbox_name, _("You can also specify an absolute path, for example: "
			       "\"/home/john/Documents/Mail\""));

	gtk_box_pack_start(GTK_BOX(hbox), wizard->mailbox_label, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), wizard->mailbox_name, TRUE, TRUE, 0);

#ifdef MAEMO
	wizard->data_root_nokia_radiobtn = gtk_radio_button_new_with_label(NULL,
		_("on internal memory"));
	wizard->data_root_mmc1_radiobtn = gtk_radio_button_new_with_label_from_widget(
		GTK_RADIO_BUTTON(wizard->data_root_nokia_radiobtn),
		_("on external memory card"));
	wizard->data_root_mmc2_radiobtn = gtk_radio_button_new_with_label_from_widget(
		GTK_RADIO_BUTTON(wizard->data_root_nokia_radiobtn),
		_("on internal memory card"));
		
	g_signal_connect(G_OBJECT(wizard->data_root_nokia_radiobtn), "toggled",
			 G_CALLBACK(data_root_changed), wizard);
	g_signal_connect(G_OBJECT(wizard->data_root_mmc1_radiobtn), "toggled",
			 G_CALLBACK(data_root_changed), wizard);
	g_signal_connect(G_OBJECT(wizard->data_root_mmc2_radiobtn), "toggled",
			 G_CALLBACK(data_root_changed), wizard);

	wizard->volmon = gnome_vfs_get_volume_monitor();
	wizard->vol_mmc1 = gnome_vfs_volume_monitor_get_volume_for_path(wizard->volmon, MMC1_PATH);
	wizard->vol_mmc2 = gnome_vfs_volume_monitor_get_volume_for_path(wizard->volmon, MMC2_PATH);

	uri = gnome_vfs_volume_get_activation_uri (wizard->vol_mmc1);
	mount_path = uri?gnome_vfs_get_local_path_from_uri (uri):NULL;
	g_free(uri);
	if (wizard->vol_mmc1 == NULL || !gnome_vfs_volume_is_mounted(wizard->vol_mmc1)
	    || strcmp(mount_path, MMC1_PATH)) {
		gtk_widget_set_sensitive(wizard->data_root_mmc1_radiobtn, FALSE);
	}
	g_free(mount_path);

	uri = gnome_vfs_volume_get_activation_uri (wizard->vol_mmc2);
	mount_path = uri?gnome_vfs_get_local_path_from_uri (uri):NULL;
	g_free(uri);
	if (wizard->vol_mmc2 == NULL || !gnome_vfs_volume_is_mounted(wizard->vol_mmc2)
	    || strcmp(mount_path, MMC2_PATH)) {
		gtk_widget_set_sensitive(wizard->data_root_mmc2_radiobtn, FALSE);
	} else {
		gtk_toggle_button_set_active(wizard->data_root_mmc2_radiobtn, TRUE);
	}
	g_free(mount_path);
	
	gnome_vfs_volume_unref(wizard->vol_mmc1);
	gnome_vfs_volume_unref(wizard->vol_mmc2);
	wizard->vol_mmc1 = NULL;
	wizard->vol_mmc2 = NULL;

	wizard->volmon_mount_sigid = g_signal_connect(G_OBJECT(wizard->volmon), 
					"volume-mounted", G_CALLBACK(wizard_vol_mount_cb), wizard);
	wizard->volmon_unmount_sigid = g_signal_connect(G_OBJECT(wizard->volmon), 
					"volume-unmounted", G_CALLBACK(wizard_vol_unmount_cb), wizard);

	vbox2 = gtk_vbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox2), wizard->data_root_nokia_radiobtn, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX(vbox2), wizard->data_root_mmc1_radiobtn, FALSE, FALSE, 0);
	gtk_box_pack_start (GTK_BOX(vbox2), wizard->data_root_mmc2_radiobtn, FALSE, FALSE, 0);

	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	PACK_BOX(hbox, _("<span weight=\"bold\">Store data</span>"), vbox2);
#endif

	return table;
}

static void smtp_auth_changed (GtkWidget *btn, gpointer data)
{
	WizardWindow *wizard = (WizardWindow *)data;
	gboolean do_auth = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(wizard->smtp_auth));
	gtk_widget_set_sensitive(wizard->smtp_username, do_auth);
	gtk_widget_set_sensitive(wizard->smtp_username_label, do_auth);
	gtk_widget_set_sensitive(wizard->smtp_password, do_auth);
	gtk_widget_set_sensitive(wizard->smtp_password_label, do_auth);
}

#ifdef USE_GNUTLS
static void cert_browse_cb(GtkWidget *widget, gpointer data)
{
	GtkEntry *dest = GTK_ENTRY(data);
	gchar *filename;
	gchar *utf8_filename;

	filename = filesel_select_file_open(_("Select certificate file"), NULL);
	if (!filename) return;

	utf8_filename = g_filename_to_utf8(filename, -1, NULL, NULL, NULL);
	if (!utf8_filename) {
		g_warning("cert_browse_cb(): failed to convert character set.");
		utf8_filename = g_strdup(filename);
	}
	gtk_entry_set_text(dest, utf8_filename);
	g_free(utf8_filename);
}
#endif

static GtkWidget* smtp_page (WizardWindow * wizard)
{
	GtkWidget *table = gtk_table_new(1, 1, FALSE);
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *hbox_spc;
	GtkWidget *smtp_auth_table;
	GtkWidget *label;
#ifdef USE_GNUTLS
	GtkWidget *button;
	GtkWidget *smtp_cert_table;
#endif
	gchar *text;
	CLAWS_TIP_DECL();
	
	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	vbox = gtk_vbox_new(FALSE, VSPACING_NARROW);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), VSPACING_NARROW_2);

	gtk_table_attach(GTK_TABLE(table), vbox, 0,1,1,2, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	wizard->smtp_server = gtk_entry_new();
	text = get_default_server(wizard, "smtp");
	gtk_entry_set_text(GTK_ENTRY(wizard->smtp_server), text);
	g_free(text);

	CLAWS_SET_TIP(wizard->smtp_server,
			     _("You can specify the port number by appending it at the end: "
			       "\"mail.example.com:25\""));

	PACK_BOX(hbox, _("<span weight=\"bold\">SMTP server address:</span>"),
		 wizard->smtp_server);

	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	wizard->smtp_auth = gtk_check_button_new_with_label(
					_("Use authentication"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wizard->smtp_auth),
			tmpl.smtpauth);
	g_signal_connect(G_OBJECT(wizard->smtp_auth), "toggled",
			 G_CALLBACK(smtp_auth_changed),
			 wizard);
	gtk_box_pack_start(GTK_BOX(hbox), wizard->smtp_auth, FALSE, FALSE, 0);

	label = gtk_label_new(_("<span size=\"small\">(empty to use the same as receive)</span>"));
	gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
	SET_TOGGLE_SENSITIVITY (wizard->smtp_auth, label);	
	gtk_box_pack_end(GTK_BOX(hbox), label, FALSE, FALSE, 0);

	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);
	smtp_auth_table = gtk_table_new(2, 2, FALSE);
	SET_TOGGLE_SENSITIVITY (wizard->smtp_auth, smtp_auth_table);
	gtk_box_pack_start(GTK_BOX(hbox), smtp_auth_table, TRUE, TRUE, 0);

	wizard->smtp_username_label = gtk_label_new(_("SMTP username:"));
	gtk_misc_set_alignment(GTK_MISC(wizard->smtp_username_label), 1, 0.5);	      
	gtk_table_attach(GTK_TABLE(smtp_auth_table), wizard->smtp_username_label, 0,1,0,1, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	text = get_default_smtp_account(wizard);
	wizard->smtp_username = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(wizard->smtp_username), text);
	g_free(text);
	gtk_table_attach(GTK_TABLE(smtp_auth_table), wizard->smtp_username, 1,2,0,1, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	wizard->smtp_password_label = gtk_label_new(_("SMTP password:"));
	gtk_misc_set_alignment(GTK_MISC(wizard->smtp_password_label), 1, 0.5);	      
	gtk_table_attach(GTK_TABLE(smtp_auth_table), wizard->smtp_password_label, 0,1,1,2, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->smtp_password = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(wizard->smtp_password), tmpl.smtppass?tmpl.smtppass:""); 
	gtk_entry_set_visibility(GTK_ENTRY(wizard->smtp_password), FALSE);
#ifdef MAEMO
	hildon_gtk_entry_set_input_mode(GTK_ENTRY(wizard->smtp_password), 
		HILDON_GTK_INPUT_MODE_FULL | HILDON_GTK_INPUT_MODE_INVISIBLE);
#endif
	gtk_table_attach(GTK_TABLE(smtp_auth_table), wizard->smtp_password, 1,2,1,2, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);
#ifdef USE_GNUTLS
	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	wizard->smtp_use_ssl = gtk_check_button_new_with_label(
					_("Use SSL to connect to SMTP server"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wizard->smtp_use_ssl),
			tmpl.smtpssl != 0);
	gtk_box_pack_start(GTK_BOX(hbox), wizard->smtp_use_ssl, FALSE, FALSE, 0);

	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	wizard->smtp_use_tls = gtk_check_button_new_with_label(
					_("Use SSL via STARTTLS"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wizard->smtp_use_tls),
			tmpl.smtpssl == 2);
	gtk_box_pack_start(GTK_BOX(hbox), wizard->smtp_use_tls, FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY (wizard->smtp_use_ssl, wizard->smtp_use_tls);
	
	smtp_cert_table = gtk_table_new(3,3, FALSE);
	gtk_box_pack_start (GTK_BOX(vbox), smtp_cert_table, FALSE, FALSE, 4);
	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);
	label = gtk_label_new(_("Client SSL certificate (optional)"));
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);
	gtk_table_attach(GTK_TABLE(smtp_cert_table), hbox, 0, 3, 0, 1, GTK_FILL, 0, 0, 0);
	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);
	label = gtk_label_new(_("File"));
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
	gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
	gtk_table_attach(GTK_TABLE(smtp_cert_table), hbox, 0, 1, 1, 2, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->smtp_ssl_cert_file = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(wizard->smtp_ssl_cert_file), tmpl.smtpssl_cert?tmpl.smtpssl_cert:"");
	gtk_table_attach(GTK_TABLE(smtp_cert_table), wizard->smtp_ssl_cert_file, 1, 2, 1, 2, GTK_FILL, 0, 0, 0);
	button = gtkut_get_browse_file_btn(_("Browse"));
	gtk_table_attach(GTK_TABLE(smtp_cert_table), button, 2, 3, 1, 2, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	g_signal_connect(G_OBJECT(button), "clicked",
			 G_CALLBACK(cert_browse_cb), wizard->smtp_ssl_cert_file);

	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);	
	label = gtk_label_new(_("Password"));
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
	gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
	gtk_table_attach(GTK_TABLE(smtp_cert_table), hbox, 0, 1, 2, 3, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->smtp_ssl_cert_pass = gtk_entry_new();
	gtk_entry_set_visibility(GTK_ENTRY(wizard->smtp_ssl_cert_pass), FALSE);
	gtk_entry_set_text(GTK_ENTRY(wizard->smtp_ssl_cert_pass), tmpl.smtpssl_cert_pass?tmpl.smtpssl_cert_pass:"");
	gtk_table_attach(GTK_TABLE(smtp_cert_table), wizard->smtp_ssl_cert_pass, 1, 2, 2, 3, GTK_FILL, 0, 0, 0);
	SET_TOGGLE_SENSITIVITY (wizard->smtp_use_ssl, smtp_cert_table);
	wizard->smtp_cert_table = smtp_cert_table;
#endif
	smtp_auth_changed(NULL, wizard);
	return table;
}

static void wizard_protocol_change(WizardWindow *wizard, RecvProtocol protocol)
{
	gchar *text;
	
	if (protocol == A_POP3) {
		text = get_default_server(wizard, "pop");
		gtk_entry_set_text(GTK_ENTRY(wizard->recv_server), text);
		gtk_widget_hide(wizard->recv_imap_label);
		gtk_widget_hide(wizard->recv_imap_subdir);
		gtk_widget_hide(wizard->subsonly_checkbtn);
		gtk_widget_show(wizard->recv_username);
		gtk_widget_show(wizard->recv_password);
		gtk_widget_show(wizard->recv_username_label);
		gtk_widget_show(wizard->recv_password_label);
		gtk_widget_hide(wizard->no_imap_warning);
#ifdef USE_GNUTLS
		gtk_widget_show(wizard->recv_use_ssl);
		gtk_widget_show(wizard->recv_use_tls);
		gtk_widget_show(wizard->recv_cert_table);
#endif
		gtk_label_set_text(GTK_LABEL(wizard->recv_label), _("<span weight=\"bold\">Server address:</span>"));
		gtk_label_set_use_markup(GTK_LABEL(wizard->recv_label), TRUE);
		gtk_dialog_set_response_sensitive (GTK_DIALOG(wizard->window), GO_FORWARD, TRUE);
		g_free(text);
		if (wizard->create_mailbox) {
			gtk_widget_show(wizard->mailbox_label);
			gtk_widget_show(wizard->mailbox_name);
		}
	} else if (protocol == A_IMAP4) {
#ifdef HAVE_LIBETPAN
		text = get_default_server(wizard, "imap");
		gtk_entry_set_text(GTK_ENTRY(wizard->recv_server), text);
		gtk_widget_show(wizard->recv_imap_label);
		gtk_widget_show(wizard->recv_imap_subdir);
		gtk_widget_show(wizard->subsonly_checkbtn);
		gtk_widget_show(wizard->recv_username);
		gtk_widget_show(wizard->recv_password);
		gtk_widget_show(wizard->recv_username_label);
		gtk_widget_show(wizard->recv_password_label);
		gtk_widget_hide(wizard->no_imap_warning);
#ifdef USE_GNUTLS
		gtk_widget_show(wizard->recv_use_ssl);
		gtk_widget_show(wizard->recv_use_tls);
		gtk_widget_show(wizard->recv_cert_table);
#endif
		gtk_label_set_text(GTK_LABEL(wizard->recv_label), _("<span weight=\"bold\">Server address:</span>"));
		gtk_label_set_use_markup(GTK_LABEL(wizard->recv_label), TRUE);
		gtk_dialog_set_response_sensitive (GTK_DIALOG(wizard->window), GO_FORWARD, TRUE);
		g_free(text);
		if (wizard->create_mailbox) {
			gtk_widget_hide(wizard->mailbox_label);
			gtk_widget_hide(wizard->mailbox_name);
		}
#else
		gtk_widget_hide(wizard->recv_imap_label);
		gtk_widget_hide(wizard->recv_imap_subdir);
		gtk_widget_hide(wizard->subsonly_checkbtn);
		gtk_widget_hide(wizard->recv_username);
		gtk_widget_hide(wizard->recv_password);
		gtk_widget_hide(wizard->recv_username_label);
		gtk_widget_hide(wizard->recv_password_label);
		gtk_widget_show(wizard->no_imap_warning);
		if (wizard->create_mailbox) {
			gtk_widget_hide(wizard->mailbox_label);
			gtk_widget_hide(wizard->mailbox_name);
		}
#ifdef USE_GNUTLS
		gtk_widget_hide(wizard->recv_use_ssl);
		gtk_widget_hide(wizard->recv_use_tls);
		gtk_widget_hide(wizard->recv_cert_table);
#endif
		gtk_dialog_set_response_sensitive (GTK_DIALOG(wizard->window), GO_FORWARD, FALSE);
#endif
	} else if (protocol == A_LOCAL) {
		gtk_entry_set_text(GTK_ENTRY(wizard->recv_server), tmpl.mboxfile?tmpl.mboxfile:"");
		gtk_label_set_text(GTK_LABEL(wizard->recv_label), _("<span weight=\"bold\">Local mailbox:</span>"));
		gtk_label_set_use_markup(GTK_LABEL(wizard->recv_label), TRUE);
		gtk_widget_hide(wizard->no_imap_warning);
		gtk_widget_hide(wizard->recv_imap_label);
		gtk_widget_hide(wizard->recv_imap_subdir);
		gtk_widget_hide(wizard->subsonly_checkbtn);
		gtk_widget_hide(wizard->recv_username);
		gtk_widget_hide(wizard->recv_password);
		gtk_widget_hide(wizard->recv_username_label);
		gtk_widget_hide(wizard->recv_password_label);
#ifdef USE_GNUTLS
		gtk_widget_hide(wizard->recv_use_ssl);
		gtk_widget_hide(wizard->recv_use_tls);
		gtk_widget_hide(wizard->recv_cert_table);
#endif
		if (wizard->create_mailbox) {
			gtk_widget_show(wizard->mailbox_label);
			gtk_widget_show(wizard->mailbox_name);
		}
		gtk_dialog_set_response_sensitive (GTK_DIALOG(wizard->window), GO_FORWARD, TRUE);
	}
}

static void wizard_protocol_changed(GtkComboBox *combo, gpointer data)
{
	WizardWindow *wizard = (WizardWindow *)data;
	RecvProtocol protocol = combobox_get_active_data(combo);

	wizard_protocol_change(wizard, protocol);	
}

static GtkWidget* recv_page (WizardWindow * wizard)
{
	GtkWidget *table = gtk_table_new(1,1, FALSE);
	GtkWidget *vbox;
	GtkWidget *hbox;
	GtkWidget *hbox_spc;	
	GtkWidget *recv_table;
	GtkWidget *label;
#ifdef USE_GNUTLS
	GtkWidget *button;
	GtkWidget *recv_cert_table;
#endif
	GtkListStore *store;
	GtkTreeIter iter;
	gchar *text;
	gint index = 0;
	CLAWS_TIP_DECL();

	gtk_table_set_row_spacings(GTK_TABLE(table), 4);
	gtk_table_set_col_spacings(GTK_TABLE(table), 8);

	vbox = gtk_vbox_new(FALSE, VSPACING_NARROW);
	gtk_container_set_border_width(GTK_CONTAINER(vbox), VSPACING_NARROW_2);

	gtk_table_attach(GTK_TABLE(table), vbox, 0,1,1,2, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	recv_table = gtk_table_new(4, 2, FALSE); 
	gtk_box_pack_start(GTK_BOX(vbox), recv_table, FALSE, FALSE, 0);

	label = gtk_label_new(_("<span weight=\"bold\">Server type:</span>"));
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
	gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
	gtk_table_attach(GTK_TABLE(recv_table), label, 0,1,0,1, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->recv_type = gtkut_sc_combobox_create(NULL, FALSE);
	store = GTK_LIST_STORE(gtk_combo_box_get_model(
			GTK_COMBO_BOX(wizard->recv_type)));

	COMBOBOX_ADD(store, _("POP3"), A_POP3);
	COMBOBOX_ADD(store, _("IMAP"), A_IMAP4);
	COMBOBOX_ADD(store, _("Local mbox file"), A_LOCAL);

	switch(tmpl.recvtype) {
	case A_POP3: 
		index = 0;
		break;
	case A_IMAP4:
		index = 1;
		break;
	case A_LOCAL:
		index = 2;
		break;
	default:
		index = 0;
	}
	gtk_combo_box_set_active(GTK_COMBO_BOX (wizard->recv_type), index);
	g_signal_connect(G_OBJECT(wizard->recv_type), "changed",
			 G_CALLBACK(wizard_protocol_changed),
			 wizard);
	gtk_table_attach(GTK_TABLE(recv_table), wizard->recv_type, 1,2,0,1, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	wizard->recv_label = gtk_label_new(_("<span weight=\"bold\">Server address:</span>"));
	gtk_misc_set_alignment(GTK_MISC(wizard->recv_label), 1, 0.5);
	gtk_label_set_use_markup(GTK_LABEL(wizard->recv_label), TRUE);
	gtk_table_attach(GTK_TABLE(recv_table), wizard->recv_label, 0,1,1,2, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->recv_server = gtk_entry_new();
	text = get_default_server(wizard, "pop");
	gtk_entry_set_text(GTK_ENTRY(wizard->recv_server), text);
	g_free(text);
	
	CLAWS_SET_TIP(wizard->recv_server,
			     _("You can specify the port number by appending it at the end: "
			       "\"mail.example.com:110\""));
	gtk_table_attach(GTK_TABLE(recv_table), wizard->recv_server, 1,2,1,2, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);
	
	wizard->recv_username_label = gtk_label_new(_("<span weight=\"bold\">Username:</span>"));
	gtk_misc_set_alignment(GTK_MISC(wizard->recv_username_label), 1, 0.5);
	gtk_label_set_use_markup(GTK_LABEL(wizard->recv_username_label), TRUE);
	gtk_table_attach(GTK_TABLE(recv_table), wizard->recv_username_label, 0,1,2,3, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->recv_username = gtk_entry_new();
	text = get_default_account(wizard);
	gtk_entry_set_text(GTK_ENTRY(wizard->recv_username), text);
	g_free(text);
	gtk_table_attach(GTK_TABLE(recv_table), wizard->recv_username, 1,2,2,3, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);
			 
	wizard->recv_password_label = gtk_label_new(_("Password:"));
	gtk_misc_set_alignment(GTK_MISC(wizard->recv_password_label), 1, 0.5);
	gtk_table_attach(GTK_TABLE(recv_table), wizard->recv_password_label, 0,1,3,4, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->recv_password = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(wizard->recv_password), tmpl.recvpass?tmpl.recvpass:"");
	gtk_entry_set_visibility(GTK_ENTRY(wizard->recv_password), FALSE);
	gtk_table_attach(GTK_TABLE(recv_table), wizard->recv_password, 1,2,3,4, 
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);
#ifdef MAEMO
	hildon_gtk_entry_set_input_mode(GTK_ENTRY(wizard->recv_password), 
		HILDON_GTK_INPUT_MODE_FULL | HILDON_GTK_INPUT_MODE_INVISIBLE);
#endif
	
#ifdef USE_GNUTLS
	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	wizard->recv_use_ssl = gtk_check_button_new_with_label(
					_("Use SSL to connect to receiving server"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wizard->recv_use_ssl),
			tmpl.recvssl != 0);
	gtk_box_pack_start(GTK_BOX(hbox), wizard->recv_use_ssl, FALSE, FALSE, 0);

	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);	
	wizard->recv_use_tls = gtk_check_button_new_with_label(
					_("Use SSL via STARTTLS"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wizard->recv_use_tls),
			tmpl.recvssl == 2);
	gtk_box_pack_start(GTK_BOX(hbox), wizard->recv_use_tls, FALSE, FALSE, 0);
	SET_TOGGLE_SENSITIVITY (wizard->recv_use_ssl, wizard->recv_use_tls);

	recv_cert_table = gtk_table_new(3,3, FALSE);
	gtk_box_pack_start (GTK_BOX(vbox), recv_cert_table, FALSE, FALSE, 4);
	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);
	label = gtk_label_new(_("Client SSL certificate (optional)"));
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0.5);
	gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);	
	gtk_table_attach(GTK_TABLE(recv_cert_table), hbox, 0, 3, 0, 1, GTK_FILL, 0, 0, 0);
	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);
	label = gtk_label_new(_("File"));
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
	gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);	
	gtk_table_attach(GTK_TABLE(recv_cert_table), hbox, 0, 1, 1, 2, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->recv_ssl_cert_file = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(wizard->recv_ssl_cert_file), tmpl.recvssl_cert?tmpl.recvssl_cert:"");
	gtk_table_attach(GTK_TABLE(recv_cert_table), wizard->recv_ssl_cert_file, 1, 2, 1, 2, GTK_FILL, 0, 0, 0);
	button = gtkut_get_browse_file_btn(_("Browse"));
	gtk_table_attach(GTK_TABLE(recv_cert_table), button, 2, 3, 1, 2, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	g_signal_connect(G_OBJECT(button), "clicked",
			 G_CALLBACK(cert_browse_cb), wizard->recv_ssl_cert_file);

	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);
	label = gtk_label_new(_("Password"));
	gtk_misc_set_alignment(GTK_MISC(label), 1, 0.5);
	gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
	gtk_table_attach(GTK_TABLE(recv_cert_table), hbox, 0, 1, 2, 3, 
			 GTK_FILL, 0, VSPACING_NARROW, 0);
	wizard->recv_ssl_cert_pass = gtk_entry_new();
	gtk_entry_set_visibility(GTK_ENTRY(wizard->recv_ssl_cert_pass), FALSE);
	gtk_entry_set_text(GTK_ENTRY(wizard->recv_ssl_cert_pass), tmpl.recvssl_cert_pass?tmpl.recvssl_cert_pass:"");
	gtk_table_attach(GTK_TABLE(recv_cert_table), wizard->recv_ssl_cert_pass, 1, 2, 2, 3, GTK_FILL, 0, 0, 0);
	SET_TOGGLE_SENSITIVITY (wizard->recv_use_ssl, recv_cert_table);	
	wizard->recv_cert_table = recv_cert_table;
#endif	
	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	wizard->recv_imap_subdir = gtk_entry_new();
	gtk_entry_set_text(GTK_ENTRY(wizard->recv_imap_subdir), tmpl.imapdir?tmpl.imapdir:"");
	wizard->recv_imap_label = gtk_label_new(_("IMAP server directory:"));
	gtk_misc_set_alignment(GTK_MISC(wizard->recv_imap_label), 1, 0.5);	      
	gtk_box_pack_start(GTK_BOX(hbox), wizard->recv_imap_label, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), wizard->recv_imap_subdir, TRUE, TRUE, 0);
	
	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	hbox_spc = gtk_hbox_new (FALSE, 0);
	gtk_widget_set_size_request (hbox_spc, 12, -1);
	gtk_box_pack_start (GTK_BOX (hbox), hbox_spc, FALSE, FALSE, 0);
	wizard->subsonly_checkbtn = gtk_check_button_new_with_label(
			_("Show only subscribed folders"));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(wizard->subsonly_checkbtn),
			tmpl.subsonly);
	gtk_box_pack_start(GTK_BOX(hbox), wizard->subsonly_checkbtn, FALSE, FALSE, 0);
	
	hbox = gtk_hbox_new(FALSE, VSPACING_NARROW);
	gtk_box_pack_start (GTK_BOX(vbox), hbox, FALSE, FALSE, 0);
	wizard->no_imap_warning = gtk_label_new(_(
			  "<span weight=\"bold\">Warning: this version of Claws Mail\n"
			  "has been built without IMAP support.</span>"));
	gtk_label_set_use_markup(GTK_LABEL(wizard->no_imap_warning), TRUE);
	gtk_box_pack_start(GTK_BOX(hbox), wizard->no_imap_warning, FALSE, FALSE, 0);

	return table;
}

static void
wizard_response_cb (GtkDialog * dialog, int response, gpointer data)
{
	WizardWindow * wizard = (WizardWindow *)data;
	int current_page, num_pages;
	gboolean skip_mailbox_page = FALSE;
#ifndef MAEMO
	gint protocol = combobox_get_active_data(GTK_COMBO_BOX(wizard->recv_type));

	if (protocol == A_IMAP4) {
		skip_mailbox_page = TRUE;
	}
#endif

	num_pages = g_slist_length(wizard->pages);

 	current_page = gtk_notebook_get_current_page (
				GTK_NOTEBOOK(wizard->notebook));
	if (response == CANCEL)
	{
		wizard->result = FALSE;
		wizard->finished = TRUE;
		gtk_widget_destroy (GTK_WIDGET(dialog));
	}
	else if (response == FINISHED)
	{
		if (!wizard_write_config(wizard)) {
 			current_page = gtk_notebook_get_current_page (
					GTK_NOTEBOOK(wizard->notebook));
			goto set_sens;
		}
		wizard->result = TRUE;
		wizard->finished = TRUE;
		gtk_widget_destroy (GTK_WIDGET(dialog));
	}
	else
	{
		if (response == GO_BACK)
		{
			if (current_page > 0) {
				current_page--;
				if (current_page == MAILBOX_PAGE && skip_mailbox_page) {
					/* mailbox */
					current_page--;
				}
				gtk_notebook_set_current_page (
					GTK_NOTEBOOK(wizard->notebook), 
					current_page);
			}
		}
		else if (response == GO_FORWARD)
		{
			if (current_page < (num_pages-1)) {
				current_page++;
				if (current_page == MAILBOX_PAGE && skip_mailbox_page) {
					/* mailbox */
					current_page++;
				}
				gtk_notebook_set_current_page (
					GTK_NOTEBOOK(wizard->notebook), 
					current_page);
			}
		}
set_sens:
		gtk_dialog_set_response_sensitive (dialog, GO_BACK, 
				current_page > 0);
		gtk_dialog_set_response_sensitive (dialog, GO_FORWARD, 
				current_page < (num_pages - 1));
		if (current_page == (num_pages -1)) {
			gtk_dialog_set_response_sensitive (dialog, FINISHED, TRUE);
			gtk_dialog_set_default_response(GTK_DIALOG(wizard->window), FINISHED);
		} else {
			gtk_dialog_set_response_sensitive (dialog, FINISHED, FALSE);
			gtk_dialog_set_default_response(GTK_DIALOG(wizard->window), GO_FORWARD);
		}

	}
}

static gint wizard_close_cb(GtkWidget *widget, GdkEventAny *event,
				 gpointer data)
{
	WizardWindow *wizard = (WizardWindow *)data;
	wizard->result = FALSE;
	wizard->finished = TRUE;
	
	return FALSE;
}

#define PACK_WARNING(text) {						\
	label = gtk_label_new(text);					\
	gtk_misc_set_alignment(GTK_MISC(label), 0, 0);			\
	gtk_box_pack_end(GTK_BOX(widget), label, FALSE, FALSE, 0);	\
}

gboolean run_wizard(MainWindow *mainwin, gboolean create_mailbox) {
	WizardWindow *wizard = g_new0(WizardWindow, 1);
	GtkWidget *page;
	GtkWidget *widget;
	GtkWidget *label;
	GtkWidget *scrolled_window;
	gchar     *text;
	GSList    *cur;
	gboolean   result;
	gint i = 0;
	wizard->mainwin = mainwin;
	wizard->create_mailbox = create_mailbox;
	
	gtk_widget_hide(mainwin->window);
	
	wizard_read_defaults();
	
	wizard->window = gtk_dialog_new_with_buttons (_("Claws Mail Setup Wizard"),
			NULL, 0, 
			GTK_STOCK_GO_BACK, GO_BACK,
			GTK_STOCK_GO_FORWARD, GO_FORWARD,
			GTK_STOCK_SAVE, FINISHED,
			GTK_STOCK_CANCEL, CANCEL,
			NULL);
	gtk_widget_set_size_request(wizard->window, -1, 480);
	gtk_window_set_position(GTK_WINDOW(wizard->window), GTK_WIN_POS_CENTER);

	g_signal_connect(wizard->window, "response", 
			  G_CALLBACK(wizard_response_cb), wizard);
	gtk_widget_realize(wizard->window);
	gtk_dialog_set_default_response(GTK_DIALOG(wizard->window), 
			GO_FORWARD);
	gtk_dialog_set_response_sensitive(GTK_DIALOG(wizard->window), 
			GO_BACK, FALSE);
	gtk_dialog_set_response_sensitive(GTK_DIALOG(wizard->window), 
			GO_FORWARD, TRUE);
	gtk_dialog_set_response_sensitive(GTK_DIALOG(wizard->window), 
			FINISHED, FALSE);
	gtk_dialog_set_response_sensitive(GTK_DIALOG(wizard->window), 
			CANCEL, TRUE);
	
	wizard->notebook = gtk_notebook_new();
	gtk_notebook_set_show_tabs(GTK_NOTEBOOK(wizard->notebook), FALSE);
	gtk_notebook_set_show_border(GTK_NOTEBOOK(wizard->notebook), FALSE);
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(wizard->window)->vbox), 
			    wizard->notebook, TRUE, TRUE, 0);
	
	wizard->pages = NULL;
	
/*welcome page: 0 */
	WELCOME_PAGE = i;
	page = create_page(wizard, _("Welcome to Claws Mail"));
	
	wizard->pages = g_slist_append(wizard->pages, page);
	widget = stock_pixmap_widget(wizard->window, 
			  	STOCK_PIXMAP_CLAWS_MAIL_LOGO);

	gtk_box_pack_start (GTK_BOX(page), widget, FALSE, FALSE, 0);
	
	text = g_strdup(_("Welcome to the Claws Mail setup wizard.\n\n"
			  "We will begin by defining some basic "
			  "information about you and your most common "
			  "mail options so that you can start to use "
			  "Claws Mail in less than five minutes."));
	widget = gtk_label_new(text);
	gtk_label_set_line_wrap(GTK_LABEL(widget), TRUE);
#ifndef MAEMO
	gtk_box_pack_start (GTK_BOX(page), widget, FALSE, FALSE, 0);
#else
	scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(page), scrolled_window, TRUE, TRUE, 0);

	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window),
 					      widget);
#endif
	g_free(text);

/* user page: 1 */
	i++;
	USER_PAGE = i;
	widget = create_page (wizard, _("About You"));
	scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(widget), scrolled_window, TRUE, TRUE, 0);

	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window),
 					      user_page(wizard));
	PACK_WARNING(_("Bold fields must be completed"));
	
	wizard->pages = g_slist_append(wizard->pages, widget);

/* recv+auth page: 2 */
	i++;
	RECV_PAGE = i;
	widget = create_page (wizard, _("Receiving mail"));
	scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(widget), scrolled_window, TRUE, TRUE, 0);

	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window),
 					      recv_page(wizard));
	PACK_WARNING(_("Bold fields must be completed"));
	
	wizard->pages = g_slist_append(wizard->pages, widget);

/*smtp page: 3 */
	i++;
	SMTP_PAGE = i;
	widget = create_page (wizard, _("Sending mail"));
	scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_box_pack_start(GTK_BOX(widget), scrolled_window, TRUE, TRUE, 0);

	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window),
 					      smtp_page(wizard));
	PACK_WARNING(_("Bold fields must be completed"));
	
	wizard->pages = g_slist_append(wizard->pages, widget);

/* mailbox page: 4 */
	if (create_mailbox) {
		i++;
		MAILBOX_PAGE = i;
		widget = create_page (wizard, _("Saving mail on disk"));
		scrolled_window = gtk_scrolled_window_new (NULL, NULL);
		gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
		gtk_box_pack_start(GTK_BOX(widget), scrolled_window, TRUE, TRUE, 0);

		gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window),
 					      mailbox_page(wizard));
		PACK_WARNING(_("Bold fields must be completed"));
	
		wizard->pages = g_slist_append(wizard->pages, widget);
	}

/* done page: 6 */
	i++;
	DONE_PAGE = i;
	page = create_page(wizard, _("Configuration finished"));
	
	wizard->pages = g_slist_append(wizard->pages, page);
	widget = stock_pixmap_widget(wizard->window, 
			  	STOCK_PIXMAP_CLAWS_MAIL_LOGO);

	gtk_box_pack_start (GTK_BOX(page), widget, FALSE, FALSE, 0);
	
	text = g_strdup(_("Claws Mail is now ready.\n"
			  "Click Save to start."));
	widget = gtk_label_new(text);
	gtk_box_pack_start (GTK_BOX(page), widget, FALSE, FALSE, 0);
	g_free(text);


	for (cur = wizard->pages; cur && cur->data; cur = cur->next) {
		gtk_notebook_append_page (GTK_NOTEBOOK(wizard->notebook), 
					  GTK_WIDGET(cur->data), NULL);
	}
	
	g_signal_connect(G_OBJECT(wizard->window), "delete_event",
			 G_CALLBACK(wizard_close_cb), wizard);
	gtk_widget_show_all (wizard->window);

	gtk_widget_hide(wizard->recv_imap_label);
	gtk_widget_hide(wizard->recv_imap_subdir);
	gtk_widget_hide(wizard->subsonly_checkbtn);

	wizard_protocol_change(wizard, tmpl.recvtype);

	while (!wizard->finished)
		gtk_main_iteration();

	result = wizard->result;
	
	GTK_EVENTS_FLUSH();

	gtk_widget_show(mainwin->window);
	g_free(wizard);

	return result;
}
