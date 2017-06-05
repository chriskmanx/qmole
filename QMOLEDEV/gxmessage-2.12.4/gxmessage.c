/* gxmessage - an xmessage clone using GTK2
 *
 * Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
 * Timothy Richard Musson
 *
 * Email: Tim Musson <trmusson@gmail.com>
 * WWW:   http://homepages.ihug.co.nz/~trmusson/programs.html#gxmessage
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>


/* Details for Copyright and bug report messages: */
#define AUTHOR  "Timothy Richard Musson"
#define YEAR    "2009"
#define MAILTO  "<trmusson@gmail.com>"


#ifdef ENABLE_NLS
#  include <libintl.h>
#  define _(String) gettext(String)
#  ifdef gettext_noop
#    define N_(String) gettext_noop(String)
#  else
#    define N_(String) (String)
#  endif
#else
#  define _(String) (String)
#  define N_(String) (String)
#  define textdomain(String) (String)
#  define gettext(String) (String)
#  define dgettext(Domain,String) (String)
#  define dcgettext(Domain,String,Type) (String)
#  define bindtextdomain(Domain,Directory) (Domain)
#endif /* ENABLE_NLS */


typedef struct _Button Button;


struct {
	gchar             *message_text;
	gint              message_len;
	Button            *button_list;
	const gchar       *default_str;
	const gchar       *title_str;
	const gchar       *geom_str;
	const gchar       *font_str;
	const gchar       *color_fg;
	const gchar       *color_bg;
	const gchar       *encoding;
	const gchar       *entry_str;
	gint              timeout;
	guint             timeout_id;
	gboolean          do_iconify;
	gboolean          do_print;
	gboolean          do_buttons;
	gboolean          do_borderless;
	gboolean          do_focus;
	GtkWrapMode       wrap_mode;
	GtkWindowPosition window_position;
	GtkWidget         *entry_widget;
	gint              exit_code;
} gx;


struct _Button {
	gboolean      is_default;
	gint          value;
	const gchar   *label;
	Button        *prev;
	Button        *next;
};


struct Option {
	gint     min_len; /* support -bu, -but, -butt, etc., as with xmessage */
	gboolean requires_arg;
	gchar    *opt_str;
};


static struct Option option[] = {
	{2, TRUE,  "buttons"},
	{1, FALSE, "center"},
	{2, TRUE,  "default"},
	{2, TRUE,  "file"},
	{2, FALSE, "nearmouse"},
	{1, FALSE, "print"},
	{3, TRUE,  "timeout"},
	{2, TRUE,  "fn"},
	{2, TRUE,  "font"},
	{1, TRUE,  "geometry"},
	{3, TRUE,  "title"},
	{2, TRUE,  "bg"},
	{2, TRUE,  "fg"},
	{2, TRUE,  "bd"},
	{2, TRUE,  "bw"},
	{1, FALSE, "iconic"},
	{2, TRUE,  "xrm"},
	{2, FALSE, "rv"},
	{2, FALSE, "reverse"},
	{2, TRUE,  "selectionTimeout"},
	{2, FALSE, "synchronous"},
	{2, TRUE,  "xnllanguage"},
	{2, TRUE,  "name"},
	{2, TRUE,  "display"},
	{2, FALSE, "borderless"},
	{1, FALSE, "wrap"},
	{3, TRUE,  "encoding"},
	{3, FALSE, "nofocus"},
	{6, TRUE,  "entrytext"},
	{3, FALSE, "entry"},
	{1, FALSE, "?"},
	{1, FALSE, "help"},
	{1, FALSE, "version"}
};

enum {
	OPT_IS_UNKNOWN = -2,
	OPT_IS_MISSING_ARG,
	OPT_BUTTONS,
	OPT_CENTER,
	OPT_DEFAULT,
	OPT_FILE,
	OPT_NEARMOUSE,
	OPT_PRINT,
	OPT_TIMEOUT,
	OPT_FN,
	OPT_FONT,
	OPT_GEOMETRY,
	OPT_TITLE,
	OPT_BG,
	OPT_FG,
	OPT_BD,
	OPT_BW,
	OPT_ICONIC,
	OPT_XRM,
	OPT_RV,
	OPT_REVERSE,
	OPT_SELECTIONTIMEOUT,
	OPT_SYNCHRONOUS,
	OPT_XNLLANGUAGE,
	OPT_NAME,
	OPT_DISPLAY,
	OPT_BORDERLESS,
	OPT_WRAP,
	OPT_ENCODING,
	OPT_FOCUS,
	OPT_ENTRYTEXT,
	OPT_ENTRY,
	OPT_HELP_Q,
	OPT_HELP,
	OPT_VERSION,
	N_OPTS
};


void prog_cleanup (void);


Button*
button_first (Button *button)
{
	if (button != NULL) {
		while (button->prev != NULL) {
			button = button->prev;
		}
	}
	return button;
}


void
button_free_all (Button *button)
{
	Button *next;

	button = button_first (button);
	while (button != NULL) {
		next = button->next;
		g_free (button);
		button = next;
	}
}


Button*
button_append (Button *button, Button *button_new)
{
	if (button != NULL) {
		button_new->prev = button;
		button->next = button_new;
	}
	return button_new;
}


gint
parse_label_value_pair (gchar *str, gint value, gint *len, gboolean *end)
{
	gchar *colon = NULL;

	*end = FALSE;
	*len = 0;

	while (*str != '\0') {
		if (*str == '\\') {
			/* unescape */
			memmove (str, str + 1, strlen (str));
		}
		else if (*str == ':') {
			/* take note of the last colon found */
			colon = str;
		}
		else if (*str == ',') {
			/* end of pair */
			break;
		}
		str++;
		*len = *len + 1;
	}

	if (*str == '\0') {
		*end = TRUE;
	}
	else {
		*str = '\0';
	}

	if (colon) {
		/* replace default value with value from string */
		*colon = '\0';
		value = atoi (++colon);
	}

	return value;
}


Button*
button_list_from_str (gchar *str)
{
	/* Split "LABEL:VALUE,LABEL:VALUE,..." into a list of buttons */

	Button *button, *blist = NULL;
	gint len, value, default_value = 101;
	gboolean end;

	if (str == NULL) return NULL;

	do {
		value = parse_label_value_pair (str, default_value, &len, &end);
		gx.do_buttons |= len > 0;
		button = g_new0 (Button, 1);
		button->label = str;
		button->value = value;
		blist = button_append (blist, button);
		str = str + len + 1;
		default_value++;
	} while (!end);

	/* return the last item */
	return blist;
}


void
button_set_default (Button *button, const gchar *str)
{
	/* Make button->is_default TRUE for each button whose label matches str */

	if (str == NULL) return;

	button = button_first (button);
	while (button != NULL) {
		button->is_default = strcmp (button->label, str) == 0;
		button = button->next;
	}
}


gint
my_get_opt (const gchar *str, gboolean not_last)
{
	/* Try to identify the command line option in str, returning a unique
	 * option/error code. The not_last variable specifies whether current
	 * option was followed by something else (a value or another option)
	 * on the command line.
	 */

	gint opt, len;

	if (strcmp (str, "+rv") == 0) return OPT_RV;
	if (*str != '-') return OPT_IS_UNKNOWN;

	str++;
	if (*str == '-') str++;
	len = strlen (str);

	if (len > 0) {
		for (opt = 0; opt < N_OPTS; opt++) {
			if (len >= option[opt].min_len &&
			    strncmp (str, option[opt].opt_str, len) == 0) {
				if (!option[opt].requires_arg || not_last) {
					return opt;
				}
				else {
					return OPT_IS_MISSING_ARG;
				}
			}
		}
	}
	return OPT_IS_UNKNOWN;
}


void
cb_window_destroy (GtkWidget *widget, GdkEvent *event, gpointer data)
{
	gtk_main_quit ();
}


gboolean
cb_key_press (GtkWidget *w, GdkEventKey *event, gpointer data)
{
	if (event->keyval == GDK_Escape) {
		gtk_main_quit ();
	}
	return FALSE;
}


void
cb_button_clicked (GtkWidget *widget, gpointer data)
{
	gx.exit_code = ((Button*)data)->value;

	if (gx.do_print) {
		g_print ("%s\n", ((Button*)data)->label);
	}
	else if (gx.entry_str != NULL) {
		g_print ("%s\n", gtk_entry_get_text (GTK_ENTRY(gx.entry_widget)));
	}
	gtk_main_quit ();
}


void
cb_entry_activated (GtkWidget *widget, gpointer data)
{
	gx.exit_code = 0;
	g_print ("%s\n", gtk_entry_get_text (GTK_ENTRY(gx.entry_widget)));
	gtk_main_quit ();
}


gboolean
cb_timeout (gint *timeout)
{
	static gint counter = 0;

	if (++counter >= *timeout) {
		gx.exit_code = 0;
		gtk_main_quit ();
	}
	return TRUE;
}


gint
geometry_get_number (const gchar **geometry)
{
	/* (This function is taken from GNOME's gnome-geometry.c) */

	gint value = 0;
	gint mult  = 1;

	if (**geometry == '-') {
		mult = -1;
		(*geometry)++;
	}
	while ( **geometry && isdigit(**geometry) ) {
		value = value * 10 + (**geometry - '0');
		(*geometry)++;
	}
	return value * mult;
}


void
geometry_parse_string (const gchar *geometry, gint *width, gint *height)
{
	/* Parse geometry string into width and height */

	*width = *height = -1;

	if (*geometry == '=') geometry++;
	if (*geometry == '\0') return;

	*width = geometry_get_number (&geometry);
	if (*width < -1) *width = -1;

	if (*geometry == 'x') {
		geometry++;
		if (*geometry == '\0') return;
		*height = geometry_get_number (&geometry);
		if (*height < -1) *height = -1;
	}
}


void
window_create (void)
{
	GtkWidget  *window;
	GtkWidget  *vbox, *vbox2;
	GtkWidget  *scroller;
	GtkWidget  *btn_box;
	GtkWidget  *btn;
	GtkWidget  *message_widget;
	Button     *button;
	GdkColor   color;
	GtkRequisition size_req;
	GtkTextBuffer *buf;
	GtkTextIter iter;
	gint       win_w, win_h;
	gint       max_w, max_h;


    gtk_window_set_default_icon_name ("gxmessage");

	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

	g_signal_connect (G_OBJECT(window), "destroy",
	                  G_CALLBACK(cb_window_destroy), NULL);

	g_signal_connect (G_OBJECT(window), "key_press_event",
	                  G_CALLBACK(cb_key_press), NULL);

	if (gx.title_str != NULL) {
		gtk_window_set_title (GTK_WINDOW(window), gx.title_str);
	}

	if (gx.do_iconify) {
		gtk_window_iconify (GTK_WINDOW(window));
	}

	if (gx.do_borderless) {
		gtk_window_set_decorated (GTK_WINDOW(window), FALSE);
	}

	gtk_window_set_accept_focus (GTK_WINDOW(window), gx.do_focus);

	/* window contents */
	gtk_container_set_border_width (GTK_CONTAINER(window), 12);

	vbox = gtk_vbox_new (FALSE, 12);
	gtk_container_add (GTK_CONTAINER(window), vbox);

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX(vbox), vbox2, TRUE, TRUE, 0);
	gtk_container_set_border_width (GTK_CONTAINER(vbox2), 0);

	scroller = gtk_scrolled_window_new (NULL, NULL);
	gtk_container_set_border_width (GTK_CONTAINER(scroller), 0);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(scroller), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW(scroller), GTK_SHADOW_ETCHED_IN);
	gtk_scrolled_window_set_placement (GTK_SCROLLED_WINDOW(scroller), GTK_CORNER_TOP_LEFT);
	gtk_box_pack_start (GTK_BOX(vbox2), scroller, TRUE, TRUE, 0);

	/* the message */
	message_widget = gtk_text_view_new ();
	gtk_widget_set_name (message_widget, "gxmessage-textview");
	gtk_text_view_set_editable (GTK_TEXT_VIEW(message_widget), FALSE);
	gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW(message_widget), TRUE);
	gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW(message_widget), gx.wrap_mode);

	if (gx.font_str != NULL) {
		PangoFontDescription *font_desc;
		font_desc = pango_font_description_from_string (gx.font_str);
		gtk_widget_modify_font (message_widget, font_desc);
		pango_font_description_free (font_desc);
	}

	gtk_container_add (GTK_CONTAINER(scroller), message_widget);

	buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW(message_widget));
	gtk_text_buffer_set_text (buf, gx.message_text, strlen (gx.message_text));

	gtk_text_buffer_get_start_iter (buf, &iter);
	gtk_text_buffer_place_cursor (buf, &iter);

	if (gx.color_fg != NULL) {
		if (gdk_color_parse (gx.color_fg, &color)) {
			gtk_widget_modify_text (message_widget, GTK_STATE_NORMAL, &color);
		}
	}
	if (gx.color_bg != NULL) {
		if (gdk_color_parse (gx.color_bg, &color)) {
			gtk_widget_modify_base (message_widget, GTK_STATE_NORMAL, &color);
		}
	}


	/* text entry */
	if (gx.entry_str != NULL) {
		gx.entry_widget = gtk_entry_new ();
		gtk_widget_set_name (gx.entry_widget, "gxmessage-entry");
		gtk_editable_set_editable (GTK_EDITABLE(gx.entry_widget), TRUE);
		gtk_entry_set_text (GTK_ENTRY(gx.entry_widget), gx.entry_str);
		gtk_box_pack_start (GTK_BOX(vbox), gx.entry_widget, FALSE, FALSE, 5);
		gtk_widget_grab_focus (gx.entry_widget);
		if (!gx.do_buttons) {
			/* allow hitting <RETURN> to close the window */
			g_signal_connect (G_OBJECT(gx.entry_widget), "activate",
			                  G_CALLBACK(cb_entry_activated), (gpointer)0);
		}
	}


	/* add buttons */
	if (gx.do_buttons) {

		button = button_first (gx.button_list);

		btn_box = gtk_hbutton_box_new ();
		gtk_button_box_set_layout (GTK_BUTTON_BOX(btn_box),
		                           GTK_BUTTONBOX_END);
		gtk_box_set_spacing (GTK_BOX(btn_box), 6);
		gtk_box_pack_end (GTK_BOX(vbox), btn_box, FALSE, FALSE, 0);

		while (button != NULL) {

			if (strcmp (button->label, "okay") == 0) {
				btn = gtk_button_new_from_stock ("gtk-ok");
			}
			else if (g_str_has_prefix (button->label, "GTK_STOCK_")) {
				gchar *s;
				gchar *p;
				p = g_ascii_strdown (button->label + 10, -1);
				s = p;
				while (*s != '\0') {
					if (*s == '_') *s = '-';
					s++;
				}
				s = g_strconcat ("gtk-", p, NULL);
				btn = gtk_button_new_from_stock (s);
				g_free (s);
				g_free (p);
			}
			else {
				btn = gtk_button_new_with_mnemonic (button->label);
			}

			g_signal_connect (G_OBJECT(btn), "clicked",
			                  G_CALLBACK(cb_button_clicked), (gpointer)button);
			gtk_box_pack_start (GTK_BOX(btn_box), btn, FALSE, FALSE, 0);

			if (button->is_default) {
				gtk_widget_grab_focus (btn);
			}

			button = button->next;
		}
	}


	/* window geometry */

	max_w = gdk_screen_width () * 0.7;
	max_h = gdk_screen_height () * 0.7;

	/* Render dummy text, to get an idea of its size. This is slow when
	 * there's a lot of text, so default to max_w and max_h in that case.
	 */
	if (gx.message_len > 20000) {
		win_w = max_w;
		win_h = max_h;
	}
	else {
		GtkWidget *dummy = gtk_label_new (gx.message_text);
		gtk_widget_modify_font (dummy,
		    gtk_widget_get_style (message_widget)->font_desc);
		gtk_container_add (GTK_CONTAINER(vbox), dummy);
		gtk_widget_size_request (dummy, &size_req);
		gtk_widget_destroy (dummy);
		/* ~50 pixels for borders and scrollbar space */
		win_w = size_req.width + 50;
		win_h = size_req.height + 50;
		if (win_w > max_w) win_w = max_w;
		if (win_h > max_h) win_h = max_h;
	}

	if (gx.entry_str != NULL) {
		gtk_widget_size_request (gx.entry_widget, &size_req);
		win_h = win_h + size_req.height + 12;
	}

	if (gx.do_buttons) {
		gtk_widget_size_request (btn, &size_req);
		win_h = win_h + size_req.height + 12;
	}

	if (gx.geom_str != NULL) {
		gint tmp_w, tmp_h;
		geometry_parse_string (gx.geom_str, &tmp_w, &tmp_h);
		if (tmp_w != -1) win_w = tmp_w;
		if (tmp_h != -1) win_h = tmp_h;
	}

	gtk_window_set_default_size (GTK_WINDOW(window), win_w, win_h);
	gtk_window_set_position (GTK_WINDOW(window), gx.window_position);

	/* open the window */
	gtk_widget_show_all (window);

	/* begin timeout */
	if (gx.timeout != 0) {
		gx.timeout_id = g_timeout_add (1000, (GtkFunction)cb_timeout,
		                               &gx.timeout);
	}
}


gchar*
read_stdin (void)
{
	GString *text;
	gchar *str;
	gint ch;

	text = g_string_new ("");

	while ( (ch = getc (stdin)) != EOF ) {
		g_string_append_c (text, ch);
	}
	str = text->str;
	g_string_free (text, FALSE);
	return str;
}


gchar*
message_to_utf8 (const gchar *str)
{
	gchar *result;
	GError *error = NULL;

	if (gx.encoding == NULL) {
		/* assume message encoding matches current locale */
		result = g_locale_to_utf8 (str, -1, NULL, NULL, NULL);
	}
	else {
		/* use encoding specified on command line */
		result = g_convert_with_fallback (str, -1, "UTF-8", gx.encoding,
		                                  NULL, NULL, NULL, NULL);
	}

	if (result == NULL) {
		/* fall back to ISO-8859-1 as source encoding */
		result = g_convert_with_fallback (str, -1, "UTF-8", "ISO-8859-1",
		                                  NULL, NULL, NULL, &error);
		if (result == NULL) {
			if (error != NULL && error->message != NULL) {
				g_printerr (PACKAGE ": %s\n", error->message);
			}
			prog_cleanup ();
		}
	}
	return result;
}


gboolean
my_gtk_init (gint argc, gchar *argv[])
{
	/* Let gtk_init see --display and --name, but no other options.
	 * Return FALSE if gtk_init fails.
	 */

	gboolean ok;
	gchar *s, **av;
	gint i, len, n = 1;

	av = g_malloc (sizeof(char*) * (argc + 1));
	av[0] = argv[0];

	for (i = 1; i < argc; i++) {
		s = argv[i];
		if (s[0] != '-') continue;
		if (s[1] == '-') s++;
		len = strlen (s);
		if (len > 2 && i + 1 < argc) {
			if (strncmp ("-display", s, len) == 0) {
				av[n++] = "--display";
				av[n++] = argv[++i];
			}
			else if (strncmp ("-name", s, len) == 0) {
				av[n++] = "--name";
				av[n++] = argv[++i];
			}
		}
	}
	av[n] = NULL;
	ok = gtk_init_check (&n, &av);
	g_free (av);
	return ok;
}


void
usage (void)
{
	g_print(_("\n%s - a GTK-based xmessage clone\n"), PACKAGE);
	g_print("\n");
	g_print(_("Usage: %s [OPTIONS] message ...\n"), PACKAGE);
	g_print(_("       %s [OPTIONS] -file FILENAME\n"), PACKAGE);
	g_print("\n");
	g_print(_("xmessage options:\n"));
	g_print(_("  -file FILENAME         Get message text from file, '-' for stdin\n"));
	g_print(_("  -buttons BUTTON_LIST   List of \"LABEL:EXIT_CODE\", comma separated\n"));
	g_print(_("  -default LABEL         Give keyboard focus to the specified button\n"));
	g_print(_("  -print                 Send the selected button's LABEL to stdout\n"));
	g_print(_("  -center                Open the window in the centre of the screen\n"));
	g_print(_("  -nearmouse             Open the window near the mouse pointer\n"));
	g_print(_("  -timeout SECONDS       Exit with code 0 after SECONDS seconds\n"));
	g_print(_("  -display DISPLAY       X display to use\n"));
	g_print(_("  -fn FONT | -font FONT  Set message font (works with GTK font names)\n"));
	g_print(_("  -fg COLOR              Set message font color\n"));
	g_print(_("  -bg COLOR              Set message background color\n"));
	g_print(_("  -geometry GEOMETRY     Set window size (position will be ignored)\n"));
	g_print(_("  -iconic                Start iconified\n"));
	g_print(_("  -name NAME             Program name as used by the window manager\n"));
	g_print(_("  -title TITLE           Set window title to TITLE\n"));
	g_print("\n");
	g_print(_("Additional %s options:\n"), PACKAGE);
	g_print(_("  -borderless            Open the window without border decoration\n"));
	g_print(_("  -nofocus               Don't focus the window when it opens\n"));
	g_print(_("  -encoding CHARSET      Expect CHARSET as the message encoding\n"));
	g_print(_("  -entry                 Prompt for text to be sent to stdout\n"));
	g_print(_("  -entrytext TEXT        Same as -entry, but with TEXT as default text\n"));
	g_print(_("  -wrap                  Wrap lines of text to fit window width\n"));
	g_print(_("  -help | -?             Show this usage information\n"));
	g_print(_("  -version               Show gxmessage version and Copyright details\n"));
	g_print("\n");
	g_print(_("Please report bugs to %s.\n"), MAILTO);
	g_print("\n");

	prog_cleanup ();
}


void
prog_cleanup (void)
{
	button_free_all (gx.button_list);
	if (gx.message_text != NULL) {
		g_free (gx.message_text);
	}
	if (gx.timeout_id != 0) {
		g_source_remove (gx.timeout_id);
	}
	exit (gx.exit_code);
}


int
main (gint argc, gchar *argv[])
{
	GString *gstr = NULL;
	gchar *ch = NULL, *tmpstr;
	const gchar *fname = NULL;
	gint opt, arg = 1;
	gboolean ok;
	gchar bu_default[] = "okay:0";

	/* The default "okay:0" string is intentionally hard-wired, to avoid
	 * breaking scripts that make use of xmessage's -print option.
	 * It must not be changed or gettextize'd. */

#ifdef ENABLE_NLS
	bindtextdomain (PACKAGE, PACKAGE_LOCALE_DIR);
	bind_textdomain_codeset (PACKAGE, "UTF-8");
	textdomain (PACKAGE);
#endif

	ok = my_gtk_init (argc, argv);

	gx.exit_code       = 1;
	gx.do_focus        = TRUE;
	gx.wrap_mode       = GTK_WRAP_NONE;
	gx.window_position = GTK_WIN_POS_NONE;

	while (arg < argc) {
		opt = my_get_opt (argv[arg], arg + 1 < argc);
		switch (opt) {
		case OPT_HELP:
		case OPT_HELP_Q:
			gx.exit_code = 0;
			usage ();
		case OPT_VERSION:
			g_print (PACKAGE "-" VERSION "\n");
			g_print("Copyright (C) %s %s\n"
			  "This is free software. You may redistribute copies of it under " \
			  "the terms of the GNU General Public License <http://www.gnu.org/licenses/gpl.html>.\n" \
			  "There is NO WARRANTY, to the extent permitted by law.\n",
			  YEAR, AUTHOR);
			exit (0);
		case OPT_ENTRY:
		case OPT_ENTRYTEXT:
			if (gx.do_print) {
				g_printerr (_("%s: can't have both -entry and -print\n"), PACKAGE);
				prog_cleanup ();
			}
			if (gx.timeout) {
				/* -entry disables -timeout */
				gx.timeout = 0;
			}
			if (opt == OPT_ENTRY) {
				gx.entry_str = "";
			}
			else {
				gx.entry_str = argv[++arg];
			}
			break;
		case OPT_BUTTONS:
			button_free_all (gx.button_list);
			gx.button_list = button_list_from_str (argv[++arg]);
			break;
		case OPT_CENTER:
			gx.window_position = GTK_WIN_POS_CENTER;
			break;
		case OPT_DEFAULT:
			gx.default_str = argv[++arg];
			break;
		case OPT_FILE:
			if (gstr != NULL) {
				g_printerr (_("%s: can't get message from both -file and command line\n"), PACKAGE);
				prog_cleanup ();
			}
			fname = argv[++arg];
			break;
		case OPT_NEARMOUSE:
			/* -center takes priority over -nearmouse */
			if (gx.window_position != GTK_WIN_POS_CENTER) {
				gx.window_position = GTK_WIN_POS_MOUSE;
			}
			break;
		case OPT_PRINT:
			if (gx.entry_str != NULL) {
				g_printerr (_("%s: can't have both -entry and -print\n"), PACKAGE);
				prog_cleanup ();
			}
			gx.do_print = TRUE;
			break;
		case OPT_TIMEOUT:
			gx.timeout = strtol (argv[++arg], &ch, 10);
			if (*ch) {
				g_printerr (_("%s: integer -timeout value expected\n"), PACKAGE);
				/* continue anyway */
			}
			if (gx.timeout < 0 || gx.entry_str != NULL) {
				/* -entry disables -timeout */
				gx.timeout = 0;
			}
			break;
		case OPT_TITLE:
			gx.title_str = argv[++arg];
			break;
		case OPT_GEOMETRY:
			gx.geom_str = argv[++arg];
			break;
		case OPT_FN:
		case OPT_FONT:
			gx.font_str = argv[++arg];
			break;
		case OPT_RV:
		case OPT_REVERSE:
		case OPT_SYNCHRONOUS:
			/* not implemented - ignore */
			break;
		case OPT_BG:
			gx.color_bg = argv[++arg];
			break;
		case OPT_FG:
			gx.color_fg = argv[++arg];
			break;
		case OPT_NAME:
		case OPT_DISPLAY:
			/* already handled by my_gtk_init - ignore and skip arg */
		case OPT_BD:
		case OPT_BW:
		case OPT_XRM:
		case OPT_SELECTIONTIMEOUT:
		case OPT_XNLLANGUAGE:
			/* not implemented - ignore and skip arg */
			arg++;
			break;
		case OPT_ICONIC:
			gx.do_iconify = TRUE;
			break;
		case OPT_BORDERLESS:
			gx.do_borderless = TRUE;
			break;
		case OPT_WRAP:
			gx.wrap_mode = GTK_WRAP_WORD;
			break;
		case OPT_ENCODING:
			gx.encoding = argv[++arg];
			break;
		case OPT_FOCUS:
			gx.do_focus = FALSE;
			break;
		case OPT_IS_MISSING_ARG:
			/* in this case, xmessage treats the "option" as normal text */
		case OPT_IS_UNKNOWN:
		default:
			if (fname != NULL) {
				g_printerr (_("%s: can't get message from both -file and command line\n"), PACKAGE);
				prog_cleanup ();
			}
			if (gstr == NULL) {
				gstr = g_string_new ("");
			}
			else {
				gstr = g_string_append_c (gstr, ' ');
			}
			gstr = g_string_append (gstr, argv[arg]);
			break;
		}
		arg++;
	}

	if (!ok) {
		g_printerr ("%s: unable to initialize GTK\n", PACKAGE);
		prog_cleanup ();
	}

	if (fname != NULL) {
		if (strcmp ("-", fname) == 0) {
			tmpstr = read_stdin ();
		}
		else if (!g_file_get_contents (fname, &tmpstr, NULL, NULL)) {
			g_printerr (_("%s: unable to read file\n"), PACKAGE);
			prog_cleanup ();
		}
		gx.message_text = message_to_utf8 (tmpstr);
		gx.message_len = strlen (tmpstr);
		g_free (tmpstr);
	}
	else if (gstr != NULL) {
		gx.message_text = message_to_utf8 (gstr->str);
		gx.message_len = gstr->len;
		g_string_free (gstr, TRUE);
	}
	else {
		g_printerr (_("%s: message text is required\n"), PACKAGE);
		g_printerr (_("Try `%s --help' for more information\n"), PACKAGE);
		prog_cleanup ();
	}

	if (gx.button_list == NULL) {
		gx.button_list = button_list_from_str (bu_default);
	}

	button_set_default (gx.button_list, gx.default_str);

	window_create ();
	gtk_main ();

	prog_cleanup ();
	return 0;
}

