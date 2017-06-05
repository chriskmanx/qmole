/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 * Author: Vladimir Nadvornik, Laurent Monin
 * based on logwindow.[ch] from Sylpheed 2.4.7 (C) Hiroyuki Yamamoto
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

#include "main.h"
#include "logwindow.h"

#include "misc.h"
#include "window.h"

#include <gdk/gdkkeysyms.h>


typedef struct _LogWindow LogWindow;

struct _LogWindow
{
	GtkWidget *window;
	GtkWidget *scrolledwin;
	GtkWidget *text;
	
	GdkColor colors[LOG_COUNT];

	guint lines;
};

typedef struct _LogDef LogDef;
struct _LogDef
{
	LogType type;
	const gchar *tag;
	const gchar *color;
};

/* Keep LogType order !! */
static LogDef logdefs[LOG_COUNT] = {
	{ LOG_NORMAL,	"normal", 	"black"	 },
	{ LOG_MSG,	"message",	"blue"	 },
	{ LOG_WARN,	"warning",	"orange" },
	{ LOG_ERROR,	"error",	"red"	 },
};

static LogWindow *logwindow = NULL;

static void hide_cb(GtkWidget *widget, LogWindow *logwin)
{
}

static gboolean key_pressed(GtkWidget *widget, GdkEventKey *event,
			    LogWindow *logwin)
{
	if (event && event->keyval == GDK_Escape)
		gtk_widget_hide(logwin->window);
	return FALSE;
}

static LogWindow *log_window_create(void)
{
	LogWindow *logwin;
	GtkWidget *window;
	GtkWidget *scrolledwin;
	GtkWidget *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;

	logwin = g_new0(LogWindow, 1);

	window = window_new(GTK_WINDOW_TOPLEVEL, "log", NULL, NULL, _("Log"));
	gtk_widget_set_size_request(window, 520, 400);
	g_signal_connect(G_OBJECT(window), "delete_event",
			 G_CALLBACK(gtk_widget_hide_on_delete), NULL);
	g_signal_connect(G_OBJECT(window), "key_press_event",
			 G_CALLBACK(key_pressed), logwin);
	g_signal_connect(G_OBJECT(window), "hide",
			 G_CALLBACK(hide_cb), logwin);
	gtk_widget_realize(window);

	scrolledwin = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolledwin),
				       GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scrolledwin),
					    GTK_SHADOW_IN);
	gtk_container_add(GTK_CONTAINER(window), scrolledwin);
	gtk_widget_show(scrolledwin);

	text = gtk_text_view_new();
	gtk_text_view_set_editable(GTK_TEXT_VIEW(text), FALSE);
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(text), GTK_WRAP_WORD);
	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text));
	gtk_text_buffer_get_start_iter(buffer, &iter);
	gtk_text_buffer_create_mark(buffer, "end", &iter, FALSE);
	gtk_container_add(GTK_CONTAINER(scrolledwin), text);
	gtk_widget_show(text);

	logwin->window = window;
	logwin->scrolledwin = scrolledwin;
	logwin->text = text;
	logwin->lines = 1;

	return logwin;
}

static void log_window_init(LogWindow *logwin)
{
	GtkTextBuffer *buffer;
	GdkColormap *colormap;
	gboolean success[LOG_COUNT];
	gint i;

	g_assert(logwin != NULL);
	g_assert(logwin->colors != NULL);

	for (i = LOG_NORMAL; i < LOG_COUNT; i++)
		{
		gboolean ok = gdk_color_parse(logdefs[i].color, &logwin->colors[i]);
		if (ok == TRUE) continue;

		if (i == LOG_NORMAL) return;
		memcpy(&logwin->colors[i], &logwin->colors[LOG_NORMAL], sizeof(GdkColor));
		}

	colormap = gdk_drawable_get_colormap(logwin->window->window);
	gdk_colormap_alloc_colors(colormap, logwin->colors, LOG_COUNT, FALSE, TRUE, success);

	for (i = LOG_NORMAL; i < LOG_COUNT; i++)
		{
		if (success[i] == FALSE)
			{
			GtkStyle *style;
			gint j;

			g_warning("LogWindow: color allocation failed\n");
			style = gtk_widget_get_style(logwin->window);
			for (j = LOG_NORMAL; j < LOG_COUNT; j++)
				logwin->colors[j] = style->black;
			break;
			}
		}

	buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(logwin->text));
	for (i = LOG_NORMAL; i < LOG_COUNT; i++)
		gtk_text_buffer_create_tag(buffer, logdefs[i].tag,
				   	   "foreground-gdk", &logwin->colors[i],
					   "family", "MonoSpace",
				   	   NULL);
}

static void log_window_show(LogWindow *logwin)
{
	GtkTextView *text = GTK_TEXT_VIEW(logwin->text);
	GtkTextBuffer *buffer;
	GtkTextMark *mark;
	
	g_assert(logwin != NULL);

	buffer = gtk_text_view_get_buffer(text);
	mark = gtk_text_buffer_get_mark(buffer, "end");
	gtk_text_view_scroll_mark_onscreen(text, mark);

	gtk_window_present(GTK_WINDOW(logwin->window));

	log_window_append("", LOG_NORMAL); // to flush memorized lines
}

void log_window_new(void)
{
	if (logwindow == NULL)
		{
		LogWindow *logwin;

		logwin = log_window_create();
		log_window_init(logwin);
		logwindow = logwin;
		}

	log_window_show(logwindow);
}

typedef struct _LogMsg LogMsg;

struct _LogMsg {
	gchar *text;
	LogType type;
};


static void log_window_insert_text(GtkTextBuffer *buffer, GtkTextIter *iter,
				   const gchar *text, const gchar *tag)
{
	gchar *str_utf8;

	if (!text || !*text) return;

	str_utf8 = utf8_validate_or_convert(text);
	gtk_text_buffer_insert_with_tags_by_name(buffer, iter, str_utf8, -1, tag, NULL);
	g_free(str_utf8);
}


void log_window_append(const gchar *str, LogType type)
{
	GtkTextView *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;
	guint line_limit = 1000; //FIXME: option
	static GList *memory = NULL;

	if (logwindow == NULL)
		{
		if (*str) {
			LogMsg *msg = g_new(LogMsg, 1);

			msg->text = g_strdup(str);
			msg->type = type;

			memory = g_list_prepend(memory, msg);

			while (g_list_length(memory) >= line_limit)
				{
				GList *work = g_list_last(memory);
				LogMsg *oldest_msg = work->data;
			
				g_free(oldest_msg->text);
				memory = g_list_delete_link(memory, work);
				}
			}
		return;
		}

	text = GTK_TEXT_VIEW(logwindow->text);
	buffer = gtk_text_view_get_buffer(text);

	if (line_limit > 0 && logwindow->lines >= line_limit)
		{
		GtkTextIter start, end;

		gtk_text_buffer_get_start_iter(buffer, &start);
		end = start;
		gtk_text_iter_forward_lines(&end, logwindow->lines - line_limit);
		gtk_text_buffer_delete(buffer, &start, &end);
		}

	gtk_text_buffer_get_end_iter(buffer, &iter);

	{
	GList *work = g_list_last(memory);

	while (work)
		{
		GList *prev;
		LogMsg *oldest_msg = work->data;
		
		log_window_insert_text(buffer, &iter, oldest_msg->text, logdefs[oldest_msg->type].tag);
		
		prev = work->prev;
		memory = g_list_delete_link(memory, work);
		work = prev;
		}
	}

	log_window_insert_text(buffer, &iter, str, logdefs[type].tag);

#if GTK_CHECK_VERSION(2,20,0)
	if (gtk_widget_get_visible(text))
#else
	if (GTK_WIDGET_VISIBLE(text))
#endif
		{
		GtkTextMark *mark;
		
		mark = gtk_text_buffer_get_mark(buffer, "end");
		gtk_text_view_scroll_mark_onscreen(text, mark);
		}

	logwindow->lines = gtk_text_buffer_get_line_count(buffer);
}
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
