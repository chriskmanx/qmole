/*
 * Geeqie
 * Copyright (C) 2008 - 2012 The Geeqie Team
 *
 */

#include <glib/gprintf.h>

#include "lirc.h"

#include "misc.h"

#ifdef HAVE_LIRC
#include <lirc/lirc_client.h>
#include "layout_image.h"

gint lirc_fd = -1;
struct lirc_config *config = NULL;
guint input_tag = 0; /* event source id */
GIOChannel *gio_chan;

/*
 *-----------------------------------------------------------------------------
 * LIRC callback
 *-----------------------------------------------------------------------------
 */

static void lirc_cleanup(void)
{
	if (config)
		{
		g_source_remove(input_tag);
		lirc_freeconfig(config);
		config = NULL;
		}
	if (lirc_fd != -1)
		{
		lirc_deinit();
		lirc_fd = -1;
		}
	if (gio_chan)
		{
		g_io_channel_shutdown(gio_chan, TRUE, NULL);
		g_io_channel_unref(gio_chan);
		}
}

static gboolean lirc_input_callback(GIOChannel *source, GIOCondition condition,
				    gpointer data)
{
	LayoutWindow *lw = data;
	gchar *ptr;
	gint ret;
	gint x = 0;
	gint y = 0;
	
	/* LIRC code and corresponding geeqie command (and parameters)*/
	gchar *code;
	gchar *cmd;

	/* parameters for geeqie command */
	gint i_parm;
	gfloat fl_parm;

	while ((ret = lirc_nextcode(&code)) == 0 && code)
		{
		while ((ret = lirc_code2char(config, code, &cmd)) == 0 && cmd)
			{
			if (g_ascii_strncasecmp("LEFT", cmd, 4) == 0)
				{
				ptr = cmd + 4;
				while (g_ascii_isspace(*ptr)) ptr++;
				i_parm = atoi(ptr);
				
				if (i_parm <= 0) i_parm = 1;
				x -= i_parm;
				}
			else if (g_ascii_strncasecmp("RIGHT", cmd, 5) == 0)
				{
				ptr = cmd + 5;
				while (g_ascii_isspace(*ptr)) ptr++;
				i_parm = atoi(ptr);
				
				if (i_parm <= 0) i_parm = 1;
				x += i_parm;
				}
			else if (g_ascii_strncasecmp("UP", cmd, 2) == 0)
				{
				ptr = cmd + 2;
				while (g_ascii_isspace(*ptr)) ptr++;
				i_parm = atoi(ptr);
				
				if (i_parm <= 0) i_parm = 1;
				y -= i_parm;
				}
			else if (g_ascii_strncasecmp("DOWN", cmd, 4) == 0)
				{
				ptr = cmd + 4;
				while (g_ascii_isspace(*ptr)) ptr++;
				i_parm = atoi(ptr);
				
				if (i_parm <= 0) i_parm = 1;
				y += i_parm;
				}
			else if (g_ascii_strcasecmp("PREV", cmd) == 0)
				{
				layout_image_prev(lw);
				}
			else if (g_ascii_strcasecmp("NEXT", cmd) == 0)
				{
				layout_image_next(lw);
				}
			else if (g_ascii_strncasecmp("ZOOM_IN", cmd, 7) == 0)
				{
				ptr = cmd + 7;
				while (g_ascii_isspace(*ptr)) ptr++;
				fl_parm = atoi(ptr) / 10.0;
				
				if (fl_parm <= 0.01) fl_parm = get_zoom_increment();
				layout_image_zoom_adjust(lw, fl_parm, FALSE);
				}
			else if (g_ascii_strncasecmp("ZOOM_OUT", cmd, 8) == 0)
				{
				ptr = cmd + 8;
				while (g_ascii_isspace(*ptr)) ptr++;
				fl_parm = atoi(ptr) / 10.0;
				
				if (fl_parm <= 0.01) fl_parm = get_zoom_increment();
				layout_image_zoom_adjust(lw, -fl_parm, FALSE);
				}
			else if (g_ascii_strcasecmp("ZOOM_MAX", cmd) == 0)
				{
				layout_image_zoom_set(lw, 0.0, FALSE);
				}
			else if (g_ascii_strcasecmp("FULL_SCREEN", cmd) == 0)
				{
				layout_image_full_screen_toggle(lw);
				}
			else if (g_ascii_strncasecmp("SET_ZOOM", cmd, 8) == 0)
				{
				ptr = cmd + 8;
				while (g_ascii_isspace(*ptr)) ptr++;
				i_parm = atoi(ptr);
				
				if (i_parm <= 0) i_parm = 1;
				layout_image_zoom_set(lw, 1.0, FALSE);
				}
			else if (g_ascii_strncasecmp("SET_INV_ZOOM", cmd, 12) == 0)
				{
				ptr = cmd + 12;
				while (g_ascii_isspace(*ptr)) ptr++;
				i_parm = atoi(ptr);
				
				if (i_parm <= 0) i_parm = 1;
				layout_image_zoom_set(lw, -i_parm, FALSE);
				}
			else if (g_ascii_strcasecmp("FIRST", cmd) == 0)
				{
				layout_image_first(lw);
				}
			else if (g_ascii_strcasecmp("LAST", cmd) == 0)
				{
				layout_image_last(lw);
				}
			else if (g_ascii_strcasecmp("PAUSE", cmd) == 0)
				{
				layout_image_slideshow_pause_toggle(lw);
				}
			else if (g_ascii_strcasecmp("ROTATE_90", cmd) == 0)
				{
				layout_image_alter_orientation(lw, ALTER_ROTATE_90);
				}
			else if (g_ascii_strcasecmp("ROTATE_90_CC", cmd) == 0)
				{
				layout_image_alter_orientation(lw, ALTER_ROTATE_90_CC);
				}
			else if (g_ascii_strcasecmp("INFO", cmd) == 0)
				{
				layout_image_overlay_toggle(lw);
				}
			else if (g_ascii_strcasecmp("EXIT", cmd) == 0)
				{
				exit_program();
				}
			}
		free(code);
		if (ret == -1) break;
		}
	if (x != 0 || y != 0)
		{
		layout_image_scroll(lw, x, y, FALSE);
		}

	if (ret == -1)
		{
		/* something went badly wrong */
		g_fprintf(stderr, _("disconnected from LIRC\n"));
		lirc_cleanup();
		return (gboolean)FALSE;
		}
	return (gboolean)TRUE;
}

void layout_image_lirc_init(LayoutWindow *lw)
{
	gint flags;
	
	DEBUG_1("Initializing LIRC...");
	lirc_fd = lirc_init(GQ_APPNAME_LC, get_debug_level() > 0);
	if (lirc_fd == -1)
		{
		g_fprintf(stderr, _("Could not init LIRC support\n"));
		return;
		}
	if (lirc_readconfig(NULL, &config, NULL) == -1)
		{
		lirc_deinit();
		g_fprintf(stderr,
			_("could not read LIRC config file\n"
			"please read the documentation of LIRC to \n"
			"know how to create a proper config file\n"));
		return;
		}
	gio_chan = g_io_channel_unix_new(lirc_fd);
	input_tag = g_io_add_watch(gio_chan, G_IO_IN,
				   lirc_input_callback, lw);
	fcntl(lirc_fd, F_SETOWN, getpid());
	flags = fcntl(lirc_fd, F_GETFL, 0);
	if (flags != -1) fcntl(lirc_fd, F_SETFL, flags|O_NONBLOCK);
	fflush(stderr);
}

#endif /* HAVE_LIRC */
/* vim: set shiftwidth=8 softtabstop=0 cindent cinoptions={1s: */
