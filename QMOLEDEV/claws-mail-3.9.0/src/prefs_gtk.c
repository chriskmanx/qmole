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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#define _GNU_SOURCE
#include <stdio.h>

#include <glib.h>
#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "defs.h"
#include "main.h"
#include "prefs.h"
#include "prefs_gtk.h"
#include "prefs_common.h"
#include "utils.h"
#include "gtkutils.h"
#include "passcrypt.h"
#include "base64.h"
#include "codeconv.h"

#define CL(x)	(((gulong) (x) >> (gulong) 8) & 0xFFUL)
#define RGB_FROM_GDK_COLOR(c) \
	((CL(c.red)   << (gulong) 16) | \
	 (CL(c.green) << (gulong)  8) | \
	 (CL(c.blue)))

#ifdef HAVE_FGETS_UNLOCKED
#define SC_FGETS fgets_unlocked
#else
#define SC_FGETS fgets
#endif

typedef enum
{
	DUMMY_PARAM
} DummyEnum;

static GHashTable *whole_cache = NULL;

static gboolean prefs_read_config_from_cache(PrefParam *param, const gchar *label,
			       const gchar *rcfile);

static void prefs_config_parse_one_line(PrefParam	*param,
					 const gchar	*buf);

void prefs_read_config(PrefParam *param, const gchar *label,
		       const gchar *rcfile, const gchar *encoding)
{
	FILE *fp;
	gchar buf[PREFSBUFSIZE];
	gchar *block_label;

	cm_return_if_fail(param != NULL);
	cm_return_if_fail(label != NULL);
	cm_return_if_fail(rcfile != NULL);

	if (encoding != NULL)
		g_warning("Encoding is ignored\n");

	debug_print("Reading configuration...\n");

	prefs_set_default(param);

	if (whole_cache != NULL) {
		if (prefs_read_config_from_cache(param, label, rcfile) == TRUE)
			return;
	}

	if ((fp = g_fopen(rcfile, "rb")) == NULL) {
		if (ENOENT != errno) FILE_OP_ERROR(rcfile, "fopen");
		return;
	}

	block_label = g_strdup_printf("[%s]", label);

#ifdef HAVE_FGETS_UNLOCKED
	flockfile(fp);
#endif

	/* search aiming block */
	while (SC_FGETS(buf, sizeof(buf), fp) != NULL) {
		gint val;

		if (encoding) {
			gchar *conv_str;

			conv_str = conv_codeset_strdup
				(buf, encoding, CS_INTERNAL);
			if (!conv_str)
				conv_str = g_strdup(buf);
			val = strncmp
				(conv_str, block_label, strlen(block_label));
			g_free(conv_str);
		} else
			val = strncmp(buf, block_label, strlen(block_label));
		if (val == 0) {
			debug_print("Found %s\n", block_label);
			break;
		}
	}
	g_free(block_label);

	while (SC_FGETS(buf, sizeof(buf), fp) != NULL) {
		strretchomp(buf);
		/* reached next block */
		if (buf[0] == '[') break;
		if (buf[0] == '#') continue;

		if (encoding) {
			gchar *conv_str;

			conv_str = conv_codeset_strdup
				(buf, encoding, CS_INTERNAL);
			if (!conv_str)
				conv_str = g_strdup(buf);
			prefs_config_parse_one_line(param, conv_str);
			g_free(conv_str);
		} else
			prefs_config_parse_one_line(param, buf);
	}

	debug_print("Finished reading configuration.\n");
#ifdef HAVE_FGETS_UNLOCKED
	funlockfile(fp);
#endif
	fclose(fp);
}

static void prefs_config_parse_one_line(PrefParam *param, const gchar *buf)
{
	gint i;
	gint name_len;
	const gchar *value;
	GdkColor color;

	for (i = 0; param[i].name != NULL; i++) {
		name_len = strlen(param[i].name);
		if (g_ascii_strncasecmp(buf, param[i].name, name_len))
			continue;
		if (buf[name_len] != '=')
			continue;
		value = buf + name_len + 1;
		/* debug_print("%s = %s\n", param[i].name, value); */

		switch (param[i].type) {
		case P_STRING:
		{
			gchar *tmp = NULL;

			if (*value) {
				if (g_utf8_validate(value, -1, NULL))
					tmp = g_strdup(value);
				else {
					tmp = conv_codeset_strdup(value,
						    conv_get_locale_charset_str_no_utf8(),
						    CS_INTERNAL);
				}
			} else {
				tmp = g_strdup("");
			}
			if (!tmp) {
				g_warning("Failed to convert character set.");
				tmp = g_strdup(value);
			}
			g_free(*((gchar **)param[i].data));
			*((gchar **)param[i].data) = tmp;
			break;
		}
		case P_INT:
			*((gint *)param[i].data) =
				(gint)atoi(value);
			break;
		case P_BOOL:
			*((gboolean *)param[i].data) =
				(*value == '0' || *value == '\0')
					? FALSE : TRUE;
			break;
		case P_ENUM:
			*((DummyEnum *)param[i].data) =
				(DummyEnum)atoi(value);
			break;
		case P_USHORT:
			*((gushort *)param[i].data) =
				(gushort)atoi(value);
			break;
		case P_COLOR:
			if (gdk_color_parse(value, &color)) {
				*((gulong *)param[i].data) = RGB_FROM_GDK_COLOR(color); 
			}
			else 
				/* be compatible and accept ints */
				*((gulong *)param[i].data) = strtoul(value, 0, 10); 
			break;
		case P_PASSWORD:
			g_free(*((gchar **)param[i].data));
			if (value[0] == '!') {
				gchar tmp[1024];
				gint len;

				len = base64_decode(tmp, &value[1], strlen(value) - 1);
				passcrypt_decrypt(tmp, len);
				tmp[len] = '\0';
				*((gchar **)param[i].data) =
					*tmp ? g_strdup(tmp) : NULL;
			} else {
				*((gchar **)param[i].data) =
					*value ? g_strdup(value) : NULL;
			}
			break;
		default:
			break;
		}
	}
}

#define TRY(func) \
if (!(func)) \
{ \
	g_warning("Failed to write configuration to file\n"); \
	if (orig_fp) fclose(orig_fp); \
	prefs_file_close_revert(pfile); \
	g_free(rcpath); \
	g_free(block_label); \
	return; \
} \

void prefs_write_config(PrefParam *param, const gchar *label,
		        const gchar *rcfile)
{
	FILE *orig_fp;
	PrefFile *pfile;
	gchar *rcpath;
	gchar buf[PREFSBUFSIZE];
	gchar *block_label = NULL;
	gboolean block_matched = FALSE;

	cm_return_if_fail(param != NULL);
	cm_return_if_fail(label != NULL);
	cm_return_if_fail(rcfile != NULL);

	rcpath = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, rcfile, NULL);
	if ((orig_fp = g_fopen(rcpath, "rb")) == NULL) {
		if (ENOENT != errno) FILE_OP_ERROR(rcpath, "fopen");
	}

	if ((pfile = prefs_write_open(rcpath)) == NULL) {
		g_warning("Failed to write configuration to file\n");
		if (orig_fp) fclose(orig_fp);
		g_free(rcpath);
		return;
	}

	block_label = g_strdup_printf("[%s]", label);

	/* search aiming block */
	if (orig_fp) {
		while (fgets(buf, sizeof(buf), orig_fp) != NULL) {
			gint val;

			val = strncmp(buf, block_label, strlen(block_label));
			if (val == 0) {
				debug_print("Found %s\n", block_label);
				block_matched = TRUE;
				break;
			} else
				TRY(fputs(buf, pfile->fp) != EOF);
		}
	}

	TRY(fprintf(pfile->fp, "%s\n", block_label) > 0);

	/* write all param data to file */
	TRY(prefs_write_param(param, pfile->fp) == 0);

	if (block_matched) {
		gboolean in_dup_block = FALSE;
		while (fgets(buf, sizeof(buf), orig_fp) != NULL) {
			/* next block */
			if (buf[0] == '[') {
				TRY(fputc('\n', pfile->fp) != EOF &&
				    fputs(buf, pfile->fp)  != EOF);
				break;
			}
		}
		while (fgets(buf, sizeof(buf), orig_fp) != NULL) {
			if (buf[0] == '[') {
				if (!strncmp(buf, block_label,
						strlen(block_label)))
					in_dup_block = TRUE;
				else
					in_dup_block = FALSE;
			}
			if (!in_dup_block)
				TRY(fputs(buf, pfile->fp) != EOF);
		}
	}

	g_free(block_label);
	block_label = NULL;

	if (orig_fp) fclose(orig_fp);
	if (prefs_file_close(pfile) < 0)
		g_warning("Failed to write configuration to file\n");
	g_free(rcpath);

	debug_print("Configuration is saved.\n");
}

gint prefs_write_param(PrefParam *param, FILE *fp)
{
	gint i;
	gchar buf[PREFSBUFSIZE];

	for (i = 0; param[i].name != NULL; i++) {
		switch (param[i].type) {
		case P_STRING:
		{
			gchar *tmp = NULL;

			if (*((gchar **)param[i].data)) {
				if (g_utf8_validate(*((gchar **)param[i].data), -1, NULL))
					tmp = g_strdup(*((gchar **)param[i].data));
				else {
					tmp = conv_codeset_strdup(*((gchar **)param[i].data),
						conv_get_locale_charset_str_no_utf8(),
						CS_INTERNAL);
					if (!tmp)
						tmp = g_strdup(*((gchar **)param[i].data));
				}
			}

			g_snprintf(buf, sizeof(buf), "%s=%s\n", param[i].name,
				   tmp ? tmp : "");

			g_free(tmp);
			break;
		}
		case P_INT:
			g_snprintf(buf, sizeof(buf), "%s=%d\n", param[i].name,
				   *((gint *)param[i].data));
			break;
		case P_BOOL:
			g_snprintf(buf, sizeof(buf), "%s=%d\n", param[i].name,
				   *((gboolean *)param[i].data));
			break;
		case P_ENUM:
			g_snprintf(buf, sizeof(buf), "%s=%d\n", param[i].name,
				   *((DummyEnum *)param[i].data));
			break;
		case P_USHORT:
			g_snprintf(buf, sizeof(buf), "%s=%d\n", param[i].name,
				   *((gushort *)param[i].data));
			break;
		case P_COLOR:
			g_snprintf(buf, sizeof buf,  "%s=#%6.6lx\n", param[i].name,
				   *((gulong *) param[i].data));
			break;
		case P_PASSWORD:
			{
				gchar *tmp = NULL, tmp2[1024] = {0};

				tmp = *((gchar **)param[i].data);
				if (tmp) {
					gint len;

					tmp = g_strdup(tmp);
					len = strlen(tmp);
					passcrypt_encrypt(tmp, len);
					base64_encode(tmp2, tmp, len);
					g_free(tmp);
					tmp = tmp2;
				}
				g_snprintf(buf, sizeof(buf), "%s=!%s\n", param[i].name,
					   tmp ?
					   tmp : "");
			}
			break;
		default:
			/* unrecognized, fail */
			debug_print("Unrecognized parameter type\n");
			return -1;
		}

		if (buf[0] != '\0') {
			if (fputs(buf, fp) == EOF) {
				perror("fputs");
				return -1;
			}
		}
	}

	return 0;
}

void prefs_set_default(PrefParam *param)
{
	gint i;
	GdkColor color;

	cm_return_if_fail(param != NULL);

	for (i = 0; param[i].name != NULL; i++) {
		if (!param[i].data) continue;

		switch (param[i].type) {
		case P_STRING:
			g_free(*((gchar **)param[i].data));
			if (param[i].defval != NULL) {
				if (!strncasecmp(param[i].defval, "ENV_", 4)) {
					const gchar *envstr;
					gchar *tmp;

					envstr = g_getenv(param[i].defval + 4);
					tmp = envstr && *envstr ?
						conv_codeset_strdup(envstr,
								    conv_get_locale_charset_str(),
								    CS_INTERNAL)
						: g_strdup("");
					if (!tmp) {
						g_warning("Failed to convert character set.");
						tmp = g_strdup(envstr);
					}
					*((gchar **)param[i].data) = tmp;
				} else if (param[i].defval[0] == '~')
					*((gchar **)param[i].data) =
						g_strconcat(get_home_dir(),
							    param[i].defval + 1,
							    NULL);
				else if (param[i].defval[0] != '\0')
					*((gchar **)param[i].data) =
						g_strdup(param[i].defval);
				else
					*((gchar **)param[i].data) = NULL;
			} else
				*((gchar **)param[i].data) = NULL;
			break;
		case P_PASSWORD:
			g_free(*((gchar **)param[i].data));
			if (param[i].defval != NULL) {
				if (param[i].defval[0] != '\0')
					*((gchar **)param[i].data) =
						g_strdup(param[i].defval);
				else
					*((gchar **)param[i].data) = NULL;
			} else
				*((gchar **)param[i].data) = NULL;
			break;
		case P_INT:
			if (param[i].defval != NULL)
				*((gint *)param[i].data) =
					(gint)atoi(param[i].defval);
			else
				*((gint *)param[i].data) = 0;
			break;
		case P_BOOL:
			if (param[i].defval != NULL) {
				if (!g_ascii_strcasecmp(param[i].defval, "TRUE"))
					*((gboolean *)param[i].data) = TRUE;
				else
					*((gboolean *)param[i].data) =
						atoi(param[i].defval) ? TRUE : FALSE;
			} else
				*((gboolean *)param[i].data) = FALSE;
			break;
		case P_ENUM:
			if (param[i].defval != NULL)
				*((DummyEnum*)param[i].data) =
					(DummyEnum)atoi(param[i].defval);
			else
				*((DummyEnum *)param[i].data) = 0;
			break;
		case P_USHORT:
			if (param[i].defval != NULL)
				*((gushort *)param[i].data) =
					(gushort)atoi(param[i].defval);
			else
				*((gushort *)param[i].data) = 0;
			break;
		case P_COLOR:
			if (param[i].defval != NULL && gdk_color_parse(param[i].defval, &color))
				*((gulong *)param[i].data) =
					RGB_FROM_GDK_COLOR(color);
			else if (param[i].defval)
				/* be compatible and accept ints */
				*((gulong *)param[i].data) = strtoul(param[i].defval, 0, 10); 
			else
				*((gulong *)param[i].data) = 0; 
			break;
		default:
			break;
		}
	}
}

void prefs_free(PrefParam *param)
{
	gint i;

	cm_return_if_fail(param != NULL);

	for (i = 0; param[i].name != NULL; i++) {
		if (!param[i].data) continue;

		switch (param[i].type) {
		case P_STRING:
		case P_PASSWORD:
			g_free(*((gchar **)param[i].data));
			break;
		default:
			break;
		}
	}
}

void prefs_button_toggled(GtkToggleButton *toggle_btn, GtkWidget *widget)
{
	gboolean is_active;

	is_active = gtk_toggle_button_get_active(toggle_btn);
	gtk_widget_set_sensitive(widget, is_active);
}

void prefs_button_toggled_reverse(GtkToggleButton *toggle_btn, GtkWidget *widget)
{
	gboolean is_active;

	is_active = gtk_toggle_button_get_active(toggle_btn);
	gtk_widget_set_sensitive(widget, !is_active);
}

void prefs_set_dialog(PrefParam *param)
{
	gint i;

	for (i = 0; param[i].name != NULL; i++) {
		if (param[i].widget_set_func)
			param[i].widget_set_func(&param[i]);
	}
}

void prefs_set_data_from_dialog(PrefParam *param)
{
	gint i;

	for (i = 0; param[i].name != NULL; i++) {
		if (param[i].data_set_func)
			param[i].data_set_func(&param[i]);
	}
}

void prefs_set_dialog_to_default(PrefParam *param)
{
	gint	   i;
	PrefParam  tmpparam;
	gchar	  *str_data = NULL;
	gint	   int_data;
	gushort    ushort_data;
	gboolean   bool_data;
	DummyEnum  enum_data;

	for (i = 0; param[i].name != NULL; i++) {
		if (!param[i].widget_set_func) continue;

		tmpparam = param[i];

		switch (tmpparam.type) {
		case P_STRING:
			if (tmpparam.defval) {
				if (!g_ascii_strncasecmp(tmpparam.defval, "ENV_", 4)) {
					str_data = g_strdup(g_getenv(param[i].defval + 4));
					tmpparam.data = &str_data;
					break;
				} else if (tmpparam.defval[0] == '~') {
					str_data =
						g_strconcat(get_home_dir(),
							    param[i].defval + 1,
							    NULL);
					tmpparam.data = &str_data;
					break;
				}
			}
			tmpparam.data = &tmpparam.defval;
			break;
		case P_PASSWORD:
			tmpparam.data = &tmpparam.defval;
			break;
		case P_INT:
			if (tmpparam.defval)
				int_data = atoi(tmpparam.defval);
			else
				int_data = 0;
			tmpparam.data = &int_data;
			break;
		case P_USHORT:
			if (tmpparam.defval)
				ushort_data = atoi(tmpparam.defval);
			else
				ushort_data = 0;
			tmpparam.data = &ushort_data;
			break;
		case P_BOOL:
			if (tmpparam.defval) {
				if (!g_ascii_strcasecmp(tmpparam.defval, "TRUE"))
					bool_data = TRUE;
				else
					bool_data = atoi(tmpparam.defval)
						? TRUE : FALSE;
			} else
				bool_data = FALSE;
			tmpparam.data = &bool_data;
			break;
		case P_ENUM:
			if (tmpparam.defval)
				enum_data = (DummyEnum)atoi(tmpparam.defval);
			else
				enum_data = 0;
			tmpparam.data = &enum_data;
			break;
		case P_OTHER:
		default:
			break;
		}
		tmpparam.widget_set_func(&tmpparam);
		g_free(str_data);
		str_data = NULL;
	}
}

void prefs_set_data_from_entry(PrefParam *pparam)
{
	gchar **str;
	const gchar *entry_str;

	cm_return_if_fail(*pparam->widget != NULL);

	entry_str = gtk_entry_get_text(GTK_ENTRY(*pparam->widget));

	switch (pparam->type) {
	case P_STRING:
	case P_PASSWORD:
		str = (gchar **)pparam->data;
		g_free(*str);
		*str = entry_str[0] ? g_strdup(entry_str) : NULL;
		break;
	case P_USHORT:
		*((gushort *)pparam->data) = atoi(entry_str);
		break;
	case P_INT:
		*((gint *)pparam->data) = atoi(entry_str);
		break;
	default:
		g_warning("Invalid PrefType for GtkEntry widget: %d\n",
			  pparam->type);
	}
}

void prefs_set_escaped_data_from_entry(PrefParam *pparam)
{
	gchar **str;

	cm_return_if_fail(*pparam->widget != NULL);

	switch (pparam->type) {
	case P_STRING:
		str = (gchar **)pparam->data;
		g_free(*str);
		*str = pref_get_pref_from_entry(GTK_ENTRY(*pparam->widget));
		break;
	default:
		g_warning("Invalid escaped PrefType for GtkEntry widget: %d\n",
			  pparam->type);
	}
}

void prefs_set_entry(PrefParam *pparam)
{
	gchar **str;

	cm_return_if_fail(*pparam->widget != NULL);

	switch (pparam->type) {
	case P_STRING:
	case P_PASSWORD:
		str = (gchar **)pparam->data;
		gtk_entry_set_text(GTK_ENTRY(*pparam->widget),
				   *str ? *str : "");
		break;
	case P_INT:
		gtk_entry_set_text(GTK_ENTRY(*pparam->widget),
				   itos(*((gint *)pparam->data)));
		break;
	case P_USHORT:
		gtk_entry_set_text(GTK_ENTRY(*pparam->widget),
				   itos(*((gushort *)pparam->data)));
		break;
	default:
		g_warning("Invalid PrefType for GtkEntry widget: %d\n",
			  pparam->type);
	}
}

void prefs_set_entry_from_escaped(PrefParam *pparam)
{
	gchar **str;

	cm_return_if_fail(*pparam->widget != NULL);

	switch (pparam->type) {
	case P_STRING:
		str = (gchar **)pparam->data;
		pref_set_entry_from_pref(GTK_ENTRY(*pparam->widget),
				   *str ? *str : "");
		break;
	default:
		g_warning("Invalid escaped PrefType for GtkEntry widget: %d\n",
			  pparam->type);
	}
}

void prefs_set_data_from_text(PrefParam *pparam)
{
	gchar **str;
	gchar *text = NULL, *tp = NULL;
	gchar *tmp, *tmpp;

	cm_return_if_fail(*pparam->widget != NULL);

	switch (pparam->type) {
	case P_STRING:
	case P_PASSWORD:
		str = (gchar **)pparam->data;
		g_free(*str);
		if (GTK_IS_EDITABLE(*pparam->widget)) {   /* need? */
			tp = text = gtk_editable_get_chars
					(GTK_EDITABLE(*pparam->widget), 0, -1);
		} else if (GTK_IS_TEXT_VIEW(*pparam->widget)) {
			GtkTextView *textview = GTK_TEXT_VIEW(*pparam->widget);
			GtkTextBuffer *buffer = gtk_text_view_get_buffer(textview);
			GtkTextIter start, end;
			gtk_text_buffer_get_start_iter(buffer, &start);
			gtk_text_buffer_get_iter_at_offset(buffer, &end, -1);
			tp = text = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);
		}

		cm_return_if_fail (tp && text);

		if (text[0] == '\0') {
			*str = NULL;
			g_free(text);
			break;
		}

		Xalloca(tmpp = tmp, strlen(text) * 2 + 1,
			{ *str = NULL; break; });
		while (*tp) {
			if (*tp == '\n') {
				*tmpp++ = '\\';
				*tmpp++ = 'n';
				tp++;
			} else
				*tmpp++ = *tp++;
		}
		*tmpp = '\0';
		*str = g_strdup(tmp);
		g_free(text);
		break;
	default:
		g_warning("Invalid PrefType for GtkText widget: %d\n",
			  pparam->type);
	}
}

void prefs_set_escaped_data_from_text(PrefParam *pparam)
{
	gchar **str;

	cm_return_if_fail(*pparam->widget != NULL);

	switch (pparam->type) {
	case P_STRING:
		str = (gchar **)pparam->data;
		g_free(*str);
		*str = pref_get_pref_from_textview(GTK_TEXT_VIEW(*pparam->widget));
		break;
	default:
		g_warning("Invalid escaped PrefType for GtkText widget: %d\n",
			  pparam->type);
	}
}

void prefs_set_text(PrefParam *pparam)
{
	gchar *buf, *sp, *bufp;
	gchar **str;
	GtkTextView *text;
	GtkTextBuffer *buffer;
	GtkTextIter iter;

	cm_return_if_fail(*pparam->widget != NULL);

	switch (pparam->type) {
	case P_STRING:
	case P_PASSWORD:
		str = (gchar **)pparam->data;
		if (*str) {
			bufp = buf = alloca(strlen(*str) + 1);
			if (!buf) buf = "";
			else {
				sp = *str;
				while (*sp) {
					if (*sp == '\\' && *(sp + 1) == 'n') {
						*bufp++ = '\n';
						sp += 2;
					} else
						*bufp++ = *sp++;
				}
				*bufp = '\0';
			}
		} else
			buf = "";

		text = GTK_TEXT_VIEW(*pparam->widget);
		buffer = gtk_text_view_get_buffer(text);
		gtk_text_buffer_set_text(buffer, "", -1);
		gtk_text_buffer_get_start_iter(buffer, &iter);
		gtk_text_buffer_insert(buffer, &iter, buf, -1);
		break;
	default:
		g_warning("Invalid PrefType for GtkTextView widget: %d\n",
			  pparam->type);
	}
}

void prefs_set_text_from_escaped(PrefParam *pparam)
{
	gchar **str;

	cm_return_if_fail(*pparam->widget != NULL);

	switch (pparam->type) {
	case P_STRING:
		str = (gchar **)pparam->data;
		pref_set_textview_from_pref(GTK_TEXT_VIEW(*pparam->widget),
				 *str ? *str : "");
		break;
	default:
		g_warning("Invalid escaped PrefType for GtkTextView widget: %d\n",
			  pparam->type);
	}
}

void prefs_set_data_from_toggle(PrefParam *pparam)
{
	cm_return_if_fail(pparam->type == P_BOOL);
	cm_return_if_fail(*pparam->widget != NULL);
	
	*((gboolean *)pparam->data) =
		gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(*pparam->widget));
}

void prefs_set_toggle(PrefParam *pparam)
{
	cm_return_if_fail(pparam->type == P_BOOL);
	cm_return_if_fail(*pparam->widget != NULL);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(*pparam->widget),
				     *((gboolean *)pparam->data));
}

void prefs_set_data_from_spinbtn(PrefParam *pparam)
{
	cm_return_if_fail(*pparam->widget != NULL);

	switch (pparam->type) {
	case P_INT:
		*((gint *)pparam->data) =
			gtk_spin_button_get_value_as_int
			(GTK_SPIN_BUTTON(*pparam->widget));
		break;
	case P_USHORT:
		*((gushort *)pparam->data) =
			(gushort)gtk_spin_button_get_value_as_int
			(GTK_SPIN_BUTTON(*pparam->widget));
		break;
	default:
		g_warning("Invalid PrefType for GtkSpinButton widget: %d\n",
			  pparam->type);
	}
}

void prefs_set_spinbtn(PrefParam *pparam)
{
	cm_return_if_fail(*pparam->widget != NULL);

	switch (pparam->type) {
	case P_INT:
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(*pparam->widget),
					  (gfloat)*((gint *)pparam->data));
		break;
	case P_USHORT:
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(*pparam->widget),
					  (gfloat)*((gushort *)pparam->data));
		break;
	default:
		g_warning("Invalid PrefType for GtkSpinButton widget: %d\n",
			  pparam->type);
	}
}

static GSList *prefs_pages = NULL;

void prefs_gtk_open(void)
{
	prefswindow_open(_("Preferences"), prefs_pages, NULL,
			&prefs_common.prefswin_width, &prefs_common.prefswin_height,
			NULL, NULL);
}

void prefs_gtk_register_page(PrefsPage *page)
{
	prefs_pages = g_slist_append(prefs_pages, page);
}

void prefs_gtk_unregister_page(PrefsPage *page)
{
	prefs_pages = g_slist_remove(prefs_pages, page);
}

static void prefs_destroy_whole_cache(gpointer to_free)
{	
	GHashTable *table = (GHashTable *)to_free;
	g_hash_table_destroy(table);
}

static void prefs_destroy_file_cache(gpointer to_free)
{	
	GHashTable *table = (GHashTable *)to_free;
	g_hash_table_destroy(table);
}

static int prefs_cache_sections(GHashTable *file_cache, const gchar *rcfile)
{
	FILE *fp = NULL;
	gchar buf[PREFSBUFSIZE];
	GHashTable *section_cache = NULL;

	if (rcfile)
		fp = g_fopen(rcfile, "rb");
	if (!fp) {
		debug_print("cache: %s: %s\n", rcfile?rcfile:"(null)", strerror(errno));
		return -1;
	}
	
#ifdef HAVE_FGETS_UNLOCKED
	flockfile(fp);
#endif
	
	while (SC_FGETS(buf, sizeof(buf), fp) != NULL) {
		strretchomp(buf);
		if (buf[0] == '\0')
			continue;
		if (buf[0] == '#')
			continue; /* comment */
		if (buf[0] == '[') { /* new section */
			gchar *blockname = g_strdup(buf+1);

			if (strrchr(blockname, ']'))
				*strrchr(blockname, ']') = '\0';

			if ((section_cache = g_hash_table_lookup(file_cache, blockname)) == NULL) {
				debug_print("new section '%s'\n", blockname);
				section_cache = g_hash_table_new_full(g_str_hash, g_str_equal,
						g_free, NULL);
				g_hash_table_insert(file_cache, 
					blockname, section_cache);
			} else {
				debug_print("section '%s' already done\n", blockname);
				g_free(blockname);
				section_cache = NULL;
				continue;
			}
		} else {
			if (!section_cache) {
				debug_print("skipping stuff %s with no section\n", buf);
				continue;
			} else {
				gchar *pref;
				
				if (!strchr(buf, '=')) {
					/* plugins do differently */
					continue;
				}
				pref = g_strdup(buf);
				
				//debug_print("new pref '%s'\n", pref);
				g_hash_table_insert(section_cache, pref, GINT_TO_POINTER(1));
			}
		}
	}
#ifdef HAVE_FGETS_UNLOCKED
	funlockfile(fp);
#endif
	fclose(fp);
	return 0;
}

static int prefs_cache(const gchar *rcfile)
{
	GHashTable *file_cache = g_hash_table_new_full(g_str_hash, g_str_equal, 
					g_free, prefs_destroy_file_cache);
	
	debug_print("new file '%s'\n", rcfile?rcfile:"(null)");
	g_hash_table_insert(whole_cache, g_strdup(rcfile), file_cache);
	
	return prefs_cache_sections(file_cache, rcfile);
}

void prefs_prepare_cache(void)
{
	gchar *clawsrc = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, COMMON_RC, NULL);
	gchar *folderitemrc = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, FOLDERITEM_RC, NULL);
	gchar *accountrc = g_strconcat(get_rc_dir(), G_DIR_SEPARATOR_S, ACCOUNT_RC, NULL);
	
	if (whole_cache == NULL) {
		whole_cache = g_hash_table_new_full(g_str_hash, g_str_equal,
				g_free, prefs_destroy_whole_cache);
	} else {
		debug_print("already cached\n");
		g_free(clawsrc);
		g_free(folderitemrc);
		g_free(accountrc);
		return;
	}
	if (prefs_cache(clawsrc) < 0 ||
	    prefs_cache(folderitemrc) < 0 ||
	    prefs_cache(accountrc) < 0)
		prefs_destroy_cache();

	g_free(clawsrc);
	g_free(folderitemrc);
	g_free(accountrc);
}

void prefs_destroy_cache(void)
{
	if (!whole_cache) {
		debug_print("no cache\n");
		return;
	}
	debug_print("destroying cache\n");
	g_hash_table_destroy(whole_cache);
	whole_cache = NULL;
	return;
}

static void prefs_parse_cache(gpointer key, gpointer value, gpointer user_data)
{
	gchar *pref = (gchar *)key;

	PrefParam *param = (PrefParam *)user_data;
	
	prefs_config_parse_one_line(param, pref);
}

static gboolean prefs_read_config_from_cache(PrefParam *param, const gchar *label,
			       const gchar *rcfile) 
{
	GHashTable *sections_table = NULL;
	GHashTable *values_table = NULL;
	sections_table = g_hash_table_lookup(whole_cache, rcfile);
	
	if (sections_table == NULL) {
		g_warning("Can't find %s in the whole cache\n", rcfile?rcfile:"(null)");
		return FALSE;
	}
	values_table = g_hash_table_lookup(sections_table, label);
	
	if (values_table == NULL) {
		debug_print("no '%s' section in '%s' cache\n", label?label:"(null)", rcfile?rcfile:"(null)");
		return TRUE;
	}
	g_hash_table_foreach(values_table, prefs_parse_cache, param);
	return TRUE;
}
