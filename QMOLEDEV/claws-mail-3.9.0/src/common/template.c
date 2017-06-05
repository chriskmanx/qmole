/*
 * Sylpheed templates subsystem 
 * Copyright (C) 2001 Alexander Barinov
 * Copyright (C) 2001-2012 Hiroyuki Yamamoto and the Claws Mail team
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
#include <glib/gi18n.h>
#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>

#include "utils.h"
#include "template.h"
#include "../codeconv.h"

static GSList *template_list;

static Template *template_load(gchar *filename)
{
	Template *tmpl;
	FILE *fp;
	gchar buf[BUFFSIZE];
	gint bytes_read;

	if ((fp = g_fopen(filename, "rb")) == NULL) {
		FILE_OP_ERROR(filename, "fopen");
		return NULL;
	}

	tmpl = g_new(Template, 1);
	tmpl->load_filename = g_strdup(filename);;
	tmpl->name = NULL;
	tmpl->subject = NULL;
	tmpl->from = NULL;
	tmpl->to = NULL;
	tmpl->cc = NULL;	
	tmpl->bcc = NULL;	
	tmpl->value = NULL;

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		if (buf[0] == '\n')
			break;
		else if (!g_ascii_strncasecmp(buf, "Name:", 5))
			tmpl->name = g_strdup(g_strstrip(buf + 5));
		else if (!g_ascii_strncasecmp(buf, "From:", 5))
			tmpl->from = g_strdup(g_strstrip(buf + 5));
		else if (!g_ascii_strncasecmp(buf, "To:", 3))
			tmpl->to = g_strdup(g_strstrip(buf + 3));
		else if (!g_ascii_strncasecmp(buf, "Cc:", 3))
			tmpl->cc = g_strdup(g_strstrip(buf + 3));
		else if (!g_ascii_strncasecmp(buf, "Bcc:", 4))
			tmpl->bcc = g_strdup(g_strstrip(buf + 4));						
		else if (!g_ascii_strncasecmp(buf, "Subject:", 8))
			tmpl->subject = g_strdup(g_strstrip(buf + 8));
	}

	if (!tmpl->name) {
		g_warning("wrong template format\n");
		template_free(tmpl);
		fclose(fp);
		return NULL;
	}

	if ((bytes_read = fread(buf, 1, sizeof(buf), fp)) == 0) {
		if (ferror(fp)) {
			FILE_OP_ERROR(filename, "fread");
			template_free(tmpl);
			fclose(fp);
			return NULL;
		}
	}
	fclose(fp);
	tmpl->value = g_strndup(buf, bytes_read);

	return tmpl;
}

void template_free(Template *tmpl)
{
	g_free(tmpl->load_filename);
	g_free(tmpl->name);
	g_free(tmpl->subject);
	g_free(tmpl->from);
	g_free(tmpl->to);
	g_free(tmpl->cc);
	g_free(tmpl->bcc);		
	g_free(tmpl->value);
	g_free(tmpl);
}

static void template_clear_config(GSList *tmpl_list)
{
	GSList *cur;
	Template *tmpl;

	for (cur = tmpl_list; cur != NULL; cur = cur->next) {
		tmpl = (Template *)cur->data;
		template_free(tmpl);
	}
	g_slist_free(tmpl_list);
}

static gint tmpl_compare(gconstpointer tmpl1, gconstpointer tmpl2)
{
	gchar *basename1, *basename2;
	long filenum1, filenum2;
	gint ret = 0;

	if ((Template *)tmpl1 == NULL || (Template *)tmpl2 == NULL)
		return 0;

	if (((Template *)tmpl1)->load_filename == NULL || ((Template *)tmpl2)->load_filename == NULL)
		return 0;

	basename1 = g_path_get_basename(((Template *)tmpl1)->load_filename);
	basename2 = g_path_get_basename(((Template *)tmpl2)->load_filename);
	filenum1 = atol(basename1);
	filenum2 = atol(basename2);
	g_free(basename1);
	g_free(basename2);

	if (filenum1 == 0 || filenum2 == 0)
		return 0;

	if (filenum1 < filenum2)
		ret = -1;
	else
		if (filenum1 > filenum2)
			ret = 1;
			
	return ret;
}

GSList *template_read_config(void)
{
	const gchar *path;
	gchar *filename;
	GDir *dir;
	const gchar *dir_name;
	struct stat s;
	Template *tmpl;
	GSList *tmpl_list = NULL;

	path = get_template_dir();
	debug_print("%s:%d reading templates dir %s\n",
		    __FILE__, __LINE__, path);

	if (!is_dir_exist(path)) {
		if (make_dir(path) < 0)
			return NULL;
	}

	if ((dir = g_dir_open(path, 0, NULL)) == NULL) {
		g_warning("failed to open directory: %s\n", path);
		return NULL;
	}

	while ((dir_name = g_dir_read_name(dir)) != NULL) {
		filename = g_strconcat(path, G_DIR_SEPARATOR_S,
				       dir_name, NULL);

		if (g_stat(filename, &s) != 0 || !S_ISREG(s.st_mode) ) {
			debug_print("%s:%d %s is not an ordinary file\n",
				    __FILE__, __LINE__, filename);
			continue;
		}

		tmpl = template_load(filename);
		if (tmpl)
			tmpl_list = g_slist_insert_sorted(tmpl_list, tmpl, tmpl_compare);

		g_free(filename);
	}

	g_dir_close(dir);

	return tmpl_list;
}

#define TRY(func) { \
if (!(func)) \
{ \
	g_warning("Failed to write template to file\n"); \
	if (fp) fclose(fp); \
	if (new) claws_unlink(new); \
	g_free(new); \
	g_free(filename); \
	return; \
} \
}

#define TRY_NO_CLOSE(func) { \
if (!(func)) \
{ \
	g_warning("Failed to write template to file\n"); \
	if (new) claws_unlink(new); \
	g_free(new); \
	g_free(filename); \
	return; \
} \
}

static void template_write_config(GSList *tmpl_list)
{
	const gchar *path;
	GSList *cur;
	Template *tmpl;
	FILE *fp;
	gint tmpl_num;

	debug_print("%s:%d writing templates\n", __FILE__, __LINE__);

	path = get_template_dir();

	if (!is_dir_exist(path)) {
		if (is_file_exist(path)) {
			g_warning("file %s already exists\n", path);
			return;
		}
		if (make_dir(path) < 0)
			return;
	}

	for (cur = tmpl_list, tmpl_num = 1; cur != NULL;
	     cur = cur->next, tmpl_num++) {
		gchar *filename, *new = NULL;

		tmpl = cur->data;

		filename = g_strconcat(path, G_DIR_SEPARATOR_S,
				       itos(tmpl_num), NULL);

		if (is_file_exist(filename)) {
			new = g_strconcat(filename, ".new", NULL);
		}

		if ((fp = g_fopen(new?new:filename, "wb")) == NULL) {
			FILE_OP_ERROR(new?new:filename, "fopen");
			g_free(new);
			g_free(filename);
			return;
		}

		TRY(fprintf(fp, "Name: %s\n", tmpl->name) > 0);
		if (tmpl->subject && *tmpl->subject != '\0')
			TRY(fprintf(fp, "Subject: %s\n", tmpl->subject) > 0);
		if (tmpl->from && *tmpl->from != '\0')
			TRY(fprintf(fp, "From: %s\n", tmpl->from) > 0);
		if (tmpl->to && *tmpl->to != '\0')
			TRY(fprintf(fp, "To: %s\n", tmpl->to) > 0);
		if (tmpl->cc && *tmpl->cc != '\0')
			TRY(fprintf(fp, "Cc: %s\n", tmpl->cc) > 0);
		if (tmpl->bcc && *tmpl->bcc != '\0')
			TRY(fprintf(fp, "Bcc: %s\n", tmpl->bcc) > 0);

		TRY(fputs("\n", fp) != EOF);

		if (tmpl->value && *tmpl->value != '\0') {
			TRY(fwrite(tmpl->value, sizeof(gchar), strlen(tmpl->value), fp) == strlen(tmpl->value));
		} else {
			TRY(fwrite("", sizeof(gchar), 1, fp) == 1);
		}
		TRY_NO_CLOSE(fclose(fp) != EOF);

		if (new) {
			claws_unlink(filename);
			rename_force(new, filename);
		}
		g_free(new);
		g_free(filename);
	}
	
	/* remove other templates */
	while (TRUE) {
		gchar *filename = g_strconcat(path, G_DIR_SEPARATOR_S,
				       itos(tmpl_num), NULL);
		if (is_file_exist(filename)) {
			debug_print("removing old template %d\n", tmpl_num);
			claws_unlink(filename);
			g_free(filename);
		} else {
			g_free(filename);
			break;
		}
		tmpl_num++;
	}
}

GSList *template_get_config(void)
{
	if (!template_list)
		template_list = template_read_config();

	return template_list;
}

void template_set_config(GSList *tmpl_list)
{
	template_clear_config(template_list);
	template_write_config(tmpl_list);
	template_list = tmpl_list;
}
