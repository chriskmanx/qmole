/*
 * gsf-infile-tar.c :
 *
 * Copyright (C) 2008 Morten Welinder (terra@gnome.org)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 * TODO:
 *   symlinks
 *   hardlinks
 *   weird headers
 */

#include <gsf-config.h>
#include <gsf/gsf-infile-impl.h>
#include <gsf/gsf-infile-tar.h>
#include <gsf/gsf-impl-utils.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-input-proxy.h>

#include <string.h>

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "libgsf:tar"

static void gsf_infile_tar_set_source (GsfInfileTar *tar, GsfInput *src);

enum {
	PROP_0,
	PROP_SOURCE
};

static GObjectClass *parent_class;

typedef struct {
	char *name;

	/* The location of data.  */
	gsf_off_t offset;
	gsf_off_t length;

	/* The directory object, or NULL for a data file.  */
	GsfInfileTar *dir;
} TarChild;

/* tar header from POSIX 1003.1-1990.  */
typedef struct {
	char name[100];               /*   0 */
	char mode[8];                 /* 100 (octal) */
	char uid[8];                  /* 108 (octal) */
	char gid[8];                  /* 116 (octal) */
	char size[12];                /* 124 (octal) */
	char mtime[12];               /* 136 (octal) */
	char chksum[8];               /* 148 (octal) */
	char typeflag;                /* 156 */
	char linkname[100];           /* 157 */
	char magic[6];                /* 257 */
	char version[2];              /* 263 */
	char uname[32];               /* 265 */
	char gname[32];               /* 297 */
	char devmajor[8];             /* 329 (octal) */
	char devminor[8];             /* 337 (octal) */
	char prefix[155];             /* 345 */
	char filler[12];              /* 500 */
} TarHeader;

#define HEADER_SIZE (sizeof (TarHeader))
#define BLOCK_SIZE 512
#define MAGIC_LONGNAME "././@LongLink"

struct _GsfInfileTar {
	GsfInfile parent;

	GsfInput *source;
	GArray *children;  /* of TarChild */
	GError *err;
};

typedef struct {
	GsfInfileClass  parent_class;
} GsfInfileTarClass;

#define GSF_INFILE_TAR_CLASS(k)    (G_TYPE_CHECK_CLASS_CAST ((k), GSF_INFILE_TAR_TYPE, GsfInfileTarClass))
#define GSF_IS_INFILE_TAR_CLASS(k) (G_TYPE_CHECK_CLASS_TYPE ((k), GSF_INFILE_TAR_TYPE))

static gsf_off_t
unpack_octal (GsfInfileTar *tar, const char *s, size_t len)
{
	gsf_off_t res = 0;

	while (len--) {
		unsigned char c = *s++;
		if (c == 0)
			break;
		if (c < '0' || c > '7') {
			tar->err = g_error_new (gsf_input_error_id (), 0,
						"Invalid tar header");
			return 0;
		}
		res = (res << 3) + (c - '0');
	}

	return res;
}


static GsfInfileTar *
tar_create_dir (GsfInfileTar *dir, const char *name)
{
	TarChild c;

	c.offset = 0;
	c.length = 0;
	c.name = g_strdup (name);
	c.dir = g_object_new (GSF_INFILE_TAR_TYPE, NULL);

	/*
	 * We set the source here, so gsf_infile_tar_constructor doesn't
	 * start reading the tarfile recursively.
	 */
	gsf_infile_tar_set_source (c.dir, dir->source);

	gsf_input_set_name (GSF_INPUT (c.dir), name);

	g_array_append_val (dir->children, c);

	return c.dir;
}

static GsfInfileTar *
tar_directory_for_file (GsfInfileTar *dir, const char *name, gboolean last)
{
	const char *s = name;

	while (1) {
		const char *s0 = s;
		char *dirname;

		/* Find a directory component, if any.  */
		while (1) {
			if (*s == 0) {
				if (last && s != s0)
					break;
				else
					return dir;
			}
			/* This is deliberately slash-only.  */
			if (*s == '/')
				break;
			s++;
		}

		dirname = g_strndup (s0, s - s0);
		while (*s == '/')
			s++;

		if (strcmp (dirname, ".") != 0) {
			GsfInput *subdir =
				gsf_infile_child_by_name (GSF_INFILE (dir),
							  dirname);
			if (subdir) {
				/* Undo the ref. */
				g_object_unref (subdir);
				dir = GSF_INFILE_TAR (subdir);
			} else
				dir = tar_create_dir (dir, dirname);
		}

		g_free (dirname);
	}
}


/*****************************************************************************/

/**
 * tar_init_info :
 * @tar : #GsfInfileTar
 *
 * Read tar headers and do some sanity checking
 * along the way.
 **/
static void
tar_init_info (GsfInfileTar *tar)
{
	TarHeader end;
	const TarHeader *header;
	gsf_off_t pos0 = gsf_input_tell (tar->source);
	char *pending_longname = NULL;

	memset (&end, 0, sizeof (end));

	while (tar->err == NULL &&
	       (header = (const TarHeader *)gsf_input_read (tar->source,
							    HEADER_SIZE,
							    NULL))) {
		char *name;
		gsf_off_t length;
		gsf_off_t offset;

		if (memcmp (header->filler, end.filler, sizeof (end.filler))) {
			tar->err = g_error_new (gsf_input_error_id (), 0,
						"Invalid tar header");
			break;
		}

		if (memcmp (header, &end, HEADER_SIZE) == 0)
			break;

		if (pending_longname) {
			name = pending_longname;
			pending_longname = NULL;
		} else
			name = g_strndup (header->name, sizeof (header->name));
		length = unpack_octal (tar, header->size, sizeof (header->size));
		offset = gsf_input_tell (tar->source);

#if 0
		g_printerr ("[%s]: %d\n", name, (int)length);
#endif

		switch (header->typeflag) {
		case '0': case 0: {
			/* Regular file. */
			GsfInfileTar *dir;
			const char *n = name, *s;
			TarChild c;

			/* This is deliberately slash-only.  */
			while ((s = strchr (n, '/')))
				n = s + 1;
			c.name = g_strdup (n);
			c.offset = offset;
			c.length = length;
			c.dir = NULL;
			dir = tar_directory_for_file (tar, name, FALSE);
			g_array_append_val (dir->children, c);
			break;
		}
		case '5': {
			/* Directory */
			(void)tar_directory_for_file (tar, name, TRUE);
			break;
		}
		case 'L': {
			const char *n;

			if (pending_longname ||
			    strcmp (name, MAGIC_LONGNAME) != 0) {
				tar->err = g_error_new (gsf_input_error_id (), 0,
							"Invalid longname header");
				break;
			}

			n = gsf_input_read (tar->source, length, NULL);
			if (!n) {
				tar->err = g_error_new (gsf_input_error_id (), 0,
							"Failed to read longname");
				break;
			}

			pending_longname = g_strndup (n, length);
			break;
		}
		default:
			/* Other -- ignore */
			break;
		}

		g_free (name);

		/* Round up to block size */
		length = (length + (BLOCK_SIZE - 1)) / BLOCK_SIZE * BLOCK_SIZE;

		if (!tar->err &&
		    gsf_input_seek (tar->source, offset + length, G_SEEK_SET)) {
			tar->err = g_error_new (gsf_input_error_id (), 0,
						"Seek failed");
			break;
		}
	}

	if (pending_longname) {
		if (!tar->err)
			tar->err = g_error_new (gsf_input_error_id (), 0,
						"Truncated archive");
		g_free (pending_longname);
	}

	if (tar->err)
		gsf_input_seek (tar->source, pos0, G_SEEK_SET);
}


/* GsfInput class functions */

static GsfInput *
gsf_infile_tar_dup (GsfInput *src_input, GError **err)
{
	GsfInfileTar *res, *src;
	unsigned ui;

	src = GSF_INFILE_TAR (src_input);
	if (src->err) {
		if (err)
			*err = g_error_copy (src->err);
		return NULL;
	}

	res = (GsfInfileTar *)g_object_new (GSF_INFILE_TAR_TYPE, NULL);
	gsf_infile_tar_set_source (res, src->source);

	for (ui = 0; ui < src->children->len; ui++) {
		/* This copies the structure.  */
		TarChild c = g_array_index (src->children, TarChild, ui);
		c.name = g_strdup (c.name);
		if (c.dir) g_object_ref (c.dir);
		g_array_append_val (res->children, c);
	}

	return NULL;
}

static guint8 const *
gsf_infile_tar_read (GsfInput *input, size_t num_bytes, guint8 *buffer)
{
	(void)input;
	(void)num_bytes;
	(void)buffer;
	return NULL;
}

static gboolean
gsf_infile_tar_seek (GsfInput *input, gsf_off_t offset, GSeekType whence)
{
	(void)input;
	(void)offset;
	(void)whence;
	return FALSE;
}

/* GsfInfile class functions */

/*****************************************************************************/

static GsfInput *
gsf_infile_tar_child_by_index (GsfInfile *infile, int target, GError **err)
{
	GsfInfileTar *tar = GSF_INFILE_TAR (infile);
	const TarChild *c;

	if (err)
		*err = NULL;

	if (target < 0 || (unsigned)target >= tar->children->len)
		return NULL;

	c = &g_array_index (tar->children, TarChild, target);
	if (c->dir)
		return g_object_ref (c->dir);
	else {
		GsfInput *input = gsf_input_proxy_new_section (tar->source,
							       c->offset,
							       c->length);
		gsf_input_set_name (input, c->name);
		return input;
	}
}

static char const *
gsf_infile_tar_name_by_index (GsfInfile *infile, int target)
{
	GsfInfileTar *tar = GSF_INFILE_TAR (infile);

	if (target < 0 || (unsigned)target >= tar->children->len)
		return NULL;

	return g_array_index (tar->children, TarChild, target).name;
}

static GsfInput *
gsf_infile_tar_child_by_name (GsfInfile *infile, char const *name, GError **err)
{
	GsfInfileTar *tar = GSF_INFILE_TAR (infile);
	unsigned ui;

	for (ui = 0; ui < tar->children->len; ui++) {
		const TarChild *c = &g_array_index (tar->children,
						    TarChild,
						    ui);
		if (strcmp (name, c->name) == 0)
			return gsf_infile_tar_child_by_index (infile, ui, err);
	}

	return NULL;
}

static int
gsf_infile_tar_num_children (GsfInfile *infile)
{
	GsfInfileTar *tar = GSF_INFILE_TAR (infile);

	return tar->children->len;
}

static void
gsf_infile_tar_finalize (GObject *obj)
{
	GsfInfileTar *tar = GSF_INFILE_TAR (obj);
	g_array_free (tar->children, TRUE);
	parent_class->finalize (obj);
}

static void
gsf_infile_tar_dispose (GObject *obj)
{
	GsfInfileTar *tar = GSF_INFILE_TAR (obj);
	unsigned ui;

	gsf_infile_tar_set_source (tar, NULL);
	g_clear_error (&tar->err);

	for (ui = 0; ui < tar->children->len; ui++) {
		TarChild *c = &g_array_index (tar->children, TarChild, ui);
		g_free (c->name);
		if (c->dir)
			g_object_unref (c->dir);
	}
	g_array_set_size (tar->children, 0);

	parent_class->dispose (obj);
}

static GObject*
gsf_infile_tar_constructor (GType                  type,
			    guint                  n_construct_properties,
			    GObjectConstructParam *construct_params)
{
	GsfInfileTar *tar = (GsfInfileTar *)
		(parent_class->constructor (type,
					    n_construct_properties,
					    construct_params));
	if (tar->source)
		tar_init_info (tar);

	return (GObject *)tar;
}


static void
gsf_infile_tar_init (GObject *obj)
{
	GsfInfileTar *tar = (GsfInfileTar *)obj;
	tar->source = NULL;
	tar->children = g_array_new (FALSE, FALSE, sizeof (TarChild));
	tar->err = NULL;
}

static void
gsf_infile_tar_get_property (GObject     *object,
			     guint        property_id,
			     GValue      *value,
			     GParamSpec  *pspec)
{
	GsfInfileTar *tar = (GsfInfileTar *)object;

	switch (property_id) {
	case PROP_SOURCE:
		g_value_set_object (value, tar->source);
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_infile_tar_set_source (GsfInfileTar *tar, GsfInput *src)
{
	if (src)
		src = gsf_input_proxy_new (src);
	if (tar->source)
		g_object_unref (tar->source);
	tar->source = src;
}

static void
gsf_infile_tar_set_property (GObject      *object,
			     guint         property_id,
			     GValue const *value,
			     GParamSpec   *pspec)
{
	GsfInfileTar *tar = (GsfInfileTar *)object;

	switch (property_id) {
	case PROP_SOURCE:
		gsf_infile_tar_set_source (tar, g_value_get_object (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}


static void
gsf_infile_tar_class_init (GObjectClass *gobject_class)
{
	GsfInputClass  *input_class  = GSF_INPUT_CLASS (gobject_class);
	GsfInfileClass *infile_class = GSF_INFILE_CLASS (gobject_class);

	gobject_class->constructor      = gsf_infile_tar_constructor;
	gobject_class->finalize		= gsf_infile_tar_finalize;
	gobject_class->dispose		= gsf_infile_tar_dispose;
	gobject_class->get_property     = gsf_infile_tar_get_property;
	gobject_class->set_property     = gsf_infile_tar_set_property;

	input_class->Dup		= gsf_infile_tar_dup;
	input_class->Read		= gsf_infile_tar_read;
	input_class->Seek		= gsf_infile_tar_seek;
	infile_class->num_children	= gsf_infile_tar_num_children;
	infile_class->name_by_index	= gsf_infile_tar_name_by_index;
	infile_class->child_by_index	= gsf_infile_tar_child_by_index;
	infile_class->child_by_name	= gsf_infile_tar_child_by_name;

	parent_class = g_type_class_peek_parent (gobject_class);

	g_object_class_install_property
		(gobject_class,
		 PROP_SOURCE,
		 g_param_spec_object ("source",
				      "Source",
				      "The archive being interpreted.",
				      GSF_INPUT_TYPE,
				      GSF_PARAM_STATIC |
				      G_PARAM_READWRITE |
				      G_PARAM_CONSTRUCT_ONLY));
}

GSF_CLASS (GsfInfileTar, gsf_infile_tar,
	   gsf_infile_tar_class_init, gsf_infile_tar_init,
	   GSF_INFILE_TYPE)

/**
 * gsf_infile_tar_new :
 * @source: A base #GsfInput
 * @err: A #GError, optionally %null
 *
 * Opens the root directory of a Tar file.
 * <note>This adds a reference to @source.</note>
 *
 * Returns: the new tar file handler
 **/
GsfInfile *
gsf_infile_tar_new (GsfInput *source, GError **err)
{
	GsfInfileTar *tar;

	g_return_val_if_fail (GSF_IS_INPUT (source), NULL);

	tar = g_object_new (GSF_INFILE_TAR_TYPE,
			    "source", source,
			    NULL);

	if (tar->err) {
		if (err)
			*err = g_error_copy (tar->err);
		g_object_unref (tar);
		return NULL;
	}

	return GSF_INFILE (tar);
}
