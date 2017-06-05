/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-utils.c:
 *
 * Copyright (C) 2002-2006 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-utils.h>
#include <gsf/gsf-input.h>
#include <gsf/gsf-doc-meta-data.h>
#include <gsf/gsf-docprop-vector.h>
#include <gsf/gsf-impl-utils.h>

#include <gsf/gsf-infile.h>
#include <gsf/gsf-infile-msole.h>
#include <gsf/gsf-infile-msvba.h>
#include <gsf/gsf-infile-stdio.h>
#include <gsf/gsf-infile-zip.h>

#include <gsf/gsf-input.h>
#include <gsf/gsf-input-gzip.h>
#include <gsf/gsf-input-http.h>
#include <gsf/gsf-input-memory.h>
#include <gsf/gsf-input-proxy.h>
#include <gsf/gsf-input-stdio.h>
#include <gsf/gsf-input-textline.h>

#include <gsf/gsf-output.h>
#include <gsf/gsf-output-bzip.h>
#include <gsf/gsf-output-csv.h>
#include <gsf/gsf-output-gzip.h>
#include <gsf/gsf-output-iconv.h>
#include <gsf/gsf-output-iochannel.h>
#include <gsf/gsf-output-memory.h>
#include <gsf/gsf-output-stdio.h>

#include <gsf/gsf-outfile.h>
#include <gsf/gsf-outfile-msole.h>
#include <gsf/gsf-outfile-stdio.h>
#include <gsf/gsf-outfile-zip.h>

#include <gsf/gsf-libxml.h>
#include <gsf/gsf-blob.h>
#include <gsf/gsf-structured-blob.h>
#include <gsf/gsf-shared-memory.h>
#include <gsf/gsf-clip-data.h>
#include <gsf/gsf-open-pkg-utils.h>

#include <gobject/gvaluecollector.h>
#include <glib/gi18n-lib.h>

#include <ctype.h>
#include <stdio.h>
#include <string.h>

/*
 * Glib gets this wrong, really.  ARM's floating point format is a weird
 * mixture.
 */
#define G_ARMFLOAT_ENDIAN 56781234
#if defined(__arm__) && !defined(__ARM_EABI__) && (G_BYTE_ORDER == G_LITTLE_ENDIAN)
#define G_FLOAT_BYTE_ORDER G_ARMFLOAT_ENDIAN
#else
#define G_FLOAT_BYTE_ORDER G_BYTE_ORDER
#endif

gboolean
gsf_debug_flag (const char *flag)
{
	GDebugKey key;
	key.key = (char *)flag;
	key.value = 1;

	return g_parse_debug_string (g_getenv ("GSF_DEBUG"), &key, 1) != 0;
}


#ifdef _GSF_GTYPE_THREADING_FIXED
typedef GTypeModule      GsfDummyTypeModule;
typedef GTypeModuleClass GsfDummyTypeModuleClass;
static gboolean
gsf_dummy_type_module_load (GTypeModule *module)
{
	gsf_init_dynamic (module);
	return TRUE;
}
static void
gsf_dummy_type_module_class_init (GTypeModuleClass *gtm_class)
{
	gtm_class->load = gsf_dummy_type_module_load;
}
static GSF_CLASS (GsfDummyTypeModule, gsf_dummy_type_module,
		  gsf_dummy_type_module_class_init, NULL,
		  G_TYPE_TYPE_MODULE)

static GTypeModule *static_type_module = NULL;
#endif

#ifdef G_OS_WIN32
#include <windows.h>
static HMODULE gsf_dll_hmodule;
BOOL WINAPI
DllMain (HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved);
BOOL WINAPI
DllMain (HINSTANCE hinstDLL, DWORD fdwReason, G_GNUC_UNUSED LPVOID lpvReserved)
{
	if (fdwReason == DLL_PROCESS_ATTACH) gsf_dll_hmodule = hinstDLL;
	return TRUE;
}
#endif

/**
 * gsf_init :
 *
 * Initializes the GSF library
 **/
void
gsf_init (void)
{
	static gboolean libgsf_initialized = FALSE;
	if (libgsf_initialized)
		return;

#ifdef ENABLE_NLS
#ifdef G_OS_WIN32
	{
		char *pkg_dir	  = g_win32_get_package_installation_directory_of_module (gsf_dll_hmodule);
		gchar *locale_dir = g_build_filename (pkg_dir, "lib/locale", NULL);
		bindtextdomain (GETTEXT_PACKAGE, locale_dir);
		g_free (locale_dir);
		g_free (pkg_dir);
	}
#else
	bindtextdomain (GETTEXT_PACKAGE, GSFLOCALEDIR);
#endif
	bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#endif

	g_type_init ();

#ifdef _GSF_GTYPE_THREADING_FIXED
	if (NULL == static_type_module) {
		static_type_module = g_object_new (gsf_dummy_type_module_get_type(), NULL);
		g_assert (static_type_module != NULL);
		g_type_module_use (static_type_module);
		g_type_module_set_name (static_type_module, "libgsf-builtin");
	}
#else
	gsf_init_dynamic (NULL);
#endif

	{
		/* Little-endian representation of M_PI.  */
		static const guint8 pibytes[8] = {
			0x18, 0x2d, 0x44, 0x54, 0xfb, 0x21, 0x09, 0x40
		};

		/*
		 * If this fails, see
		 *   http://bugzilla.gnome.org/show_bug.cgi?id=350973
		 */
		double pi = gsf_le_get_double (pibytes);
		if (!(pi > 3.14 && pi < 3.15))
			g_error ("Compilation trouble with endianess.");
	}
}

/**
 * gsf_shutdown:
 *
 * De-intializes the GSF library
 * Currently does nothing.
 **/
void
gsf_shutdown (void)
{
}

#ifdef _GSF_GTYPE_THREADING_FIXED
#define REGISTER(prefix)						\
	do {								\
		prefix ## _register_type (module);			\
		types = g_slist_prepend (types,				\
			g_type_class_ref (prefix ## _get_type()));	\
	} while (0)
#else
/* Assign the value to avoid compiler warnings */
#define REGISTER(prefix)	t = prefix ## _get_type()
#endif

/**
 * gsf_init_dynamic :
 * @module : #GTypeModule.
 *
 * Initializes the GSF library and associates it with a type module @mod.
 **/
void
gsf_init_dynamic (GTypeModule *module)
{
#ifndef _GSF_GTYPE_THREADING_FIXED
	GType t;
	if (NULL != module) {
		g_warning ("glib's support of dynamic types is not thread safe.\n"
			   "Support for gsf_init_dynamic has been disabled until that is fixed");
	}
#endif
	REGISTER (gsf_input);
	REGISTER (gsf_input_gzip);
	REGISTER (gsf_input_http);
	REGISTER (gsf_input_memory);
	REGISTER (gsf_input_proxy);
	REGISTER (gsf_input_stdio);
	REGISTER (gsf_input_textline);

	REGISTER (gsf_infile);
	REGISTER (gsf_infile_msole);
	REGISTER (gsf_infile_msvba);
	REGISTER (gsf_infile_stdio);
	REGISTER (gsf_infile_zip);

	REGISTER (gsf_output);
	REGISTER (gsf_output_bzip);
	REGISTER (gsf_output_csv_quoting_mode);
	REGISTER (gsf_output_csv);
	REGISTER (gsf_output_gzip);
	REGISTER (gsf_output_iconv);
	REGISTER (gsf_output_iochannel);
	REGISTER (gsf_output_memory);
	REGISTER (gsf_output_stdio);

	REGISTER (gsf_outfile);
	REGISTER (gsf_outfile_msole);
	REGISTER (gsf_outfile_stdio);
	REGISTER (gsf_outfile_zip);
	REGISTER (gsf_outfile_open_pkg);

	REGISTER (gsf_shared_memory);
	REGISTER (gsf_structured_blob);
	REGISTER (gsf_xml_out);
	REGISTER (gsf_blob);
	REGISTER (gsf_clip_data);
	REGISTER (gsf_doc_meta_data);
	REGISTER (gsf_docprop_vector);
}

/**
 * gsf_shutdown:
 *
 * De-intializes the GSF library from a type module.
 * Currently does nothing.
 **/
void
gsf_shutdown_dynamic (G_GNUC_UNUSED GTypeModule *module)
{
}

static void
gsf_mem_dump_full (guint8 const *ptr, size_t len, gsf_off_t offset)
{
	static const char hexdigit[16] = "0123456789abcdef";

	while (len > 0) {
		char hexpart[3 * 16 + 1], *phex = hexpart;
		char pic[17];
		size_t j;
		for (j = 0; j < 16; j++) {
			if (len > 0) {
				*phex++ = hexdigit[*ptr >> 4];
				*phex++ = hexdigit[*ptr & 0xf];
				pic[j] = (*ptr >= '!' && *ptr < 127 ? *ptr : '.');
				len--;
				ptr++;
			} else {
				*phex++ = 'X';
				*phex++ = 'X';
				pic[j] = '*';
			}
			*phex++ = ' ';
		}
		hexpart[3 * 16] = 0;
		pic[16] = 0 ;

		g_print ("%8lx | %s| %s\n", (long)offset, hexpart, pic);
		offset += 16;
	}
}

/**
 * gsf_mem_dump :
 * @ptr: memory area to be dumped.
 * @len: how many bytes will be dumped.
 *
 * Dump @len bytes from the memory location given by @ptr.
 **/
void
gsf_mem_dump (guint8 const *ptr, size_t len)
{
	gsf_mem_dump_full (ptr, len, 0);
}

/**
 * gsf_input_dump :
 * @input: a #GsfInput
 * @dump_as_hex: If %TRUE, dump in hexidecmal format
 *
 * Dumps @input's contents to STDOUT, optionally in hex format.
 */
void
gsf_input_dump (GsfInput *input, gboolean dump_as_hex)
{
	gsf_off_t offset = 0;
	size_t size, count;
	guint8 const *data;

	/* read in small blocks to excercise things */
	size = gsf_input_size (GSF_INPUT (input));
	while (size > 0) {
		count = size;
		if (count > 0x100)
			count = 0x100;
		data = gsf_input_read (GSF_INPUT (input), count, NULL);
		g_return_if_fail (data != NULL);
		if (dump_as_hex)
			gsf_mem_dump_full (data, count, offset);
		else
			fwrite (data, 1, count, stdout);
		size -= count;
		offset += count;
	}
	if (!dump_as_hex)
		fflush (stdout);
}

/**
 * gsf_le_get_guint64
 * @p: pointer to storage
 *
 * Interpret binary data as a guint64 (8 byte unsigned integer type) in little
 * endian order.
 *
 * Returns: interpreted data
 */
guint64
gsf_le_get_guint64 (void const *p)
{
#if G_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (guint64) == 8) {
		guint64 li;
		int     i;
		guint8 *t  = (guint8 *)&li;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (li);

		for (i = 0; i < sd; i++)
			t[i] = p2[sd - 1 - i];

		return li;
	} else {
		g_error ("Big endian machine, but weird size of guint64");
	}
#elif G_BYTE_ORDER == G_LITTLE_ENDIAN
	if (sizeof (guint64) == 8) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		guint64 data;
		memcpy (&data, p, sizeof (data));
		return data;
	} else {
		g_error ("Little endian machine, but weird size of guint64");
	}
#else
#error "Byte order not recognised -- out of luck"
#endif
}

/**
 * gsf_le_get_float :
 * @p: pointer to storage
 *
 * Interpret binary data as a float in little endian order.
 *
 *
 * Returns: interpreted data
 */
float
gsf_le_get_float (void const *p)
{
#if G_FLOAT_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (float) == 4) {
		float   f;
		int     i;
		guint8 *t  = (guint8 *)&f;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (f);

		for (i = 0; i < sd; i++)
			t[i] = p2[sd - 1 - i];

		return f;
	} else {
		g_error ("Big endian machine, but weird size of floats");
	}
#elif (G_FLOAT_BYTE_ORDER == G_LITTLE_ENDIAN) || (G_FLOAT_BYTE_ORDER == G_ARMFLOAT_ENDIAN)
	if (sizeof (float) == 4) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		float data;
		memcpy (&data, p, sizeof (data));
		return data;
	} else {
		g_error ("Little endian machine, but weird size of floats");
	}
#else
#error "Floating-point byte order not recognised -- out of luck"
#endif
}

/**
 * gsf_le_set_float :
 * @p: pointer to storage
 * @f: float to be stored
 *
 * Store a value of type float in memory in little endian order.
 */
void
gsf_le_set_float (void *p, float f)
{
#if G_FLOAT_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (float) == 4) {
		int     i;
		guint8 *t  = (guint8 *)&f;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (f);

		for (i = 0; i < sd; i++)
			p2[sd - 1 - i] = t[i];
	} else {
		g_error ("Big endian machine, but weird size of floats");
	}
#elif (G_FLOAT_BYTE_ORDER == G_LITTLE_ENDIAN) || (G_FLOAT_BYTE_ORDER == G_ARMFLOAT_ENDIAN)
	if (sizeof (float) == 4) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		memcpy (p, &f, sizeof (f));
	} else {
		g_error ("Little endian machine, but weird size of floats");
	}
#else
#error "Floating-point byte order not recognised -- out of luck"
#endif
}

/**
 * gsf_le_get_double :
 * @p: pointer to storage
 *
 * Interpret binary data as a double in little endian order.
 *
 * Returns: interpreted data
 */
double
gsf_le_get_double (void const *p)
{
#if G_FLOAT_BYTE_ORDER == G_ARMFLOAT_ENDIAN
	double data;
	memcpy ((char *)&data + 4, p, 4);
	memcpy ((char *)&data, (char const *)p + 4, 4);
	return data;
#elif G_FLOAT_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (double) == 8) {
		double  d;
		int     i;
		guint8 *t  = (guint8 *)&d;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (d);

		for (i = 0; i < sd; i++)
			t[i] = p2[sd - 1 - i];

		return d;
	} else {
		g_error ("Big endian machine, but weird size of doubles");
	}
#elif G_FLOAT_BYTE_ORDER == G_LITTLE_ENDIAN
	if (sizeof (double) == 8) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		double data;
		memcpy (&data, p, sizeof (data));
		return data;
	} else {
		g_error ("Little endian machine, but weird size of doubles");
	}
#else
#error "Floating-point byte order not recognised -- out of luck"
#endif
}

/**
 * gsf_le_set_double :
 * @p: pointer to storage
 * @d: double to be stored
 *
 * Store a value of type double in memory in little endian order
 */
void
gsf_le_set_double (void *p, double d)
{
#if G_FLOAT_BYTE_ORDER == G_ARMFLOAT_ENDIAN
	memcpy (p, (char const *)&d + 4, 4);
	memcpy ((char *)p + 4, &d, 4);
#elif G_FLOAT_BYTE_ORDER == G_BIG_ENDIAN
	if (sizeof (double) == 8) {
		int     i;
		guint8 *t  = (guint8 *)&d;
		guint8 *p2 = (guint8 *)p;
		int     sd = sizeof (d);

		for (i = 0; i < sd; i++)
			p2[sd - 1 - i] = t[i];
	} else {
		g_error ("Big endian machine, but weird size of doubles");
	}
#elif G_FLOAT_BYTE_ORDER == G_LITTLE_ENDIAN
	if (sizeof (double) == 8) {
		/*
		 * On i86, we could access directly, but Alphas require
		 * aligned access.
		 */
		memcpy (p, &d, sizeof (d));
	} else {
		g_error ("Little endian machine, but weird size of doubles");
	}
#else
#error "Floating-point byte order not recognised -- out of luck"
#endif
}

/**
 * gsf_extension_pointer:
 * @path: A filename or file path.
 *
 * Extracts the extension from the end of a filename (the part after the final
 * '.' in the filename).
 *
 * Returns: A pointer to the extension part of the filename, or a
 * pointer to the end of the string if the filename does not
 * have an extension.
 */
char const *
gsf_extension_pointer (char const *path)
{
	char const *s, *end;

	g_return_val_if_fail (path != NULL, NULL);

	end = path + strlen (path);
	for (s = end; s > path; ) {
		s--;
		if (G_IS_DIR_SEPARATOR (*s))
			break;
		if (*s == '.')
			return s + 1;
	}

	return end;
}

/**
 * gsf_iconv_close :
 * @handle : handle to be closed.
 *
 * A utility wrapper to safely close an iconv handle.
 **/
void
gsf_iconv_close (GIConv handle)
{
	if (handle != NULL && handle != ((GIConv)-1))
		g_iconv_close (handle);
}

/**
 * gsf_filename_to_utf8:
 * @filename: file name suitable for open(2).
 * @quoted: if %TRUE, the resulting utf8 file name will be quoted
 *    (unless it is invalid).
 *
 * A utility wrapper to make sure filenames are valid utf8.
 * Caller must g_free the result.
 *
 * Returns: @filename using utf-8 encoding for display
 **/
char *
gsf_filename_to_utf8 (char const *filename, gboolean quoted)
{
	char *dname = g_filename_display_name (filename);
	char *result;

	if (quoted) {
		result = g_strconcat ("\"", dname, "\"", NULL);
		g_free (dname);
	} else
		result = dname;

	return result;
}

/**
 * gsf_base64_encode_close :
 * @in : Data to be encoded
 * @inlen : Length of data to be encoded
 * @break_lines : Whether to use line breaks
 * @out : Encoded data.
 * @state: holds the number of bits that are stored in @save
 * @save: leftover bits that have not yet been decoded
 *
 * This funcion should be called to when finished encoding everything, to
 * flush off the last little bit.
 *
 * Returns: a count of the number of bytes in the final block.
 **/
size_t
gsf_base64_encode_close (guint8 const *in, size_t inlen,
			 gboolean break_lines, guint8 *out, int *state, unsigned int *save)
{
	guint8 *outptr = out;
	if (inlen > 0)
		outptr += gsf_base64_encode_step (in, inlen, break_lines,
						  outptr, state, save);
	outptr += g_base64_encode_close (break_lines, outptr, state, save);
	return outptr-out;
}

/**
 * gsf_base64_encode_step :
 * @in : input stream
 * @len : max length of data to decode
 * @break_lines : Whether to use line breaks
 * @out : output stream
 * @state : holds the number of bits that are stored in @save
 * @save : leftover bits that have not yet been decoded
 *
 * Performs an 'encode step', only encodes blocks of 3 characters from @in into
 * the output @out at a time, saves left-over state in @state and @save
 * (initialise to 0 on first invocation).
 *
 * Returns: the number of bytes encoded
 */
size_t
gsf_base64_encode_step (guint8 const *in, size_t len,
			gboolean break_lines, guint8 *out, int *state, unsigned int *save)
{
	return g_base64_encode_step (in, len, break_lines, out, state, save);
}


/**
 * gsf_base64_decode_step:
 * @in: input stream
 * @len: max length of data to decode
 * @out: output stream
 * @state: holds the number of bits that are stored in @save
 * @save: leftover bits that have not yet been decoded
 *
 * Decodes a chunk of base64 encoded data
 *
 * Returns: the number of bytes converted
 **/
size_t
gsf_base64_decode_step (guint8 const *in, size_t len, guint8 *out,
			int *state, guint *save)
{
	return g_base64_decode_step (in, len, out, state, save);
}

/**
 * gsf_base64_encode_simple :
 * @data : data stream
 * @len : max length of data to encode
 *
 * Encodes data from @data back into @data using base64 encoding.
 *
 * Returns: the number of bytes encoded
 */
guint8 *
gsf_base64_encode_simple (guint8 const *data, size_t len)
{
	guint8 *out;
	int state = 0;
	guint save = 0;
	gboolean break_lines = TRUE;  /* This differs from g_base64_encode */
	size_t outlen = len * 4 / 3 + 5;

	if (break_lines)
		outlen += outlen / 72 + 1;
	out = g_new (guint8, outlen);
	outlen = gsf_base64_encode_close (data, len, break_lines,
					  out, &state, &save);
	out[outlen] = '\0';
	return out;
}

/**
 * gsf_base64_decode_simple :
 * @data : data stream
 * @len : max length of data to decode
 *
 * Decodes a chunk of base64 encoded data from @data back into @data.
 *
 * Returns: the number of bytes converted
 */
size_t
gsf_base64_decode_simple (guint8 *data, size_t len)
{
	int state = 0;
	guint save = 0;
	return gsf_base64_decode_step (data, len, data, &state, &save);
}


/* Largely a copy of g_object_new_valist.  */
/**
 * gsf_property_settings_collect_valist:
 * @object_type: the GType for which the properties are being set.
 * @p_n_params: a pointer to the number of properties collected.  (Used for
 *   both input and output.)
 * @p_params: a pointer to the GParameter array that holds the properties.
 *   (Used for both input and output.  This may point to a %NULL pointer if
 *   there are no properties collected yet.)
 * @first_property_name: the name of the first property being set, or NULL.
 * @var_args: a va_list holding the remainder of the property names and
 *   values, terminated by a %NULL.
 *
 * This function builds a GParameter array suitable for g_object_newv.
 **/
void
gsf_property_settings_collect_valist (GType object_type,
				      GParameter **p_params,
				      size_t *p_n_params,
				      const gchar *first_property_name,
				      va_list var_args)
{
  GObjectClass *class;
  GParameter *params = *p_params;
  const gchar *name;
  size_t n_params = *p_n_params;
  size_t n_alloced_params = n_params;  /* We might have more.  */

  g_return_if_fail (G_TYPE_IS_OBJECT (object_type));

  class = g_type_class_ref (object_type);

  name = first_property_name;
  while (name)
    {
      gchar *error = NULL;
      GParamSpec *pspec = g_object_class_find_property (class, name);
      if (!pspec)
	{
	  g_warning ("%s: object class `%s' has no property named `%s'",
		     G_STRFUNC,
		     g_type_name (object_type),
		     name);
	  break;
	}

      if (n_params >= n_alloced_params)
	{
	  n_alloced_params += 16;
	  params = g_renew (GParameter, params, n_alloced_params);
	}
      params[n_params].name = name;
      params[n_params].value.g_type = 0;
      g_value_init (&params[n_params].value, G_PARAM_SPEC_VALUE_TYPE (pspec));
      G_VALUE_COLLECT (&params[n_params].value, var_args, 0, &error);
      if (error)
	{
	  g_warning ("%s: %s", G_STRFUNC, error);
	  g_free (error);
          g_value_unset (&params[n_params].value);
	  break;
	}
      n_params++;
      name = va_arg (var_args, gchar*);
    }

  g_type_class_unref (class);

  *p_params = params;
  *p_n_params = n_params;
}

/* This is a vararg version of gsf_property_settings_collect_valist.  */
void
gsf_property_settings_collect (GType object_type,
			       GParameter **p_params,
			       size_t *p_n_params,
			       const gchar *first_property_name,
			       ...)
{
  va_list var_args;
  va_start (var_args, first_property_name);
  gsf_property_settings_collect_valist (object_type, p_params, p_n_params, first_property_name, var_args);
  va_end (var_args);
}

void
gsf_property_settings_free (GParameter *params,
			    size_t n_params)
{
	while (n_params--)
		g_value_unset (&params[n_params].value);
	g_free (params);
}



/* Errors */

/**
 * gsf_error_quark:
 *
 * Returns:  the #GQuark used to identify libgsf errors in #GError structures.
 * 	Specific error codes come from the #GsfError enumeration.
 **/
GQuark
gsf_error_quark (void)
{
	static GQuark quark;

	if (quark == 0)
		quark = g_quark_from_static_string ("gsf-error-quark");

	return quark;
}
