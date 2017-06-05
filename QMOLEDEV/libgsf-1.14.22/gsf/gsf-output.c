/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-output.c: interface for storing data
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
#include <gsf/gsf-output-impl.h>
#include <gsf/gsf-impl-utils.h>
#include <string.h>

static gsf_off_t gsf_output_real_vprintf (GsfOutput *output,
	char const* format, va_list args) G_GNUC_PRINTF (2, 0);

#define GET_CLASS(instance) G_TYPE_INSTANCE_GET_CLASS (instance, GSF_OUTPUT_TYPE, GsfOutputClass)

static GObjectClass *parent_class;

enum {
	PROP_0,
	PROP_NAME,
	PROP_SIZE,
	PROP_CLOSED,
	PROP_POS
};

static void
gsf_output_set_property (GObject      *object,
			 guint         property_id,
	 G_GNUC_UNUSED   GValue const *value,
			 GParamSpec   *pspec)
{
	switch (property_id) {
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_output_get_property (GObject     *object,
			 guint        property_id,
			 GValue      *value,
			 GParamSpec  *pspec)
{
	/* gsf_off_t is typedef'd to gint64 */
	switch (property_id) {
	case PROP_NAME:
		g_value_set_string (value, gsf_output_name (GSF_OUTPUT (object)));
		break;
	case PROP_SIZE:
		g_value_set_int64 (value, gsf_output_size (GSF_OUTPUT (object)));
		break;
	case PROP_POS:
		g_value_set_int64 (value, gsf_output_tell (GSF_OUTPUT (object)));
		break;
	case PROP_CLOSED:
		g_value_set_boolean (value, gsf_output_is_closed (GSF_OUTPUT (object)));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void
gsf_output_dispose (GObject *obj)
{
	GsfOutput *output = GSF_OUTPUT (obj);

	if (!output->is_closed) {
		/* g_warning ("Disposing of an unclosed stream"); */
		gsf_output_close (output);
	}

	gsf_output_set_container (output, NULL);
	gsf_output_set_name (output, NULL);

	g_free (output->printf_buf);
	output->printf_buf = NULL;

	g_clear_error (&output->err);

	parent_class->dispose (obj);
}

static void
gsf_output_init (GObject *obj)
{
	GsfOutput *output = GSF_OUTPUT (obj);

	output->cur_offset	= 0;
	output->cur_size	= 0;
	output->name		= NULL;
	output->wrapped_by	= NULL;
	output->container	= NULL;
	output->err		= NULL;
	output->is_closed	= FALSE;
	output->printf_buf	= NULL;
	output->printf_buf_size = 0;
}

static void
gsf_output_class_init (GObjectClass *gobject_class)
{
	GsfOutputClass  *output_class  = GSF_OUTPUT_CLASS (gobject_class);

	gobject_class->dispose      = gsf_output_dispose;
	gobject_class->set_property = gsf_output_set_property;
	gobject_class->get_property = gsf_output_get_property;
	output_class->Vprintf       = gsf_output_real_vprintf;

	parent_class = g_type_class_peek_parent (gobject_class);

	g_object_class_install_property (gobject_class,
					 PROP_NAME,
					 g_param_spec_string ("name", "Name",
							      "The Output's Name",
							      NULL,
							      GSF_PARAM_STATIC |
							      G_PARAM_READABLE));
	g_object_class_install_property (gobject_class,
					 PROP_SIZE,
					 g_param_spec_int64 ("size", "Size",
							     "The Output's Size",
							     0, G_MAXINT64, 0,
							     GSF_PARAM_STATIC |
							     G_PARAM_READABLE));
	g_object_class_install_property (gobject_class,
					 PROP_POS,
					 g_param_spec_int64 ("position", "Position",
							     "The Output's Current Position",
							     0, G_MAXINT64, 0,
							     GSF_PARAM_STATIC |
							     G_PARAM_READABLE));
	g_object_class_install_property (gobject_class,
					 PROP_CLOSED,
					 g_param_spec_boolean ("is-closed", "Is Closed",
							       "Whether the Output is Closed",
							       FALSE,
							       GSF_PARAM_STATIC |
							       G_PARAM_READABLE));
}

GSF_CLASS_ABSTRACT (GsfOutput, gsf_output,
		    gsf_output_class_init, gsf_output_init,
		    G_TYPE_OBJECT)

/**
 * gsf_output_name :
 * @output: #GsfOutput
 *
 * Give the name of @output.
 *
 * Returns: @output's name in utf8 form, DO NOT FREE THIS STRING
 **/
char const *
gsf_output_name (GsfOutput const *output)
{
	g_return_val_if_fail (GSF_IS_OUTPUT (output), NULL);
	return output->name;
}

/**
 * gsf_output_container :
 * @output :
 *
 * Returns: but does not add a reference to @output's container.
 * 	Potentially %NULL
 **/
GsfOutfile *
gsf_output_container (GsfOutput const *output)
{
	g_return_val_if_fail (GSF_IS_OUTPUT (output), NULL);
	return output->container;
}

/**
 * gsf_output_size :
 * @output: #GsfOutput
 *
 * Determine the size of the output stream @output.
 *
 * Returns: the size of the output, or -1 if it does not have a size.
 **/
gsf_off_t
gsf_output_size (GsfOutput *output)
{
	g_return_val_if_fail (GSF_IS_OUTPUT (output), -1);
	return output->cur_size;
}

/**
 * gsf_output_close :
 * @output: #GsfOutput
 *
 * Close a stream.
 *
 * Returns: %FALSE on error
 **/
gboolean
gsf_output_close (GsfOutput *output)
{
	gboolean res;

	g_return_val_if_fail (GSF_IS_OUTPUT (output),
		gsf_output_set_error (output, 0, "<internal>"));
	g_return_val_if_fail (!output->is_closed,
		gsf_output_set_error (output, 0, "<internal>"));

	/* The implementation will log any errors, but we can never try to
	 * close multiple times even on failure.
	 */
	res = GET_CLASS (output)->Close (output);
	output->is_closed = TRUE;
	return res;
}

/**
 * gsf_output_is_closed :
 * @output: #GsfOutput
 *
 * Returns: %TRUE if @output has already been closed.
 **/
gboolean
gsf_output_is_closed (GsfOutput const *output)
{
	g_return_val_if_fail (GSF_IS_OUTPUT (output), TRUE);
	return output->is_closed;
}

/**
 * gsf_output_tell :
 * @output : #GsfOutput
 *
 * Tell the current position in @output, similar to
 * <citerefentry><refentrytitle>ftell</refentrytitle>
 * <manvolnum>3</manvolnum></citerefentry>.
 *
 * Returns: the current position in the file
 **/
gsf_off_t
gsf_output_tell	(GsfOutput *output)
{
	g_return_val_if_fail (output != NULL, 0);

	return output->cur_offset;
}

/**
 * gsf_output_seek :
 * @output : #GsfOutput
 * @offset : Relative amount to reposition
 * @whence : What the offset is relative to.
 *
 * Reposition in output stream @output. @whence specifies what the offset is
 * relative to: the beginning of the stream (%G_SEEK_SET), current position in
 * the stream (%G_SEEK_CUR) or the end of the stream (%G_SEEK_END).
 * This function is similar to
 * <citerefentry><refentrytitle>fseek</refentrytitle>
 * <manvolnum>3</manvolnum></citerefentry>.
 *
 * Returns: %FALSE on error.
 **/
gboolean
gsf_output_seek	(GsfOutput *output, gsf_off_t offset, GSeekType whence)
{
	gsf_off_t pos = offset;

	g_return_val_if_fail (output != NULL, FALSE);

	switch (whence) {
	case G_SEEK_SET: break;
	case G_SEEK_CUR: pos += output->cur_offset;	break;
	case G_SEEK_END: pos += output->cur_size;	break;
	default :
		g_warning ("Invalid seek type %d", (int)whence);
		return FALSE;
	}

	if (pos < 0) {
		g_warning ("Invalid seek position %" GSF_OFF_T_FORMAT
			   ", which is before the start of the file", pos);
		return FALSE;
	}

	/* If we go nowhere, just return.  This in particular handles null
	 * seeks for streams with no seek method.
	 */
	if (pos == output->cur_offset)
		return TRUE;

	if (GET_CLASS (output)->Seek (output, offset, whence)) {
		/* NOTE : it is possible for the current pos to be beyond the
		 * end of the file.  The intervening space is not filled with 0
		 * until something is written.
		 */
		output->cur_offset = pos;
		return TRUE;
	}

	/* the implementation should have assigned whatever errors are necessary */
	return FALSE;
}

static inline gboolean
gsf_output_inc_cur_offset (GsfOutput *output, gsf_off_t num_bytes)
{
	output->cur_offset += num_bytes;
	if (output->cur_offset < num_bytes)
		return gsf_output_set_error (output, 0, "Output size overflow.");
	if (output->cur_size < output->cur_offset)
		output->cur_size = output->cur_offset;
	return TRUE;
}

/**
 * gsf_output_write :
 * @output : Output stream
 * @num_bytes : Number of bytes to write
 * @data : Data to write.
 *
 * Write @num_bytes of @data to @output.
 *
 * Returns: %FALSE on error.
 **/
gboolean
gsf_output_write (GsfOutput *output,
		  size_t num_bytes, guint8 const *data)
{
	g_return_val_if_fail (output != NULL, FALSE);

	if (num_bytes == 0)
		return TRUE;
	if (GET_CLASS (output)->Write (output, num_bytes, data))
		return gsf_output_inc_cur_offset (output, num_bytes);

	/* the implementation should have assigned whatever errors are necessary */
	return FALSE;
}

/**
 * gsf_output_error :
 * @output:
 *
 * Returns: the last error logged on the output, or %NULL.
 **/
GError const *
gsf_output_error (GsfOutput const *output)
{
	g_return_val_if_fail (GSF_IS_OUTPUT (output), NULL);
	return output->err;
}

/**
 * gsf_output_set_name :
 * @output: #GsfOutput
 * @name: the new name
 *
 * <note>This is a utility routine that should only be used by derived
 * outputs.</note>
 *
 * Returns: %TRUE if the assignment was ok.
 **/
gboolean
gsf_output_set_name (GsfOutput *output, char const *name)
{
	g_return_val_if_fail (GSF_IS_OUTPUT (output), FALSE);

	if (g_strcmp0 (name, output->name)) {
		g_free (output->name);
		output->name = g_strdup (name);
		g_object_notify (G_OBJECT (output), "name");
	}

	return TRUE;
}

/**
 * gsf_output_set_name_from_filename :
 * @output : the output stream
 * @filename : the (fs-sys encoded) filename
 *
 * <note>This is a utility routine that should only be used by derived
 * outputs.</note>
 *
 * Returns: %TRUE if the assignment was ok.
 **/
gboolean
gsf_output_set_name_from_filename (GsfOutput *output, char const *filename)
{
	char *name;
	gboolean res;

	g_return_val_if_fail (GSF_IS_OUTPUT (output), FALSE);

	name = filename
		? g_filename_to_utf8 (filename, -1, NULL, NULL, NULL)
		: NULL;
	res = gsf_output_set_name (output, name);
	g_free (name);
	return res;
}

/**
 * gsf_output_set_container :
 * @output: #GsfOutput
 * @container: #GsfOutfile
 *
 * <note>This is a utility routine that should only be used by derived
 * outputs.</note>
 *
 * Returns: %TRUE if the assignment was ok.
 **/
gboolean
gsf_output_set_container (GsfOutput *output, GsfOutfile *container)
{
	g_return_val_if_fail (GSF_IS_OUTPUT (output), FALSE);

	if (container != NULL)
		g_object_ref (container);
	if (output->container != NULL)
		g_object_unref (output->container);
	output->container = container;
	return TRUE;
}

/**
 * gsf_output_set_error :
 * @output: #GsfOutput
 * @code: The error id
 * @format: printf style format string
 * @Varargs: arguments for @format
 *
 * <note>This is a utility routine that should only be used by derived
 * outputs.</note>
 *
 * Returns: Always returns %FALSE to facilitate its use.
 **/
gboolean
gsf_output_set_error (GsfOutput  *output,
		      gint        code,
		      char const *format,
		      ...)
{
	g_return_val_if_fail (GSF_IS_OUTPUT (output), FALSE);

	g_clear_error (&output->err);

	if (format != NULL) {
		char *message;
		va_list args;

		va_start (args, format);
		message = g_strdup_vprintf (format, args);
		va_end (args);

		output->err = g_error_new_literal (gsf_output_error_id (),
		                                   code,
		                                   message);
		g_free (message);
	}

	return FALSE;
}

static void
cb_output_unwrap (GsfOutput *wrapee, G_GNUC_UNUSED GObject *wrapper)
{
	wrapee->wrapped_by = NULL;
}

/**
 * gsf_output_wrap :
 * @wrapper:
 * @wrapee:
 *
 * Returns: %TRUE if the wrapping succeeded.
 **/
gboolean
gsf_output_wrap (GObject *wrapper, GsfOutput *wrapee)
{
	g_return_val_if_fail (wrapper != NULL, FALSE);
	g_return_val_if_fail (wrapee != NULL, FALSE);

	if (wrapee->wrapped_by != NULL) {
		g_warning ("Attempt to wrap an output that is already wrapped.");
		return FALSE;
	}

	g_object_weak_ref (wrapper,
			   (GWeakNotify) cb_output_unwrap, wrapee);
	wrapee->wrapped_by = wrapper;
	return TRUE;
}

/**
 * gsf_output_unwrap :
 * @wrapper:
 * @wrapee:
 *
 * Returns: %TRUE if the unwrapping succeeded.
 **/
gboolean
gsf_output_unwrap (GObject *wrapper, GsfOutput *wrapee)
{
	g_return_val_if_fail (wrapee != NULL, FALSE);
	g_return_val_if_fail (wrapee->wrapped_by == wrapper, FALSE);

	wrapee->wrapped_by = NULL;
	g_object_weak_unref (wrapper,
			     (GWeakNotify) cb_output_unwrap, wrapee);
	return TRUE;
}

GQuark
gsf_output_error_id (void)
{
	static GQuark quark;
	if (!quark)
		quark = g_quark_from_static_string ("gsf_output_error");
	return quark;
}

/**
 * gsf_output_printf :
 * @output : A #GsfOutput
 * @format : The printf-style format string
 * @Varargs : the arguments for @format
 *
 * Output @Varargs to @output using the format string @format, similar to
 * <citerefentry><refentrytitle>printf</refentrytitle>
 * <manvolnum>3</manvolnum></citerefentry>.
 *
 * Returns: %TRUE if successful, %FALSE if not
 **/
gboolean
gsf_output_printf (GsfOutput *output, char const *format, ...)
{
	va_list args;
	gboolean res;

	va_start (args, format);
	res = (gsf_output_vprintf (output, format, args) >= 0);
	va_end (args);
	return res;
}

/**
 * gsf_output_vprintf :
 * @output : A #GsfOutput
 * @format : The printf-style format string
 * @args : the arguments for @format
 *
 * Output @args to @output using the format string @format, similar to
 * <citerefentry><refentrytitle>vprintf</refentrytitle>
 * <manvolnum>3</manvolnum></citerefentry>.
 *
 * Returns: number of bytes printed, a negative value if not successful
 **/
gsf_off_t
gsf_output_vprintf (GsfOutput *output, char const *format, va_list args)
{
	gsf_off_t num_bytes;

	g_return_val_if_fail (output != NULL, -1);
	g_return_val_if_fail (format != NULL, -1);
	/* g_return_val_if_fail (strlen (format) > 0, -1); -- Why? */

	num_bytes = GET_CLASS (output)->Vprintf (output, format, args);

	if (num_bytes >= 0)
		if (!gsf_output_inc_cur_offset (output, num_bytes))
			return -1;
	return num_bytes;
}

static gsf_off_t
gsf_output_real_vprintf (GsfOutput *output, char const *fmt, va_list args)
{
	gsf_off_t reslen;
	va_list args2;

	/*
	 * We need to make a copy as args will become unusable after
	 * the g_vsnprintf call.
	 */
	G_VA_COPY (args2, args);

	if (NULL == output->printf_buf) {
		output->printf_buf_size = 128;
		output->printf_buf = g_new (char, output->printf_buf_size);
	}
	reslen = g_vsnprintf (output->printf_buf, output->printf_buf_size, fmt, args);

	/* handle C99 or older -1 case of vsnprintf */
	if (reslen < 0 || reslen >= output->printf_buf_size) {
		g_free (output->printf_buf);
		output->printf_buf = g_strdup_vprintf (fmt, args2);
		reslen = output->printf_buf_size = strlen (output->printf_buf);
	}
	va_end (args2);

	if (reslen == 0 ||
	    GET_CLASS (output)->Write (output, reslen, output->printf_buf))
		return reslen;

	return -1;
}

/**
 * gsf_output_puts:
 * @output: A #GsfOutput
 * @line: %null terminated string to write
 *
 * Like fputs, this assumes that the line already ends with a newline
 *
 * Returns: %TRUE if successful, %FALSE if not
 **/
gboolean
gsf_output_puts (GsfOutput *output, char const *line)
{
	size_t nbytes = 0;

	g_return_val_if_fail (line != NULL, FALSE);

	nbytes = strlen (line);
	return gsf_output_write (output, nbytes, line);
}
