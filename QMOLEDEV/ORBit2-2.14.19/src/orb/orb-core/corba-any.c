#include "config.h"
#include <orbit/orbit.h>
#include "orb-core-private.h"
#include <string.h>

#define PTR_PLUS(ptr, offset) \
	((gpointer) (((guchar *)(ptr)) + (offset)))
#define CONST_PTR_PLUS(ptr, offset) \
	((gconstpointer) (((const guchar *)(ptr)) + (offset)))

/** Adds the size of TYPE to the gpointer PTR. */
#define ADDSIZE(ptr, type) \
	((gpointer) (((guchar *)(ptr)) + sizeof (type)))

#define SKIP_ALIAS(tc) \
	while ((tc)->kind == CORBA_tk_alias) { (tc) = (tc)->subtypes [0]; }

static void
giop_byteswap (guchar       *outdata,
	       const guchar *data,
	       gulong        datalen)
{
	const guchar *source_ptr = data;
	guchar       *dest_ptr = (guchar *) outdata + datalen - 1;

	while (dest_ptr >= outdata)
		*dest_ptr-- = *source_ptr++;
}

size_t
ORBit_gather_alloc_info (CORBA_TypeCode tc)
{
	SKIP_ALIAS (tc);

	switch (tc->kind) {
	case CORBA_tk_long:
	case CORBA_tk_ulong:
	case CORBA_tk_enum:
		return sizeof (CORBA_long);
	case CORBA_tk_short:
	case CORBA_tk_ushort:
		return sizeof (CORBA_short);
	case CORBA_tk_float:
		return sizeof (CORBA_float);
	case CORBA_tk_double:
		return sizeof (CORBA_double);
	case CORBA_tk_boolean:
	case CORBA_tk_char:
	case CORBA_tk_octet:
		return sizeof (CORBA_octet);
	case CORBA_tk_any:
		return sizeof (CORBA_any);
	case CORBA_tk_TypeCode:
		return sizeof (CORBA_TypeCode);
	case CORBA_tk_Principal:
		return sizeof (CORBA_Principal);
	case CORBA_tk_objref:
		return sizeof (CORBA_Object);
	case CORBA_tk_except:
	case CORBA_tk_struct: {
		int i, sum;

		for (sum = i = 0; i < tc->sub_parts; i++) {
			sum = ALIGN_VALUE (sum, tc->subtypes[i]->c_align);
			sum += ORBit_gather_alloc_info (tc->subtypes[i]);
		}
		sum = ALIGN_VALUE (sum, tc->c_align);

		return sum;
	}
	case CORBA_tk_union: {
		int i, n, align, prevalign, prev, sum;
		sum = ORBit_gather_alloc_info (tc->discriminator);
		n = -1;
		align = 1;
		for (prev = prevalign = i = 0; i < tc->sub_parts; i++) {
			prevalign = align;
			align = tc->subtypes[i]->c_align;
			if (align > prevalign)
				n = i;

			prev = MAX (prev, ORBit_gather_alloc_info (tc->subtypes[i]));
		}
		if (n >= 0)
		  sum = ALIGN_VALUE (sum, tc->subtypes[n]->c_align);
		sum += prev;
		sum = ALIGN_VALUE (sum, tc->c_align);
		return sum;
	}
	case CORBA_tk_wstring:
	case CORBA_tk_string:
		return sizeof (char *);
	case CORBA_tk_sequence:
		return sizeof (CORBA_sequence_CORBA_octet);
	case CORBA_tk_array:
		return ORBit_gather_alloc_info (tc->subtypes[0]) * tc->length;
	case CORBA_tk_longlong:
	case CORBA_tk_ulonglong:
		return sizeof (CORBA_long_long);
	case CORBA_tk_longdouble:
		return sizeof (CORBA_long_double);
	case CORBA_tk_wchar:
		return sizeof (CORBA_wchar);
	case CORBA_tk_fixed:
		return sizeof (CORBA_fixed_d_s);
	case CORBA_tk_void:
	case CORBA_tk_null:
	default:
		return 0;
	}
}

/*
 * ORBit_marshal_value:
 * @buf: the send buffer.
 * @val: a pointer to the location of the value.
 * @tc:  the TypeCode indicating the type of the value.
 *
 * Marshals the value onto the buffer and changes @val to point 
 * to the location after the value.
 */
void
ORBit_marshal_value (GIOPSendBuffer *buf,
		     gconstpointer  *val,
		     CORBA_TypeCode  tc)
{
	CORBA_unsigned_long i, ulval;
	gconstpointer       subval;

	SKIP_ALIAS (tc);

	switch (tc->kind) {
	case CORBA_tk_wchar:
	case CORBA_tk_ushort:
	case CORBA_tk_short:
		giop_send_buffer_append_aligned (buf, *val, sizeof (CORBA_short));
		*val = ((guchar *)*val) + sizeof (CORBA_short);
		break;
	case CORBA_tk_enum:
	case CORBA_tk_long:
	case CORBA_tk_ulong:
		giop_send_buffer_append_aligned (buf, *val, sizeof (CORBA_long));
		*val = ((guchar *)*val) + sizeof (CORBA_long);
		break;
	case CORBA_tk_float:
		giop_send_buffer_append_aligned (buf, *val, sizeof (CORBA_float));
		*val = ((guchar *)*val) + sizeof (CORBA_float);
		break;
	case CORBA_tk_double:
		giop_send_buffer_append_aligned (buf, *val, sizeof (CORBA_double));
		*val = ((guchar *)*val) + sizeof (CORBA_double);
		break;
	case CORBA_tk_boolean:
	case CORBA_tk_char:
	case CORBA_tk_octet:
		giop_send_buffer_append (buf, *val, sizeof (CORBA_octet));
		*val = ((guchar *)*val) + sizeof (CORBA_octet);
		break;
	case CORBA_tk_any:
		ORBit_marshal_any (buf, *val);
		*val = ((guchar *)*val) + sizeof (CORBA_any);
		break;
	case CORBA_tk_Principal:
		ulval = *(CORBA_unsigned_long *) (*val);
		giop_send_buffer_append (buf, *val, sizeof (CORBA_unsigned_long));

		giop_send_buffer_append (buf,
					*(char **) ((char *)*val + sizeof (CORBA_unsigned_long)),
					ulval);
		*val = ((guchar *)*val) + sizeof (CORBA_Principal);
		break;
	case CORBA_tk_objref:
		ORBit_marshal_object (buf, *(CORBA_Object *)*val);
		*val = ((guchar *)*val) + sizeof (CORBA_Object);
		break;
	case CORBA_tk_TypeCode:
		ORBit_encode_CORBA_TypeCode (*(CORBA_TypeCode *)*val, buf);
		*val = ((guchar *)*val) + sizeof (CORBA_TypeCode);
		break;
	case CORBA_tk_except:
	case CORBA_tk_struct: {
		gconstpointer val0 = *val;
		int offset;
		for (i = offset = 0; i < tc->sub_parts; i++) {
			offset = ALIGN_VALUE (offset, tc->subtypes[i]->c_align);
			*val = PTR_PLUS (val0, offset);
			ORBit_marshal_value (buf, val, tc->subtypes[i]);
			offset += ORBit_gather_alloc_info (tc->subtypes[i]);
		}
		offset = ALIGN_VALUE (offset, tc->c_align);
		*val = PTR_PLUS (val0, offset);
		break;
	}
	case CORBA_tk_union: {
		gconstpointer   val0 = *val;
		gconstpointer	discrim, body;
		CORBA_TypeCode 	subtc;
		int             sz = 0;

		discrim = *val;
		ORBit_marshal_value (buf, val, tc->discriminator);

		subtc = ORBit_get_union_tag (tc, &discrim, FALSE);
		for (i = 0; i < tc->sub_parts; i++)
			sz = MAX (sz, ORBit_gather_alloc_info (tc->subtypes[i]));

		*val = PTR_PLUS (val0, ALIGN_VALUE (ORBit_gather_alloc_info (tc->discriminator),
					   tc->c_align));
		body = *val;
		ORBit_marshal_value (buf, &body, subtc);
		/* FIXME:
		 * WATCHOUT: end of subtc may not be end of union
		 */
		*val = PTR_PLUS (*val, ALIGN_VALUE (sz, tc->c_align));
		break;
	}
	case CORBA_tk_wstring: {
		CORBA_wchar endian_marker = 0xfeff;

		ulval = (CORBA_wstring_len (*(CORBA_wchar **)*val) + 1) * 2;
		giop_send_buffer_append_aligned (buf, &ulval,
						 sizeof (CORBA_unsigned_long));
		giop_send_buffer_append (buf, &endian_marker, 2);
		giop_send_buffer_append (buf, *(CORBA_wchar **)*val, ulval - 2);
		*val = ((guchar *)*val) + sizeof (CORBA_wchar *);
		break;
	}
	case CORBA_tk_string:
		giop_send_buffer_append_string (buf, *(char **)*val);
		*val = ((guchar *)*val) + sizeof (char *);
		break;
	case CORBA_tk_sequence: {
		const CORBA_sequence_CORBA_octet *sval;
		sval = *val;
		giop_send_buffer_align (buf, sizeof (CORBA_unsigned_long));
		giop_send_buffer_append (buf, &sval->_length,
					 sizeof (CORBA_unsigned_long));

		subval = sval->_buffer;

		switch (tc->subtypes[0]->kind) {
		case CORBA_tk_boolean:
		case CORBA_tk_char:
		case CORBA_tk_octet:
			giop_send_buffer_append (buf, subval, sval->_length);
			break;
		default:
			for (i = 0; i < sval->_length; i++)
				ORBit_marshal_value (buf, &subval, tc->subtypes[0]);
			break;
		}
		*val = ((guchar *)*val) + sizeof (CORBA_sequence_CORBA_octet);
		break;
	}
	case CORBA_tk_array:
		switch (tc->subtypes[0]->kind) {
		case CORBA_tk_boolean:
		case CORBA_tk_char:
		case CORBA_tk_octet:
			giop_send_buffer_append (buf, *val, tc->length);
			*val = ((guchar *)*val) + tc->length;
			break;
		default:
	  		for (i = 0; i < tc->length; i++)
				ORBit_marshal_value (buf, val, tc->subtypes[0]);
			break;
		}
		break;
	case CORBA_tk_longlong:
	case CORBA_tk_ulonglong:
		giop_send_buffer_append_aligned (buf, *val, sizeof (CORBA_long_long));
		*val = ((guchar *)*val) + sizeof (CORBA_long_long);
		break;
	case CORBA_tk_longdouble:
		giop_send_buffer_append_aligned (buf, *val, sizeof (CORBA_long_double));
		*val = ((guchar *)*val) + sizeof (CORBA_long_double);
		break;
	case CORBA_tk_fixed:
		/* XXX todo */
		g_error ("CORBA_fixed NYI");
		break;
	case CORBA_tk_null:
	case CORBA_tk_void:
		break;
	default:
		g_error ("Can't encode unknown type %d", tc->kind);
		break;
	}
}

static glong
ORBit_get_union_switch (CORBA_TypeCode  tc,
			gconstpointer  *val, 
			gboolean        update)
{
	glong retval = 0; /* Quiet gcc */

	SKIP_ALIAS (tc);

	switch (tc->kind) {
	case CORBA_tk_ulong:
	case CORBA_tk_long:
	case CORBA_tk_enum: {
		CORBA_long tmp;

		memcpy (&tmp, *val, sizeof (CORBA_long));
		retval = (glong) tmp;

		if (update)
			*val = ((guchar *)*val) + sizeof (CORBA_long);
		break;
	}
	case CORBA_tk_ushort:
	case CORBA_tk_short: {
		CORBA_short tmp;

		memcpy (&tmp, *val, sizeof (CORBA_short));
		retval = (glong) tmp;

		if (update)
			*val = ((guchar *)*val) + sizeof (CORBA_short);
		break;
	}
	case CORBA_tk_char:
	case CORBA_tk_boolean:
	case CORBA_tk_octet:
		retval = *(CORBA_octet *)*val;
		if (update)
			*val = ((guchar *)*val) + sizeof (CORBA_char);
		break;
	default:
		g_error ("Wow, some nut has passed us a weird "
			 "type[%d] as a union discriminator!", tc->kind);
	}

	return retval;
}

/* This function (and the one above it) exist for the
   sole purpose of finding out which CORBA_TypeCode a union discriminator value
   indicates.

   If {update} is TRUE, {*val} will be advanced by the native size
   of the descriminator type.

   Hairy stuff.
*/
CORBA_TypeCode
ORBit_get_union_tag (CORBA_TypeCode union_tc,
		     gconstpointer *val, 
		     gboolean       update)
{
	CORBA_TypeCode retval = CORBA_OBJECT_NIL;
	glong          discrim_val;
	int            i;

	discrim_val = ORBit_get_union_switch (
		union_tc->discriminator, val, update);

	for (i = 0; i < union_tc->sub_parts; i++) {
		if (i == union_tc->default_index)
			continue;

		if (union_tc->sublabels [i] == discrim_val) {
			retval = union_tc->subtypes[i];
			break;
		}
	}

	if (retval)
		return retval;

	else if (union_tc->default_index >= 0)
		return union_tc->subtypes[union_tc->default_index];

	else
		return TC_null;
}

void
ORBit_marshal_arg (GIOPSendBuffer *buf,
		   gconstpointer val,
		   CORBA_TypeCode tc)
{
	ORBit_marshal_value (buf, &val, tc);
}

void
ORBit_marshal_any (GIOPSendBuffer *buf, const CORBA_any *val)
{
	gconstpointer mval = val->_value;

	ORBit_encode_CORBA_TypeCode (val->_type, buf);

	ORBit_marshal_value (buf, &mval, val->_type);
}

/* FIXME: we need two versions of this - one for swap
 * endianness, and 1 for not */
gboolean
ORBit_demarshal_value (CORBA_TypeCode  tc,
		       gpointer       *val,
		       GIOPRecvBuffer *buf,
		       CORBA_ORB       orb)
{
	CORBA_long i;

	SKIP_ALIAS (tc);

	switch (tc->kind) {
	case CORBA_tk_short:
	case CORBA_tk_ushort:
	case CORBA_tk_wchar: {
		CORBA_unsigned_short *ptr;
		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_short));
		if ((buf->cur + sizeof (CORBA_short)) > buf->end)
			return TRUE;
		ptr = *val;
		*ptr = *(CORBA_unsigned_short *)buf->cur;
		if (giop_msg_conversion_needed (buf))
			*ptr = GUINT16_SWAP_LE_BE (*ptr);
		buf->cur += sizeof (CORBA_short);
		*val = ((guchar *)*val) + sizeof (CORBA_short);
		break;
	}
	case CORBA_tk_long:
	case CORBA_tk_ulong:
	case CORBA_tk_enum: {
		CORBA_unsigned_long *ptr;
		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_long));
		if ((buf->cur + sizeof (CORBA_long)) > buf->end)
			return TRUE;
		ptr = *val;
		*ptr = *(CORBA_unsigned_long *)buf->cur;
		if (giop_msg_conversion_needed (buf))
			*ptr = GUINT32_SWAP_LE_BE (*ptr);
		buf->cur += sizeof (CORBA_long);
		*val = ((guchar *)*val) + sizeof (CORBA_long);
		break;
	}
	case CORBA_tk_longlong:
	case CORBA_tk_ulonglong: {
		CORBA_unsigned_long_long *ptr;
		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_long_long));
		if ((buf->cur + sizeof (CORBA_long_long)) > buf->end)
			return TRUE;
		ptr = *val;
		*ptr = *(CORBA_unsigned_long_long *)buf->cur;
		if (giop_msg_conversion_needed (buf))
			*ptr = GUINT64_SWAP_LE_BE (*ptr);
		buf->cur += sizeof (CORBA_long_long);
		*val = ((guchar *)*val) + sizeof (CORBA_long_long);
		break;
	}
	case CORBA_tk_longdouble: {
		CORBA_long_double *ptr;
		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_long_double));
		if ((buf->cur + sizeof (CORBA_long_double)) > buf->end)
			return TRUE;
		ptr = *val;
		if (giop_msg_conversion_needed (buf))
			giop_byteswap ((guchar *)ptr, buf->cur, sizeof (CORBA_long_double));
		else
			*ptr = *(CORBA_long_double *)buf->cur;
		buf->cur += sizeof (CORBA_long_double);
		*val = ((guchar *)*val) + sizeof (CORBA_long_double);
		break;
	}
	case CORBA_tk_float: {
		CORBA_float *ptr;
		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_float));
		if ((buf->cur + sizeof (CORBA_float)) > buf->end)
			return TRUE;
		ptr = *val;
		if (giop_msg_conversion_needed (buf))
			giop_byteswap ((guchar *)ptr, buf->cur, sizeof (CORBA_float));
		else
			*ptr = *(CORBA_float *)buf->cur;
		buf->cur += sizeof (CORBA_float);

		*val = ((guchar *)*val) + sizeof (CORBA_float);
		break;
	}
	case CORBA_tk_double: {
		CORBA_double *ptr;
		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_double));
		if ((buf->cur + sizeof (CORBA_double)) > buf->end)
			return TRUE;
		ptr = *val;
		if (giop_msg_conversion_needed (buf))
			giop_byteswap ((guchar *)ptr, buf->cur, sizeof (CORBA_double));
		else
			*ptr = *(CORBA_double *)buf->cur;
		buf->cur += sizeof (CORBA_double);

		*val = ((guchar *)*val) + sizeof (CORBA_double);
		break;
	}
	case CORBA_tk_boolean:
	case CORBA_tk_char:
	case CORBA_tk_octet: {
		CORBA_octet *ptr;
		if ((buf->cur + sizeof (CORBA_octet)) > buf->end)
			return TRUE;
		ptr = *val;
		*ptr = *buf->cur;
		buf->cur++;
      
		*val = ((guchar *)*val) + sizeof (CORBA_octet);
		break;
	}
	case CORBA_tk_any: {
		CORBA_any *decoded;

		decoded = *val;
		decoded->_release = CORBA_FALSE;
		if (ORBit_demarshal_any (buf, decoded, orb))
			return TRUE;
		*val = ((guchar *)*val) + sizeof (CORBA_any);
		break;
	}
	case CORBA_tk_Principal: {
		CORBA_Principal *p;

		p = *val;
		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_long));
		p->_release = TRUE;
		if ((buf->cur + sizeof (CORBA_unsigned_long)) > buf->end)
			return TRUE;
		if (giop_msg_conversion_needed (buf))
			p->_length = GUINT32_SWAP_LE_BE (*(CORBA_unsigned_long *)buf->cur);
		else
			p->_length = *(CORBA_unsigned_long *)buf->cur;
		buf->cur += sizeof (CORBA_unsigned_long);
		if ((buf->cur + p->_length) > buf->end
		    || (buf->cur + p->_length) < buf->cur)
			return TRUE;
		p->_buffer = ORBit_alloc_simple (p->_length);
		memcpy (p->_buffer, buf->cur, p->_length);
		buf->cur += p->_length;
		*val = ((guchar *)*val) + sizeof (CORBA_sequence_CORBA_octet);
		break;
	}
	case CORBA_tk_objref:
		if (ORBit_demarshal_object ((CORBA_Object *)*val, buf, orb))
			return TRUE;
		*val = ((guchar *)*val) + sizeof (CORBA_Object);
		break;
	case CORBA_tk_TypeCode:
		if (ORBit_decode_CORBA_TypeCode (*val, buf))
			return TRUE;
		*val = ((guchar *)*val) + sizeof (CORBA_TypeCode);
		break;
	case CORBA_tk_except:
	case CORBA_tk_struct: {
		int offset;
		gpointer val0 = *val;
		for (i = offset = 0; i < tc->sub_parts; i++) {
			offset = ALIGN_VALUE (offset, tc->subtypes[i]->c_align);
			*val = PTR_PLUS (val0, offset);
			if (ORBit_demarshal_value (tc->subtypes[i], val, buf, orb))
				return TRUE;
			offset += ORBit_gather_alloc_info (tc->subtypes[i]);
		}
		offset = ALIGN_VALUE (offset, tc->c_align);
		*val = PTR_PLUS (val0, offset);
		break;
	}
	case CORBA_tk_union: {
		gpointer        val0 = *val;
		CORBA_TypeCode  subtc;
		gpointer        discrim;
		gpointer        body;
		int	        sz = 0;

		discrim = *val;
		if (ORBit_demarshal_value (tc->discriminator, val, buf, orb))
			return TRUE;

		subtc = ORBit_get_union_tag (tc, (gconstpointer*)&discrim, FALSE);
		for (i = 0; i < tc->sub_parts; i++)
			sz = MAX (sz, ORBit_gather_alloc_info (tc->subtypes[i]));

		*val = PTR_PLUS (val0, ALIGN_VALUE (ORBit_gather_alloc_info (tc->discriminator),
					   tc->c_align));
		body = *val;
		if (ORBit_demarshal_value (subtc, &body, buf, orb))
			return TRUE;

		/* WATCHOUT: end subtc body may not be end of union */
		*val = PTR_PLUS (*val, ALIGN_VALUE (sz, tc->c_align));
		break;
	}
	case CORBA_tk_string:
		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_long));
		if ((buf->cur + sizeof (CORBA_long)) > buf->end)
			return TRUE;
		i = *(CORBA_unsigned_long *)buf->cur;
		if (giop_msg_conversion_needed (buf))
			i = GUINT32_SWAP_LE_BE (i);
		buf->cur += sizeof (CORBA_unsigned_long);
		if ((buf->cur + i) > buf->end
		    || (buf->cur + i) < buf->cur)
			return TRUE;
		*(char **)*val = CORBA_string_dup ((char *)buf->cur);
		*val = ((guchar *)*val) + sizeof (CORBA_char *);
		buf->cur += i;
		break;
	case CORBA_tk_wstring: {
		CORBA_wchar endian_marker = 0, *ptr;

		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_long));
		if ((buf->cur + sizeof (CORBA_long)) > buf->end)
			return TRUE;
		i = *(CORBA_unsigned_long *)buf->cur;
		if (giop_msg_conversion_needed (buf))
			i = GUINT32_SWAP_LE_BE (i);
		buf->cur += sizeof (CORBA_unsigned_long);
		if ((buf->cur + i) > buf->end
		    || (buf->cur + i) < buf->cur)
			return TRUE;
		if (i >= 2) {
			endian_marker = *(CORBA_wchar *)buf->cur;
			if (endian_marker != 0xfeff &&
			    endian_marker != 0xfffe)
				endian_marker = 0;
			else {
				i -= 2;
				buf->cur += 2;
			}
		}
		if (!endian_marker) {
			((unsigned char *)&endian_marker)[0] = 0xfe;
			((unsigned char *)&endian_marker)[1] = 0xff;
		}
		ptr = CORBA_wstring_alloc ((i + 1) / 2);
		*(CORBA_wchar **)*val = ptr;
		if (endian_marker == 0xfffe) {
			while(i >= 2) {
				*ptr++ = GUINT16_SWAP_LE_BE(
				         	*((CORBA_wchar *)buf->cur));
				buf->cur += 2;
				i -= 2;
			}
			*ptr = 0;
		} else {
			memcpy(ptr, buf->cur, i);
			ptr[(i + 1) / 2] = 0;
			buf->cur += i;
			i = 0;
		}
		*val = ((guchar *)*val) + sizeof (CORBA_wchar *);
		buf->cur += i;
		break;
	}
	case CORBA_tk_sequence: {
		CORBA_sequence_CORBA_octet *p;
		gpointer subval;

		p = *val;
		p->_release = TRUE;
		buf->cur = ALIGN_ADDRESS (buf->cur, sizeof (CORBA_long));
		if ((buf->cur + sizeof (CORBA_long)) > buf->end)
			return TRUE;
		if (giop_msg_conversion_needed (buf))
			p->_length = GUINT32_SWAP_LE_BE (*(CORBA_unsigned_long *)buf->cur);
		else
			p->_length = *(CORBA_unsigned_long *)buf->cur;
		buf->cur += sizeof (CORBA_long);

		p->_maximum = p->_length;
		if (p->_length == 0)
			p->_buffer = NULL;

		else if (tc->subtypes[0]->kind == CORBA_tk_octet ||
			 tc->subtypes[0]->kind == CORBA_tk_boolean ||
			 tc->subtypes[0]->kind == CORBA_tk_char) {
			/* This special-casing could be taken further to apply to
			   all atoms... */
			if ((buf->cur + p->_length) > buf->end ||
			    (buf->cur + p->_length) < buf->cur)
				return TRUE;
			p->_buffer = ORBit_alloc_simple (p->_length);
			memcpy (p->_buffer, buf->cur, p->_length);
			buf->cur = ((guchar *)buf->cur) + p->_length;
		} else {
			CORBA_unsigned_long alloc = 4096;

			p->_buffer = ORBit_alloc_tcval (tc->subtypes[0],
							MIN (p->_length, alloc));
			subval = p->_buffer;

			for (i = 0; i < p->_length; i++) {
				if (i == alloc) {
					size_t delta = (guchar *)subval - (guchar *)p->_buffer;

					p->_buffer = ORBit_realloc_tcval (
						p->_buffer, tc->subtypes [0],
						alloc, MIN (alloc * 2, p->_length));
					alloc = alloc * 2; /* exponential */
					subval = p->_buffer + delta;
				}

				if (ORBit_demarshal_value (tc->subtypes[0], &subval,
							   buf, orb)) {
					CORBA_free (p->_buffer);
					p->_buffer = NULL;
					p->_length = 0;
					return TRUE;
				}
			}
		}

		*val = ((guchar *)*val) + sizeof (CORBA_sequence_CORBA_octet);
		break;
	}
	case CORBA_tk_array:
		for (i = 0; i < tc->length; i++)
			if (ORBit_demarshal_value (tc->subtypes[0], val, buf, orb))
				return TRUE;
		break;
	case CORBA_tk_void:
	case CORBA_tk_null:
		break;
	case CORBA_tk_fixed:
	default:
		return TRUE;
		break;
	}

	return FALSE;
}

gpointer
ORBit_demarshal_arg (GIOPRecvBuffer *buf,
		     CORBA_TypeCode tc,
		     CORBA_ORB      orb)
{
	gpointer retval, val;

	retval = val = ORBit_alloc_by_tc (tc);

	if (ORBit_demarshal_value (tc, &val, buf, orb))
	{
	    CORBA_free (retval);
	    return NULL;
	}

	return retval;
}

gboolean
ORBit_demarshal_any (GIOPRecvBuffer *buf,
		     CORBA_any      *retval,
		     CORBA_ORB       orb)
{
	gpointer val;

	CORBA_any_set_release (retval, CORBA_TRUE);

	if (ORBit_decode_CORBA_TypeCode (&retval->_type, buf))
		return TRUE;

	val = retval->_value = ORBit_alloc_by_tc (retval->_type);
	if (ORBit_demarshal_value (retval->_type, &val, buf, orb))
		return TRUE;

	return FALSE;
}

gpointer
CORBA_any__freekids (gpointer mem, gpointer dat)
{
	CORBA_any *t;

	t = mem;
	if (t->_type)
		ORBit_RootObject_release_T (
			(ORBit_RootObject) t->_type);

	if (t->_release)
		ORBit_free_T (t->_value);

	return t + 1;
}

CORBA_any *
CORBA_any__alloc (void)
{
	return ORBit_alloc_by_tc (TC_CORBA_any);
}

void
ORBit_copy_value_core (gconstpointer *val,
		       gpointer      *newval,
		       CORBA_TypeCode tc)
{
	CORBA_long i;
	gconstpointer pval1; 
	gpointer pval2;

	SKIP_ALIAS (tc);

	switch (tc->kind) {
	case CORBA_tk_wchar:
	case CORBA_tk_short:
	case CORBA_tk_ushort:
		*(CORBA_short *)*newval = *(CORBA_short *)*val;
		*val = ((guchar *)*val) + sizeof (CORBA_short);
		*newval = ((guchar *)*newval) + sizeof (CORBA_short);
		break;
	case CORBA_tk_enum:
	case CORBA_tk_long:
	case CORBA_tk_ulong:
		*(CORBA_long *)*newval = *(CORBA_long *)*val;
		*val = ((guchar *)*val) + sizeof (CORBA_long);
		*newval = ((guchar *)*newval) + sizeof (CORBA_long);
		break;
	case CORBA_tk_longlong:
	case CORBA_tk_ulonglong:
		*(CORBA_long_long *)*newval = *(CORBA_long_long *)*val;
		*val = ((guchar *)*val) + sizeof (CORBA_long_long);
		*newval = ((guchar *)*newval) + sizeof (CORBA_long_long);
		break;
	case CORBA_tk_longdouble:
		*(CORBA_long_double *)*newval = *(CORBA_long_double *)*val;
		*val = ((guchar *)*val) + sizeof (CORBA_long_double);
		*newval = ((guchar *)*newval) + sizeof (CORBA_long_double);
		break;
	case CORBA_tk_float:
		*(CORBA_long *)*newval = *(CORBA_long *)*val;
		*val = ((guchar *)*val) + sizeof (CORBA_float);
		*newval = ((guchar *)*newval) + sizeof (CORBA_float);
		break;
	case CORBA_tk_double:
		*(CORBA_double *)*newval = *(CORBA_double *)*val;
		*val = ((guchar *)*val) + sizeof (CORBA_double);
		*newval = ((guchar *)*newval) + sizeof (CORBA_double);
		break;
	case CORBA_tk_boolean:
	case CORBA_tk_char:
	case CORBA_tk_octet:
		*(CORBA_octet *)*newval = *(CORBA_octet *)*val;
		*val = ((guchar *)*val) + sizeof (CORBA_octet);
		*newval = ((guchar *)*newval) + sizeof (CORBA_octet);
		break;
	case CORBA_tk_any: {
		const CORBA_any *oldany;
		CORBA_any *newany;
		oldany = *val;
		newany = *newval;
		newany->_type = ORBit_RootObject_duplicate (oldany->_type);
		newany->_value = ORBit_copy_value (oldany->_value, oldany->_type);
		newany->_release = CORBA_TRUE;
		*val = ((guchar *)*val) + sizeof (CORBA_any);
		*newval = ((guchar *)*newval) + sizeof (CORBA_any);
		break;
	}
	case CORBA_tk_Principal:
		*(CORBA_Principal *)*newval = *(CORBA_Principal *)*val;
		((CORBA_Principal *)*newval)->_buffer =
			CORBA_sequence_CORBA_octet_allocbuf (((CORBA_Principal *)*newval)->_length);
		((CORBA_Principal *)*newval)->_release = CORBA_TRUE;
		memcpy (((CORBA_Principal *)*newval)->_buffer,
		       ((CORBA_Principal *)*val)->_buffer,
		       ((CORBA_Principal *)*val)->_length);
		*val = ((guchar *)*val) + sizeof (CORBA_Principal);
		*newval = ((guchar *)*newval) + sizeof (CORBA_Principal);
		break;
	case CORBA_tk_TypeCode:
	case CORBA_tk_objref:
		*(CORBA_Object *)*newval = ORBit_RootObject_duplicate (*(CORBA_Object *)*val);
		*val = ((guchar *)*val) + sizeof (CORBA_Object);
		*newval = ((guchar *)*newval) + sizeof (CORBA_Object);
		break;
	case CORBA_tk_struct:
	case CORBA_tk_except: {
		int offset;
		gconstpointer val0 = *val;
		gpointer newval0 = *newval;

		for (i = offset = 0; i < tc->sub_parts; i++) {
			offset = ALIGN_VALUE (offset, tc->subtypes[i]->c_align);
			*val = PTR_PLUS (val0, offset);
			*newval = PTR_PLUS (newval0, offset);
			ORBit_copy_value_core (val, newval, tc->subtypes[i]);
			offset += ORBit_gather_alloc_info (tc->subtypes[i]);
		}
		offset = ALIGN_VALUE (offset, tc->c_align);
		*val = PTR_PLUS (val0, offset);
		*newval = PTR_PLUS (newval0, offset);
		break;
	}
	case CORBA_tk_union: {
		gconstpointer val0 = *val;
		gpointer newval0 = *newval;
		CORBA_TypeCode utc;
		gint	       union_align = tc->c_align;
		size_t	       union_size = ORBit_gather_alloc_info (tc);
		size_t         aligned_size;

		pval1 = *val;
		pval2 = *newval;

		utc = ORBit_get_union_tag (tc, (gconstpointer *)val, FALSE);

		ORBit_copy_value_core (&pval1, &pval2, tc->discriminator);

		aligned_size = ALIGN_VALUE (ORBit_gather_alloc_info (tc->discriminator),
					    union_align);
		pval1 = PTR_PLUS (val0, aligned_size);
		pval2 = PTR_PLUS (newval0, aligned_size);

		ORBit_copy_value_core (&pval1, &pval2, utc);

		*val = ((guchar *)*val) + union_size;
		*newval = ((guchar *)*newval) + union_size;
		break;
	}
	case CORBA_tk_wstring:
	case CORBA_tk_string:
		*(CORBA_char **)*newval = CORBA_string_dup (*(CORBA_char **)*val);
		*val = ((guchar *)*val) + sizeof (CORBA_char *);
		*newval = ((guchar *)*newval) + sizeof (CORBA_char *);
		break;
	case CORBA_tk_sequence:
		((CORBA_Principal *)*newval)->_release = CORBA_TRUE;
		((CORBA_Principal *)*newval)->_length =
			((CORBA_Principal *)*newval)->_maximum =
			((CORBA_Principal *)*val)->_length;
		((CORBA_Principal *)*newval)->_buffer = pval2 =
			ORBit_alloc_tcval (tc->subtypes[0],
					  ((CORBA_Principal *)*val)->_length);
		pval1 = ((CORBA_Principal *)*val)->_buffer;
	
		for (i = 0; i < ((CORBA_Principal *)*newval)->_length; i++)
			ORBit_copy_value_core (&pval1, &pval2, tc->subtypes [0]);
		*val = ((guchar *)*val) + sizeof (CORBA_sequence_CORBA_octet);
		*newval = ((guchar *)*newval) + sizeof (CORBA_sequence_CORBA_octet);
		break;
	case CORBA_tk_array:
		for (i = 0; i < tc->length; i++)
			ORBit_copy_value_core (val, newval, tc->subtypes[0]);
		break;
	case CORBA_tk_fixed:
		g_error ("CORBA_fixed NYI!");
		break;
	case CORBA_tk_void:
	case CORBA_tk_null:
		break;
	default:
		g_error ("Can't handle copy of value kind %d", tc->kind);
	}
}

gpointer
ORBit_copy_value (gconstpointer value, CORBA_TypeCode tc)
{
	gpointer retval, newval;

	if (!value)
		return NULL;

	retval = newval = ORBit_alloc_by_tc (tc);
	ORBit_copy_value_core (&value, &newval, tc);

	return retval;
}

void
CORBA_any__copy (CORBA_any *out, const CORBA_any *in)
{
	out->_type = ORBit_RootObject_duplicate (in->_type);
	out->_value = ORBit_copy_value (in->_value, in->_type);
	out->_release = CORBA_TRUE;
}

#define ALIGN_COMPARE(a,b,tk,type,align)	\
	case CORBA_tk_##tk:			\
		ret = *(CORBA_##type *) *a == *(CORBA_##type *) *b;	\
		*a = ((guchar *) *a) + sizeof (CORBA_##type);		\
		*b = ((guchar *) *b) + sizeof (CORBA_##type);		\
		return ret

CORBA_boolean
ORBit_value_equivalent (gpointer *a, gpointer *b,
			CORBA_TypeCode tc,
			CORBA_Environment *ev)
{
	gboolean ret;
	int i;

	SKIP_ALIAS (tc);

	switch (tc->kind) {
	case CORBA_tk_null:
	case CORBA_tk_void:
		return TRUE;

		ALIGN_COMPARE (a, b, short,  short, ORBIT_ALIGNOF_CORBA_SHORT);
		ALIGN_COMPARE (a, b, ushort, short, ORBIT_ALIGNOF_CORBA_SHORT);
		ALIGN_COMPARE (a, b, wchar,  short, ORBIT_ALIGNOF_CORBA_SHORT);

		ALIGN_COMPARE (a, b, enum, long,  ORBIT_ALIGNOF_CORBA_LONG);
		ALIGN_COMPARE (a, b, long, long,  ORBIT_ALIGNOF_CORBA_LONG);
		ALIGN_COMPARE (a, b, ulong, long, ORBIT_ALIGNOF_CORBA_LONG);

		ALIGN_COMPARE (a, b, longlong, long_long,  ORBIT_ALIGNOF_CORBA_LONG_LONG);
		ALIGN_COMPARE (a, b, ulonglong, long_long, ORBIT_ALIGNOF_CORBA_LONG_LONG);

		ALIGN_COMPARE (a, b, longdouble, long_double, ORBIT_ALIGNOF_CORBA_LONG_DOUBLE);

		ALIGN_COMPARE (a, b, float, float,   ORBIT_ALIGNOF_CORBA_FLOAT);
		ALIGN_COMPARE (a, b, double, double, ORBIT_ALIGNOF_CORBA_DOUBLE);

		ALIGN_COMPARE (a, b, char,    octet, ORBIT_ALIGNOF_CORBA_OCTET);
		ALIGN_COMPARE (a, b, octet,   octet, ORBIT_ALIGNOF_CORBA_OCTET);

	case CORBA_tk_boolean: {
		gboolean ba, bb;

		ba = *(CORBA_octet *) *a;
		bb = *(CORBA_octet *) *b;
		*a = ((guchar *) *a) + sizeof (CORBA_octet);
		*b = ((guchar *) *b) + sizeof (CORBA_octet);

		return (ba && bb) || (!ba && !bb);
	}

	case CORBA_tk_string:
		ret = !strcmp (*(char **)*a, *(char **)*b);
		*a = ((guchar *) *a) + sizeof (CORBA_char *);
		*b = ((guchar *) *b) + sizeof (CORBA_char *);
		return ret;

	case CORBA_tk_wstring:
		g_warning ("wstring totaly broken");
  		return FALSE;

	case CORBA_tk_TypeCode:
	case CORBA_tk_objref:
		ret = CORBA_Object_is_equivalent (*a, *b, ev);
		*a = ((guchar *) *a) + sizeof (CORBA_Object);
		*b = ((guchar *) *b) + sizeof (CORBA_Object);
		return ret;

	case CORBA_tk_any: {
		CORBA_any *any_a, *any_b;

		any_a = *((CORBA_any **) *a);
		any_b = *((CORBA_any **) *b);

		ret = ORBit_any_equivalent (any_a, any_b, ev);

		*a = ((guchar *) *a) + sizeof (CORBA_any *);
		*b = ((guchar *) *b) + sizeof (CORBA_any *);

		return ret;
	}

	case CORBA_tk_struct:
	case CORBA_tk_except: {
		int offset;
		gpointer a0 = *a;
		gpointer b0 = *b;
		int i;

		for (i = offset = 0; i < tc->sub_parts; i++) {
			offset = ALIGN_VALUE (offset, tc->subtypes[i]->c_align);
			*a = PTR_PLUS (a0, offset);
			*b = PTR_PLUS (b0, offset);
			if (!ORBit_value_equivalent (a, b, tc->subtypes [i], ev))
				return FALSE;
			offset += ORBit_gather_alloc_info (tc->subtypes[i]);
		}

		offset = ALIGN_VALUE (offset, tc->c_align);
		*a = PTR_PLUS (a0, offset);
		*b = PTR_PLUS (b0, offset);
		return TRUE;
	}

	case CORBA_tk_sequence: {
		CORBA_Principal *ap, *bp;
		gpointer a_val, b_val;

		ap = (CORBA_Principal *) *a;
		bp = (CORBA_Principal *) *b;

		if (ap->_length != bp->_length)
			return FALSE;

		a_val = ap->_buffer;
		b_val = bp->_buffer;

		for (i = 0; i < ap->_length; i++) {
			if (!ORBit_value_equivalent (&a_val, &b_val, tc->subtypes [0], ev))
				return FALSE;
		}
		*a = ((guchar *) *a) + sizeof (CORBA_sequence_CORBA_octet);
		*b = ((guchar *) *b) + sizeof (CORBA_sequence_CORBA_octet);
		return TRUE;
	}

	case CORBA_tk_union: {
		CORBA_TypeCode utc_a, utc_b;
		gint           union_align = tc->c_align;
		size_t         union_size = ORBit_gather_alloc_info (tc);
		gpointer       a_orig, b_orig;
		size_t         aligned_size;

		a_orig = *a;
		b_orig = *b;

		utc_a = ORBit_get_union_tag (tc, (gconstpointer *)a, FALSE);
		utc_b = ORBit_get_union_tag (tc, (gconstpointer *)b, FALSE);

		if (!CORBA_TypeCode_equal (utc_a, utc_b, ev))
			return FALSE;

		if (!ORBit_value_equivalent (a, b, tc->discriminator, ev))
			return FALSE;

		aligned_size = ALIGN_VALUE (ORBit_gather_alloc_info (tc->discriminator),
					   union_align);
		*a = PTR_PLUS (a_orig, aligned_size);
		*b = PTR_PLUS (b_orig, aligned_size);
		if (!ORBit_value_equivalent (a, b, utc_a, ev))
			return FALSE;

		*a = ((guchar *) a_orig) + ALIGN_VALUE (union_size, union_align);
		*b = ((guchar *) b_orig) + ALIGN_VALUE (union_size, union_align);
		return TRUE;
	}

	case CORBA_tk_array:
		for (i = 0; i < tc->length; i++) {
			if (!ORBit_value_equivalent (a, b, tc->subtypes [0], ev))
				return FALSE;
		}
		return TRUE;

	default:
		g_warning ("ORBit_value_equivalent unimplemented");
		return FALSE;
	};
}

/*
 * Compares the typecodes of each any
 */
CORBA_boolean
ORBit_any_equivalent (CORBA_any *obj, CORBA_any *any, CORBA_Environment *ev)
{
	gpointer a, b;

	/* Is this correct ? */
	if (obj == NULL &&
	    any == NULL)
		return TRUE;
	if (!obj || !any)
		return FALSE;
	if (!obj->_type || !any->_type) {
		CORBA_exception_set_system (
			ev, ex_CORBA_BAD_PARAM, CORBA_COMPLETED_NO);
		return FALSE;
	}

	if (!CORBA_TypeCode_equal (obj->_type, any->_type, ev))
		return FALSE;

	if (ev->_major != CORBA_NO_EXCEPTION)
		return FALSE;
	
	a = obj->_value;
	b = any->_value;

	return ORBit_value_equivalent (&a, &b, any->_type, ev);
}

/* Friendly sequence allocators */

#define BASE_TYPES \
	     CORBA_tk_short: \
	case CORBA_tk_long: \
	case CORBA_tk_enum: \
	case CORBA_tk_ushort: \
	case CORBA_tk_ulong: \
	case CORBA_tk_float: \
	case CORBA_tk_double: \
	case CORBA_tk_boolean: \
	case CORBA_tk_char: \
	case CORBA_tk_octet: \
	case CORBA_tk_longlong: \
	case CORBA_tk_ulonglong: \
	case CORBA_tk_longdouble: \
	case CORBA_tk_wchar

gpointer
ORBit_sequence_alloc (CORBA_TypeCode      sequence_tc,
		      CORBA_unsigned_long length)
{
	CORBA_TypeCode tc = sequence_tc;
  	CORBA_sequence_CORBA_octet *seq;

	g_return_val_if_fail (sequence_tc != NULL, NULL);

	SKIP_ALIAS (tc);
	g_return_val_if_fail (tc->kind == CORBA_tk_sequence, NULL);
	
	seq = ORBit_alloc_by_tc (sequence_tc);
	seq->_buffer = ORBit_small_allocbuf (tc, length);
	seq->_length  = length;
	seq->_maximum = length;
	
	CORBA_sequence_set_release (seq, CORBA_TRUE);

	g_assert (ORBit_alloc_get_tcval (seq) == sequence_tc);

	return seq;
}

void
ORBit_sequence_set_size (gpointer            sequence,
			 CORBA_unsigned_long length)
{
	CORBA_TypeCode tc, subtc;
	CORBA_sequence_CORBA_octet *seq = sequence;

	g_return_if_fail (seq != NULL);
	g_return_if_fail (seq->_length <= seq->_maximum);

	if (seq->_length == length)
		return;

	tc = ORBit_alloc_get_tcval (sequence);
	SKIP_ALIAS (tc);
	g_return_if_fail (tc->kind == CORBA_tk_sequence);
	subtc = tc->subtypes[0];

	if (length < seq->_length) {
		guint i;

		switch (subtc->kind) {
		case BASE_TYPES: /* leave some in-line values */
			break;
		default: {
			guint element_size = ORBit_gather_alloc_info (subtc);

			for (i = length; i < seq->_length; i++)
				ORBit_freekids_via_TypeCode
					(subtc, (guchar *)seq->_buffer + i * element_size);

			/* Don't trust the API user not to poke at it again */
			memset ((guchar *)seq->_buffer + length * element_size,
				0, (seq->_length - length) * element_size);
			break;
		}
		}
	} else {
		if (length > seq->_maximum) {
			guint new_len = MAX (length, seq->_maximum * 2);

			seq->_buffer = ORBit_realloc_tcval
				(seq->_buffer, subtc,
				 seq->_maximum, new_len);
			seq->_maximum = new_len;
		}
	}
	seq->_length = length;
}

void
ORBit_sequence_append (gpointer      sequence,
		       gconstpointer element)
{
	guint element_size;
	guchar *dest;
	CORBA_TypeCode tc, subtc;
  	CORBA_sequence_CORBA_octet *seq = sequence;

	g_return_if_fail (seq != NULL);
	g_return_if_fail (seq->_length <= seq->_maximum);

	tc = ORBit_alloc_get_tcval (sequence);
	SKIP_ALIAS (tc);
	subtc = tc->subtypes [0];
	g_return_if_fail (tc->kind == CORBA_tk_sequence);

	if (seq->_length == seq->_maximum) {
		guint new_len = MAX (2, (seq->_maximum * 2));

		seq->_buffer = ORBit_realloc_tcval
			(seq->_buffer, subtc,
			 seq->_maximum, new_len );
		seq->_maximum = new_len;
	}

	element_size = ORBit_gather_alloc_info (subtc);
	
	dest = seq->_buffer;
	dest += element_size * seq->_length;
	ORBit_copy_value_core (&element, (gpointer)&dest, subtc);

	seq->_length++;
}

void
ORBit_sequence_remove (gpointer sequence,
                       guint    idx)
{
	guint element_size, remaining;
	guchar *elem;
	CORBA_TypeCode tc, subtc;
  	CORBA_sequence_CORBA_octet *seq = sequence;

	tc = ORBit_alloc_get_tcval (sequence);
	SKIP_ALIAS (tc);
	g_return_if_fail (tc->kind == CORBA_tk_sequence);
	g_return_if_fail (seq != NULL);
	g_return_if_fail (seq->_length <= seq->_maximum);
	g_return_if_fail (idx < seq->_length);

	subtc = tc->subtypes [0];
	element_size = ORBit_gather_alloc_info (subtc);
	elem = seq->_buffer + element_size*idx;
        remaining = seq->_length - idx - 1;
        ORBit_freekids_via_TypeCode (subtc, elem);
          /* shift remaining elements into free slot */
        memmove (elem, elem + element_size, element_size*remaining);
          /* zero last element */
        memset (elem + element_size*remaining, 0, element_size);

	seq->_length--;
}

void
ORBit_sequence_concat (gpointer      sequence,
		       gconstpointer append)
{
	gint  i;	
	guint element_size;
	guchar *src;
	CORBA_TypeCode tc, subtc;
  	CORBA_sequence_CORBA_octet *seq = (CORBA_sequence_CORBA_octet *)append;

	g_return_if_fail (seq != NULL);
	g_return_if_fail (seq->_length <= seq->_maximum);

	tc = ORBit_alloc_get_tcval (sequence);
	SKIP_ALIAS (tc);
	subtc = tc->subtypes [0];
	g_return_if_fail (tc->kind == CORBA_tk_sequence);

	element_size = ORBit_gather_alloc_info (subtc);

	src = seq->_buffer;

	for (i = 0; i < seq->_length; ++i, src += element_size) 
		ORBit_sequence_append (sequence, (gpointer)src);
}
