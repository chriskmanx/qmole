/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */
/* egg-asn1x.c - ASN.1/DER parse and coding routines

   Copyright (C) 2009 Stefan Walter

   The Gnome Keyring Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Keyring Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Author: Stef Walter <stef@memberwebs.com>
*/

/*
 * Some portions are:
 *
 *      Copyright (C) 2004, 2006, 2008, 2009 Free Software Foundation
 *      Copyright (C) 2002 Fabio Fiorina
 *
 * This file is part of LIBTASN1.
 *
 * The LIBTASN1 library is free software; you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA
 */

#include "config.h"

#include "egg-asn1x.h"

#include <libtasn1.h>

#include <stdlib.h>
#include <string.h>

/* From libtasn1's int.h */
enum {
	TYPE_CONSTANT = 1,
	TYPE_IDENTIFIER = 2,
	TYPE_INTEGER = 3,
	TYPE_BOOLEAN = 4,
	TYPE_SEQUENCE = 5,
	TYPE_BIT_STRING = 6,
	TYPE_OCTET_STRING = 7,
	TYPE_TAG = 8,
	TYPE_DEFAULT = 9,
	TYPE_SIZE = 10,
	TYPE_SEQUENCE_OF = 11,
	TYPE_OBJECT_ID = 12,
	TYPE_ANY = 13,
	TYPE_SET = 14,
	TYPE_SET_OF = 15,
	TYPE_DEFINITIONS = 16,
	TYPE_TIME = 17,
	TYPE_CHOICE = 18,
	TYPE_IMPORTS = 19,
	TYPE_NULL = 20,
	TYPE_ENUMERATED = 21,
	TYPE_GENERALSTRING = 27
};

enum {
	FLAG_UNIVERSAL = (1<<8),
	FLAG_PRIVATE = (1<<9),
	FLAG_APPLICATION = (1<<10),
	FLAG_EXPLICIT = (1<<11),
	FLAG_IMPLICIT = (1<<12),
	FLAG_TAG = (1<<13),
	FLAG_OPTION = (1<<14),
	FLAG_DEFAULT = (1<<15),
	FLAG_TRUE = (1<<16),
	FLAG_FALSE = (1<<17),
	FLAG_LIST = (1<<18),
	FLAG_MIN_MAX = (1<<19),
	FLAG_1_PARAM = (1<<20),
	FLAG_SIZE = (1<<21),
	FLAG_DEFINED_BY = (1<<22),
	FLAG_GENERALIZED = (1<<23),
	FLAG_UTC = (1<<24),
	FLAG_IMPORTS = (1<<25),
	FLAG_NOT_USED = (1<<26),
	FLAG_SET = (1<<27),
	FLAG_ASSIGN = (1<<28),
	FLAG_DOWN = (1<<29),
	FLAG_RIGHT = (1<<30),
};

typedef struct _Aenc Aenc;
typedef struct _Atlv Atlv;
typedef struct _Anode Anode;
typedef struct _Abuf Abuf;
typedef struct _Abits Abits;

struct _Aenc {
	EggAsn1xEncoder encoder;
	gpointer data;
};

struct _Atlv {
	guchar cls;
	gulong tag;
	gint off;
	gint oft;
	gint len;
	const guchar *buf;
	const guchar *end;
};

struct _Anode {
	const ASN1_ARRAY_TYPE *def;
	const ASN1_ARRAY_TYPE *join;
	GList *opts;

	Atlv *tlv;
	Aenc *enc;

	gpointer user_data;
	GDestroyNotify destroy;

	gchar* failure;

	gint chosen : 1;
};

struct _Abuf {
	guchar* data;
	gsize n_data;
	gpointer user_data;
};

struct _Abits {
	guint n_bits;
	guchar *bits;
	GDestroyNotify destroy;
};

/* Forward Declarations */
static gboolean anode_decode_anything (GNode*, Atlv*);
static gboolean anode_decode_anything_for_flags (GNode *, Atlv*, gint);
static gboolean anode_validate_anything (GNode*);
static gboolean anode_encode_prepare (GNode*, gboolean want);

static gint
atoin (const char *p, gint digits)
{
	gint ret = 0, base = 1;
	while(--digits >= 0) {
		if (p[digits] < '0' || p[digits] > '9')
			return -1;
		ret += (p[digits] - '0') * base;
		base *= 10;
	}
	return ret;
}

static GNode*
anode_new (const ASN1_ARRAY_TYPE *def)
{
	Anode *an = g_slice_new0 (Anode);
	an->def = def;
	return g_node_new (an);
}

static gpointer
anode_copy_func (gconstpointer src, gpointer unused)
{
	const Anode *san = src;
	Anode *an = g_slice_new0 (Anode);
	an->def = san->def;
	an->join = san->join;
	an->opts = g_list_copy (san->opts);
	return an;
}

static GNode*
anode_clone (GNode *node)
{
	return g_node_copy_deep (node, anode_copy_func, NULL);
}

static int
anode_def_type (GNode *node)
{
	Anode *an = node->data;
	gint type = an->join ? an->join->type : an->def->type;
	return type & 0xFF;
}

static gboolean
anode_def_type_is_real (GNode *node)
{
	switch (anode_def_type (node)) {
	case TYPE_INTEGER:
	case TYPE_BOOLEAN:
	case TYPE_BIT_STRING:
	case TYPE_OCTET_STRING:
	case TYPE_OBJECT_ID:
	case TYPE_TIME:
	case TYPE_NULL:
	case TYPE_ENUMERATED:
	case TYPE_GENERALSTRING:
		return TRUE;
	case TYPE_SEQUENCE:
	case TYPE_SEQUENCE_OF:
	case TYPE_ANY:
	case TYPE_SET:
	case TYPE_SET_OF:
	case TYPE_CHOICE:
		return TRUE;
	case TYPE_CONSTANT:
	case TYPE_IDENTIFIER:
	case TYPE_TAG:
	case TYPE_DEFAULT:
	case TYPE_SIZE:
	case TYPE_DEFINITIONS:
	case TYPE_IMPORTS:
		return FALSE;
	default:
		g_return_val_if_reached (FALSE);
	}
}

static int
anode_def_flags (GNode *node)
{
	Anode *an = node->data;
	gint type = an->def->type;
	if (an->join)
		type |= an->join->type;
	return type & 0xFFFFFF00;
}

static const gchar*
anode_def_name (GNode *node)
{
	Anode *an = node->data;
	return an->def->name;
}

static const gchar*
anode_def_value (GNode *node)
{
	Anode *an = node->data;
	return an->def->value;
}

static gulong
anode_def_value_as_ulong (const ASN1_ARRAY_TYPE *def)
{
	gchar *end = NULL;
	gulong lval;

	g_return_val_if_fail (def->value, G_MAXULONG);
	lval = strtoul (def->value, &end, 10);
	g_return_val_if_fail (end && !end[0], G_MAXULONG);
	return lval;
}

static GNode*
anode_child_with_name (GNode *node, const gchar *name)
{
	GNode *child;

	for (child = node->children; child; child = child->next) {
		if (g_str_equal (name, anode_def_name (child)))
			return child;
	}

	return NULL;
}

static void
anode_opt_add (GNode *node, const ASN1_ARRAY_TYPE *def)
{
	Anode *an = node->data;
	an->opts = g_list_append (an->opts, (gpointer)def);
}

static ASN1_ARRAY_TYPE*
anode_opt_lookup (GNode *node, gint type, const gchar *name)
{
	Anode *an = node->data;
	ASN1_ARRAY_TYPE* def;
	GList *l;

	for (l = an->opts; l; l = g_list_next (l)) {
		def = l->data;
		if (name && def->name && !g_str_equal (name, def->name))
			continue;
		if ((def->type & 0xFF) == type)
			return def;
	}

	return NULL;
}

static ASN1_ARRAY_TYPE*
anode_opt_lookup_value (GNode *node, gint type, const gchar *value)
{
	Anode *an = node->data;
	ASN1_ARRAY_TYPE* def;
	GList *l;

	for (l = an->opts; l; l = g_list_next (l)) {
		def = l->data;
		if (value && def->value && !g_str_equal (value, def->value))
			continue;
		if ((def->type & 0xFF) == type)
			return def;
	}

	return NULL;
}

static GList*
anode_opts_lookup (GNode *node, gint type, const gchar *name)
{
	Anode *an = node->data;
	ASN1_ARRAY_TYPE* def;
	GList *l, *res = NULL;

	for (l = an->opts; l; l = g_list_next (l)) {
		def = l->data;
		if (name && def->name && !g_str_equal (name, def->name))
			continue;
		if ((def->type & 0xFF) == type)
			res = g_list_prepend (res, def);
	}

	return g_list_reverse (res);
}

static gint
compare_tlvs (Atlv *tlva, Atlv *tlvb)
{
	gint la = tlva->off + tlva->len;
	gint lb = tlvb->off + tlvb->len;
	gint res;

	g_assert (tlva->buf);
	g_assert (tlvb->buf);
	res = memcmp (tlva->buf, tlvb->buf, MIN (la, lb));
	if (la == lb || res != 0)
		return res;
	return la < lb ? -1 : 1;
}

static void
anode_set_tlv_data (GNode *node, Atlv *tlv)
{
	Anode *an = node->data;
	g_assert (!an->tlv);
	g_assert (tlv->len >= 0);
	an->tlv = g_slice_new0 (Atlv);
	memcpy (an->tlv, tlv, sizeof (Atlv));
}

static Atlv*
anode_get_tlv_data (GNode *node)
{
	Anode *an = node->data;
	return an->tlv;
}

static void
anode_clr_tlv_data (GNode *node)
{
	Anode *an = node->data;
	if (an->tlv);
		g_slice_free (Atlv, an->tlv);
	an->tlv = NULL;
}

static void
anode_clr_enc_data (GNode *node)
{
	Anode *an = node->data;
	if (an->enc) {
		g_slice_free (Aenc, an->enc);
		an->enc = NULL;
	}
}

static void
anode_set_enc_data (GNode *node, EggAsn1xEncoder encoder, gpointer enc_data)
{
	Anode *an = node->data;
	g_assert (!an->enc);
	an->enc = g_slice_new0 (Aenc);
	an->enc->encoder = encoder;
	an->enc->data = enc_data;
}

static void
anode_set_user_data (GNode *node, gpointer user_data, GDestroyNotify destroy)
{
	Anode *an;
	g_assert (node && node->data);
	an = node->data;
	if (an->destroy)
		(an->destroy) (an->user_data);
	an->user_data = user_data;
	an->destroy = destroy;
}

static Aenc*
anode_get_enc_data (GNode *node)
{
	Anode *an = node->data;
	return an->enc;
}

static gboolean
anode_failure (GNode *node, const gchar *failure)
{
	Anode *an = node->data;
	const gchar *prefix = an->def->name;
	if (!prefix && an->join)
		prefix = an->join->name;
	if (!prefix)
		prefix = an->def->value;
	if (!prefix && an->join)
		prefix = an->join->value;
	if (!prefix)
		prefix = "unknown";

	g_free (an->failure);
	an->failure = g_strdup_printf ("%s: %s", prefix, failure);
	return FALSE; /* So this can be chained */
}

static const gchar*
anode_failure_get (GNode *node)
{
	Anode *an = node->data;
	return an->failure;
}

static void
anode_clear (GNode *node)
{
	Anode *an = node->data;
	anode_clr_tlv_data (node);
	anode_clr_enc_data (node);
	anode_set_user_data (node, NULL, NULL);
	g_free (an->failure);
	an->failure = NULL;
}

static gboolean
anode_free_func (GNode *node, gpointer unused)
{
	Anode *an = node->data;
	anode_clear (node);
	g_list_free (an->opts);
	g_slice_free (Anode, an);
	return FALSE;
}

static void
abits_destroy (gpointer data)
{
	Abits *ab = data;
	g_assert (ab);
	if (ab->destroy)
		(ab->destroy) (ab->bits);
	g_slice_free (Abits, ab);
}

static void
anode_destroy (GNode *node)
{
	if (!G_NODE_IS_ROOT (node))
		g_node_unlink (node);
	g_node_traverse (node, G_IN_ORDER, G_TRAVERSE_ALL, -1, anode_free_func, NULL);
	g_node_destroy (node);
}

static gulong
anode_calc_tag_for_flags (GNode *node, gint flags)
{
	ASN1_ARRAY_TYPE *def;

	/* A context specific tag */
	if (flags & FLAG_TAG) {
		def = anode_opt_lookup (node, TYPE_TAG, NULL);
		g_return_val_if_fail (def, G_MAXULONG);
		return anode_def_value_as_ulong (def);
	}

	/* A tag from the universal set */
	switch (anode_def_type (node)) {
	case TYPE_INTEGER:
		return ASN1_TAG_INTEGER;
	case TYPE_ENUMERATED:
		return ASN1_TAG_ENUMERATED;
	case TYPE_BOOLEAN:
		return ASN1_TAG_BOOLEAN;
	case TYPE_BIT_STRING:
		return ASN1_TAG_BIT_STRING;
	case TYPE_OCTET_STRING:
		return ASN1_TAG_OCTET_STRING;
	case TYPE_OBJECT_ID:
		return ASN1_TAG_OBJECT_ID;
	case TYPE_NULL:
		return ASN1_TAG_NULL;
	case TYPE_GENERALSTRING:
		return ASN1_TAG_GENERALSTRING;
	case TYPE_TIME:
		if (flags & FLAG_GENERALIZED)
			return ASN1_TAG_GENERALIZEDTime;
		else if (flags & FLAG_UTC)
			return ASN1_TAG_UTCTime;
		else
			g_return_val_if_reached (G_MAXULONG);
	case TYPE_SEQUENCE:
	case TYPE_SEQUENCE_OF:
		return ASN1_TAG_SEQUENCE;
	case TYPE_SET:
	case TYPE_SET_OF:
		return ASN1_TAG_SET;

	/* These should be handled specially */
	case TYPE_ANY:
	case TYPE_CHOICE:
		return G_MAXULONG;

	/* These are not real nodes */
	case TYPE_CONSTANT:
	case TYPE_IDENTIFIER:
	case TYPE_TAG:
	case TYPE_DEFAULT:
	case TYPE_SIZE:
	case TYPE_DEFINITIONS:
	case TYPE_IMPORTS:
		g_return_val_if_reached (G_MAXULONG);

	/* Unknown value */
	default:
		g_return_val_if_reached (G_MAXULONG);
	}
}

static gulong
anode_calc_tag (GNode *node)
{
	return anode_calc_tag_for_flags (node, anode_def_flags (node));
}

static gboolean
anode_calc_explicit_for_flags (GNode *node, gint flags)
{
	const ASN1_ARRAY_TYPE *opt;
	if ((flags & FLAG_TAG) != FLAG_TAG)
		return FALSE;
	opt = anode_opt_lookup (node, TYPE_TAG, NULL);
	g_return_val_if_fail (opt, FALSE);
	if ((opt->type & FLAG_IMPLICIT) == FLAG_IMPLICIT)
		return FALSE;
	return TRUE;
}

static gboolean
anode_calc_explicit (GNode *node)
{
	return anode_calc_explicit_for_flags (node, anode_def_flags (node));
}

/* -------------------------------------------------------------------------
 * DECODE
 */

static gboolean
anode_decode_cls_tag (const guchar *data, const guchar *end,
                      guchar *cls, gulong *tag, gint *cb)
{
	gint punt, ris, last;
	gint n_data;

	g_assert (end >= data);
	g_assert (cls);
	g_assert (cb);

	n_data = end - data;

	if (n_data < 2)
		return FALSE;

	*cls = data[0] & 0xE0;

	/* short form */
	if ((data[0] & 0x1F) != 0x1F) {
		*cb = 1;
		ris = data[0] & 0x1F;

	/* Long form */
	} else {
		punt = 1;
		ris = 0;
		while (punt <= n_data && data[punt] & 128) {
			int last = ris;
			ris = ris * 128 + (data[punt++] & 0x7F);

			/* wrapper around, and no bignums... */
			if (ris < last)
				return FALSE;
		}

		if (punt >= n_data)
			return FALSE;

		last = ris;
		ris = ris * 128 + (data[punt++] & 0x7F);

		/* wrapper around, and no bignums... */
		if (ris < last)
			return FALSE;

		*cb = punt;
	}

	if (tag)
		*tag = ris;

	return TRUE;
}

static gint
anode_decode_length (const guchar *data, const guchar *end, gint *cb)
{
	gint ans, last;
	gint k, punt;
	gint n_data;

	g_assert (data);
	g_assert (end);
	g_assert (end >= data);
	g_assert (cb);

	*cb = 0;
	n_data = end - data;

	if (n_data == 0)
		return 0;

	/* short form */
	if (!(data[0] & 128)) {
		*cb = 1;
		return data[0];

	/* Long form */
	} else {
		k = data[0] & 0x7F;
		punt = 1;

		/* definite length method */
		if (k) {
			ans = 0;
			while (punt <= k && punt < n_data) {
				last = ans;
				ans = ans * 256 + data[punt++];

				/* we wrapped around, no bignum support... */
				if (ans < last)
					return -2;
			}

		/* indefinite length method */
		} else {
			ans = -1;
		}

		*cb = punt;
		return ans;
	}
}

static gboolean
anode_decode_cls_tag_len (const guchar *data, const guchar *end,
                          guchar *cls, gulong *tag, gint *off, gint *len)
{
	gint cb1, cb2;

	g_assert (data);
	g_assert (end);
	g_assert (end >= data);
	g_assert (off);
	g_assert (len);

	if (!anode_decode_cls_tag (data, end, cls, tag, &cb1))
		return FALSE;
	*len = anode_decode_length (data + cb1, end, &cb2);
	if (*len < -1)
		return FALSE;
	*off = cb1 + cb2;
	if (*len >= 0 && data + *off + *len > end)
		return FALSE;
	return TRUE;
}

static gboolean
anode_check_indefinite_end (guchar cls, gulong tag, gint len)
{
	return (cls == ASN1_CLASS_UNIVERSAL && tag == 0 && len == 0);
}

static gboolean
anode_decode_indefinite_len (const guchar *data, const guchar *end, gint *rlen)
{
	gint result = 0;
	gint der_len;
	gint len;
	guchar cls;
	gulong tag;
	gint off;

	g_assert (data <= end);
	der_len = end - data;

	while (result < der_len) {
		if (!anode_decode_cls_tag_len (data + result, end, &cls, &tag, &off, &len))
			return FALSE;

		/* The indefinite end */
		if (anode_check_indefinite_end (cls, tag, len))
			break;

		result += off;

		/* Mid way check */
		if (result > der_len)
			break;

		if (len < 0) {
			if (!anode_decode_indefinite_len (data + result, end, &len))
				return FALSE;
			g_assert (len >= 0);
		}

		if (result + len > der_len)
			return FALSE;
		result += len;
	}

	if (result > der_len)
		return FALSE;
	*rlen = result;
	return TRUE;
}

static gboolean
anode_decode_tlv_for_data (const guchar *data, const guchar *end, Atlv *tlv)
{
	g_assert (data <= end);
	if (!anode_decode_cls_tag_len (data, end, &tlv->cls,
	                               &tlv->tag, &tlv->off, &tlv->len))
		return FALSE;
	tlv->buf = data;
	if (tlv->len < 0)
		tlv->end = end;
	else
		tlv->end = tlv->buf + tlv->len + tlv->off;
	g_assert (tlv->end <= end);
	return TRUE;
}

static gboolean
anode_decode_tlv_for_contents (Atlv *outer, gboolean first, Atlv *tlv)
{
	const guchar *data;
	const guchar *end;

	if (first) {
		data = outer->buf + outer->off;
		end = outer->end;
	} else {
		data = tlv->end;
		end = outer->end;
	}

	/* The end */
	if (end == data) {
		tlv->cls = ASN1_CLASS_UNIVERSAL;
		tlv->tag = 0;
		tlv->len = 0;
		tlv->off = 0;
		tlv->buf = data;
		tlv->end = end;
		return TRUE;
	}

	g_return_val_if_fail (end > data, FALSE);
	if (!anode_decode_tlv_for_data (data, end, tlv))
		return FALSE;

	/* Caller should stop before indefinite end, and not consume */
	if (anode_check_indefinite_end (tlv->cls, tlv->tag, tlv->len)) {
		tlv->buf = data;
		tlv->end = data;
		tlv->off = 0;
	}

	return TRUE;
}

static gboolean
anode_decode_choice (GNode *node, Atlv *tlv)
{
	gboolean have = FALSE;
	GNode *child;
	Anode *an;

	for (child = node->children; child; child = child->next) {
		an = (Anode*)child->data;
		if (!have && anode_decode_anything (child, tlv)) {
			an->chosen = 1;
			have = TRUE;
		} else {
			an->chosen = 0;
		}
	}

	if (!have)
		return anode_failure (node, "no choice is present");

	return TRUE;
}

static gboolean
anode_decode_struct_string (GNode *node, Atlv *outer)
{
	gint i = 0;
	Atlv tlv;

	/* Recalculated below */
	outer->len = 0;

	for (i = 0; TRUE; ++i) {
		if (!anode_decode_tlv_for_contents (outer, i == 0, &tlv))
			return anode_failure (node, "invalid encoding of child");
		if (tlv.tag != outer->tag)
			return anode_failure (node, "contents have an invalid tag");
		outer->len = (tlv.end - outer->buf) - outer->off;
	}

	g_assert (outer->len >= 0);
	return TRUE;
}

static gboolean
anode_decode_struct_any (GNode *node, Atlv *tlv)
{
	if (tlv->len < 0) {
		if (!anode_decode_indefinite_len (tlv->buf + tlv->off, tlv->end, &tlv->len))
			return anode_failure (node, "could not find end of encoding");
		tlv->end = tlv->buf + tlv->off + tlv->len;
	}

	return TRUE;
}

static gboolean
anode_decode_sequence_or_set (GNode *node, Atlv *outer)
{
	GNode *child;
	Atlv tlv;
	gint i;

	/* Recalculated below */
	outer->len = 0;

	/*
	 * The reason we can parse a set just like a sequence, is because in DER,
	 * the order of the SET is predefined by the tags. In addition the definitions
	 * we have are sorted.
	 */

	for (child = node->children, i = 0; child; child = child->next, ++i) {

		if (!anode_decode_tlv_for_contents (outer, i == 0, &tlv))
			return anode_failure (node, "invalid encoding of child");

		if (!anode_decode_anything (child, &tlv))
			return FALSE;

		outer->len = (tlv.end - outer->buf) - outer->off;
	}

	g_assert (outer->len >= 0);
	return TRUE;
}

static gboolean
anode_decode_sequence_or_set_of (GNode *node, Atlv *outer)
{
	GNode *child, *other;
	Atlv tlv;
	gint i;

	outer->len = 0;

	/* The first child */
	child = node->children;
	g_return_val_if_fail (child, FALSE);

	/* Remove all the other children */
	while (child->next)
		anode_destroy (child->next);

	/* Try to dig out as many of them as possible */
	for (i = 0; TRUE; ++i) {

		if (!anode_decode_tlv_for_contents (outer, i == 0, &tlv))
			return anode_failure (node, "invalid encoding of child");

		/* The end of the road for us */
		if (tlv.off == 0)
			break;

		if (i == 0) {
			other = child;
		} else {
			other = anode_clone (child);
			g_node_append (node, other);
		}

		if (!anode_decode_anything (other, &tlv))
			return FALSE;

		outer->len = (tlv.end - outer->buf) - outer->off;
	}

	g_assert (outer->len >= 0);
	return TRUE;
}

static gboolean
anode_decode_primitive (GNode *node, Atlv *tlv, gint flags)
{
	gint type;

	/* Must have a definite length */
	if (tlv->len < 0)
		return anode_failure (node, "primitive value with an indefinite length");

	type = anode_def_type (node);
	switch (type) {

	/* The primitive value types */
	case TYPE_INTEGER:
	case TYPE_ENUMERATED:
	case TYPE_BOOLEAN:
	case TYPE_BIT_STRING:
	case TYPE_OCTET_STRING:
	case TYPE_OBJECT_ID:
	case TYPE_NULL:
	case TYPE_GENERALSTRING:
	case TYPE_TIME:
		anode_set_tlv_data (node, tlv);
		return TRUE;

	/* Transparent types */
	case TYPE_ANY:
		anode_set_tlv_data (node, tlv);
		return TRUE;

	case TYPE_CHOICE:
		if (!anode_decode_choice (node, tlv))
			return FALSE;
		anode_set_tlv_data (node, tlv);
		return TRUE;

	default:
		return anode_failure (node, "primitive value of an unexpected type");
	}

	g_assert_not_reached ();
}

static gboolean
anode_decode_structured (GNode *node, Atlv *tlv, gint flags)
{
	gboolean definite;
	const guchar *end;
	Atlv ctlv;
	gint len;
	gulong tag;
	guchar cls;
	gint off = 0;

	definite = (tlv->len >= 0);
	end = tlv->end;

	/* An explicit, wrapped tag */
	if (anode_calc_explicit_for_flags (node, flags)) {
		if ((tlv->cls & ASN1_CLASS_CONTEXT_SPECIFIC) == 0)
			return anode_failure (node, "missing context specific tag");
		if (!anode_decode_tlv_for_contents (tlv, TRUE, &ctlv))
			return anode_failure (node, "invalid encoding of child");
		flags &= ~FLAG_TAG;
		if (!anode_decode_anything_for_flags (node, &ctlv, flags))
			return FALSE;

		/* Use most of the child's tlv */
		tlv->cls = ctlv.cls;
		tlv->tag = ctlv.tag;
		tlv->off += ctlv.off;
		tlv->oft = ctlv.off;
		tlv->len = ctlv.len;
		anode_clr_tlv_data (node);

	/* Other structured types */
	} else {
		switch (anode_def_type (node)) {
		case TYPE_ANY:
			if (!anode_decode_struct_any (node, tlv))
				return FALSE;
			break;
		case TYPE_CHOICE:
			if (!anode_decode_choice (node, tlv))
				return FALSE;
			break;
		case TYPE_GENERALSTRING:
		case TYPE_OCTET_STRING:
			if (!anode_decode_struct_string (node, tlv))
				return FALSE;
			break;
		case TYPE_SEQUENCE:
		case TYPE_SET:
			if (!anode_decode_sequence_or_set (node, tlv))
				return FALSE;
			break;
		case TYPE_SEQUENCE_OF:
		case TYPE_SET_OF:
			if (!anode_decode_sequence_or_set_of (node, tlv))
				return FALSE;
			break;
		default:
			return FALSE;
		}
	}

	g_return_val_if_fail (tlv->len >= 0, FALSE);

	/* Indefinite, needs to be terminated with zeros */
	if (!definite) {
		if (!anode_decode_cls_tag_len (tlv->buf + (tlv->off + tlv->len), end,
		                               &cls, &tag, &off, &len))
			return anode_failure (node, "end of indefinite content is missing");
		if (!anode_check_indefinite_end (cls, tag, len))
			return anode_failure (node, "end of indefinite content is invalid");
		end = tlv->buf + tlv->off + tlv->len + off;
	}

	/* A structure must be filled up, no stuff ignored */
	if (tlv->buf + tlv->off + tlv->len + off < end)
		return anode_failure (node, "extra data at the end of the content");
	g_return_val_if_fail (tlv->buf + tlv->off + tlv->len + off == end, FALSE);

	tlv->end = end;
	anode_set_tlv_data (node, tlv);
	return TRUE;
}

static gboolean
anode_decode_option_or_default (GNode *node, Atlv *tlv, gint flags)
{
	if (flags & FLAG_OPTION || flags & FLAG_DEFAULT) {
		tlv->len = 0;
		tlv->end = tlv->buf;
		tlv->off = 0;
		anode_clr_tlv_data (node);
		return TRUE;
	}

	return FALSE;
}

static gboolean
anode_decode_anything_for_flags (GNode *node, Atlv *tlv, gint flags)
{
	gboolean ret;
	gulong tag;

	tag = anode_calc_tag_for_flags (node, flags);

	/* We don't know what the tag is supposed to be */
	if (tag == G_MAXULONG)
		tag = tlv->tag;

	/* Tag does not match, what do we do? */
	if (tlv->off == 0 || tag != tlv->tag) {
		if (anode_decode_option_or_default (node, tlv, flags))
			return TRUE;
		return anode_failure (node, "decoded tag did not match expected");
	}

	/* Structured value */
	if (tlv->cls & ASN1_CLASS_STRUCTURED)
		ret = anode_decode_structured (node, tlv, flags);

	/* A primitive simple value */
	else
		ret = anode_decode_primitive (node, tlv, flags);

	return ret;
}

static gboolean
anode_decode_anything (GNode *node, Atlv *tlv)
{
	gint flags = anode_def_flags (node);

	if (!anode_decode_anything_for_flags (node, tlv, flags))
		return anode_decode_option_or_default (node, tlv, flags);

	return TRUE;
}

gboolean
egg_asn1x_decode (GNode *asn, gconstpointer data, gsize n_data)
{
	Atlv tlv;

	g_return_val_if_fail (asn, FALSE);
	g_return_val_if_fail (data, FALSE);
	g_return_val_if_fail (n_data, FALSE);

	egg_asn1x_clear (asn);

	if (!anode_decode_tlv_for_data (data, (const guchar*)data + n_data, &tlv))
		return anode_failure (asn, "content is not encoded properly");

	if (!anode_decode_anything (asn, &tlv))
		return FALSE;

	if (tlv.end - tlv.buf != n_data)
		return FALSE;

	return egg_asn1x_validate (asn);
}

/* -----------------------------------------------------------------------------------
 * ENCODING
 */

static void
anode_encode_length (gulong len, guchar *ans, gint *cb)
{
	guchar temp[sizeof (gulong)];
	gint k;

	g_assert (cb);

	/* short form */
	if (len < 128) {
		if (ans != NULL)
			ans[0] = (unsigned char)len;
		*cb = 1;

	/* Long form */
	} else {
		k = 0;
		while (len) {
			temp[k++] = len & 0xFF;
			len = len >> 8;
		}
		*cb = k + 1;
		if (ans != NULL) {
			ans[0] = ((unsigned char) k & 0x7F) + 128;
			while (k--)
				ans[*cb - 1 - k] = temp[k];
		}
	}
}

static gint
anode_encode_cls_tag_len (guchar *data, gsize n_data, guchar cls,
                          gulong tag, gint len)
{
	guchar temp[sizeof(gulong)];
	gint cb;
	gint off = 0;
	gint k;

	/* Short form */
	if (tag < 31) {
		off += 1;
		if (data) {
			g_assert (n_data >= off);
			data[0] = (cls & 0xE0) + ((guchar) (tag & 0x1F));
		}
	/* Long form */
	} else {
		k = 0;
		while (tag) {
			temp[k++] = tag & 0x7F;
			tag = tag >> 7;
		}
		off = k + 1;
		if (data) {
			g_assert (n_data >= off);
			data[0] = (cls & 0xE0) + 31;
			while (data && k--)
				data[off - 1 - k] = temp[k] + 128;
			data[off - 1] -= 128;
		}
	}

	/* And now the length */
	cb = n_data - off;
	anode_encode_length (len, data ? data + off : NULL, &cb);
	off += cb;

	g_assert (!data || n_data >= off);
	return off;
}

static void
anode_encode_tlv_and_enc (GNode *node, gsize n_data, EggAsn1xEncoder encoder,
                          gpointer user_data, GDestroyNotify destroy)
{
	gboolean explicit = FALSE;
	gulong tag;
	gint flags;
	Atlv tlv;

	g_assert (node);
	g_assert (encoder);

	/* The data length */
	memset (&tlv, 0, sizeof (tlv));
	tlv.len = n_data;

	/* Figure out the basis if the class */
	switch (anode_def_type (node)) {
	case TYPE_INTEGER:
	case TYPE_BOOLEAN:
	case TYPE_BIT_STRING:
	case TYPE_OCTET_STRING:
	case TYPE_OBJECT_ID:
	case TYPE_TIME:
	case TYPE_ENUMERATED:
	case TYPE_GENERALSTRING:
	case TYPE_NULL:
		tlv.cls = ASN1_CLASS_UNIVERSAL;
		break;
	/* Container types */
	case TYPE_SEQUENCE:
	case TYPE_SET:
	case TYPE_SEQUENCE_OF:
	case TYPE_SET_OF:
		tlv.cls = (ASN1_CLASS_STRUCTURED | ASN1_CLASS_UNIVERSAL);
		break;

	/* Transparent types shouldn't get here */
	case TYPE_ANY:
	case TYPE_CHOICE:
		g_return_if_reached ();

	default:
		g_return_if_reached ();
	};

	/* Build up the class */
	flags = anode_def_flags (node);
	if (flags & FLAG_TAG) {
		explicit = anode_calc_explicit_for_flags (node, flags);
		if (explicit)
			flags &= ~FLAG_TAG;
		else
			tlv.cls |= ASN1_CLASS_CONTEXT_SPECIFIC;
	}

	/* And now the tag */
	tlv.tag = anode_calc_tag_for_flags (node, flags);

	/* Calculate the length for the main thingy */
	tlv.off = anode_encode_cls_tag_len (NULL, 0, tlv.cls, tlv.tag, tlv.len);

	/* Wrap that in another explicit tag if necessary */
	if (explicit) {
		tag = anode_calc_tag (node);
		g_return_if_fail (tag != G_MAXULONG);
		tlv.oft = anode_encode_cls_tag_len (NULL, 0, 0, tag, tlv.off + tlv.len);
		tlv.off += tlv.oft;
	}

	/* Not completely filled in */
	tlv.buf = tlv.end = NULL;

	anode_clear (node);
	anode_set_tlv_data (node, &tlv);
	anode_set_enc_data (node, encoder, user_data);
	anode_set_user_data (node, user_data, destroy);
}

static gboolean
anode_encode_build (GNode *node, guchar *data, gsize n_data)
{
	gint type;
	guchar cls;
	gulong tag;
	Aenc *enc;
	Atlv *tlv;
	gint off = 0;

	type = anode_def_type (node);
	tlv = anode_get_tlv_data (node);
	g_return_val_if_fail (tlv, FALSE);

	/* Should have an encoder */
	enc = anode_get_enc_data (node);
	g_return_val_if_fail (enc, FALSE);

	/* If it's a choice node, use the choice for calculations */
	if (type == TYPE_CHOICE) {
		node = egg_asn1x_get_choice (node);
		g_return_val_if_fail (node, FALSE);
	}

	/* Encode any explicit tag */
	if (anode_calc_explicit (node)) {
		tag = anode_calc_tag (node);
		g_return_val_if_fail (tag != G_MAXULONG, FALSE);
		cls = (ASN1_CLASS_STRUCTURED | ASN1_CLASS_CONTEXT_SPECIFIC);
		g_assert (tlv->oft > 0 && tlv->oft < tlv->off);
		off = anode_encode_cls_tag_len (data, n_data, cls, tag, (tlv->off - tlv->oft) + tlv->len);
		g_assert (off == tlv->oft);
	}

	/* Now encode the main tag */
	off += anode_encode_cls_tag_len (data + off, n_data - off, tlv->cls, tlv->tag, tlv->len);
	g_assert (off == tlv->off);

	/* Setup the remainder of the tlv */
	g_assert (tlv->len + tlv->off == n_data);
	tlv->buf = data;
	tlv->end = data + n_data;

	/* Encode in the data */
	if (!(enc->encoder) (enc->data, data + tlv->off, tlv->len))
		return FALSE;

	return TRUE;
}

static void
anode_encode_rollback (GNode *node)
{
	GNode *child;
	Aenc *enc;
	Atlv *tlv;

	/* Undo any references to our new buffer */
	enc = anode_get_enc_data (node);
	if (enc) {
		tlv = anode_get_tlv_data (node);
		g_return_if_fail (tlv);
		tlv->buf = tlv->end = NULL;
	}

	for (child = node->children; child; child = child->next)
		anode_encode_rollback (child);
}

static void
anode_encode_commit (GNode *node)
{
	GNode *child;

	/* Remove and free all the encoder stuff */
	anode_clr_enc_data (node);

	for (child = node->children; child; child = child->next)
		anode_encode_commit (child);
}

static gint
compare_bufs (gconstpointer a, gconstpointer b)
{
	const Abuf *ba = a;
	const Abuf *bb = b;
	gint res = memcmp (ba->data, bb->data, MIN (ba->n_data, bb->n_data));
	if (ba->n_data == bb->n_data || res != 0)
		return res;
	return ba->n_data < bb->n_data ? -1 : 1;
}

static gboolean
traverse_and_sort_set_of (GNode *node, gpointer user_data)
{
	EggAllocator allocator = user_data;
	GList *bufs, *l;
	Abuf *buf;
	guchar *data;
	gsize n_data;
	Atlv *tlv;
	GNode *child;
	GNode *next;

	g_assert (allocator);

	/* We have to sort any SET OF :( */
	if (anode_def_type (node) != TYPE_SET_OF)
		return FALSE;

	bufs = NULL;
	for (child = node->children; child; child = next) {
		next = child->next;

		tlv = anode_get_tlv_data (child);
		if (!tlv)
			continue;

		/* Allocate enough memory */
		n_data = tlv->len + tlv->off;
		data = (allocator) (NULL, n_data + 1);
		if (!data)
			break;

		if (!anode_encode_build (child, data, n_data)) {
			(allocator) (data, 0);
			continue;
		}

		buf = g_slice_new0 (Abuf);
		buf->user_data = child;
		buf->n_data = n_data;
		buf->data = data;
		bufs = g_list_prepend (bufs, buf);
		g_node_unlink (child);
	}

	bufs = g_list_sort (bufs, compare_bufs);

	for (l = bufs; l; l = g_list_next (l)) {
		buf = l->data;
		g_node_append (node, buf->user_data);
		(allocator) (buf->data, 0);
		g_slice_free (Abuf, buf);
	}

	anode_encode_rollback (node);
	g_list_free (bufs);
	return FALSE;
}

static gboolean
anode_encoder_simple (gpointer user_data, guchar *data, gsize n_data)
{
	memcpy (data, user_data, n_data);
	return TRUE;
}

static gboolean
anode_encoder_structured (gpointer user_data, guchar *data, gsize n_data)
{
	GNode *node = user_data;
	GNode *child;
	gsize length;
	Atlv *tlv;

	for (child = node->children; child; child = child->next) {
		tlv = anode_get_tlv_data (child);
		if (tlv) {
			length = tlv->off + tlv->len;
			g_assert (length <= n_data);
			if (!anode_encode_build (child, data, length))
				return FALSE;
			data += length;
			n_data -= length;
		}
	}

	return TRUE;
}

static gboolean
anode_encoder_choice (gpointer user_data, guchar *data, gsize n_data)
{
	GNode *node = user_data;
	Aenc *enc = NULL;
	GNode *child;
	Atlv *tlv, *ctlv;

	tlv = anode_get_tlv_data (node);
	g_return_val_if_fail (tlv, FALSE);

	child = egg_asn1x_get_choice (node);
	g_return_val_if_fail (child, FALSE);

	ctlv = anode_get_tlv_data (child);
	g_assert (ctlv);

	enc = anode_get_enc_data (child);
	g_return_val_if_fail (enc, FALSE);
	if (!(enc->encoder) (enc->data, data, n_data))
		return FALSE;

	/* Child's buffer matches ours */
	ctlv->buf = tlv->buf;
	ctlv->end = tlv->end;

	return TRUE;
}

static gboolean
anode_encoder_bit_string (gpointer user_data, guchar *data, gsize n_data)
{
	Abits *ab = user_data;
	guchar empty, mask;
	gsize len;

	empty = ab->n_bits % 8;
	if (empty > 0)
		empty = 8 - empty;
	len = (ab->n_bits / 8) + (empty ? 1 : 0);
	g_assert (n_data == len + 1);

	/* Fill in the amount of empty */
	data[0] = empty;
	data += 1;

	/* Fill in the actual data */
	memcpy (data, ab->bits, len);

	/* Set the extra bits to zero */
	if (len && empty) {
		mask = 0xFF >> (8 - empty);
		data[len - 1] &= ~mask;
	}

	return TRUE;
}

static gboolean
anode_encode_prepare_simple (GNode *node, gboolean want)
{
	Aenc *enc;
	Atlv *tlv;

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL)
		return FALSE;

	/* Transfer the tlv data over to enc */
	enc = anode_get_enc_data (node);
	if (enc == NULL)
		anode_set_enc_data (node, anode_encoder_simple, (guchar*)tlv->buf + tlv->off);

	tlv->buf = tlv->end = NULL;
	return TRUE;
}

static gboolean
anode_encode_prepare_choice (GNode *node, gboolean want)
{
	Atlv *tlv;
	GNode *child;
	gint type;

	type = anode_def_type (node);
	g_assert (type == TYPE_CHOICE);

	child = egg_asn1x_get_choice (node);
	if (!child)
		return FALSE;

	if (!anode_encode_prepare (child, want))
		return FALSE;

	tlv = anode_get_tlv_data (child);
	g_return_val_if_fail (tlv, FALSE);
	anode_clr_tlv_data (node);
	anode_set_tlv_data (node, tlv);
	anode_set_enc_data (node, anode_encoder_choice, node);

	return TRUE;

}

static gboolean
anode_encode_prepare_structured (GNode *node, gboolean want)
{
	gboolean child_want;
	gsize length;
	gboolean had;
	Atlv *tlv;
	GNode *child;
	gint type;

	type = anode_def_type (node);
	child_want = want;
	had = FALSE;
	length = 0;

	if (type == TYPE_SEQUENCE_OF || type == TYPE_SET_OF)
		child_want = FALSE;
	if (anode_def_flags (node) & FLAG_OPTION)
		want = FALSE;

	for (child = node->children; child; child = child->next) {
		if (anode_encode_prepare (child, child_want)) {
			tlv = anode_get_tlv_data (child);
			g_return_val_if_fail (tlv, FALSE);
			length += tlv->off + tlv->len;
			had = TRUE;
		}
	}

	if (had == FALSE) {
		/* See if we should encode an empty set or seq of */
		if (type == TYPE_SEQUENCE_OF || type == TYPE_SET_OF) {
			if (!want)
				return FALSE;
		} else {
			return FALSE;
		}
	}

	anode_encode_tlv_and_enc (node, length, anode_encoder_structured, node, NULL);
	return TRUE;
}

static gboolean
anode_encode_prepare (GNode *node, gboolean want)
{
	switch (anode_def_type (node)) {
	case TYPE_INTEGER:
	case TYPE_BOOLEAN:
	case TYPE_BIT_STRING:
	case TYPE_OCTET_STRING:
	case TYPE_OBJECT_ID:
	case TYPE_TIME:
	case TYPE_ENUMERATED:
	case TYPE_GENERALSTRING:
	case TYPE_ANY:
	case TYPE_NULL:
		return anode_encode_prepare_simple (node, want);
		break;
	case TYPE_SEQUENCE:
	case TYPE_SEQUENCE_OF:
	case TYPE_SET:
	case TYPE_SET_OF:
		return anode_encode_prepare_structured (node, want);
		break;
	case TYPE_CHOICE:
		return anode_encode_prepare_choice (node, want);
		break;
	default:
		g_return_val_if_reached (FALSE);
	};
}

gpointer
egg_asn1x_encode (GNode *asn, EggAllocator allocator, gsize *n_data)
{
	guchar *data;
	gsize length;
	Atlv *tlv;

	g_return_val_if_fail (asn, NULL);
	g_return_val_if_fail (n_data, NULL);
	g_return_val_if_fail (anode_def_type_is_real (asn), NULL);

	if (!allocator)
		allocator = g_realloc;

	if (!anode_encode_prepare (asn, TRUE)) {
		anode_failure (asn, "missing value(s)");
		return NULL;
	}

	/* We must sort all the nasty SET OF nodes */
	g_node_traverse (asn, G_POST_ORDER, G_TRAVERSE_ALL, -1,
	                 traverse_and_sort_set_of, allocator);

	tlv = anode_get_tlv_data (asn);
	g_return_val_if_fail (tlv, NULL);

	/* Allocate enough memory for entire thingy */
	length = tlv->off + tlv->len;
	data = (allocator) (NULL, length + 1);
	if (data == NULL)
		return NULL;

	if (anode_encode_build (asn, data, length) &&
	    anode_validate_anything (asn)) {
		anode_encode_commit (asn);
		*n_data = length;
		return data;
	}

	(allocator) (data, 0);
	anode_encode_rollback (asn);
	return NULL;
}

/* -----------------------------------------------------------------------------------
 * READING, WRITING, GETTING, SETTING
 */

static int
two_to_four_digit_year (int year)
{
	time_t now;
	struct tm tm;
	int century, current;

	g_return_val_if_fail (year >= 0 && year <= 99, -1);

	/* Get the current year */
	now = time (NULL);
	g_return_val_if_fail (now >= 0, -1);
	if (!gmtime_r (&now, &tm))
		g_return_val_if_reached (-1);

	current = (tm.tm_year % 100);
	century = (tm.tm_year + 1900) - current;

	/*
	 * Check if it's within 40 years before the
	 * current date.
	 */
	if (current < 40) {
		if (year < current)
			return century + year;
		if (year > 100 - (40 - current))
			return (century - 100) + year;
	} else {
		if (year < current && year > (current - 40))
			return century + year;
	}

	/*
	 * If it's after then adjust for overflows to
	 * the next century.
	 */
	if (year < current)
		return century + 100 + year;
	else
		return century + year;
}

#ifndef HAVE_TIMEGM
time_t timegm(struct tm *t)
{
	time_t tl, tb;
	struct tm *tg;

	tl = mktime (t);
	if (tl == -1)
	{
		t->tm_hour--;
		tl = mktime (t);
		if (tl == -1)
			return -1; /* can't deal with output from strptime */
		tl += 3600;
	}
	tg = gmtime (&tl);
	tg->tm_isdst = 0;
	tb = mktime (tg);
	if (tb == -1)
	{
		tg->tm_hour--;
		tb = mktime (tg);
		if (tb == -1)
			return -1; /* can't deal with output from gmtime */
		tb += 3600;
	}
	return (tl - (tb - tl));
}
#endif // NOT_HAVE_TIMEGM

static gboolean
parse_utc_time (const gchar *time, gsize n_time,
                struct tm* when, gint *offset)
{
	const char *p, *e;
	int year;

	g_assert (when);
	g_assert (time);
	g_assert (offset);

	/* YYMMDDhhmmss.ffff Z | +0000 */
	if (n_time < 6 || n_time >= 28)
		return FALSE;

	/* Reset everything to default legal values */
	memset (when, 0, sizeof (*when));
	*offset = 0;
	when->tm_mday = 1;

	/* Select the digits part of it */
	p = time;
	for (e = p; *e >= '0' && *e <= '9'; ++e);

	if (p + 2 <= e) {
		year = atoin (p, 2);
		p += 2;

		/*
		 * 40 years in the past is our century. 60 years
		 * in the future is the next century.
		 */
		when->tm_year = two_to_four_digit_year (year) - 1900;
	}
	if (p + 2 <= e) {
		when->tm_mon = atoin (p, 2) - 1;
		p += 2;
	}
	if (p + 2 <= e) {
		when->tm_mday = atoin (p, 2);
		p += 2;
	}
	if (p + 2 <= e) {
		when->tm_hour = atoin (p, 2);
		p += 2;
	}
	if (p + 2 <= e) {
		when->tm_min = atoin (p, 2);
		p += 2;
	}
	if (p + 2 <= e) {
		when->tm_sec = atoin (p, 2);
		p += 2;
	}

	if (when->tm_year < 0 || when->tm_year > 9999 ||
	    when->tm_mon < 0 || when->tm_mon > 11 ||
	    when->tm_mday < 1 || when->tm_mday > 31 ||
	    when->tm_hour < 0 || when->tm_hour > 23 ||
	    when->tm_min < 0 || when->tm_min > 59 ||
	    when->tm_sec < 0 || when->tm_sec > 59)
		return FALSE;

	/* Make sure all that got parsed */
	if (p != e)
		return FALSE;

	/* Now the remaining optional stuff */
	e = time + n_time;

	/* See if there's a fraction, and discard it if so */
	if (p < e && *p == '.' && p + 5 <= e)
		p += 5;

	/* See if it's UTC */
	if (p < e && *p == 'Z') {
		p += 1;

	/* See if it has a timezone */
	} else if ((*p == '-' || *p == '+') && p + 3 <= e) {
		int off, neg;

		neg = *p == '-';
		++p;

		off = atoin (p, 2) * 3600;
		if (off < 0 || off > 86400)
			return -1;
		p += 2;

		if (p + 2 <= e) {
			off += atoin (p, 2) * 60;
			p += 2;
		}

		/* Use TZ offset */
		if (neg)
			*offset = 0 - off;
		else
			*offset = off;
	}

	/* Make sure everything got parsed */
	if (p != e)
		return FALSE;

	return TRUE;
}

static gboolean
parse_general_time (const gchar *time, gsize n_time,
                    struct tm* when, gint *offset)
{
	const char *p, *e;

	g_assert (time);
	g_assert (when);
	g_assert (offset);

	/* YYYYMMDDhhmmss.ffff Z | +0000 */
	if (n_time < 8 || n_time >= 30)
		return FALSE;

	/* Reset everything to default legal values */
	memset (when, 0, sizeof (*when));
	*offset = 0;
	when->tm_mday = 1;

	/* Select the digits part of it */
	p = time;
	for (e = p; *e >= '0' && *e <= '9'; ++e);

	if (p + 4 <= e) {
		when->tm_year = atoin (p, 4) - 1900;
		p += 4;
	}
	if (p + 2 <= e) {
		when->tm_mon = atoin (p, 2) - 1;
		p += 2;
	}
	if (p + 2 <= e) {
		when->tm_mday = atoin (p, 2);
		p += 2;
	}
	if (p + 2 <= e) {
		when->tm_hour = atoin (p, 2);
		p += 2;
	}
	if (p + 2 <= e) {
		when->tm_min = atoin (p, 2);
		p += 2;
	}
	if (p + 2 <= e) {
		when->tm_sec = atoin (p, 2);
		p += 2;
	}

	if (when->tm_year < 0 || when->tm_year > 9999 ||
	    when->tm_mon < 0 || when->tm_mon > 11 ||
	    when->tm_mday < 1 || when->tm_mday > 31 ||
	    when->tm_hour < 0 || when->tm_hour > 23 ||
	    when->tm_min < 0 || when->tm_min > 59 ||
	    when->tm_sec < 0 || when->tm_sec > 59)
		return FALSE;

	/* Make sure all that got parsed */
	if (p != e)
		return FALSE;

	/* Now the remaining optional stuff */
	e = time + n_time;

	/* See if there's a fraction, and discard it if so */
	if (p < e && *p == '.' && p + 5 <= e)
		p += 5;

	/* See if it's UTC */
	if (p < e && *p == 'Z') {
		p += 1;

	/* See if it has a timezone */
	} else if ((*p == '-' || *p == '+') && p + 3 <= e) {
		int off, neg;

		neg = *p == '-';
		++p;

		off = atoin (p, 2) * 3600;
		if (off < 0 || off > 86400)
			return -1;
		p += 2;

		if (p + 2 <= e) {
			off += atoin (p, 2) * 60;
			p += 2;
		}

		/* Use TZ offset */
		if (neg)
			*offset = 0 - off;
		else
			*offset = off;
	}

	/* Make sure everything got parsed */
	if (p != e)
		return FALSE;

	return TRUE;
}

static gboolean
anode_read_time (GNode *node, Atlv *tlv, struct tm *when, glong *value)
{
	const gchar *data;
	gboolean ret;
	gint offset = 0;
	gint flags;

	g_assert (when);
	g_assert (value);

	flags = anode_def_flags (node);
	data = (gchar*)(tlv->buf + tlv->off);

	if (flags & FLAG_GENERALIZED)
		ret = parse_general_time (data, tlv->len, when, &offset);
	else if (flags & FLAG_UTC)
		ret = parse_utc_time (data, tlv->len, when, &offset);
	else
		g_return_val_if_reached (FALSE);

	if (!ret)
		return anode_failure (node, "invalid time content");

	/* In order to work with 32 bit time_t. */
	if (sizeof (time_t) <= 4 && when->tm_year >= 2038) {
		*value = (time_t)2145914603;  /* 2037-12-31 23:23:23 */

	/* Convert to seconds since epoch */
	} else {
		*value = timegm (when);
		g_return_val_if_fail (*time >= 0, FALSE);
		*value += offset;
	}

	return TRUE;
}

static gboolean
anode_read_integer_as_ulong (GNode *node, Atlv *tlv, gulong *value)
{
	const guchar *p;
	gsize k;

	if (tlv->len < 1 || tlv->len > sizeof (gulong))
		return FALSE;

	p = tlv->buf + tlv->off;
	*value = 0;
	for (k = 0; k < tlv->len; ++k)
		*value |= p[k] << (8 * ((tlv->len - 1) - k));

	return TRUE;
}

static gboolean
anode_write_integer_ulong (gulong value, guchar *data, gsize *n_data)
{
	guchar buf[sizeof (gulong)];
	gint bytes, i, off;

	for (i = 0; i < sizeof (gulong); ++i) {
		off = sizeof (gulong) - (i + 1);
		buf[i] = (value >> (off * 8)) & 0xFF;
	}

	for (bytes = sizeof (gulong) - 1; bytes >= 0; --bytes)
		if (!buf[bytes])
			break;

	bytes = sizeof (gulong) - (bytes + 1);
	if (bytes == 0)
		bytes = 1;

	if (data) {
		g_assert (*n_data >= bytes);
		memcpy (data, buf + (sizeof (gulong) - bytes), bytes);
	}
	*n_data = bytes;
	return TRUE;
}

static gboolean
anode_read_string (GNode *node, Atlv *tlv, gpointer value, gsize *n_value)
{
	Atlv ctlv;
	guchar *buf;
	gint n_buf;
	gint i;

	g_assert (tlv);
	g_assert (n_value);

	buf = value;
	n_buf = *n_value;

	/* Is it constructed ? */
	if (tlv->cls & ASN1_CLASS_STRUCTURED) {
		*n_value = 0;
		for (i = 0; TRUE; ++i) {
			if (!anode_decode_tlv_for_contents (tlv, i == 0, &ctlv))
				return anode_failure (node, "invalid encoding of child");
			if (ctlv.off == 0)
				break;
			if (ctlv.cls & ASN1_CLASS_STRUCTURED)
				return FALSE;
			*n_value += ctlv.len;
			if (buf) {
				if (n_buf >= ctlv.len)
					memcpy (buf, ctlv.buf + ctlv.off, ctlv.len);
				buf += ctlv.len;
				n_buf -= ctlv.len;
			}
		}
		if (n_buf < 0)
			return FALSE;

	/* Primitive, just return the contents */
	} else {
		*n_value = tlv->len;
		if (buf) {
			if (n_buf < tlv->len)
				return FALSE;
			memcpy (buf, tlv->buf + tlv->off, tlv->len);
		}
	}

	return TRUE;
}

static gboolean
anode_read_boolean (GNode *node, Atlv *tlv, gboolean *value)
{
	g_assert (node);
	g_assert (tlv);
	g_assert (value);

	if (tlv->len != 1)
		return FALSE;
	if (tlv->buf[tlv->off] == 0x00)
		*value = FALSE;
	else if (tlv->buf[tlv->off] == 0xFF)
		*value = TRUE;
	else
		return FALSE;
	return TRUE;
}

static gboolean
anode_write_boolean (gboolean value, guchar *data, gsize *n_data)
{
	if (data) {
		g_assert (*n_data >= 1);
		if (value)
			data[0] = 0xFF;
		else
			data[0] = 0x00;
	}
	*n_data = 1;
	return TRUE;
}

static gboolean
anode_read_object_id (GNode *node, Atlv *tlv, gchar **oid)
{
	GString *result = NULL;
	const guchar *p;
	gboolean lead;
	guint val, pval;
	gint k;

	g_assert (tlv);
	if (tlv->len <= 0)
		return FALSE;
	p = tlv->buf + tlv->off;

	if (oid)
		result = g_string_sized_new (32);

	pval = p[0] / 40;
	val = p[0] - pval * 40;

	if (result)
		g_string_append_printf (result, "%u.%u", pval, val);

	/* TODO: Validate first byte? */
	for (k = 1, lead = 1, val = 0, pval = 0; k < tlv->len; ++k) {
		/* X.690: the leading byte must never be 0x80 */
		if (lead && p[k] == 0x80) {
			anode_failure (node, "object id encoding is invalid");
			break;
		}
		val = val << 7;
		val |= p[k] & 0x7F;
		/* Check for wrap around */
		if (val < pval) {
			anode_failure (node, "object id encoding is invalid");
			break;
		}
		pval = val;
		if (!(p[k] & 0x80)) {
			if (result)
				g_string_append_printf (result, ".%u", val);
			pval = val = 0;
			lead = 1;
		}
	}

	if (k < tlv->len) {
		if (result)
			g_string_free (result, TRUE);
		return FALSE;
	}

	if (result)
		*oid = g_string_free (result, FALSE);
	return TRUE;
}

static gboolean
anode_write_oid (const gchar *oid, guchar *data, gsize *n_data)
{
	const gchar *p, *next;
	gint num, num1;
	guchar bit7;
	gboolean had;
	gint i, k, at;

	p = oid;
	at = 0;
	num1 = 0;

	for (i = 0; oid[0]; ++i, oid = next) {
		p = strchr (oid, '.');
		if (p == NULL)
			next = p = oid + strlen (oid);
		else
			next = p + 1;
		if (p == oid)
			return FALSE;
		num = atoin (oid, p - oid);
		if (num < 0)
			return FALSE;
		if (i == 0) {
			num1 = num;
		} else if (i == 1) {
			if (data) {
				g_assert (*n_data > at);
				data[at] = 40 * num1 + num;
			}
			++at;
		} else {
			for (had = FALSE, k = 4; k >= 0; k--) {
				bit7 = (num >> (k * 7)) & 0x7F;
				if (bit7 || had || !k) {
					if (k)
						bit7 |= 0x80;
					if (data) {
						g_assert (*n_data > at);
						data[at] = bit7;
					}
					++at;
					had = 1;
				}
			}
		}
	}

	if (at < 2)
		return FALSE;
	if (data)
		g_assert (*n_data >= at);
	*n_data = at;
	return TRUE;
}

GNode*
egg_asn1x_node (GNode *asn, ...)
{
	GNode *node = asn;
	const gchar *name;
	va_list va;
	gint type;
	gint index;

	g_return_val_if_fail (asn, NULL);
	va_start (va, asn);

	for (;;) {
		type = anode_def_type (node);

		/* Use integer indexes for these */
		if (type == TYPE_SEQUENCE_OF || type == TYPE_SET_OF) {
			index = va_arg (va, gint);
			if (index == 0)
				return node;

			/* Only consider nodes that have data */
			node = g_node_nth_child (node, 0);
			while (node) {
				if (egg_asn1x_have (node)) {
					--index;
					if (index == 0)
						break;
				}
				node = g_node_next_sibling (node);
			}

			if (node == NULL)
				return NULL;

		/* Use strings for these */
		} else {
			name = va_arg (va, const gchar*);
			if (name == NULL)
				return node;
			/* Warn if they're using indexes here */
			if (name <= (const gchar*)4096) {
				g_warning ("possible misuse of egg_asn1x_node, expected a string, but got an index");
				return NULL;
			}
			node = anode_child_with_name (node, name);
			if (node == NULL)
				return NULL;
		}
	}
}

const gchar*
egg_asn1x_name (GNode *node)
{
	g_return_val_if_fail (node, NULL);
	return anode_def_name (node);
}

guint
egg_asn1x_count (GNode *node)
{
	guint result = 0;
	GNode *child;
	gint type;

	g_return_val_if_fail (node, 0);

	type = anode_def_type (node);
	if (type != TYPE_SEQUENCE_OF && type != TYPE_SET_OF) {
		g_warning ("node passed to egg_asn1x_count was not a sequence of or set of");
		return 0;
	}

	for (child = node->children; child; child = child->next) {
		if (egg_asn1x_have (child))
			++result;
	}

	return result;
}

GNode*
egg_asn1x_append (GNode *node)
{
	GNode *child;
	gint type;

	g_return_val_if_fail (node, NULL);

	type = anode_def_type (node);
	if (type != TYPE_SEQUENCE_OF && type != TYPE_SET_OF) {
		g_warning ("node passed to egg_asn1x_append was not a sequence of or set of");
		return NULL;
	}

	/* There must be at least one child */
	child = node->children;
	g_return_val_if_fail (child, NULL);

	child = anode_clone (child);
	anode_clear (child);
	g_node_append (node, child);

	return child;
}

gboolean
egg_asn1x_have (GNode *node)
{
	Atlv *tlv;

	g_return_val_if_fail (node, FALSE);

	/* TODO: Handle default values */

	tlv = anode_get_tlv_data (node);
	return tlv != NULL && tlv->buf != NULL;
}

gboolean
egg_asn1x_get_boolean (GNode *node, gboolean *value)
{
	ASN1_ARRAY_TYPE *opt;
	Atlv *tlv;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_BOOLEAN, FALSE);

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL) {

		if ((anode_def_flags (node) & FLAG_DEFAULT) == 0)
			return FALSE;

		/* Try to get a default */
		opt = anode_opt_lookup (node, TYPE_DEFAULT, NULL);
		g_return_val_if_fail (opt, FALSE);

		/* Parse out the default value */
		if ((opt->type & FLAG_TRUE) == FLAG_TRUE)
			*value = TRUE;
		else if ((opt->type & FLAG_FALSE) == FLAG_FALSE)
			*value = FALSE;
		else
			g_return_val_if_reached (FALSE);
		return TRUE;
	}

	return anode_read_boolean (node, tlv, value);
}

gboolean
egg_asn1x_set_boolean (GNode *node, gboolean value)
{
	guchar *data;
	gsize n_data;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_BOOLEAN, FALSE);

	/* TODO: Handle default values */

	n_data = 1;
	data = g_malloc0 (1);
	if (!anode_write_boolean (value, data, &n_data))
		return FALSE;
	anode_encode_tlv_and_enc (node, n_data, anode_encoder_simple, data, g_free);
	return TRUE;
}

gboolean
egg_asn1x_set_null (GNode *node)
{
	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_NULL, FALSE);

	/* Encode zero characters */
	anode_encode_tlv_and_enc (node, 0, anode_encoder_simple, "", NULL);
	return TRUE;
}

GQuark
egg_asn1x_get_enumerated (GNode *node)
{
	gchar buf[sizeof (gulong) * 3];
	ASN1_ARRAY_TYPE *opt;
	gulong val;
	Atlv *tlv;

	g_return_val_if_fail (node, 0);
	g_return_val_if_fail (anode_def_type (node) == TYPE_ENUMERATED, 0);

	tlv = anode_get_tlv_data (node);

	/* TODO: Defaults */

	if (tlv == NULL || tlv->buf == NULL)
		return 0;

	/* TODO: Signed values */

	if (!anode_read_integer_as_ulong (node, tlv, &val))
		return 0;

	/* Format that as a string */
	if (g_snprintf (buf, sizeof (buf), "%lu", val) < 0)
		g_return_val_if_reached (0);

	/* Lookup that value in our table */
	opt = anode_opt_lookup_value (node, TYPE_CONSTANT, buf);
	if (opt == NULL || opt->name == NULL)
		return 0;

	return g_quark_from_static_string (opt->name);
}

gboolean
egg_asn1x_set_enumerated (GNode *node, GQuark value)
{
	ASN1_ARRAY_TYPE *opt;
	const gchar *name;
	gpointer data;
	gsize n_data;
	gulong val;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_ENUMERATED, FALSE);

	/* TODO: Handle default values */

	name = g_quark_to_string (value);
	g_return_val_if_fail (name, FALSE);

	opt = anode_opt_lookup (node, TYPE_CONSTANT, name);
	g_return_val_if_fail (opt && opt->value, FALSE);

	/* TODO: Signed values */

	val = anode_def_value_as_ulong (opt);
	g_return_val_if_fail (val != G_MAXULONG, FALSE);

	n_data = sizeof (gulong);
	data = g_malloc0 (n_data);
	if (!anode_write_integer_ulong (val, data, &n_data))
		return FALSE;

	anode_encode_tlv_and_enc (node, n_data, anode_encoder_simple, data, g_free);
	return TRUE;
}

gboolean
egg_asn1x_get_integer_as_ulong (GNode *node, gulong *value)
{
	const ASN1_ARRAY_TYPE *opt;
	const gchar *defval;
	Atlv *tlv;
	gchar *end;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (value, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_INTEGER, FALSE);

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL) {

		if ((anode_def_flags (node) & FLAG_DEFAULT) == 0)
			return FALSE;

		/* Try to get a default */
		opt = anode_opt_lookup (node, TYPE_DEFAULT, NULL);
		g_return_val_if_fail (opt, FALSE);
		g_return_val_if_fail (opt->value, FALSE);
		defval = opt->value;

		opt = anode_opt_lookup (node, TYPE_CONSTANT, defval);
		if (opt != NULL) {
			g_return_val_if_fail (opt->value, FALSE);
			defval = opt->value;
		}

		/* Parse out the default value */
		*value = strtoul (defval, &end, 10);
		g_return_val_if_fail (end && !end[0], FALSE);
		return TRUE;
	}

	return anode_read_integer_as_ulong (node, tlv, value);
}

gboolean
egg_asn1x_set_integer_as_ulong (GNode *node, gulong value)
{
	guchar *data;
	gsize n_data;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_INTEGER, FALSE);

	/* TODO: Handle default values */

	n_data = sizeof (gulong);
	data = g_malloc0 (n_data);
	if (!anode_write_integer_ulong (value, data, &n_data))
		return FALSE;
	anode_encode_tlv_and_enc (node, n_data, anode_encoder_simple, data, g_free);
	return TRUE;
}

gpointer
egg_asn1x_get_integer_as_raw (GNode *node, EggAllocator allocator, gsize *n_data)
{
	Atlv *tlv;
	gpointer data;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (n_data, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_INTEGER, FALSE);

	if (!allocator)
		allocator = g_realloc;

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL)
		return NULL;

	data = (allocator) (NULL, tlv->len);
	if (data == NULL)
		return NULL;

	memcpy (data, tlv->buf + tlv->off, tlv->len);
	*n_data = tlv->len;
	return data;
}

gboolean
egg_asn1x_set_integer_as_raw (GNode *node, gconstpointer data, gsize n_data, GDestroyNotify destroy)
{
	gboolean sign;
	guchar *p;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (data, FALSE);
	g_return_val_if_fail (n_data > 0, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_INTEGER, FALSE);

	/* Make sure the integer is properly encoded in twos complement*/
	p = (guchar*)data;
	sign = !!(p[0] & 0x80);
	if (sign) {
		g_warning ("integer in egg_asn1x_set_integer_as_raw is not two's complement");
		return FALSE;
	}

	anode_encode_tlv_and_enc (node, n_data, anode_encoder_simple, (gpointer)data, destroy);
	return TRUE;
}

gconstpointer
egg_asn1x_get_raw_element (GNode *node, gsize *n_element)
{
	Atlv *tlv;

	g_return_val_if_fail (node, NULL);
	g_return_val_if_fail (n_element, NULL);

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL)
		return NULL;

	if (anode_calc_explicit (node)) {
		*n_element = (tlv->len + tlv->off) - tlv->oft;
		return tlv->buf + tlv->oft;
	} else {
		*n_element = tlv->len + tlv->off;
		return tlv->buf;
	}
}

gboolean
egg_asn1x_set_raw_element (GNode *node, gpointer data,
                           gsize n_data, GDestroyNotify destroy)
{
	Atlv dtlv, *tlv;
	gint oft, flags;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (data, FALSE);
	g_return_val_if_fail (n_data, FALSE);

	anode_clear (node);
	memset (&dtlv, 0, sizeof (dtlv));

	/* Decode the beginning TLV */
	if (!anode_decode_tlv_for_data (data, (const guchar*)data + n_data, &dtlv))
		return FALSE;

	/*
	 * Decode the data into place properly, to make sure it fits. Note
	 * we are not decoding any explicit outer tagging, this is just
	 * the internal value. In addition we do not support optional
	 * and default values, which would decode successfully in
	 * unexpected ways.
	 */
	flags = anode_def_flags (node);
	flags &= ~(FLAG_TAG | FLAG_DEFAULT | FLAG_OPTION);
	if (!anode_decode_anything_for_flags (node, &dtlv, flags))
		return FALSE;

	/* There was extra data */
	if (dtlv.end - dtlv.buf != n_data)
		return FALSE;

	/* Clear buffer from TLV so it gets encoded */
	tlv = anode_get_tlv_data (node);
	g_assert (tlv);
	tlv->buf = tlv->end = NULL;

	/* Explicit tagging: leave space for the outer tag */
	if (anode_calc_explicit (node)) {
		oft = anode_encode_cls_tag_len (NULL, 0, (ASN1_CLASS_STRUCTURED | ASN1_CLASS_CONTEXT_SPECIFIC),
		                                anode_calc_tag (node), n_data);

		tlv->off += oft;
		tlv->oft = oft;
	}

	/* Setup encoding of the contents */
	anode_set_enc_data (node, anode_encoder_simple, (gpointer)(dtlv.buf + dtlv.off));
	anode_set_user_data (node, data, destroy);
	return TRUE;
}

gconstpointer
egg_asn1x_get_raw_value (GNode *node, gsize *n_content)
{
	Atlv *tlv;

	g_return_val_if_fail (node, NULL);
	g_return_val_if_fail (n_content, NULL);

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL)
		return NULL;
	g_return_val_if_fail (!(tlv->cls & ASN1_CLASS_STRUCTURED), NULL);

	*n_content = tlv->len;
	return tlv->buf + tlv->off;
}

gboolean
egg_asn1x_set_raw_value (GNode *node, gsize length, EggAsn1xEncoder encoder,
                         gpointer user_data, GDestroyNotify destroy)
{
	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (encoder, FALSE);

	anode_encode_tlv_and_enc (node, length, encoder, user_data, destroy);
	return TRUE;
}

guchar*
egg_asn1x_get_string_as_raw (GNode *node, EggAllocator allocator, gsize *n_string)
{
	gsize length;
	guchar *string;
	Atlv *tlv;
	gint type;

	g_return_val_if_fail (node, NULL);
	g_return_val_if_fail (n_string, NULL);

	if (!allocator)
		allocator = g_realloc;

	type = anode_def_type (node);
	g_return_val_if_fail (type == TYPE_OCTET_STRING || type == TYPE_GENERALSTRING, NULL);

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL)
		return NULL;

	if (!anode_read_string (node, tlv, NULL, &length))
		return NULL;

	string = (allocator) (NULL, length + 1);
	if (string == NULL)
		return NULL;

	if (!anode_read_string (node, tlv, string, &length)) {
		(allocator) (string, 0);
		return NULL;
	}

	/* Courtesy null termination, string must however be validated! */
	string[length] = 0;
	*n_string = length;
	return string;
}

gboolean
egg_asn1x_set_string_as_raw (GNode *node, guchar *data, gsize n_data, GDestroyNotify destroy)
{
	gint type;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (data, FALSE);

	type = anode_def_type (node);
	g_return_val_if_fail (type == TYPE_OCTET_STRING || type == TYPE_GENERALSTRING, FALSE);

	anode_encode_tlv_and_enc (node, n_data, anode_encoder_simple, data, destroy);
	return TRUE;
}

gchar*
egg_asn1x_get_string_as_utf8 (GNode *node, EggAllocator allocator)
{
	gchar *string;
	gsize n_string;

	g_return_val_if_fail (node, NULL);

	if (allocator == NULL)
		allocator = g_realloc;

	string = (gchar*)egg_asn1x_get_string_as_raw (node, allocator, &n_string);
	if (!string)
		return NULL;

	if (!g_utf8_validate (string, n_string, NULL)) {
		(allocator) (string, 0);
		return NULL;
	}

	return string;
}

gboolean
egg_asn1x_set_string_as_utf8 (GNode *node, gchar *data, GDestroyNotify destroy)
{
	gsize n_data;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (data, FALSE);

	n_data = strlen (data);
	if (!g_utf8_validate (data, n_data, NULL))
		return FALSE;

	return egg_asn1x_set_string_as_raw (node, (guchar*)data, n_data, destroy);
}

guchar*
egg_asn1x_get_bits_as_raw (GNode *node, EggAllocator allocator, guint *n_bits)
{
	Atlv *tlv;
	gpointer bits;
	guchar padded;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (n_bits, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_BIT_STRING, FALSE);

	if (!allocator)
		allocator = g_realloc;

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL)
		return NULL;

	padded = *(tlv->buf + tlv->off);
	g_return_val_if_fail (padded < 8, NULL);
	g_return_val_if_fail (tlv->len > 1, NULL);

	bits = (allocator) (NULL, tlv->len);
	if (bits == NULL)
		return NULL;

	memcpy (bits, tlv->buf + tlv->off + 1, tlv->len - 1);
	*n_bits = ((tlv->len - 1) * 8) - padded;
	return bits;
}

gboolean
egg_asn1x_set_bits_as_raw (GNode *node, guchar *bits, guint n_bits, GDestroyNotify destroy)
{
	gint type;
	gsize length;
	Abits *ab;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (bits, FALSE);

	type = anode_def_type (node);
	g_return_val_if_fail (type == TYPE_BIT_STRING, FALSE);

	length = (n_bits / 8);
	if (n_bits % 8)
		length += 1;

	ab = g_slice_new0 (Abits);
	ab->bits = bits;
	ab->n_bits = n_bits;
	ab->destroy = destroy;

	anode_encode_tlv_and_enc (node, length + 1, anode_encoder_bit_string, ab, abits_destroy);
	return TRUE;
}

gboolean
egg_asn1x_get_bits_as_ulong (GNode *node, gulong *bits, guint *n_bits)
{
	Atlv *tlv;
	guint i, length;
	guchar empty;
	const guchar *p;
	gulong value;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (bits, FALSE);
	g_return_val_if_fail (n_bits, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_BIT_STRING, FALSE);

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL)
		return FALSE;

	empty = *(tlv->buf + tlv->off);
	g_return_val_if_fail (empty < 8, FALSE);
	g_return_val_if_fail (tlv->len > 1, FALSE);

	length = ((tlv->len - 1) * 8) - empty;
	if (length > sizeof (gulong) * 8)
		return FALSE;

	value = 0;
	p = tlv->buf + tlv->off + 1;

	for (i = 0; i < tlv->len - 1; ++i)
		value = value << 8 | p[i];

	*bits = value >> empty;
	*n_bits = length;
	return TRUE;
}

gboolean
egg_asn1x_set_bits_as_ulong (GNode *node, gulong bits, guint n_bits)
{
	guchar *data;
	gulong value;
	gint type;
	gsize i, length;
	guchar empty;
	Abits *ab;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (bits, FALSE);
	g_return_val_if_fail (n_bits <= sizeof (gulong) * 8, FALSE);

	type = anode_def_type (node);
	g_return_val_if_fail (type == TYPE_BIT_STRING, FALSE);

	empty = n_bits % 8;
	if (empty > 0)
		empty = 8 - empty;
	length = (n_bits / 8) + (empty ? 1 : 0);

	data = g_malloc0 (sizeof (gulong));
	value = bits << empty;

	for (i = 0; i < length; ++i)
		data[(length - i) - 1] = (value >> i * 8) & 0xFF;

	ab = g_slice_new0 (Abits);
	ab->bits = data;
	ab->n_bits = n_bits;
	ab->destroy = g_free;

	anode_encode_tlv_and_enc (node, length + 1, anode_encoder_bit_string, ab, abits_destroy);
	return TRUE;
}

glong
egg_asn1x_get_time_as_long (GNode *node)
{
	struct tm when;
	Atlv *tlv;
	glong time;
	gint type;

	g_return_val_if_fail (node, -1);
	type = anode_def_type (node);

	/* Time is often represented as a choice, so work than in here */
	if (type == TYPE_CHOICE) {
		node = egg_asn1x_get_choice (node);
		if (node == NULL)
			return -1;
		g_return_val_if_fail (anode_def_type (node) == TYPE_TIME, -1);
		return egg_asn1x_get_time_as_long (node);
	}

	g_return_val_if_fail (type == TYPE_TIME, -1);

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL)
		return -1;

	if (!anode_read_time (node, tlv, &when, &time))
		return -1;
	return time;
}

gboolean
egg_asn1x_get_time_as_date (GNode *node, GDate *date)
{
	struct tm when;
	Atlv *tlv;
	glong time;
	gint type;

	g_return_val_if_fail (node, FALSE);
	type = anode_def_type (node);

	/* Time is often represented as a choice, so work than in here */
	if (type == TYPE_CHOICE) {
		node = egg_asn1x_get_choice (node);
		if (node == NULL)
			return FALSE;
		g_return_val_if_fail (anode_def_type (node) == TYPE_TIME, FALSE);
		return egg_asn1x_get_time_as_date (node, date);
	}

	g_return_val_if_fail (type == TYPE_TIME, FALSE);

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL)
		return FALSE;

	if (!anode_read_time (node, tlv, &when, &time))
		return FALSE;

	g_date_set_dmy (date, when.tm_mday, when.tm_mon + 1, when.tm_year + 1900);
	return TRUE;
}

gchar*
egg_asn1x_get_oid_as_string (GNode *node)
{
	gchar *oid;
	Atlv *tlv;

	g_return_val_if_fail (node, NULL);
	g_return_val_if_fail (anode_def_type (node) == TYPE_OBJECT_ID, NULL);

	tlv = anode_get_tlv_data (node);
	if (tlv == NULL || tlv->buf == NULL)
		return NULL;

	if (!anode_read_object_id (node, tlv, &oid))
		return NULL;

	return oid;
}

gboolean
egg_asn1x_set_oid_as_string (GNode *node, const gchar *oid)
{
	guchar *data;
	gsize n_data;

	g_return_val_if_fail (oid, FALSE);
	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_OBJECT_ID, FALSE);

	/* Encoding will always be shorter than string */
	n_data = strlen (oid);
	data = g_malloc0 (n_data);

	if (!anode_write_oid (oid, data, &n_data)) {
		g_free (data);
		return FALSE;
	}

	anode_encode_tlv_and_enc (node, n_data, anode_encoder_simple, data, g_free);
	return TRUE;
}

GQuark
egg_asn1x_get_oid_as_quark (GNode *node)
{
	GQuark quark;
	gchar *oid;

	oid = egg_asn1x_get_oid_as_string (node);
	if (!oid)
		return 0;
	quark = g_quark_from_string (oid);
	g_free (oid);
	return quark;
}

gboolean
egg_asn1x_set_oid_as_quark (GNode *node, GQuark oid)
{
	const gchar *str;

	g_return_val_if_fail (oid, FALSE);
	str = g_quark_to_string (oid);
	g_return_val_if_fail (str, FALSE);

	return egg_asn1x_set_oid_as_string (node, str);
}

GNode*
egg_asn1x_get_choice (GNode *node)
{
	GNode *child;
	Anode *an;

	g_return_val_if_fail (node, NULL);

	/* One and only one of the children must be set */
	for (child = node->children; child; child = child->next) {
		an = (Anode*)child->data;
		if (an->chosen)
			return child;
	}

	return NULL;
}

gboolean
egg_asn1x_set_choice (GNode *node, GNode *choice)
{
	GNode *child;
	Anode *an;

	g_return_val_if_fail (node, FALSE);
	g_return_val_if_fail (anode_def_type (node) == TYPE_CHOICE, FALSE);

	/* One and only one of the children must be set */
	for (child = node->children; child; child = child->next) {
		an = (Anode*)child->data;
		if (child == choice) {
			an->chosen = 1;
			choice = NULL;
		} else {
			an->chosen = 0;
		}
	}

	/* The choice is not one of the child nodes */
	g_return_val_if_fail (!choice, FALSE);

	return TRUE;
}

/* -----------------------------------------------------------------------------------
 * VALIDATION
 */

static gboolean
anode_parse_size (GNode *node, const gchar *text, gulong *value)
{
	ASN1_ARRAY_TYPE *def;
	gchar *end = NULL;

	if (text == NULL) {
		*value = 0;
		return FALSE;
	} else if (g_str_equal (text, "MAX")) {
		*value = G_MAXULONG;
		return TRUE;
	} else if (g_ascii_isalpha (text[0])) {
		def = anode_opt_lookup (node, TYPE_INTEGER, text);
		g_return_val_if_fail (def, FALSE);
		return anode_parse_size (node, def->value, value);
	}

	*value = strtoul (text, &end, 10);
	g_return_val_if_fail (end && !end[0], FALSE);
	return TRUE;
}


static gboolean
anode_validate_size (GNode *node, gulong length)
{
	ASN1_ARRAY_TYPE *size;
	gulong value1 = 0;
	gulong value2 = G_MAXULONG;

	if (anode_def_flags (node) & FLAG_SIZE) {
		size = anode_opt_lookup (node, TYPE_SIZE, NULL);
		g_return_val_if_fail (size, FALSE);
		if (!anode_parse_size (node, size->value, &value1))
			g_return_val_if_reached (FALSE);
		if (size->type & FLAG_MIN_MAX) {
			if (!anode_parse_size (node, size->name, &value2))
				g_return_val_if_reached (FALSE);
			if (length < value1 || length >= value2)
				return anode_failure (node, "content size is out of bounds");
		} else {
			if (length != value1)
				return anode_failure (node, "content size is not correct");
		}
	}

	return TRUE;
}

static gboolean
anode_validate_integer (GNode *node, Atlv *tlv)
{
	GList *constants, *l;
	gulong value, check;
	gboolean found;
	gint flags;

	g_assert (tlv);

	/* Integers must be at least one byte long */
	if (tlv->len <= 0)
		return anode_failure (node, "zero length integer");

	flags = anode_def_flags (node);
	if (flags & FLAG_LIST) {
		/* Parse out the value, we only support small integers*/
		if (!anode_read_integer_as_ulong (node, tlv, &value))
			return anode_failure (node, "integer not part of list");

		/* Look through the list of constants */
		found = FALSE;
		constants = anode_opts_lookup (node, TYPE_CONSTANT, NULL);
		for (l = constants; l; l = g_list_next (l)) {
			check = anode_def_value_as_ulong (l->data);
			g_return_val_if_fail (check != G_MAXULONG, FALSE);
			if (check == value) {
				found = TRUE;
				break;
			}
		}
		g_list_free (constants);

		if (!found)
			return anode_failure (node, "integer not part of listed set");
	}

	return TRUE;
}

static gboolean
anode_validate_enumerated (GNode *node, Atlv *tlv)
{
	g_assert (tlv);

	if (!anode_validate_integer (node, tlv))
		return FALSE;
	g_assert (tlv->len); /* Checked above */
	/* Enumerated must be positive */
	if (tlv->buf[tlv->off] & 0x80)
		return anode_failure (node, "enumerated must be positive");
	return TRUE;
}

static gboolean
anode_validate_boolean (GNode *node, Atlv *tlv)
{
	g_assert (tlv);

	/* Must one byte, and zero or all ones */
	if (tlv->len != 1)
		return anode_failure (node, "invalid length boolean");
	if (tlv->buf[tlv->off] != 0x00 && tlv->buf[tlv->off] != 0xFF)
		return anode_failure (node, "boolean must be true or false");
	return TRUE;
}

static gboolean
anode_validate_bit_string (GNode *node, Atlv *tlv)
{
	guchar empty, mask;
	g_assert (tlv);

	/* At least two bytes in length */
	if (tlv->len < 1)
		return anode_failure (node, "invalid length bit string");
	/* First byte is the number of free bits at end */
	empty = tlv->buf[tlv->off];
	if (empty > 7)
		return anode_failure (node, "bit string has invalid header");
	/* Free bits at end must be zero */
	mask = 0xFF >> (8 - empty);
	if (tlv->len > 1 && tlv->buf[tlv->off + tlv->len - 1] & mask)
		return anode_failure (node, "bit string has invalid trailing bits");
	return TRUE;
}

static gboolean
anode_validate_string (GNode *node, Atlv *tlv)
{
	gsize length;

	if (!anode_read_string (node, tlv, NULL, &length))
		return anode_failure (node, "string content is invalid");

	return anode_validate_size (node, (gulong)length);
}

static gboolean
anode_validate_object_id (GNode *node, Atlv *tlv)
{
	return anode_read_object_id (node, tlv, NULL);
}

static gboolean
anode_validate_null (GNode *node, Atlv *tlv)
{
	g_assert (tlv);
	return (tlv->len == 0);
}

static gboolean
anode_validate_time (GNode *node, Atlv *tlv)
{
	glong time;
	struct tm when;
	return anode_read_time (node, tlv, &when, &time);
}

static gboolean
anode_validate_choice (GNode *node)
{
	GNode *child, *choice;
	Anode *an;

	/* One and only one of the children must be set */
	choice = egg_asn1x_get_choice (node);
	if (!choice)
		return anode_failure (node, "one choice must be set");

	if (!anode_validate_anything (choice))
		return FALSE;

	for (child = node->children; child; child = child->next) {
		if (child != choice) {
			an = (Anode*)child->data;
			if (an->chosen)
				return anode_failure (node, "only one choice may be set");
		}
	}

	return TRUE;
}

static gboolean
anode_validate_sequence_or_set (GNode *node)
{
	GNode *child;
	gulong tag = 0;
	gint count = 0;
	gint type;
	Atlv *tlv;

	type = anode_def_type (node);

	/* All of the children must validate properly */
	for (child = node->children; child; child = child->next) {
		if (!anode_validate_anything (child))
			return FALSE;

		/* Tags must be in ascending order */
		tlv = anode_get_tlv_data (child);
		if (tlv && type == TYPE_SET) {
			if (count > 0 && tag > tlv->tag)
				return anode_failure (node, "content must be in ascending order");
			tag = tlv->tag;
			++count;
		}
	}

	return TRUE;
}

static gboolean
anode_validate_sequence_or_set_of (GNode *node)
{
	GNode *child;
	Atlv *tlv, *ptlv;
	gulong tag;
	gulong count;
	gint type;

	tag = 0;
	count = 0;
	tlv = ptlv = NULL;

	type = anode_def_type (node);

	/* All the children must validate properly */
	for (child = node->children; child; child = child->next) {
		tlv = anode_get_tlv_data (child);
		if (tlv) {
			if (!anode_validate_anything (child))
				return FALSE;

			/* Tag must have same tag as top */
			if (count == 0)
				tag = anode_calc_tag (child);
			else if (tag != G_MAXULONG && tlv->tag != tag)
				return anode_failure (node, "invalid mismatched content");

			/* Set of must be in ascending order */
			if (type == TYPE_SET_OF && ptlv && compare_tlvs (ptlv, tlv) > 0)
				return anode_failure (node, "content must be in ascending order");
			ptlv = tlv;
			++count;
		}
	}

	return anode_validate_size (node, count);
}

static gboolean
anode_validate_anything (GNode *node)
{
	Atlv *tlv;
	gint type;

	type = anode_def_type (node);
	tlv = anode_get_tlv_data (node);

	if (!tlv) {
		if (anode_def_flags (node) & FLAG_OPTION)
			return TRUE;
		if (anode_def_flags (node) & FLAG_DEFAULT)
			return TRUE;
		return anode_failure (node, "missing value");
	}

	g_return_val_if_fail (tlv->buf, FALSE);

	switch (type) {

	/* The primitive value types */
	case TYPE_INTEGER:
		return anode_validate_integer (node, tlv);
	case TYPE_ENUMERATED:
		return anode_validate_enumerated (node, tlv);
	case TYPE_BOOLEAN:
		return anode_validate_boolean (node, tlv);
	case TYPE_BIT_STRING:
		return anode_validate_bit_string (node, tlv);
	case TYPE_OCTET_STRING:
		return anode_validate_string (node, tlv);
	case TYPE_OBJECT_ID:
		return anode_validate_object_id (node, tlv);
	case TYPE_NULL:
		return anode_validate_null (node, tlv);
	case TYPE_GENERALSTRING:
		return anode_validate_string (node, tlv);
	case TYPE_TIME:
		return anode_validate_time (node, tlv);

	/* Transparent types */
	case TYPE_ANY:
		return TRUE;
	case TYPE_CHOICE:
		return anode_validate_choice (node);

	/* Structured types */
	case TYPE_SEQUENCE:
	case TYPE_SET:
		return anode_validate_sequence_or_set (node);

	case TYPE_SEQUENCE_OF:
	case TYPE_SET_OF:
		return anode_validate_sequence_or_set_of (node);

	default:
		g_return_val_if_reached (FALSE);
	}
}

gboolean
egg_asn1x_validate (GNode *asn)
{
	g_return_val_if_fail (asn, FALSE);
	return anode_validate_anything (asn);
}

/* -----------------------------------------------------------------------------------
 * TREE CREATION
 */

static gint
compare_nodes_by_tag (gconstpointer a, gconstpointer b)
{
	GNode *na = (gpointer)a;
	GNode *nb = (gpointer)b;
	gulong taga, tagb;

	g_return_val_if_fail (anode_def_flags (na) & FLAG_TAG, 0);
	g_return_val_if_fail (anode_def_flags (nb) & FLAG_TAG, 0);

	taga = anode_calc_tag (na);
	g_return_val_if_fail (taga != G_MAXULONG, 0);

	tagb = anode_calc_tag (nb);
	g_return_val_if_fail (tagb != G_MAXULONG, 0);

	if (taga == tagb)
		return 0;
	return (taga < tagb) ? -1 : 1;
}

static const ASN1_ARRAY_TYPE*
adef_next_sibling (const ASN1_ARRAY_TYPE *def)
{
	int depth = 0;

	g_assert (def);
	g_assert (def->value || def->type || def->name);

	if ((def->type & FLAG_RIGHT) == 0)
		return NULL;

	/* Skip past any children */
	if ((def->type & FLAG_DOWN) == FLAG_DOWN) {
		depth += 1;
		while (depth > 0) {
			++def;
			if ((def->type & FLAG_DOWN) == FLAG_DOWN)
				depth += 1;
			if ((def->type & FLAG_RIGHT) == 0)
				depth -= 1;
		}
	}

	++def;
	g_return_val_if_fail (def->value || def->type || def->name, NULL);
	return def;
}

static const ASN1_ARRAY_TYPE*
adef_first_child (const ASN1_ARRAY_TYPE *def)
{
	g_assert (def);
	g_assert (def->value || def->type || def->name);

	if ((def->type & FLAG_DOWN) == 0)
		return NULL;

	++def;
	g_return_val_if_fail (def->value || def->type || def->name, NULL);
	return def;
}

static const ASN1_ARRAY_TYPE*
lookup_def_of_type (const ASN1_ARRAY_TYPE *defs, const gchar *name, gint type)
{
	const ASN1_ARRAY_TYPE *def;

	g_assert (defs);
	g_assert (defs->value || defs->type || defs->name);

	for (def = adef_first_child (defs); def; def = adef_next_sibling (def)) {
		if ((def->type & 0xFF) == type && def->name && g_str_equal (name, def->name))
			return def;
	}

	return NULL;
}

static gboolean
traverse_and_prepare (GNode *node, gpointer data)
{
	const ASN1_ARRAY_TYPE *defs = data;
	const ASN1_ARRAY_TYPE *def;
	const gchar *identifier;
	Anode *an, *anj;
	GNode *join = NULL;
	GNode *child, *next;
	GList *list = NULL, *l;

	/* A while, because the stuff we join, could also be an identifier */
	while (anode_def_type (node) == TYPE_IDENTIFIER) {
		an = node->data;
		identifier = an->join ? an->join->value : an->def->value;
		g_return_val_if_fail (identifier, TRUE);
		egg_asn1x_destroy (join);
		join = egg_asn1x_create (defs, identifier);
		g_return_val_if_fail (join, TRUE);
		anj = join->data;
		an->join = anj->def;
	}

	/* Move all the children of join node into our node */
	if (join) {
		list = NULL;
		for (child = join->children, list = NULL; child; child = child->next)
			list = g_list_prepend (list, child);
		list = g_list_reverse (list);
		for (l = list; l; l = g_list_next (l)) {
			child = l->data;
			g_node_unlink (child);
			g_node_append (node, child);
		}
		g_list_free (list);
		list = NULL;
	}

	/* Lookup the max set size */
	if (anode_def_type (node) == TYPE_SIZE) {
		identifier = anode_def_name (node);
		if (identifier && !g_str_equal (identifier, "MAX") &&
		    g_ascii_isalpha (identifier[0])) {
			def = lookup_def_of_type (defs, identifier, TYPE_INTEGER);
			g_return_val_if_fail (def, TRUE);
			anode_opt_add (node, def);
		}
	}

	/* Anything child not a real node, we put into opts */
	if (anode_def_type_is_real (node)) {
		child = node->children;
		while (child) {
			next = child->next;
			if (!anode_def_type_is_real (child)) {
				an = child->data;
				anode_opt_add (node, an->def);
				for (l = an->opts; l; l = g_list_next (l))
					anode_opt_add (node, l->data);
				g_node_unlink (child);
				anode_destroy (child);
			}
			child = next;
		}
	}

	if (join) {
		an = join->data;
		for (l = an->opts; l; l = g_list_next (l))
			anode_opt_add (node, l->data);
		egg_asn1x_destroy (join);
	}

	/* Sort the children of any sets */
	if (anode_def_type (node) == TYPE_SET) {
		for (child = node->children; child; child = child->next)
			list = g_list_prepend (list, child);
		list = g_list_sort (list, compare_nodes_by_tag);
		for (l = list; l; l = g_list_next (l))
			g_node_unlink (l->data);
		for (l = list; l; l = g_list_next (l))
			g_node_append (node, l->data);
		g_list_free (list);
		list = NULL;
	}

	/* Continue traversal */
	return FALSE;
}

static const ASN1_ARRAY_TYPE*
match_oid_in_definition (const ASN1_ARRAY_TYPE *def, GHashTable *names,
                          const gchar *match, const gchar **problem)
{
	const ASN1_ARRAY_TYPE *result = NULL;
	const ASN1_ARRAY_TYPE *odef;
	const gchar *value;
	GString *oid = NULL;

	g_assert (match);
	g_assert (problem);
	g_assert (names);

	for (odef = adef_first_child (def); odef; odef = adef_next_sibling (odef)) {
		if ((odef->type & 0xFF) != TYPE_CONSTANT)
			continue;

		g_return_val_if_fail (odef->value, NULL);
		if (strspn (odef->value, "01234567890") == strlen (odef->value)) {
			value = odef->value;

		} else {
			value = g_hash_table_lookup (names, odef->value);

			/* A name resolution problem */
			if (!value) {
				if (oid)
					g_string_free (oid, TRUE);
				*problem = odef->value;
				return NULL;
			}
		}

		if (oid) {
			g_string_append_c (oid, '.');
			g_string_append (oid, value);
		} else {
			oid = g_string_new (value);
		}
	}

	if (oid != NULL) {
		if (g_str_equal (oid->str, match))
			result = adef_next_sibling (def);
		g_assert (def->name);
		g_hash_table_insert (names, (gchar*)def->name, g_string_free (oid, FALSE));
	}

	return result;
}

static const ASN1_ARRAY_TYPE*
match_oid_in_definitions (const ASN1_ARRAY_TYPE *defs, const gchar *match)
{
	const ASN1_ARRAY_TYPE *def;
	const ASN1_ARRAY_TYPE *result;
	GHashTable *names;
	gboolean progress;
	const gchar *problem;

	names = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, g_free);
	result = NULL;

	for (;;) {
		progress = FALSE;
		problem = NULL;

		for (def = adef_first_child (defs); def; def = adef_next_sibling (def)) {

			/* Only work with object ids, and ones with names */
			if ((def->type & 0xFF) != TYPE_OBJECT_ID || !def->name)
				continue;

			/* If we've already seen this one, skip */
			if (g_hash_table_lookup (names, def->name))
				continue;

			progress = TRUE;
			result = match_oid_in_definition (def, names, match, &problem);
			if (result != NULL)
				break;
		}

		if (!problem || result) {
			break;
		} else if (problem && !progress) {
			g_warning ("couldn't find oid definition in ASN.1 for: %s", problem);
			g_return_val_if_reached (NULL);
		}
	}

	g_hash_table_destroy (names);
	return result;
}

GNode*
egg_asn1x_create (const ASN1_ARRAY_TYPE *defs, const gchar *type)
{
	const ASN1_ARRAY_TYPE *def;
	GNode *root, *parent, *node;
	int flags;

	g_return_val_if_fail (defs, NULL);
	g_return_val_if_fail (type, NULL);

	/* An OID */
	if (strspn (type, "0123456789.") == strlen (type)) {
		def = match_oid_in_definitions (defs, type);

	/* An Identifier */
	} else {
		for (def = adef_first_child (defs); def; def = adef_next_sibling (def)) {
			if (def->name && g_str_equal (type, def->name))
				break;
		}
	}

	if (def == NULL || !def->name || !def->type)
		return NULL;

	/* The node for this item */
	root = anode_new (def);

	/* Build up nodes for underlying level */
	if (def->type & FLAG_DOWN) {
		node = root;
		for (;;) {
			if (def->type & FLAG_DOWN) {
				parent = node;
			} else if (def->type & FLAG_RIGHT) {
				g_assert (node->parent);
				parent = node->parent;
			} else {
				parent = node->parent;
				while (parent) {
					flags = anode_def_flags (parent);
					parent = parent->parent;
					if (flags & FLAG_RIGHT)
						break;
				}
			}

			if (!parent)
				break;

			++def;
			node = anode_new (def);
			g_node_append (parent, node);
		}
	}

	/* Load up sub identifiers */
	g_node_traverse (root, G_POST_ORDER, G_TRAVERSE_ALL, -1,
	                 traverse_and_prepare, (gpointer)defs);

	return root;
}

GNode*
egg_asn1x_create_quark (const ASN1_ARRAY_TYPE *defs, GQuark type)
{
	g_return_val_if_fail (type, NULL);
	return egg_asn1x_create (defs, g_quark_to_string (type));
}

GNode*
egg_asn1x_create_and_decode (const ASN1_ARRAY_TYPE *defs, const gchar *identifier,
                             gconstpointer data, gsize n_data)
{
	GNode *asn;

	g_return_val_if_fail (defs, NULL);
	g_return_val_if_fail (identifier, NULL);

	asn = egg_asn1x_create (defs, identifier);
	g_return_val_if_fail (asn, NULL);

	if (!egg_asn1x_decode (asn, data, n_data)) {
		egg_asn1x_destroy (asn);
		return NULL;
	}

	return asn;
}

/* -----------------------------------------------------------------------------------
 * DUMPING and MESSAGES
 */

static void
dump_append_type (GString *output, gint type)
{
	#define XX(x) if (type == TYPE_##x) g_string_append (output, #x " ")
	XX(CONSTANT); XX(IDENTIFIER); XX(INTEGER); XX(BOOLEAN); XX(SEQUENCE); XX(BIT_STRING);
	XX(OCTET_STRING); XX(TAG); XX(DEFAULT); XX(SIZE); XX(SEQUENCE_OF); XX(OBJECT_ID); XX(ANY);
	XX(SET); XX(SET_OF); XX(DEFINITIONS); XX(TIME); XX(CHOICE); XX(IMPORTS); XX(NULL);
	XX(ENUMERATED); XX(GENERALSTRING);
	if (output->len == 0)
		g_string_printf (output, "%d ", (int)type);
	#undef XX
}

static void
dump_append_flags (GString *output, gint flags)
{
	#define XX(x) if ((FLAG_##x & flags) == FLAG_##x) g_string_append (output, #x " ")
	XX(UNIVERSAL); XX(PRIVATE); XX(APPLICATION); XX(EXPLICIT); XX(IMPLICIT); XX(TAG); XX(OPTION);
	XX(DEFAULT); XX(TRUE); XX(FALSE); XX(LIST); XX(MIN_MAX); XX(1_PARAM); XX(SIZE); XX(DEFINED_BY);
	XX(GENERALIZED); XX(UTC); XX(IMPORTS); XX(NOT_USED); XX(SET); XX(ASSIGN);
	/* XX(DOWN); XX(RIGHT); */
	#undef XX
}

static gboolean
traverse_and_dump (GNode *node, gpointer unused)
{
	ASN1_ARRAY_TYPE *def;
	guint i, depth;
	GString *output;
	gchar *string;
	Atlv *tlv;
	Anode *an;
	GList *l;

	depth = g_node_depth (node);
	for (i = 0; i < depth - 1; ++i)
		g_printerr ("    ");

	tlv = anode_get_tlv_data (node);
	output = g_string_new ("");
	dump_append_type (output, anode_def_type (node));
	dump_append_flags (output, anode_def_flags (node));
	string = g_utf8_casefold (output->str, output->len - 1);
	g_string_free (output, TRUE);
	g_printerr ("+ %s: %s [%s]%s\n", anode_def_name (node), anode_def_value (node),
	            string, tlv && tlv->buf ? " *" : "");
	g_free (string);

	/* Print out all the options */
	an = node->data;
	for (l = an->opts; l; l = g_list_next (l)) {
		for (i = 0; i < depth; ++i)
			g_printerr ("    ");

		def = l->data;
		output = g_string_new ("");
		dump_append_type (output, def->type & 0xFF);
		dump_append_flags (output, def->type);
		string = g_utf8_casefold (output->str, output->len - 1);
		g_string_free (output, TRUE);
		g_printerr ("- %s: %s [%s]\n", def->name, (const gchar*)def->value, string);
		g_free (string);
	}

	return FALSE;
}

void
egg_asn1x_dump (GNode *asn)
{
	g_return_if_fail (asn);
	g_node_traverse (asn, G_PRE_ORDER, G_TRAVERSE_ALL, -1, traverse_and_dump, NULL);
}

static gboolean
traverse_and_get_failure (GNode *node, gpointer user_data)
{
	const gchar **failure = user_data;
	g_assert (!*failure);
	*failure = anode_failure_get (node);
	return (*failure != NULL);
}

const gchar*
egg_asn1x_message (GNode *asn)
{
	const gchar *failure = NULL;
	g_return_val_if_fail (asn, NULL);
	g_node_traverse (asn, G_POST_ORDER, G_TRAVERSE_ALL, -1, traverse_and_get_failure, &failure);
	return failure;
}

/* -----------------------------------------------------------------------------------
 * CLEARING and DESTROYING
 */

static gboolean
traverse_and_clear (GNode *node, gpointer unused)
{
	GNode *child, *next;
	gint type;

	anode_clear (node);

	type = anode_def_type (node);
	if (type == TYPE_SET_OF || type == TYPE_SEQUENCE_OF) {

		/* The first 'real' child is the template */
		child = node->children;
		g_return_val_if_fail (child, TRUE);

		/* And any others are extras */
		child = child->next;
		while (child) {
			next = child->next;
			anode_destroy (child);
			child = next;
		}
	}

	/* Don't stop traversal */
	return FALSE;
}

void
egg_asn1x_clear (GNode *asn)
{
	g_return_if_fail (asn);
	g_node_traverse (asn, G_POST_ORDER, G_TRAVERSE_ALL, -1, traverse_and_clear, NULL);
}

void
egg_asn1x_destroy (gpointer data)
{
	GNode *node = data;

	if (node != NULL) {
		g_return_if_fail (G_NODE_IS_ROOT (node));
		anode_destroy (node);
	}
}

/* --------------------------------------------------------------------------------
 * TIME PARSING
 */

glong
egg_asn1x_parse_time_general (const gchar *time, gssize n_time)
{
	gboolean ret;
	glong value;
	struct tm when;
	gint offset = 0;

	g_return_val_if_fail (time, -1);

	if (n_time < 0)
		n_time = strlen (time);

	ret = parse_general_time (time, n_time, &when, &offset);
	if (!ret)
		return -1;

	/* In order to work with 32 bit time_t. */
	if (sizeof (time_t) <= 4 && when.tm_year >= 2038) {
		value = (time_t)2145914603;  /* 2037-12-31 23:23:23 */

	/* Convert to seconds since epoch */
	} else {
		value = timegm (&when);
		g_return_val_if_fail (*time >= 0, FALSE);
		value += offset;
	}

	return value;
}

glong
egg_asn1x_parse_time_utc (const gchar *time, gssize n_time)
{
	gboolean ret;
	glong value;
	struct tm when;
	gint offset = 0;

	g_return_val_if_fail (time, -1);

	if (n_time < 0)
		n_time = strlen (time);

	ret = parse_utc_time (time, n_time, &when, &offset);
	if (!ret)
		return -1;

	/* In order to work with 32 bit time_t. */
	if (sizeof (time_t) <= 4 && when.tm_year >= 2038) {
		value = (time_t)2145914603;  /* 2037-12-31 23:23:23 */

	/* Convert to seconds since epoch */
	} else {
		value = timegm (&when);
		g_return_val_if_fail (*time >= 0, FALSE);
		value += offset;
	}

	return value;
}

/* --------------------------------------------------------------------------------
 * BASIC RAW ELEMENT INFO
 */

gssize
egg_asn1x_element_length (gconstpointer data, gsize n_data)
{
	guchar cls;
	int counter = 0;
	int cb, len;
	gulong tag;

	if (anode_decode_cls_tag (data, (const guchar*)data + n_data, &cls, &tag, &cb)) {
		counter += cb;
		len = anode_decode_length ((const guchar*)data + cb, (const guchar*)data + n_data, &cb);
		counter += cb;
		if (len >= 0) {
			len += counter;
			if (n_data >= len)
				return len;
		}
	}

	return -1;
}

gconstpointer
egg_asn1x_element_content (gconstpointer data, gsize n_data, gsize *n_content)
{
	int counter = 0;
	guchar cls;
	gulong tag;
	int cb, len;

	g_return_val_if_fail (data != NULL, NULL);
	g_return_val_if_fail (n_content != NULL, NULL);

	/* Now get the data out of this element */
	if (!anode_decode_cls_tag (data, (const guchar*)data + n_data, &cls, &tag, &cb))
		return NULL;

	counter += cb;
	len = anode_decode_length ((const guchar*)data + cb, (const guchar*)data + n_data, &cb);
	if (len < 0)
		return NULL;
	counter += cb;

	*n_content = len;
	return (const guchar*)data + counter;
}
