#include "config.h"
#include <string.h>
#include <orbit/orbit.h>
#include "orb-core-export.h"
#include "../util/orbit-purify.h"

static gboolean
free_entry (gpointer key, gpointer value, gpointer user_data)
{
	g_free (key);
	g_free (value);

	return TRUE;
}

static void ORBit_Context_free_fn(ORBit_RootObject ctx);

static void
free_child (gpointer value, gpointer user_data)
{
	CORBA_Context ctx = value;

	ctx->parent.refs = 0;
	ctx->parent_ctx = CORBA_OBJECT_NIL;
	ORBit_Context_free_fn (ORBIT_ROOT_OBJECT (ctx));
}

static void
ORBit_Context_free_fn (ORBit_RootObject obj_in)
{
	CORBA_Context ctx = (CORBA_Context) obj_in;

	if (ctx->children) {
		g_slist_foreach (ctx->children, free_child, NULL);
		g_slist_free (ctx->children);
	}

	if (ctx->mappings) {
		g_hash_table_foreach_remove (ctx->mappings, free_entry, NULL);
		g_hash_table_destroy (ctx->mappings);
	}

	if (ctx->parent_ctx != CORBA_OBJECT_NIL)
		ctx->parent_ctx->children = g_slist_remove (
			ctx->parent_ctx->children, ctx);
	  
	g_free (ctx->the_name);
	
	p_free (ctx, struct CORBA_Context_type);
}

static const ORBit_RootObject_Interface CORBA_Context_epv =
{
	ORBIT_ROT_CONTEXT,
	ORBit_Context_free_fn
};

static CORBA_Context
CORBA_Context_new (CORBA_Context      parent,
		   const char        *name,
		   CORBA_Environment *ev)
{
	CORBA_Context retval;

	retval = g_new0 (struct CORBA_Context_type, 1);

	ORBit_RootObject_init (&retval->parent, &CORBA_Context_epv);

	if (name)
		retval->the_name = g_strdup (name);

	retval->parent_ctx = parent;
	if (parent)
		parent->children = g_slist_prepend (parent->children, retval);

	return ORBit_RootObject_duplicate (retval);
}

void
CORBA_ORB_get_default_context (CORBA_ORB          orb,
			       CORBA_Context     *ctx,
			       CORBA_Environment *ev)
{
	g_return_if_fail (ev != NULL);

	if (!orb->default_ctx)
		orb->default_ctx = CORBA_Context_new (
			CORBA_OBJECT_NIL, NULL, ev);

	*ctx = ORBit_RootObject_duplicate (orb->default_ctx);
}

void
CORBA_Context_set_one_value (CORBA_Context      ctx,
			     const CORBA_char  *prop_name,
			     const CORBA_char  *value,
			     CORBA_Environment *ev)
{
	gpointer old_nom, old_value;

	g_return_if_fail (ev != NULL);

	if (!ctx->mappings)
		ctx->mappings = g_hash_table_new (g_str_hash, g_str_equal);

	if (g_hash_table_lookup_extended (
		ctx->mappings, prop_name, &old_nom, &old_value)) {
		g_hash_table_remove (ctx->mappings, prop_name);
		g_free (old_nom);
		g_free (old_value);
	}

	g_hash_table_insert (ctx->mappings,
			     g_strdup (prop_name),
			     g_strdup (value));
}

void
CORBA_Context_set_values(CORBA_Context ctx,
			 const CORBA_NVList values,
			 CORBA_Environment * ev)
{
  int i;

  for(i = 0; i < values->list->len; i++)
    {
      CORBA_NamedValue *nvp;

      nvp = ((CORBA_NamedValue *)values->list->data) + i;

      g_assert(nvp->argument._type == TC_CORBA_string);

      CORBA_Context_set_one_value(ctx, nvp->name, nvp->argument._value, ev);
    }
}

typedef struct {
	CORBA_Context      ctx;
	const CORBA_char  *prop_name;
	CORBA_NVList       values;
	CORBA_Environment *ev;
	int                len;
} CTXSearchInfo;

static gboolean
list_has_key (CORBA_NVList list, const char *key)
{
	int i;

	for (i = 0; i < list->list->len; i++) {
		CORBA_NamedValue *nvp;

		nvp = ((CORBA_NamedValue *)list->list->data) + i;

		if (!strcmp(nvp->name, key))
			return TRUE;
	}

	return FALSE;
}

static void
search_props (gpointer       key,
	      gpointer       value,
	      CTXSearchInfo *csi)
{
	if (strncmp (key, csi->prop_name, csi->len))
		return;

	if (list_has_key (csi->values, key))
		return;

	CORBA_NVList_add_item (
		csi->values, key, TC_CORBA_string,
		(CORBA_OpaqueValue) &value,
		strlen (value) + 1, CORBA_IN_COPY_VALUE, NULL);
}

static void
ctx_get_values (CORBA_Context      ctx,
		CORBA_Flags        op_flags,
		const CORBA_char  *prop_name,
		CORBA_NVList      *values,
		gint               is_wc,
		CORBA_Environment *ev)
{
	gboolean go_up = FALSE;

	if (is_wc >= 0) {
		CTXSearchInfo csi;
  
		csi.ctx = ctx;
		csi.prop_name = prop_name;
		csi.values = *values;
		csi.ev = ev;
		csi.len = is_wc;

		if (ctx->mappings)
			g_hash_table_foreach (
				ctx->mappings, (GHFunc) search_props, &csi);

		go_up = TRUE;
	} else {
		char *val = NULL;

		if (ctx->mappings)
			val = g_hash_table_lookup (ctx->mappings, prop_name);

		if (val)
			CORBA_NVList_add_item (
				*values, prop_name, TC_CORBA_string,
				(CORBA_OpaqueValue) &val,
				strlen (val) + 1, CORBA_IN_COPY_VALUE, ev);
		else
			go_up = TRUE;
	}

	if (go_up && ctx->parent_ctx &&
	    !(op_flags & CORBA_CTX_RESTRICT_SCOPE))
		ctx_get_values (ctx->parent_ctx, op_flags,
				prop_name, values, is_wc, ev);
}

void
CORBA_Context_get_values (CORBA_Context      ctx,
			  const CORBA_char  *start_scope,
			  const CORBA_Flags  op_flags,
			  const CORBA_char  *prop_name,
			  CORBA_NVList      *values,
			  CORBA_Environment *ev)
{
	if (start_scope && *start_scope) {
		while (ctx && (!ctx->the_name ||
			       strcmp (ctx->the_name, start_scope)))
			ctx = ctx->parent_ctx;

		if (!ctx) {
			CORBA_exception_set_system (
				ev, ex_CORBA_INV_IDENT, CORBA_COMPLETED_NO);
			return;
		}
	}

	CORBA_ORB_create_list (CORBA_OBJECT_NIL, 0, values, ev);

	ctx_get_values (ctx, op_flags, prop_name, values,
			(prop_name [strlen (prop_name) - 1] == '*'), ev);

	if ((*values)->list->len == 0) {
		CORBA_NVList_free (*values, ev);
		*values = NULL;
		CORBA_exception_set_system (
			ev, ex_CORBA_UNKNOWN, CORBA_COMPLETED_NO);
	}
}

static void
delete_props (gpointer key, gpointer value, CTXSearchInfo *csi)
{
	if (strncmp (key, csi->prop_name, csi->len))
		return;

	g_hash_table_remove (csi->ctx->mappings, key);
	g_free (key);
	g_free (value);
}

void
CORBA_Context_delete_values (CORBA_Context      ctx,
			     const CORBA_char  *prop_name,
			     CORBA_Environment *ev)
{
	char *ctmp;
	int   wc_pos;

	if (!ctx->mappings)
		return;

	ctmp = strchr (prop_name, '*');
	if (ctmp)
		wc_pos = ctmp - prop_name;
	else
		wc_pos = -1;

	if (wc_pos >= 0) {
		CTXSearchInfo csi;

		memset (&csi, 0, sizeof (csi));
		csi.ctx = ctx;
		csi.prop_name = prop_name;
		csi.ev = ev;
		csi.len = wc_pos;

		g_hash_table_foreach (ctx->mappings, (GHFunc) delete_props, &csi);
	} else {
		gpointer old_nom, old_value;

		if (g_hash_table_lookup_extended (
			ctx->mappings, prop_name, &old_nom, &old_value)) {
			g_free (old_nom);
			g_free (old_value);
		}
	}
}

void
CORBA_Context_create_child (CORBA_Context      ctx,
			    const CORBA_char  *ctx_name,
			    CORBA_Context     *child_ctx,
			    CORBA_Environment *ev)
{
	*child_ctx = CORBA_Context_new (ctx, ctx_name, ev);
}

void
CORBA_Context_delete (CORBA_Context      ctx,
		      const CORBA_Flags  del_flags,
		      CORBA_Environment *ev)
{
	if ((del_flags & CORBA_CTX_DELETE_DESCENDENTS) ||
	    !ctx->children)
		free_child (ctx, NULL);
}

void
ORBit_Context_marshal (CORBA_Context                   ctx, 
		       const ORBit_ContextMarshalItem *mlist,
		       CORBA_unsigned_long             nitems, 
		       GIOPSendBuffer                 *buf)
{
	CORBA_unsigned_long  real_nitems, ltmp;
	guchar              *marker;
	int                  i;

	marker = giop_send_buffer_append_aligned (buf, &nitems, 4);

	if (!ctx->mappings) {
		real_nitems = 0;
		memcpy (marker, &real_nitems, 4);

		return;
	}

	for (real_nitems = i = 0; i < nitems; i++) {
		char *value;

		value = g_hash_table_lookup (ctx->mappings, mlist[i].str);
		if (!value)
			continue;

		/* Key */
		giop_send_buffer_append_aligned (buf, &mlist[i].len, sizeof(mlist[i].len));
		giop_send_buffer_append (buf, mlist[i].str, mlist[i].len);
		real_nitems++;

		/* Value */
		ltmp = strlen (value) + 1;
		giop_send_buffer_append_aligned (buf, &ltmp, 4);
		giop_send_buffer_append (buf, value, ltmp);
		real_nitems++;
	}

	memcpy (marker, &real_nitems, 4);
}

#define ALIGNFOR(x) recv_buffer->cur = ALIGN_ADDRESS(recv_buffer->cur, sizeof(x))

gboolean
ORBit_Context_demarshal (CORBA_Context   parent,
			 CORBA_Context   initme,
			 GIOPRecvBuffer *buf)
{
	CORBA_unsigned_long nstrings, keylen, vallen, i;
	char               *key, *value;

	initme->parent.refs = ORBIT_REFCOUNT_STATIC;
	initme->parent_ctx = parent;
	initme->mappings = NULL;
 
	buf->cur = ALIGN_ADDRESS (buf->cur, 4);
	if ((buf->cur + 4) > buf->end)
		goto errout;
	nstrings = *(CORBA_unsigned_long *) buf->cur;
	if (giop_msg_conversion_needed (buf))
		nstrings = GUINT32_SWAP_LE_BE (nstrings);
	buf->cur += 4;
	if ((buf->cur + nstrings * 8) > buf->end)
		goto errout;

	if (nstrings)
		initme->mappings = g_hash_table_new (g_str_hash, g_str_equal);
	else
		goto errout;

	for (i = 0; i < nstrings; ) {
		buf->cur = ALIGN_ADDRESS (buf->cur, 4);
		if ((buf->cur + 4) > buf->end)
			goto errout;
		keylen = *(CORBA_unsigned_long *) buf->cur;
		if (giop_msg_conversion_needed (buf))
			keylen = GUINT32_SWAP_LE_BE(keylen);
		buf->cur += 4;
		if ((buf->cur + keylen) > buf->end ||
		    (buf->cur + keylen) < buf->cur)
			goto errout;
		key = buf->cur;
		buf->cur += keylen;
		i++;

		if (i >= nstrings)
			break;

		buf->cur = ALIGN_ADDRESS (buf->cur, 4);
		if ((buf->cur + 4) > buf->end)
			goto errout;
		vallen = *(CORBA_unsigned_long *) buf->cur;
		if (giop_msg_conversion_needed (buf))
			vallen = GUINT32_SWAP_LE_BE(vallen);
		buf->cur += 4;
		if ((buf->cur + vallen) > buf->end ||
		    (buf->cur + vallen) < buf->cur)
			goto errout;
		value = buf->cur;
		buf->cur += vallen;
		i++;

		g_hash_table_insert (initme->mappings, key, value);
	}

	return FALSE;

 errout:
	if (initme->mappings)
		g_hash_table_destroy (initme->mappings);

	return TRUE;
}

void
ORBit_Context_server_free (CORBA_Context ctx)
{
	g_hash_table_destroy (ctx->mappings);
}
