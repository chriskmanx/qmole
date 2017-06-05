#include "config.h"
#include <orbit/orbit.h>
#include <string.h>
#include "../util/orbit-purify.h"

typedef struct {
	CORBA_TypeCode tc;
	guint index; 
} TCRecursionNode;

typedef struct {
	GSList *prior_tcs;   /* Could be a hash table by typecode */
	guint   start_idx;
} TCEncodeContext;

typedef struct {
	GSList *prior_tcs;   /* Could be a hash table by offset */
	guint   current_idx; /* The offset from the start of the toplevel buffer of this buffer */
} TCDecodeContext;

/* Two methods called in co-recursion */
static gboolean tc_dec (CORBA_TypeCode  *t,
			GIOPRecvBuffer  *c,
			TCDecodeContext *ctx);
static void     tc_enc (CORBA_TypeCode   tc,
			GIOPSendBuffer  *buf,
			TCEncodeContext *ctx);

#define get_wptr(buf) (buf->msg.header.message_size - ctx->start_idx)
#define get_rptr(buf) (buf->cur-buf->message_body)

#define CDR_put_string(buf,str) (giop_send_buffer_append_string ((buf), (str)))
#define CDR_put_ulong(buf, n) (giop_send_buffer_align(buf, sizeof(n)), \
			       giop_send_buffer_append(buf, &(n), sizeof(n)))
#define CDR_put_long(buf, n) CDR_put_ulong(buf, n)
#define CDR_put_ulong_long(buf, n) CDR_put_ulong(buf, n)
#define CDR_put_wchar(buf, n) CDR_put_ulong(buf, n)
#define CDR_put_octet(buf, n) CDR_put_ulong(buf, n)
#define CDR_put_ushort(buf, n) CDR_put_ulong(buf, n)
#define CDR_put_short(buf, n) CDR_put_ulong(buf, n)

typedef void (*CORBA_TypeCodeEncoder)(CORBA_TypeCode t,
				      GIOPSendBuffer *c,
				      TCEncodeContext* ctx);

typedef gboolean (*CORBA_TypeCodeDecoder)(CORBA_TypeCode t,
					  GIOPRecvBuffer *c,
					  TCDecodeContext* ctx);

typedef enum {
	TK_EMPTY,
	TK_SIMPLE,
	TK_COMPLEX
} TkType;

typedef struct {
	TkType                type;
	CORBA_TypeCodeEncoder encoder;
	CORBA_TypeCodeDecoder decoder;
	CORBA_TypeCode        basic_type;
} TkInfo;

#define DEF_TC_BASIC(nom, c_align)                                      \
	ORBIT2_MAYBE_CONST struct CORBA_TypeCode_struct TC_CORBA_##nom##_struct = { \
		{&ORBit_TypeCode_epv, ORBIT_REFCOUNT_STATIC},           \
		CORBA_tk_##nom,                                         \
		0, 0, c_align,						\
		0, 0, NULL, NULL,					\
		#nom,                                                   \
		"IDL:omg.org/CORBA/" #nom ":1.0",			\
		NULL, CORBA_OBJECT_NIL, -1				\
}

#define CORBA_tk_Object CORBA_tk_objref
#define CORBA_tk_unsigned_long CORBA_tk_ulong
#define CORBA_tk_long_long CORBA_tk_longlong
#define CORBA_tk_unsigned_long_long CORBA_tk_ulonglong
#define CORBA_tk_unsigned_short CORBA_tk_ushort
#define CORBA_tk_long_double CORBA_tk_longdouble

ORBIT2_MAYBE_CONST struct CORBA_TypeCode_struct TC_null_struct = {
	{&ORBit_TypeCode_epv, ORBIT_REFCOUNT_STATIC},
	CORBA_tk_null, 0, 0, -1, 0, 0, NULL,
	CORBA_OBJECT_NIL, "null", "IDL:omg.org/CORBA/Null:1.0"
};

ORBIT2_MAYBE_CONST struct CORBA_TypeCode_struct TC_void_struct = {
	{&ORBit_TypeCode_epv, ORBIT_REFCOUNT_STATIC},
	CORBA_tk_void, 0, 0, -1, 0, 0, NULL,
	CORBA_OBJECT_NIL, "void", "IDL:omg.org/CORBA/void:1.0"
};

DEF_TC_BASIC(char, ORBIT_ALIGNOF_CORBA_CHAR);
DEF_TC_BASIC(wchar, ORBIT_ALIGNOF_CORBA_WCHAR);
DEF_TC_BASIC(string, ORBIT_ALIGNOF_CORBA_POINTER);
DEF_TC_BASIC(long, ORBIT_ALIGNOF_CORBA_LONG);
DEF_TC_BASIC(unsigned_long, ORBIT_ALIGNOF_CORBA_LONG);
DEF_TC_BASIC(float, ORBIT_ALIGNOF_CORBA_FLOAT);
DEF_TC_BASIC(double, ORBIT_ALIGNOF_CORBA_DOUBLE);
DEF_TC_BASIC(short, ORBIT_ALIGNOF_CORBA_SHORT);
DEF_TC_BASIC(unsigned_short, ORBIT_ALIGNOF_CORBA_SHORT);
DEF_TC_BASIC(boolean, ORBIT_ALIGNOF_CORBA_BOOLEAN);
DEF_TC_BASIC(octet, ORBIT_ALIGNOF_CORBA_OCTET);
DEF_TC_BASIC(any, ORBIT_ALIGNOF_CORBA_ANY);
DEF_TC_BASIC(TypeCode, ORBIT_ALIGNOF_CORBA_POINTER);
DEF_TC_BASIC(Principal, 1); /* Hmm */
DEF_TC_BASIC(Object, ORBIT_ALIGNOF_CORBA_POINTER);
DEF_TC_BASIC(wstring, ORBIT_ALIGNOF_CORBA_POINTER);
DEF_TC_BASIC(long_double, ORBIT_ALIGNOF_CORBA_LONG_DOUBLE);
DEF_TC_BASIC(long_long, ORBIT_ALIGNOF_CORBA_LONG_LONG);
DEF_TC_BASIC(unsigned_long_long, ORBIT_ALIGNOF_CORBA_LONG_LONG);

static void
tc_enc_tk_objref(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
	CDR_put_string (c, t->repo_id);
	CDR_put_string (c, t->name);
}

static void
tc_enc_tk_sequence(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
	tc_enc (*t->subtypes, c, ctx);
	CDR_put_ulong (c, t->length);
}

static void
tc_enc_tk_string(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
	CDR_put_ulong(c, t->length);
}

static void
tc_enc_tk_struct(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
	CORBA_unsigned_long i;

	CDR_put_string(c, t->repo_id);
	CDR_put_string(c, t->name);
	CDR_put_ulong(c, t->sub_parts);

	for(i = 0; i < t->sub_parts; i++) {
		CDR_put_string (c, t->subnames [i]);
		tc_enc (t->subtypes [i], c, ctx);
	}
}

static void
tc_enc_tk_union (CORBA_TypeCode   t,
		 GIOPSendBuffer  *c,
		 TCEncodeContext *ctx)
{
	CORBA_unsigned_long i;

	CDR_put_string (c, t->repo_id);
	CDR_put_string (c, t->name);
	tc_enc (t->discriminator, c, ctx);
	CDR_put_long (c, t->default_index);
	CDR_put_ulong (c, t->sub_parts);

	i = t->sub_parts;

#define MEMBER_LOOPER_ENC(putname, typename, tkname)		 		\
	case CORBA_tk_##tkname:							\
		for(i = 0; i < t->sub_parts; i++) {				\
			CORBA_##typename tmp;					\
			tmp = (CORBA_##typename) t->sublabels [i];		\
			CDR_put_##putname (c, tmp);				\
			CDR_put_string (c, t->subnames [i]);			\
			tc_enc (t->subtypes [i], c, ctx);			\
		}								\
		break

#define UNION_MEMBERS(dir)							\
	MEMBER_LOOPER_##dir(ulong, long, long);					\
    case CORBA_tk_enum: /* fall through */					\
	MEMBER_LOOPER_##dir(ulong, unsigned_long, ulong);			\
	MEMBER_LOOPER_##dir(octet, boolean, boolean);				\
	MEMBER_LOOPER_##dir(octet, char, char);					\
	MEMBER_LOOPER_##dir(ushort, short, short);				\
	MEMBER_LOOPER_##dir(ushort, unsigned_short, ushort);			\
	MEMBER_LOOPER_##dir(ulong_long, long_long, longlong);			\
	MEMBER_LOOPER_##dir(ulong_long, unsigned_long_long, ulonglong);		\
	MEMBER_LOOPER_##dir(wchar, wchar, wchar);

	switch (t->discriminator->kind) {
		UNION_MEMBERS (ENC);
	default:
		g_error ("tc_enc_tk_union: Illegal union discriminator "
			 "type %s\n", t->discriminator->name);
		break;
    }
}

static void
tc_enc_tk_enum(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
  CORBA_unsigned_long i;
  CDR_put_string(c, t->repo_id);
  CDR_put_string(c, t->name);
  CDR_put_ulong(c, t->sub_parts);
  for(i=0;i<t->sub_parts;i++)
    CDR_put_string(c, t->subnames[i]);
}

static void
tc_enc_tk_alias(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
  CDR_put_string(c, t->repo_id);
  CDR_put_string(c, t->name);
  tc_enc(*t->subtypes, c, ctx);
}

static void
tc_enc_tk_except(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
  gulong i;
  CDR_put_string(c, t->repo_id);
  CDR_put_string(c, t->name);
  CDR_put_ulong(c, t->sub_parts);
  for(i=0;i<t->sub_parts;i++){
    CDR_put_string(c, t->subnames[i]);
    tc_enc(t->subtypes[i], c, ctx);
  }
}

static void
tc_enc_tk_array(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
  tc_enc(*t->subtypes, c, ctx);
  CDR_put_ulong(c, t->length);
}

static void
tc_enc_tk_wstring(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
  CDR_put_ulong(c, t->length);
}

static void
tc_enc_tk_fixed(CORBA_TypeCode t, GIOPSendBuffer *c, TCEncodeContext* ctx)
{
  CDR_put_ushort(c, t->digits);
  CDR_put_short(c, t->scale);
}

static void
ORBit_TypeCode_free_fn (ORBit_RootObject obj_in)
{
	CORBA_TypeCode tc = (CORBA_TypeCode) obj_in;
	int i;

	g_free ((char*)(tc->name));
	g_free ((char*)(tc->repo_id));

	for (i = 0; i < tc->sub_parts; i++) {
		if (tc->subnames)
			g_free ((char*)(tc->subnames [i]));

		if (tc->subtypes)
			ORBit_RootObject_release_T (tc->subtypes [i]);
	}

	g_free (tc->subnames);
	g_free (tc->subtypes);
	g_free (tc->sublabels);

	if (tc->discriminator)
		ORBit_RootObject_release_T (tc->discriminator);

	p_free (tc, struct CORBA_TypeCode_struct);
}

static gboolean
CDR_get (GIOPRecvBuffer *buf,
	 guchar         *ptr,
	 guint           len)
{
	buf->cur = ALIGN_ADDRESS (buf->cur, len);

	if ((buf->cur + len) > buf->end)
		return TRUE;

	memcpy (ptr, buf->cur, len);

	if (len != 1 && giop_msg_conversion_needed (buf))
		switch (len) {
		case 2:
			*(guint16 *)ptr = GUINT16_SWAP_LE_BE (*(guint16 *)ptr);
			break;
		case 4:
			*(guint32 *)ptr = GUINT32_SWAP_LE_BE (*(guint32 *)ptr);
			break;
		case 8:
			*(guint64 *)ptr = GUINT64_SWAP_LE_BE (*(guint64 *)ptr);
			break;
		default:
			g_assert_not_reached ();
			break;
		}

	buf->cur += len;

	return FALSE;
}

#define CDR_get_ulong(x, y)       CDR_get(x, (guchar *)y, 4)
#define CDR_get_ushort(x, y)      CDR_get(x, (guchar *)y, 2)
#define CDR_get_short(x, y)       CDR_get(x, (guchar *)y, 2)
#define CDR_get_ulong_long(x, y)  CDR_get(x, (guchar *)y, 8)
#define CDR_get_octet(x, y)       CDR_get(x, (guchar *)y, 1)
#define CDR_get_wchar(x, y)       CDR_get(x, (guchar *)y, 2)

static gboolean
CDR_get_const_string(GIOPRecvBuffer *buf, char **ptr)
{
  CORBA_unsigned_long len;

  if(CDR_get_ulong(buf, &len))
    return TRUE;

  if((buf->cur + len) > buf->end
     || (buf->cur + len) < buf->cur)
    return TRUE;
  *ptr = g_memdup(buf->cur, len);
  buf->cur += len;

  return FALSE;
}

static CORBA_short
ORBit_TC_find_c_alignment (CORBA_TypeCode tc)
{
	CORBA_short retval = 1;
	int         i;

	while (tc->kind == CORBA_tk_alias)
		tc = tc->subtypes[0];

	switch(tc->kind) {
	case CORBA_tk_union:
		retval = MAX (retval,
			      ORBit_TC_find_c_alignment (tc->discriminator));
	case CORBA_tk_except:
	case CORBA_tk_struct:
#if ORBIT_ALIGNOF_CORBA_STRUCT > 1
		retval = MAX (retval, ORBIT_ALIGNOF_CORBA_STRUCT);
#endif
		for(i = 0; i < tc->sub_parts; i++)
			retval = MAX (retval, 
				      ORBit_TC_find_c_alignment (tc->subtypes[i]));
		return retval;
	case CORBA_tk_ulong:
	case CORBA_tk_long:
	case CORBA_tk_enum:
		return ORBIT_ALIGNOF_CORBA_LONG;
	case CORBA_tk_ushort:
	case CORBA_tk_short:
	case CORBA_tk_wchar:
		return ORBIT_ALIGNOF_CORBA_SHORT;
	case CORBA_tk_longlong:
	case CORBA_tk_ulonglong:
		return ORBIT_ALIGNOF_CORBA_LONG_LONG;
	case CORBA_tk_float:
		return ORBIT_ALIGNOF_CORBA_FLOAT;
	case CORBA_tk_double:
		return ORBIT_ALIGNOF_CORBA_DOUBLE;
	case CORBA_tk_longdouble:
		return ORBIT_ALIGNOF_CORBA_LONG_DOUBLE;
	case CORBA_tk_boolean:
	case CORBA_tk_char:
	case CORBA_tk_octet:
		return ORBIT_ALIGNOF_CORBA_CHAR;
	case CORBA_tk_string:
	case CORBA_tk_wstring:
	case CORBA_tk_TypeCode:
	case CORBA_tk_objref:
		return ORBIT_ALIGNOF_CORBA_POINTER;
	case CORBA_tk_sequence:
		return ORBIT_ALIGNOF_CORBA_SEQ;
	case CORBA_tk_any:
		return ORBIT_ALIGNOF_CORBA_ANY;
	case CORBA_tk_array:
		return ORBit_TC_find_c_alignment (tc->subtypes[0]);
	case CORBA_tk_fixed:
		return ORBIT_ALIGNOF_CORBA_FIXED;
	default:
		return 1;
	}
}

static gboolean
tc_dec_tk_objref (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	if (CDR_get_const_string (c, &t->repo_id))
		return TRUE;
	if (CDR_get_const_string (c, &t->name))
		return TRUE;
	return FALSE;
}

static gboolean
tc_dec_tk_sequence (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	t->subtypes = g_new0 (CORBA_TypeCode, 1);
	if (tc_dec (&t->subtypes[0], c, ctx))
		return TRUE;
	t->sub_parts = 1;
	if (CDR_get_ulong (c, &t->length))
		return TRUE;
	return FALSE;
}

static gboolean
tc_dec_tk_string (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	if (CDR_get_ulong (c, &t->length))
		return TRUE;
	return FALSE;
}
 
static gboolean
tc_dec_tk_struct (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	CORBA_unsigned_long i;

	if (CDR_get_const_string (c, &t->repo_id))
		return TRUE;
	if (CDR_get_const_string (c, &t->name))
		return TRUE;
	if (CDR_get_ulong (c, &t->sub_parts))
		return TRUE;

	t->subnames = g_new0 (char*, t->sub_parts);
	t->subtypes = g_new0 (CORBA_TypeCode, t->sub_parts);

	for (i = 0; i < t->sub_parts; i++) {
		if (CDR_get_const_string (c, &t->subnames [i]))
			return TRUE;
		if (tc_dec (&t->subtypes [i], c, ctx))
			return TRUE;
	}

	return FALSE;
}

static gboolean
tc_dec_tk_union (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	CORBA_unsigned_long i;

	if (CDR_get_const_string (c, &t->repo_id))
		return TRUE;
	if (CDR_get_const_string (c, &t->name))
		return TRUE;
	if (tc_dec (&t->discriminator, c, ctx))
		return TRUE;    
	if (CDR_get_ulong (c, &t->default_index))
		return TRUE;    
	if (CDR_get_ulong (c, &t->sub_parts))
		return TRUE;

	t->sublabels = g_new0 (CORBA_long, t->sub_parts);
	t->subnames  = g_new0 (char *, t->sub_parts);
	t->subtypes  = g_new0 (CORBA_TypeCode, t->sub_parts);

#define MEMBER_LOOPER_DEC(getname, typename, tkname)			\
	case CORBA_tk_##tkname:						\
		for(i = 0; i < t->sub_parts; i++){ 			\
			CORBA_##typename tmp;				\
			if (CDR_get_##getname (c, &tmp))		\
				return TRUE;				\
			t->sublabels [i] = (CORBA_long) tmp;		\
			if (CDR_get_const_string (c, &t->subnames[i]))	\
				return TRUE;				\
			if (tc_dec (&t->subtypes[i], c, ctx))		\
				return TRUE;				\
		}							\
		break

	switch (t->discriminator->kind) {
		UNION_MEMBERS (DEC);
	default:
		/* XXX: what is correct error handling? */
		g_error("Don't know how to handle this type (%d) of discriminator.",
			t->discriminator->kind);
	}

	return FALSE;
}

static gboolean
tc_dec_tk_enum (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	CORBA_unsigned_long i;

	if (CDR_get_const_string (c, &t->repo_id))
		return TRUE;
	if (CDR_get_const_string (c, &t->name))
		return TRUE;
	if (CDR_get_ulong (c, &t->sub_parts))
		return TRUE;

	t->subnames = g_new0 (char*, t->sub_parts);
	for(i = 0; i < t->sub_parts; i++) {
		if (CDR_get_const_string (c, &t->subnames[i]))
			return TRUE;
	}

	return FALSE;
}

static gboolean
tc_dec_tk_alias (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	CDR_get_const_string (c, &t->repo_id);
	CDR_get_const_string (c, &t->name);

	t->subtypes = g_new0 (CORBA_TypeCode, 1);
	if (tc_dec (t->subtypes, c, ctx))
		return TRUE;
	t->sub_parts = 1;

	return FALSE;
}


static gboolean
tc_dec_tk_except (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	gulong i;

	if (CDR_get_const_string (c, &t->repo_id))
		return TRUE;
	if (CDR_get_const_string (c, &t->name))
		return TRUE;
	if (CDR_get_ulong (c, &t->sub_parts))
		return TRUE;

	t->subtypes = g_new0 (CORBA_TypeCode, t->sub_parts);
	t->subnames = g_new0 (char *, t->sub_parts);

	for (i = 0; i < t->sub_parts; i++) {
		if (CDR_get_const_string (c, &t->subnames[i]))
			return TRUE;
		if (tc_dec (&t->subtypes[i], c, ctx))
			return TRUE;
	}

	return FALSE;
}

static gboolean
tc_dec_tk_array (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	t->subtypes = g_new (CORBA_TypeCode, 1);

	if (tc_dec (t->subtypes, c, ctx))
		return TRUE;

	t->sub_parts = 1;
	if (CDR_get_ulong (c, &t->length))
		return TRUE;

	return FALSE;
}


static gboolean
tc_dec_tk_wstring (CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	if (CDR_get_ulong (c, &t->length))
		return TRUE;
	return FALSE;
}

static gboolean
tc_dec_tk_fixed(CORBA_TypeCode t, GIOPRecvBuffer *c, TCDecodeContext* ctx)
{
	if (CDR_get_ushort (c, &t->digits))
		return TRUE;
	if (CDR_get_short (c, &t->scale))
		return TRUE;

	return FALSE;
}

/* FIXME: Right now this function doesn't record whether or not it has
   already visited a given TypeCode.  I'm not sure if every recursive
   type will have a tk_recursive node in it; if not, then this will
   need to be reworked a bit.  */
static CORBA_boolean
typecode_equiv_internal (CORBA_TypeCode obj,
			 CORBA_TypeCode tc,
			 gboolean       strict_equal,
			 CORBA_Environment *ev)
{
	int i;

	g_return_val_if_fail (tc != NULL, CORBA_FALSE);
	g_return_val_if_fail (obj != NULL, CORBA_FALSE);

	if (!strict_equal) {
		while (obj->kind == CORBA_tk_alias)
			obj = obj->subtypes [0];

		while (tc->kind  == CORBA_tk_alias)
			tc  = tc->subtypes [0];
	}

	if (obj->kind != tc->kind)
		return CORBA_FALSE;

	switch (obj->kind) {
	case CORBA_tk_wstring:
	case CORBA_tk_string:
		return obj->length == tc->length;
	case CORBA_tk_objref:
		return ! strcmp (obj->repo_id, tc->repo_id);
	case CORBA_tk_except:
	case CORBA_tk_struct:
		if (strcmp (obj->repo_id, tc->repo_id)
		    || obj->sub_parts != tc->sub_parts)
			return CORBA_FALSE;
		for (i = 0; i < obj->sub_parts; ++i)
			if (! typecode_equiv_internal (obj->subtypes[i],
						       tc->subtypes[i],
						       strict_equal, ev))
				return CORBA_FALSE;
		break;
	case CORBA_tk_union:
		if (strcmp (obj->repo_id, tc->repo_id)
		    || obj->sub_parts != tc->sub_parts
		    || ! typecode_equiv_internal (obj->discriminator,
						   tc->discriminator,
						   strict_equal, ev)
		    || obj->default_index != tc->default_index)
			return CORBA_FALSE;
		for (i = 0; i < obj->sub_parts; ++i)

			if (! typecode_equiv_internal (obj->subtypes[i],
						       tc->subtypes[i],
						       strict_equal, ev)
			    || obj->sublabels [i] != tc->sublabels [i])
				return CORBA_FALSE;

		break;
	case CORBA_tk_enum:
		if (obj->sub_parts != tc->sub_parts
		    || strcmp (obj->repo_id, tc->repo_id))
			return CORBA_FALSE;
		for (i = 0; i < obj->sub_parts; ++i)
			if (strcmp (obj->subnames[i], tc->subnames[i]))
				return CORBA_FALSE;
		break;
	case CORBA_tk_sequence:
	case CORBA_tk_array:
		if (obj->length != tc->length)
			return CORBA_FALSE;
		g_assert (obj->sub_parts == 1);
		g_assert (tc->sub_parts == 1);
		return typecode_equiv_internal (obj->subtypes[0],
						tc->subtypes[0],
						strict_equal, ev);
	case CORBA_tk_alias:
		if (strcmp (obj->repo_id, tc->repo_id))
			return CORBA_FALSE;
		
		g_assert (obj->sub_parts == 1);
		g_assert (tc->sub_parts == 1);

		return typecode_equiv_internal (obj->subtypes[0],
						tc->subtypes[0],
						strict_equal, ev);
		break;
	case CORBA_tk_recursive:
		return obj->recurse_depth == tc->recurse_depth;
	case CORBA_tk_fixed:
		return obj->digits == tc->digits && obj->scale == tc->scale;

	default:
		/* Everything else is primitive.  */
		break;
	}

	return CORBA_TRUE;
}

CORBA_boolean
CORBA_TypeCode_equal (CORBA_TypeCode obj,
		      const CORBA_TypeCode tc,
		      CORBA_Environment *ev)
{
	return typecode_equiv_internal (obj, tc, TRUE, ev);
}

CORBA_boolean
CORBA_TypeCode_equivalent (CORBA_TypeCode obj,
			   const CORBA_TypeCode tc,
			   CORBA_Environment *ev)
{
	return typecode_equiv_internal (obj, tc, FALSE, ev);
}

CORBA_TypeCode
CORBA_TypeCode_get_compact_typecode (CORBA_TypeCode     typecode,
				     CORBA_Environment *ev)
{
	/* FIXME: implement */
	CORBA_exception_set_system (
		ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);

	return CORBA_OBJECT_NIL;
}

CORBA_TCKind
CORBA_TypeCode_kind (CORBA_TypeCode     typecode,
		     CORBA_Environment *ev)
{
	return typecode->kind;
}

CORBA_RepositoryId
CORBA_TypeCode_id (CORBA_TypeCode     typecode,
		   CORBA_Environment *ev)
{
	if (!(typecode->kind == CORBA_tk_objref             ||
	      typecode->kind == CORBA_tk_value              ||
	      typecode->kind == CORBA_tk_value_box          ||
	      typecode->kind == CORBA_tk_abstract_interface ||
	      typecode->kind == CORBA_tk_native             ||
	      typecode->kind == CORBA_tk_struct             ||
	      typecode->kind == CORBA_tk_union              ||
	      typecode->kind == CORBA_tk_enum               ||
	      typecode->kind == CORBA_tk_alias              ||
	      typecode->kind == CORBA_tk_except)) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return NULL;
	}

	return CORBA_string_dup (typecode->repo_id);
}

CORBA_Identifier
CORBA_TypeCode_name (CORBA_TypeCode     typecode,
		     CORBA_Environment *ev)
{
	if (!(typecode->kind == CORBA_tk_objref             ||
	      typecode->kind == CORBA_tk_struct             ||
	      typecode->kind == CORBA_tk_union              ||
	      typecode->kind == CORBA_tk_enum               ||
	      typecode->kind == CORBA_tk_alias              ||
	      typecode->kind == CORBA_tk_abstract_interface ||
	      typecode->kind == CORBA_tk_value              ||
	      typecode->kind == CORBA_tk_value_box          ||
	      typecode->kind == CORBA_tk_native             ||
	      typecode->kind == CORBA_tk_except)) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return NULL;
	}

	return CORBA_string_dup (typecode->name);
}

CORBA_unsigned_long
CORBA_TypeCode_member_count (CORBA_TypeCode     typecode,
			     CORBA_Environment *ev)
{
	if (!(typecode->kind == CORBA_tk_struct ||
	      typecode->kind == CORBA_tk_union  ||
	      typecode->kind == CORBA_tk_value  ||
	      typecode->kind == CORBA_tk_enum   ||
	      typecode->kind == CORBA_tk_except)) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return 0;
	}

	return typecode->sub_parts;
}

CORBA_Identifier
CORBA_TypeCode_member_name (CORBA_TypeCode       typecode,
			    const CORBA_unsigned_long index,
			    CORBA_Environment   *ev)
{
	if (!(typecode->kind == CORBA_tk_struct ||
	      typecode->kind == CORBA_tk_union  ||
	      typecode->kind == CORBA_tk_value  ||
	      typecode->kind == CORBA_tk_enum   ||
	      typecode->kind == CORBA_tk_except)) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return NULL;
	}

	if (index > typecode->sub_parts) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/Bounds/1.0", NULL);
		return NULL;
	}

	return CORBA_string_dup (typecode->subnames [index]);
}

CORBA_TypeCode
CORBA_TypeCode_member_type (CORBA_TypeCode       typecode,
			    const CORBA_unsigned_long index,
			    CORBA_Environment   *ev)
{
	if (!(typecode->kind == CORBA_tk_struct ||
	      typecode->kind == CORBA_tk_union  ||
	      typecode->kind == CORBA_tk_value  ||
	      typecode->kind == CORBA_tk_enum   ||
	      typecode->kind == CORBA_tk_except)) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return NULL;
	}

	if (index > typecode->sub_parts) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/Bounds/1.0", NULL);
		return NULL;
	}

	return ORBit_RootObject_duplicate (typecode->subtypes [index]);
}

CORBA_any *
CORBA_TypeCode_member_label (CORBA_TypeCode       typecode,
			     const CORBA_unsigned_long index,
			     CORBA_Environment   *ev)
{
	CORBA_any *retval;

	if (typecode->kind != CORBA_tk_union) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return NULL;
	}

	if (index > typecode->sub_parts) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/Bounds/1.0", NULL);
		return NULL;
	}

	retval = CORBA_any__alloc ();

	retval->_type  = ORBit_RootObject_duplicate (typecode->discriminator);
	retval->_value = ORBit_copy_value (&typecode->sublabels [index],
					   typecode->discriminator);
	retval->_release = CORBA_TRUE;

	return retval;
}

CORBA_TypeCode
CORBA_TypeCode_discriminator_type (CORBA_TypeCode     typecode,
				   CORBA_Environment *ev)
{
	if (typecode->kind != CORBA_tk_union) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return NULL;
	}

	return ORBit_RootObject_duplicate (typecode->discriminator);
}

CORBA_long
CORBA_TypeCode_default_index (CORBA_TypeCode     typecode,
			      CORBA_Environment *ev)
{
	if (typecode->kind != CORBA_tk_union) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return 0;
	}

	return typecode->default_index;
}

CORBA_unsigned_long
CORBA_TypeCode_length (CORBA_TypeCode     typecode,
		       CORBA_Environment *ev)
{
	if (!(typecode->kind == CORBA_tk_string   ||
	      typecode->kind == CORBA_tk_wstring  ||
	      typecode->kind == CORBA_tk_sequence ||
	      typecode->kind == CORBA_tk_array)) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return 0;
	}

	return typecode->length;
}

CORBA_TypeCode
CORBA_TypeCode_content_type (CORBA_TypeCode     typecode,
			     CORBA_Environment *ev)
{
	if (!(typecode->kind == CORBA_tk_sequence  ||
	      typecode->kind == CORBA_tk_array     ||
	      typecode->kind == CORBA_tk_value_box ||
	      typecode->kind == CORBA_tk_alias)) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return NULL;
	}

	g_assert (typecode->sub_parts == 1);

	return ORBit_RootObject_duplicate (typecode->subtypes [0]);
}

CORBA_unsigned_short
CORBA_TypeCode_fixed_digits (CORBA_TypeCode     typecode,
			     CORBA_Environment *ev)
{
	if (typecode->kind != CORBA_tk_fixed) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return 0;
	}

	return typecode->digits;
}

CORBA_short
CORBA_TypeCode_fixed_scale (CORBA_TypeCode     typecode,
			    CORBA_Environment *ev)
{
	if (typecode->kind != CORBA_tk_fixed) {
		CORBA_exception_set (
			ev, CORBA_USER_EXCEPTION,
			"IDL:omg.org/CORBA/TypeCode/BadKind/1.0", NULL);
		return 0;
	}

	return typecode->scale;
}

CORBA_Visibility
CORBA_TypeCode_member_visibility (CORBA_TypeCode             typecode,
				  const CORBA_unsigned_long  index,
				  CORBA_Environment         *ev)
{
	/* FIXME: implement */
	CORBA_exception_set_system (
		ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);

	return 0;
}

CORBA_ValueModifier
CORBA_TypeCode_type_modifier (CORBA_TypeCode     typecode,
			      CORBA_Environment *ev)
{
	/* FIXME: implement */
	CORBA_exception_set_system (
		ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);

	return 0;
}

CORBA_TypeCode
CORBA_TypeCode_concrete_base_type (CORBA_TypeCode     typecode,
				   CORBA_Environment *ev)
{
	/* FIXME: implement */
	CORBA_exception_set_system (
		ev, ex_CORBA_NO_IMPLEMENT, CORBA_COMPLETED_NO);

	return CORBA_OBJECT_NIL;
}

const char *
ORBit_tk_to_name (CORBA_unsigned_long tk)
{
#define CSE(a, b) \
	case CORBA_tk_ ## a: return b;

	switch (tk) {
		CSE (null, "null");
		CSE (void, "void");
		CSE (short, "short");
		CSE (long, "long");
		CSE (ushort, "ushort");
		CSE (ulong, "ulong");
		CSE (float, "float");
		CSE (double, "double");
		CSE (boolean, "boolean");
		CSE (char, "char");
		CSE (octet, "octet");
		CSE (any, "any");
		CSE (TypeCode, "TypeCode");
		CSE (Principal, "Principal");
		CSE (objref, "objref");
		CSE (struct, "struct");
		CSE (union, "union");
		CSE (enum, "enum");
		CSE (string, "string");
		CSE (sequence, "sequence");
		CSE (array, "array");
		CSE (alias, "alias");
		CSE (except, "except");
		CSE (longlong, "longlong");
		CSE (ulonglong, "ulonglong");
		CSE (longdouble, "longdouble");
		CSE (wchar, "wchar");
		CSE (wstring, "wstring");
		CSE (fixed, "fixed");
		CSE (value, "value");
		CSE (value_box, "value_box");
		CSE (native, "native");
		CSE (abstract_interface, "abstract_interface");
	default:
		return "Invalid type";
	}
}

static const TkInfo tk_info[CORBA_tk_last]= {
	{ TK_EMPTY, NULL, NULL, TC_null }, /* tk_null */
	{ TK_EMPTY, NULL, NULL, TC_void }, /* tk_void */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_short }, /* tk_short */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_long }, /* tk_long */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_unsigned_short }, /* tk_ushort */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_unsigned_long }, /* tk_ulong */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_float }, /* tk_float */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_double }, /* tk_double */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_boolean }, /* tk_boolean */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_char }, /* tk_char */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_octet }, /* tk_octet */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_any }, /* tk_any */
	{ TK_EMPTY, NULL, NULL, TC_CORBA_TypeCode }, /* tk_TypeCode */
        { TK_EMPTY, NULL, NULL, TC_CORBA_Principal }, /* tk_Principal */
	{ TK_COMPLEX, tc_enc_tk_objref, tc_dec_tk_objref }, /* tk_objref */
	{ TK_COMPLEX, tc_enc_tk_struct, tc_dec_tk_struct }, /* tk_struct */
        { TK_COMPLEX, tc_enc_tk_union, tc_dec_tk_union }, /* tk_union */
        { TK_COMPLEX, tc_enc_tk_enum, tc_dec_tk_enum }, /* tk_enum */
        { TK_SIMPLE, tc_enc_tk_string, tc_dec_tk_string }, /* tk_string */
        { TK_COMPLEX, tc_enc_tk_sequence, tc_dec_tk_sequence }, /* tk_sequence */
        { TK_COMPLEX, tc_enc_tk_array, tc_dec_tk_array }, /* tk_array */
        { TK_COMPLEX, tc_enc_tk_alias, tc_dec_tk_alias }, /* tk_alias */
        { TK_COMPLEX, tc_enc_tk_except, tc_dec_tk_except }, /* tk_except */
        { TK_EMPTY, NULL, NULL, TC_CORBA_long_long}, /* tk_longlong */
        { TK_EMPTY, NULL, NULL, TC_CORBA_unsigned_long_long }, /* tk_ulonglong */
        { TK_EMPTY, NULL, NULL, TC_CORBA_long_double }, /* tk_longdouble */
        { TK_EMPTY, NULL, NULL, TC_CORBA_wchar }, /* tk_wchar */
	{ TK_SIMPLE, tc_enc_tk_wstring, tc_dec_tk_wstring}, /* tk_wstring */
	{ TK_SIMPLE, tc_enc_tk_fixed, tc_dec_tk_fixed} /* tk_fixed */
};

ORBIT2_MAYBE_CONST ORBit_RootObject_Interface ORBit_TypeCode_epv = {
	ORBIT_ROT_TYPECODE,
	ORBit_TypeCode_free_fn
};

static gboolean
tc_dec (CORBA_TypeCode  *t,
	GIOPRecvBuffer  *c,
	TCDecodeContext *ctx)
{
	guint               tmp_index;
	const TkInfo       *info;
	CORBA_TCKind        kind;
	GIOPRecvBuffer     *encaps;
	TCRecursionNode    *node;
	CORBA_unsigned_long lkind;

	if (CDR_get_ulong (c, &lkind))
		return TRUE;

	kind = lkind;

	if (lkind >= CORBA_tk_last) {
		CORBA_long offset;
		GSList    *l;

		if (lkind != CORBA_tk_recursive) {
			g_warning ("%s: invalid CORBA_TCKind, lkind=%lu",
				   G_STRFUNC, (unsigned long int) lkind);
			return TRUE;
		}

		if (CDR_get_ulong (c, &offset))
			return TRUE;

		for (l = ctx->prior_tcs; l; l = l->next) {
			node = l->data;
			if (offset == node->index - ctx->current_idx - get_rptr (c)) {
				*t = ORBit_RootObject_duplicate (node->tc);
				return FALSE;
			}
/*			else
				g_warning ("back tc mismactch '%d' == '%d - %d - %d' = '%d' Tk %d",
					   offset, node->index, ctx->current_idx, get_rptr (c),
					   node->index - ctx->current_idx - get_rptr (c),
					   node->tc->kind);*/
		}
		
		/* FIXME: we should handle this slightly gracefully */
		g_error ("tc_dec: Invalid CORBA_TypeCode recursion "
			 "offset in input buffer\n");
		g_assert_not_reached ();
	}

	g_assert (kind < CORBA_tk_last);

	node = g_new (TCRecursionNode, 1);
	node->index = ctx->current_idx + get_rptr(c) - 4; /* -4 for the TCKind */
	info = &tk_info [kind];

	if (info->type == TK_EMPTY)
		node->tc = info->basic_type;

	else {
		CORBA_TypeCode      tc;

		tc = g_new0 (struct CORBA_TypeCode_struct, 1);

		ORBit_RootObject_init (&tc->parent, &ORBit_TypeCode_epv);
		ORBit_RootObject_duplicate (tc);

		tc->kind = kind;

		switch (info->type) {

		case TK_EMPTY:
			g_assert_not_reached ();
			break;

		case TK_COMPLEX:
			tmp_index = ctx->current_idx;
			ctx->current_idx += get_rptr (c) + 4;
			/* NB. the encaps buffer is for data validation */
			encaps = giop_recv_buffer_use_encaps_buf (c);
			info->decoder (tc, encaps, ctx);
			ctx->current_idx = tmp_index;
			giop_recv_buffer_unuse (encaps);
			break;
			
		case TK_SIMPLE:
			info->decoder (tc, c, ctx);
			break;
		}

		tc->c_align = ORBit_TC_find_c_alignment (tc);
		node->tc = tc;
	}

	*t = node->tc;
	ctx->prior_tcs = g_slist_prepend (ctx->prior_tcs, node);

	return FALSE;
}

gboolean
ORBit_decode_CORBA_TypeCode (CORBA_TypeCode *tc,
			     GIOPRecvBuffer *buf)
{
	GSList         *l;
	gboolean        retval;
	TCDecodeContext ctx;

	ctx.current_idx = 0;
	ctx.prior_tcs = NULL;

	retval = tc_dec (tc, buf, &ctx);

	for (l = ctx.prior_tcs; l; l = l->next)
		g_free (l->data);

	g_slist_free (ctx.prior_tcs);

	return retval;
}

/* Encode a typecode to a codec, possibly recursively */
static void
tc_enc (CORBA_TypeCode tc, GIOPSendBuffer *buf, TCEncodeContext *ctx)
{
	TCRecursionNode     *node;
	const TkInfo        *info;
	GSList              *l;
	CORBA_unsigned_long  len, tmp;
	guchar              *marker;
	gint8                endianness;

	g_assert (CLAMP (0, tc->kind, CORBA_tk_last) == tc->kind);

	giop_send_buffer_align (buf, 4);

	info = &tk_info [tc->kind];

	/* For base types it is better to marshal just the tc->kind */
	if (info->type != TK_EMPTY) {

		for (l = ctx->prior_tcs; l; l = l->next) {
			node = l->data;

			if (node->tc == tc) {
				tmp = CORBA_tk_recursive;
				giop_send_buffer_append_aligned (buf, &tmp, 4);
				len = node->index - buf->msg.header.message_size - 4;
				giop_send_buffer_append_aligned (buf, &len, 4);
				return;
			}
		}

		/* only keep track of larger, more complex typecodes */
		node = g_new (TCRecursionNode, 1);
		node->tc = tc;
		node->index = buf->msg.header.message_size;
		ctx->prior_tcs = g_slist_prepend (ctx->prior_tcs, node);
	}

	giop_send_buffer_append (buf, &tc->kind, sizeof (tc->kind));

	switch (info->type) {
	case TK_EMPTY:
		break;
	case TK_COMPLEX:
		marker = giop_send_buffer_append_aligned (buf, NULL, 4);

		tmp = buf->msg.header.message_size;
		endianness = GIOP_FLAG_ENDIANNESS;

		giop_send_buffer_append (buf, &endianness, 1);

		info->encoder (tc, buf, ctx);

		len = buf->msg.header.message_size - tmp;

		memcpy (marker, &len, 4);
		break;
	case TK_SIMPLE:
		info->encoder (tc, buf, ctx);
		break;
	}
}

void
ORBit_encode_CORBA_TypeCode (CORBA_TypeCode  tc,
			     GIOPSendBuffer *buf)
{
	GSList         *l;
	TCEncodeContext ctx;

	ctx.start_idx = buf->msg.header.message_size;
	ctx.prior_tcs = NULL;

	tc_enc (tc, buf, &ctx);

	for (l = ctx.prior_tcs; l; l = l->next)
		g_free (l->data);

	g_slist_free (ctx.prior_tcs);
}
