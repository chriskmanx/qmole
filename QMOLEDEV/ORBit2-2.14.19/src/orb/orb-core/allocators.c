#include "config.h"
#include <string.h>
#include <orbit/orbit.h>
#include "orb-core-private.h"

/***
    The argument {mem} is a chuck of memory described by {tc}, and its
    contents is freed, but {mem} itself is not freed. That is, if {mem}
    contains anything interesting (objrefs, pointers), they are freed.
    A pointer to the end of {mem} is returned. This should always be
    the same as {mem + ORBit_gather_alloc_info(tc)}. Also, any pointers
    within {mem} are zeroed; thus it should be safe to call this
    function multiple times on the same chunk of memory.

    This function is a modified version of ORBit_free_via_TypeCode().
    Aside from the arguments, the bigest difference is that this
    does not free the {tc} which is passed in. The old style led
    to a lot of pointless dups, and also failed miserably when
    arrays of things were allocated.
**/
static gpointer
ORBit_freekids_via_TypeCode_T (gpointer       mem,
			       CORBA_TypeCode tc)
{
	int i;
	guchar *retval = NULL;
	CORBA_TypeCode subtc;

/*  g_warning ("Freeing via tc '%s' at %p",
    ORBit_tk_to_name (tc->kind), mem);*/

	switch (tc->kind) {
	case CORBA_tk_any: {
		CORBA_any *pval = mem;
		if (pval->_release)
			ORBit_free_T (pval->_value);
		pval->_value = NULL;
		ORBit_RootObject_release_T (pval->_type);
		pval->_type = NULL;
		retval = (guchar *) (pval + 1);
		break;
	}
	case CORBA_tk_TypeCode:
	case CORBA_tk_objref: {
		CORBA_Object *pval = mem;

		ORBit_RootObject_release_T (*pval);
		*pval = NULL;
		retval = ((guchar *)mem) + sizeof (*pval);
		break;
	}
	case CORBA_tk_Principal: {
		CORBA_Principal *pval = mem;
		if (pval->_release)
			ORBit_free_T (pval->_buffer);
		pval->_buffer = NULL;
		retval = (guchar *)(pval + 1);
		break;
	}
	case CORBA_tk_except:
	case CORBA_tk_struct:
		mem = ALIGN_ADDRESS (mem, tc->c_align);
		for (i = 0; i < tc->sub_parts; i++) {
			subtc = tc->subtypes [i];
			mem = ALIGN_ADDRESS (mem, subtc->c_align);
			mem = ORBit_freekids_via_TypeCode_T (mem, subtc);
		}
		mem = ALIGN_ADDRESS (mem, tc->c_align);
		retval = mem;
		break;
	case CORBA_tk_union: {
		int sz = 0;
		int al = 1;
		gconstpointer cmem;

		cmem = ALIGN_ADDRESS (mem, MAX (tc->discriminator->c_align,
						tc->c_align));
		subtc = ORBit_get_union_tag (tc, &cmem, TRUE);
		for (i = 0; i < tc->sub_parts; i++) {
			al = MAX (al, tc->subtypes [i]->c_align);
			sz = MAX (sz, ORBit_gather_alloc_info (tc->subtypes [i]));
		}
		mem = ALIGN_ADDRESS (cmem, al);
		ORBit_freekids_via_TypeCode_T (mem, subtc);
		/* the end of the body (subtc) may not be the
		 * same as the end of the union */
		retval = ((guchar *)mem) + sz;
		break;
	}
	case CORBA_tk_wstring:
	case CORBA_tk_string: {
		CORBA_char **pval = mem;
		ORBit_free_T (*pval);
		*pval = NULL;
		retval = (guchar *)mem + sizeof (*pval);
		break;
	}
	case CORBA_tk_sequence: {
		CORBA_sequence_CORBA_octet *pval = mem;
		if (pval->_release)
			ORBit_free_T (pval->_buffer);
		pval->_buffer = NULL;
		retval = (guchar *)mem + sizeof(*pval);
		break;
	}
	case CORBA_tk_array:
		for (i = 0; i < tc->length; i++)
			mem = ORBit_freekids_via_TypeCode_T(
				mem, tc->subtypes[0]);
		retval = mem;
		break;
	case CORBA_tk_alias:
		retval = ORBit_freekids_via_TypeCode_T (
			mem, tc->subtypes[0]);
		break;
	default: {
		gulong length;
		length = ORBit_gather_alloc_info (tc);
		retval = (guchar *) ALIGN_ADDRESS (mem, tc->c_align) + length;
		break;
	}
	}
	return retval;
}

gpointer
ORBit_freekids_via_TypeCode (CORBA_TypeCode tc, gpointer mem)
{
	gpointer ret;

	LINK_MUTEX_LOCK   (ORBit_RootObject_lifecycle_lock);

	ret = ORBit_freekids_via_TypeCode_T (mem, tc);

	LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);

	return ret;
}

void
CORBA_free (gpointer mem)
{
	ORBit_free (mem);
}

#define SHORT_PREFIX_LEN (MAX (sizeof (ORBitMemHow), \
			       ORBIT_ALIGNOF_CORBA_LONG_DOUBLE))
#define LONG_PREFIX_LEN  (sizeof (CORBA_long_double) > sizeof (ORBit_MemPrefix) ? \
			  ORBIT_ALIGNOF_CORBA_LONG_DOUBLE : \
			  MAX (sizeof (ORBit_MemPrefix), \
			       sizeof (CORBA_long_double) + \
			       ORBIT_ALIGNOF_CORBA_LONG_DOUBLE))

void
ORBit_free_T (gpointer mem)
{
	int               i;
	guchar           *x;
	CORBA_TypeCode    tc;
	ORBitMemHow       how;
	ORBit_MemPrefix  *prefix;
	ORBit_Mem_free_fn free_fn;

	if (!mem)
		return;

	if ((gulong)mem & 0x1) {
		g_free ((guchar *)mem - 1);
		return;
	}

	how = *(((ORBitMemHow *) mem) - 1);

	switch (ORBIT_MEMHOW_HOW (how)) {
	case ORBIT_MEMHOW_TYPECODE:
		prefix = (ORBit_MemPrefix *) 
			((guchar *) mem - LONG_PREFIX_LEN);

		free_fn = (ORBit_Mem_free_fn)
			ORBit_freekids_via_TypeCode_T;
		tc      = prefix->u.tc;
		break;

	case ORBIT_MEMHOW_FREEFNC:
		prefix = (ORBit_MemPrefix *) 
			((guchar *) mem - LONG_PREFIX_LEN);

		free_fn = prefix->u.free_fn;
		tc      = NULL;
		break;
	case ORBIT_MEMHOW_SIMPLE:
		g_free ((guchar *)mem - SHORT_PREFIX_LEN);
		return;
	case ORBIT_MEMHOW_NONE:
	default:
		return;
	}

	how = ORBIT_MEMHOW_ELEMENTS (how);

	for (i = 0, x = mem; i < how; i++)
		x = free_fn (x, tc);
	
	g_free (prefix);

	if (tc)
		ORBit_RootObject_release_T (tc);
}

void
ORBit_free (gpointer mem)
{
	if (!mem)
		return;

	LINK_MUTEX_LOCK   (ORBit_RootObject_lifecycle_lock);

	ORBit_free_T (mem);

	LINK_MUTEX_UNLOCK (ORBit_RootObject_lifecycle_lock);
}

CORBA_char *
ORBit_alloc_string (size_t string_length)
{
	guchar *mem;

	mem = g_malloc (string_length + 1);

	return (CORBA_char *)(mem + 1);
}

gpointer
ORBit_alloc_simple (size_t block_size)
{
	guchar *mem, *prefix;

	if (!block_size)
		return NULL;

	prefix = g_malloc (SHORT_PREFIX_LEN + block_size);

	mem = (guchar *) prefix + SHORT_PREFIX_LEN;

	*((ORBitMemHow *)mem - 1) = ORBIT_MEMHOW_SIMPLE;

	return mem;
}

gpointer
ORBit_alloc_with_free_fn (size_t            element_size,
			  guint             num_elements,
			  ORBit_Mem_free_fn free_fn)
{
	guchar *mem;
	ORBit_MemPrefix *prefix;

	if (!num_elements)
		return NULL;

	prefix = g_malloc (LONG_PREFIX_LEN +
			   element_size * num_elements);
	prefix->u.free_fn = free_fn;

	mem = (guchar *) prefix + LONG_PREFIX_LEN;

	*((ORBitMemHow *)mem - 1) = ORBIT_MEMHOW_MAKE (
		ORBIT_MEMHOW_FREEFNC, num_elements);

	return mem;
}

gpointer
ORBit_alloc_tcval (CORBA_TypeCode tc,
		   guint          num_elements)
{
	guchar *mem;
	guint   element_size;
	ORBit_MemPrefix *prefix;

	if (!num_elements)
		return NULL;

	if (!(element_size = ORBit_gather_alloc_info (tc)))
		return NULL;

	prefix = g_malloc0 (LONG_PREFIX_LEN +
			    element_size * num_elements);

	prefix->u.tc = ORBit_RootObject_duplicate (tc);

	mem = (guchar *)prefix + LONG_PREFIX_LEN;

	*((ORBitMemHow *)mem - 1) = ORBIT_MEMHOW_MAKE (
		ORBIT_MEMHOW_TYPECODE, num_elements);

	return mem;
}

gpointer
ORBit_realloc_tcval (gpointer       old,
		     CORBA_TypeCode tc,
		     guint          old_num_elements,
		     guint          num_elements)
{
	guint element_size;
	ORBit_MemPrefix *prefix;
	guchar *mem;

	g_assert (num_elements > old_num_elements);

	if (!num_elements)
		return NULL;

	if (!old_num_elements && !old)
		return ORBit_alloc_tcval (tc, num_elements);

	if (!(element_size = ORBit_gather_alloc_info (tc)))
		return NULL;

	prefix = g_realloc ((guchar *) old - LONG_PREFIX_LEN,
			    LONG_PREFIX_LEN +
			    element_size * num_elements);

	/* Initialize as yet unused memory to 'safe' values */
	memset ((guchar *) prefix + LONG_PREFIX_LEN +
		old_num_elements * element_size,
		0, (num_elements - old_num_elements) * element_size);

	mem = (guchar *)prefix + LONG_PREFIX_LEN;

	*((ORBitMemHow *)mem - 1) = ORBIT_MEMHOW_MAKE (
		ORBIT_MEMHOW_TYPECODE, num_elements);

	return mem;
}

CORBA_TypeCode
ORBit_alloc_get_tcval (gpointer mem)
{
	ORBitMemHow how;
	ORBit_MemPrefix *prefix;

	if (!mem)
		return NULL;

	if ((gulong)mem & 0x1)
		return TC_CORBA_string;

	how = *(((ORBitMemHow *) mem) - 1);

	if (ORBIT_MEMHOW_HOW (how) == ORBIT_MEMHOW_TYPECODE) {
		prefix = (ORBit_MemPrefix *) 
		  ((guchar *) mem - LONG_PREFIX_LEN);

		return ORBit_RootObject_duplicate (prefix->u.tc);
	} else
		g_error ("Can't determine type of %p (%u)", mem, how);

	return NULL;
}

gpointer
ORBit_alloc_by_tc (CORBA_TypeCode tc)
{
	guchar *mem;
	guint   size;
	ORBit_MemPrefix *prefix;

	if (!(size = ORBit_gather_alloc_info (tc)))
		return NULL;

	prefix = g_malloc0 (LONG_PREFIX_LEN + size);

	prefix->u.tc = ORBit_RootObject_duplicate (tc);

	mem = (guchar *)prefix + LONG_PREFIX_LEN;

	*((ORBitMemHow *)mem - 1) = ORBIT_MEMHOW_MAKE (
		ORBIT_MEMHOW_TYPECODE, 1);

	return mem;
}
