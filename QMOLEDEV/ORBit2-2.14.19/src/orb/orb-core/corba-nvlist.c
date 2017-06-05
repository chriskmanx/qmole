#include "config.h"
#include <orbit/orbit.h>

void
CORBA_NVList_add_item (CORBA_NVList             list,
		       const CORBA_char        *item_name,
		       const CORBA_TypeCode     item_type,
		       const CORBA_OpaqueValue  value,
		       const CORBA_long         value_len,
		       const CORBA_Flags        item_flags,
		       CORBA_Environment       *ev)
{
	CORBA_NamedValue newval;

	g_assert (list != NULL);

	newval.name = CORBA_string_dup (item_name);

	newval.argument._type = ORBit_RootObject_duplicate (
		item_type);

	if (item_flags & CORBA_IN_COPY_VALUE) {
		newval.argument._value = ORBit_copy_value (
			value, item_type);
		newval.argument._release = CORBA_TRUE;
	} else {
		newval.argument._value = value;
		newval.argument._release = CORBA_FALSE;
	}

	newval.len = value_len; /* Is this even useful? *sigh* */
	newval.arg_modes = item_flags;

	g_array_append_val (list->list, newval);
}

static void
ORBit_NamedValue_free (CORBA_NamedValue *nv)
{
	ORBit_free (nv->name);
	nv->name = NULL;
}

void
CORBA_NVList_free (CORBA_NVList       list,
		   CORBA_Environment *ev)
{
	int i;

	CORBA_NVList_free_memory (list, ev);

	if (list->list) {
		for (i = 0; i < list->list->len; i++) {
			CORBA_NamedValue *nv;

			nv = &g_array_index (
				list->list, CORBA_NamedValue, i);
			ORBit_NamedValue_free (nv);
		}

		g_array_free (list->list, TRUE);
		list->list = NULL;
	}

	g_free (list);
}

void
CORBA_NVList_free_memory (CORBA_NVList       list,
			  CORBA_Environment *ev)
{
	int i;

	if (list->list) {
		for (i = 0; i < list->list->len; i++) {
			CORBA_NamedValue *nv;

			nv = &g_array_index (
				list->list, CORBA_NamedValue, i);

			if (nv->argument._release)
				ORBit_free (nv->argument._value);
			nv->argument._value = NULL;
			ORBit_RootObject_release (nv->argument._type);
			nv->argument._type = NULL;
		}
	}
}

void
CORBA_NVList_get_count (CORBA_NVList      list,
			CORBA_long       *count,
		       CORBA_Environment *ev)
{
	*count = list->list->len;
}
