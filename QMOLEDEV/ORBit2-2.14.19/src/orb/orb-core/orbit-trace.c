#ifdef G_ENABLE_DEBUG

#include "config.h"
#include <unistd.h>
#include <string.h>
#include <sys/time.h>
#include <ctype.h>
#include <orbit/orbit.h>
#include <stdlib.h>

#include "orb-core-private.h"
#include "orbit-debug.h"

/*
 * Flip this switch to debug the
 * debug code.
 */
#undef DEBUG_TRACE

#define NOT_NULL(str) ((str) == NULL ? "(null)" : (str))

void
ORBit_trace_objref (const CORBA_Object obj)
{
	if (!obj)
		tprintf ("[nil]");
	else
		tprintf ("[%p]", obj);
}

void
ORBit_trace_typecode (const CORBA_TypeCode tc)
{
	g_return_if_fail (tc != NULL);
	tprintf ("tc:%s", NOT_NULL (TC_CORBA_TCKind->subnames [tc->kind]));
}

void
ORBit_trace_any (const CORBA_any *any)
{
	gconstpointer p;

	g_return_if_fail (any != NULL);

	tprintf ("{ ");
	ORBit_trace_typecode (any->_type);
	tprintf (", ");
	p = any->_value;
	ORBit_trace_value (&p, any->_type);
	tprintf (" }");
}

void
ORBit_trace_value (gconstpointer *val, CORBA_TypeCode tc)
{
	CORBA_unsigned_long      i;
	gconstpointer            subval;

#ifdef DEBUG_TRACE
	fprintf (stderr, "\ntrace '%s' %p align %d, (size %d)\n",
		 NOT_NULL (TC_CORBA_TCKind->subnames [tc->kind]), *val,
		 tc->c_align,
		 ORBit_gather_alloc_info (tc));
#endif

	*val = ALIGN_ADDRESS (*val, tc->c_align);

	switch (tc->kind) {

	case CORBA_tk_alias: {
		subval = *val;
		ORBit_trace_value (&subval, tc->subtypes[0]);
		break;
	}

#define _ORBIT_HANDLE_TYPE(ctk,typ,ctpye,wirebits,wirebytes,format) \
	case CORBA_tk_##ctk: \
		tprintf (format, *(CORBA_##typ *) *val); \
		break;

/* CORBA_tk_, CORBA_ type, C-stack type, wire size (bits), wire size (bytes), print format */
	_ORBIT_HANDLE_TYPE (short, short, int, 16, 2, "%d");
	_ORBIT_HANDLE_TYPE (long, long, int, 32, 4, "0x%x");
	/* FIXME: for an enum, would be nice to dump the string name ! */
	_ORBIT_HANDLE_TYPE (enum, long, int, 32, 4, "%d");
	_ORBIT_HANDLE_TYPE (ushort, unsigned_short, unsigned int, 16, 2, "%u");
	_ORBIT_HANDLE_TYPE (ulong, unsigned_long, unsigned int, 32, 4, "0x%x");
	_ORBIT_HANDLE_TYPE (longlong, long_long, long long, 64, 8, "%"G_GINT64_FORMAT);
	_ORBIT_HANDLE_TYPE (ulonglong, unsigned_long_long, unsigned long long, 64, 8, "%"G_GUINT64_FORMAT);
	_ORBIT_HANDLE_TYPE (boolean, boolean, int, 8, 1, "%d");
	_ORBIT_HANDLE_TYPE (char, char, int, 8, 1, "'%c'");
	_ORBIT_HANDLE_TYPE (wchar, wchar, int, 16, 2, "'%lc'");
	_ORBIT_HANDLE_TYPE (octet, octet, int, 8, 1, "0x%x");
	_ORBIT_HANDLE_TYPE (float, float, double, 32, 4, "%f");
	_ORBIT_HANDLE_TYPE (double, double, double, 64, 8, "%g");

	case CORBA_tk_any:
		ORBit_trace_any (*val);
		break;

	case CORBA_tk_objref:
		ORBit_trace_objref (*(CORBA_Object*)*val);
		break;

	case CORBA_tk_TypeCode:
		ORBit_trace_typecode (*(CORBA_TypeCode *)*val);
		break;

	case CORBA_tk_except:
	case CORBA_tk_struct:
		subval = *val;
		tprintf ("{ ");
		for (i = 0; i < tc->sub_parts; i++) {
			ORBit_trace_value (&subval, tc->subtypes [i]);
			if (i < tc->sub_parts - 1)
				tprintf (", ");
		}
		tprintf (" }");
		break;

	case CORBA_tk_union: {
		CORBA_TypeCode subtc;
		int            al = 0, sz = 0;
		gconstpointer  body, discrim;

		subval = *val;

		tprintf ("{ d=");

		ORBit_trace_value (&subval, tc->discriminator);
		tprintf (" v=");

		discrim = *val;
		subtc = ORBit_get_union_tag (tc, &discrim, FALSE);
		for (i = 0; i < tc->sub_parts; i++) {
			al = MAX (al, tc->subtypes [i]->c_align);
			sz = MAX (sz, ORBit_gather_alloc_info (tc->subtypes [i]));
		}
		body = ALIGN_ADDRESS (subval, al);
		ORBit_trace_value (&body, subtc);
		tprintf (" }");
		break;
	}

	case CORBA_tk_wstring:
		tprintf ("wstring");
		break;

	case CORBA_tk_string: {
		const char * v = (*(const char **)*val);
		if (v == NULL)
			tprintf("(null)");
		else {
			static int max = -1;
			const char * v = (*(const char **)*val);
			GString *str;
			int len = strlen (v);

			if (max < 0) {
				max = 64;
				if (g_getenv ("ORBIT2_DEBUG_STRMAX"))
					max = atoi (g_getenv ("ORBIT2_DEBUG_STRMAX"));
			}
			str = g_string_sized_new (max + 8);
			for (i = 0; i < MIN (max, len); i++) {
				if (g_ascii_isprint (v[i]) && v[i] != '#')
					g_string_append_c (str, v[i]);
				else {
					g_string_append_c (str, '#');
					g_string_append_printf (str, "0x%2x", v[i]);
					g_string_append_c (str, '#');
				}
			}
			if (len > max)
				g_string_append (str, " ...");

			tprintf ("'%s'", str->str);
			
			g_string_free (str, TRUE);
		}
		break;
	}

	case CORBA_tk_sequence: {
		const CORBA_sequence_CORBA_octet *sval = *val;
		subval = sval->_buffer;

		tprintf ("seq[%d]={ ", sval->_length);

		for(i = 0; i < sval->_length; i++) {
			ORBit_trace_value (&subval, tc->subtypes[0]);
			if (i < sval->_length - 1)
				tprintf (", ");
		}
		tprintf (" }");
		break;
	}

	case CORBA_tk_array: {
		subval = *val;

		tprintf ("array[%d]={ ", tc->length);
		for(i = 0; i < tc->length; i++) {
			ORBit_trace_value (&subval, tc->subtypes[0]);
			if (i < tc->length - 1)
				tprintf (", ");
		}
		tprintf (" }");
		break;
	}

	case CORBA_tk_longdouble:
	case CORBA_tk_fixed:
		tprintf ("wierd");
		break;

	case CORBA_tk_null:
		tprintf ("null");
		break;

	case CORBA_tk_void:
		tprintf ("void");
		break;

	default:
		g_error("Can't encode unknown type %d", tc->kind);
	}

	*val = ((guchar *)*val) + ORBit_gather_alloc_info (tc);
}

void
ORBit_trace_timestamp (void)
{
	GTimeVal t;

	g_get_current_time (&t);
	tprintf ("%lu.%06lu ", t.tv_sec, t.tv_usec);
}

void
ORBit_trace_header (CORBA_Object   object,
		    ORBit_IMethod *m_data)
{

	tprintf ("p%5d ", getpid ());

	tprintf_timestamp ();
	tprintf (": (");
	ORBit_trace_objref (object);
	tprintf (")->%s (", NOT_NULL (m_data->name));
}

void
ORBit_trace_end_method (void)
{
	tprintf (" ");
	tprintf_timestamp ();
	tprintf ("\n");
}

void
ORBit_trace_profiles (CORBA_Object obj)
{
	tprintf ("p %d: Obj %p (%s) profiles: ",
		 getpid (), obj,
		 g_quark_to_string (obj ? obj->type_qid : 0));
	if (obj) {
		GSList *l;

		for (l = obj->profile_list; l; l = l->next) {
			char *s;
			s = IOP_profile_dump (obj, l->data);
			tprintf ("'%s' ", s);
			g_free (s);
		}
	}
	tprintf ("\n");
}

#endif /* G_ENABLE_DEBUG */
