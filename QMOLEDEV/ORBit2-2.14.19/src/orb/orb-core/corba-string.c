#include "config.h"
#include <string.h>
#include <orbit/orbit.h>

CORBA_char *
CORBA_string_alloc (CORBA_unsigned_long len)
{
	return ORBit_alloc_string (len + 1);
}

CORBA_char *
CORBA_string_dup (const CORBA_char *str)
{
	CORBA_char         *retval;
	CORBA_unsigned_long len;

	if (!str)
		return NULL;

	len = strlen (str) + 1;

	retval = ORBit_alloc_string (len);
	memcpy (retval, str, len);

	return retval;
}

CORBA_wchar *
CORBA_wstring_alloc (CORBA_unsigned_long len)
{
	return ORBit_alloc_simple ((len + 1) * 2);
}

CORBA_unsigned_long
CORBA_wstring_len (CORBA_wchar *ws)
{
	int i;

	for (i = 0; ws [i]; i++)
		; /**/

	return i;
}

CORBA_wchar *
CORBA_wstring_dup (const CORBA_wchar *str)
{
	CORBA_wchar *retval;
	CORBA_unsigned_long len;

	if (!str)
		return NULL;

	len = CORBA_wstring_len (str);

	retval = CORBA_wstring_alloc (len);
	memcpy (retval, str, (len + 1) * 2);

	return retval;
}
