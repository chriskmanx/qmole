#include <config.h>
#include <orbit/util/orbit-util.h>

gulong
ORBit_wchar_strlen (CORBA_wchar *wstr)
{
	gulong i;

	for (i = 0; wstr[i]; i++)
		;

	return i;
}
