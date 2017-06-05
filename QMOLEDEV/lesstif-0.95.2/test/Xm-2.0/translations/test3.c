/*
 * $Header: /cvsroot/lesstif/lesstif/test/Xm-2.0/translations/test3.c,v 1.1 2002/04/03 09:42:50 dannybackx Exp $
 */
 
#include <stdlib.h>
#include <stdio.h>

#include <Xm/Xm.h>

#if XmVersion < 2
int
main(int argc, char *argv[])
{
 puts("This program requires Motif 2.0\n");
 exit(1);
}

#else

int
main(int argc, char *argv[])
{
	Boolean		r;
	int		t[10], *tt = &t[0], c;
	KeySym		k[10], *kk = &k[0];
	Modifiers	m[10], *mm = &m[0];
	char		*a;

	a = "Ctrl<Key>o";
	c = _XmMapKeyEvents(a, &tt, &kk, &mm);
	printf("_XmMapKeyEvents(%s) -> %d\n", a, c);

	a = "Ctrl<Key>o,Alt<Key>a";
	c = _XmMapKeyEvents(a, &tt, &kk, &mm);
	printf("_XmMapKeyEvents(%s) -> %d\n", a, c);

	exit(0);
}
#endif /* XmVersion != 2000 */
