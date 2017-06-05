/*
 $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test11.c,v 1.5 2001/06/15 09:17:36 amai Exp $

From:        Eric Howe <mu@clio.trends.ca>
To:          rwscott@hungry.com
Subject:     Re: cvs commit: hungry/lesstif/lib/Xm FileSB.c
Date:        Sat, 4 Jul 1998 14:11:46 -0400 (EDT)
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/FileSB.h>

#include "mkdirtree.h"

static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

static void
dumpstring(char *prefix, XmString xs)
{
	char *s = NULL;
	if(!XmStringGetLtoR(xs, XmFONTLIST_DEFAULT_TAG, &s))
		printf("%s(null)\n", prefix);
	else
		printf("%s%s\n", prefix, s);
	XtFree(s);
}

static void
dumpstuff(Widget w)
{
	XmString mask, pat, dir;

	XtVaGetValues(w,
		XmNdirMask,	&mask,
		XmNpattern,	&pat,
		XmNdirectory,	&dir,
		NULL);

	dumpstring("XmNdirMask   = ", mask);
	dumpstring("XmNpattern   = ", pat);
	dumpstring("XmNdirectory = ", dir);
	printf("\n");
}

static void
dothings(Widget w, ...)
{
	Widget   f;
	va_list  ap;
	Cardinal n;
	Arg      a[10];
	XmString x[10];
	char     *res, *val;

	memset(x, '\0', sizeof(x));

	n = 0;
	va_start(ap, w);
	while((res = (char *)va_arg(ap, char *)) != NULL) {
		val = (char *)va_arg(ap, char *);
		printf("set '%s' to '%s'\n", res, val);
		x[n] = XmStringCreateLocalized(val);
		XtSetArg(a[n], res, x[n]);
		++n;
	}

	va_end(ap);

	f = XtCreateManagedWidget("f", xmFileSelectionBoxWidgetClass, w, a, n);

	for(n = 0; n < sizeof(x)/sizeof(x[0]); ++n)
		if(x[n] != NULL)
			XmStringFree(x[n]);

	dumpstuff(f);
	XtDestroyWidget(f);
}

/*
 * Make sure none of these constants have "/tmp" as the path.
 * We chdir to /tmp before the fun starts to make things come
 * out the same no matter where you are and we don't want to
 * mask any problems by having the current directory equal
 * one of the directories in a resource setting.
 */
#define	DIRMASK1	"*.c"
#define	DIRMASK2	"/etc/*.c"
#define	DIRECTORY	"/bin/"
#define	PATTERN		"*.h"

int
main(int argc, char **argv)
{
	XtAppContext ac;
	Widget top, fsb;

	/*
	 * Go to /tmp so that the output will be consistent no
	 * matter where we're run from.  I'll just assume that anyone
	 * can chdir to /tmp.
	 */
	chdir("/tmp");

	top = XtVaAppInitialize(&ac, "fsbinit", NULL, 0, &argc, argv,
				FallBack, NULL);

	/*
	 * None.  All defaults.
	 */
	dothings(top, NULL);

	/*
	 * Singles.  The first dirMask thing is the half-bug with mgv
	 * that started this whole investigation.
	 */
	dothings(top, XmNdirMask,   DIRMASK1,  NULL);
	dothings(top, XmNdirMask,   DIRMASK2,  NULL);
	dothings(top, XmNdirectory, DIRECTORY, NULL);
	dothings(top, XmNpattern,   PATTERN,   NULL);

	/*
	 * Two at once.
	 */
	dothings(top, XmNdirMask,   DIRMASK2,  XmNdirectory, DIRECTORY, NULL);
	dothings(top, XmNdirMask,   DIRMASK2,  XmNpattern,   PATTERN,   NULL);
	dothings(top, XmNdirectory, DIRECTORY, XmNpattern,   PATTERN,   NULL);

	/*
	 * All three.
	 */
	dothings(top,
		XmNdirMask,   DIRMASK2,
		XmNdirectory, DIRECTORY,
		XmNpattern,   PATTERN,
		NULL);

	XtDestroyWidget(top);
	return 0;
}
