/*
 $Header: /cvsroot/lesstif/lesstif/test/Xm/filesb/test12.c,v 1.4 2001/06/15 09:17:36 amai Exp $
From:        Eric Howe <mu@clio.trends.ca>
To:          lesstif@hungry.com
Subject:     FileSB defaultQualify proc
Date:        Sun, 5 Jul 1998 22:36:57 -0400 (EDT)
*/

/*
 * This test program will test the file selector's XmNqualifySearchDataProc
 * with various combinations of value, mask, dir, and pattern.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <Xm/Xm.h>
#include <Xm/FileSB.h>
#include "mkdirtree.h"


static char *FallBack[] = {
		"*.geometrySlop: 0",
		NULL
};

static XmQualifyProc defaultQualify;

/*
 * I'm sorry (a polite lie) but this is just too much to type more than once.
 * True, it's better than "XmRepTypeInstallTearOffModelConverter" but that
 * abomination doesn't get used nearly as much.
 */
typedef XmFileSelectionBoxCallbackStruct XmFSB;

static char *
tostring(XmString xs, char *buf)
{
	char *s = NULL;
	if(!XmStringGetLtoR(xs, XmFONTLIST_DEFAULT_TAG, &s))
		strcpy(buf, "(null)");
	else
		sprintf(buf, "'%s'", s);
	XtFree(s);
	return buf;
}

/*
 * The lengths are the number of *bytes* in the corresponding XmString
 * so they're always different with Lesstif (there's a non-zero chance
 * that they'll be different between, say, SGI Motif and Solaris Motif
 * too). The Motif 2.0.1 man page for XmFileSelectionBox says that the
 * length fields are obsolete and only exist for backwards compatibility.
 * Hence, I deem the length fields irrelevant and don't print them.
 */
static void
dumpit(XmFSB *cbs)
{
	char buf[1024];
	printf("\tvalue = %s\n", tostring(cbs->value,   buf));
	printf("\tmask  = %s\n", tostring(cbs->mask,    buf));
	printf("\tdir   = %s\n", tostring(cbs->dir,     buf));
	printf("\tpat   = %s\n", tostring(cbs->pattern, buf));
}

static void
qualify(Widget w, XmFSB *in, XmFSB *out)
{
	printf("in:\n");
	dumpit(in);
	defaultQualify(w, (XtPointer)in, (XtPointer)out);
	printf("out:\n");
	dumpit(out);
	printf("------------------------------------------------------\n");
}

/*
 * Make sure none of these constants have "/tmp" as the path.
 * We chdir to /tmp before the fun starts to make things come
 * out the same no matter where you are and we don't want to
 * mask any problems by having the current directory equal
 * one of the directories in a resource setting.
 */
#define VALUE		"/dev/null"
#define	DIRMASK		"/etc/*.c"
#define	DIRECTORY	"/bin/"
#define	PATTERN		"*.h"

static struct {
	char *value;
	char *mask;
	char *dir;
	char *pattern;
} tests[] = {
	{ 0,     0,       0,         0       },

	{ VALUE, 0,       0,         0       },
	{ 0,     DIRMASK, 0,         0       },
	{ 0,     0,       DIRECTORY, 0       },
	{ 0,     0,       0,         PATTERN },

	{ VALUE, DIRMASK, 0,         0       },
	{ VALUE, 0,       DIRECTORY, 0       },
	{ VALUE, 0,       0,         PATTERN },
	{ 0,     DIRMASK, DIRECTORY, 0       },
	{ 0,     DIRMASK, 0,         PATTERN },
	{ 0,     0,       DIRECTORY, PATTERN },

	{ VALUE, DIRMASK, DIRECTORY, 0       },
	{ VALUE, DIRMASK, 0,         PATTERN },
	{ VALUE, 0,       DIRECTORY, PATTERN },
	{ 0,     DIRMASK, DIRECTORY, PATTERN },

	{ VALUE, DIRMASK, DIRECTORY, PATTERN },
};
#define N_TESTS (sizeof(tests)/sizeof(tests[0]))

static void
clear(XmFSB *x)
{
	if(x->value != NULL)
		XmStringFree(x->value);
	if(x->mask != NULL)
		XmStringFree(x->mask);
	if(x->dir != NULL)
		XmStringFree(x->dir);
	if(x->pattern != NULL)
		XmStringFree(x->pattern);
	memset(x, '\0', sizeof(XmFSB));
}

static void
set(XmString *x, int *len, char *s)
{
	if(s == NULL)
		return;
	*x   = XmStringCreateLocalized(s);
	*len = XmStringLength(*x);
}

int
main(int argc, char **argv)
{
	XtAppContext ac;
	Widget       top, fsb;
	int          i;
	XmFSB        in, out;

	/*
	 * Go to /tmp so that the output will be consistent no
	 * matter where we're run from.  I'll just assume that anyone
	 * can chdir to /tmp.
	 */
	chdir("/tmp");

	top = XtVaAppInitialize(&ac, "defq", NULL, 0, &argc, argv,
				FallBack, NULL);

	fsb = XtCreateManagedWidget("fsb", xmFileSelectionBoxWidgetClass, top,
				NULL, 0);
	XtVaGetValues(fsb, XmNqualifySearchDataProc, &defaultQualify, NULL);

	memset(&in,  '\0', sizeof(in));
	memset(&out, '\0', sizeof(out));
	for(i = 0; i < N_TESTS; ++i) {
		set(&in.value,   &in.length,         tests[i].value);
		set(&in.mask,    &in.mask_length,    tests[i].mask);
		set(&in.dir,     &in.dir_length,     tests[i].dir);
		set(&in.pattern, &in.pattern_length, tests[i].pattern);

		qualify(fsb, &in, &out);

		clear(&in);
		clear(&out);
	}

	XtDestroyWidget(top);

	return 0;
}
