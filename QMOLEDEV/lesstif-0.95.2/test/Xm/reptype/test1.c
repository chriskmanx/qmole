/* $Header: /cvsroot/lesstif/lesstif/test/Xm/reptype/test1.c,v 1.2 2002/04/13 12:06:27 amai Exp $ */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <Xm/Xm.h>
#include <Xm/RepType.h>

#define TestRepType "TestRepType"

static char *FallBack[] = {
    NULL
};

int
main(int argc, char **argv)
{
    int target;
    int GlobalErrors = 0;
    XtAppContext app;
    Widget Shell;
    XmRepTypeId id;
    XrmValue from, to;
    XrmValue rev_from, rev_to;
    static String value_names[] = {
	"client",
	"server",
    };
    static unsigned char values[] = {
	100,
	101,
    };

    XtSetLanguageProc(NULL, NULL, NULL);

    Shell = XtVaAppInitialize(&app, "Shell", 
    	NULL, 0, 
    	&argc, argv, 
    	FallBack, 
    	NULL);

    XtRealizeWidget(Shell);
    id = XmRepTypeRegister(TestRepType, 
	value_names, values,
	XtNumber(value_names));
    XmRepTypeAddReverse(id);
    printf("XmRepTypeId %i\n", id);

    for (target = 0; target < XtNumber(value_names); target++)
    {
	from.size = sizeof(String);
	from.addr = value_names[target];
	printf("Convert %s >%s< -> %s ",
	       XmRString,
	       from.addr,
	       TestRepType);
	XtConvertAndStore(Shell, XmRString, &from, TestRepType, &to);
	if (to.size != sizeof(unsigned char) || *(to.addr) != values[target])
	{
	    printf("Convert error\n");
	    GlobalErrors++;
	}
	else
	{
	    printf(">%d< (okay)\n", *(to.addr));
	}

	rev_from.size = to.size;
	rev_from.addr = (XtPointer)&(values[target]);
	printf("Convert %s >%d< -> XmRString ",
	       TestRepType,
	       *(rev_from.addr));
	rev_to.size = 0;
	rev_to.addr = NULL;
	XtConvertAndStore(Shell, TestRepType, &rev_from, XmRString, &rev_to);
	if (rev_to.size != sizeof(String)
	    || strcmp(rev_to.addr, value_names[target]) != 0)
	{
	    printf("Convert error\n");
	    GlobalErrors++;
	}
	else
	{
	    printf(">%s< (okay)\n", rev_to.addr);
	}
    }
    exit(GlobalErrors);
}
