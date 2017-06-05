/* $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test5.c,v 1.3 2002/04/17 16:32:01 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/XmP.h>

#ifdef LESSTIF_VERSION
typedef struct __XmStringRec {
    unsigned char tag;
    unsigned char len;
    unsigned char data[1];
} XmStringRec;

struct __XmStringContextRec {
     struct __XmStringRec	*string;
     unsigned char data[4];
};

#define XmSTRING_COMPONENT_XMSTRING	(XmSTRING_COMPONENT_LOCALE_TEXT + 1)
#define XmSTRING_TAG			0xDFU
#define XmSTRING_LENGTH			0x80U


int
asn1_dump(unsigned char *string)
{
    unsigned length, i, nlen, j;
    struct __XmStringRec *str = (struct __XmStringRec *)string;
    unsigned char *next;

    printf("STRING: TAG: %02x LEN: %02x\n", str->tag, str->len);
    fflush(stdout);

    if (str->tag != XmSTRING_TAG || str->len != XmSTRING_LENGTH) {
	printf("IS NOT AN XmSTRING\n");
	fflush(stdout);
	return 0;
    }

    next = str->data;
    str = (struct __XmStringRec *)next;

    if (str->tag != XmSTRING_COMPONENT_XMSTRING) {
	printf("IS NOT AN XmSTRING: %d\n", __LINE__);
	fflush(stdout);
	return 0;
    }

    length = 0;

    if (str->len > XmSTRING_LENGTH) {

	for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++) {
	    length <<= 8;
	    length |= str->data[i];
	    if (i > sizeof(unsigned)) {
		printf("Invalid XmString\n");
		fflush(stdout);
		exit (0);
	    }
	}
    }
    else {
	i = 0;
	length = str->len & ~XmSTRING_LENGTH;
    }

    next = &str->data[i];

    if (length < 0) {
	printf("String is malformed\n");
	fflush(stdout);
	return 0;
    }
    else if (length == 0)
	return 0;

    for (;;) {
        str = (struct __XmStringRec *)next;

	/* primitive type -- doesn't recurse */
	nlen = 0;

	if (str->len > XmSTRING_LENGTH) {

	    for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++) {
		nlen <<= 8;
		nlen |= str->data[i];
		if (i > sizeof(unsigned)) {
		    printf("Invalid XmString\n");
		    fflush(stdout);
		    exit (0);
		}
	    }
	}
	else {
	    i = 0;
	    nlen = str->len & ~XmSTRING_LENGTH;
	}

	switch (str->tag) {
	case XmSTRING_COMPONENT_UNKNOWN:
	    printf("UNKNOWN COMPONENT: length %d\n", nlen);
	    fflush(stdout);
	    break;

	case XmSTRING_COMPONENT_CHARSET:
	    printf("CHARSET:\n");
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		putchar(str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + 2;
	    break;

	case XmSTRING_COMPONENT_TEXT:
	    printf("TEXT: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		putchar(str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + 2;
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    printf("DIRECTION: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		printf("%d ", str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + 2;
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    printf("SEPARATOR: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		printf("%d ", str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + 2;
	    break;

	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    printf("LOCALE TEXT: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		putchar(str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + 2;
	    break;
	default:
	    printf("invalid tag: %02x\n", str->tag);
	    fflush(stdout);
	    nlen = 1;
	}

	next += nlen;
	length -= nlen;
	if (length < 0) {
	    printf("String is malformed\n");
	    fflush(stdout);
	    return 0;
	}
	else if (length == 0)
	    return 0;
    }
}
#endif

int 
main(int argc,
     char **argv)
{
#ifdef LESSTIF_VERSION
    Widget toplevel;
    XtAppContext app;
    Dimension width, height;
    XmFontList fl;
    XmString xmstr, xmstr1, xmstr2, xmstr3, xmstr4, xmstr5, xmstr6, xmstr7;
    XmFontList fontlist;
    char buf[] = { 0xdf, 0x80, 0x06, 0x00, 0x01, 0x00 };

    printf("SIZEOF CONTEXT: %d\n", sizeof(struct __XmStringContextRec));
    fflush(stdout);

    toplevel = XtVaAppInitialize(&app, "XmString", NULL, 0, &argc, argv, NULL, NULL);

    fl = _XmGetDefaultFontList(toplevel, XmTEXT_FONTLIST);

    xmstr3 = XmStringSeparatorCreate();
    asn1_dump((unsigned char *)xmstr3);
    printf("*************************************************\n");
    fflush(stdout);


    xmstr = XmStringCreateSimple("Hello World this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test this is a long test");
    asn1_dump((unsigned char *)xmstr);
    printf("*************************************************\n");
    fflush(stdout);

    xmstr2 = XmStringCreateLtoR("Héllo\nWörld", XmFONTLIST_DEFAULT_TAG);
    asn1_dump((unsigned char *)xmstr2);
    printf("*************************************************\n");
    fflush(stdout);

    xmstr4 = XmStringDirectionCreate(XmSTRING_DIRECTION_R_TO_L);
    asn1_dump((unsigned char *)xmstr4);
    printf("@@@@@@@@*************************************************\n");
    fflush(stdout);

    xmstr5 = XmStringSegmentCreate("Hello World", XmFONTLIST_DEFAULT_TAG,
				   XmSTRING_DIRECTION_R_TO_L, True);
    asn1_dump((unsigned char *)xmstr5);
    printf("#######*************************************************\n");
    fflush(stdout);

    xmstr6 = XmStringCreate("Hello World", XmFONTLIST_DEFAULT_TAG);
    asn1_dump((unsigned char *)xmstr6);
    printf("*************************************************\n");
    fflush(stdout);

    xmstr7 = XmStringCreateLocalized("Hello World");
    asn1_dump((unsigned char *)xmstr7);
    printf("*************************************************\n");
    fflush(stdout);

    XmStringExtent(fl, xmstr, &width, &height);

    printf ("String 'Hello World' has dimensions %dx%d\n", width, height);

    XmStringExtent(fl, xmstr2, &width, &height);

    printf ("String 'Hello\\nWorld' has dimensions %dx%d\n", width, height);

    xmstr1 = XmStringCreate("Here is a ", "MY_FONT1");
    xmstr2 = XmStringCreate("different font", "MY_FONT");
    xmstr = XmStringConcat(xmstr1, xmstr2);

    fontlist = XmFontListAppendEntry(NULL,
			   XmFontListEntryCreate("MY_FONT",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-o-normal--17-0-75-75-p-*-iso8859-1")));

    fontlist = XmFontListAppendEntry(fontlist,
			   XmFontListEntryCreate("MY_FONT1",
						 XmFONT_IS_FONT,
						 XLoadQueryFont(XtDisplay(toplevel), 
 	                                         "-adobe-helvetica-bold-r-normal--17-0-75-75-p-*-iso8859-1")));

    asn1_dump((unsigned char *)xmstr);
    printf("*************************************************\n");
    fflush(stdout);

    if (_XmStringIsXmString((XmString)buf))
	printf("IS STRING\n");
    else
	printf("ISN'T STRING\n");

    asn1_dump((unsigned char *)buf);
    printf("*************************************************\n");
    fflush(stdout);

#endif
    exit(0);
}
