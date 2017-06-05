/* $Header: /cvsroot/lesstif/lesstif/test/Xm/xmstring/test7.c,v 1.3 2002/04/17 16:32:01 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <Xm/Label.h>
#include <Xm/XmP.h>

#define ASN_LENGTH_MODIFIER	0x80U

#ifdef LESSTIF_VERSION
typedef struct __XmStringRec {
    unsigned char tag;
    unsigned char len;
    unsigned char data[1];
} XmStringRec;

typedef struct _XmtStringContextRec {
    struct _XmStringRec *string;
    int current_segment;
} XmStringContextRec;

int
asn1_dump(unsigned char *string) {
    unsigned length, i, nlen;
    struct __XmStringRec *str = (struct __XmStringRec *)string;
    unsigned char *next;

    printf("STRING: TAG: %02x LEN: %02x\n", str->tag, str->len);

    length = 0;
    /* indefinite length ? */
    if (str->len != ASN_LENGTH_MODIFIER) {

	if (str->len > ASN_LENGTH_MODIFIER) {

	    for (i = 0; i < (str->len & ~ASN_LENGTH_MODIFIER); i++) {
		length <<= 8;
		length |= str->data[i];
		if (i > sizeof(unsigned)) {
		    fprintf(stderr, "Invalid XmString\n");
		    exit (0);
		}
	    }
	}
	else {
	    i = 0;
	    length = str->len & ~ASN_LENGTH_MODIFIER;
	}
	next = &str->data[i];

	/* primitive type -- doesn't recurse */
	switch (str->tag) {
	case XmSTRING_COMPONENT_UNKNOWN:
	    printf("UNKNOWN COMPONENT: length %d\n", length);
	    return length + i + 2;
	case XmSTRING_COMPONENT_CHARSET:
	    printf("CHARSET:\n");
	    for (nlen = 0; nlen < length; nlen++)
		putchar(str->data[i + nlen]);
	    putchar('\n');
	    return length + i + 2;
	case XmSTRING_COMPONENT_TEXT:
	    printf("TEXT: %d\n", length);
	    for (nlen = 0; nlen < length; nlen++)
		putchar(str->data[i + nlen]);
	    putchar('\n');
	    return length + i + 2;
	case XmSTRING_COMPONENT_DIRECTION:
	    printf("DIRECTION: %d\n", length);
	    for (nlen = 0; nlen < length; nlen++)
		printf("%d ", str->data[i + nlen]);
	    putchar('\n');
	    return length + i + 2;
	case XmSTRING_COMPONENT_SEPARATOR:
	    printf("SEPARATOR: %d\n", length);
	    for (nlen = 0; nlen < length; nlen++)
		printf("%d ", str->data[i + nlen]);
	    putchar('\n');
	    return length + i + 2;
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    printf("LOCALE TEXT: %d\n", length);
	    for (nlen = 0; nlen < length; nlen++)
		putchar(str->data[i + nlen]);
	    putchar('\n');
	    return length + i + 2;
	case (XmSTRING_COMPONENT_LOCALE_TEXT+1):
	    i += length + 2;
	    for (;;) {
		nlen = asn1_dump(next);
		next += nlen;
		length -= nlen;
		if (length <= 0)
		    return 0;
	    }
	default:
	    printf("invalid tag: %02x\n", str->tag);
	    return 1;
	}
    }
    else { /* it's indefinite */
	next = str->data;
	str = (struct __XmStringRec *)next;
	while (str->tag != 0 && str->len != 0) {
	    nlen = asn1_dump(next);
	    if (nlen == 0)
		break;
	    next += nlen;
	    str = (struct __XmStringRec *)next;
	}
    }
    return 0;
}
#endif

int
main(int argc, char **argv)
{
#ifdef LESSTIF_VERSION
  Widget toplevel;
  XtAppContext app;
  XmFontList fontlist;
  XmString xmstr1 = XmStringCreate("Here is a ", "MY_FONT1");
  XmString xmstr2 = XmStringCreate("different font", "MY_FONT");
  char buf[] = { 0xdf, 0x80, 0x06, 0x00, 0x01, 0x00 };
  XmStringContext context;
  char *text;
  XmStringCharSet cs;
  XmStringDirection dir;
  Boolean sep;

  XmString xmstr = XmStringConcat(xmstr1, xmstr2);

  XtSetLanguageProc(NULL, NULL, NULL);

  toplevel = XtVaAppInitialize(&app, "Label", NULL, 0, &argc, argv, NULL, NULL);

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

  printf("SINGLE SEGMENT\n");
  text = NULL;
  cs = "-adobe-helvetica-bold-r-normal--17-0-75-75-p-*-iso8859-1";
  _XmStringSingleSegment(xmstr, &text, &cs);
  printf("text: %s cs: %s\n", text ? text : "(empty)", cs ? cs : "(empty)");

  if (_XmStringIsXmString((XmString)buf))
	printf("IS STRING\n");
  else
	printf("ISN'T STRING\n");

  asn1_dump((unsigned char *)buf);

  XmStringInitContext(&context, xmstr);
  while (XmStringGetNextSegment(context, &text, &cs, &dir, &sep)) {
    printf("%s %s %d %d\n", text ? text : "(null)", cs ? cs : "(null)",
	   dir, sep);
  }
  XmStringFreeContext(context);

  printf("current charset: %s\n", _XmStringGetCurrentCharset());
#endif
  exit(0);
}
