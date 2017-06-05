/*
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/XmString.c,v 1.6 2006/04/19 18:42:22 dannybackx Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 * Copyright © 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Xm-2.1/XmString.c,v 1.6 2006/04/19 18:42:22 dannybackx Exp $";

/*
 * ** Danny had added a flag here, but I think the behavior he added is always
 *  desirable.  What this refers to is checking in XmStringGetLtoR for the
 *  value of XmSTRING_DEFAULT_CHARSET for the tag. - MLM
 *
 * Not defining this breaks certain things badly (Danny 11/8/1996).
 * As I can feel that this statement requires justification by examples,
 *  here' s one :
 *    The XmStringCreateSimple() routine uses tag XmFONTLIST_DEFAULT_TAG.
 * If you use its result with XmStringGetLtoR and use XmSTRING_DEFAULT_CHARSET
 *  for tag, (as pre-1.2 documentation says you should), then an invalid result
 *  would be obtained.
 *
 * ** There is an implicit assumption that charsets come before the text
 *  they are used by in the internal representation.  Don't change this
 *  if you want to live.
 */

#include <LTconfig.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
/* #include <locale.h> */
#include <limits.h>

#include <XmI/XmI.h>
#include <Xm/XmP.h>
#include <Xm/DisplayP.h>

#include <XmI/LTmisc.h>
#include <XmI/MacrosI.h>

#include <XmI/DebugUtil.h>


/*
 * for information on the external encoding, see doc/XmStrings.txt
 */
/*
 * 1 octect tag 1 octet (nominal) len
 */
#define ASN1_HEADER_SIZE	2
/*
 * ASN1 header + XmSTRING header
 */
#define XmSTRING_HEADER_SIZE	(ASN1_HEADER_SIZE * 2)

/* We don't need this in a production build */
#ifdef LESSTIF_PRODUCTION
#ifdef XMSTRING_DEBUG
#undef XMSTRING_DEBUG
#endif
#else
#ifndef XMSTRING_DEBUG
#define XMSTRING_DEBUG
#endif
#endif

#if	USE_XFT
#include <X11/Xft/Xft.h>
#include <X11/extensions/Xrender.h>
#endif

/**************************** PRIVATE FUNCTIONS ****************************/

#if 0
	/* These are never ever used. */
#ifdef XMSTRING_DEBUG
static int
asn1_dump(unsigned char *string)
{
    unsigned length, i, nlen, j;
    struct __XmStringExtRec *str = (struct __XmStringExtRec *)string;
    unsigned char *next;

    printf("STRING: TAG: %02x LEN: %02x\n", str->tag, str->len);
    fflush(stdout);

    if (str->tag != XmSTRING_TAG || str->len != XmSTRING_LENGTH)
    {
	printf("IS NOT AN XmSTRING\n");
	fflush(stdout);
	return 0;
    }

    next = str->data;
    str = (struct __XmStringExtRec *)next;

    if (str->tag != XmSTRING_COMPONENT_XMSTRING)
    {
	printf("IS NOT AN XmSTRING: %d\n", __LINE__);
	fflush(stdout);
	return 0;
    }

    length = 0;

    if (str->len > XmSTRING_LENGTH)
    {

	for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++)
	{
	    length <<= 8;
	    length |= str->data[i];
	    if (i > sizeof(unsigned))
	    {
		printf("Invalid XmString\n");
		fflush(stdout);
		return 0;
	    }
	}
    }
    else
    {
	i = 0;
	length = str->len & ~XmSTRING_LENGTH;
    }

    next = &str->data[i];

    if (length < 0)
    {
	printf("String is malformed\n");
	fflush(stdout);
	return 0;
    }
    else if (length == 0)
    {
	return 0;
    }

    for (;;)
    {
	str = (struct __XmStringExtRec *)next;

	/* primitive type -- doesn't recurse */
	nlen = 0;

	if (str->len > XmSTRING_LENGTH)
	{

	    for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++)
	    {
		nlen <<= 8;
		nlen |= str->data[i];
		if (i > sizeof(unsigned))
		{
		    printf("Invalid XmString\n");
		    fflush(stdout);
		    exit(0);
		}
	    }
	}
	else
	{
	    i = 0;
	    nlen = str->len & ~XmSTRING_LENGTH;
	}

	switch (str->tag)
	{
	case XmSTRING_COMPONENT_UNKNOWN:
	    printf("UNKNOWN COMPONENT: length %d\n", nlen);
	    fflush(stdout);
	    nlen++;
	    break;

	case XmSTRING_COMPONENT_CHARSET:
	    printf("CHARSET:\n");
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		putchar(str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_TEXT:
	    printf("TEXT: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		putchar(str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    printf("DIRECTION: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		printf("%d ", str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    printf("SEPARATOR: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		printf("%d ", str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    printf("LOCALE TEXT: %d\n", nlen);
	    fflush(stdout);
	    for (j = 0; j < nlen; j++)
		putchar(str->data[i + j]);
	    putchar('\n');
	    fflush(stdout);
	    nlen += i + ASN1_HEADER_SIZE;
	    break;
	default:
	    printf("invalid tag: %02x\n", str->tag);
	    fflush(stdout);
	    nlen = 1;
	}

	next += nlen;
	length -= nlen;
	if (length < 0)
	{
	    printf("String is malformed\n");
	    fflush(stdout);
	    return 0;
	}
	else if (length == 0)
	{
	    printf("\n\n");
	    fflush(stdout);
	    return 0;
	}
    }
}

static void
_Xm_dump_fontlist(XmFontList fontlist)
{
    int i;

    printf("Fontlist: %p\n", fontlist);
    for (i = 0;
	 fontlist && fontlist->renditions[i]->tag
	 	&& strlen(fontlist->renditions[i]->tag) != 0;
	 i++)
    {
	printf("Fontlist entry: %d : tag: %s : type: %d : font: %p\n",
	       i, fontlist->renditions[i]->tag, fontlist->renditions[i]->type,
	       fontlist->renditions[i]->font);
    }
    printf("\n");
}


static void
_Xm_dump_fontlist_cache(void)
{
}


static void
_Xm_dump_external(XmString str)
{
    asn1_dump(str);
    printf("\n");
}


static void
_Xm_dump_internal(_XmString str)
{
    int i;

    if (!str)
    {
	printf(" NULL internal string\n");
	return;
    }

    for (i = 0; i < str->number_of_components; i++)
    {
	switch (str->components[i]->type)
	{
	case XmSTRING_COMPONENT_UNKNOWN:
	    printf(" %d: UNKNOWN component\n", i);
	    break;

	case XmSTRING_COMPONENT_CHARSET:
	    printf(" %d: CHARSET: %s\n", i, str->components[i]->data);
	    break;

	case XmSTRING_COMPONENT_TEXT:
	    printf(" %d: TEXT: %s, font: %d\n", i, str->components[i]->data,
		   str->components[i]->font);
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    printf(" %d: DIRECTION: %d\n", i, (int)str->components[i]->data[0]);
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    printf(" %d: SEPARATOR\n", i);
	    break;

	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    printf(" %d: LOCALE TEXT: %s, font: %d\n", i,
		   str->components[i]->data, str->components[i]->font);
	    break;

	default:
	    break;
	}
    }
    printf("\n");
}
#endif /* XMSTRING_DEBUG */
#endif	/* Never ever used */

/* Don't trust wchar_t to be 2-bytes.  On some systems it's 4-bytes */
typedef unsigned short unicode_t;

/* Find the length of a Unicode string */
static int 
_ucstrlen(const unicode_t* uch) 
{
    int len = 0;
    while (uch[len] != 0) ++len;
    return (len);
}

static _XmString
__XmAllocNewXmString(int number_of_components)
{
    _XmString newString = (_XmString)XtCalloc(1, sizeof(struct __XmStringRec));
    int i;

    newString->number_of_components = number_of_components;

    if (number_of_components)
    {
	newString->components =
	    (_XmStringComponent *)XtMalloc(sizeof(_XmStringComponent)
					   * newString->number_of_components);
    }

    for (i = 0; i < number_of_components; i++)
    {
	newString->components[i] =
	    (_XmStringComponent)XtCalloc(1, sizeof(_XmStringComponentRec));
    }

    return newString;
}

static void
__XmGrowXmString(_XmString string)
{
    string->number_of_components++;

    if (string->number_of_components == 1)
    {
	string->components = (_XmStringComponent *)XtMalloc(sizeof(_XmStringComponent));
    }
    else
    {
	string->components =
	    (_XmStringComponent *)XtRealloc((char *)string->components,
					    sizeof(_XmStringComponent)
					    * string->number_of_components);
    }

    string->components[string->number_of_components - 1] =
	(_XmStringComponent)XtCalloc(1, sizeof(_XmStringComponentRec));
}

static _XmStringComponent
__XmStringPeekNextComponent(_XmStringContext context)
{
    if (context == NULL)
    {
	return NULL;
    }

    if (context->current_component < (context->string->number_of_components - 1))
    {
	return context->string->components[context->current_component + 1];
    }
    else
    {
	return NULL;
    }
}

static _XmStringComponent
__XmStringGetNextComponent(_XmStringContext context)
{
    if (context == NULL)
	return NULL;

    context->current_component++;

    if (context->current_component < context->string->number_of_components)
    {
	return context->string->components[context->current_component];
    }
    else
    {
	return NULL;
    }
}

static void
__XmStringComponentCopy(struct __XmStringComponentRec *dest,
			struct __XmStringComponentRec *src)
{
    dest->type = src->type;
    dest->length = src->length;

    if (src->type == XmSTRING_COMPONENT_WIDECHAR_TEXT) {
	dest->data = (char*)XtMalloc(src->length + sizeof(unicode_t));
	memcpy(dest->data, src->data, src->length + sizeof(unicode_t));
    } else {
    	dest->data = XtNewString(src->data);
    }
}


static _XmString
__XmStringFromASN1(XmString string)
{
    unsigned i, nlen, length;
    struct __XmStringExtRec *str = (struct __XmStringExtRec *)string;
    unsigned char *next;
    char *charset;
    _XmString intern;

    if (!string)
    {
	return NULL;
    }

    if (str->tag != XmSTRING_TAG || str->len != XmSTRING_LENGTH)
    {
	return NULL;
    }

    next = str->data;
    str = (struct __XmStringExtRec *)next;

    if (str->tag != XmSTRING_COMPONENT_XMSTRING)
    {
	return NULL;
    }

    length = 0;

    if (str->len > XmSTRING_LENGTH)
    {

	for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++)
	{
	    length <<= 8;
	    length |= str->data[i];
	    if (i > sizeof(unsigned))
	    {
		_XmWarning(NULL, "Invalid XmString\n");
		return NULL;
	    }
	}
    }
    else
    {
	i = 0;
	length = str->len & ~XmSTRING_LENGTH;
    }

    next = &str->data[i];

    if (length <= 0)
    {
	return NULL;
    }

    intern = __XmAllocNewXmString(0);

    for (;;)
    {
	str = (struct __XmStringExtRec *)next;

	/* primitive type -- doesn't recurse */
	nlen = 0;

	if (str->len > XmSTRING_LENGTH)
	{

	    for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++)
	    {
		nlen <<= 8;
		nlen |= str->data[i];
		if (i > sizeof(unsigned))
		{
		    _XmWarning(NULL, "Invalid XmString\n");
		    _XmStringFree(intern);
		    return NULL;
		}
	    }
	}
	else
	{
	    i = 0;
	    nlen = str->len & ~XmSTRING_LENGTH;
	}

	switch (str->tag)
	{
	case XmSTRING_COMPONENT_UNKNOWN:
	    _XmWarning(NULL, "UNKNOWN COMPONENT IN EXTERNAL STRING (0x%x)\n",
	    	str->tag);
	    nlen++;
	    break;

	case XmSTRING_COMPONENT_CHARSET:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components - 1]->type =
		XmSTRING_COMPONENT_CHARSET;

	    charset = XtMalloc(nlen + 1);
	    memcpy(charset, &str->data[i], nlen);
	    charset[nlen] = 0;
	    if (_XmStringIsCurrentCharset(charset))
	    {
		intern->components[intern->number_of_components - 1]->data =
		    XtNewString(XmFONTLIST_DEFAULT_TAG);
		intern->components[intern->number_of_components - 1]->length =
		    strlen(XmFONTLIST_DEFAULT_TAG);
		XtFree(charset);
	    }
	    else
	    {
		intern->components[intern->number_of_components - 1]->data =
		    charset;
		intern->components[intern->number_of_components - 1]->length =
		    nlen;
	    }

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_TEXT:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components - 1]->type =
		XmSTRING_COMPONENT_TEXT;
	    intern->components[intern->number_of_components - 1]->length =
		nlen;
	    intern->components[intern->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    memcpy(intern->components[intern->number_of_components - 1]->data,
   	           &str->data[i],
		  nlen);
	    intern->components[intern->number_of_components - 1]->data[nlen] = 0;

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_WIDECHAR_TEXT:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components - 1]->type =
		XmSTRING_COMPONENT_WIDECHAR_TEXT;
	    intern->components[intern->number_of_components - 1]->length =
		nlen;
	    intern->components[intern->number_of_components - 1]->data =
		XtMalloc(nlen + sizeof(unicode_t));
	    memcpy(intern->components[intern->number_of_components - 1]->data,
   	           &str->data[i],
		  nlen);
	    /* It takes two bytes of zeros to null-terminate a widechar string */
	    intern->components[intern->number_of_components - 1]->data[nlen] = 0;
	    intern->components[intern->number_of_components - 1]->data[nlen+1] = 0;

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components - 1]->type =
		XmSTRING_COMPONENT_DIRECTION;
	    intern->components[intern->number_of_components - 1]->length =
		1;
	    intern->components[intern->number_of_components - 1]->data =
		XtMalloc(1);
	    intern->components[intern->number_of_components - 1]->data[0] =
		str->data[0];

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components - 1]->type =
		XmSTRING_COMPONENT_SEPARATOR;
	    intern->components[intern->number_of_components - 1]->length =
		0;
	    intern->components[intern->number_of_components - 1]->data = NULL;

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    __XmGrowXmString(intern);

	    intern->components[intern->number_of_components - 1]->type =
		XmSTRING_COMPONENT_LOCALE_TEXT;
	    intern->components[intern->number_of_components - 1]->length =
		nlen;
	    intern->components[intern->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    memcpy(intern->components[intern->number_of_components - 1]->data,
 	          &str->data[i],
		  nlen);
	    intern->components[intern->number_of_components - 1]->data[nlen] = 0;

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_RENDITION_BEGIN:
	case XmSTRING_COMPONENT_RENDITION_END:
		__XmGrowXmString(intern);
		intern->components[intern->number_of_components - 1]->type =
			str->tag;
		intern->components[intern->number_of_components - 1]->length =
			nlen;
		intern->components[intern->number_of_components - 1]->data =
			XtMalloc(nlen + 1);
		memcpy(intern->components[intern->number_of_components - 1]->data,
			&str->data[i],
			nlen);
		intern->components[intern->number_of_components - 1]->data[nlen] = 0;

		nlen += i + ASN1_HEADER_SIZE;
		break;
	case XmSTRING_COMPONENT_LOCALE:
	case XmSTRING_COMPONENT_LAYOUT_PUSH:
	case XmSTRING_COMPONENT_LAYOUT_POP:
		break;
	case XmSTRING_COMPONENT_TAB:
	    __XmGrowXmString(intern);
	    intern->components[intern->number_of_components - 1]->type = XmSTRING_COMPONENT_TAB;
	    intern->components[intern->number_of_components - 1]->length = 0;
	    intern->components[intern->number_of_components - 1]->data = NULL;

	    nlen += i + ASN1_HEADER_SIZE;
	    break;

	default:
	    _XmWarning(NULL, "XmString has invalid tag: %02x\n", str->tag);
	    nlen = 1;
	}

	next += nlen;
	if (length < nlen)
	{
	    _XmWarning(NULL, "XmString is malformed\n");
	    _XmStringFree(intern);
	    return NULL;
	}

	length -= nlen;
	if (length == 0)
	{
	    return intern;
	}

    }
}

static XmString
__XmStringToASN1(_XmString string)
{
    XmString external;
    struct __XmStringExtRec *str;
    int totlen, i, j, nlen, added, tlen;
    char *tmp;

    if (!string)
    {
	return NULL;
    }

    totlen = 0;
    for (i = 0; i < string->number_of_components; i++)
    {
	switch (string->components[i]->type)
	{
	case XmSTRING_COMPONENT_CHARSET:
	    if (strcmp(string->components[i]->data,
		       XmFONTLIST_DEFAULT_TAG) == 0)
	    {
		nlen = strlen(_XmStringGetCurrentCharset());
	    }
	    else
	    {
		nlen = string->components[i]->length;
	    }

	    totlen += nlen + ASN1_HEADER_SIZE;

	    if (nlen >= XmSTRING_LENGTH)
	    {
		nlen = string->components[i]->length;
		while (nlen)
		{
		    totlen++;
		    nlen >>= 8;
		}
	    }
	    break;

	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	case XmSTRING_COMPONENT_WIDECHAR_TEXT:
	    totlen += string->components[i]->length + ASN1_HEADER_SIZE;
	    if (string->components[i]->length >= XmSTRING_LENGTH)
	    {
		nlen = string->components[i]->length;
		while (nlen)
		{
		    totlen++;
		    nlen >>= 8;
		}
	    }
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    totlen += 3;
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    totlen += ASN1_HEADER_SIZE;
	    break;

	case XmSTRING_COMPONENT_UNKNOWN:
	    _XmWarning(NULL, "UNKNOWN COMPONENT IN INTERNAL STRING\n");
	    break;

	case XmSTRING_COMPONENT_RENDITION_BEGIN:
	case XmSTRING_COMPONENT_RENDITION_END:
		totlen += string->components[i]->length + ASN1_HEADER_SIZE;
		if (string->components[i]->length >= XmSTRING_LENGTH) {
			nlen = string->components[i]->length;
			while (nlen) {
				totlen++;
				nlen >>= 8;
			}
		}
		break;
	case XmSTRING_COMPONENT_LOCALE:
	case XmSTRING_COMPONENT_LAYOUT_PUSH:
	case XmSTRING_COMPONENT_LAYOUT_POP:
		break;
	case XmSTRING_COMPONENT_TAB:
		totlen += ASN1_HEADER_SIZE;
		break;
	}
    }

    added = 0;
    /* additional space for length data in extralong strings */
    if (totlen >= XmSTRING_LENGTH)
    {
	nlen = totlen;
	while (nlen)
	{
	    added++;
	    nlen >>= 8;
	}
    }

    /* add four bytes for the standard header */
    external = (unsigned char *)XtMalloc(totlen + XmSTRING_HEADER_SIZE + added);

    /* standard header and overall length */
    str = (struct __XmStringExtRec *)external;
    str->tag = XmSTRING_TAG;
    str->len = XmSTRING_LENGTH;
    str = (struct __XmStringExtRec *)str->data;

    str->tag = XmSTRING_COMPONENT_XMSTRING;
    if (totlen >= XmSTRING_LENGTH)
    {
	str->len = XmSTRING_LENGTH;
	nlen = totlen;
	j = 0;
	while (totlen)
	{
	    totlen >>= 8;
	    j++;
	}

	while (totlen)
	{
	}

	str->len += j;
	for (i = j - 1; i >= 0; i--)
	{
	    str->data[i] = nlen & 0x00FFU;
	    nlen >>= 8;
	}
	str = (struct __XmStringExtRec *)&str->data[j];
    }
    else
    {
	str->len = totlen;
	str = (struct __XmStringExtRec *)str->data;
    }

    for (i = 0; i < string->number_of_components; i++)
    {
	switch (string->components[i]->type)
	{
	case XmSTRING_COMPONENT_CHARSET:

	    str->tag = XmSTRING_COMPONENT_CHARSET;

	    if (strcmp(string->components[i]->data,
		       XmFONTLIST_DEFAULT_TAG) == 0)
	    {
		tmp = _XmStringGetCurrentCharset();
		nlen = strlen(tmp);
	    }
	    else
	    {
		tmp = string->components[i]->data;
		nlen = string->components[i]->length;
	    }

	    if (nlen >= XmSTRING_LENGTH)
	    {
		tlen = nlen;
		totlen = 0;
		while (tlen)
		{
		    totlen++;
		    tlen >>= 8;
		}
		str->len = XmSTRING_LENGTH + totlen;
		tlen = nlen;
		for (j = totlen - 1; j >= 0; j--)
		{
		    str->data[j] = tlen & 0x00FFU;
		    tlen >>= 8;
		}
		memcpy(&str->data[totlen], tmp,
		      nlen);
		str = (struct __XmStringExtRec *)&str->data[totlen + nlen];
	    }
	    else
	    {
		str->len = nlen;
		memcpy(str->data, tmp, nlen);
		str = (struct __XmStringExtRec *)&str->data[nlen];
	    }
	    break;

	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	case XmSTRING_COMPONENT_WIDECHAR_TEXT:
	    /* LOCALE_TEXT had its own case in this switch, but the only
	     * difference was the setting of str->tag, which is easily
	     * replaced by string->components[i]->type, making this case
	     * usable for multiple types -- dwilliss - 13-May-04  
	     */
	    str->tag = string->components[i]->type;
	    if (string->components[i]->length >= XmSTRING_LENGTH)
	    {
		nlen = string->components[i]->length;
		totlen = 0;
		while (nlen)
		{
		    totlen++;
		    nlen >>= 8;
		}
		str->len = XmSTRING_LENGTH + totlen;
		nlen = string->components[i]->length;
		for (j = totlen - 1; j >= 0; j--)
		{
		    str->data[j] = nlen & 0x00FFU;
		    nlen >>= 8;
		}
		memcpy(&str->data[totlen],
 		      string->components[i]->data,
		      string->components[i]->length);
		str = (struct __XmStringExtRec *)&str->data[totlen +
						string->components[i]->length];
	    }
	    else
	    {
		str->len = string->components[i]->length;
		memcpy(str->data,
 		       string->components[i]->data,
		       string->components[i]->length);
		str = (struct __XmStringExtRec *)&str->data[string->components[i]->length];
	    }
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    str->tag = XmSTRING_COMPONENT_DIRECTION;
	    str->len = 1;
	    str->data[0] = string->components[i]->data[0];
	    str = (struct __XmStringExtRec *)&str->data[1];
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    str->tag = XmSTRING_COMPONENT_SEPARATOR;
	    str->len = 0;
	    str = (struct __XmStringExtRec *)str->data;
	    break;

	case XmSTRING_COMPONENT_UNKNOWN:
	default:
	    _XmWarning(NULL, "UNKNOWN COMPONENT IN INTERNAL STRING\n");
	    break;
	case XmSTRING_COMPONENT_RENDITION_BEGIN:
#if 0
	    str->tag = XmSTRING_COMPONENT_RENDITION_BEGIN;
	    str->len = strlen(str->data);
	    str = (struct __XmStringExtRec *)
	    	(((char *)str->data)+ str->len + 1);
	    break;
#endif
	case XmSTRING_COMPONENT_RENDITION_END:
#if 0
	    str->tag = XmSTRING_COMPONENT_RENDITION_END;
	    str->len = strlen(str->data);
	    str = (struct __XmStringExtRec *)
	    	(((char *)str->data)+ str->len + 1);
#endif
	    str->tag = string->components[i]->type;
	    if (string->components[i]->length >= XmSTRING_LENGTH)
	    {
		nlen = string->components[i]->length;
		totlen = 0;
		while (nlen)
		{
		    totlen++;
		    nlen >>= 8;
		}
		str->len = XmSTRING_LENGTH + totlen;
		nlen = string->components[i]->length;
		for (j = totlen - 1; j >= 0; j--)
		{
		    str->data[j] = nlen & 0x00FFU;
		    nlen >>= 8;
		}
		memcpy(&str->data[totlen],
 		      string->components[i]->data,
		      string->components[i]->length);
		str = (struct __XmStringExtRec *)&str->data[totlen +
						string->components[i]->length];
	    }
	    else
	    {
		str->len = string->components[i]->length;
		memcpy(str->data,
 		       string->components[i]->data,
		       string->components[i]->length);
		str = (struct __XmStringExtRec *)&str->data[string->components[i]->length];
	    }
	    break;
	case XmSTRING_COMPONENT_LOCALE:
	case XmSTRING_COMPONENT_LAYOUT_PUSH:
	case XmSTRING_COMPONENT_LAYOUT_POP:
		break;
	case XmSTRING_COMPONENT_TAB:
		str->tag = XmSTRING_COMPONENT_TAB;
		str->len = 0;
		str = (struct __XmStringExtRec *)str->data;
		break;
	}
    }

    return external;
}

static Boolean
__XmStringSegmentExtent(XmFontList flist, _XmStringComponent comp,
			Dimension *width, Dimension *height,
			Dimension *ascent, Dimension *descent)
{
    int dc, amax = 0, dmax = 0;
    XCharStruct ov;
    XRectangle ink, log;

    *height = 0;
    *width = 0;
    *ascent = 0;
    *descent = 0;

    if ((comp->type != XmSTRING_COMPONENT_TEXT &&
	 comp->type != XmSTRING_COMPONENT_WIDECHAR_TEXT &&
	 comp->type != XmSTRING_COMPONENT_LOCALE_TEXT) ||
	comp->font == XmUNSPECIFIED)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "__XmStringSegmentExtent: got NULL Font/bad text\n"));

	return False;
    }
    if (!flist)
    {
	_XmWarning(NULL, "__XmStringSegmentExtent: got NULL FontList");
	return False;
    }

    switch (flist->renditions[comp->font]->type) {
    case XmFONT_IS_FONT:
	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmStringSegmentExtent(%p, XmFONT_IS_FONT)\n", flist));
	{
	    XFontStruct *fontstruct = (XFontStruct *)flist->renditions[comp->font]->font;

	    if (fontstruct) {
		    if (fontstruct->min_byte1 || fontstruct->max_byte1) {
			/*
			 * This also happens in XTextExtents
			 * *width = XTextWidth16(fontstruct,
			 *		      (XChar2b *)comp->data, comp->length / 2);
			 */
			XTextExtents16(fontstruct,
				       (XChar2b *)comp->data, comp->length / 2,
				       &dc, &amax, &dmax, &ov);
			*width = ov.width;
		    } else {
			/*
			 * This also happens in XTextExtents
			 * *width = XTextWidth(fontstruct, comp->data, comp->length);
			 */

			XTextExtents(fontstruct,
				     comp->data, comp->length,
				     &dc, &amax, &dmax, &ov);
			*width = ov.width;

			DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmStringSegmentExtent: '%s' len %d\n",
				comp->data, comp->length));
		    }

		    *height = amax + dmax;
		    *ascent = amax;
		    *descent = dmax;
	    } else {
		    /* FIX ME */
	    }
	}
	break;

    case XmFONT_IS_FONTSET:
	DEBUGOUT(_LtDebug(__FILE__, NULL,
		"_XmStringSegmentExtent(%p,XmFONT_IS_FONTSET)\n", flist));

	XmbTextExtents((XFontSet)flist->renditions[comp->font]->font,
		       comp->data, comp->length, &ink, &log);

	*height = log.height;
	*width = log.width;
	*ascent = -log.y;
	*descent = log.height+log.y;
	break;

    case XmFONT_IS_XFT:
#if	USE_XFT
	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmStringSegmentExtent(%p,XmFONT_IS_XFT)\n", flist));

	*ascent = flist->renditions[comp->font]->xft_font->ascent;
	*descent = flist->renditions[comp->font]->xft_font->descent;
	*height = flist->renditions[comp->font]->xft_font->height;
    {
	XGlyphInfo	info;
	XftTextExtents8(flist->dpy,
		flist->renditions[comp->font]->xft_font,
		(unsigned char*)comp->data, comp->length,
		&info);
	*width = info.width;

#if 1
/* Don't use this ! */
	*height = info.height;
#endif
    }
    	*height = flist->renditions[comp->font]->font_average_height;
#endif
	break;
    case XmFONT_IS_XOC:
#if	USE_BIDI
	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmStringSegmentExtent(%p,XmFONT_IS_XOC)\n", flist));

	/* FIX ME */
#endif
	break;
    }

    DEBUGOUT(_LtDebug(__FILE__, NULL,
	"_XmStringSegmentExtent => width %d height %d\n",
	*width, *height));

    return True;
}

/**************************** INTERNAL FUNCTIONS ***************************/

extern XFontStruct *
_XmGetFirstFont(XmFontListEntry entry)
{
    XFontStruct **flist;
    char **fnames;
    int num;

    if (entry == NULL)
    {
	return NULL;
    }

    if (entry->type == XmFONT_IS_FONT)
    {
	return (XFontStruct *)entry->font;
    }
    else if (entry->type == XmFONT_IS_FONTSET)
    {
	num = XFontsOfFontSet((XFontSet)entry->font, &flist, &fnames);
	if (num)
	{
	    return flist[0];
	}
    }

    return NULL;
}


extern Boolean
_XmFontListGetDefaultFont(XmFontList fontlist,
			  XFontStruct **font_struct)
{
    int i;

    if (fontlist == NULL) {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "_XmFontListGetDefaultFont(NULL, ...)\n"));
	*font_struct = NULL;
	return False;
    }

    for (i = 0; fontlist->renditions[i]->tag != NULL; i++) {
	if (!strcmp(XmFONTLIST_DEFAULT_TAG, fontlist->renditions[i]->tag)) {
	    break;
	}
    }

    /* Default to first entry */
    if (fontlist->renditions[i]->tag == NULL)
	i = 0;

    if (fontlist->renditions[i]->type == XmFONT_IS_FONT) {
	if ((*font_struct = (XFontStruct *)fontlist->renditions[i]->font))
		return True;
	else
		return False;
    } else if (fontlist->renditions[i]->type == XmFONT_IS_FONTSET) {
	if ((*font_struct = _XmGetFirstFont(fontlist->renditions[i])))
		return True;
	else
		return False;
    }

    *font_struct = NULL;
    return False;
}


extern Boolean
_XmFontListSearch(XmFontList fontlist,
		  XmStringCharSet charset,
		  short *indx,
		  XFontStruct **font_struct)
{
    int i, p;

    if (fontlist == NULL || charset == NULL) {
	*indx = 0;
	*font_struct = NULL;
	return False;
    }

    p = INT_MAX;
    for (i = 0; i < fontlist->count; i++) {
	if (!strcmp(charset, fontlist->renditions[i]->tag))
	{
		p = i;
	}
    }
    if (p != INT_MAX) i = p;

/*  if (fontlist->renditions[i]->tag == NULL && */
    /* What if the fontset has a general catch-all */
    if (p == INT_MAX && strcmp(charset, XmFONTLIST_DEFAULT_TAG) != 0) {
	for (i = 0; i < fontlist->count; i++) {
	    if (!strcmp(XmFONTLIST_DEFAULT_TAG, fontlist->renditions[i]->tag)) {
	    	p = i;
		break;
	    }
	}
    }
#if 0
    /* Try the other way around : what if the charset is the catch-all */
    else if (p == INT_MAX && strcmp(charset, XmFONTLIST_DEFAULT_TAG) == 0) {
	    /* Cut the crap, take the first entry */
	    p = 0;
    }
#endif

    if (p == INT_MAX) {
	*indx = XmUNSPECIFIED;
	*font_struct = NULL;
	return False;
    } else
	i = p;

    *indx = i;
    if (fontlist->renditions[i]->type == XmFONT_IS_FONT) {
	if ((*font_struct = (XFontStruct *)fontlist->renditions[i]->font))
		return True;
	else
		return False;
    } else if (fontlist->renditions[i]->type == XmFONT_IS_FONTSET) {
	if ((*font_struct = _XmGetFirstFont(fontlist->renditions[i])))
		return True;
	else
		return False;
    }

    *font_struct = NULL;
    return False;
}

/*
 * See the difference between _XmString and XmString (internal vs.
 *      external representation).
 *
 * Returns True if the argument is external representation.
 */
extern Boolean
_XmStringIsXmString(XmString string)
{
    struct __XmStringExtRec *str = (struct __XmStringExtRec *)string;
    unsigned char *next;

    if (!string)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "_XmStringIsXmString: string is NULL\n"));

	return False;
    }
    if (str->tag != XmSTRING_TAG || str->len != XmSTRING_LENGTH)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "_XmStringIsXmString: tag or len is invalid\n"));

	return False;
    }
    next = str->data;
    str = (struct __XmStringExtRec *)next;

    if (str->tag != XmSTRING_COMPONENT_XMSTRING)
    {
	return False;
    }

    return True;
}


extern Boolean
_XmStringEmpty(_XmString string)
{
    int i;

    if (string == NULL || string->number_of_components == 0)
    {
	return True;
    }

    for (i = 0; i < string->number_of_components; i++)
    {
	if ((string->components[i]->type == XmSTRING_COMPONENT_TEXT ||
	     string->components[i]->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
	     string->components[i]->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
	    string->components[i]->length != 0)
	{
	    return False;
	}
    }

    return True;
}


/*   Helper function for XmStringIsVoid().
 *   CheckMe !!
 */
static Boolean
_XmStringIsVoid(_XmString string)
{
    int i;

    if (string == NULL || string->number_of_components == 0)
    {
	return True;
    }

    for (i = 0; i < string->number_of_components; i++)
    {
	if ( (string->components[i]->type == XmSTRING_COMPONENT_TEXT ||
	      string->components[i]->type == XmSTRING_COMPONENT_LOCALE_TEXT ||
	      string->components[i]->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
	      string->components[i]->type == XmSTRING_COMPONENT_SEPARATOR ||
	      string->components[i]->type == XmSTRING_COMPONENT_TAB) &&
	    string->components[i]->length != 0)
	{
	    return False;
	}
    }

    return True;
}

extern _XmString
_XmStringCreate(XmString cs)
{
    if (!cs)
    {
	return NULL;
    }

    return __XmStringFromASN1(cs);
}


extern void
_XmStringFree(_XmString string)
{
    int i;

    if (string == NULL)
	return;

    for (i = 0; i < string->number_of_components; i++)
    {
       /* Peter Stein, 07/2000 */                                                       
       /* in some wired circumstances this may not be the case */                       
       /* leading to a crash */                                                         
	if (string->components[i] && string->components[i]->data /*&& string->components[i]->length*/)
	{
	    /* rws 30 Aug 1998
	       If we only free this if the length is greater than 0 we end
	       up not freeing the space from the string "" which has a length
	       of 0 but there has been space allocated for it.
	     */
	    XtFree((char *)string->components[i]->data);
	}

	XtFree((char *)string->components[i]);
    }
                                                                                                  
    if( string->number_of_components > 0 )                                                  
      {                                                                                       
       /* Peter Stein, 07/2000: */                                                      
       /* initialisation in __XmAllocNewXmString() */                                   
       /*      only done in this case */                                                
       /* so free should also only happen than*/                                        
         XtFree((char *)string->components);                                              
      }
    XtFree((char *)string);
}


extern char *
_XmStringGetCurrentCharset(void)
{
    char *lang, *p;

    if ((lang = getenv("LANG")) == NULL)
    {
	return XmFALLBACK_CHARSET;
    }

    if (strcmp(lang, "C") == 0 ||
	strcmp(lang, "POSIX") == 0 ||
	strcmp(lang, "ISO8859-1") == 0 ||
	strcmp(lang, "ISO-8859-1") == 0)
    {
	return XmSTRING_OS_CHARSET;
    }

    if ((p = strchr(lang, '.')) != NULL && *(p+1) != 0) {
	return p+1;
    }

    return XmFALLBACK_CHARSET;
}


extern char *
_XmCharsetCanonicalize(String charset)
{
    return NULL;
}

/* amai: we don't know whether this matches precisely the 
   Motif version of that private call, but it seems to be
   there, and so we export our version */
extern void
_XmStringUpdate(XmFontList fontlist, _XmString string)
{
    short i, where, def, tf;
    XFontStruct *dummy;

    if (!_XmFontListSearch(fontlist, XmFONTLIST_DEFAULT_TAG, &def, &dummy)) {
	tf = XmUNSPECIFIED;
    } else {
	tf = def;
    }

    if (def == XmUNSPECIFIED) {
	_XmFontListSearch(fontlist, fontlist->renditions[0]->tag, &def, &dummy);
    }

    for (i = 0; i < string->number_of_components; i++) {
	if (string->components[i]->type == XmSTRING_COMPONENT_CHARSET) {
	    if (_XmFontListSearch(fontlist,
				  string->components[i]->data,
				  &where,
				  &dummy)) {
		tf = where;
	    }
	}
	if (string->components[i]->type == XmSTRING_COMPONENT_LOCALE_TEXT) {
	    string->components[i]->font = def;
	}

	if (string->components[i]->type == XmSTRING_COMPONENT_TEXT
	|| string->components[i]->type == XmSTRING_COMPONENT_WIDECHAR_TEXT) {
	    if (tf != XmUNSPECIFIED) {
		string->components[i]->font = tf;
	    } else {
		string->components[i]->font = def;
	    }
	}
    }
}


extern _XmString
_XmStringCopy(_XmString s)
{
    _XmString newString;
    int i;

    if (s == NULL)
    {
	return NULL;
    }

    newString = __XmAllocNewXmString(s->number_of_components);

    for (i = 0; i < newString->number_of_components; i++)
    {
	__XmStringComponentCopy(newString->components[i], s->components[i]);
    }

    return newString;
}


extern Boolean
_XmStringByteCompare(_XmString a, _XmString b)
{
    _XmStringContext context1 = NULL;
    _XmStringComponent foo1;
    _XmStringContext context2 = NULL;
    _XmStringComponent foo2;

    if (!_XmStringInitContext(&context1, a))
    {
	return False;
    }

    if (!_XmStringInitContext(&context2, b))
    {
	_XmStringFreeContext(context1);
	return False;
    }

    while ((foo1 = __XmStringGetNextComponent(context1)))
    {
	if (!(foo2 = __XmStringGetNextComponent(context2)))
	{
	    _XmStringFreeContext(context1);
	    _XmStringFreeContext(context2);
	    return False;
	}

	if (foo1->type == XmSTRING_COMPONENT_SEPARATOR)
	{
	    if (foo2->type == XmSTRING_COMPONENT_SEPARATOR)
	    {
		continue;
	    }
	    else
	    {
		_XmStringFreeContext(context1);
		_XmStringFreeContext(context2);
		return False;
	    }
	}
	else if (foo2->type == XmSTRING_COMPONENT_SEPARATOR)
	{
	    _XmStringFreeContext(context1);
	    _XmStringFreeContext(context2);
	    return False;
	}

	if (strcmp(foo1->data, foo2->data))
	{
	    _XmStringFreeContext(context1);
	    _XmStringFreeContext(context2);
	    return False;
	}
    }

    _XmStringFreeContext(context1);
    _XmStringFreeContext(context2);

    return True;
}


extern Boolean
_XmStringHasSubstring(_XmString string, _XmString substring)
{
    int i, j, k;
    Boolean match;

    /* if both empty, match */
    if (_XmStringEmpty(string) && _XmStringEmpty(substring))
    {
	return True;
    }

    /* if first string is empty, never match */
    if (_XmStringEmpty(string))
    {
	return False;
    }

    /* if second empty, match */
    if (_XmStringEmpty(substring))
    {
	return True;
    }

    for (i = 0; i < string->number_of_components; i++)
    {
	if (string->components[i]->type == substring->components[0]->type &&
	    string->components[i]->length >= substring->components[0]->length &&
	    ((string->components[i]->data != NULL &&
	      substring->components[0]->data != NULL &&
               strstr( string->components[i]->data,
                       substring->components[0]->data ) != NULL) ||
	     (string->components[i]->data == NULL &&
	      substring->components[0] == NULL)))
	{
	    match = True;

	    for (j = i + 1, k = 1; k < substring->number_of_components; j++, k++)
	    {
		if (string->components[j]->type !=
		    substring->components[k]->type ||
		    string->components[j]->length <
		    substring->components[k]->length ||
		    ((string->components[j]->data != NULL &&
		      substring->components[k]->data != NULL &&
		      strstr(string->components[j]->data,
			      substring->components[k]->data) == NULL) ||
		     (string->components[j]->data != NULL &&
		      substring->components[k] == NULL) ||
		     (string->components[j]->data == NULL &&
		      substring->components[k] != NULL)))
		{
		    match = False;
		    break;
		}
	    }

	    if (match)
	    {
		return True;
	    }
	}
    }

    return False;
}


extern XmString
_XmStringCreateExternal(XmFontList fontlist, _XmString cs)
{
    /* need to change the string maybe, so that if a font doesn't exist
     * it changes to default? */
    if (fontlist)
    {
	_XmStringUpdate(fontlist, cs);
    }

    return __XmStringToASN1(cs);
}


extern char *
_XmStringGetTextConcat(XmString string)
{
    int ilen = 0, len = 0;
    char *ret = NULL;
    _XmString str;
    _XmStringComponent comp;
    _XmStringContext context = NULL;

    str = _XmStringCreate(string);

    _XmStringInitContext(&context, str);

    while ((comp = __XmStringGetNextComponent(context)) != NULL)
    {
	/* Cannot be made to work for WIDECHAR_TEXT since it returns char* */
	if (comp->type == XmSTRING_COMPONENT_TEXT ||
	    comp->type == XmSTRING_COMPONENT_LOCALE_TEXT)
	{
	    if (len == 0)
	    {
		len = comp->length;
		ret = XtMalloc(len + 1);
	    }
	    else
	    {
		len = comp->length;
		ret = XtRealloc(ret, ilen + len + 1);
	    }
	    memcpy(&ret[ilen], comp->data, len);
	    ret[ilen + len] = 0;
	    ilen += len;
	}
    }

    _XmStringFreeContext(context);

    _XmStringFree(str);

    return ret;
}


extern Boolean
_XmStringIsCurrentCharset(XmStringCharSet c)
{
    if (!c)
    {
	return False;
    }

    /* returns true if this charset is in the current locale */
    if (strcmp(c, _XmStringGetCurrentCharset()) == 0)
       return True;
    else  
       return False;
}


extern Boolean
_XmStringSingleSegment(XmString str, char **pTextOut,
		       XmStringCharSet *pCharsetOut)
{
    XmStringContext context;
    Boolean ret = False;
    XmStringDirection ddir;
    Boolean dsep;

    if (!XmStringInitContext(&context, str))
    {
	return False;
    }

    ret = XmStringGetNextSegment(context, pTextOut, pCharsetOut, &ddir, &dsep);

    XmStringFreeContext(context);

    return ret;
}

/*
 * In Motif 2.x this becomes XmeSetWMShellTitle and even gets documented!
 * (this function, apparently, has also been name XmeStringUpdateWMShellTitle
 * as well, sigh).
 *
 * If "xmstr" consists of a single segment with the "ISO8859-1" tag, then
 * the title and icon name will be set to the text of that segment and
 * the encoding will be set to STRING; If "xmstr" consists of a single
 * segment with the default font tag, then we do the same except the
 * encoding is set to None.  Otherwise, we convert to compound text and
 * set the encoding to COMPOUND_TEXT.
 *
 * FIX ME - deal with the font tags as per the above spec.
 */

extern void
_XmStringUpdateWMShellTitle(XmString xmstr, Widget shell)
{
    String title = NULL;
    Atom encoding;
    Boolean freeit;
    Arg a[4];
    int n;

    if (XmStringGetLtoR(xmstr, XmFONTLIST_DEFAULT_TAG, &title)
    &&  title != NULL)
    {
	freeit   = True;
	encoding = None;
    }
    else
    {
	title    = "";
	freeit   = False;
	encoding = None;
    }

    n = 0;
    XtSetArg(a[n], XmNtitle,            title);    ++n;
    XtSetArg(a[n], XmNtitleEncoding,    encoding); ++n;
    XtSetArg(a[n], XmNiconName,         title);    ++n;
    XtSetArg(a[n], XmNiconNameEncoding, encoding); ++n;
    XtSetValues(shell, a, n);

    if (freeit)
    {
	XtFree(title);
    }
}


extern Boolean
_XmStringInitContext(_XmStringContext *context, _XmString string)
{
    _XmStringContext p;

    if (string && context)
    {
	p = (_XmStringContext)XtMalloc(sizeof(struct __XmStringContextRec));

	p->string = string;
	p->current_component = -1;

	*context = p;
	return True;
    }
    else
    {
	return False;
    }
}


extern Boolean
_XmStringGetNextSegment(_XmStringContext context,
			XmStringCharSet *tag,
			XmStringDirection *direction,
			char **text,
			short *char_count,
			Boolean *separator)
{
    Boolean valid = False;
    XmStringComponentType type = XmSTRING_COMPONENT_UNKNOWN;
    char *ntext;
    XmStringCharSet ntag;
    XmStringDirection ndir;
    _XmStringComponent next;

    /*
     * note that tag is not zero'd.  That's because tag values need
     * to persist across calls to this func, unless a new tag is seen.
     */
    if (text)
    {
	*text = NULL;
    }
    if (direction)
    { /* like in original motif */
	*direction = XmSTRING_DIRECTION_L_TO_R;
    }
    if (separator)
    {
	*separator = False;
    }
    ntext = NULL;
    ntag = NULL;
    ndir = XmSTRING_DIRECTION_DEFAULT;

    while ((next = __XmStringGetNextComponent(context)) != NULL)
    {
	valid = True;
	type = next->type;

	switch (type)
	{
	case XmSTRING_COMPONENT_CHARSET:
	    ntag = next->data;
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    ndir = next->data[0];
	    break;

	case XmSTRING_COMPONENT_SEPARATOR:
	    break;

	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	case XmSTRING_COMPONENT_WIDECHAR_TEXT:
	    ntext = next->data;
	    *char_count = next->length;
	    break;

	case XmSTRING_COMPONENT_UNKNOWN:
	default:
	    break;
	}

	if (ntext != NULL && text)
	{
	    *text = ntext;
	}
	if (ntag != NULL && tag)
	{
	    *tag = ntag;
	}
	if (ndir != XmSTRING_DIRECTION_DEFAULT && direction)
	{
	    *direction = ndir;
	}

	next = __XmStringPeekNextComponent(context);
	if (next)
	{
	    type = next->type;

	    if (type == XmSTRING_COMPONENT_SEPARATOR)
	    {
		*separator = True;
		break;
	    }

	    if ((type == XmSTRING_COMPONENT_LOCALE_TEXT ||
		 type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
		 type == XmSTRING_COMPONENT_TEXT) && text && *text)
	    {
		break;
	    }

	    if (type == XmSTRING_COMPONENT_CHARSET && tag && *tag)
	    {
		break;
	    }

	    if (type == XmSTRING_COMPONENT_DIRECTION && direction &&
		*direction != XmSTRING_DIRECTION_DEFAULT)
	    {
		break;
	    }
	}
	ntext = NULL;
	ntag = NULL;
	ndir = XmSTRING_DIRECTION_DEFAULT;
    }

    if (tag && !*tag)
    {
	*tag = XmFONTLIST_DEFAULT_TAG;
    }

    return valid;
}


extern void
_XmStringFreeContext(_XmStringContext context)
{
    XtFree((char *)context);
}

extern void
_XmStringExtent(XmFontList fontlist, _XmString string,
		Dimension *width, Dimension *height)
{
	_XmStringContext	context = NULL;
	Boolean			have_seg, have_line_height;
	Dimension		current_height = 0, current_width = 0;
	Dimension		line_height = 0, default_line_height;
	Dimension		amax, dmax, wd, ht;
	int			pending_newlines, i = 0;
	_XmStringComponent	comp;
	XmRendition		r, rend;
	XmRenderTable		rt;

	rt = XmRenderTableCopy(fontlist, 0, 0);
	rend = 0;

	if (!string) {
		*width = *height = 0;
		return;
	}

	DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmStringExtent start\n"));

	_XmStringUpdate(rt, string);
	_XmStringInitContext(&context, string);

	*height = *width = 0;

	have_seg = False;
	have_line_height = False;
	line_height = 0;
	default_line_height = 0;
	pending_newlines = 0;

	while ((comp = __XmStringGetNextComponent(context)) != NULL) {
		i++;	/* Count #iterations for debug */
		DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmStringExtent(%d): %s\n", i,
				_LtDebugXmStringComponentType2String(comp->type)));
		/* Determine which rendition to use */
		r = rend ? rend : rt->renditions[0];
		if (comp->type == XmSTRING_COMPONENT_TEXT ||
				comp->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
				comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) {
			if (__XmStringSegmentExtent(rt, comp,
					&wd, &ht, &amax, &dmax)) {
				have_seg = True;
				if (ht > line_height)
					line_height = ht;	/*amax + dmax; */
			}
			current_width += wd;

			DEBUGOUT(_LtDebug(__FILE__, NULL,
				"_XmStringExtent: text segment, line height %d\n", ht));
		} else if (comp->type == XmSTRING_COMPONENT_SEPARATOR) {
			DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmStringExtent: separator\n"));

			/* What if we don't have a height ... ? */
			if (line_height == 0) {
				if (default_line_height == 0) {
					/* Find a line height ! */
					_XmStringComponentRec	blank;
					_XmStringComponent blankcomp = &blank;

					blankcomp->type = XmSTRING_COMPONENT_TEXT;
					blankcomp->data = " ";
					blankcomp->length = 1;
					blankcomp->font = comp->font;

					__XmStringSegmentExtent(rt, blankcomp,
						&wd, &default_line_height, &amax, &dmax);

					DEBUGOUT(_LtDebug(__FILE__, NULL,
						"_XmStringExtent: separator found height %d\n",
						default_line_height));
				}
				line_height = default_line_height;
	    		}
			if (default_line_height == 0)
				default_line_height = line_height;
			/* What if we don't have a height ... ? (end) */

			if (*width < current_width) {
				*width = current_width;
			}

			if (!have_seg && !have_line_height) {
				pending_newlines++;
			} else if (have_seg && !have_line_height) {
				have_line_height = True;
				default_line_height = line_height;
				current_height += default_line_height * pending_newlines;
				pending_newlines = 0;
			} else if (have_seg) {
				default_line_height = line_height;
			}

			if (!have_seg && have_line_height) {
				current_height += default_line_height;

				DEBUGOUT(_LtDebug(__FILE__, NULL,
					"_XmStringExtent-separator: default_line_height %d added\n",
					default_line_height));
	    		} else {
				current_height += line_height;

				DEBUGOUT(_LtDebug(__FILE__, NULL,
					"_XmStringExtent-separator: line_height %d added\n",
					line_height));
	    		}

			current_width = 0;
			line_height = 0;
			have_seg = False;
		} else if (comp->type == XmSTRING_COMPONENT_TAB) {
			XmTabList	tl;
			int		i;
			Position	tabx;
			Position	prev;

			/* Look for a usable tab list */
			for (tl=NULL, i=0; i<rt->count; i++) {
				XmRendition	rr = rt->renditions[i];
				if (rr->tab_list && rr->tab_list != (XmTabList)XmAS_IS) {
					tl = rr->tab_list;
					break;
				}
			}

			for (tabx=0, i=0; tl && i<tl->count; i++) {
				Position	x = 0;
				Display		*d = r->dpy;	/* For code re-use */

				switch (tl->tabs[i]->units) {
				case XmINCHES:
					x = tl->tabs[i]->value * 25.4
						* DisplayWidth(d, 0)
						/ DisplayWidthMM(d, 0) ;
					break;
				case XmPIXELS:
					x = tl->tabs[i]->value;
					break;
				case Xm100TH_MILLIMETERS:
					x = tl->tabs[i]->value * 100
						* DisplayWidth(d, 0)
						/ DisplayWidthMM(d, 0) ;
					break;
				case Xm1000TH_INCHES:
					x = tl->tabs[i]->value * 1000 * 25.4
						* DisplayWidth(d, 0)
						/ DisplayWidthMM(d, 0) ;
					break;
				case XmCENTIMETERS:
					x = tl->tabs[i]->value
						* DisplayWidth(d, 0)
						/ DisplayWidthMM(d, 0)
						/ 10;
					break;
				case XmMILLIMETERS:
					x = tl->tabs[i]->value
						* DisplayWidth(d, 0)
						/ DisplayWidthMM(d, 0);
					break;
				case XmPOINTS:
					x = tl->tabs[i]->value * 72
						* DisplayWidth(d, 0)
						/ DisplayWidthMM(d, 0) ;
					break;
				case Xm100TH_POINTS:
					x = tl->tabs[i]->value * 7200
						* DisplayWidth(d, 0)
						/ DisplayWidthMM(d, 0) ;
					break;
				case XmFONT_UNITS:
				case Xm100TH_FONT_UNITS:
				default:
					_XmWarning(NULL,
						"Tab untreated unit type %d\n",
						r->tab_list->tabs[i]->units);
				}

				prev = tabx;
				if (tl->tabs[i]->offset_model == XmABSOLUTE) {
					tabx = x;
				} else {	/* XmRELATIVE */
					tabx += x;
				}
				if (prev < current_width && current_width < tabx) {
					current_width = tabx;
					continue;
				}
			}
		} else if (comp->type == XmSTRING_COMPONENT_RENDITION_BEGIN) {
			char	*tag;

			/* Get the rendition tag */
			tag = XtMalloc(comp->length + 1);
			memcpy(tag, comp->data, comp->length);
			tag[comp->length] = '\0';

			/* Get a rendition that matches the tag */
			rend = XmRenderTableGetRendition(rt, tag);

			if (rend == NULL) {
				_XmRenderTableFinaliseTag(NULL, rt, tag);
				rend = XmRenderTableGetRendition(rt, tag);
			}

			if (rend == NULL) {
				XmDisplayCallbackStruct	cbs;
				Widget	dw = XmGetXmDisplay(rt->dpy);
				XtCallCallbackList(dw,
					DisplayNoRenditionCB(dw),
					(XtPointer)&cbs);
			}

			/* Push it in front of the private rendertable */
			if (rend)
				rt = _XmRenderTablePushRendition(rt, rend);

			DEBUGOUT(_LtDebug(__FILE__, NULL,
				"_XmStringDraw Rendition(%s) -> %p\n",
				tag, rend));

			XmRenditionFree(rend);
			XtFree(tag);
			rend = NULL;
		
		} else if (comp->type == XmSTRING_COMPONENT_RENDITION_END) {
			char	*tag;

			/* Get the rendition tag */
			tag = XtMalloc(comp->length + 1);
			memcpy(tag, comp->data, comp->length);
			tag[comp->length] = '\0';

			/* Get a rendition that matches the tag */
			rend = XmRenderTableGetRendition(rt, tag);

			/* Pop it from the private rendertable */
			if (rend) {
				rt = _XmRenderTablePopRendition(rt, rend);
				XmRenditionFree(rend);
			}
			rend = 0;
			XtFree(tag);
		} else if (comp->type == XmSTRING_COMPONENT_CHARSET) {
			;	/* Nothing */
		} else if (comp->type == XmSTRING_COMPONENT_DIRECTION) {
			;	/* Nothing */
		} else {
			DEBUGOUT(_LtDebug(__FILE__, NULL,
				"_XmStringExtent: bad component type %d\n",
				comp->type));
		}
	}

	if (have_seg) {
		current_height += line_height;
	} else {
		current_height += default_line_height;
	}

	if (*height < current_height) {
		*height = current_height;
	}

	if (*width < current_width) {
		*width = current_width;
	}

	_XmStringFreeContext(context);

	XmRenderTableFree(rt);
}

extern Dimension
_XmStringWidth(XmFontList fontlist, _XmString string)
{
    Dimension width = 0, dontcare;

    _XmStringExtent(fontlist, string, &width, &dontcare);

    return width;
}


extern Dimension
_XmStringHeight(XmFontList fontlist, _XmString string)
{
    Dimension height = 0, dontcare;

    _XmStringExtent(fontlist, string, &dontcare, &height);

    return height;
}


extern Dimension
_XmStringBaseline(XmFontList fontlist, _XmString string)
{
    _XmStringContext context = NULL;
    Dimension current_baseline = 0, wd, ht, asc, desc;
    _XmStringComponent comp;

    if (!_XmStringInitContext(&context, string))
    {
	return (Dimension)0;
    }

    _XmStringUpdate(fontlist, string);

    while ((comp = __XmStringGetNextComponent(context)) != NULL)
    {
	if ((comp->type == XmSTRING_COMPONENT_TEXT ||
	     comp->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
	     comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
	    comp->font != XmUNSPECIFIED)
	{
	    __XmStringSegmentExtent(fontlist, comp, &wd, &ht, &asc, &desc);

	    if (asc > current_baseline)
	    {
		current_baseline = asc;
	    }
	}
	else if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
	{
	    break;
	}
    }

    _XmStringFreeContext(context);

    return current_baseline;
}


extern int
_XmStringLineCount(_XmString string)
{
    _XmStringContext context = NULL;
    char *text;
    char *tag=NULL;
    short len;
    XmStringDirection dir;
    Boolean sep;
    int lc;

    lc = 0;

    if (!_XmStringInitContext(&context, string))
    {
	return 0;
    }

    while (_XmStringGetNextSegment(context, &tag, &dir, &text, &len, &sep))
    {
	if (sep)
	{
	    lc++;
	}
    }
    lc++;

    _XmStringFreeContext(context);

    return lc;
}

/*
 * Motif 2.* version of _XmStringDraw().
 *
 * Two differences with the Motif 1.x case :
 *	fontlist is really a renderTable
 *	string can have rendition tags in it
 */
extern void
_XmStringDraw(Display *d, Window w,
		XmFontList fontlist,
		_XmString string,
		GC gc,
		Position x, Position y, Dimension width,
		unsigned char align, unsigned char lay_dir,
		XRectangle *clip)
{
	_XmStringContext	context = NULL;
	_XmStringComponent	comp;
	Position		current_y;
	Position		current_x = 0, tabx;
	Boolean			have_seg, have_line_height, clipped;
	Dimension		default_line_height;
	XRectangle		ink, log;
	int			pending_newlines, i, inc;
	char			*tag;
	XmRenderTable		ort, rt = NULL;
	XmRendition		r, rend = NULL;
	XmTabList		tl;
	GC			mygc;

	if (w == 0 || string == 0) {
		return;
	}
	if (!fontlist) {
		_XmWarning(NULL, "_XmStringDraw: got NULL FontList");
		return;
	}

	if (clip) {
		DEBUGOUT(_LtDebug(__FILE__, XtWindowToWidget(d, w),
			"_XmStringDraw x %d y %d wid %d clip x %d y %d wid %d ht %d\n",
			x, y, width,
			clip->x, clip->y, clip->width, clip->height));
	} else {
		DEBUGOUT(_LtDebug(__FILE__, XtWindowToWidget(d, w),
			"_XmStringDraw x %d y %d wid %d no-clip\n",
			x, y, width));
	}

	current_y = y;
	_XmStringUpdate(fontlist, string);
	_XmStringInitContext(&context, string);

	mygc = XCreateGC(d, w, 0, NULL);
	XCopyGC(d, gc, ~0, mygc);

	pending_newlines = 0;
	have_line_height = False;
	clipped = False;
	default_line_height = 0;

	rt = XmRenderTableCopy(fontlist, 0, 0);
	inc = 0;	/* This is the number to increment comp->font with
			 * because of the push()/pop() operations.
			 */

	/* repeat while there's at least one thing in the string */
	while (__XmStringPeekNextComponent(context) != NULL) {
		Dimension line_height, line_width, line_ascent, line_descent;
		Dimension seg_height, seg_width, seg_ascent, seg_descent;
		int start;

		start = context->current_component;

		line_width = line_height = 0;
		line_ascent = line_descent = 0;
		have_seg = False;

		/*
		 * first, calculate the line extents
		 */
		while ((comp = __XmStringGetNextComponent(context)) != NULL) {
			DEBUGOUT(_LtDebug(__FILE__, NULL, "_XmStringDraw[%d] type %d\n",
				context->current_component, comp->type));

			if ((comp->type == XmSTRING_COMPONENT_TEXT ||
				comp->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
				comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
				comp->font != XmUNSPECIFIED) {

				/* This uses comp->font so deal with 'inc' */
				comp->font += inc;
				__XmStringSegmentExtent(rt, comp,
					&seg_width, &seg_height,
					&seg_ascent, &seg_descent);
				comp->font -= inc;

				line_width += seg_width;
				if (seg_height > line_height) {
					line_height = seg_height;
				}
				if (seg_ascent > line_ascent) {
					line_ascent = seg_ascent;
				}
				if (seg_descent > line_descent) {
					line_descent = seg_descent;
				}

				have_seg = True;
	    		} else if (comp->type == XmSTRING_COMPONENT_SEPARATOR) {
				if (!have_seg && !have_line_height) {
					pending_newlines++;
				} else if (have_seg && !have_line_height) {
					default_line_height = line_ascent + line_descent;
					have_line_height = True;
					current_y += pending_newlines * default_line_height;
				} else if (have_seg) {
					default_line_height = line_ascent + line_descent;
				}
				break;
			} else {
				switch (comp->type) {
				case XmSTRING_COMPONENT_CHARSET:
					/*
					 * Treat charset as a rendition begin,
					 * which is mostly right except there's no end.
					 */
				case XmSTRING_COMPONENT_RENDITION_BEGIN:
					/* Get the rendition tag */
					tag = XtMalloc(comp->length + 1);
					memcpy(tag, comp->data, comp->length);
					tag[comp->length] = '\0';

					/* Get a rendition that matches the tag */
					rend = XmRenderTableGetRendition(rt, tag);

					if (rend == NULL) {
						_XmRenderTableFinaliseTag(NULL, rt, tag);
						rend = XmRenderTableGetRendition(rt, tag);
					}

					if (rend == NULL) {
						XmDisplayCallbackStruct	cbs;
						Widget	wid = XtWindowToWidget(d, w),
							dw = XmGetXmDisplay(d);
						XtCallCallbackList(wid,
							DisplayNoRenditionCB(dw),
							(XtPointer)&cbs);
					}

					/* Push it in front of the private rendertable */
					if (rend) {
						inc++;
						ort = rt;
						rt = _XmRenderTablePushRendition(ort, rend);
						XmRenderTableFree(ort);

					}

					DEBUGOUT(_LtDebug(__FILE__, NULL,
						"_XmStringDraw Rendition(%s) -> %p\n",
						tag, rend));

					XmRenditionFree(rend);
					rend = NULL;
					XtFree(tag);
					tag = NULL;
					break;
				case XmSTRING_COMPONENT_RENDITION_END:
					/* Get the rendition tag */
					tag = XtMalloc(comp->length + 1);
					memcpy(tag, comp->data, comp->length);
					tag[comp->length] = '\0';

					/* Get a rendition that matches the tag */
					rend = XmRenderTableGetRendition(rt, tag);

					/* Pop it from the private rendertable */
					if (rend) {
						inc--;
						ort = rt;
						rt = _XmRenderTablePopRendition(ort, rend);
						XmRenderTableFree(ort);
						XmRenditionFree(rend);
					}
					rend = 0;
					XtFree(tag);
					tag = NULL;
					break;
				case XmSTRING_COMPONENT_UNKNOWN:
				case XmSTRING_COMPONENT_DIRECTION:
				case XmSTRING_COMPONENT_LOCALE:
				case XmSTRING_COMPONENT_LAYOUT_PUSH:
				case XmSTRING_COMPONENT_LAYOUT_POP:
				case XmSTRING_COMPONENT_TAB:
				default:
					DEBUGOUT(_LtDebug(__FILE__, NULL,
						"_XmStringDraw component type %d\n",
						comp->type));
				}
			}
		}

		if (!have_seg && !have_line_height) {
			continue;
		} else if (!have_seg && have_line_height) {
			current_y += default_line_height;
			continue;
		} else {
			current_y += line_ascent;
		}

		/* Set clipping if necessary, or perhaps disregard the line entirely */
		if (clip) {
			if (current_y + line_descent <= clip->y ||
					current_y - line_ascent >= clip->y + clip->height) {
				current_y += line_descent;
				continue;
			}
			if (!clipped && (current_y - line_ascent < clip->y ||
					current_y + line_descent > clip->y + clip->height)) {
				XSetClipRectangles(d, mygc, 0, 0, clip, 1, YXBanded);
				clipped = True;
			}
		}

		context->current_component = start;

		/* now we handle the starting x of this line based on the alignment */
		switch (align) {
		case XmALIGNMENT_CENTER:
			current_x = x + (width - line_width + 1) / 2;
			break;
		case XmALIGNMENT_BEGINNING:
			current_x = x;
			break;
		case XmALIGNMENT_END:
			current_x = x + width - line_width;
			break;
		}

		while ((comp = __XmStringGetNextComponent(context)) != NULL) {
			/* Determine which rendition to use */
			r = rend ? rend : rt->renditions[0];

			if (comp->type == XmSTRING_COMPONENT_SEPARATOR) {
				break;
			/*
			 * Was handling COMPONENT_CHARSET above, why not here?
			 * Added COMPONENT_CHARSET below 13-May-04, dwilliss, MicroImages 
			 */
			} else if (comp->type == XmSTRING_COMPONENT_RENDITION_BEGIN ||
				   comp->type == XmSTRING_COMPONENT_CHARSET
				) {
				/* Get the rendition tag */
				tag = XtMalloc(comp->length + 1);
				memcpy(tag, comp->data, comp->length);
				tag[comp->length] = '\0';

				/* Get a rendition that matches the tag */
				rend = XmRenderTableGetRendition(rt, tag);

				if (rend == NULL) {
					_XmRenderTableFinaliseTag(NULL, rt, tag);
					rend = XmRenderTableGetRendition(rt, tag);
				}

				if (rend == NULL) {
					XmDisplayCallbackStruct	cbs;
					Widget	wid = XtWindowToWidget(d, w),
						dw = XmGetXmDisplay(d);
					XtCallCallbackList(wid,
						DisplayNoRenditionCB(dw),
						(XtPointer)&cbs);
				}

				/* Push it in front of the private rendertable */
				if (rend) {
					inc++;
					ort = rt;
					rt = _XmRenderTablePushRendition(ort, rend);
					XmRenderTableFree(ort);
				}

				XmRenditionFree(rend);
				rend = NULL;
				XtFree(tag);
				continue;
			} else if (comp->type == XmSTRING_COMPONENT_RENDITION_END) {
				/* Get the rendition tag */
				tag = XtMalloc(comp->length + 1);
				memcpy(tag, comp->data, comp->length);
				tag[comp->length] = '\0';

				/* Get a rendition that matches the tag */
				rend = XmRenderTableGetRendition(rt, tag);

				/* Pop it from the private rendertable */
				if (rend) {
					ort = rt;
					rt = _XmRenderTablePopRendition(ort, rend);
					inc--;
					XmRenditionFree(rend);
					XmRenderTableFree(ort);
				}
				rend = 0;
				XtFree(tag);
				continue;
			} else if (comp->type == XmSTRING_COMPONENT_TAB) {
				Position	prev;

				/* Look for a usable tab list */
				for (tl=NULL, i=0; i<rt->count; i++) {
					XmRendition	rr = rt->renditions[i];
					if (rr->tab_list && rr->tab_list != (XmTabList)XmAS_IS) {
						tl = rr->tab_list;
						break;
					}
				}

				for (tabx=0, i=0; tl && i<tl->count; i++) {
					Position	x = 0;

					switch (tl->tabs[i]->units) {
					/* The statements below are copy/pasted from
					 * a sequence higher in this file
					 */
					case XmINCHES:
						x = tl->tabs[i]->value * 25.4
							* DisplayWidth(d, 0)
							/ DisplayWidthMM(d, 0) ;
						break;
					case XmPIXELS:
						x = tl->tabs[i]->value;
						break;
					case Xm100TH_MILLIMETERS:
						x = tl->tabs[i]->value * 100
							* DisplayWidth(d, 0)
							/ DisplayWidthMM(d, 0) ;
						break;
					case Xm1000TH_INCHES:
						x = tl->tabs[i]->value * 1000 * 25.4
							* DisplayWidth(d, 0)
							/ DisplayWidthMM(d, 0) ;
						break;
					case XmCENTIMETERS:
						x = tl->tabs[i]->value
							* DisplayWidth(d, 0)
							/ DisplayWidthMM(d, 0)
							/ 10;
						break;
					case XmMILLIMETERS:
						x = tl->tabs[i]->value
							* DisplayWidth(d, 0)
							/ DisplayWidthMM(d, 0);
						break;
					case XmPOINTS:
						x = tl->tabs[i]->value * 72
							* DisplayWidth(d, 0)
							/ DisplayWidthMM(d, 0) ;
						break;
					case Xm100TH_POINTS:
						x = tl->tabs[i]->value * 7200
							* DisplayWidth(d, 0)
							/ DisplayWidthMM(d, 0) ;
						break;
					case XmFONT_UNITS:
					case Xm100TH_FONT_UNITS:
					default:
						_XmWarning(NULL,
							"Tab untreated unit type %d\n",
							r->tab_list->tabs[i]->units);
					}

					prev = tabx;
					if (tl->tabs[i]->offset_model == XmABSOLUTE) {
						tabx = x;
					} else {	/* XmRELATIVE */
						tabx += x;
					}

					if (prev < current_x && current_x < tabx) {
						current_x = tabx;
						continue;
					}
				}	/* end for */
				continue;
			} else if ((comp->type != XmSTRING_COMPONENT_TEXT &&
					comp->type != XmSTRING_COMPONENT_WIDECHAR_TEXT &&
					comp->type != XmSTRING_COMPONENT_LOCALE_TEXT) ||
					comp->font == XmUNSPECIFIED) {
				DEBUGOUT(_LtDebug(__FILE__, NULL,
					"_XmStringDraw untreated case 0x%2x\n", comp->type));
				continue;
			}

			switch (r->type) {
			case XmFONT_IS_FONT:
				comp->font += inc;
				seg_width = (((XFontStruct *)r->font)->min_byte1 |
				     ((XFontStruct *)r->font)->max_byte1)
				    ? XTextWidth16(((XFontStruct *)r->font),
						   (XChar2b *)comp->data, comp->length >> 1)
				    : XTextWidth(((XFontStruct *)r->font),
						 comp->data, comp->length);
				comp->font -= inc;
				break;

			case XmFONT_IS_FONTSET:
				comp->font += inc;
				XmbTextExtents((XFontSet)r->font,
					comp->data, comp->length, &ink, &log);
				/* need to do something with the extents of the font set here. */
				seg_width = log.width;
				comp->font -= inc;
				break;
#if 0
			case XmAS_IS:
/*				fprintf(stderr, "Yaw\n");	*/
#endif
			case XmFONT_IS_XFT:
#if	USE_XFT
			{
				XGlyphInfo	ext;

				if (fontlist->renditions[comp->font]->xft_font) {
					XftTextExtents8(d,
						fontlist->renditions[comp->font]->xft_font,
						(unsigned char*)comp->data,
						comp->length,
						&ext);
					seg_width = ext.width;
				} else {
					seg_width = (((XFontStruct *)r->font)->min_byte1 |
						((XFontStruct *)r->font)->max_byte1)
						? XTextWidth16(((XFontStruct *)r->font),
						(XChar2b *)comp->data, comp->length >> 1)
						: XTextWidth(((XFontStruct *)r->font),
						comp->data, comp->length);
				}
			}
				break;
#endif
			case XmFONT_IS_XOC:
#if	USE_BIDI
#endif
				break;
			}
#if 0
			/* Set clipping if necessary, or perhaps skip this segment */
			if (clip) {
				if (current_x + seg_width <= clip->x ||
					current_x >= clip->x + clip->width) {
					current_x += seg_width;
					continue;
				}
				if (!clipped && (current_x < clip->x ||
		    			current_x + seg_width > clip->x + clip->width)) {
					XSetClipRectangles(d, mygc, 0, 0, clip, 1, YXBanded);
					clipped = True;
				}
			}
#endif
			/* Set the new parameters from the rendition */
			if (r->rendition_foreground != (Pixel)XmAS_IS) {
				XSetForeground(d, mygc, r->rendition_foreground);
			}
			if (r->rendition_background != (Pixel)XmAS_IS) {
				XSetBackground(d, mygc, r->rendition_background);	
			}

			switch (r->type) {
			case XmFONT_IS_FONT:
				XSetFont(d, mygc, ((XFontStruct *)r->font)->fid);

				DEBUGOUT(_LtDebug(__FILE__, NULL,
					"_XmStringDraw fnt(%s) x %d y %d\n",
					r->font_name, current_x, current_y));

				if (((XFontStruct *)r->font)->min_byte1 |
					((XFontStruct *)r->font)->max_byte1)
				{
					XDrawString16(d, w, mygc,
						current_x, current_y,
						(XChar2b *)comp->data,
						comp->length >> 1);
				} else {
					XDrawString(d, w, mygc,
						current_x, current_y,
						comp->data,
						comp->length);
				}
				break;

			case XmFONT_IS_FONTSET:
				XmbDrawString(d, w, (XFontSet)r->font, mygc,
					current_x, current_y,
					comp->data, comp->length);
				break;
#if	USE_XFT
			case XmFONT_IS_XFT:
				_XmXftDrawString(d, w, r, 1, 
					current_x, current_y,
					comp->data, comp->length);
				break;
			case XmFONT_IS_XOC:
				break;
#endif
			}
			current_x += seg_width;
		}

		current_y += line_descent;
	}

    	_XmStringFreeContext(context);
	if (clipped) {
		XSetClipMask(d, mygc, None);
	}

	if (rend)
		XmRenditionFree(rend);
	XmRenderTableFree(rt);
	XFreeGC(d, mygc);
}

extern void
_XmStringDrawImage(Display *d, Window w,
		   XmFontList fontlist, _XmString string,
		   GC gc, Position x, Position y, Dimension width,
		   unsigned char align, unsigned char lay_dir,
		   XRectangle *clip)
{
    _XmStringContext context = NULL;
    _XmStringComponent comp;
    Position current_y;
    Position current_x = 0;
    Boolean have_seg, have_line_height, clipped;
    Dimension default_line_height;
    XRectangle ink, log;
    int pending_newlines;

    current_y = y;

    _XmStringUpdate(fontlist, string);
    _XmStringInitContext(&context, string);

    pending_newlines = 0;
    have_line_height = False;
    clipped = False;
    default_line_height = 0;

    /* repeat while there's at least one thing in the string */
    while (__XmStringPeekNextComponent(context) != NULL)
    {
	Dimension line_height, line_width, line_ascent, line_descent;
	Dimension seg_height, seg_width, seg_ascent, seg_descent;
	int start;

	start = context->current_component;

	line_width = line_height = 0;
	line_ascent = line_descent = 0;
	have_seg = False;

	/*
	 * first, calculate the line extents
	 */
	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if ((comp->type == XmSTRING_COMPONENT_TEXT ||
		 comp->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
		 comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
		comp->font != XmUNSPECIFIED)
	    {
		__XmStringSegmentExtent(fontlist, comp,
					&seg_width, &seg_height,
					&seg_ascent, &seg_descent);

		line_width += seg_width;
		if (seg_height > line_height)
		{
		    line_height = seg_height;
		}
		if (seg_ascent > line_ascent)
		{
		    line_ascent = seg_ascent;
		}
		if (seg_descent > line_descent)
		{
		    line_descent = seg_descent;
		}

		have_seg = True;
	    }

	    if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
	    {
		if (!have_seg && !have_line_height)
		{
		    pending_newlines++;
		}
		else if (have_seg && !have_line_height)
		{
		    default_line_height = line_ascent + line_descent;
		    have_line_height = True;
		    current_y += pending_newlines * default_line_height;
		}
		else if (have_seg)
		{
		    default_line_height = line_ascent + line_descent;
		}

		break;
	    }
	}

	if (!have_seg && !have_line_height)
	{
	    continue;
	}
	else if (!have_seg && have_line_height)
	{
	    current_y += default_line_height;
	    continue;
	}
	else
	{
	    current_y += line_ascent;
	}

	/* Set clipping if necessary, or perhaps disregard the line entirely */
	if (clip)
	{
	    if (current_y + line_descent <= clip->y ||
		current_y - line_ascent >= clip->y + clip->height)
	    {
		current_y += line_descent;
		continue;
	    }
	    if (!clipped && (current_y - line_ascent < clip->y ||
		current_y + line_descent > clip->y + clip->height))
	    {
		XSetClipRectangles(d, gc, 0, 0, clip, 1, YXBanded);
		clipped = True;
	    }
	}

	context->current_component = start;

	/* now we handle the starting x of this line based on the alignment */
	switch (align)
	{
	case XmALIGNMENT_CENTER:
	    current_x = x + (width - line_width + 1) / 2;
	    break;

	case XmALIGNMENT_BEGINNING:
	    current_x = x;
	    break;

	case XmALIGNMENT_END:
	    current_x = x + width - line_width;
	    break;
	}

	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
	    {
		break;
	    }
	    else if ((comp->type != XmSTRING_COMPONENT_TEXT &&
		      comp->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
		      comp->type != XmSTRING_COMPONENT_LOCALE_TEXT) ||
		     comp->font == XmUNSPECIFIED)
	    {
		continue;
	    }

	    switch (fontlist->renditions[comp->font]->type)
	    {
	    case XmFONT_IS_FONT:
		seg_width =
		    (((XFontStruct *)fontlist->renditions[comp->font]->font)->min_byte1 |
		     ((XFontStruct *)fontlist->renditions[comp->font]->font)->max_byte1)
		    ? XTextWidth16(((XFontStruct *)fontlist->renditions[comp->font]->font),
				   (XChar2b *)comp->data, comp->length >> 1)
		    : XTextWidth(((XFontStruct *)fontlist->renditions[comp->font]->font),
				 comp->data, comp->length);
		break;

	    case XmFONT_IS_FONTSET:
		XmbTextExtents((XFontSet)fontlist->renditions[comp->font]->font,
			       comp->data, comp->length, &ink, &log);
		/* need to do something with the extents of the font set here. */
		seg_width = log.width;
		break;
#if	USE_XFT
	    case XmFONT_IS_XFT:
	    	{
			XGlyphInfo	ext;

			XftTextExtents8(d,
				fontlist->renditions[comp->font]->xft_font,
				(unsigned char*)comp->data,
				comp->length,
				&ext);
			seg_width = ext.width;
		}
		break;
#endif
	    case XmFONT_IS_XOC:
#if	USE_BIDI
#endif
		break;
	    }

	    /* Set clipping if necessary, or perhaps skip this segment */
	    if (clip)
	    {
		if (current_x + seg_width <= clip->x ||
		    current_x >= clip->x + clip->width)
		{
		    current_x += seg_width;
		    continue;
		}
		if (!clipped && (current_x < clip->x ||
		    current_x + seg_width > clip->x + clip->width))
		{
		    XSetClipRectangles(d, gc, 0, 0, clip, 1, YXBanded);
		    clipped = True;
		}
	    }

	    switch (fontlist->renditions[comp->font]->type)
	    {
	    case XmFONT_IS_FONT:
		XSetFont(d, gc,
			 ((XFontStruct *)fontlist->renditions[comp->font]->font)->fid);
		if (((XFontStruct *)fontlist->renditions[comp->font]->font)->min_byte1 |
		    ((XFontStruct *)fontlist->renditions[comp->font]->font)->max_byte1)
		{
		    XDrawImageString16(d, w, gc, current_x, current_y,
				  (XChar2b *)comp->data, comp->length >> 1);
		}
		else
		{
		    XDrawImageString(d, w, gc, current_x, current_y,
				     comp->data, comp->length);
 		}
 		break;

	    case XmFONT_IS_FONTSET:
		XmbDrawImageString(d, w, (XFontSet)fontlist->renditions[comp->font]->font,
				   gc, current_x, current_y,
				   comp->data, comp->length);
		break;
#if	USE_XFT
	    case XmFONT_IS_XFT:
		_XmXftDrawString(d, w, fontlist->renditions[comp->font], 1, 
			current_x, current_y,
			comp->data, comp->length);
		break;
#endif
	    case XmFONT_IS_XOC:
#if	USE_BIDI
#endif
		break;
	    }

	    current_x += seg_width;
	}

	current_y += line_descent;
    }
    _XmStringFreeContext(context);
    if (clipped)
    {
	XSetClipMask(d, gc, None);
    }
}


extern void
_XmStringDrawUnderline(Display *d, Window w, XmFontList fontlist,
		       _XmString string, GC gc,
		       Position x, Position y, Dimension width,
		       unsigned char align, unsigned char lay_dir,
		       XRectangle *clip, _XmString underline)
{
    _XmStringContext context = NULL, und_c = NULL;
    _XmStringComponent comp, und;
    Position current_y;
    Position current_x = 0;
    int xo;
    char *p, *u;
    Boolean have_seg, have_line_height, clipped;
    Dimension default_line_height;
    XRectangle ink, log;
    int pending_newlines;

    if (w == 0)
    {
	return;
    }

    DEBUGOUT(_LtDebug(__FILE__, XtWindowToWidget(d, w),
		      "_XmStringDrawUnderline x %d y %d wid %d\n",
		      x, y, width));
    if (!underline)
    {
	DEBUGOUT(_LtDebug(__FILE__, XtWindowToWidget(d, w),
		       "_XmStringDrawUnderline, underlined string is NULL\n"));
    }

    /* Currently only works if "underline" is a single-segment LtoR string */
    _XmStringInitContext(&und_c, underline);
    while ((und = __XmStringGetNextComponent(und_c)) != NULL)
    {
	if ((und->type == XmSTRING_COMPONENT_TEXT ||
	     und->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
	     und->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
	    und->font != XmUNSPECIFIED)
	{
	    break;
	}
    }

    if (und->type == XmSTRING_COMPONENT_TEXT ||
	und->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
	und->type == XmSTRING_COMPONENT_LOCALE_TEXT)
    {
	u = und->data;
    }
    else
    {
	u = NULL;
    }

    current_y = y;

    _XmStringUpdate(fontlist, string);
    _XmStringInitContext(&context, string);

    pending_newlines = 0;
    have_line_height = False;
    clipped = False;
    default_line_height = 0;

    /* repeat while there's at least one thing in the string */
    while (__XmStringPeekNextComponent(context) != NULL)
    {
	Dimension line_height, line_width, line_ascent, line_descent;
	Dimension seg_height, seg_width, seg_ascent, seg_descent;
	int start;

	start = context->current_component;

	line_width = line_height = 0;
	line_ascent = line_descent = 0;
	have_seg = False;

	/*
	 * first, calculate the line extents
	 */
	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if ((comp->type == XmSTRING_COMPONENT_TEXT ||
		 comp->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
		 comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
		comp->font != XmUNSPECIFIED)
	    {
		__XmStringSegmentExtent(fontlist, comp,
					&seg_width, &seg_height,
					&seg_ascent, &seg_descent);

		line_width += seg_width;
		if (seg_height > line_height)
		{
		    line_height = seg_height;
		}
		if (seg_ascent > line_ascent)
		{
		    line_ascent = seg_ascent;
		}
		if (seg_descent > line_descent)
		{
		    line_descent = seg_descent;
		}

		have_seg = True;
	    }

	    if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
	    {
		if (!have_seg && !have_line_height)
		{
		    pending_newlines++;
		}
		else if (have_seg && !have_line_height)
		{
		    default_line_height = line_ascent + line_descent;
		    have_line_height = True;
		    current_y += pending_newlines * default_line_height;
		}
		else if (have_seg)
		{
		    default_line_height = line_ascent + line_descent;
		}

		break;
	    }
	}

	if (!have_seg && !have_line_height)
	{
	    continue;
	}
	else if (!have_seg && have_line_height)
	{
	    current_y += default_line_height;
	    continue;
	}
	else
	{
	    current_y += line_ascent;
	}

	/* Set clipping if necessary, or perhaps disregard the line entirely */
	if (clip)
	{
	    if (current_y + line_descent < clip->y ||
		current_y - line_ascent >= clip->y + clip->height)
	    {
		current_y += line_descent;
		continue;
	    }
	    if (!clipped && (current_y - line_ascent < clip->y ||
		current_y + line_descent >= clip->y + clip->height))
	    {
		XSetClipRectangles(d, gc, 0, 0, clip, 1, YXBanded);
		clipped = True;
	    }
	}

	context->current_component = start;

	/* now we handle the starting x of this line based on the alignment */
	switch (align)
	{
	case XmALIGNMENT_CENTER:
	    current_x = x + (width - line_width + 1) / 2;
	    break;

	case XmALIGNMENT_BEGINNING:
	    current_x = x;
	    break;

	case XmALIGNMENT_END:
	    current_x = x + width - line_width;
	    break;
	}

	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
	    {
		break;
	    }
	    else if ((comp->type != XmSTRING_COMPONENT_TEXT &&
		      comp->type != XmSTRING_COMPONENT_WIDECHAR_TEXT &&
		      comp->type != XmSTRING_COMPONENT_LOCALE_TEXT) ||
		     comp->font == XmUNSPECIFIED)
	    {
		continue;
	    }

	    switch (fontlist->renditions[comp->font]->type)
	    {
	    case XmFONT_IS_FONT:
		seg_width =
		    (((XFontStruct *)fontlist->renditions[comp->font]->font)->min_byte1 |
		     ((XFontStruct *)fontlist->renditions[comp->font]->font)->max_byte1)
		    ? XTextWidth16(((XFontStruct *)fontlist->renditions[comp->font]->font),
				   (XChar2b *)comp->data, comp->length >> 1)
		    : XTextWidth(((XFontStruct *)fontlist->renditions[comp->font]->font),
				 comp->data, comp->length);
		break;

	    case XmFONT_IS_FONTSET:
		XmbTextExtents((XFontSet)fontlist->renditions[comp->font]->font,
			       comp->data, comp->length, &ink, &log);
		/* need to do something with the extents of the font set here. */
		seg_width = log.width;
		break;
#if	USE_XFT
	    case XmFONT_IS_XFT:
	    	{
			XGlyphInfo	ext;

			XftTextExtents8(d,
				fontlist->renditions[comp->font]->xft_font,
				(unsigned char*)comp->data,
				comp->length,
				&ext);
			seg_width = ext.width;
		}
		break;
#endif
	    case XmFONT_IS_XOC:
#if	USE_BIDI
#endif
		break;
	    }

	    /* Set clipping if necessary, or perhaps skip this segment */
	    if (clip)
	    {
		if (current_x + seg_width <= clip->x ||
		    current_x >= clip->x + clip->width)
		{
		    current_x += seg_width;
		    continue;
		}
		if (!clipped && (current_x < clip->x ||
		    current_x + seg_width > clip->x + clip->width))
		{
		    XSetClipRectangles(d, gc, 0, 0, clip, 1, YXBanded);
		    clipped = True;
		}
	    }

	    switch (fontlist->renditions[comp->font]->type)
	    {
	    case XmFONT_IS_FONT:
		{
		    XFontStruct *fontstruct =
		    (XFontStruct *)fontlist->renditions[comp->font]->font;

		   Boolean is_wide;

		   if (!fontstruct) break;
		    /* modified by Codematic 11.01.1998 */

		   is_wide =
		    fontstruct->min_byte1 || fontstruct->max_byte1;

		    XSetFont(d, gc, fontstruct->fid);

		    if (is_wide)
		    {

			XDrawString16(d, w, gc, current_x, current_y,
				      (XChar2b *)comp->data, comp->length / 2);
		    }
		    else
		    {
			XDrawString(d, w, gc, current_x, current_y,
				    comp->data, comp->length);
		    }

		    /* Now *first* underline, then update current_x */

		    if (u && (p = strstr(comp->data, u)))
		    {		/* Underline */
			int ww, llen = p - comp->data;
			char *f = XtMalloc(llen + 1);

			strncpy(f, comp->data, llen);
			f[llen] = '\0';
			if (is_wide)
			{
			    xo = XTextWidth16(fontstruct, (XChar2b *)f,
					      llen / 2);
			}
			else
			{
			    xo = XTextWidth(fontstruct, f, llen);
			}
			XtFree(f);

			/* ww = width of underlined text */
			if (is_wide)
			{
			    ww = XTextWidth16(fontstruct, (XChar2b *)u,
					      strlen(u) / 2);
			}
			else
			{
			    ww = XTextWidth(fontstruct, u, strlen(u));
			}

			/*
			 * Subtracted 1 from the width to take the space
			 * between characters into account. Is there a
			 * decent way to do this ??  Danny 17/4/96
			 */
			XDrawLine(d, w, gc,
				  current_x + xo,
				  current_y + line_descent,
				  current_x + xo + ww - 1,
				  current_y + line_descent);
		    }
		}
		break;

	    case XmFONT_IS_FONTSET:
		XmbDrawImageString(d, w, (XFontSet)fontlist->renditions[comp->font]->font,
				   gc, current_x, current_y,
				   comp->data, comp->length);

		/* Added by SG 08/08/1998
		 * Now *first* underline, then update current_x
		 */

		if (u && (p = strstr(comp->data, u))) {	/* Underline */
		    int ww, llen = p - comp->data;

		    XmbTextExtents((XFontSet)fontlist->renditions[comp->font]->font,
				   comp->data, llen, &ink, &log);
		    xo = log.width;

		    /* ww = width of underlined text */
		    XmbTextExtents((XFontSet)fontlist->renditions[comp->font]->font,
			       u, strlen(u), &ink, &log);
		    ww = log.width;

		    /*
		     * Subtracted 1 from the width to take the space
		     * between characters into account. Is there a
		     * decent way to do this ??  Danny 17/4/96
		     */
		    XDrawLine(d, w, gc,
			      current_x + xo,
			      current_y + line_descent,
			      current_x + xo + ww - 1,
			      current_y + line_descent);
		}
			break;

#if	USE_XFT
	    case XmFONT_IS_XFT:
		/* FIX ME need to underline */
		_XmXftDrawString(d, w, fontlist->renditions[comp->font], 1, 
			current_x, current_y,
			comp->data, comp->length);
		if (u && (p = strstr(comp->data, u))) {	/* Underline */
			int ww, llen = p - comp->data;
			XGlyphInfo	ext;

			XftTextExtents8(d,
				fontlist->renditions[comp->font]->xft_font,
				(unsigned char*)comp->data, llen,
				&ext);
			seg_width = ext.width;
#if 0
			XmbTextExtents((XFontSet)fontlist->renditions[comp->font]->font,
				comp->data, llen, &ink, &log);
#endif
			xo = ext.width;

			/* ww = width of underlined text */
#if 0
			XmbTextExtents((XFontSet)fontlist->renditions[comp->font]->font,
				u, strlen(u), &ink, &log);
#endif
			XftTextExtents8(d,
				fontlist->renditions[comp->font]->xft_font,
				(unsigned char*)u, strlen(u), &ext);
			ww = ext.width;

			/*
			 * Subtracted 1 from the width to take the space
			 * between characters into account. Is there a
			 * decent way to do this ??  Danny 17/4/96
			 */
			XDrawLine(d, w, gc,
				current_x + xo,
				current_y + line_descent,
				current_x + xo + ww - 1,
				current_y + line_descent);
		}
		break;
#endif
	    case XmFONT_IS_XOC:
#if	USE_BIDI
#endif
		break;
	    }

	    current_x += seg_width;
	}

	current_y += line_descent;
    }
    _XmStringFreeContext(und_c);
    _XmStringFreeContext(context);
    if (clipped)
    {
	XSetClipMask(d, gc, None);
    }
}


extern void
_XmStringDrawMnemonic(Display *d,
		      Window w,
		      XmFontList fontlist,
		      _XmString string,
		      GC gc,
		      Position x,
		      Position y,
		      Dimension width,
		      unsigned char alignment,
		      unsigned char layout_direction,
		      XRectangle *clip,
		      String mnemonic,
		      XmStringCharSet charset)
{
    XmString und;
    _XmString _und;

    DEBUGOUT(_LtDebug(__FILE__, XtWindowToWidget(d, w),
		      "_XmStringDrawMnemonic(%s, %s)\n",
		      _LtDebugXmString2String((XmString)string), mnemonic));

    if (mnemonic == NULL)
    {
	_XmStringDraw(d, w, fontlist, string, gc, x, y, width, alignment,
		      layout_direction, clip);
	return;
    }

    und = XmStringCreate(mnemonic, charset);
    _und = _XmStringCreate(und);

    _XmStringDrawUnderline(d, w, fontlist, string,
			   gc, x, y, width, alignment,
			   layout_direction, clip, _und);

    XmStringFree(und);
    _XmStringFree(_und);
}

/*
 * This is essentially _XmStringDraw without the drawing part.
 * It expects baselines to point to memory already allocated.
 */
extern void
_XmStringBaselines(XmFontList fontlist, _XmString string, Position y,
		   Dimension *baselines)
{
    _XmStringContext context = NULL;
    _XmStringComponent comp = NULL;
    Boolean have_seg, have_line_height;
    Dimension default_line_height, default_line_ascent;
    int pending_newlines;

    _XmStringUpdate(fontlist, string);
    _XmStringInitContext(&context, string);

    pending_newlines = 0;
    have_line_height = False;
    default_line_height = 0;
    default_line_ascent = 0;

    while (__XmStringPeekNextComponent(context) != NULL)
    {
	Dimension line_height, line_ascent, line_descent;
	Dimension seg_height, seg_width, seg_ascent, seg_descent;
	int start;

	start = context->current_component;

	line_height = 0;
	line_ascent = line_descent = 0;
	have_seg = False;

	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if ((comp->type == XmSTRING_COMPONENT_TEXT ||
		 comp->type == XmSTRING_COMPONENT_WIDECHAR_TEXT ||
		 comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
		comp->font != XmUNSPECIFIED)
	    {
		__XmStringSegmentExtent(fontlist, comp,
					&seg_width, &seg_height,
					&seg_ascent, &seg_descent);

		if (seg_height > line_height)
		{
		    line_height = seg_height;
		}
		if (seg_ascent > line_ascent)
		{
		    line_ascent = seg_ascent;
		}
		if (seg_descent > line_descent)
		{
		    line_descent = seg_descent;
		}

		have_seg = True;
	    }
	    else if (comp->type == XmSTRING_COMPONENT_SEPARATOR)
	    {
		if (!have_seg && !have_line_height)
		{
		    pending_newlines++;
		}
		else if (have_seg && !have_line_height)
		{
		    default_line_height = line_ascent + line_descent;
		    default_line_ascent = line_ascent;
		    have_line_height = True;
		    while (pending_newlines--)
		    {
			*baselines++ = y + default_line_ascent;
			y += default_line_height;
		    }
		}
		else if (have_seg)
		{
		    default_line_height = line_ascent + line_descent;
		    default_line_ascent = line_ascent;
		}
		break;
	    }
	}

	if (!have_seg && !have_line_height)
	{
	    continue;
	}
	else if (!have_seg && have_line_height)
	{
	    *baselines++ = y + default_line_ascent;
	    y += default_line_height;
	    continue;
	}
	else
	{
	    y += line_ascent;
	}

	*baselines++ = y;
	y += line_descent;
    }
    if (comp)
    {
	*baselines = have_line_height
	    ? y + default_line_ascent
	    : y;
    }
    _XmStringFreeContext(context);
}

/************************* PUBLIC FUNCTIONS *****************************/

extern Dimension
XmStringBaseline(XmFontList fontlist,
		 XmString string)
{
    Dimension baseline;
    _XmString str;

    if (!_XmStringIsXmString(string))
    {
	return 0;
    }

    str = _XmStringCreate(string);
    baseline = _XmStringBaseline(fontlist, str);
    _XmStringFree(str);

    return baseline;
}


extern Boolean
XmStringByteCompare(XmString s1,
		    XmString s2)
{
    _XmString str1, str2;
    Boolean ret;

    if (!_XmStringIsXmString(s1) || !_XmStringIsXmString(s2))
    {
	return False;
    }

    str1 = _XmStringCreate(s1);
    str2 = _XmStringCreate(s2);

    ret = _XmStringByteCompare(str1, str2);

    _XmStringFree(str1);
    _XmStringFree(str2);

    return ret;
}


extern Boolean
XmStringCompare(XmString s1, XmString s2)
{
 XmStringContext context1 = NULL, context2 = NULL;
 char *text1, *text2, *tag1, *tag2;
 Boolean separator1, separator2, compare_tags;
 XmStringDirection direction1, direction2;

 if (!_XmStringIsXmString(s1) || !_XmStringIsXmString(s2))
	return False;

 if (!XmStringInitContext(&context1, s1))
	return False;
 if (!XmStringInitContext(&context2, s2))
	{ XmStringFreeContext(context1); return False; }

 while (XmStringGetNextSegment(context1,
	 &text1, &tag1, &direction1, &separator1)) {
	if (!XmStringGetNextSegment(context2,
	      &text2, &tag2, &direction2, &separator2)) {
		XtFree(text1);
		XtFree(tag1);
		XmStringFreeContext(context1);
		XmStringFreeContext(context2);
		return False;
	}
	compare_tags = (strcmp(tag1, XmFONTLIST_DEFAULT_TAG) != 0 &&
			strcmp(tag2, XmFONTLIST_DEFAULT_TAG) != 0);
	 /* Don't compare tags when one of them = XmFONTLIST_DEFAULT_TAG */
	/*
	printf("%p %p\n>%s< >%s<\n%i %i\n%i %i\n", text1, text2, text1, text2, direction1, direction2, separator1, separator2);
	*/
	if (
	    ((text1 == NULL || text2 == NULL) && text1 != text2) 
	    || (text1 != text2 && strcmp(text1, text2) != 0) /* diferent text */
	    || direction1 != direction2
	    || (compare_tags && strcmp(tag1, tag2) != 0)
	    || separator1 != separator2
	   )
	{
		/*
		printf("Different\n");
		*/
		XtFree(text1);
		XtFree(text2);
		XtFree(tag1);
		XtFree(tag2);
		XmStringFreeContext(context1);
		XmStringFreeContext(context2);
		return False;
	}
		/*
		printf("Equal\n");
		*/
 }

 XtFree(text1);
 XtFree(text2);
 XtFree(tag1);
 XtFree(tag2);
 XmStringFreeContext(context1);
 XmStringFreeContext(context2);
 return True;
}


extern XmString
XmStringConcat(XmString str1, XmString str2)
{
    _XmString s1, s2;
    _XmString newString;
    int i, base, total;
    XmString ret;

#if 0
    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmStringConcat() entering\n"));
#endif
    if (!_XmStringIsXmString(str1) && !_XmStringIsXmString(str2))
    {
	return NULL;
    }
    else if (!_XmStringIsXmString(str1) && _XmStringIsXmString(str2))
    {
	return XmStringCopy(str2);
    }
    else if (_XmStringIsXmString(str1) && !_XmStringIsXmString(str2))
    {
	return XmStringCopy(str1);
    }

    s1 = _XmStringCreate(str1);
    s2 = _XmStringCreate(str2);

    if (s1)
    {
	total = s1->number_of_components;
    }
    else
    {
	total = 0;
    }
    base = total;
    total += s2->number_of_components;

    newString = __XmAllocNewXmString(total);

    if (s1)
    {
	for (i = 0; i < base; i++)
	{
	    __XmStringComponentCopy(newString->components[i],
				    s1->components[i]);
	}
    }

    for (i = 0; i < s2->number_of_components; i++)
    {
	__XmStringComponentCopy(newString->components[base + i],
				s2->components[i]);
    }

    _XmStringFree(s1);
    _XmStringFree(s2);

    ret = _XmStringCreateExternal(NULL, newString);

    _XmStringFree(newString);
#if 0
    DEBUGOUT(_LtDebug(__FILE__, NULL, "XmStringConcat() leaving\n"));
#endif
    return ret;
}


extern XmString
XmStringCopy(XmString s)
{
    _XmString s1;
    XmString ret;

    if (!_XmStringIsXmString(s))
    {
	return NULL;
    }

    s1 = _XmStringCreate(s);
    ret = _XmStringCreateExternal(NULL, s1);

    _XmStringFree(s1);

    return ret;
}


extern XmString
XmStringNConcat(XmString s1, XmString s2, int num_bytes)
{
    _XmString str1, str2;
    XmString ret;
    int i, nlen, tmp;

    if (!_XmStringIsXmString(s1) || !_XmStringIsXmString(s2))
    {
	return NULL;
    }

    str1 = _XmStringCreate(s1);
    str2 = _XmStringCreate(s2);

    i = 0;
    while (num_bytes && i < str2->number_of_components)
    {
	num_bytes -= ASN1_HEADER_SIZE;	/* header */
	if (num_bytes < 0)
	{
	    break;
	}

	if (num_bytes == 0)
	{
	    __XmGrowXmString(str1);
	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 0;
	    break;
	}

	if (str2->components[i]->type == XmSTRING_COMPONENT_SEPARATOR)
	{
	    __XmGrowXmString(str1);

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 0;

	    i++;
	    continue;
	}

	if (str2->components[i]->type == XmSTRING_COMPONENT_DIRECTION)
	{
	    num_bytes--;

	    __XmGrowXmString(str1);

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 1;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(1);
	    str1->components[str1->number_of_components - 1]->data[0] =
		str2->components[i]->data[0];

	    i++;
	    continue;
	}

	__XmGrowXmString(str1);
	nlen = str2->components[i]->length;

	if (num_bytes < XmSTRING_LENGTH && nlen >= XmSTRING_LENGTH)
	{
	    nlen = num_bytes;
	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = nlen;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    memcpy(str1->components[str1->number_of_components - 1]->data,
	           str2->components[i]->data,
		   nlen);
	    str1->components[str1->number_of_components - 1]->data[nlen] = 0;
	}
	else
	{
	    if (nlen > num_bytes)
	    {
		nlen = num_bytes;
	    }

	    if (nlen >= XmSTRING_LENGTH)
	    {
		for (tmp = nlen; tmp >= XmSTRING_LENGTH; tmp >>= 8)
		    num_bytes--;
	    }

	    if (nlen > num_bytes)
	    {
		nlen = num_bytes;
	    }

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = nlen;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    memcpy(str1->components[str1->number_of_components - 1]->data,
 	           str2->components[i]->data,
		   nlen);
	    str1->components[str1->number_of_components - 1]->data[nlen] = 0;
	}

	num_bytes -= nlen;
    }

    ret = _XmStringCreateExternal(NULL, str1);

    _XmStringFree(str1);
    _XmStringFree(str2);

    return ret;
}


extern XmString
XmStringNCopy(XmString s1, int num_bytes)
{
    _XmString str1, str2;
    XmString ret;
    int i, nlen, tmp;

    if (!_XmStringIsXmString(s1))
    {
	return NULL;
    }

    str1 = __XmAllocNewXmString(0);
    str2 = _XmStringCreate(s1);

    i = 0;
    while (num_bytes && i < str2->number_of_components)
    {
	num_bytes -= ASN1_HEADER_SIZE;	/* header */
	if (num_bytes < 0)
	{
	    break;
	}

	if (num_bytes == 0)
	{
	    __XmGrowXmString(str1);
	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 0;
	    break;
	}

	if (str2->components[i]->type == XmSTRING_COMPONENT_SEPARATOR)
	{
	    __XmGrowXmString(str1);

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 0;

	    i++;
	    continue;
	}

	if (str2->components[i]->type == XmSTRING_COMPONENT_DIRECTION)
	{
	    num_bytes--;

	    __XmGrowXmString(str1);

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = 1;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(1);
	    str1->components[str1->number_of_components - 1]->data[0] =
		str2->components[i]->data[0];

	    i++;
	    continue;
	}

	__XmGrowXmString(str1);
	nlen = str2->components[i]->length;

	if (num_bytes < XmSTRING_LENGTH && nlen >= XmSTRING_LENGTH)
	{
	    nlen = num_bytes;
	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = nlen;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    memcpy(str1->components[str1->number_of_components - 1]->data,
	           str2->components[i]->data,
		   nlen);
	    str1->components[str1->number_of_components - 1]->data[nlen] = 0;
	}
	else
	{
	    if (nlen > num_bytes)
	    {
		nlen = num_bytes;
	    }

	    if (nlen >= XmSTRING_LENGTH)
	    {
		for (tmp = nlen; tmp >= XmSTRING_LENGTH; tmp >>= 8)
		{
		    num_bytes--;
		}
	    }

	    if (nlen > num_bytes)
	    {
		nlen = num_bytes;
	    }

	    str1->components[str1->number_of_components - 1]->type =
		str2->components[i]->type;
	    str1->components[str1->number_of_components - 1]->length = nlen;
	    str1->components[str1->number_of_components - 1]->data =
		XtMalloc(nlen + 1);
	    memcpy(str1->components[str1->number_of_components - 1]->data,
	           str2->components[i]->data,
		   nlen);
	    str1->components[str1->number_of_components - 1]->data[nlen] = 0;
	}

	num_bytes -= nlen;
    }

    ret = _XmStringCreateExternal(NULL, str1);

    _XmStringFree(str1);
    _XmStringFree(str2);

    return ret;
}


extern XmString
XmStringSegmentCreate(char *text,
		      char *tag,
		      XmStringDirection direction,
		      Boolean separator)
{
    _XmString str = NULL;
    XmString ret;

    if (text && tag)
    {
	if (strcmp(tag, XmFONTLIST_DEFAULT_TAG) == 0 ||
	    strcmp(tag, XmSTRING_DEFAULT_CHARSET) == 0)
	{
	    str = __XmAllocNewXmString(2);

	    str->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
	    str->components[0]->length = strlen(text);
	    str->components[0]->data = XtMalloc(1);
	    str->components[0]->data[0] = direction;

	    str->components[1]->type = XmSTRING_COMPONENT_LOCALE_TEXT;
	    str->components[1]->length = strlen(text);
	    str->components[1]->data = XtNewString(text);
	}
	else
	{
	    str = __XmAllocNewXmString(3);

	    str->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
	    str->components[0]->length = strlen(text);
	    str->components[0]->data = XtMalloc(1);
	    str->components[0]->data[0] = direction;

	    str->components[1]->type = XmSTRING_COMPONENT_CHARSET;
	    str->components[1]->length = strlen(tag);
	    str->components[1]->data = XtNewString(tag);

	    if (strncasecmp(tag, "iso10646", 8) == 0)
	    {
		/* I'm not sure if this is the best way to determine if something
		 * is Unicode. It might be better to find the RenderTable entry
		 * for the tag and check the encoding of the font it refers to.
		 * If somebody comes up with a better solution, search for the
		 * other places where I compared tag to "iso10646" and change
		 * them too.	- 14-May-04 - dwilliss
		 */
		int length = _ucstrlen((unicode_t*)text);
		str->components[2]->type = XmSTRING_COMPONENT_WIDECHAR_TEXT;
		str->components[2]->length = length * sizeof(unicode_t); /* Is # of bytes */
		str->components[2]->data = (char*)XtMalloc(sizeof(unicode_t) * (length + 1));
		memcpy(str->components[2]->data, text, sizeof(unicode_t)*(length+1));
	    } else {
		str->components[2]->type = XmSTRING_COMPONENT_TEXT;
		str->components[2]->length = strlen(text);
		str->components[2]->data = XtNewString(text);
	    }
	}
    }
    else if (text)
    {
	str = __XmAllocNewXmString(2);

	str->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
	str->components[0]->length = 0;
	str->components[0]->data = XtMalloc(1);
	str->components[0]->data[0] = direction;

	str->components[1]->type = XmSTRING_COMPONENT_LOCALE_TEXT;
	str->components[1]->length = strlen(text);
	str->components[1]->data = XtNewString(text);
    }
    else
    {
	str = __XmAllocNewXmString(1);

	str->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
	str->components[0]->length = 0;
	str->components[0]->data = XtMalloc(1);
	str->components[0]->data[0] = direction;
    }

    if (separator)
    {
	__XmGrowXmString(str);
	str->components[str->number_of_components - 1]->type =
	    XmSTRING_COMPONENT_SEPARATOR;
	str->components[str->number_of_components - 1]->length = 0;
	str->components[str->number_of_components - 1]->data = 0;
    }

    ret = _XmStringCreateExternal(NULL, str);
    _XmStringFree(str);

    return ret;
}


extern XmString
XmStringSeparatorCreate(void)
{
    _XmString newString = __XmAllocNewXmString(1);
    XmString ret;

    newString->components[0]->type = XmSTRING_COMPONENT_SEPARATOR;
    newString->components[0]->length = 0;
    newString->components[0]->data = NULL;

    ret = _XmStringCreateExternal(NULL, newString);
    _XmStringFree(newString);

    return ret;
}


extern XmString
XmStringDirectionCreate(XmStringDirection direction)
{
    _XmString newString = __XmAllocNewXmString(1);
    XmString ret;

    newString->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
    newString->components[0]->length = 0;
    newString->components[0]->data = XtMalloc(1);
    newString->components[0]->data[0] = direction;

    ret = _XmStringCreateExternal(NULL, newString);
    _XmStringFree(newString);

    return ret;
}

/*
 * XmStringCreate creates a compound string with two components: text and a font list element tag.
 */
extern XmString
XmStringCreate(char *text, char *tag)
{
    _XmString str;
    XmString ret;

    if (text && tag && strcmp(tag, XmFONTLIST_DEFAULT_TAG) != 0) {
	str = __XmAllocNewXmString(2);

	str->components[0]->type = XmSTRING_COMPONENT_CHARSET;
	str->components[0]->length = strlen(tag);
	str->components[0]->data = XtNewString(tag);

	if (strncasecmp(tag, "iso10646", 8) == 0) {
	    int length = _ucstrlen((unicode_t*)text);
	    str->components[1]->type = XmSTRING_COMPONENT_WIDECHAR_TEXT;
	    str->components[1]->length = length * sizeof(unicode_t); /* Is # of bytes */
	    str->components[1]->data = (char*)XtMalloc((length + 1) * sizeof(unicode_t));
	    memcpy(str->components[1]->data, text, (length+1) * sizeof(unicode_t));
	} else {
	    str->components[1]->type = XmSTRING_COMPONENT_TEXT;
	    str->components[1]->length = strlen(text);
	    str->components[1]->data = XtNewString(text);
	}
    } else if (text) {
	str = __XmAllocNewXmString(1);

	str->components[0]->type = XmSTRING_COMPONENT_LOCALE_TEXT;
	str->components[0]->length = strlen(text);
	str->components[0]->data = XtNewString(text);
    } else {
	return NULL;
    }

    ret = _XmStringCreateExternal(NULL, str);
    _XmStringFree(str);

    return ret;
}

/*
 * This function is obsolete and exists for compatibility with previous releases.
 * It is replaced by XmStringGenerate.
 * XmStringCreateLtoR creates a compound string with two components: text and a tag component.
 * This function scans for \n  characters in the text. When one is found, the text up to that
 * point is put into a segment followed by a separator component. No final separator component
 * is appended to the end of the compound string. The direction component defaults to
 * left-to-right. This function assumes that the encoding is single byte rather than multibyte.
 */
extern XmString
XmStringCreateLtoR(char *text, char *tag)
{
	XmString	ret;
	_XmString	str;
	char		*t;
	char		*p, *b;

	if (text == NULL) {
		return NULL;
	}

	t = XtNewString(text);
	p = strstr(t, "\n");

	if (p) {	/* if there was actually a return character in the string. */
		Boolean at_end;

		if (*(p + 1)) {
			at_end = False;
		} else {
			at_end = True;
		}

		*p = 0;

		ret = XmStringSegmentCreate(t, tag, XmSTRING_DIRECTION_L_TO_R, True);

		str = _XmStringCreate(ret);

		XmStringFree(ret);

		while (!at_end) {
			Boolean need_text;
			Boolean need_sep;

			p++;
			b = p;

			if ((p = strstr(b, "\n")) == NULL) {
				at_end = True;
				need_sep = False;
			} else {
				*p = 0;
				need_sep = True;
				if (*(p + 1)) {
					at_end = False;
				} else {
					at_end = True;
				}
			}

			if ((p != NULL && p != b) || strlen(b) != 0) {
				need_text = True;
			} else {
				need_text = False;
			}

			if (need_text) {
				__XmGrowXmString(str);

				if (strcmp(tag, XmFONTLIST_DEFAULT_TAG) == 0) {
					str->components[str->number_of_components - 1]->type =
					XmSTRING_COMPONENT_LOCALE_TEXT;
					str->components[str->number_of_components - 1]->length =
					strlen(b);
					str->components[str->number_of_components - 1]->data = XtNewString(b);
				} else if (strncasecmp(tag, "iso10646", 8) == 0) {
					int length = _ucstrlen((unicode_t*)text);
					str->components[str->number_of_components - 1]->type =
					XmSTRING_COMPONENT_WIDECHAR_TEXT;
					str->components[str->number_of_components - 1]->length = length * sizeof(unicode_t); /* Is # of bytes */
					str->components[str->number_of_components - 1]->data = (char*)XtMalloc((length + 1) * sizeof(unicode_t));
					memcpy(str->components[str->number_of_components - 1]->data, text, (length+1) * sizeof(unicode_t));
				} else {
					str->components[str->number_of_components - 1]->type =
						XmSTRING_COMPONENT_TEXT;
					str->components[str->number_of_components - 1]->length =
						strlen(b);
					str->components[str->number_of_components - 1]->data = XtNewString(b);
				}

			}

			if (need_sep) {
				__XmGrowXmString(str);

				str->components[str->number_of_components - 1]->type =
					XmSTRING_COMPONENT_SEPARATOR;
				str->components[str->number_of_components - 1]->length = 0;
				str->components[str->number_of_components - 1]->data = NULL;
			}
		}

		ret = _XmStringCreateExternal(NULL, str);
		_XmStringFree(str);
	} else {
		ret = XmStringSegmentCreate(text, tag,
		XmSTRING_DIRECTION_L_TO_R, False);
	}

	XtFree(t);

	return ret;
}

extern XmString
XmStringLtoRCreate(char *text, char *tag)
{
	return XmStringCreateLtoR(text, tag);
}

/*
 * IMPORTANT NOTE
 * 	The documentation below says we should call XmStringCreate but this is not
 * 	the way OpenMotif 2.2 works. Its behaviour is as if we should call
 * 	XmStringCreateLtoR, which is why we do this.
 *
 * XmStringCreateLocalized creates a compound string containing the specified text
 * in the current language environment.
 * An identical compound string would result from the function XmStringCreate called
 * with XmFONTLIST_DEFAULT_TAG explicitly as the tag component.
 */
extern XmString
XmStringCreateLocalized(char *text)
{
	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmStringCreateLocalized(%s)\n", text));
	return XmStringGenerate(text, NULL, XmCHARSET_TEXT, NULL);
}

/*
 * XmStringCreateSimple creates a compound string with a text component and a charset tag.
 * It derives the character set from the current language environment.
 *
 * The routine attempts to derive a character set from the value of the LANG environment variable.
 * If this does not result in a valid character set, the routine uses a vendor-specific default.
 * If the vendor has not specified a different value, this default is ISO8859-1.
 */
extern XmString
XmStringCreateSimple(char *text)
{
    _XmString str;
    XmString ret;

    if (!text)
    {
	return NULL;
    }

    str = __XmAllocNewXmString(2);

    str->components[0]->type = XmSTRING_COMPONENT_CHARSET;
    str->components[0]->length = strlen(XmFONTLIST_DEFAULT_TAG);
    str->components[0]->data = XtNewString(XmFONTLIST_DEFAULT_TAG);

    str->components[1]->type = XmSTRING_COMPONENT_TEXT;
    str->components[1]->length = strlen(text);
    str->components[1]->data = XtNewString(text);

    ret = _XmStringCreateExternal(NULL, str);

    _XmStringFree(str);

    return ret;
}

/*
 * This is the default mapping table for XmStringGenerate().
 * Don't change the order !!
 */
static struct __XmParseMappingRec _dft_tbl[] = {
	{
		NULL,		/* client_data */
		XmINSERT,	/* include_status */
		NULL,		/* invoke_parse_proc */
		"\n",		/* pattern */
		XmCHARSET_TEXT,	/* pattern_type */
		NULL		/* substitute - initialized at run time */
	},
	{
		NULL,
		XmINSERT,
		NULL,
		"\t",
		XmCHARSET_TEXT,
		NULL
	},
};

static struct __XmParseMappingRec *_dft_ptr = &_dft_tbl[0];
static XmParseTable _XmStringGenerateDefaultTable = &_dft_ptr;
static int	_XmStringGenerateDefaultTableSize = XtNumber(_dft_tbl);

/*
 * XmStringGenerate
 *
 * calls the XmStringParseText function with a default parse table of entries
 * consisting of '\n', which maps to Separator, and '\t', which maps to Tab.
 * Matching RENDITION_BEGIN and RENDITION_END components containing rendition
 * are placed around the resulting XmString.
 *
 * text
 *	Specifies a NULL-terminated string containing characters of a type
 *	determined by type.
 *
 * tag
 *	Specifies the tag to be used in creating the result. The type of tag
 *	created (charset or locale) depends on the text type and the value
 *	given. If specified value is NULL, and type indicates that a charset
 *	tag should be created, then the tag will have the value of
 *	XmFONTLIST_DEFAULT_TAG. If tag is NULL, and type indicates a locale
 *	tag, then the tag will have the value of _MOTIF_DEFAULT_LOCALE.
 *
 * type
 *	Specifies the type of text to be passed in, and the tag type. If a
 *	locale tag should be created, then type has a value of either
 *	XmMULTIBYTE_TEXT or XmWIDECHAR_TEXT. If a charset should be created,
 *	type has a value of XmCHARSET_TEXT.
 *
 * rendition
 *	Specifies the rendition tag to be used in an
 *	XmSTRING_COMPONENT_RENDITION_BEGIN component which will begin the
 *	returned string and in an XmSTRING_COMPONENT_RENDITION_END component
 *	which will end it. If rendition is NULL, no rendition tag is placed.
 *
 * RETURN
 *	Returns a new compound string. The function will allocate space to
 *	hold the returned compound string. When the application no longer
 *	needs the returned compound string, the application should call
 *	XmStringFree.
 */
extern XmString
XmStringGenerate(XtPointer    text,
                 XmStringTag  tag,
                 XmTextType   type,
                 XmStringTag  rendition)
{
	XmString	pt, b, e, r;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmStringGenerate(%s,%s,_,%s)\n",
		text, tag, rendition));

	if (! _dft_tbl[0].substitute) {
		_dft_tbl[0].substitute = XmStringComponentCreate(XmSTRING_COMPONENT_SEPARATOR, 0, NULL);
		_dft_tbl[1].substitute = XmStringComponentCreate(XmSTRING_COMPONENT_TAB, 0, NULL);
	}

	if (tag == NULL) {
		if (type == XmCHARSET_TEXT)
			tag = XmFONTLIST_DEFAULT_TAG;
		else if (type == XmMULTIBYTE_TEXT || type == XmWIDECHAR_TEXT)
			tag = _MOTIF_DEFAULT_LOCALE;
		else {
			_XmWarning(NULL, "XmStringGenerate: invalid tag type\n");
			tag = XmFONTLIST_DEFAULT_TAG;
		}
	}

	pt = XmStringParseText(text,
		NULL,
		tag,
		type,
		_XmStringGenerateDefaultTable,
		_XmStringGenerateDefaultTableSize,
		NULL);

	if (rendition) {
		/* If we got a rendition passed to us, use it. */
		b = XmStringComponentCreate(XmSTRING_COMPONENT_RENDITION_BEGIN,
			strlen(rendition), rendition);
		e = XmStringComponentCreate(XmSTRING_COMPONENT_RENDITION_END,
			strlen(rendition), rendition);
		r = XmStringConcat(b, pt);
		XmStringFree(b);
		XmStringFree(pt);
		pt = XmStringConcat(r, e);
		XmStringFree(r);
		XmStringFree(e);
	}

	return pt;
}

/*
 * The Motif 2.1 documentation says that the GC can be left in an
 * "undefined" state when the XmString contains a font.
 */
extern void
XmStringDraw(Display *d,
	     Window w,
	     XmFontList fontlist,
	     XmString string,
	     GC gc,
	     Position x,
	     Position y,
	     Dimension width,
	     unsigned char alignment,
	     unsigned char layout_direction,
	     XRectangle *clip)
{
    _XmString str;

    if (!_XmStringIsXmString(string))
    {
	return;
    }

    str = _XmStringCreate(string);

    _XmStringDraw(d, w, fontlist, str, gc, x, y, width,
		  alignment, layout_direction, clip);

    _XmStringFree(str);
}


extern void
XmStringDrawImage(Display *d,
		  Window w,
		  XmFontList fontlist,
		  XmString string,
		  GC gc,
		  Position x,
		  Position y,
		  Dimension width,
		  unsigned char alignment,
		  unsigned char layout_direction,
		  XRectangle *clip)
{
    _XmString str;

    if (!_XmStringIsXmString(string))
    {
	return;
    }

    str = _XmStringCreate(string);

    _XmStringDrawImage(d, w, fontlist, str, gc, x, y, width,
		       alignment, layout_direction, clip);

    _XmStringFree(str);
}


/*
 * Similar to XmStringDraw, but with an additional parameter
 *
 * If the underline string is contained in string, then that part
 * of string is underlined.
 */
extern void
XmStringDrawUnderline(Display *d,
		      Window w,
		      XmFontList fontlist,
		      XmString string,
		      GC gc,
		      Position x,
		      Position y,
		      Dimension width,
		      unsigned char alignment,
		      unsigned char layout_direction,
		      XRectangle *clip,
		      XmString underline)
{
    _XmString str, und;

    if (!_XmStringIsXmString(string))
    {
	return;
    }


    str = _XmStringCreate(string);

    if (underline != NULL)
    {

	und = _XmStringCreate(underline);

	_XmStringDrawUnderline(d, w, fontlist, str, gc, x, y, width,
			       alignment, layout_direction, clip, und);

	/*  _XmStringFree(und); FIX ME Why is this commented? MLM */
    }
    else
    {
	_XmStringDraw(d, w, fontlist, str, gc, x, y, width,
		      alignment, layout_direction, clip);
    }
    _XmStringFree(str);
}


extern Boolean
XmStringEmpty(XmString s1)
{
    _XmString str;
    Boolean ret;

    if (!_XmStringIsXmString(s1))
    {
	return True;
    }

    str = _XmStringCreate(s1);

    ret = _XmStringEmpty(str);

    _XmStringFree(str);

    return ret;
}


extern void
XmStringExtent(XmFontList fontlist,
	       XmString string,
	       Dimension *width,
	       Dimension *height)
{
    _XmString str;

    *width = *height = 0;

    if (!_XmStringIsXmString(string))
    {
	return;
    }

    str = _XmStringCreate(string);

    _XmStringUpdate(fontlist, str);
    _XmStringExtent(fontlist, str, width, height);
    _XmStringFree(str);

    if (!string)
    {
	DEBUGOUT(_LtDebug(__FILE__, NULL,
			  "XmStringExtent() string is NULL\n"));
    }

}


/*
 * according to Motif, this does only return the first segment in a
 * multi-segment string
 */
extern Boolean
XmStringGetLtoR(XmString string,
		XmStringCharSet tag,
		char **text)
{
    Boolean Found = False;
    XmStringContext context = NULL;

    *text = NULL;
    if (!_XmStringIsXmString(string))
    {
	return False;
    }

    XmStringInitContext(&context, string);

    while (XmStringGetNextSegment(context, NULL, NULL, NULL, NULL))
    {
	if (context->text && context->charset && tag &&
	    strcmp(context->charset, tag) == 0)
	{
	    *text = XtNewString(context->text);
	    Found = True;
	    break;
	}

	/*
	 * Provide some kind of compatibility between 1.1's
	 * XmSTRING_DEFAULT_CHARSET and 1.2's XmFONTLIST_DEFAULT_TAG
	 */
	if (context->charset &&
	    strcmp(context->charset, XmFONTLIST_DEFAULT_TAG) == 0 &&
	    strcmp(tag, XmSTRING_DEFAULT_CHARSET) == 0)
	{
	    *text = XtNewString(context->text);
	    Found = True;
	    break;
	}

	if (context->charset &&
	    strcmp(tag, XmFONTLIST_DEFAULT_TAG) == 0 &&
	    strcmp(context->charset, XmSTRING_DEFAULT_CHARSET) == 0)
	{
	    *text = XtNewString(context->text);
	    Found = True;
	    break;
	}
    }

    XmStringFreeContext(context);
    return Found;
}


extern void
XmStringFree(XmString string)
{
    if (!_XmStringIsSpecified(string))
    {
	return;
    }

    if (!_XmStringIsXmString(string))
    {
	return;
    }

    XtFree((char *)string);
}


extern Boolean
XmStringInitContext(XmStringContext *context,
		    XmString string)
{
    if (!_XmStringIsXmString(string))
    {
	return False;
    }

    *context = (XmStringContext)XtCalloc(1, sizeof(struct _XmtStringContextRec));
    (*context)->string = _XmStringCreate(string);
    (*context)->current_component = -1;

    return True;
}


extern XmStringComponentType
XmStringGetNextComponent(XmStringContext context,
			 char **text,
			 XmStringCharSet *tag,
			 XmStringDirection *direction,
			 XmStringComponentType *unknown_tag,
			 unsigned short *unknown_length,
			 unsigned char **unknown_value)
{
    _XmStringComponent r;

    if (context == NULL)
    {
	return XmSTRING_COMPONENT_UNKNOWN;
    }

    context->current_component++;

    if (context->current_component < context->string->number_of_components)
    {
	r = context->string->components[context->current_component];

	switch (r->type)
	{
	case XmSTRING_COMPONENT_CHARSET:
	    if (r->data && tag)
	    {
		*tag = XtNewString(r->data);
	    }
	    break;

	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	    if (r->data && text)
	    {
		*text = XtNewString(r->data);
	    }
	    break;

	case XmSTRING_COMPONENT_WIDECHAR_TEXT:
	    if (r->data && text)
	    {	/* + 2 to account for 2-byte null terminator */
		/* length is bytes */
		*text = XtMalloc(r->length + sizeof(unicode_t));
		memcpy(*text, r->data, r->length + sizeof(unicode_t));
	    }
	    break;

	case XmSTRING_COMPONENT_DIRECTION:
	    if (direction)
		*direction = r->data[0];
	    break;

	case XmSTRING_COMPONENT_UNKNOWN:
	case XmSTRING_COMPONENT_END:
	case XmSTRING_COMPONENT_USER_BEGIN:
	case XmSTRING_COMPONENT_USER_END:
	    if (unknown_tag)
		*unknown_tag = r->type;
	    if (unknown_length)
		*unknown_length = r->length;
	    if (unknown_value) {
		*unknown_value = (unsigned char *)XtMalloc(r->length);
		memcpy(*unknown_value, r->data, r->length);
	    }
	    break;

	case XmSTRING_COMPONENT_RENDITION_BEGIN:
	case XmSTRING_COMPONENT_RENDITION_END:
	    if (r->data && tag) {
		*tag = XtNewString(r->data);
	    }
	    break;
	case XmSTRING_COMPONENT_LOCALE:
	case XmSTRING_COMPONENT_LAYOUT_PUSH:
	case XmSTRING_COMPONENT_LAYOUT_POP:
	case XmSTRING_COMPONENT_TAB:
		break;

	default:
	    _XmWarning(NULL,
		       "XmStringGetNextComponent: unknown type %d\n", r);
	}
	return r->type;
    }
    else
    {
	return XmSTRING_COMPONENT_END;
    }
}


extern XmStringComponentType
XmStringPeekNextComponent(XmStringContext context)
{
    if (context == NULL)
    {
	return XmSTRING_COMPONENT_UNKNOWN;
    }

    if (context->current_component < (context->string->number_of_components - 1))
    {
	return context->string->components[context->current_component + 1]->type;
    }
    else
    {
	return XmSTRING_COMPONENT_END;
    }
}


/*
 * Assumption: if you change charsets or directions before a separator,
 * then that is another segment.  I don't know if this is right, but it
 * makes sense? MLM
 */
extern Boolean
XmStringGetNextSegment(XmStringContext context,
		       char **text,
		       XmStringCharSet *tag,
		       XmStringDirection *direction,
		       Boolean *separator)
{
    Boolean ret;

    ret = _XmStringGetNextSegment((_XmStringContext)context,
				  &context->charset, &context->direction,
				  &context->text, &context->textlen,
				  &context->separator);

    if (!ret)
    {
	return False;
    }

    if (text)
    {
	/* XXX May not be the safest way to determine that string is Unicode */
        if (strncasecmp(context->charset, "iso10646", 8) == 0) {
	    int size = (_ucstrlen((unicode_t*)context->text) + 1) * 2;
	    void* ucstr = XtMalloc(size);
	    memcpy(ucstr, context->text, size);
	    *text = (char*)ucstr;
	} else {
	    *text = XtNewString(context->text);
    	}
    }

    if (tag)
    {
	*tag = XtNewString(context->charset);
    }

    if (direction)
    {
	*direction = context->direction;
    }

    if (separator)
    {
	*separator = context->separator;
    }

    return True;
}


extern void
XmStringFreeContext(XmStringContext context)
{
    _XmStringFree(context->string);

    XtFree((char *)context);
}


extern Boolean
XmStringHasSubstring(XmString string,
		     XmString substring)
{
    _XmString str, substr;
    Boolean ret;

    if (!_XmStringIsXmString(string) || !_XmStringIsXmString(substring))
    {
	return False;
    }

    str = _XmStringCreate(string);
    substr = _XmStringCreate(substring);

    ret = _XmStringHasSubstring(str, substr);

    _XmStringFree(str);
    _XmStringFree(substr);

    return ret;
}


extern Dimension
XmStringWidth(XmFontList fontlist,
	      XmString string)
{
    Dimension width, height;

    XmStringExtent(fontlist, string, &width, &height);

    return width;
}


extern Dimension
XmStringHeight(XmFontList fontlist, XmString string)
{
    Dimension width, height;

    XmStringExtent(fontlist, string, &width, &height);

    return height;
}


extern int
XmStringLength(XmString s1)
{
    unsigned length, i;
    struct __XmStringExtRec *str = (struct __XmStringExtRec *)s1;
    unsigned char *next;

    if (!_XmStringIsXmString(s1))
    {
	return 0;
    }

    /* skip header */
    next = str->data;
    str = (struct __XmStringExtRec *)next;
    length = 0;

    if (str->len > XmSTRING_LENGTH)
    {

	for (i = 0; i < (str->len & ~XmSTRING_LENGTH); i++)
	{
	    length <<= 8;
	    length |= str->data[i];
	    if (i > sizeof(unsigned))
	    {
		return 0;
	    }
	}
    }
    else
    {
	length = str->len & ~XmSTRING_LENGTH;
    }

    return length + XmSTRING_HEADER_SIZE;
}


extern int
XmStringLineCount(XmString string)
{
    _XmString str;
    int lc;

    if (!_XmStringIsXmString(string))
    {
	return 0;
    }

    str = _XmStringCreate(string);

    lc = _XmStringLineCount(str);

    _XmStringFree(str);

    return lc;
}


/*
 * This sucker appears to be undocumented.
 * Literal search in OM docs brings up a thread-safe variant
 * only 'XmStringCreateFontList_r' !?!
 */
extern XmFontList
XmStringCreateFontList( XFontStruct *font, XmStringCharSet charset)
{
	return XmFontListCreate(font, charset);
}


extern XmString
XmStringConcatAndFree(XmString str1, XmString str2)
{
  XmString tmp;
  tmp = XmStringConcat(str1, str2);
  XmStringFree(str1);
  XmStringFree(str2);
  return tmp;
}

/*
 * MxFTP uses this as
 *	item = (char *)XmStringUnparse(tempstr,NULL,XmCHARSET_TEXT,
 *		XmCHARSET_TEXT,NULL,0,XmOUTPUT_ALL);
 */
extern XtPointer
XmStringUnparse(XmString string, XmStringTag tag, XmTextType tag_type,
	XmTextType output_type, XmParseTable parse_table,
	Cardinal parse_count, XmParseModel parse_model)
{
    _XmStringContext	context = NULL;
    _XmStringComponent	comp = NULL;
    _XmString		_string;
    Boolean		have_seg, have_line_height;
    Dimension		default_line_height, default_line_ascent;
    int			pending_newlines;
    char		*res = NULL;

    if (tag != NULL) {
	_XmWarning(NULL, "XmStringUnparse currently only handles tag == NULL case");
    }

    _string = _XmStringCreate(string);
    _XmStringInitContext(&context, _string);

    pending_newlines = 0;
    have_line_height = False;
    default_line_height = 0;
    default_line_ascent = 0;

    DEBUGOUT(_LtDebug(__FILE__, NULL,
	"XmStringUnparse[internal] _string %p context %p\n",
	_string, context));

    while (__XmStringPeekNextComponent(context) != NULL)
    {
	Dimension line_height, line_ascent, line_descent;
	int start, reslen = 0;

	start = context->current_component;

	line_height = 0;
	line_ascent = line_descent = 0;
	have_seg = False;

	while ((comp = __XmStringGetNextComponent(context)) != NULL)
	{
	    if ((comp->type == XmSTRING_COMPONENT_TEXT ||
		 comp->type == XmSTRING_COMPONENT_LOCALE_TEXT) &&
		comp->font != XmUNSPECIFIED)
	    {
		DEBUGOUT(_LtDebug(__FILE__, NULL,
			"XmStringUnparse[internal] len %d '%s' font %d\n",
			comp->length, comp->data ? comp->data : "(null)",
			comp->font));
		if (comp->data) {
			if (res) {
				int oldlen = reslen;
				reslen += strlen(comp->data);
				res = (char *)realloc(res, reslen + 1);
				strcpy(res+oldlen, comp->data);
			} else {
				reslen = strlen(comp->data);
				res = (char *)malloc(reslen + 1);
				strcpy(res, comp->data);
			}
		}
			
	    }
	    else if (comp->type == XmSTRING_COMPONENT_WIDECHAR_TEXT)
	    {
		/* 2-byte data has to cope with 2-byte null terminator */
		if (comp->data) {
			if (res) {
				int oldlen = reslen;
				int newlen = _ucstrlen((unicode_t*)comp->data) * sizeof(unicode_t);
				reslen += newlen;
				res = (char *)realloc(res, reslen + sizeof(unicode_t));
				memcpy(res+oldlen, comp->data, newlen + sizeof(unicode_t));
			} else {
				reslen = _ucstrlen((unicode_t*)comp->data) * sizeof(unicode_t);
				res = (char *)malloc(reslen + sizeof(unicode_t));
				memcpy(res, comp->data, reslen + sizeof(unicode_t));
			}
		 }
	    } else if (comp->type == XmSTRING_COMPONENT_SEPARATOR) {
	    }
	}

    }
    _XmStringFreeContext(context);
    _XmStringFree(_string);

    return res;
}

/*
 * XmStringParseText converts characters specified in text to corresponding components
 * in the returned compound string. The resulting compound string consists of at least
 * one locale or charset tag component and a series of XmString text components and other
 * components. The conversion proceeds according to the parse information contained in
 * parse_table. See the Motif Programmer's Guide for more information about parsing
 * and parse tables.
 *
 * # If type is XmCHARSET_TEXT, the associated tag is interpreted as a charset name.
 *	If tag has a value of NULL, a charset component whose value is the result of mapping
 *	XmFONTLIST_DEFAULT_TAG is created.
 *
 * # If type is XmMULTIBYTE_TEXT or XmWIDECHAR_TEXT, the associated tag is interpreted as
 *	a language environment name. If tag has a value of NULL, a locale component with
 *	a value of _MOTIF_DEFAULT_LOCALE is created. If type is XmMULTIBYTE_TEXT or
 *	XmWIDECHAR_TEXT, tag must be NULL or _MOTIF_DEFAULT_LOCALE.
 *
 * XmStringParseText also scans the string for characters that have matches in parse_table.
 * Whenever a match is found, the text up to that point is concatenated with the mapped component.
 *
 * text
 *	Specifies the NULL-terminated string containing characters of a type determined by type.
 *	This is updated to point to after the last character scanned.
 *
 * text_end
 *	Specifies a pointer into text. If a NULL is supplied to the text_end parameter,
 *	then XmStringParseText parses text until NULL is encountered, or until it reaches
 *	a point in text where it is directed to stop (for example, by a parse_proc).
 *	Otherwise, the value supplied to the text_end parameter is the pointer into text
 *	where parsing is to stop, and the returned character is the one where parsing did stop.
 *
 * tag
 *	Specifies the tag to be used in creating the result. The type of string tag created
 *	(charset or locale) depends on the text type and the passed in tag value. If the tag
 *	value is NULL and if type indicates that a charset string tag should be created,
 *	the string tag has the value that is the result of mapping XmFONTLIST_DEFAULT_TAG. If
 *	type indicates a locale string tag, the string tag has the value _MOTIF_DEFAULT_LOCALE.
 *
 * type
 *	Specifies the type of text and the tag type. If a locale tag should be created, type
 *	has a value of either XmMULTIBYTE_TEXT or XmWIDECHAR_TEXT. If type has value of
 *	XmCHARSET_TEXT, a charset tag will be created.
 *
 * parse_table
 *	Specifies the parse table to be used in scanning for characters to be converted
 *	to other compound string components.
 *
 * parse_count
 *	Specifies the number of entries in parse_table.
 *
 * call_data
 *	Specifies data to be passed to the parse procedures.
 */
extern XmString
XmStringParseText(XtPointer text,
	XtPointer *text_end,
	XmStringTag tag,
	XmTextType type,
	XmParseTable parse_table,
	Cardinal parse_count,
	XtPointer call_data)
{
	char		*the_end, *p, *start, *tmp,
			*txt = (char *)text;
	int		i, len;
	XmString	xms, x2, x3;

	DEBUGOUT(_LtDebug(__FILE__, NULL, "XmStringParseText(%s, %s)\n", txt, tag));

	if (text_end == NULL) {
		/* Find the end of the string
		 * so we have only one check further along */
		for (p=txt; *p; p++) ;
		the_end = p;
	} else
		the_end = *(char **)text_end;

	/* Fields in XmParseTable :
	 *	XtPointer	client_data
	 *	XmIncludeStatus	include_status
	 *	XmParseProc	invoke_parse_proc
	 *	XtPointer	pattern
	 *	XmTextType	pattern_type
	 *	XmString	substitute
	 */
	start = NULL;	/* Indicates xms is not initialised */
	p = txt;
	while (p < the_end) {
		for (i=0; i<parse_count; i++) {
			if (parse_table[0][i].pattern == NULL)
				continue;
			len = strlen(parse_table[0][i].pattern);
			if (strncmp(p, parse_table[0][i].pattern, len) == 0) {
				if (start) {
					/* Append */
					tmp = XtMalloc(p - start + 1);
					strncpy(tmp, start, p - start);
					tmp[p-start] = '\0';
					DEBUGOUT(_LtDebug(__FILE__, NULL, "Yow1(%s)\n", tmp));
					x2 = XmStringCreateLtoR(tmp, tag);
					XtFree(tmp);
					x3 = XmStringConcat(xms, x2);
					XmStringFree(xms);
					XmStringFree(x2);
					xms = XmStringConcat(x3, parse_table[0][i].substitute);
					XmStringFree(x3);

					start = p + len;
					p = start - 1;
				} else {
					start = txt;
					tmp = XtMalloc(p - start + 1);
					strncpy(tmp, start, p - start);
					tmp[p-start] = '\0';
					DEBUGOUT(_LtDebug(__FILE__, NULL, "Yow2(%s)\n", tmp));
					x2 = XmStringCreateLtoR(tmp, tag);
					XtFree(tmp);

					xms = XmStringConcat(x2, parse_table[0][i].substitute);
					XmStringFree(x2);

					start = p + len;
					p = start - 1;
				}
			}
		}
		p++;
	}
	
	/* Append the last part */
	if (start) {
		/* Append */
		tmp = XtMalloc(p - start + 1);
		strncpy(tmp, start, p - start);
		tmp[p-start] = '\0';
					DEBUGOUT(_LtDebug(__FILE__, NULL, "Yow3(%s)\n", tmp));
		x2 = XmStringCreateLtoR(tmp, tag);
		XtFree(tmp);

		x3 = XmStringConcat(xms, x2);
		XmStringFree(xms);
		XmStringFree(x2);
		xms = x3;

		start = p + len;
		p = start;
	} else {
		start = txt;
		tmp = XtMalloc(p - start + 1);
		strncpy(tmp, start, p - start);
		tmp[p-start] = '\0';
					DEBUGOUT(_LtDebug(__FILE__, NULL, "Yow4(%s)\n", tmp));
		xms = XmStringCreateLtoR(tmp, tag);
		XtFree(tmp);

		start = p + len;
		p = start;
	}
	return xms;
}

extern Cardinal
XmStringToXmStringTable(XmString string, XmString break_comp, XmStringTable *table)
{
	
	_XmWarning(NULL, "XmStringToXmStringTable() is not implemented yet!\n");
	return (Cardinal)0;
}

extern XmString
XmStringTableToXmString(XmStringTable table, Cardinal count, XmString break_component)
{
	_XmWarning(NULL, "XmStringTableToXmString() is not implemented yet!\n");
	return (XmString)NULL;
}

extern XtPointer *
XmStringTableUnparse(XmStringTable table, Cardinal count, XmStringTag tag,
	XmTextType tag_type, XmTextType output_type, XmParseTable parse,
	Cardinal parse_count, XmParseModel parse_model)
{
	_XmWarning(NULL, "XmStringTableUnparse() is not implemented yet!\n");
	return (XtPointer *)NULL;
}


extern XmStringTable
XmStringTableParseStringArray(XtPointer *strings, Cardinal count,
	XmStringTag tag, XmTextType type, XmParseTable parse,
	Cardinal parse_count, XtPointer call_data)
{
	_XmWarning(NULL, "XmStringTableParseStringArray() is not implemented yet!\n");
	return (XmStringTable)NULL;
}


extern XmString
XmStringPutRendition(XmString string, XmStringTag rendition)
{
	_XmWarning(NULL, "XmStringPutRendition() is not implemented yet!\n");
	return (XmString)NULL;
}


extern XmParseMapping
XmParseMappingCreate(ArgList  arg_list, Cardinal arg_count)
{
	_XmWarning(NULL, "XmParseMappingCreate() is not implemented yet!\n");
	return (XmParseMapping)NULL;
}


extern void
XmParseMappingSetValues(XmParseMapping parse_mapping, ArgList arg_list, Cardinal arg_count)
{
	_XmWarning(NULL, "XmParseMappingSetValues() is not implemented yet!\n");
}


extern void
XmParseMappingGetValues(XmParseMapping parse_mapping, ArgList arg_list, Cardinal arg_count)
{
	_XmWarning(NULL, "XmParseMappingGetValues() is not implemented yet!\n");
}


extern void
XmParseMappingFree(XmParseMapping parse_mapping)
{
	_XmWarning(NULL, "XmParseMappingFree() is not implemented yet!\n");
}


extern void
XmParseTableFree(XmParseTable parse_table, Cardinal parse_count)
{
	_XmWarning(NULL, "XmParseTableFree() is not implemented yet!\n");
}


extern XmStringComponentType
XmStringGetNextTriple(XmStringContext context,
           unsigned int *length,
	   XtPointer *value)
{
	_XmWarning(NULL, "XmStringGetNextTriple() is not implemented yet!\n");
	return (XmStringComponentType)NULL;
}


extern XmString
XmStringComponentCreate(XmStringComponentType ctype,
			unsigned int length,
			XtPointer value)
{
	XmString	r;
	_XmString	str = NULL;

	switch (ctype) {
	case XmSTRING_COMPONENT_SEPARATOR:
		return XmStringSeparatorCreate();
	case XmSTRING_COMPONENT_TAB:
		str = __XmAllocNewXmString(1);

		str->components[0]->type = ctype;
		str->components[0]->length = 0;
		str->components[0]->data = NULL;
		r = _XmStringCreateExternal(NULL, str);
		_XmStringFree(str);

		return r;
	case XmSTRING_COMPONENT_END:
		/* These all have value NULL and length 0 */
		break;
	case XmSTRING_COMPONENT_LAYOUT_POP:
		/* These all have value NULL and length 0 */
		break;
	case XmSTRING_COMPONENT_DIRECTION:
		str = __XmAllocNewXmString(1);
		str->components[0]->type = XmSTRING_COMPONENT_DIRECTION;
		str->components[0]->length = 0;
		str->components[0]->data = XtMalloc(sizeof(int));
		str->components[0]->data[0] = (int)value;
		r = _XmStringCreateExternal(NULL, str);
		_XmStringFree(str);
		return r;
	case XmSTRING_COMPONENT_RENDITION_BEGIN:
	case XmSTRING_COMPONENT_RENDITION_END:
		str = __XmAllocNewXmString(1);
		str->components[0]->type = ctype;
		str->components[0]->length = length;
		str->components[0]->data = XtMalloc(length);
		memcpy(str->components[0]->data, value, length);
		r = _XmStringCreateExternal(NULL, str);
		_XmStringFree(str);
		return r;
	case XmSTRING_COMPONENT_LAYOUT_PUSH:
	case XmSTRING_COMPONENT_LOCALE_TEXT:
	case XmSTRING_COMPONENT_WIDECHAR_TEXT:
	case XmSTRING_COMPONENT_TEXT:
	case XmSTRING_COMPONENT_UNKNOWN:
	case XmSTRING_COMPONENT_LOCALE:
	case XmSTRING_COMPONENT_TAG:
#if 0
	case XmSTRING_COMPONENT_CHARSET:
	case XmSTRING_COMPONENT_FONTLIST_ELEMENT_TAG:
#endif
		break;
	default:
		_XmWarning(NULL, "XmStringComponentCreate() is not implemented yet!\n");
		}
	return (XmString)NULL;
}


extern XmIncludeStatus
XmeGetNextCharacter(XtPointer *text_in_out,
                    XtPointer text_end,
                    XmTextType type,
                    XmStringTag tag,
                    XmParseMapping entry,
                    int pattern_length,
                    XmString *str_include,
                    XtPointer call_data)
{
       _XmWarning(NULL, "XmeGetNextCharacter() is not implemented yet!\n");
       return XmINSERT; /* this is not sufficient to indicate a failure ... */
}


extern XmIncludeStatus
XmeGetDirection(XtPointer *text_in_out,
                XtPointer text_end,
                XmTextType type,
                XmStringTag tag,
                XmParseMapping entry,
                int pattern_length,
                XmString *str_include,
                XtPointer call_data)
{
       _XmWarning(NULL, "XmeGetDirection() is not implemented yet!\n");
       return XmINSERT; /* this is not sufficient to indicate a failure ... */
}


extern XmStringComponentType
XmStringPeekNextTriple(XmStringContext context)
{
       _XmWarning(NULL, "XmStringPeekNextTriple() is not implemented yet!\n");
       return XmSTRING_COMPONENT_UNKNOWN;
}


/* similar structure to XmStringEmpty() */
extern Boolean
XmStringIsVoid(XmString s1)
{
    _XmString str;
    Boolean ret;

    if (!_XmStringIsXmString(s1))
    {
	return True;
    }

    str = _XmStringCreate(s1);

    ret = _XmStringIsVoid(str);

    _XmStringFree(str);

    return ret;
}
