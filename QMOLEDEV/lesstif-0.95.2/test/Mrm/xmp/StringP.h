/*
 *
 * StringP.h - XmpString Private header
 *
 */

#ifndef _XmpStringP_h
#define _XmpStringP_h

#include <String.h>
#include <BlobP.h>

typedef struct _XmpStringClassPart
{
	unsigned char 		default_fontlist_type;
	XtPointer		extension;
} XmpStringClassPart;

typedef struct _XmpStringClassRec
{
	CoreClassPart		core_class;
	XmPrimitiveClassPart	primitive_class;
	XmpBlobClassPart	blob_class;
	XmpStringClassPart	string_class;
} XmpStringClassRec;

extern XmpStringClassRec xmpStringClassRec;

typedef struct _XmpStringPart
{
	_XmString		text;
	unsigned char		alignment;
	unsigned char		string_direction;
	XmFontList		font_list;
} XmpStringPart;


typedef struct _XmpStringRec
{
	CorePart		core;
	XmPrimitivePart		primitive;
	XmpBlobPart		blob;
	XmpStringPart		string;
} XmpStringRec;

#endif /* _XmpStringP_h */
