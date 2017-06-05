/*
 *
 * String.h - XmpString Public header
 *
 */

#ifndef _XmpString_h
#define _XmpString_h

#include <Xm/Xm.h>

#ifndef XmpIsString
#define XmpIsString(w) XtIsSubclass(w, xmpStringWidgetClass)
#endif

#define XmNtext "text"
#define XmCText "Text"

externalref WidgetClass xmpStringWidgetClass;

typedef struct _XmpStringClassRec *XmpStringWidgetClass;
typedef struct _XmpStringRec *XmpStringWidget;

extern Widget XmpCreateString();

#endif /* _XmpString_h */
