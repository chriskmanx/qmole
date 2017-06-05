/*
 *
 * Blob.h - XmpBlob Public header
 *
 */

#ifndef _XmpBlob_h
#define _XmpBlob_h

#include <Xm/Xm.h>

#ifndef XmpIsBlob
#define XmpIsBlob(w) XtIsSubclass(w, xmpBlobWidgetClass)
#endif

#define XmNblobShape "blobShape"
#define XmCBlobShape "BlobShape"
#define XmRBlobShape "BlobShape"

enum { XmBLOB_OVAL=0, XmBLOB_RECTANGLE=1 };

externalref WidgetClass xmpBlobWidgetClass;

typedef struct _XmpBlobClassRec *XmpBlobWidgetClass;
typedef struct _XmpBlobRec *XmpBlobWidget;

extern Widget XmpCreateBlob();

#endif /* _XmpBlob_h */
