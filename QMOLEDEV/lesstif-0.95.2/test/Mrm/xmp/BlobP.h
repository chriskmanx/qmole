/*
 *
 * BlobP.h - XmpBlob Private header
 *
 */

#ifndef _XmpBlobP_h
#define _XmpBlobP_h

#include <Blob.h>
#include <Xm/PrimitiveP.h>

#define XmInheritDrawVisual	((XtWidgetProc) _XtInherit)
#define XmInheritDrawShadow	((XtWidgetProc) _XtInherit)
#define XmInheritCreateGC	((XtWidgetProc) _XtInherit)
#define XmInheritDestroyGC	((XtWidgetProc) _XtInherit)
#define XmInheritSelectGC	((XmpSelectGCProc) _XtInherit)
#define XmInheritCalcWidgetSize	((XtWidgetProc) _XtInherit)
#define XmInheritCalcVisualSize	((XtWidgetProc) _XtInherit)

typedef GC (*XmpSelectGCProc)();

typedef struct _XmpBlobClassPart
{
	XtWidgetProc		draw_visual;
	XtWidgetProc		draw_shadow;
	XtWidgetProc		create_gc;
	XtWidgetProc		destroy_gc;
	XmpSelectGCProc		select_gc;
	XtWidgetProc		calc_widget_size;
	XtWidgetProc		calc_visual_size;
	XtPointer		extension;
} XmpBlobClassPart;

typedef struct _XmpBlobClassRec
{
	CoreClassPart		core_class;
	XmPrimitiveClassPart	primitive_class;
	XmpBlobClassPart	blob_class;
} XmpBlobClassRec;

extern XmpBlobClassRec xmpBlobClassRec;

typedef struct _XmpBlobPart
{
	unsigned char		blob_shape;
	Dimension		margin_height;
        Dimension		margin_width;

	GC			normal_gc;
	GC			insensitive_gc;
	Dimension		pref_width;
	Dimension		pref_height;
	Boolean			compute_width;
	Boolean			compute_height;
	Position		visual_x;
	Position		visual_y;
	Dimension		visual_width;
	Dimension		visual_height;
	Boolean			reconfigure;
} XmpBlobPart;

typedef struct _XmpBlobRec
{
	CorePart		core;
	XmPrimitivePart		primitive;
	XmpBlobPart		blob;
} XmpBlobRec;

#endif /* _XmpBlobP_h */
