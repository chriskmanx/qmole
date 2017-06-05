/*
 *
 * Grid.h - XmpGrid Public header
 *
 */

#ifndef _XmpGrid_h
#define _XmpGrid_h

#include <Xm/Xm.h>

#ifndef XmpIsGrid
#define XmpIsGrid(w) XtIsSubclass(w, xmpGridWidgetClass)
#endif

#define XmNgridMarginWidth "gridMarginWidth"
#define XmNgridMarginHeight "gridMarginHeight"

#define XmCGridMarginWidth "GridMarginWidth"
#define XmCGridMarginHeight "GridMarginHeight"

externalref WidgetClass xmpGridWidgetClass;

typedef struct _XmpGridClassRec * XmpGridWidgetClass;
typedef struct _XmpGridRec      * XmpGridWidget;

extern Widget XmpCreateGrid();

#endif /* _XmpGrid_h */
