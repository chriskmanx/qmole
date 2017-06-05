/*
 *
 * GridP.h - XmpGrid Private header
 *
 */

#ifndef _XmpGridP_h
#define _XmpGridP_h

#include <Grid.h>
#include <Xm/ManagerP.h>

typedef struct
{
	XtPointer		extension;
} XmpGridClassPart;

typedef struct _XmpGridClassRec
{
	CoreClassPart		core_class;
	CompositeClassPart	composite_class;
	ConstraintClassPart	constraint_class;
	XmManagerClassPart	manager_class;
	XmpGridClassPart	grid_class;
} XmpGridClassRec;

externalref XmpGridClassRec xmpGridClassRec;

typedef struct
{
	short			rows;
	short			columns;
	Dimension		margin_width;
	Dimension		margin_height;

	Dimension		pref_width;
	Dimension		pref_height;
	Boolean			compute_width;
	Boolean			compute_height;
	Boolean			processing_constraints;
} XmpGridPart;

typedef struct _XmpGridRec
{
	CorePart		core;
	CompositePart		composite;
	ConstraintPart		constraint;
	XmManagerPart		manager;
	XmpGridPart		grid;
} XmpGridRec;

typedef struct _XmpGridConstraintPart
{
	Dimension		grid_margin_width;
	Dimension		grid_margin_height;
} XmpGridConstraintPart, * XmpGridConstraint;

typedef struct _XmpGridConstraintRec
{
	XmManagerConstraintPart	manager;
	XmpGridConstraintPart	grid;
} XmpGridConstraintRec, * XmpGridConstraintPtr;

#endif /* _XmpGridP_h */
