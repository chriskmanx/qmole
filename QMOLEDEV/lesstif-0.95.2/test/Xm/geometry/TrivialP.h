#ifndef TRIVIAL_P_H
#define TRIVIAL_P_H

#include <Xm/XmP.h>
#include <Xm/BulletinBP.h>
#include "Trivial.h"

typedef struct _XmTrivialClassPart {
	int duh;
} XmTrivialClassPart;

typedef struct _XmSmartMessageBoxClassRec {
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ConstraintClassPart constraint_class;
	XmManagerClassPart manager_class;
	XmBulletinBoardClassPart bulletin_board_class;
	XmTrivialClassPart trivial_class;
} XmTrivialClassRec, *XmTrivialWidgetClass;

typedef struct _XmTrivialPart {
	int gaah;
} XmTrivialPart;

typedef struct _XmTrivialRec {
	CorePart core;
	CompositePart	 composite;
	ConstraintPart constraint;
	XmManagerPart manager;
	XmBulletinBoardPart bulletin_board;
	XmTrivialPart trivial;
} XmTrivialRec, *XmTrivialPtr;

#endif
