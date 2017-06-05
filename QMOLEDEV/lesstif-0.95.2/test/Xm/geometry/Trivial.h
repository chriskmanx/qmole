#ifndef TRIVIAL_H
#define TRIVIAL_H

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>

extern WidgetClass xmTrivialWidgetClass;

typedef struct _XmTrivialRec *XmTrivialWidget;
typedef struct _XmTrivialConstraintRec *XmTrivialConstraint;

#ifndef XmIsTrivial
#define XmIsTrivial(a) (XtIsSubclass(a, xmTrivialWidgetClass))
#endif

Widget XmCreateCreateTrivial(Widget _p, char *_n, ArgList _a, Cardinal _narg);

#endif
