/*
 * NewToggleBP.h - Sieht nicht nur einfach besser aus als das Original aus der
 *                 Motif-Mottenkiste, sondern darueberhinaus koennen die
 *                 einzelnen Zustaende der "Ankreuzkaestchen" und "Radio-
 *                 schalter" von den Anwender einfacher erfasst werden. Warum
 *                 eigentlich nicht gleich so?!
 *
 * Version 0.90a
 * 
 * (c) 1994 Harald Albrecht
 * Institut fuer Geometrie und Praktische Mathematik
 * RWTH Aachen, Germany
 * albrecht@igpm.rwth-aachen.de
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (see the file COPYING for more details);
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, 
 * Cambridge, MA 02139, USA.
 *
 */

#ifndef __newtogglebp_h
#define __newtogglebp_h

#include <Xm/XmP.h>
#include <Xm/LabelP.h>
#include <Xm/ToggleBP.h>

#include "NewToggleB.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ---------------------------------------------------------------------------
 * Klassendefinition der ToggleButton-Ersatzklasse. Zuerst kommen einmal die
 * neuen Elemente des Klassendatensatzes. Ausserdem gibt's bei dieser Klasse
 * einiges zu vererben...
 */
typedef void (*XmNewToggleButtonIndicatorDrawProc)(XmNewToggleButtonWidget);
typedef void (*XmNewToggleButtonTextDrawProc)(XmNewToggleButtonWidget);
#define XmInheritNewToggleButtonIndicatorDrawProc \
	((XmNewToggleButtonIndicatorDrawProc) _XtInherit)
#define XmInheritNewToggleButtonTextDrawProc \
	((XmNewToggleButtonTextDrawProc) _XtInherit)

#define CallDrawIndicatorMethod(w) \
    (*((XmNewToggleButtonClassRec *)(w->core.widget_class))-> \
        newtoggle_class.DrawIndicator)(w)
#define CallDrawTextMethod(w) \
    (*((XmNewToggleButtonClassRec *)(w->core.widget_class))-> \
        newtoggle_class.DrawText)(w)

typedef struct _XmNewToggleButtonClassPart {
    XmNewToggleButtonIndicatorDrawProc DrawIndicator;
    XmNewToggleButtonTextDrawProc      DrawText;
} XmNewToggleButtonClassPart;
/*
 * Nun folgt die vollstaendige Klassenstruktur, mit all' den Feldern,
 * die bereits von den Vorfahren geerbt wurden.
 */
typedef struct _XmNewToggleButtonClassRec {
    CoreClassPart              core_class;
    XmPrimitiveClassPart       primitive_class;
    XmLabelClassPart           label_class;
    XmToggleButtonClassPart    toggle_class;
    XmNewToggleButtonClassPart newtoggle_class;
} XmNewToggleButtonClassRec;
extern XmNewToggleButtonClassRec xmNewToggleButtonClassRec;


/* ---------------------------------------------------------------------------
 * Instanzdefinition: Hier sind alle Variablen abgelegt, die jeweils ein
 * Widget der Klasse XmNewToggleButton (oder Nachfolger) besitzt.
 */
typedef struct _XmNewToggleButtonPart {
    Boolean		TriState;
    Boolean             ShowReflection;
    Boolean             SwapShadows;
    Boolean             ShowTick;
} XmNewToggleButtonPart;

typedef struct _XmNewToggleButtonRec {
    CorePart	          core;
    XmPrimitivePart       primitive;
    XmLabelPart		  label;
    XmToggleButtonPart    toggle;
    XmNewToggleButtonPart new_toggle;
} XmNewToggleButtonRec;

#ifdef __cplusplus
}
#endif

#endif

/* Ende von NewToggleBP.h */
