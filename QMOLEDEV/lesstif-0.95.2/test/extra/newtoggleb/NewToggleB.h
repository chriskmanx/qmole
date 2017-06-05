/*
 * NewToggleB.h - Sieht nicht nur einfach besser aus als das Original aus der
 *                Motif-Mottenkiste, sondern darueberhinaus koennen die
 *                einzelnen Zustaende der "Ankreuzkaestchen" und "Radio-
 *                schalter" von den Anwender einfacher erfasst werden. Warum
 *                eigentlich nicht gleich so?!
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

#ifndef __newtoggleb_h
#define __newtoggleb_h

#include <Xm/Xm.h>
#include <Xm/ToggleB.h>

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif


#define XmNtriState			"triState"
#define XmCTriState			"TriState"
#define XmNshowReflection		"showReflection"
#define XmCShowReflection		"ShowReflection"
#define XmNswapShadows			"swapShadows"
#define XmCSwapShadows			"SwapShadows"

#define XmTOGGLE_OFF			False
#define XmTOGGLE_ON			True
#define XmTOGGLE_DONTKNOW		42


extern WidgetClass xmNewToggleButtonWidgetClass; /* Die Klasse hoechstselbst */

typedef struct _XmNewToggleButtonClassRec *XmNewToggleButtonWidgetClass;
typedef struct _XmNewToggleButtonRec      *XmNewToggleButtonWidget;


#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif

/* Ende von NewToggleB.h */
