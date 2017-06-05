/*
 * ToggleBplus.h - Sieht nicht nur einfach besser aus als das Original aus der
 *                 Motif-Mottenkiste, sondern darueberhinaus koennen die
 *                 einzelnen Zustaende der "Ankreuzkaestchen" und "Radio-
 *                 schalter" von den Anwender einfacher erfasst werden. Warum
 *                 eigentlich nicht gleich so?!
 *
 * Version 0.91 vom 31.10.1994
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
#ifndef __togglebplus_h
#define __togglebplus_h

#include <Xm/Xm.h>
#include <Xm/ToggleB.h>

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

extern Widget XmCreateToggleButtonPlus(Widget Parent, String Name,
                                       ArgList Args, Cardinal NumArgs);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif
/* Ende von ToggleBplus.h */
