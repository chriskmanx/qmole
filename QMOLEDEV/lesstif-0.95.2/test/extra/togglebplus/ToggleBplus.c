/*
 * ToggleBplus.c - Sieht nicht nur einfach besser aus als das Original aus der
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

#include "ToggleBplus.h"
/* Wegen Motif 1.1 ist auch noch LabelP.h einzulesen... */
#include <Xm/LabelP.h>
#include <Xm/ToggleBP.h>



#define XmTOGGLE_ON (True)
#define XmTOGGLE_OFF (False)



#define TICKMARK_FLAG		(1 << 0)
#define SWAPSHADOW_FLAG		(1 << 1)


static Boolean GetNextState(XmToggleButtonWidget w, Boolean State)
{
    return !State;
}

/* ---------------------------------------------------------------------------
 * Liefert True zurueck, falls sich der ToggleButton in einem Menu befindet.
 * In diesem Fall muessen wir uns an einigen Stellen im Programmcode anders
 * verhalten.
 */
static Boolean InMenu(XmToggleButtonWidget w)
{
    return w->label.menu_type != XmWORK_AREA;
} /* InMenu */

/* ---------------------------------------------------------------------------
 * Zeichne den Indikator, das ist entweder ein Ankreuzkaestchen oder aber ein
 * runder Radioschalter. (Ach, waren das noch Zeiten, als man sich an den
 * Roehren die Finger waermen konnte und man noch zum Betrieb so richtig
 * Spannung brauchte -- nicht so laeppische 3,3V!!!)
 *
 * Parameter:
 *   w			Dasjenige Widget, fuer das der Indikator zu zeichnen
 *			ist. Den aktuellen Zustand erfaehrt man ueber die
 *			Instanz-Struktur dieses Widgets aus der Variable
 *			w->toggle.visual_state.
 */
static void DrawIndicator(XmToggleButtonWidget w)
{
    Display   *Dsp = XtDisplay((Widget) w);
    Window    Win = XtWindow((Widget) w);
    GC        TopGC, BottomGC;
    Position  x, y;
    Dimension size;
    Boolean   State = w->toggle.visual_set;

    if ( !XtIsRealized(w) ) return;    
    /* Zuerst muessen wir uns hier einmal die geeigneten GCs zusammenstellen,
     * mit deren Hilfe wir den sog. Indikator zeichnen.
     */
    if ( State == XmTOGGLE_OFF ) {
        TopGC    = w->primitive.top_shadow_GC;
        BottomGC = w->primitive.bottom_shadow_GC;
    } else {
        TopGC    = w->primitive.bottom_shadow_GC;
        BottomGC = w->primitive.top_shadow_GC;
    }
    
    /* Jetzt muessen wir noch berechnen, wo denn der Indikator zu zeichnen
     * ist und wie gross er tatsaechlich werden soll -- toggle.indicator_dim
     * jedenfalls muss nicht so ganz exakt die Groesse repraesentieren...
     * Zudem faellt die Default-Groesse auch noch unterschiedlich aus, je
     * nachdem, ob sich der ToggleButton in einem Menu befindet oder nicht.
     * Damit aber bei unserem neuen Design die Sache nicht zu klein wird,
     * muessen wie noch 'was draufschlagen, wenn wir in einem Menu sind.
     */
    size = w->toggle.indicator_dim - 3 ;
    if ( w->label.menu_type != XmWORK_AREA ) {
        if ( w->toggle.ind_type != XmONE_OF_MANY ) size += 2;
        else                                       size += 4;
    }
    
    /* Zu klein darf der Indikator aber auch nicht werden!! */
    if ( size < 6 ) size = 6;
    
    x = w->label.margin_width + w->primitive.highlight_thickness +
        w->primitive.shadow_thickness;
    if ( w->label.string_direction == XmSTRING_DIRECTION_R_TO_L )
        x = w->core.width - x - size;
    y = ((w->core.height - size) >> 1);

    /* Sicherheitshalber erst einmal saeubern... */
    XClearArea(Dsp, Win, 
               x, (w->core.height - w->toggle.indicator_dim) >> 1,
               w->toggle.indicator_dim, w->toggle.indicator_dim,
               False);
    
    if ( w->toggle.ind_type != XmONE_OF_MANY ) {    
        /* Es ist ein sog. Ankreuzkaestchen. Hiervon koennen jeweils be-
         * liebig viele angekreuzt oder auch nicht angekreuzt sein. Dar-
         * gestellt werden sie konsequenterweise als Kaestchen.
         */
        if ( !InMenu(w) ) {
            XDrawLine(Dsp, Win, TopGC, x, y, x + size, y);
            XDrawLine(Dsp, Win, TopGC, x, y, x, y + size);
            XDrawLine(Dsp, Win, BottomGC, x + size, y + 1, x + size, y + size);
            XDrawLine(Dsp, Win, BottomGC, x + 1, y + size, x + size, y + size);
        }
        if ( State != XmTOGGLE_OFF ) {
            /* Berechne zuerst ein paar Werte, die immer dann benoetigt
             * werden, wenn irgendetwas innerhalb des Ankreuzkaestchen
             * gezeichnet werden muss.
             */
            short halfthickness;

            if ( w->label.menu_type == XmWORK_AREA )
                { x += 1; y += 1; size -= 2; }
            halfthickness = size / 6;
            if (halfthickness <= 1 ) halfthickness = 1;
            x += halfthickness; y += halfthickness;
            size -= halfthickness << 1;

            if ( State != XmTOGGLE_ON )
                XFillRectangle(Dsp, Win, w->label.insensitive_GC,
                               x, y, size + 1, size + 1);
            else {
                XPoint pts[6];
                /* Male nun das Kreuzchen in das Kaestchen. Kreuz ist
                 * immer ca. 1/8 der Breite/Hoehe dick.
                 */
                pts[0].x = x; pts[0].y = y + size + 1;
                pts[1].x = halfthickness; pts[1].y = 0;
                pts[2].x = size - halfthickness + 1; pts[2].y = -pts[2].x;
                pts[3].x = 0; pts[3].y = -halfthickness;
                pts[4].x = -halfthickness - 1; pts[4].y = 0;
                pts[5].x = pts[2].y + 1; pts[5].y = -pts[5].x;
                XFillPolygon(Dsp, Win, w->toggle.select_GC,
                             &pts[0], 6, Convex, CoordModePrevious);

/*                if ( w->new_toggle.UseTickmark ) { */
                    pts[0].x = x; pts[0].y = y + size + 1;
                    pts[1].x = halfthickness + 1; pts[1].y = 0;
                    pts[2].x = 0; 
                      pts[2].y = -((w->toggle.indicator_dim * 3) / 5);
                    pts[3].x = -pts[1].x; pts[3].y = (pts[1].x >> 1) + 1;
                    XFillPolygon(Dsp, Win, w->toggle.select_GC,
                        	 &pts[0], 4, Convex, CoordModePrevious);
/*                } else {
        	    pts[0].x = x; pts[0].y = y;
        	    pts[1].x = 0; pts[1].y = halfthickness;
        	    pts[2].x = size - halfthickness + 1; pts[2].y = pts[2].x;
        	    pts[3].x = halfthickness; pts[3].y = 0;
        	    pts[4].x = 0; pts[4].y = -halfthickness - 1;
        	    pts[5].x = -pts[2].x + 1; pts[5].y = pts[5].x;
        	    XFillPolygon(Dsp, Win, w->toggle.select_GC,
                        	 &pts[0], 6, Nonconvex, CoordModePrevious);
                } */
            }
        }
    } else {
        /* Sonst zeichnen wir einen Radio-Button. Hiervon kann jeweils
         * immer nur einer aus einer Menge "gedrueckt" sein. Wie der Name
         * bereits anklingen laesst, sind die Radio-Buttons rund. Ist
         * der Button im gedrueckten Zustand, so wird das Innere ausge-
         * malt.
         */
        if ( !InMenu(w) || State == XmTOGGLE_ON ) {
            XDrawArc(Dsp, Win, BottomGC, x, y, size, size, 
                     (45 + 180) * 64, 180 * 64);
            XDrawArc(Dsp, Win, TopGC, x, y, size, size, 
                     45 * 64, 180 * 64);
        }
        if ( State != XmTOGGLE_OFF ) {
            short halfthickness, oldsize;

            halfthickness = size / 8;
            oldsize = size; size -= (halfthickness * 2) + 2;
            if ( size < 2 ) size = 2;
            XFillArc(Dsp, Win, State == XmTOGGLE_ON ? w->toggle.select_GC :
                                                      w->label.insensitive_GC,
                     x + halfthickness + 1, y + halfthickness + 1, 
                     size, size, 0, 360 * 64);
            XDrawArc(Dsp, Win, State == XmTOGGLE_ON ? w->toggle.select_GC :
                                                      w->label.insensitive_GC,
                     x + halfthickness + 1, y + halfthickness + 1, 
                     size, size, 0, 360 * 64);
            /* Beachte Rundungsfehler! Daher auch die scheinbar um-
             * staendliche Berechnung der Lage der linken oberen Ecke
             * des den Kreis umschreibenden Rechtecks. Tja, INTEGER-
             * Arithmetik hat's eben in sich...
             */
            XFillArc(Dsp, Win, w->primitive.top_shadow_GC,
                     x + (oldsize >> 1) - (oldsize >> 2), 
                     y + (oldsize >> 1) - (oldsize >> 2), 
                     (oldsize >> 2), (oldsize >> 2), 0, 360 * 64);
            XDrawArc(Dsp, Win, w->primitive.top_shadow_GC,
                     x + (oldsize >> 1) - (oldsize >> 2), 
                     y + (oldsize >> 1) - (oldsize >> 2), 
                     (oldsize >> 2), (oldsize >> 2), 0, 360 * 64);
        }
    }
} /* DrawIndicator */

/* ---------------------------------------------------------------------------
 * Sobald die Nachricht vom Neuzeichnen eintrifft, male den Indikator neu, so-
 * fern dieses erforderlich ist. Bislang bin hier noch etwas faul: der Indi-
 * kator wird immer dann neu gezeichnet, wenn der letzte Expose-Event einer
 * Sequenz eintrifft.
 */
static void ExposureHandler(Widget w, XtPointer ClientData, XEvent *Event,
                            Boolean *ContDispatch)
{
    if ( Event->xexpose.count == 0 )
        DrawIndicator((XmToggleButtonWidget) w);
} /* ExposureHandler */

/* ---------------------------------------------------------------------------
 * Wenn der Button vor Wut geladen ist oder sich wieder abgeregt hat, muss
 * jedesmal der Indikator neu gezeichnet werden.
 */
static void ArmAndDisarmCallback(Widget w, XtPointer ClientData, 
                        XtPointer CallData)
{
    DrawIndicator((XmToggleButtonWidget) w);
} /* ArmAndDisarmCallback */

/* ---------------------------------------------------------------------------
 * Aendert sich der Zustand des ToggleButtons, dann brauchen wir hier einfach
 * nur den Indikator neu zeichnen. Das andere braucht uns nicht weiter zu
 * kuemmern, das erledigt das Widget schon selbst.
 */
static void ValueChangedCallback(Widget w, XtPointer ClientData, 
                                 XtPointer CallData)
{
    DrawIndicator((XmToggleButtonWidget) w);
} /* ValueChangedCallback */

/* ---------------------------------------------------------------------------
 * Sobald der Mauszeiger sich aus dem Button entfernt oder sich dort wieder
 * blicken laesst, solange der Button im geladenen Zustand ist, muessen wir
 * den Indikator neu zeichnen.
 */
static void EnterToggleButton(Widget wid, XEvent *Event, String *Params, 
                              Cardinal *NumParams)
{
    XmToggleButtonWidget w = (XmToggleButtonWidget) wid;
    
    XtCallActionProc(wid, "Enter", Event, Params, *NumParams);
    if ( w->toggle.Armed && !InMenu(w) ) {
        w->toggle.visual_set = GetNextState(w, w->toggle.set);
        DrawIndicator(w);
    }
} /* EnterToggleButton */

static void LeaveToggleButton(Widget wid, XEvent *Event, String *Params, 
                              Cardinal *NumParams)
{
    XmToggleButtonWidget w = (XmToggleButtonWidget) wid;
    
    XtCallActionProc(wid, "Leave", Event, Params, *NumParams);
    if ( w->toggle.Armed && !InMenu(w) ) {
	w->toggle.visual_set = w->toggle.set;
	DrawIndicator(w);
    }
} /* LeaveToggleButton */

/* ---------------------------------------------------------------------------
 *
 */
static XtActionsRec Actions[] = {
    { "TogglePlusEnter",		(XtActionProc) EnterToggleButton },
    { "TogglePlusLeave",		(XtActionProc) LeaveToggleButton }
};

static char NewTranslationsDecr[] = 
    "<EnterWindow>:			TogglePlusEnter()	\n\
     <LeaveWindow>:			TogglePlusLeave()	"
    ;
static XtTranslations NewTranslations = NULL;

Widget XmCreateToggleButtonPlus(Widget Parent, String Name,
                                ArgList Args, Cardinal NumArgs)
{
    Widget    ToggleButton;
    Arg       ButtonArgs[20];
    int       n;
    Dimension IndicatorSize, LeftMargin;

    if ( NewTranslations == NULL ) {
        NewTranslations = XtParseTranslationTable(NewTranslationsDecr);
    }

    ToggleButton = XmCreateToggleButton(Parent, Name, Args, NumArgs);
    if ( ToggleButton ) {
    
        n = 0;
        XtSetArg(ButtonArgs[n], XmNindicatorSize, &IndicatorSize); n++;
        XtSetArg(ButtonArgs[n], XmNmarginLeft, &LeftMargin); n++;
        XtGetValues(ToggleButton, ButtonArgs, n);
        
/* LeftMargin = LeftMargin; */
        
    /*
     * Innerhalb von Menues muss shadowThickness erhalten bleiben, an-
     * sonsten sieht man's nicht mehr, dass der Button geladen ist.
     */
        n = 0;
        XtSetArg(ButtonArgs[n], XmNindicatorOn, False); n++;
        XtSetArg(ButtonArgs[n], XmNmarginLeft, LeftMargin); n++;
        XtSetArg(ButtonArgs[n], XmNfillOnSelect, False); n++;
        if ( !InMenu((XmToggleButtonWidget) ToggleButton) )
	{
            XtSetArg(ButtonArgs[n], XmNshadowThickness, 0); n++;
	}
        XtSetValues(ToggleButton, ButtonArgs, n);

        XtAppAddActions(XtWidgetToApplicationContext(ToggleButton), 
	                Actions, XtNumber(Actions));
        XtOverrideTranslations(ToggleButton, NewTranslations);
    
        XtAddEventHandler(ToggleButton, 
                          ExposureMask, 
                          False,
                          (XtEventHandler) ExposureHandler, 
                          (XtPointer) NULL);
        
        XtAddCallback(ToggleButton, XmNvalueChangedCallback,
                      (XtCallbackProc) ValueChangedCallback,
                      (XtPointer) NULL);
        XtAddCallback(ToggleButton, XmNarmCallback,
                      (XtCallbackProc) ArmAndDisarmCallback,
                      (XtPointer) NULL);
        XtAddCallback(ToggleButton, XmNdisarmCallback,
                      (XtCallbackProc) ArmAndDisarmCallback,
                      (XtPointer) NULL);

    }
    return ToggleButton;
} /* XmCreateToggleButtonPlus */


/* Ende von ToggleBplus.c */
