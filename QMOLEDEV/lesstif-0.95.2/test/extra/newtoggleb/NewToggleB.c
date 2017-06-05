/*
 * NewToggleB.c - Sieht nicht nur einfach besser aus als das Original aus der
 *                Motif-Mottenkiste, sondern darueberhinaus koennen die
 *                einzelnen Zustaende der "Ankreuzkaestchen" und "Radio-
 *                schalter" von den Anwender einfacher erfasst werden. Warum
 *                eigentlich nicht gleich so?!
 *                Und wer hat da denn vor einiger Zeit in den NetNews auf
 *                die Anfrage eines gestressten Programmierers gesuelzt, dass
 *                man ToggleButtons nicht ableiten kann und soll...? Ha! Es
 *                geht eben doch (Greetings to Leonardo DaVinci...) Alles
 *                zwar mangels Dokumentation eines der letzten wahren Aben-
 *                teuer unserer Zeit, aber das Ergebnis kann sich doch sehen
 *                lassen, oder?!
 *
 * Version 0.91b
 * Aktueller Zustand:
 *   16.05.1994  ToggleButton-Klasse mit 3 Zustaenden sowie einem besseren
 *               Aussehen implementiert. Ausserdem ist der ToggleButton auch
 *               in Menues einsetzbar! Arbeitet auch mit XmNradioBehavior
 *               in RowColumn-Widgets!
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

#include "NewToggleBP.h"

#include <Xm/BaseClassP.h>
#include <Xm/DrawP.h>
#include <Xm/MenuUtilP.h>

/* ---------------------------------------------------------------------------
 * Resourcen-Liste...
 * Hier werden diejenigen Resourcen definiert, die von "aussen" - also
 * fuer den Programmierer oder Anwender - benutzbar und veraenderbar  
 * sind.
 * 
 * Der Aufbau der einzelnen Eintraege ist immer wieder gleich:
 * Resourcen-Name       XmN... oder XtN
 * Resourcen-Klasse     XmC... oder XtC
 * Resourcen-Type       XmR... oder XtR (Datentyp der Variable in der
 *                      struct der jeweiligen Widgetinstanz)
 * Resourcen-Groesse    aktuelle Groesse dieses Datentyps   
 * Resourcen-Offset     Lage der Variable innerhalb der struct der
 *                      Widgetinstanz
 * Defaultwert-Type     Typ des Defaultwertes
 * Defaultwert          (normalerweise) Zeiger auf den Defaultwert
 */
#define offset(field) XtOffsetOf(XmNewToggleButtonRec, field)
static XtResource resources[] = {
    { /* Ist es ein normaler Toggle-Button (bzw. Radio-Button) oder hat
       * er 3 Zustaende (= Kompatibilitaet zu anderen Oberflaechen) ?
       */
        XmNtriState, XmCTriState, XmRBoolean, sizeof(Boolean),
        offset(new_toggle.TriState), XmRString, "False"
    },
/*    {
        XmNselectColor, XmCSelectColor, XmRPixel, sizeof(Pixel),
        offset(toggle.select_color), XtRCallProc, _XmForegroundColorDefault
    }, */
    {
        XmNshowReflection, XmCShowReflection, XmRBoolean, sizeof(Boolean),
        offset(new_toggle.ShowReflection), XmRString, "True"
    },
    {
        XmNswapShadows, XmCSwapShadows, XmRBoolean, sizeof(Boolean),
        offset(new_toggle.SwapShadows), XmRString, "True"
    }
}; /* resources */ 
                                                      

/* ---------------------------------------------------------------------------
 * Funktions-Prototypen fuer die 'Methoden' des ComboBox-Widgets. Eigentlich
 * sind das in unserem Falle gar nicht allzu viele. Die meiste Arbeit wird
 * spaeter von den einzelnen Action-Routinen geleistet, die durch bestimmte
 * Ereignisse (oder Ereignistransitionen) ausgeloest werden.
 */
static void             ClassPartInit(WidgetClass);
static void             Repaint(XmNewToggleButtonWidget, XEvent *, Region);
static Boolean          SetValues(XmNewToggleButtonWidget, 
                                  XmNewToggleButtonWidget, 
                                  XmNewToggleButtonWidget, 
                                  ArgList, Cardinal *);

/* ---------------------------------------------------------------------------
 * Diese Funktions-Prototypen stellen neue "Methoden" dar, die erstmalig von
 * dieser ToggleButton-Ersatzklasse eingefuehrt werden.
 */
static void             DrawIndicator(XmNewToggleButtonWidget);
static void             DrawText(XmNewToggleButtonWidget);

/* ---------------------------------------------------------------------------
 * Hier versammeln sich alle Translations, die entweder neu hinzukommen oder
 * aber von XmLabel ererbt wurden. Zu beachten ist, dass diejenigen Trans-
 * lationen, die bereits XmPrimitive kennt, zumindest innerhalb der Klassen-
 * struktur geerbt werden. Allerdings werden nichts desto trotz bei allen
 * Nachkommen von XmLabel moeglicherweise einige Translationen von XmPri-
 * mitive wieder ueberschrieben.
 * Schluck: in der Tat koennen wir einfach diejenigen Translationen von
 * XmToggleButton uebernehmen, wir muessen lediglich dafuer Sorge tragen,
 * neue Action-Prozeduren zu registrieren. In diesem Fall werden dann
 * praktischerweise gleich die neuen Routinen benutzt. Ergo: es folgt hier
 * keine Translation-Tabelle...
 */

/* ---------------------------------------------------------------------------
 * Hier werden nun alle sog. Action-Routinen aufgezaehlt, die diese Widget-
 * klasse neu definiert. Dabei werden zum Teil bereits bestehende Action-
 * routinen der Vorfahrenklasse ueberschrieben.
 * Zuvor (vor der Zuordnung) sind aber noch ein Prototypen vonnoeten, damit
 * der Compiler nicht meckert.
 */
#define ACTIONPROC(proc) \
    static void proc(XmNewToggleButtonWidget, XEvent *, String *, Cardinal *)

ACTIONPROC(EnterWidget);
ACTIONPROC(LeaveWidget);
ACTIONPROC(ArmWidget);
ACTIONPROC(DisarmWidget);
ACTIONPROC(SelectWidget);
ACTIONPROC(ArmAndActivateWidget);
ACTIONPROC(BtnUpWidget);

static XtActionsRec actions[] = {
	{ "Enter",          (XtActionProc) EnterWidget          },
	{ "Leave",          (XtActionProc) LeaveWidget          },
	{ "Arm",            (XtActionProc) ArmWidget            },
	{ "Disarm",         (XtActionProc) DisarmWidget         },
	{ "Select",         (XtActionProc) SelectWidget         },
	{ "ArmAndActivate", (XtActionProc) ArmAndActivateWidget },
	{ "BtnUp",          (XtActionProc) BtnUpWidget          }
}; /* actions */

/* ---------------------------------------------------------------------------
 * Klassen-Definition unserer neuen ToggleButton-Klasse.
 */
XmNewToggleButtonClassRec xmNewToggleButtonClassRec = {
    { /*** core-Klasse ***/
    /* superclass		    */	(WidgetClass) &xmToggleButtonClassRec,
    /* class_name		    */	"XmNewToggleButton",
    /* widget_size		    */	sizeof(XmNewToggleButtonRec),
    /* class_initialize   	    */	NULL,
    /* class_part_initialize	    */	(XtWidgetClassProc) ClassPartInit,
    /* class_inited       	    */	False, /* IMMER mit FALSE initialisieren !! */
    /* initialize	  	    */	NULL,
    /* initialize_hook		    */	NULL,
    /* realize		  	    */	(XtRealizeProc) XmInheritRealize,
    /* actions		  	    */	actions,
    /* num_actions	  	    */	XtNumber(actions),
    /* resources	  	    */	resources,
    /* num_resources	  	    */	XtNumber(resources),
    /* xrm_class	  	    */	NULLQUARK,
    /* compress_motion	  	    */	True,
    /* compress_exposure  	    */	XtExposeCompressMultiple,
    /* compress_enterleave	    */	True,
    /* visible_interest	  	    */	False,
    /* destroy		  	    */	NULL,
    /* resize		  	    */	(XtWidgetProc) XmInheritResize,
    /* expose		  	    */	(XtExposeProc) Repaint,
    /* set_values	  	    */	(XtSetValuesFunc) SetValues,
    /* set_values_hook		    */	NULL,
    /* set_values_almost	    */	XtInheritSetValuesAlmost,
    /* get_values_hook		    */	NULL,
    /* accept_focus	 	    */	NULL,
    /* version			    */	XtVersion,
    /* callback_private   	    */	NULL,
    /* tm_table		   	    */	NULL,
    /* query_geometry		    */	XtInheritQueryGeometry,
    /* display_accelerator	    */	XtInheritDisplayAccelerator,
    /* extension          	    */	NULL
    }, 
    { /*** xmPrimitive-Klasse ***/
    /* border_highlight		    */	(XtWidgetProc) _XtInherit,
    /* border_unhighlight	    */	(XtWidgetProc) _XtInherit,
    /* translations		    */  XtInheritTranslations,
    /* arm_and_activate		    */  NULL,
    /* syn_resources		    */  NULL,
    /* num_syn_resources	    */  0,
    /* extension		    */	NULL
    }, 
    { /*** xmLabel-Klasse ***/
    /* setOverrideCallback	    */	(XtWidgetProc) XmInheritSetOverrideCallback,
    /* menuProcs		    */  NULL,
    /* translations		    */  XtInheritTranslations,
    /* extension                    */  NULL
    }, 
    { /*** xmToggleButton-Klasse ***/
    /* foo			    */  0
    }, 
    { /*** xmNewToggleButton-Klasse ***/
    /* DrawIndicator		    */  XmInheritNewToggleButtonIndicatorDrawProc,
    /* DrawText                     */  XmInheritNewToggleButtonTextDrawProc
    }
}; /* xmNewToggleButtonClassRec */
WidgetClass xmNewToggleButtonWidgetClass = (WidgetClass) &xmNewToggleButtonClassRec;

#include <stdio.h>


/* ---------------------------------------------------------------------------
 * Diese Initialisierungsroutine wird immer dann aufgerufen, wenn entweder
 * diese Widgetklasse oder aber eine davon abgeleitete Klasse erstmalig in
 * "Betrieb" genommen wird. Hier sorgen wir dafuer, dass abgeleitete Klassen
 * die neu eingefuehrten Methoden erben koennen. Dabei handelt es sich um
 * Methoden, um sowohl den "Indikator" als auch den Text/Pixmap zu zeichnen.
 * Leider hat die OSF sowas naemlich bei ihrer ToggleButton-Klasse total
 * vergessen! Daher ja auch der ganze Aufwand hier!!
 *
 * Parameter:
 *   wc			Diejenige Widgetklasse, die erstmalig in Betrieb ge-
 *			nommen werden soll. Dieses ist entweder die hier neu
 *			eingefuehrte Klasse oder aber moeglicherweise eine
 *			davon abgeleitete.
 */
static void ClassPartInit(WidgetClass wc)
{
    /* Sorge dafuer, dass die neu definierten Methoden von den Nachkommen
     * geerbt werden koennen.
     */
    if ( ((XmNewToggleButtonClassRec *) wc)->newtoggle_class.DrawIndicator ==
           XmInheritNewToggleButtonIndicatorDrawProc )
        ((XmNewToggleButtonClassRec *) wc)->newtoggle_class.DrawIndicator =
            (XmNewToggleButtonIndicatorDrawProc) DrawIndicator;
    if ( ((XmNewToggleButtonClassRec *) wc)->newtoggle_class.DrawText ==
           XmInheritNewToggleButtonTextDrawProc )
        ((XmNewToggleButtonClassRec *) wc)->newtoggle_class.DrawText =
            (XmNewToggleButtonTextDrawProc) DrawText;
    /* ...eigentlich ja ueberfluessig, da bereits in den vorherigen Klassen
     * durchgefuehrt... aber schaden tut's auch nicht!
     */
    _XmFastSubclassInit(wc, XmTOGGLE_BUTTON_BIT | XmLABEL_BIT | 
                            XmPRIMITIVE_BIT);
} /* ClassPartInit */

/* ---------------------------------------------------------------------------
 * Mit Hilfe dieser Routine kann anhand des aktuellen Zustands ermittelt
 * werden, in welchen Zustand der Button als naechstes uebergehen wird.
 *
 * Parameter:
 *   w			Widget, fuer den der naechste Zustand erfragt werden
 *			soll.
 *   State		Der aktuelle Zustand. Hierzu bitte den folgenden
 *			Zustand ermitteln.
 *
 * Ergebnis:
 *   Der naechste Zustand des Buttons.
 */
static Boolean ToggleButtonNextState(XmNewToggleButtonWidget w,
                                     Boolean State)
{
    if ( w->new_toggle.TriState )
        switch ( State ) {
            case XmTOGGLE_ON      : return XmTOGGLE_OFF;
            case XmTOGGLE_OFF     : return XmTOGGLE_DONTKNOW;
            case XmTOGGLE_DONTKNOW: default: return XmTOGGLE_ON;
        }
    switch ( State ) {
        case XmTOGGLE_ON      : return XmTOGGLE_OFF;
        case XmTOGGLE_OFF     : return XmTOGGLE_ON;
        case XmTOGGLE_DONTKNOW: default: return XmTOGGLE_ON;
    }
} /* ToggleButtonNextState */

/* ---------------------------------------------------------------------------
 * Zeichne den Indikator, das ist entweder ein Ankreuzkaestchen oder aber ein
 * runder Radioschalter. (Ach, waren das noch Zeiten, als man sich an den
 * Roehren die Finger waermen konnte und man noch zum Betrieb so richtig
 * Spannung brauchte -- nicht so laeppische 3,3V!!!)
 *
 * Parameter:
 *   w			Dasjenige Widget, fuer das der Indikator zu zeichnen
 *			ist. Den aktuellen Zustand erfaehrt man aus der
 *			Instanz-Struktur dieses Widgets aus der Variable
 *			w->toggle.visual_state.
 */
static void DrawIndicator(XmNewToggleButtonWidget w)
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
    if ( (State == XmTOGGLE_OFF) || !(w->new_toggle.SwapShadows) ||
         (w->label.menu_type != XmWORK_AREA) ) {
        TopGC    = w->primitive.top_shadow_GC;
        BottomGC = w->primitive.bottom_shadow_GC;
    } else {
        TopGC    = w->primitive.bottom_shadow_GC;
        BottomGC = w->primitive.top_shadow_GC;
    }
    
    /* Jetzt muessen wir noch berechnen, wo denn der Indikator zu zeichnen
     * ist.
     */
    size = w->toggle.indicator_dim - 3 ;
    if ( w->label.menu_type != XmWORK_AREA ) size += 2;
    if ( size < 4 ) size = 4;
    x = w->label.margin_width + w->primitive.highlight_thickness;
    if ( w->label.string_direction == XmSTRING_DIRECTION_R_TO_L )
        x = w->core.width - x - size;
    y = ((w->core.height - size) >> 1);

/*    XClearArea(Dsp, Win, x, y, size, size, False); */
    /* Wir muessen hier dafuer sorgen, dass ein evtl. doch noch von
     * XtSetValues gezeichneter alter Indikator entsorgt wird!
     */
    XClearArea(Dsp, Win, 
               x, w->label.TextRect.y,
               w->toggle.indicator_dim, w->label.TextRect.height,
               False);
    
    if ( w->toggle.ind_type != XmONE_OF_MANY ) {    
        /* Es ist ein sog. Ankreuzkaestchen. Hiervon koennen jeweils be-
         * liebig viele angekreuzt oder auch nicht angekreuzt sein. Dar-
         * gestellt werden sie konsequenterweise als Kaestchen.
         */
        XDrawLine(Dsp, Win, TopGC, x, y, x + size, y);
        XDrawLine(Dsp, Win, TopGC, x, y, x, y + size);
        XDrawLine(Dsp, Win, BottomGC, x + size, y + 1, x + size, y + size);
        XDrawLine(Dsp, Win, BottomGC, x + 1, y + size, x + size, y + size);
        if ( State != XmTOGGLE_OFF )
            if ( State != XmTOGGLE_ON )
                XFillRectangle(Dsp, Win, w->label.insensitive_GC,
                               x + 2, y + 2, size - 3, size - 3);
            else {
                if ( w->label.menu_type != XmWORK_AREA ) {
                    x -= 2; y -= 2; size += 4;
                }
                XDrawLine(Dsp, Win, w->toggle.select_GC,
                          x + 2, y + 2, x + size - 2, y + size - 2);
                XDrawLine(Dsp, Win, w->toggle.select_GC,
                          x + 3, y + 2, x + size - 2, y + size - 3);
                XDrawLine(Dsp, Win, w->toggle.select_GC,
                          x + 2, y + 3, x + size - 3, y + size - 2);
                XDrawLine(Dsp, Win, w->toggle.select_GC,
                          x + size - 2, y + 2, x + 2, y + size - 2);
                XDrawLine(Dsp, Win, w->toggle.select_GC,
                          x + size - 3, y + 2, x + 2, y + size - 3);
                XDrawLine(Dsp, Win, w->toggle.select_GC,
                          x + size - 2, y + 3, x + 3, y + size - 2);
            }
    } else {
        /* Sonst zeichnen wir einen Radio-Button. Hiervon kann jeweils
         * immer nur einer aus einer Menge "gedrueckt" sein. Wie der Name
         * bereits anklingen laesst, sind die Radio-Buttons rund. Ist
         * der Button im gedrueckten Zustand, so wird das Innere ausge-
         * malt.
         */
        XDrawArc(Dsp, Win, BottomGC, x, y, size, size, 
                 (45 + 180) * 64, 180 * 64);
        XDrawArc(Dsp, Win, TopGC, x, y, size, size, 
                 45 * 64, 180 * 64);
        if ( State != XmTOGGLE_OFF ) {
            XFillArc(Dsp, Win, State == XmTOGGLE_ON ? w->toggle.select_GC :
                                                      w->label.insensitive_GC,
                     x + 1, y + 1, size - 2, size - 2, 0, 360 * 64);
            if ( w->new_toggle.ShowReflection )
                XFillArc(Dsp, Win, w->primitive.top_shadow_GC,
                         x + 3, y + 3, 
                         (size >> 1) - 2, (size >> 1) - 2, 0, 360 * 64);
        }
    }
} /* DrawIndicator */

/* ---------------------------------------------------------------------------
 * Auf Anfrage hin entweder den Text des Buttons oder aber das passende Pixmap
 * ausgeben.
 */
static void DrawText(XmNewToggleButtonWidget w)
{
    Pixmap pixmap;
    
    if ( !XtIsRealized(w) ) return;    

    if ( w->label.label_type == XmPIXMAP ) {
        pixmap = XmUNSPECIFIED_PIXMAP;
        if ( w->toggle.set ) {
            if ( XtIsSensitive((Widget)w) )
                pixmap = w->toggle.on_pixmap != XmUNSPECIFIED_PIXMAP ?
                           w->toggle.on_pixmap : w->label.pixmap;
            else
                pixmap = w->toggle.insen_pixmap != XmUNSPECIFIED_PIXMAP ?
                           w->toggle.insen_pixmap : w->label.pixmap_insen;
        } else
            if ( !XtIsSensitive((Widget)w) )
                pixmap = w->label.pixmap_insen;
        if ( pixmap == XmUNSPECIFIED_PIXMAP ) pixmap = w->label.pixmap;
        if ( pixmap == XmUNSPECIFIED_PIXMAP )
            XClearArea(XtDisplay(w), XtWindow(w), 
                0, 0, w->label.TextRect.width, w->label.TextRect.height,
                False);
        else
            XCopyArea(XtDisplay(w), 
                pixmap, XtWindow(w), w->label.normal_GC,
                0, 0, w->label.TextRect.width, w->label.TextRect.height,
                w->label.TextRect.x, w->label.TextRect.y);
    } else
        _XmStringDrawMnemonic(XtDisplay(w), XtWindow(w),
            w->label.font, w->label._label, 
            XtIsSensitive((Widget)w) ? w->label.normal_GC : w->label.insensitive_GC,
            w->label.TextRect.x, w->label.TextRect.y,
            w->label.TextRect.width, w->label.alignment, 
            w->label.string_direction, NULL, 
            XKeysymToString(w->label.mnemonic), w->label.mnemonicCharset);

    /* In jedem Fall wird aber ein eventueller Accelerator ausgegeben! */
    if ( w->label._acc_text && !_XmStringEmpty(w->label._acc_text) )
        _XmStringDraw(XtDisplay(w), XtWindow(w),
            w->label.font, w->label._acc_text,
            XtIsSensitive((Widget)w) ? w->label.normal_GC : w->label.insensitive_GC,
            w->label.acc_TextRect.x, w->label.acc_TextRect.y,
            w->label.acc_TextRect.width, w->label.alignment, 
            w->label.string_direction, NULL);
} /* DrawText */

/* ---------------------------------------------------------------------------
 * Muss ein Teil des Widgets neu gezeichnet werden, so rufe dazu einfach die
 * passenden Methoden dieser Widgetklasse auf. Nachkommen duerfen die Methoden
 * auf Wunsch auch austauschen.
 *
 * Parameter:
 *   w			Das von Neuzeichnen betroffene Widget
 *   event		Naehere Informationen darueber, was neu zu zeichnen
 *			ist...
 *   region		Wo ist was neu zu zeichnen...
 */
static void Repaint(XmNewToggleButtonWidget w, XEvent *event, Region region)
{
    CallDrawIndicatorMethod(w);
    CallDrawTextMethod(w);
} /* Repaint */


/* ---------------------------------------------------------------------------
 * Rufe eine der beliebten Callbacks des ToggleButtons auf, nachdem irgend-
 * etwas passiert ist...
 */
static void CallCallbacks(XmNewToggleButtonWidget w, 
                          XEvent *event, int reason)
{
    XmToggleButtonCallbackStruct CBData;

    CBData.reason = reason;
    CBData.event  = event;
    CBData.set    = w->toggle.set;
    if ( w->label.skipCallback && (reason == XmCR_VALUE_CHANGED) ) {
        CBData.reason = XmCR_ACTIVATE;
        XtCallCallbacks(XtParent(w), XmNentryCallback, (XtPointer) &CBData);
    } else
        XtCallCallbacks((Widget) w, 
                        reason == XmCR_ARM    ? XmNarmCallback :
                        reason == XmCR_DISARM ? XmNdisarmCallback :
                                                XmNvalueChangedCallback,
                        (XtPointer) &CBData);
} /* CallCallbacks */

/* ---------------------------------------------------------------------------
 * Irgendwie hat die OSF doch ziemlich kraeftig herumgepfuscht...von klarer
 * OOP keine Rede! Hier muessen wir ggf. dafuer sorgen, dass alle Widgets in
 * einem RowColumn-Widgets das Radio-Verhalten verpasst bekommen. Nicht etwa,
 * dass das RowColumn-Widget das selbst taete...nein, dass muessen wir schon
 * selbst tun!
 */
static void CheckRadioBehavior(XmNewToggleButtonWidget w)
{
    Widget     Parent = XtParent(w), Child;
    WidgetList Children;
    Cardinal   NumChildren, i;
    Boolean    RadioBehavior, RadioAlwaysOne;
    
    if ( XmIsRowColumn(Parent) ) {
        XtVaGetValues(Parent, XmNradioBehavior,  &RadioBehavior,
                              XmNradioAlwaysOne, &RadioAlwaysOne, 
                              XmNchildren,       &Children,
                              XmNnumChildren,    &NumChildren,
                              NULL);
        if ( RadioBehavior ) {
            for ( i = 0; i < NumChildren; ++i ) {
                Child = Children[i];
                if ( (Child != (Widget) w) && XtIsManaged(w) )
                    XtVaSetValues(Children[i], XmNset, False, NULL);
            }
            if ( RadioAlwaysOne && w->toggle.set == XmTOGGLE_OFF ) {
                w->toggle.set        = XmTOGGLE_ON;
                w->toggle.visual_set = w->toggle.set;
            }
        }
    }
} /* CheckRadioBehavior */

/* ---------------------------------------------------------------------------
 * Holla - Der Mauszeiger kommt, um uns zu besuchen in unserem bescheidenen
 * Widget. In diesem Fall schauen wir zuerst nach, ob denn der ToggleButton im
 * "geladenen" Zustand ist. Nur in diesem Fall muessen wir ueberhaupt von
 * diesen Ereignissen Notiz nehmen und den Button in demjenigen Zustand
 * zeichnen, in den er uebergehen wuerde, liesse der Benutzer die Maustaste
 * *innerhalb* des Widgets wieder los (= umschalten). Ausnahme von der
 * soeben genannten Regel: wir befinden uns in einem Pulldown- oder Popup-
 * Menu.
 */
static void EnterWidget(XmNewToggleButtonWidget w, XEvent *event, 
                        String *Params, Cardinal *NumParams)
{
    XtCallActionProc((Widget) w, "PrimitiveEnter", event, Params, *NumParams);
    if ( w->label.menu_type != XmWORK_AREA ) {
        /* Wir sitzen in einem Menu und muessen nun noch wissen, ob die 
         * Tastaturbedienung abgeschaltet wurde. Dann bedeutet dies, dass 
         * der Fokus dem Mauszeiger folgt und der ToggleButton immer dann
         * im "geladenen" Zustand dargestellt werden muss, wenn der Maus-
         * zeiger hier auftaucht.
         */
        if ( _XmGetInDragMode((Widget) w) ) {
            _XmDrawShadows(XtDisplay((Widget) w), XtWindow((Widget) w),
                           w->primitive.top_shadow_GC,
                           w->primitive.bottom_shadow_GC,
                           0, 0, w->core.width, w->core.height,
                           w->primitive.shadow_thickness,
                           XmSHADOW_OUT);
            w->toggle.Armed = True;
            CallCallbacks(w, event, XmCR_ARM);
        }
    } else
        if ( w->toggle.Armed ) {
            w->toggle.visual_set = ToggleButtonNextState(w, w->toggle.set);
            CallDrawIndicatorMethod(w);
            CallCallbacks(w, event, XmCR_ARM);
        }
} /* EnterWidget */

/* ---------------------------------------------------------------------------
 * Sollte der Mauszeiger uns wieder verlassen, so muessen wir u.U. wieder den
 * alten Zustand wiederherstellen. Dieses ist immer dann erforderlich, wenn
 * der ToggleButton noch im "geladenen" Zustand ist, sobald der Mauszeiger das
 * Fenster des Widgets verlaesst. Da der Button zumindestens fuer den Be-
 * nutzer in den ungeladenen Zustand uebergeht, wird der entsprechende Call-
 * back noch flugs aufgerufen.
 */
static void LeaveWidget(XmNewToggleButtonWidget w, XEvent *event, 
                        String *Params, Cardinal *NumParams)
{
    XtCallActionProc((Widget) w, "PrimitiveLeave", event, Params, *NumParams);
    if ( w->label.menu_type != XmWORK_AREA ) {
        if ( _XmGetInDragMode((Widget) w) ) {
            _XmClearBorder(XtDisplay((Widget) w), XtWindow((Widget) w),
                           0, 0, w->core.width, w->core.height,
                           w->primitive.shadow_thickness);
            w->toggle.Armed = False;
            CallCallbacks(w, event, XmCR_DISARM);
        }
    } else
        if ( w->toggle.Armed ) {
            w->toggle.visual_set = w->toggle.set;
            CallDrawIndicatorMethod(w);
            CallCallbacks(w, event, XmCR_DISARM);
        }
} /* LeaveWidget */

/* ---------------------------------------------------------------------------
 * Wenn der Benutzer auf den ToggleButton mit der linken Maustaste klickt, so
 * nimmt sich zum Einen der Button einfach den Tastaturfokus, zum anderen
 * geht er in den "geladenen" Zustand ueber. Darin verharrt er dann, um darauf
 * zu lauern, wann er denn losgehen darf. Mit Hilfe der Variablen Armed laesst
 * sich im folgenden bei jedem EnterNotify oder LeaveNotify Ereignis auf
 * einfache Weise klaeren, ob visual_set wieder an "set" angepasst werden muss
 * (falls im Zustand "armed"). Diese Routine wird aber dann nicht aufgerufen,
 * wenn wir uns innerhalb eines Menus befinden...!
 */
static void ArmWidget(XmNewToggleButtonWidget w, XEvent *event, 
                      String *Params, Cardinal *NumParams)
{
    XmProcessTraversal((Widget) w, XmTRAVERSE_CURRENT);
    w->toggle.visual_set = ToggleButtonNextState(w, w->toggle.set);
    w->toggle.Armed      = True;
    CallDrawIndicatorMethod(w);
    CallCallbacks(w, event, XmCR_ARM);
} /* ArmWidget */


static void DisarmWidget(XmNewToggleButtonWidget w, XEvent *event, 
                         String *Params, Cardinal *NumParams)
{
    w->toggle.visual_set = w->toggle.set;
    w->toggle.Armed      = False;
    CallDrawIndicatorMethod(w);
    CallCallbacks(w, event, XmCR_DISARM);
} /* DisarmWidget */


static void SelectWidget(XmNewToggleButtonWidget w, XEvent *event, 
                         String *Params, Cardinal *NumParams)
{
    /* Nur wenn die Maustaste innerhalb des eigentlichen Widgetfensters
     * losgelassen wurde, wird dieses als Aufforderung interpretiert, den
     * Zustand des ToggleButtons zu wechseln.
     */    
    if ( w->toggle.set != w->toggle.visual_set ) {
        w->toggle.set   = w->toggle.visual_set;
        w->toggle.Armed = False;
        CheckRadioBehavior(w);
        CallDrawIndicatorMethod(w);
        CallCallbacks(w, event, XmCR_VALUE_CHANGED);
    }
} /* SelectWidget */

/* ---------------------------------------------------------------------------
 * Waehrend die Tastaturbedienung innerhalb des Menues ausgeschaltet war,
 * liess der Anwender die Maustaste ueber unserem Widget los. Das bedeutet,
 * dass es seinen Zustand wechseln soll! Dieses Ereignis tritt nur dann auf,
 * wenn der ToggleButton sich in einem Menue befindet, ansonsten wird die
 * zum Ausloesen erforderliche Transition ueberhaupt nicht angemeldet bei
 * den Intrinsics.
 */
static void BtnUpWidget(XmNewToggleButtonWidget w, XEvent *event, 
                        String *Params, Cardinal *NumParams)
{
    w->toggle.set        = ToggleButtonNextState(w, w->toggle.set);
    w->toggle.visual_set = w->toggle.set;
    w->toggle.Armed      = False;
    CallDrawIndicatorMethod(w);
    CallCallbacks(w, event, XmCR_VALUE_CHANGED);
    CallCallbacks(w, event, XmCR_DISARM);
} /* BtnUpWidget */

/* ---------------------------------------------------------------------------
 * Bei Tastaturbedienung erfolgt der Aufruf dieser Routine, um den Zustand des
 * ToggleButtons zu wechseln.
 */
static void ArmAndActivateWidget(XmNewToggleButtonWidget w, XEvent *event, 
                                 String *Params, Cardinal *NumParams)
{
    w->toggle.set        = ToggleButtonNextState(w, w->toggle.set);
    w->toggle.visual_set = w->toggle.set;
    w->toggle.Armed      = False;
    if ( w->label.menu_type )
        XtCallActionProc(XtParent((Widget) w) , "MenuShellPopdownDone", 
                         event, Params, *NumParams);
    CheckRadioBehavior(w);
    CallDrawIndicatorMethod(w);
    CallCallbacks(w, event, XmCR_ARM);
    CallCallbacks(w, event, XmCR_VALUE_CHANGED);
    CallCallbacks(w, event, XmCR_DISARM);
} /* ArmAndActivateWidget */

/* ---------------------------------------------------------------------------
 * Der Programmierer moechte eine Variable (aka Ressource) einer Instanz
 * dieses ToggleButtons veraendern. Wir muessen nun nachschauen, ob das
 * irgendwelche Konsequenzen nach sich zieht und ggf. den ToggleButton neu
 * zeichnen.
 */
static Boolean SetValues(XmNewToggleButtonWidget cur, 
                         XmNewToggleButtonWidget req, 
                         XmNewToggleButtonWidget new, 
                         ArgList args, Cardinal *NumArgs)
{
    Boolean Redraw = False;
    
    if ( (new->new_toggle.SwapShadows != cur->new_toggle.SwapShadows) ||
         (new->new_toggle.ShowReflection != 
            cur->new_toggle.ShowReflection )                          ||
         (new->new_toggle.TriState != cur->new_toggle.TriState) )
        Redraw = True;
    
    new->toggle.visual_set = new->toggle.set;
    if ( cur->toggle.set != new->toggle.set )
        CallDrawIndicatorMethod(new);

    return Redraw;
} /* SetValues */


/* Ende von NewToggleB.c */
