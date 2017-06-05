/**
 *
 * $Header: /cvsroot/lesstif/lesstif/lib/Dt/DtPrintSetupData.c,v 1.18 2001/09/25 17:41:58 amai Exp $
 *
 * Copyright © 2000,2001 LessTif Development Team
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static const char rcsid[] = "$Header: /cvsroot/lesstif/lesstif/lib/Dt/DtPrintSetupData.c,v 1.18 2001/09/25 17:41:58 amai Exp $";

#include <LTconfig.h>

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <Xm/Xm.h>
#include <Dt/Print.h>
#include <Dt/DtPrintSetupBoxP.h>

#include <XmI/DebugUtil.h>

extern DtPrintSetupData *
DtPrintCopySetupData(DtPrintSetupData *target, const DtPrintSetupData *source)
{
	DEBUGOUT(_LtDebug(__FILE__, NULL, "DtPrintCopySetupData\n"));
	return NULL;	/* FIX ME */
}

/*
 * Two different cases :
 * 1. Quick print button
 *	Parameter "wid" is a DtPrintSetupBox.
 *	DtPrintFillSetupData fills the print_data structure. The caller must
 *		free allocated memory locations by calling DtPrintFreeSetupData().
 *	The X printer connection is managed by the DtPrintSetupBox.
 *	If printer_name is NULL then DtNprinterName is used. If DtNprinterName
 *		differs from printer_name, then DtNprinterName is updated.
 *	Destination and dest_info will be updated.
 *	If a connection cannot be established then the DtPrintSetupBox will be
 *		managed, showing an error dialog. The user can then dismiss that
 *		dialog and use the DtPrintSetupBox to select another printer and
 *		restart the printing operation.
 * 2. GUI-less operation
 *	Parameter "wid" is NULL.
 *	The X printer connection returned, including the print context, is managed
 *		by the caller.
 *	If printer_name is NULL then DtPrintFillSetupData will determine a printer
 *		and set printer_name to this value.
 *	Destination and dest_info will be updated.
 *	The caller can free allocated memory by calling DtPrintFreeSetupData().
 */
XtEnum
DtPrintFillSetupData(Widget wid, DtPrintSetupData *print_data)
{
	char		*name = NULL, *list = NULL, *p, *q, *d, *z, *at;
	Display		*dpy = NULL, *pdpy = NULL;
	XPPrinterList	plist;
	int		count;
	XtAppContext	appc = NULL;
	String		argv[] = { "this" };
	int		argc = 1;
	XPContext	ctx;

	list = getenv("XPSERVERLIST");

	if (wid == NULL) {
	/* GUI - less case (or the DtPrintSetupBox widget itself wants this). */
		DEBUGOUT(_LtDebug(__FILE__, wid, "DtPrintFillSetupData(NULL)\n"));

		appc = XtCreateApplicationContext();	/* FIX ME ?? */

		/* Get a printer name */
		if (print_data && print_data->printer_name)
			name = print_data->printer_name;
		else if ((name = getenv("XPRINTER")))
			;
		else if ((name = getenv("PDPRINTER")))
			;
		else if ((name = getenv("LPDEST")))
			;
		else
			name = getenv("PRINTER");

		if (! name) {
			DEBUGOUT(_LtDebug(__FILE__, wid, "DtPrintFillSetupData: no name\n"));
	/*
	 * Destroying the application context here seems to kill test2
	 *
			XtDestroyApplicationContext(appc);
	 */
			return DtPRINT_NO_PRINTER;
		}
		DEBUGOUT(_LtDebug(__FILE__, wid, "DtPrintFillSetupData(name %s)\n", name));

		/* Create an X printing connection */
		if (print_data && print_data->print_display)
			dpy = print_data->print_display;

		/* Maybe the printer name is fully qualified */
		if ((p = strchr(name, '@'))) {
			d = XtNewString(p+1);
			if ((q = strchr(d, ':')) == NULL) {
				d = XtRealloc(d, strlen(d)+4);
				strcat(d, ":0");
			}
			/* Now d contains a host:display string, try to use it */
			pdpy = XtOpenDisplay(appc, d, "", "", NULL, 0, &argc, argv);
			DEBUGOUT(_LtDebug(__FILE__, wid, "DtPrintFillSetupData opened display %p\n", pdpy));
		}

		/* Check this display */
		if (dpy) {
			plist = XpGetPrinterList(dpy, name, &count);
			if (count > 0) {
				pdpy = dpy;
				print_data->print_display = pdpy;
				ctx = XpCreateContext(pdpy, plist[0].name);
				XpSetContext(pdpy, ctx);
				print_data->print_context = ctx;

				if (print_data->printer_name)
					XtFree(print_data->printer_name);
				print_data->printer_name = XtNewString(plist[0].name);
				if (print_data->dest_info)
					XtFree(print_data->dest_info);
				print_data->dest_info = XtNewString(plist[0].desc);
				DEBUGOUT(_LtDebug(__FILE__, wid, "Got %d entries on this display\n", count));
			}
		}

		/* Check the list of print servers */
		if (list && ! pdpy) {
			DEBUGOUT(_LtDebug(__FILE__, wid, "Try XpServerlist [%s]\n", list));
			z = XtMalloc(strlen(list)+5);
			/* Cut the list into pieces */
			for (p=list; *p; p++) {
				while (*p && isspace(*p)) p++;
				if (*p == '\0')
					break;
				for (q=p, d=z; *q && !isspace(*q); q++)
					*(d++) = *q;
				*d = '\0';

				/* Now z contains a piece of the list */
				if (! strchr(z, ':'))
					strcat(z, ":0");
				DEBUGOUT(_LtDebug(__FILE__, wid, "DtPrintFillSetupData try to open display <%s>\n", z));
				pdpy = XtOpenDisplay(appc, z, "", "", NULL, 0, &argc, argv);
				if (! pdpy) {
					if (*q)
						p = q;
					else
						break;
					continue;
				}
				DEBUGOUT(_LtDebug(__FILE__, wid, "... dpy %p\n", pdpy));

				plist = XpGetPrinterList(pdpy, name, &count);
				if (count > 0) {
					print_data->print_display = pdpy;
					ctx = XpCreateContext(pdpy, plist[0].name);
					XpSetContext(pdpy, ctx);
					print_data->print_context = ctx;

					if (print_data->printer_name) {
						XtFree(print_data->printer_name);
					}
					print_data->printer_name = XtNewString(plist[0].name);
					if (print_data->dest_info) {
						XtFree(print_data->dest_info);
					}
					print_data->dest_info = XtNewString(plist[0].desc);

					DEBUGOUT(_LtDebug(__FILE__, wid, "Got %d entries on display %s\n", count, z));
				}

				if (*q)
					p=q;
				else
					break;
			}
			XtFree(z);
		}

	/*
	 * Destroying the application context here seems to kill test2
	 *
		XtDestroyApplicationContext(appc);
	 */
	} else {
		/* wid is the DtPrintSetupBox */
		DEBUGOUT(_LtDebug(__FILE__, wid, "DtPrintFillSetupData()\n"));

		appc = XtWidgetToApplicationContext(wid);

		if (print_data && print_data->printer_name)
			name = print_data->printer_name;
		else if (PSB_PrinterName(wid))
			name = PSB_PrinterName(wid);

		/* This is - mostly - copied from above, there must be a better way */
		/* Check the list of print servers */
		if (list && ! pdpy) {
			DEBUGOUT(_LtDebug(__FILE__, wid, "Try XpServerlist [%s]\n", list));
			z = XtMalloc(strlen(list)+5);
			/* Cut the list into pieces */
			for (p=list; *p; p++) {
				while (*p && isspace(*p)) p++;
				if (*p == '\0')
					break;
				for (q=p, d=z; *q && !isspace(*q); q++)
					*(d++) = *q;
				*d = '\0';

				/* Now z contains a piece of the list */
				if (! strchr(z, ':'))
					strcat(z, ":0");
				DEBUGOUT(_LtDebug(__FILE__, wid,
					"DtPrintFillSetupData try to open display <%s>\n", z));
				pdpy = XtOpenDisplay(appc, z, "", "", NULL, 0, &argc, argv);
				if (! pdpy) {
					if (*q)
						p = q;
					else
						break;
					continue;
				}
				DEBUGOUT(_LtDebug(__FILE__, wid, "... dpy %p\n", pdpy));

				plist = XpGetPrinterList(pdpy, name, &count);
				if (count > 0) {
					print_data->print_display = pdpy;
					if (print_data->print_context == (XPContext)0) {
						ctx = XpCreateContext(pdpy, plist[0].name);
						print_data->print_context = ctx;
					} else {
						ctx = print_data->print_context;
					}

					XpSetContext(pdpy, ctx);
		
					if (print_data->printer_name) {
						XtFree(print_data->printer_name);
					}
					print_data->printer_name = XtNewString(plist[0].name);
					if (print_data->dest_info) {
						XtFree(print_data->dest_info);
					}
					print_data->dest_info = XtNewString(plist[0].desc);

					DEBUGOUT(_LtDebug(__FILE__, wid,
						"Got %d entries on display %s\n", count, z));
				}

				if (*q)
					p=q;
				else
					break;
			}
			XtFree(z);
		}
	}
	return DtPRINT_SUCCESS;
}

extern void
DtPrintFreeSetupData(DtPrintSetupData *target)
{
	DEBUGOUT(_LtDebug(__FILE__, NULL, "DtPrintFreeSetupData\n"));

	if (target->print_display && target->print_context)
		XpDestroyContext(target->print_display, target->print_context);

	target->print_context = (XPContext)0;
	if (target->print_display)
		XtCloseDisplay(target->print_display);
	target->print_display = (Display *)0;
}


extern XtEnum
DtPrintResetConnection(Widget wid, DtPrintResetConnectionMode mode)
{
	DEBUGOUT(_LtDebug(__FILE__, NULL, "DtPrintResetConnection\n"));
	return (XtEnum)0;
}
