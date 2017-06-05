/* Copyright (C) 2008 AbiSource Corporation B.V.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef AP_UNIXDIALOG_GENERICPROGRESS_H
#define AP_UNIXDIALOG_GENERICPROGRESS_H

#include <gtk/gtk.h>
#include <xp/ap_Dialog_GenericProgress.h>

class XAP_Frame;

class AP_UnixDialog_GenericProgress : public AP_Dialog_GenericProgress
{
public:
	AP_UnixDialog_GenericProgress(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id);
	static XAP_Dialog * static_constructor(XAP_DialogFactory * pFactory, XAP_Dialog_Id id);
	void						runModal(XAP_Frame * pFrame);

	virtual void				close();
	virtual void				setProgress(UT_uint32 progress);

private:
	GtkWidget*	 				_constructWindow(void);
	void						_populateWindowData(void);

	GtkWidget*					m_wWindowMain;
	GtkWidget*					m_wCancel;
	GtkWidget*					m_wProgress;
};

#endif /* AP_UNIXDIALOG_GENERICPROGRESS_H */
