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

#ifndef AP_DIALOG_GENERICPROGRESS_H
#define AP_DIALOG_GENERICPROGRESS_H

#include "ut_types.h"
#include "xap_Dialog.h"

extern pt2Constructor ap_Dialog_GenericProgress_Constructor;

class AP_Dialog_GenericProgress : public XAP_Dialog_NonPersistent
{
public:
	AP_Dialog_GenericProgress(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id);
	virtual ~AP_Dialog_GenericProgress(void);

	virtual void					runModal(XAP_Frame * pFrame) = 0;

	void							setTitle(const UT_UTF8String& sTitle)
		{ m_sTitle = sTitle; }
	
	const UT_UTF8String&			getTitle()
		{ return m_sTitle; }

	void							setInformation(const UT_UTF8String& sInformation)
		{ m_sInformation = sInformation; }

	const UT_UTF8String&			getInformation()
		{ return m_sInformation; }

	virtual void					close() = 0;
	virtual void					setProgress(UT_uint32 progress) = 0;

	typedef enum { a_OK, a_CANCEL } tAnswer;

	AP_Dialog_GenericProgress::tAnswer	getAnswer(void) const
		{ return m_answer; }

protected:
	AP_Dialog_GenericProgress::tAnswer m_answer;
	
private:
	UT_UTF8String		m_sTitle;
	UT_UTF8String		m_sInformation;
};

#endif /* AP_DIALOG_GENERICPROGRESS_H */
