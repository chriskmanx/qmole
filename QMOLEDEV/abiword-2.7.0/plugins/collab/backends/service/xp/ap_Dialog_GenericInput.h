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

#ifndef AP_DIALOG_GENERICINPUT_H
#define AP_DIALOG_GENERICINPUT_H

#include "ut_types.h"
#include "xap_Dialog.h"

extern pt2Constructor ap_Dialog_GenericInput_Constructor;

class AP_Dialog_GenericInput : public XAP_Dialog_NonPersistent
{
public:
	AP_Dialog_GenericInput(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id);
	virtual ~AP_Dialog_GenericInput(void);

	virtual void					runModal(XAP_Frame * pFrame) = 0;

	// functions related to password style input
	bool							isPassword()
		{ return m_bIsPassword; }
	void							setPassword(bool bIsPassword)
		{ m_bIsPassword = bIsPassword; }

	// messages to display
	void							setTitle(const UT_UTF8String& sTitle)
		{ m_sTitle = sTitle; }
	
	const UT_UTF8String&			getTitle()
		{ return m_sTitle; }

	void							setQuestion(const UT_UTF8String& sQuestion)
		{ m_sQuestion = sQuestion; }

	const UT_UTF8String&			getQuestion()
		{ return m_sQuestion; }

	void							setLabel(const UT_UTF8String& sLabel)
		{ m_sLabel = sLabel; }

	const UT_UTF8String&			getLabel()
		{ return m_sLabel; }

	typedef enum { a_OK, a_CANCEL } tAnswer;

	AP_Dialog_GenericInput::tAnswer	getAnswer(void) const
		{ return m_answer; }

	const UT_UTF8String&			getInput() const
		{ return m_input; }

protected:
	void							_setInput(const UT_UTF8String& input)
		{ m_input = input; }

	AP_Dialog_GenericInput::tAnswer m_answer;
	
private:
	UT_UTF8String		m_sTitle;
	UT_UTF8String		m_sQuestion;
	UT_UTF8String		m_sLabel;
	bool				m_bIsPassword;
	UT_UTF8String		m_input;
};

#endif /* AP_DIALOG_GENERICINPUT_H */
