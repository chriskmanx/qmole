/* Copyright (C) 2006 Marc Maurer <uwog@uwog.net>
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

#ifndef AP_DIALOG_COLLABORATIONADDBUDDY_H
#define AP_DIALOG_COLLABORATIONADDBUDDY_H

#include <map>
#include <string>
#include "ut_types.h"
#include "xap_Frame.h"
#include "xap_Dialog.h"
#include "xav_View.h"
#include "ut_vector.h"

using std::map;
using std::string;

class ConnectionHandler;

extern pt2Constructor ap_Dialog_CollaborationAddBuddy_Constructor;

class AP_Dialog_CollaborationAddBuddy : public XAP_Dialog_NonPersistent
{
public:
	AP_Dialog_CollaborationAddBuddy(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id);
	virtual ~AP_Dialog_CollaborationAddBuddy(void);

	virtual void					runModal(XAP_Frame * pFrame) = 0;

	typedef enum { a_OK, a_CANCEL } tAnswer;

	AP_Dialog_CollaborationAddBuddy::tAnswer	getAnswer(void) const
		{ return m_answer; }

	const UT_UTF8String&			getName() const
		{ return m_name; }

	virtual AccountHandler*			_getActiveAccount() = 0;

protected:
	// TODO: we need more than just a name in the future, so we need a PropertyMap here
	void							_setName(const UT_UTF8String& name)
		{ m_name = name; }

	AP_Dialog_CollaborationAddBuddy::tAnswer m_answer;

	
private:
	UT_UTF8String		m_name;
};

#endif /* AP_DIALOG_COLLABORATIONADDBUDDY_H */
