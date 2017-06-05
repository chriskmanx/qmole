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

#ifndef AP_DIALOG_COLLABORATIONACCOUNTS_H
#define AP_DIALOG_COLLABORATIONACCOUNTS_H

#include "ut_types.h"
#include "xap_Frame.h"
#include "xap_Dialog.h"
#include "xav_View.h"
#include "ut_vector.h"
#include <session/xp/AbiCollabSessionManager.h>
#include <account/xp/Event.h>
#include <account/xp/EventListener.h>

class ConnectionHandler;

extern pt2Constructor ap_Dialog_CollaborationAccounts_Constructor;

class AP_Dialog_CollaborationAccounts : public XAP_Dialog_NonPersistent, EventListener
{
public:
	AP_Dialog_CollaborationAccounts(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id);
	virtual ~AP_Dialog_CollaborationAccounts(void);

	virtual void		runModal(XAP_Frame * pFrame) = 0;
	void				createNewAccount();

	typedef enum { a_CLOSE } tAnswer;

	AP_Dialog_CollaborationAccounts::tAnswer	getAnswer(void) const;
	
protected:
	bool				_addAccount(AccountHandler* pHandler);
	bool				_deleteAccount(AccountHandler* pAccount);
	
	AP_Dialog_CollaborationAccounts::tAnswer	m_answer;
};

#endif /* AP_DIALOG_COLLABORATIONACCOUNTS_H */
