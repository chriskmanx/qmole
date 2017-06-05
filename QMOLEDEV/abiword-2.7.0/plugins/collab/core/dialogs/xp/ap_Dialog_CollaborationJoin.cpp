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

#include <stdlib.h>
#include <stdio.h>

#include "xap_App.h"
#include "xap_Dialog_Id.h"
#include "xap_DialogFactory.h"
#include <xp/AbiCollabSessionManager.h>

#include "ap_Dialog_CollaborationJoin.h"
#include "ap_Dialog_CollaborationAddBuddy.h"

#include <account/xp/AccountHandler.h>
#include <account/xp/AccountEvent.h>

// TODO : remove this!!!
#include <backends/xmpp/xp/XMPPBuddy.h>

AP_Dialog_CollaborationJoin::AP_Dialog_CollaborationJoin(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id)
	: XAP_Dialog_NonPersistent(pDlgFactory, id, "interface/dialogcollaborationjoin"),
	m_pBuddy(BuddyPtr()),
	m_pDocHandle(NULL)
{
	AbiCollabSessionManager::getManager()->registerEventListener(this);
}

AP_Dialog_CollaborationJoin::~AP_Dialog_CollaborationJoin(void)
{
	AbiCollabSessionManager::getManager()->unregisterEventListener(this);
}

void AP_Dialog_CollaborationJoin::_eventAddBuddy()
{
	// Get the current view that the user is in.
	XAP_Frame* pFrame = XAP_App::getApp()->getLastFocussedFrame();
	// Get an Add Buddy dialog instance
	XAP_DialogFactory* pFactory = static_cast<XAP_DialogFactory *>(XAP_App::getApp()->getDialogFactory());
	UT_return_if_fail(pFactory);
	AP_Dialog_CollaborationAddBuddy* pDialog = static_cast<AP_Dialog_CollaborationAddBuddy*>(
				pFactory->requestDialog(AbiCollabSessionManager::getManager()->getDialogAddBuddyId())
			);
			
	// Run the dialog
	pDialog->runModal(pFrame);
	if (pDialog->getAnswer() == AP_Dialog_CollaborationAddBuddy::a_OK)
	{
		AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
		UT_return_if_fail(pManager->getAccounts().size() != 0)
	
		AccountHandler* pAccount = pDialog->_getActiveAccount();
		UT_return_if_fail(pAccount);
	
		// Add the buddy to the dialog
		// FIXME: use pHandler->constructBuddy here, instead of the hard-coded XMPP buddy!!!
		XMPPBuddyPtr pNewBuddy(new XMPPBuddy(pAccount, pDialog->getName().utf8_str()));
		_addBuddy(pAccount, pNewBuddy);
		
		// signal that we want to add a buddy to our list
		AccountAddBuddyRequestEvent event;
		event.addRecipient(pNewBuddy);
		pManager->signal(event);
	}
	pFactory->releaseDialog(pDialog);
}

void AP_Dialog_CollaborationJoin::_addBuddy(AccountHandler* pHandler, BuddyPtr pBuddy)
{
	UT_return_if_fail(pHandler);
	UT_return_if_fail(pBuddy);

	UT_DEBUGMSG(("Adding buddy (%s) to handler (%s)\n", pBuddy->getDescription().utf8_str(), pHandler->getDescription().utf8_str()));
	pHandler->addBuddy(pBuddy);
	pHandler->getSessionsAsync(pBuddy);
}

void AP_Dialog_CollaborationJoin::_refreshAllDocHandlesAsync()
{
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();

	const std::vector<AccountHandler *> accounts = pManager->getAccounts();
	for (UT_uint32 i = 0; i < accounts.size(); i++)
	{
		// update all document handles
		AccountHandler* pHandler = accounts[i];
		pHandler->getSessionsAsync();
	}
}

void AP_Dialog_CollaborationJoin::_refreshAccounts()
{
	// Called when we get a signal to add or delete an account
	// should also be called from within your dialog initiation to set control states properly.
	
	
	// Check to see if we should enable buddy addition
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	const std::vector<AccountHandler*>& accounts = pManager->getAccounts();
	bool bEnableAddition = false;

	// Loop through accounts
	for (UT_uint32 i = 0; i < accounts.size() && ! bEnableAddition; i++)
	{
		UT_continue_if_fail(accounts[i]);
		bEnableAddition = accounts[i]->allowsManualBuddies();
	}
	_enableBuddyAddition(bEnableAddition);
}

void AP_Dialog_CollaborationJoin::signal(const Event& event, BuddyPtr /*pSource*/)
{
	UT_DEBUGMSG(("AP_Dialog_CollaborationJoin::signal()\n"));
	switch (event.getClassType())
	{
		case PCT_AccountNewEvent:
		// case Event::AccountDelete:
			_refreshAccounts();
			break;
		case PCT_AccountAddBuddyEvent:
		case PCT_AccountDeleteBuddyEvent:
		case PCT_AccountBuddyOnlineEvent:
		case PCT_AccountBuddyOfflineEvent:
			// FIXME: ick ick ick! (I shouldn't need to explain this)
			_refreshWindow();
			break;
		case PCT_AccountBuddyAddDocumentEvent:
			{
				// FIXME: until the event actually tells us which session
				// was started or closed, we'll simply request the whole 
				// session list from the remote buddy
				// FIXME: we should be able to refresh the list for a specific buddy/session
				//const AccountBuddyAddDocumentEvent* pAde = static_cast<const AccountBuddyAddDocumentEvent*>(&event);
				//DocHandle* pDocHandler = pAde->getDocHandle();
				// TODO: only refresh the buddy belonging to this handle
				_refreshWindow();
			}
			break;	
		case PCT_StartSessionEvent:
			// FIXME: until the event actually tells us which session
			// was started, we'll simply request the whole 
			// session list from the remote buddy
			// FIXME: we should be able to refresh the list for a specific buddy/session
			_refreshAllDocHandlesAsync();
			break;
		case PCT_CloseSessionEvent:
			// TODO: only remove the closed session from the list
			_refreshWindow();
			break;
		default:
			// we will ignore the rest
			break;
	}
}
