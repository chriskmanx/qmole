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
#include "ap_Dialog_CollaborationAddAccount.h"
#include <account/xp/AccountHandler.h>

#include "ap_Dialog_CollaborationAccounts.h"

AP_Dialog_CollaborationAccounts::AP_Dialog_CollaborationAccounts(XAP_DialogFactory * pDlgFactory, XAP_Dialog_Id id)
	: XAP_Dialog_NonPersistent(pDlgFactory, id, "interface/dialogcollaborationaccounts")
{
	AbiCollabSessionManager::getManager()->registerEventListener(this);
}

AP_Dialog_CollaborationAccounts::~AP_Dialog_CollaborationAccounts(void)
{
	AbiCollabSessionManager::getManager()->unregisterEventListener(this);
}

bool AP_Dialog_CollaborationAccounts::_addAccount(AccountHandler* pAccount)
{
	UT_return_val_if_fail(pAccount, false);
	
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);
	
	bool success = pManager->addAccount(pAccount);
	if (success)
		pManager->storeProfile();
	return success;
}

bool AP_Dialog_CollaborationAccounts::_deleteAccount(AccountHandler* pAccount)
{
	UT_return_val_if_fail(pAccount, false);
	
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);
	
	bool success = pManager->destroyAccount(pAccount);
	if (success)
		pManager->storeProfile();
	return success;
}

void AP_Dialog_CollaborationAccounts::createNewAccount()
{
	// Get the current view that the user is in.
	XAP_Frame* pFrame = XAP_App::getApp()->getLastFocussedFrame();
	// Get an Accounts dialog instance
	XAP_DialogFactory* pFactory = static_cast<XAP_DialogFactory *>(XAP_App::getApp()->getDialogFactory());
	UT_return_if_fail(pFactory);
	AP_Dialog_CollaborationAddAccount* pDialog = static_cast<AP_Dialog_CollaborationAddAccount*>(
				pFactory->requestDialog(AbiCollabSessionManager::getManager()->getDialogAddAccountId())
			);
			
	// Run the dialog
	pDialog->runModal(pFrame);
	if (pDialog->getAnswer() == AP_Dialog_CollaborationAddAccount::a_OK)
	{
		AccountHandler* pHandler = pDialog->getAccountHandler();
		if (pHandler)
		{
			// store the new account
			if (_addAccount(pHandler)) // if not a duplicate
				pHandler->connect(); // connect the account
		}
		else
		{
			UT_DEBUGMSG(("No account handler selected, ignoring...\n"));
		}
	}
	pFactory->releaseDialog(pDialog);
}
