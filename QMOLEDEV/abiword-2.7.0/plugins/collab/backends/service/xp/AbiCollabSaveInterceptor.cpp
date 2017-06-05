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

#include <boost/bind.hpp>
#include "xap_App.h"
#include "ev_Menu_Actions.h"
#include "ap_Menu_Id.h"
#include "ap_Menu_Functions.h"
#include "ev_Toolbar_Actions.h"
#include "ap_Toolbar_Id.h"
#include "ap_Toolbar_Functions.h"
#include "ap_LoadBindings.h"
#include "ev_EditEventMapper.h"
#include "xap_Dlg_MessageBox.h"
#include "fv_View.h"
#include "xap_Frame.h"
#include "ut_debugmsg.h"
#include "AsyncWorker.h"
#include "ServiceAccountHandler.h"
#include <core/session/xp/AbiCollab.h>
#include <core/session/xp/AbiCollabSessionManager.h>
#include "AbiCollabSaveInterceptor.h"



static bool AbiCollabSaveInterceptor_interceptor(AV_View * v, EV_EditMethodCallData * d)
{
	return ServiceAccountHandler::m_saveInterceptor.intercept(v, d);
}

#define DO_NOT_USE ""

static ap_bs_Char CharTable[] =
{
//	{char, /* desc   */ { none,					_C,					_A,				_A_C				}},
	{0x53, /* S      */ { "insertData",			"fileSaveAs", 		DO_NOT_USE,		""					}},
	{0x73, /* s      */ { "insertData",			SAVE_INTERCEPTOR_EM,		    DO_NOT_USE,		""					}},
};

AbiCollabSaveInterceptor::AbiCollabSaveInterceptor()
	: m_pOldSaveEM(NULL)
{
	UT_DEBUGMSG(("Installing Save menu interceptor!\n"));
	EV_EditMethodContainer *pEMC = XAP_App::getApp()->getEditMethodContainer();

	// store the old/normale editmethod to fall back when a document is not under service control
	m_pOldSaveEM = pEMC->findEditMethodByName("fileSave");

	UT_return_if_fail(m_pOldSaveEM);
	// install the edit method we will use to save to the webapp
	EV_EditMethod* mySaveInterceptor = new EV_EditMethod (
								SAVE_INTERCEPTOR_EM, 
								&AbiCollabSaveInterceptor_interceptor, 
								0, "AbiCollab Service Save Interceptor"
							);
	pEMC->addEditMethod(mySaveInterceptor);	
	
	// install the new menu action with our custom save edit method
	XAP_App::getApp()->getMenuActionSet()->setAction(
			AP_MENU_ID_FILE_SAVE,
			false, /* holds submenu */
			false, /* raises dialog */
			false, /* is checkable */
			false, /* is radio */
			SAVE_INTERCEPTOR_EM,
			ap_GetState_Changes,
			NULL, /* state label */
			NULL
		);
	
	// install the new toolbar action with our custom save edit method
	XAP_App::getApp()->getToolbarActionSet()->setAction(
			AP_TOOLBAR_ID_FILE_SAVE,
			EV_TBIT_PushButton,
			SAVE_INTERCEPTOR_EM,
			AV_CHG_ALL,
			ap_ToolbarGetState_Changes
		);

	// install the new CTRL-s hook
	// TODO: what to the with Save As?
	const char * szCurrMode = XAP_App::getApp()->getInputMode();
	EV_EditBindingMap* pEbMap = XAP_App::getApp()->getBindingMap(szCurrMode);
	UT_return_if_fail(pEbMap);
		
	AP_BindingSet* pBindingSet = static_cast<AP_BindingSet*>(XAP_App::getApp()->getBindingSet());
	UT_return_if_fail(pBindingSet);

	pBindingSet->_loadChar(pEbMap, CharTable, 2, NULL, 0);	
}

bool AbiCollabSaveInterceptor::saveRemotely(PD_Document * pDoc)
{
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	AbiCollab* pSession = pManager->getSession(pDoc);
	// the session id should be unique on a specific account handler; let's 
	// just look it up amongst all our account handlers (not too efficient or
	// elegant, but it works)
	for (UT_uint32 i = 0; i < pManager->getAccounts().size(); i++)
	{
		AccountHandler* pHandler = pManager->getAccounts()[i];
		UT_continue_if_fail(pHandler);
		if (pHandler->getStorageType() == SERVICE_ACCOUNT_HANDLER_TYPE)
		{
			ServiceAccountHandler* pServiceHandler = static_cast<ServiceAccountHandler*>(pHandler);
			ConnectionPtr connection_ptr = pServiceHandler->getConnection(pDoc);
			if (!connection_ptr)
				continue; // apparently we need another abicollab account
			UT_DEBUGMSG(("Found the abicollab webservice account handler (%s) that controls this session!\n", pServiceHandler->getDescription().utf8_str()));

			pManager->beginAsyncOperation(pSession);
			// FIXME: guarantee save order!
			boost::shared_ptr<AsyncWorker<UT_Error> > async_save_ptr(
						new AsyncWorker<UT_Error>(
							boost::bind(&ServiceAccountHandler::saveDocument, pServiceHandler, pDoc, connection_ptr),
							boost::bind(&AbiCollabSaveInterceptor::_save_cb, this, _1, pSession)
						)
					);
			async_save_ptr->start();
			
			// make the document clean (even if it isn't _yet_)
			pDoc->setClean();
			pDoc->signalListeners(PD_SIGNAL_DOCNAME_CHANGED);
			return true;
		}
	}
	return false;
}

bool AbiCollabSaveInterceptor::intercept(AV_View * v, EV_EditMethodCallData * d)
{
	UT_DEBUGMSG(("AbiCollabSaveInterceptor_intercept\n"));
	UT_return_val_if_fail(v, false);
	FV_View* pView = static_cast<FV_View*>(v);
	
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);

	PD_Document* pDoc = pView->getDocument();
	UT_return_val_if_fail(pDoc, false);
	
	if (!pManager->isInSession(pDoc))
		return m_pOldSaveEM->Fn(v, d);

	UT_DEBUGMSG(("Document is in a collaboration session!\n"));
	AbiCollab* pSession = pManager->getSession(pDoc);
	UT_return_val_if_fail(pSession, m_pOldSaveEM->Fn(v, d));

	if(saveRemotely(pDoc))
	{
	    XAP_Frame * pFrame = static_cast<XAP_Frame *> (pView->getParentData());
	    if (pFrame->getViewNumber() > 0)
	      XAP_App::getApp()->updateClones(pFrame);
	    return true;
	}
	UT_DEBUGMSG(("This session does not use the abicollab webservice; saving the old fashioned way...\n"));
	return m_pOldSaveEM->Fn(v, d);
}

void AbiCollabSaveInterceptor::_save_cb(UT_Error error, AbiCollab* pSession)
{
	UT_DEBUGMSG(("AbiCollabSaveInterceptor::_save_cb()\n"));

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);	

	pManager->endAsyncOperation(pSession);
	
	// WARNING: do NOT assume we have a valid view or frame here: it could already 
	// have been deleted if the frame was closed (or abiword shutdown) before this 
	// callback came back.
	// You can safely use the AbiCollab pointer or PD_Document pointer though, as
	// the AbiCollabSessionManager makes sure those are still valid.
	
	if (error != UT_OK)
	{
		// idealy we would use the same frame that was used to save the document,
		// but we don't know if that one is still valid
		if (XAP_App::getApp()->getLastFocussedFrame())
		{
			// TODO: add the document name, error type and perhaps the server name
			// TODO: offer some kind of solution to the user
			UT_UTF8String msg("An error occured while saving this document to the web-service!");
			XAP_App::getApp()->getLastFocussedFrame()->showMessageBox(msg.utf8_str(), XAP_Dialog_MessageBox::b_O, XAP_Dialog_MessageBox::a_OK);
		}
	}
	return;
}
