/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006,2007 by Marc Maurer <uwog@uwog.net>
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

#include "pd_Document.h"
#include "ut_bytebuf.h"
#include "ServiceAccountHandler.h"
#include "AbiCollabService_Export.h"
#include "AbiCollabSaveInterceptor.h"
#include "xap_Frame.h"
#include "xap_App.h"
#include "xav_View.h"

AbiCollabService_Export::AbiCollabService_Export(PD_Document* pDoc, ServiceAccountHandler* pService) : 
	m_pDoc(pDoc),
	m_pService(pService)
{
}

AbiCollabService_Export::~AbiCollabService_Export()
{
}

bool AbiCollabService_Export::populate(PL_StruxFmtHandle /*sfh*/, const PX_ChangeRecord * /*pcr*/)
{
	return true;
}

/*!
 * Implements the "populateStrux" method of the document listener class.
 * It takes the supplied change record and makes an XML-like representation
 * of it. Eventually these can either be stored in a file or sent over the
 * internet to a remote AbiWord where it can be translated back.
 */
bool AbiCollabService_Export::populateStrux(PL_StruxDocHandle /*sdh*/,
                                            const PX_ChangeRecord * /*pcr*/,
                                            PL_StruxFmtHandle * /*psfh*/)
{
	return true;
}

/*!
 * Implements the "change" method of the document listener class.
 * It takes the supplied change record and makes an XML-like representation
 * of it. Eventually these can either be stored in a file or sent over the
 * internet to a remote AbiWord where it can be translated back.
 */
bool AbiCollabService_Export::change(PL_StruxFmtHandle /*sfh*/,
                                     const PX_ChangeRecord * /*pcr*/)
{
	return true;
}


/*!
 * Implements the "insertStrux" method of the document listener class.
 * It takes the supplied change record and makes an XML-like representation
 * of it. Eventually these can either be stored in a file or sent over the
 * internet to a remote AbiWord where it can be translated back.
 */
bool AbiCollabService_Export::insertStrux(PL_StruxFmtHandle /*sfh*/,
                                          const PX_ChangeRecord * /*pcr*/,
                                          PL_StruxDocHandle /*sdh*/,
                                          PL_ListenerId /*lid*/,
                                          void (* /*pfnBindHandles*/)(PL_StruxDocHandle sdhNew,
															PL_ListenerId lid,
															PL_StruxFmtHandle sfhNew))
{
	return true;
}

/*!
 * Don't know if we need this method for abiCollab
 */
void  AbiCollabService_Export::deferNotifications(void)
{
}

/*!
 * Don't know if we need this method for abiCollab
 */
void AbiCollabService_Export::processDeferredNotifications(void)
{
}

/*!
 * Implements the signal() method of the Document listener class.
 */
bool AbiCollabService_Export::signal(UT_uint32 iSignal)
{
	if((iSignal == PD_SIGNAL_SAVEDOC) && m_pDoc->isDirty())
	{
		bool bSavedRemotely = ServiceAccountHandler::m_saveInterceptor.saveRemotely(m_pDoc);
		if(bSavedRemotely)
		{
			UT_GenericVector<AV_View *> vecViews;
			m_pDoc->getAllViews(&vecViews);
			AV_View * pView = vecViews.getNthItem(0);
			XAP_Frame * pFrame = static_cast<XAP_Frame *> (pView->getParentData());
			if (pFrame->getViewNumber() > 0)
				XAP_App::getApp()->updateClones(pFrame);

		}
		return bSavedRemotely;
	}
	return true;
}

/*!
 * This virtual method is called from the AbiWord main tree upon doing a replace document with an attached
 * AbiCollab_Export connected to the document.
 */
void AbiCollabService_Export::setNewDocument(PD_Document * /*pDoc*/)
{
	UT_DEBUGMSG(("AbiCollabService_Export::setNewDocument()\n"));
/*	ServiceAccountHandler* pService = ServiceAccountHandler::getService();
	UT_return_if_fail(pService);
	
	pService->closeDocument(m_iID, m_pDoc);
	m_pDoc = pDoc;*/
}

/*!
 * This virtual method is called if the attached document is deleted with an attached
 * AbiCollab_Export connected to the document.
 */
void AbiCollabService_Export::removeDocument(void)
{
	UT_DEBUGMSG(("AbiCollabService_Export::removeDocument()\n"));
	m_pService->removeExporter();
}
