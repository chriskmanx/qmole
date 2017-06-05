/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiCollab- Code to enable the modification of remote documents.
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
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

/*!
Connect current document to a remote document
*/
/*bool AbiCollab_Connect(AV_View* v, EV_EditMethodCallData *d)
{
	// Construct the Collaboration dialog
	XAP_DialogFactory * pDialogFactory
		= static_cast<XAP_DialogFactory *>(XAP_App::getApp()->getDialogFactory());
	
	AP_Dialog_Collaboration* pDialog = static_cast<AP_Dialog_Collaboration*> (pDialogFactory->requestDialog(s_CollabFactoryContainer.getDialogCollabId()));
	pDialog->runModal(pFrame);

...

	pDialogFactory->releaseDialog(pDialog);
	return true;
}*/

/*!
Disconnect the current document from a remote document
*/
/*bool AbiCollab_Disconnect(AV_View* v, EV_EditMethodCallData *d)
{
	// Get the current view that the user is in.
	XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
	FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());
	PD_Document * pDoc = static_cast<PD_Document *>(pFrame->getCurrentDoc());
	UT_UCS4String sUCS4(d->m_pData,d->m_dataLength);
	UT_UTF8String sUTF8(sUCS4);

	AbiCollab* pCollab = s_CollabFactoryContainer.get(pDoc, sUTF8);
	UT_return_val_if_fail(pCollab, false);
	pCollab->getExport()->signal(PD_SIGNAL_DOCCLOSED);
	
	// remove the document listener
	if (!pDoc->removeListener(pCollab->getDocListenerId()))
	{
		UT_DEBUGMSG(("Eeeek, couldn't remove document listener!\n"));
	}
		// remove the main loop idle handler
	if(pCollab->getRemoteListener())
	{
		pCollab->getRemoteListener()->stop();
		delete pCollab->getRemoteListener();
		pCollab->setRemoteListener(NULL);
	}
	
	// disconnect all connection handlers
	for (std::vector<AbiCollab_ConnectionHandler*>::const_iterator pos = pCollab->getConnections().begin(); pos != pCollab->getConnections().end(); pos++)
	{
		
		//AbiCollab_ConnectionHandler* handler = (*pos);
		//handler->disconnect();
		//DELETEP(handler);
	}
	
	// remove the collaboration object from the global list (will delete it as well)
	s_CollabFactoryContainer.destroy(pDoc, sUTF8);
	
	return true;
}*/

/*!
 * This method loads invokes AbiWord as a server to a remote document.
 * All the connect details are assumed to be in the call data.
 * This includes:
 * Local path-to-file to be loaded.
 * Name of Jabber server
 * Port of jabber server
 * username at Jabber server
 * password at Jabber server
 * name of remote user to connect.
 * server name of remote server to connect from
 * Start with the following command line..
 * AbiWord-2.6 --plugin AbiCollab myServer myPort myUserName myPassword remoteUser remoteServer pathToDocument 
 */
/*bool AbiCollab_invoke(AV_View * v, EV_EditMethodCallData * d)
{
	UT_UTF8String sInput(d->m_pData,d->m_dataLength);
	//
	// Need to Create an AbiCollab for this document.
	//
	UT_UCS4String sUCS4(d->m_pData,d->m_dataLength);
	UT_UTF8String sUTF8(sUCS4);
	UT_DEBUGMSG(("Creating Collab with string |%s| \n",sUTF8.utf8_str()));
	UT_UTF8String sServer,sPort,sUsername,sPassword;
	UT_UTF8String * psPathName = NULL;
	UT_UTF8String * psRemoteServer = NULL;
	UT_UTF8String * psRemoteUser = NULL;
	bool b = s_CollabFactoryContainer.extractParams(sUTF8,sServer,sPort,sUsername,sPassword,&psRemoteServer,&psRemoteUser,&psPathName);
	if(!b)
	{
	    UT_DEBUGMSG(("Error in command line startup! \n"));
	    return false;
	}
	//
	// TODO create an AbiCollab from an existing document name.	
	//
	AbiCollab* pCollab = s_CollabFactoryContainer.create(NULL, sUTF8,true,psPathName);
	UT_sint32 iPort = atoi(sPort.utf8_str());
//	b= pCollab->createHandler(sServer,iPort,sUsername,sPassword,NULL);
//	if(!b)
//		return false;
	//
	// We only accept edits here
	//
	pCollab->setOffering(true);
	//pCollab->startRemoteListener();

	UT_DEBUGMSG(("We are offering a collaboration!\n"));
	DELETEP(psPathName);
	DELETEP(psRemoteServer);
	DELETEP(psRemoteUser);

	//
	// OK block here until we get a quit
	//
	gtk_main();
	return true;
}*/
