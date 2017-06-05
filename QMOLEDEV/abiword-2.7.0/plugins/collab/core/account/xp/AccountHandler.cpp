/* Copyright (C) 2006,2007 by Marc Maurer <uwog@uwog.net>
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

#include "ut_path.h"

#include <xp/AbiCollab_Plugin.h>
#include <xp/AbiCollab.h>
#include <xp/AbiCollab_Export.h>
#include <xp/AbiCollabSessionManager.h>
#include "AccountEvent.h"
#include "AccountHandler.h"
#include "SessionEvent.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "xap_Dlg_MessageBox.h"
#include "pp_Author.h"
#include <set>

const string AccountHandler::getProperty(const string& key)
{ 
	PropertyMap::iterator pos = m_properties.find(key);
	if (pos != m_properties.end())
		return pos->second;
	else
		return "";
}

bool AccountHandler::autoConnect()
{
	const std::string autoconnect = getProperty("autoconnect");
	return strcmp(autoconnect.c_str(), "true") == 0;
}

bool AccountHandler::operator==(AccountHandler & rhHandler) {
	// Backends should override this if they need specific properties blacklisted beyond just autoconnect
	PropertyMap::iterator iter = m_properties.begin();
	PropertyMap::iterator otherMapIter;
	PropertyMap::iterator end = m_properties.end();
	
	// If different number of properties, then they can't be the same
	// Assumes that no items on the blacklist might be missing - this could be a false assumption.
	bool returnval=(m_properties.size() == rhHandler.m_properties.size());
	while (returnval && iter != end)
	{
		// Check to see if property is on the blacklist
		// TODO: replace this with a vector if we ever need more than one on the blacklist.
		if ((iter->first) != "autoconnect")
		{
			// not on the blacklist
			if ((otherMapIter = rhHandler.m_properties.find((iter->first))) != rhHandler.m_properties.end())
			{
				// if the other property map has this property
				returnval = ((iter->second) == (otherMapIter->second));
			}
		}
		iter++;
	}
	return returnval;
}

void AccountHandler::addBuddy(BuddyPtr pBuddy)
{
	UT_return_if_fail(pBuddy);
	m_vBuddies.push_back(pBuddy);
	
	// signal all listeners we have a new buddy
	AccountAddBuddyEvent event;
	// TODO: fill the event
	AbiCollabSessionManager::getManager()->signal(event);
}

void AccountHandler::deleteBuddy(BuddyPtr pBuddy)
{
	UT_return_if_fail(pBuddy);
	for (std::vector<BuddyPtr>::iterator it = m_vBuddies.begin(); it != m_vBuddies.end(); it++)
	{
		BuddyPtr pB = *it;
		UT_continue_if_fail(pB);
		if (pB == pBuddy)
		{
			m_vBuddies.erase(it);
			return;
		}
	}
	UT_ASSERT_HARMLESS(UT_NOT_REACHED);
}

void AccountHandler::deleteBuddies()
{
	m_vBuddies.clear();
}
		
void AccountHandler::getSessionsAsync()
{
	for (std::vector<BuddyPtr>::iterator it = m_vBuddies.begin(); it != m_vBuddies.end(); it++)
		getSessionsAsync(*it);
}

void AccountHandler::getSessionsAsync(BuddyPtr pBuddy)
{
	GetSessionsEvent event;
	send(&event, pBuddy);
}

void AccountHandler::joinSessionAsync(BuddyPtr pBuddy, DocHandle& docHandle)
{
	JoinSessionRequestEvent event( docHandle.getSessionId() );
	send(&event, pBuddy);
}

bool AccountHandler::hasSession(const UT_UTF8String& sSessionId)
{
	for (std::vector<BuddyPtr>::iterator it = m_vBuddies.begin(); it != m_vBuddies.end(); it++)
	{
		BuddyPtr pBuddy = *it;
		UT_continue_if_fail(pBuddy);
		if (pBuddy->getDocHandle(sSessionId))
			return true;
	}
	return false;
}

void AccountHandler::signal(const Event& event, BuddyPtr pSource)
{
	UT_DEBUGMSG(("AccountHandler::signal()\n"));

	// broadcast this event over our network (if applicable for each message type)
	std::vector<BuddyPtr> vRecipients = 
		(event.isBroadcast() ? getBuddies() : event.getRecipients());
	
	for (std::vector<BuddyPtr>::iterator it = vRecipients.begin(); it != m_vBuddies.end(); it++)
	{
		BuddyPtr pRecipient = *it;
		UT_continue_if_fail(pRecipient);

		if (!pSource || (pSource != pRecipient))
		{
			send(&event, pRecipient);
		}
		else
		{
			// the event originated from this buddy, so make sure not to send it
			// back to him, as it would result in a broadcast storm and
			// kill the network really fast
		}
	}
}

void AccountHandler::handleMessage(Packet* pPacket, BuddyPtr pBuddy)
{
	UT_return_if_fail(pPacket);
	UT_return_if_fail(pBuddy);	

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);
	
	//
	// handle the incoming packet: first check for a protocol error, then ask the 
	// session manager to handle it, then try to handle it ourselves
	//
	if (_handleProtocolError(pPacket, pBuddy) ||
		pManager->processPacket(*this, pPacket, pBuddy))
	{
		DELETEP(pPacket);
		return;
	}

	// it seems we need to handle the packet ourselves
	_handlePacket(pPacket, pBuddy);
	DELETEP(pPacket);
}

Packet* AccountHandler::_createPacket(const std::string& packet, BuddyPtr pBuddy)
{
	UT_return_val_if_fail(pBuddy, NULL);
	
	// create archive
	IStrArchive isa( packet );
	
	// serialize version
	int version;
	isa << COMPACT_INT(version);
	if (version != ABICOLLAB_PROTOCOL_VERSION)
	{
		if (version > 0)
		{
			UT_DEBUGMSG(("Discarding packet, wrong version %d (expected %d)\n", version, ABICOLLAB_PROTOCOL_VERSION));
			_sendProtocolError(pBuddy, PE_Invalid_Version);
			return NULL;
		}
		else
		{
			UT_DEBUGMSG(("Got error packet (hopefully), revision=%d\n", version));
			// if it's a version 0 message, handle normally, picked up in _handlePacket
		}
	}
	
	// serialize class id and attempt to reconstruct
	UT_uint8 classId;
	isa << classId;
	Packet* newPacket = Packet::createPacket( (PClassType)classId );
	if (!newPacket)
	{
		UT_DEBUGMSG(("Discarding packet, got unknown class %d\n", classId));
		return NULL;
	}
		
	// debug
	UT_DEBUGMSG(("PACKET DESERIALIZED: [%s] %u bytes in serialized string\n", Packet::getPacketClassname( (PClassType)classId ), isa.Size()));
	
	// serialize packet
	isa << *newPacket;
	
	return newPacket;
}

void AccountHandler::_createPacketStream( std::string& sString, const Packet* pPacket )
{
	UT_return_if_fail( pPacket );
	
	// create archive
	OStrArchive osa;
	
	// serialize version
	int version = pPacket->getProtocolVersion();
	osa << COMPACT_INT(version);
	
	// serialize class id
	UT_uint8 classId = pPacket->getClassType();
	osa << classId;
	
	// serialize packet
	osa << const_cast<Packet&>( *pPacket );		// it's a writing archive, so cast is safe
	
	sString = osa.getData();
	
	// debug
	UT_DEBUGMSG(("PACKET SENT: [%s] %u bytes in serialized string\n", Packet::getPacketClassname( (PClassType)classId ), osa.Size()));
}

bool AccountHandler::_handleProtocolError(Packet* packet, BuddyPtr buddy)
{
	// packet and buddy must always be set
	UT_return_val_if_fail(packet, false);
	UT_return_val_if_fail(buddy, false);

	// error report?
	if (packet->getClassType() != PCT_ProtocolErrorPacket)
		return false;

	// we have an error!
	ProtocolErrorPacket* pee = static_cast<ProtocolErrorPacket*>(packet);
	// report the error
	_reportProtocolError(pee->getRemoteVersion(), pee->getErrorEnum(), buddy);
	// and remove buddy
	forceDisconnectBuddy(buddy);
	return true;
}

void AccountHandler::_handlePacket(Packet* packet, BuddyPtr buddy)
{
	// packet and buddy must always be set
	UT_return_if_fail(packet);
	UT_return_if_fail(buddy);
	
	// as must the session manager
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);
	
	// manager didn't handle it, see what we can do
	switch (packet->getClassType()) 
	{			
		case PCT_JoinSessionRequestEvent:
		{
			JoinSessionRequestEvent* jse = static_cast<JoinSessionRequestEvent*>(packet);
			
			// lookup session
			AbiCollab* pSession = pManager->getSessionFromSessionId(jse->getSessionId());
			UT_return_if_fail(pSession);
		
			// lookup exporter
			ABI_Collab_Export* pExport = pSession->getExport();
			UT_return_if_fail(pExport);
			
			// lookup adjusts
			const UT_GenericVector<ChangeAdjust *>* pExpAdjusts = pExport->getAdjusts();
			UT_return_if_fail(pExpAdjusts);
		
			PD_Document* pDoc = pSession->getDocument();

			// add this author to the document if we don't recognize him
			UT_sint32 iAuthorId = -1;
			UT_UTF8String buddyDescriptor = buddy->getDescriptor();
			UT_GenericVector<pp_Author*> authors = pDoc->getAuthors();
			UT_DEBUGMSG(("Scanning %d authors to see if we recognize this buddy\n", authors.getItemCount()));
			for (UT_sint32 i = 0; i < authors.getItemCount(); i++)
			{
				pp_Author* pAuthor = authors.getNthItem(i);
				UT_continue_if_fail(pAuthor);

				const gchar* szDescriptor = NULL;
				pAuthor->getProperty("abicollab-descriptor", szDescriptor);
				if (!szDescriptor)
					continue;

				if (buddyDescriptor != szDescriptor)
					continue;

				// yay, we know this author!
				iAuthorId = pAuthor->getAuthorInt();
				UT_DEBUGMSG(("Found known author with descriptior %s, id %d!\n", buddyDescriptor.utf8_str(), iAuthorId));
				break;
			}
			
			if (iAuthorId == -1)
			{
				// we don't know this author yet, create a new author object for him
				iAuthorId = pDoc->findFirstFreeAuthorInt();
				pp_Author * pA = pDoc->addAuthor(iAuthorId);
				PP_AttrProp * pPA = pA->getAttrProp();
				pPA->setProperty("abicollab-descriptor", buddyDescriptor.utf8_str());
				pDoc->sendAddAuthorCR(pA);
				UT_DEBUGMSG(("Added a new author to the documument with descriptor %s, id %d\n", buddyDescriptor.utf8_str(), iAuthorId));
			}
			
			// serialize entire document into string
			JoinSessionRequestResponseEvent jsre(jse->getSessionId(), iAuthorId);
			if (AbiCollabSessionManager::serializeDocument(pDoc, jsre.m_sZABW, false /* no base64 */) == UT_OK)
			{
				// set more document properties
				jsre.m_iRev = pDoc->getCRNumber();
				jsre.m_sDocumentId = pDoc->getDocUUIDString();
				if (pDoc->getFilename())
					jsre.m_sDocumentName = UT_go_basename_from_uri(pDoc->getFilename());
				
				// send to buddy!
				send(&jsre, buddy);
				
				// add this buddy to the collaboration session
				pSession->addCollaborator(buddy);
			}
			break;
		}
		
		case PCT_JoinSessionRequestResponseEvent:
		{
			JoinSessionRequestResponseEvent* jsre = static_cast<JoinSessionRequestResponseEvent*>( packet );
			PD_Document* pDoc = 0;
			if (AbiCollabSessionManager::deserializeDocument(&pDoc, jsre->m_sZABW, false) == UT_OK)
			{
				if (pDoc)
				{
					// NOTE: we could adopt the same document name here, but i'd
					// rather not at the moment - MARCM
					pDoc->forceDirty();
					if (jsre->m_sDocumentName.size() > 0)
					{
						gchar* fname = g_strdup(jsre->m_sDocumentName.utf8_str());
						pDoc->setFilename(fname);
					}
					pManager->joinSession(jsre->getSessionId(), pDoc, jsre->m_sDocumentId, jsre->m_iRev, jsre->getAuthorId(), buddy, NULL);
				}
				else 
				{
					UT_DEBUGMSG(("AccountHandler::_handlePacket() - deserializing document failed!\n"));
				}
			}
			break;
		}
		
		case PCT_GetSessionsEvent:
		{
			GetSessionsResponseEvent gsre;
			const UT_GenericVector<AbiCollab *> sessions = pManager->getSessions();
			for (UT_sint32 i = 0; i < sessions.getItemCount(); i++)
			{
				AbiCollab* pSession = sessions.getNthItem(i);
				if (pSession && pSession->isLocallyControlled())
				{
					const PD_Document * pDoc = pSession->getDocument();
					if (pDoc)
					{
						// determine name
						UT_UTF8String documentBaseName;
						if (pDoc->getFilename())
							documentBaseName = UT_go_basename_from_uri(pDoc->getFilename());
						// set session info
						gsre.m_Sessions[ pSession->getSessionId() ] = documentBaseName;
					}
					else
						UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
				}
			}
			send(&gsre, buddy);
			break;
		}
		
		case PCT_GetSessionsResponseEvent:
		{
			GetSessionsResponseEvent* gsre = static_cast<GetSessionsResponseEvent*>( packet );
			UT_GenericVector<DocHandle*> vDocHandles;
			for (std::map<UT_UTF8String,UT_UTF8String>::iterator it=gsre->m_Sessions.begin(); it!=gsre->m_Sessions.end(); ++it) {
				DocHandle* pDocHandle = new DocHandle((*it).first, (*it).second);
				vDocHandles.addItem(pDocHandle);
			}
			pManager->setDocumentHandles(buddy, vDocHandles);
			break;
		}
		
		default:
		{
			UT_DEBUGMSG(("Unhandled packet class: 0x%x\n", packet->getClassType()));
			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
			break;
		}
	}
}

#ifdef WIN32
// return true if we process the command, false otherwise
BOOL AccountHandler::_onCommand(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	return false;
}
#endif

void AccountHandler::_reportProtocolError(UT_sint32 remoteVersion, UT_sint32 errorEnum, BuddyPtr /* buddy*/) 
{
#ifndef DEBUG
	UT_UNUSED(remoteVersion);
	UT_UNUSED(errorEnum);
#endif
	UT_DEBUGMSG(("_reportProtocolError: showProtocolErrorReports=%d remoteVersion=%d errorEnum=%d\n", showProtocolErrorReports, remoteVersion, errorEnum));
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
/*
	if (showProtocolErrorReports) {
		static std::set<std::string> reportedBuddies;

		if (reportedBuddies.insert( buddy.getName().utf8_str() ).second) {
			UT_UTF8String msg;
			switch (errorEnum) {
				case PE_Invalid_Version:
					msg = UT_UTF8String_sprintf("Your buddy %s is using a different version of collaboration software (expected %d, got %d).\n"
												"You will not be able to collaborate with him/her.", 
												buddy.getDescription().utf8_str(), // TODO: make the name more user-friendly
												ABICOLLAB_PROTOCOL_VERSION, remoteVersion);
					break;
				default:
					msg = UT_UTF8String_sprintf("An unknown error code %d was reported by buddy %s.", errorEnum,
													buddy.getDescription().utf8_str()); // TODO: make the name more user-friendly
					break;
			}
			XAP_App::getApp()->getLastFocussedFrame()->showMessageBox(
				msg.utf8_str(),
				XAP_Dialog_MessageBox::b_O,
				XAP_Dialog_MessageBox::a_OK);
		}
	}
*/
}

void AccountHandler::_sendProtocolError(BuddyPtr pBuddy, UT_sint32 errorEnum)
{
	UT_return_if_fail(pBuddy);
	ProtocolErrorPacket event(errorEnum);
	send(&event, pBuddy);
}

void AccountHandler::enableProtocolErrorReports(bool enable) 
{
	showProtocolErrorReports = enable;
}

bool AccountHandler::showProtocolErrorReports = true;

/*
 * ProtocolErrorPacket
 */
REGISTER_PACKET(ProtocolErrorPacket);

ProtocolErrorPacket::ProtocolErrorPacket()
: m_errorEnum(0)
, m_remoteVersion(0)
{
}

ProtocolErrorPacket::ProtocolErrorPacket( UT_sint32 errorEnum )
: m_errorEnum( errorEnum )
, m_remoteVersion( ABICOLLAB_PROTOCOL_VERSION )
{
}

void ProtocolErrorPacket::serialize(Archive & ar)
{
	Packet::serialize( ar );
	ar << COMPACT_INT(m_errorEnum) << COMPACT_INT(m_remoteVersion);
}
