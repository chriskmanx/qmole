/* Copyright (C) 2007 One Laptop Per Child
 * Author: Marc Maurer <uwog@uwog.net>
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

#include "SugarUnixAccountHandler.h"
#include "SugarBuddy.h"
#include <account/xp/AccountEvent.h>
#include <account/xp/Event.h>
#include <session/xp/AbiCollabSessionManager.h>
#include <session/xp/AbiCollab.h>
#include <ev_EditMethod.h>
#include <xap_App.h>
#include <fv_View.h>

// some fucntion prototype declarations
static bool s_offerTube(AV_View* v, EV_EditMethodCallData *d);
static bool s_joinTube(AV_View* v, EV_EditMethodCallData *d);
static bool s_buddyJoined(AV_View* v, EV_EditMethodCallData *d);
static bool s_buddyLeft(AV_View* v, EV_EditMethodCallData *d);
static DBusHandlerResult s_dbus_handle_message(DBusConnection *connection, DBusMessage *message, void *user_data);

#define INTERFACE "com.abisource.abiword.abicollab.olpc"
#define SEND_ALL_METHOD "SendAll"
#define SEND_ONE_METHOD "SendOne"

SugarAccountHandler* SugarAccountHandler::m_pHandler = NULL;
SugarAccountHandler* SugarAccountHandler::getHandler() { return m_pHandler; }

SugarAccountHandler::SugarAccountHandler()
	: AccountHandler(),
	m_pTube(NULL),
	m_bLocallyControlled(false)
{
	UT_DEBUGMSG(("SugarAccountHandler::SugarAccountHandler()\n"));
	m_pHandler = this;
	_registerEditMethods();
}

SugarAccountHandler::~SugarAccountHandler()
{
	m_pHandler = NULL;
	if (m_pTube)
	{
		dbus_connection_unref(m_pTube);
		m_pTube = NULL;
	}
}

UT_UTF8String SugarAccountHandler::getDescription()
{
	return "Sugar Presence Service";
}

UT_UTF8String SugarAccountHandler::getDisplayType()
{
	return "Sugar Presence Service";
}

UT_UTF8String SugarAccountHandler::getStaticStorageType()
{
	return "com.abisource.abiword.abicollab.backend.sugar";
}

void SugarAccountHandler::storeProperties()
{
	// no need to implement this as we will be getting
	// all our info always directly from sugar
}

ConnectResult SugarAccountHandler::connect()
{
	UT_ASSERT_HARMLESS(UT_NOT_REACHED);
	return CONNECT_SUCCESS;
}

bool SugarAccountHandler::disconnect()
{
	UT_ASSERT_HARMLESS(UT_NOT_REACHED);
	return true;
}

bool SugarAccountHandler::isOnline()
{
	return true;
}

BuddyPtr SugarAccountHandler::constructBuddy(const PropertyMap& props)
{
	UT_DEBUGMSG(("SugarAccountHandler::constructBuddy()\n"));

	PropertyMap::const_iterator cit = props.find("dbusAddress");
	UT_return_val_if_fail(cit != props.end(), SugarBuddyPtr());
	UT_return_val_if_fail(cit->second.size() > 0, SugarBuddyPtr());

	UT_DEBUGMSG(("Constructing SugarBuddy (dbusAddress: %s)\n", cit->second.c_str()));
	// NOTE: the buddy name must uniquely identify a buddy, and I can't
	// guarantee at the moment that the name we could get from the sugar
	// presence framework would always be unique to one buddy; hence the
	// dbus address will do for now
	return boost::shared_ptr<SugarBuddy>(new SugarBuddy(this, cit->second.c_str()));
}

BuddyPtr SugarAccountHandler::constructBuddy(const std::string& /*descriptor*/, BuddyPtr /*pBuddy*/)
{
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
	return SugarBuddyPtr();
}

bool SugarAccountHandler::recognizeBuddyIdentifier(const std::string& /*identifier*/)
{
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
	return false;
}

void  SugarAccountHandler::handleEvent(Session& /*pSession*/)
{
	// TODO: implement me
}

void SugarAccountHandler::signal(const Event& event, BuddyPtr pSource)
{
	AccountHandler::signal(event, pSource);

	UT_DEBUGMSG(("SugarAccountHandler::signal()\n"));

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);

	// we just want to listen for when we get a document handle from the other side
	// (this obviously only makes sense for a joining party, not an offering one;
	// the offering party should never even receive such an event
	if (event.getClassType() == PCT_AccountBuddyAddDocumentEvent)
	{
		UT_DEBUGMSG(("We received a document handle from an offering party; let's join it immediately!\n"));
		AccountBuddyAddDocumentEvent& abade = (AccountBuddyAddDocumentEvent&)event;

		if (!m_bLocallyControlled)
		{
			DocHandle* pDocHandle = abade.getDocHandle();
			if (pDocHandle)
			{
				UT_DEBUGMSG(("Got dochandle, going to initiate a join on it!\n"));
				// FIXME: remove const cast
				pManager->joinSessionInitiate(pSource, pDocHandle);
			}
			else
				UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
		}
		else
			UT_ASSERT_HARMLESS(UT_NOT_REACHED);
	}
}

bool SugarAccountHandler::send(const Packet* pPacket)
{
	UT_DEBUGMSG(("SugarAccountHandler::send(const Packet* pPacket)\n"));
	UT_return_val_if_fail(pPacket, false);
	UT_return_val_if_fail(m_pTube, false);

	// TODO: implement me

	return true;
}

bool SugarAccountHandler::send(const Packet* pPacket, BuddyPtr pBuddy)
{
	UT_DEBUGMSG(("SugarAccountHandler::send(const Packet* pPacket, const Buddy& buddy)\n"));
	UT_return_val_if_fail(pPacket, false);
	UT_return_val_if_fail(m_pTube, false);
	
	SugarBuddyPtr pSugarBuddy = boost::static_pointer_cast<SugarBuddy>(pBuddy);
	UT_DEBUGMSG(("Sending packet to sugar buddy on dbus addess: %s\n", pSugarBuddy->getDBusAddress().utf8_str()));

	DBusMessage* pMessage = dbus_message_new_method_call(pSugarBuddy->getDBusAddress().utf8_str(), "/org/laptop/Sugar/Presence/Buddies", INTERFACE, SEND_ONE_METHOD);
	// TODO: check dst
	/*bool dst =*/ dbus_message_set_destination(pMessage, pSugarBuddy->getDBusAddress().utf8_str());
	UT_DEBUGMSG(("Destination (%s) set on message\n", pSugarBuddy->getDBusAddress().utf8_str()));

	// we don't want replies, because then then easily run into dbus timeout problems 
	// when sending large packets
	// TODO: this means we should probably use signals though
	dbus_message_set_no_reply(pMessage, TRUE);
	
	// make to-be-send-stream once
	std::string data;
	_createPacketStream( data, pPacket );

	const char* packet_contents = &data[0];
	/*bool append =*/ dbus_message_append_args(pMessage,
					DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE, &packet_contents, data.size(),
					DBUS_TYPE_INVALID);
	UT_DEBUGMSG(("Appended packet contents\n"));

	bool sent = dbus_connection_send(m_pTube, pMessage, NULL);
	UT_ASSERT_HARMLESS(sent);
	if (sent)
		dbus_connection_flush(m_pTube);
	dbus_message_unref(pMessage);
	return sent;
}

Packet* SugarAccountHandler::createPacket(const std::string& packet, BuddyPtr pBuddy)
{
	return _createPacket(packet, pBuddy);
}

void SugarAccountHandler::_registerEditMethods()
{
	UT_DEBUGMSG(("SugarAccountHandler::_registerEditMethods()\n"));

    // First we need to get a pointer to the application itself.
    XAP_App *pApp = XAP_App::getApp();
    EV_EditMethodContainer* pEMC = pApp->getEditMethodContainer();

	EV_EditMethod *emOfferTube = new EV_EditMethod (
		"com.abisource.abiword.abicollab.olpc.offerTube",     // name of callback function
		s_offerTube,       // callback function itself.
		0,                      // no additional data required.
		""                      // description -- allegedly never used for anything
	);
	pEMC->addEditMethod(emOfferTube);

	EV_EditMethod *emJoinTube = new EV_EditMethod (
		"com.abisource.abiword.abicollab.olpc.joinTube",     // name of callback function
		s_joinTube,       // callback function itself.
		0,                      // no additional data required.
		""                      // description -- allegedly never used for anything
	);
	pEMC->addEditMethod(emJoinTube);

	EV_EditMethod *emBuddyJoined = new EV_EditMethod (
		"com.abisource.abiword.abicollab.olpc.buddyJoined",     // name of callback function
		s_buddyJoined,       // callback function itself.
		0,                      // no additional data required.
		""                      // description -- allegedly never used for anything
	);
	pEMC->addEditMethod(emBuddyJoined);

	EV_EditMethod *emBuddyLeft = new EV_EditMethod (
		"com.abisource.abiword.abicollab.olpc.buddyLeft",     // name of callback function
		s_buddyLeft,       // callback function itself.
		0,                      // no additional data required.
		""                      // description -- allegedly never used for anything
	);
	pEMC->addEditMethod(emBuddyLeft);

}

bool SugarAccountHandler::offerTube(FV_View* pView, const UT_UTF8String& tubeDBusAddress)
{
	UT_DEBUGMSG(("SugarAccountHandler::offerTube()\n"));
	UT_return_val_if_fail(pView, false);

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);

	// TODO: check that we aren't already in a session; this backend can only host one session at a time (for now)

	PD_Document * pDoc = pView->getDocument();
	UT_return_val_if_fail(pDoc, false);

	UT_DEBUGMSG(("Got tube address: %s\n", tubeDBusAddress.utf8_str()));

	m_pTube = dbus_connection_open(tubeDBusAddress.utf8_str(), NULL);
	if (m_pTube)
	{
		UT_DEBUGMSG(("Opened a dbus connection for tube: %s\n", tubeDBusAddress.utf8_str()));

		UT_DEBUGMSG(("Adding dbus handlers to the main loop\n"));
		dbus_connection_setup_with_g_main(m_pTube, NULL);

		UT_DEBUGMSG(("Adding message filter\n"));
		dbus_connection_add_filter(m_pTube, s_dbus_handle_message, this, NULL);

		m_bLocallyControlled = true;

		// we are "connected" now, time to start sending out, and listening to messages (such as events)
		pManager->registerEventListener(this);
		// start hosting a session on the current document
		UT_UTF8String sID;
		pManager->startSession(pDoc, sID, NULL, "");
		return true;
	}
	else
	{
		UT_DEBUGMSG(("Failed to open a dbus connection for tube: %s\n", tubeDBusAddress.utf8_str()));
	}

	return false;
}

bool SugarAccountHandler::joinTube(FV_View* pView, const UT_UTF8String& tubeDBusAddress)
{
	UT_DEBUGMSG(("SugarAccountHandler::joinTube()\n"));
	UT_return_val_if_fail(pView, false);

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);

	// TODO: check that we aren't already in a session; this backend can only join one session at a time (for now)

	m_pTube = dbus_connection_open(tubeDBusAddress.utf8_str(), NULL);
	if (m_pTube)
	{
		UT_DEBUGMSG(("Opened a dbus connection for tube: %s\n", tubeDBusAddress.utf8_str()));

		UT_DEBUGMSG(("Adding dbus handlers to the main loop\n"));
		dbus_connection_setup_with_g_main(m_pTube, NULL);

		UT_DEBUGMSG(("Adding message filter\n"));
		dbus_connection_add_filter(m_pTube, s_dbus_handle_message, this, NULL);

		m_bLocallyControlled = false;

		// we are "connected" now, time to start sending out, and listening to messages (such as events)
		pManager->registerEventListener(this);
	}
	else
	{
		UT_DEBUGMSG(("Failed to open a dbus connection for tube: %s\n", tubeDBusAddress.utf8_str()));
	}

	return false;
}

bool SugarAccountHandler::joinBuddy(FV_View* pView, const UT_UTF8String& buddyDBusAddress)
{
	UT_DEBUGMSG(("SugarAccountHandler::joinBuddy()\n"));
	UT_return_val_if_fail(pView, false);

	SugarBuddyPtr pBuddy = boost::shared_ptr<SugarBuddy>(new SugarBuddy(this, buddyDBusAddress));
	addBuddy(pBuddy);

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);

	PD_Document * pDoc = pView->getDocument();
	UT_return_val_if_fail(pDoc, false);

	if (m_bLocallyControlled)
	{
		AbiCollab* pSession = pManager->getSession(pDoc);
		UT_return_val_if_fail(pSession, false);
		pSession->addCollaborator(pBuddy);
		return true;
	}
	else
	{
		UT_DEBUGMSG(("Buddy joined, while we are NOT hosting a session; requesting sessions buddy: %s\n", pBuddy->getDescriptor(false).utf8_str()));
		getSessionsAsync(pBuddy);
		return true;
	}

	return false;
}

bool SugarAccountHandler::disjoinBuddy(FV_View* pView, const UT_UTF8String& buddyDBusAddress)
{
	UT_DEBUGMSG(("SugarAccountHandler::disjoinBuddy()\n"));
	UT_return_val_if_fail(pView, false);

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);

	PD_Document * pDoc = pView->getDocument();
	UT_return_val_if_fail(pDoc, false);
	
	m_ignoredBuddies.erase( buddyDBusAddress ); // buddy name is buddyDBusAddress!

 	if (m_bLocallyControlled)
	{
		UT_DEBUGMSG(("Dropping buddy %s from the session!", buddyDBusAddress.utf8_str()));
		AbiCollab* pSession = pManager->getSessionFromDocumentId(pDoc->getDocUUIDString());
		if (pSession)
		{	
			SugarBuddyPtr pTmpBuddy = boost::shared_ptr<SugarBuddy>(new SugarBuddy(this, buddyDBusAddress));
			pSession->removeCollaborator(pTmpBuddy);
			return true;
		}
	}
	else
	{
		UT_DEBUGMSG(("The session owner (%s) left!", buddyDBusAddress.utf8_str()));
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
		return true;
	}

	return false;
}

void SugarAccountHandler::forceDisconnectBuddy(BuddyPtr pBuddy)
{
	UT_return_if_fail(pBuddy);
	m_ignoredBuddies.insert(pBuddy->getDescriptor(false));
}

SugarBuddyPtr SugarAccountHandler::getBuddy(const UT_UTF8String& dbusAddress)
{
	for (std::vector<BuddyPtr>::iterator it = getBuddies().begin(); it != getBuddies().end(); it++)
	{
		SugarBuddyPtr pBuddy = boost::static_pointer_cast<SugarBuddy>(*it);
		UT_continue_if_fail(pBuddy);
		if (pBuddy->getDBusAddress() == dbusAddress)
			return pBuddy;
	}
	return SugarBuddyPtr();
}

static bool s_offerTube(AV_View* v, EV_EditMethodCallData *d)
{
	UT_DEBUGMSG(("s_offerTube()\n"));
	UT_return_val_if_fail(v, false);
	UT_return_val_if_fail(d && d->m_pData && d->m_dataLength > 0, false);

	FV_View* pView = static_cast<FV_View *>(v);
	UT_UTF8String tubeDBusAddress(d->m_pData, d->m_dataLength);

	SugarAccountHandler* pHandler = SugarAccountHandler::getHandler();
	UT_return_val_if_fail(pHandler, false);
	return pHandler->offerTube(pView, tubeDBusAddress);
}

static bool s_joinTube(AV_View* v, EV_EditMethodCallData *d)
{
	UT_DEBUGMSG(("s_joinTube()\n"));
	UT_return_val_if_fail(v, false);
	UT_return_val_if_fail(d && d->m_pData && d->m_dataLength > 0, false);

	FV_View* pView = static_cast<FV_View *>(v);
	UT_UTF8String tubeDBusAddress(d->m_pData, d->m_dataLength);
	UT_DEBUGMSG(("Got tube address: %s\n", tubeDBusAddress.utf8_str()));

	SugarAccountHandler* pHandler = SugarAccountHandler::getHandler();
	UT_return_val_if_fail(pHandler, false);
	return pHandler->joinTube(pView, tubeDBusAddress);
}

static bool s_buddyJoined(AV_View* v, EV_EditMethodCallData *d)
{
	UT_DEBUGMSG(("s_buddyJoined()\n"));
	UT_return_val_if_fail(SugarAccountHandler::getHandler(), false);
	UT_return_val_if_fail(d && d->m_pData && d->m_dataLength > 0, false);

	FV_View* pView = static_cast<FV_View *>(v);
	UT_UTF8String buddyPath(d->m_pData, d->m_dataLength);
	UT_DEBUGMSG(("Adding buddy with dbus path: %s\n", buddyPath.utf8_str()));

	SugarAccountHandler* pHandler = SugarAccountHandler::getHandler();
	UT_return_val_if_fail(pHandler, false);
	return pHandler->joinBuddy(pView, buddyPath);
}

static bool s_buddyLeft(AV_View* v, EV_EditMethodCallData *d)
{
	UT_DEBUGMSG(("s_buddyLeft()\n"));
	UT_return_val_if_fail(SugarAccountHandler::getHandler(), false);
	UT_return_val_if_fail(d && d->m_pData && d->m_dataLength > 0, false);

	FV_View* pView = static_cast<FV_View *>(v);
	UT_UTF8String buddyPath(d->m_pData, d->m_dataLength);
	UT_DEBUGMSG(("Removing buddy with dbus path %s\n", buddyPath.utf8_str()));

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);

	SugarAccountHandler* pHandler = SugarAccountHandler::getHandler();
	UT_return_val_if_fail(pHandler, false);

	if (pHandler->isLocallyControlled())
	{
		return pHandler->disjoinBuddy(pView, buddyPath);
	}
	else
	{
		// not much we can do here, but cry; kill off the entire handler
		pManager->destroyAccount(pHandler);
		return true;
	}
}

DBusHandlerResult s_dbus_handle_message(DBusConnection *connection, DBusMessage *message, void *user_data)
{
	UT_DEBUGMSG(("s_dbus_handle_message()\n"));
	UT_return_val_if_fail(connection, DBUS_HANDLER_RESULT_NOT_YET_HANDLED);
	UT_return_val_if_fail(message, DBUS_HANDLER_RESULT_NOT_YET_HANDLED);
	UT_return_val_if_fail(user_data, DBUS_HANDLER_RESULT_NOT_YET_HANDLED);
	SugarAccountHandler* pHandler = reinterpret_cast<SugarAccountHandler*>(user_data);

	if (dbus_message_is_method_call(message, INTERFACE, SEND_ONE_METHOD))
	{
		UT_DEBUGMSG(("%s message accepted!\n", SEND_ONE_METHOD));

		const char* senderDBusAddress = dbus_message_get_sender(message);

		DBusError error;
		dbus_error_init (&error);
		const char* packet_data = 0;
		int packet_size = 0;
	    if (dbus_message_get_args(message, &error, 
					DBUS_TYPE_ARRAY, DBUS_TYPE_BYTE, &packet_data, &packet_size,
					DBUS_TYPE_INVALID))
		{
			UT_DEBUGMSG(("Received packet from %s\n", senderDBusAddress));
			
			if (!pHandler->isIgnoredBuddy(senderDBusAddress))
			{
				// import the packet
				BuddyPtr pBuddy = pHandler->getBuddy(senderDBusAddress);
				if (!pBuddy)
				{
					UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
					pBuddy = boost::shared_ptr<SugarBuddy>(new SugarBuddy( pHandler, senderDBusAddress));
					pHandler->addBuddy(pBuddy);
				}

				// FIXME: inefficient copying of data
				std::string packet_str(' ', packet_size);
				memcpy(&packet_str[0], packet_data, packet_size);
				Packet* pPacket = pHandler->createPacket(packet_str, pBuddy);
				UT_return_val_if_fail(pPacket, DBUS_HANDLER_RESULT_NOT_YET_HANDLED); // TODO: shouldn't we just disconnect here?

				// handle!
				pHandler->handleMessage(pPacket, pBuddy);
			}

			//dbus_free(packet);
			return DBUS_HANDLER_RESULT_HANDLED;
		}
		else
			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
	}	

	UT_DEBUGMSG(("Unhandled message\n"));
	return DBUS_HANDLER_RESULT_NOT_YET_HANDLED;
}
