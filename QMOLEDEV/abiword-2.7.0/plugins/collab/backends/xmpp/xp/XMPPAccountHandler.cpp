/* 
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006-2008 by Marc Maurer <uwog@uwog.net>
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

#include <gsf/gsf-utils.h>

#include "pd_Document.h"
#include "ut_assert.h"
#include "ut_debugmsg.h"
#include "xap_Frame.h"
#include "xap_Strings.h"
#include "xap_App.h"
#include "xap_Dlg_MessageBox.h"

#include "XMPPAccountHandler.h"
#include "XMPPBuddy.h"

#include <account/xp/AccountEvent.h>
#include <account/xp/SessionEvent.h>

#include <session/xp/AbiCollabSessionManager.h>
#include <session/xp/AbiCollab.h>

#include <packet/xp/AbiCollab_Packet.h>
#include <packet/xp/EventPacket.h>

#include <plugin/xp/AbiCollab_Plugin.h>

static LmHandlerResult presence_handler(LmMessageHandler* /*handler*/,
					LmConnection * /*connection*/, LmMessage* m,
					gpointer /*user_data*/)
{
//	XMPPAccountHandler * pHandler = static_cast<XMPPAccountHandler *>(user_data);
	LmMessageNode* node = lm_message_get_node(m);
	if (node)
	{
		const char* from = static_cast<const char*>(lm_message_node_get_attribute (node, "from"));
		if (from)
		{
			const gchar* type = lm_message_node_get_attribute (node, "type");
			
			if (type && strcmp(type, "unavailable") == 0)
			{
				UT_DEBUGMSG(("Disconnect presence from %s\n", from));
				// TODO: handle this
			}
			else
			{
				UT_DEBUGMSG(("Connect presence from %s\n", from));
				// TODO: handle this
			}
		}
		else
		{
			UT_DEBUGMSG(("presence message without from\n"));
		}
	}
	return LM_HANDLER_RESULT_REMOVE_MESSAGE;
}

static LmHandlerResult stream_error_handler(LmMessageHandler* /*handler*/,
					LmConnection* /*connection*/, LmMessage* /*m*/,
					gpointer user_data)
{
	XMPPAccountHandler * pHandler = static_cast<XMPPAccountHandler *>(user_data);
	UT_return_val_if_fail(pHandler, LM_HANDLER_RESULT_REMOVE_MESSAGE);	

	/*LmMessageNode* node = lm_message_get_node(m);
	UT_DEBUGMSG(("Stream error message |%s|=|%s|\n", node->name, node->value));*/

	// FIXME: for now, we assume we are disconnected; not sure if this is always the case
	pHandler->disconnect();
	
	return LM_HANDLER_RESULT_REMOVE_MESSAGE;

}

static LmHandlerResult chat_handler(LmMessageHandler* /*handler*/,
				    LmConnection* /*connection*/, LmMessage* m,
				    gpointer user_data)
{
	XMPPAccountHandler * pHandler = static_cast<XMPPAccountHandler *>(user_data);
	UT_return_val_if_fail(pHandler, LM_HANDLER_RESULT_REMOVE_MESSAGE);
	
	/* TODO: we should run run through all the nodes to find the message node */
	LmMessageNode* node = lm_message_get_node(m);
	if (strcmp(node->name, "message") == 0)
	{
		for (LmMessageNode* child = node->children; child != 0; child = child->next)
		{
			if (strcmp(child->name, "body") == 0)
			{
				/*
				   Note: we don't trust a message body with a from-address in it, as it could be forged. 
				   Instead get the from-address directly from the LmMessage
				*/
				std::string buddy = lm_message_node_get_attribute (m->node, "from");
				std::string::size_type pos  = buddy.find_last_of("/");
				if (pos != std::string::npos)
					buddy.resize(pos);

				// TODO: check the resource as an additional sanity check

				UT_DEBUGMSG(("chat_handler(): Got a message from buddy: %s\n", buddy.c_str()));
				pHandler->handleMessage(child->value, buddy);
				break;
			}
		}
	}
	
	return LM_HANDLER_RESULT_REMOVE_MESSAGE;
}

static void lm_connection_open_async_cb(LmConnection* /*connection*/, gboolean success, gpointer user_data)
{
	UT_DEBUGMSG(("lm_connection_open_async_cb()\n"));

	XMPPAccountHandler * pHandler = static_cast<XMPPAccountHandler *>(user_data);
	UT_return_if_fail(pHandler);

	if (success)
	{
		pHandler->authenticate();
	}
	else
	{
		UT_DEBUGMSG(("Failed to open XMPP connection!\n"));
		pHandler->tearDown();
	}
}

static void lm_connection_authenticate_async_cb(LmConnection* /*connection*/, gboolean success, gpointer user_data)
{
	UT_DEBUGMSG(("lm_connection_authenticate_async_cb()\n"));

	XMPPAccountHandler * pHandler = static_cast<XMPPAccountHandler *>(user_data);
	UT_return_if_fail(pHandler);

	if (success)
	{
		pHandler->setup();
	}
	else
	{
		UT_DEBUGMSG(("Failed to authenticate against the XMPP server!\n"));
		pHandler->tearDown();
	}
}

/*
 * XMPPAccountHandler
 */

XMPPAccountHandler::XMPPAccountHandler():
	m_pConnection(NULL),
	m_pPresenceHandler(NULL),
	m_pStreamErrorHandler(NULL),
	m_pChatHandler(NULL),
	m_bLoggedIn(false)
{
}

XMPPAccountHandler::~XMPPAccountHandler()
{
	disconnect();
}

UT_UTF8String XMPPAccountHandler::getDescription()
{
	const std::string username = getProperty("username");
	const std::string server = getProperty("server");	
	return UT_UTF8String_sprintf("%s@%s", username.c_str(), server.c_str());
}

UT_UTF8String XMPPAccountHandler::getDisplayType()
{
	return "Jabber (XMPP)";
}

UT_UTF8String XMPPAccountHandler::getStaticStorageType()
{
	return "com.abisource.abiword.abicollab.backend.xmpp";
}

ConnectResult XMPPAccountHandler::connect()
{
	UT_DEBUGMSG(("XMPPAccountHandler::connect()\n"));

	if (m_bLoggedIn)
		return CONNECT_ALREADY_CONNECTED;
	
	if (m_pConnection)
		return CONNECT_IN_PROGRESS;

	// try to request a frame here; note that this might return 0, for example on application startup
	XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
	
	const std::string server = getProperty("server");
	const std::string username = getProperty("username");
	const std::string port = getProperty("port"); // TODO: unused atm
	const std::string resource = getProperty("resource");

	std::string jid = username + "@" + server;
	
	UT_DEBUGMSG(("Connecting to server: |%s|, username: |%s|, resource: |%s|\n",
	             server.c_str(), username.c_str(), resource.c_str()));
	m_pConnection = lm_connection_new(server.c_str());
	UT_return_val_if_fail(m_pConnection, CONNECT_INTERNAL_ERROR);

	lm_connection_set_jid(m_pConnection, jid.c_str());

	GError* error = NULL;
	if (!lm_connection_open(m_pConnection, lm_connection_open_async_cb, this, NULL, &error)) 
	{
		UT_DEBUGMSG(("Failed to open: %s\n", error ? error->message : ""));
		lm_connection_unref(m_pConnection);
		m_pConnection = NULL;
		
		if (pFrame)
		{
			// inform the user of the connection failure
			// TODO: this shouldn't be here, the caller should handle this
			UT_UTF8String msg;
			// TODO: make this localizable
			UT_UTF8String_sprintf(msg, "Error while connecting to %s: %s\n", server.c_str(), (error ? error->message : "")); 
			pFrame->showMessageBox(msg.utf8_str(), XAP_Dialog_MessageBox::b_O, XAP_Dialog_MessageBox::a_OK);
		}			
		return CONNECT_FAILED;
	}	

	return CONNECT_IN_PROGRESS;
}

bool XMPPAccountHandler::authenticate()
{
	UT_DEBUGMSG(("XMPPAccountHandler::authenticate()\n"));

	UT_return_val_if_fail(m_pConnection, false);

	// try to request a frame here; note that this might return 0, for example on application startup
	XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
	
	const std::string server = getProperty("server");
	const std::string username = getProperty("username");
	const std::string password = getProperty("password");
	const std::string resource = getProperty("resource");
	
	GError* error = NULL;
	if (!lm_connection_authenticate(m_pConnection, username.c_str(), password.c_str(),
				resource.c_str(), lm_connection_authenticate_async_cb, this, NULL, &error))
	{
		UT_DEBUGMSG(("connect() - couldn't authenticate with '%s' '%s':\n%s\n", 
				username.c_str(), password.c_str(), (error ? error->message : "")));
				
		lm_connection_close(m_pConnection, NULL);
		lm_connection_unref(m_pConnection);
		m_pConnection = NULL;
		
		if (pFrame)
		{
			// inform the user of the authentication failure
			// TODO: this shouldn't be here, the caller should handle this
			UT_UTF8String msg;
			// TODO: make this localizable
			UT_UTF8String_sprintf(msg, "Error while connecting to %s: %s\n", server.c_str(), (error ? error->message : "")); 
			pFrame->showMessageBox(msg.utf8_str(), XAP_Dialog_MessageBox::b_O, XAP_Dialog_MessageBox::a_OK);
		}

		return  false;
	}
	UT_DEBUGMSG(("connect() - user (%s) authenticated!\n", username.c_str()));

	return true;
}	

bool XMPPAccountHandler::setup()
{
	UT_DEBUGMSG(("XMPPAccountHandler::setup()\n"));

	UT_return_val_if_fail(m_pConnection, false);

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);	

	// try to request a frame here; note that this might return 0, for example on application startup
	XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();

	const std::string server = getProperty("server");

	// Register message handler for presence messages
	m_pPresenceHandler = lm_message_handler_new((LmHandleMessageFunction)presence_handler, reinterpret_cast< gpointer >(this), NULL);
	lm_connection_register_message_handler(m_pConnection, m_pPresenceHandler, LM_MESSAGE_TYPE_PRESENCE, LM_HANDLER_PRIORITY_NORMAL);

	// Register message handler for stream error messages
	m_pStreamErrorHandler = lm_message_handler_new((LmHandleMessageFunction)stream_error_handler, reinterpret_cast< gpointer >(this), NULL);
	lm_connection_register_message_handler(m_pConnection, m_pStreamErrorHandler, LM_MESSAGE_TYPE_STREAM_ERROR, LM_HANDLER_PRIORITY_NORMAL);

	// Register message handler for chat messages
	m_pChatHandler = lm_message_handler_new((LmHandleMessageFunction)chat_handler, reinterpret_cast< gpointer >(this), NULL);
	lm_connection_register_message_handler(m_pConnection, m_pChatHandler, LM_MESSAGE_TYPE_MESSAGE, LM_HANDLER_PRIORITY_NORMAL);

	// Send presence message to server
	GError* error = NULL;
	LmMessage* m = lm_message_new_with_sub_type(NULL, LM_MESSAGE_TYPE_PRESENCE, LM_MESSAGE_SUB_TYPE_NOT_SET);
	if (!lm_connection_send(m_pConnection, m, &error)) 
	{
		UT_DEBUGMSG(("Presence message could not be sent: %s", error ? error->message : ""));
		lm_connection_close(m_pConnection, NULL);
		lm_connection_unref(m_pConnection);
		m_pConnection = NULL;
		
		// FIXME: unregister the message handlers!
		// ...
		
		if (pFrame)
		{
			// inform the user of the sending error
			// TODO: this shouldn't be here, the caller should handle this
			UT_UTF8String msg;
			// TODO: make this localizable
			UT_UTF8String_sprintf(msg, "Error while connecting to %s: %s\n", server.c_str(), (error ? error->message : "")); 
			pFrame->showMessageBox(msg.utf8_str(), XAP_Dialog_MessageBox::b_O, XAP_Dialog_MessageBox::a_OK);			
		}
		
		return false;
	}
	lm_message_unref(m);

	m_bLoggedIn = true;

	// we are connected now, time to start sending out messages (such as events)
	pManager->registerEventListener(this);
	// signal all listeners we are logged in
	AccountOnlineEvent event;
	// TODO: fill the event
	AbiCollabSessionManager::getManager()->signal(event);
		
	return true;;
}

bool XMPPAccountHandler::disconnect()
{
	UT_DEBUGMSG(("XMPPAccountHandler::disconnect()\n"));

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);

	tearDown();
	
	// signal all listeners we are logged out
	AccountOfflineEvent event;
	// TODO: fill the event
	AbiCollabSessionManager::getManager()->signal(event);
	// we are disconnected now, no need to sent out messages (such as events) anymore
	pManager->unregisterEventListener(this);	
	
	return true;
}

bool XMPPAccountHandler::tearDown()
{
	if (m_pConnection)
	{
		// unregister and destroy the message handler callbacks
		if (m_pPresenceHandler)
		{
			lm_connection_unregister_message_handler(m_pConnection, m_pPresenceHandler, LM_MESSAGE_TYPE_PRESENCE);
			lm_message_handler_unref(m_pPresenceHandler);
			m_pPresenceHandler = NULL;
		}
	
		if (m_pStreamErrorHandler)
		{
			lm_connection_unregister_message_handler(m_pConnection, m_pStreamErrorHandler, LM_MESSAGE_TYPE_STREAM_ERROR);
			lm_message_handler_unref(m_pStreamErrorHandler);
			m_pStreamErrorHandler = NULL;
		}

		if (m_pChatHandler)
		{
			lm_connection_unregister_message_handler(m_pConnection, m_pChatHandler, LM_MESSAGE_TYPE_MESSAGE);
			lm_message_handler_unref(m_pChatHandler);
			m_pChatHandler = NULL;
		}
	
		lm_connection_close(m_pConnection, NULL);
		lm_connection_unref(m_pConnection);
		m_pConnection = NULL;
	}
	m_bLoggedIn = false;
	return true;
}

bool XMPPAccountHandler::send(const Packet* pPacket)
{
	UT_return_val_if_fail(pPacket, false);
	
	const std::string resource = getProperty("resource");

	// make to-be-send-stream once
	std::string data;
	_createPacketStream(data, pPacket);
	
	// XMPP doesn't like binary strings, base64 encode them
	guint8* base64data = gsf_base64_encode_simple(reinterpret_cast<guint8*>(&data[0]), data.size());
	UT_return_val_if_fail(base64data, false);
	
	for (std::vector<BuddyPtr>::iterator it = getBuddies().begin(); it != getBuddies().end(); it++)
	{
		XMPPBuddyPtr pBuddy = boost::static_pointer_cast<XMPPBuddy>(*it);
		UT_continue_if_fail(pBuddy);
		if (!_send(reinterpret_cast<char*>(base64data), pBuddy))
		{
			UT_DEBUGMSG(("Error while sending message to '%s'\n", pBuddy->getAddress().c_str()));
		}
	}
	g_free(base64data);
	
	return true;
}

bool XMPPAccountHandler::send(const Packet* pPacket, BuddyPtr pBuddy)
{
	UT_return_val_if_fail(pPacket, false);
	UT_return_val_if_fail(pBuddy, false);

	// make to-be-send-stream once
	std::string data;
	_createPacketStream(data, pPacket);
	
	// XMPP doesn't like binary strings, base64 encode them
	guint8* base64data = gsf_base64_encode_simple(reinterpret_cast<guint8*>(&data[0]), data.size());
	UT_return_val_if_fail(base64data, false);

	/*bool res = */_send(reinterpret_cast<char*>(base64data), boost::static_pointer_cast<XMPPBuddy>(pBuddy));
	g_free(base64data);
	
	return true;
}

bool XMPPAccountHandler::_send(const char* base64data, XMPPBuddyPtr pBuddy)
{
	UT_return_val_if_fail(base64data, false);
	UT_return_val_if_fail(pBuddy, false);
	
	if (!m_pConnection)
		return false;
	
	GError* error = NULL;
	
	// TODO: make sure these properties are always there
	const std::string resource = getProperty("resource");
	const std::string server = getProperty("server");

	// fully qualified address
	std::string fqa = pBuddy->getAddress() + "/" + resource;

	UT_DEBUGMSG(("Sending packet |%s| to |%s|\n", base64data, fqa.c_str()));
	LmMessage* m = lm_message_new (fqa.c_str(), LM_MESSAGE_TYPE_MESSAGE);
	lm_message_node_add_child (m->node, "body", base64data);
	if (!lm_connection_send (m_pConnection, m, &error))
	{
		UT_DEBUGMSG(("Error while sending message to '%s':\n%s\n",
				base64data, (error ? error->message : "") ));
		lm_message_unref(m);
		return false;
	}
	lm_message_unref(m);
	return true;
}

BuddyPtr XMPPAccountHandler::constructBuddy(const PropertyMap& vProps)
{
	PropertyMap::const_iterator pos = vProps.find("name");
	if (pos != vProps.end())
	{
		UT_return_val_if_fail(pos->second.size() > 0, XMPPBuddyPtr());
		UT_DEBUGMSG(("Constructing buddy (%s)\n", pos->second.c_str()));
		return XMPPBuddyPtr(new XMPPBuddy(this, pos->second.c_str()));
	}
	UT_ASSERT_HARMLESS(UT_NOT_REACHED);
	return XMPPBuddyPtr();
}

BuddyPtr XMPPAccountHandler::constructBuddy(const std::string& /*descriptor*/, BuddyPtr /*pBuddy*/)
{
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
	return XMPPBuddyPtr();
}

bool XMPPAccountHandler::recognizeBuddyIdentifier(const std::string& /*identifier*/)
{
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
	return false;
}

void XMPPAccountHandler::handleMessage(const gchar* packet_data, const std::string& from_address)
{
	UT_return_if_fail(packet_data);
	UT_return_if_fail(from_address.size() > 0);
	
	XMPPBuddyPtr pBuddy = _getBuddy(from_address);
	if (!pBuddy)
	{
		// yay, a message from a new buddy
		pBuddy = XMPPBuddyPtr(new XMPPBuddy(this, from_address.c_str()));
		addBuddy(pBuddy);
	}

	// construct the packet
	// NOTE: all packets are base64 encoded when sent over this backend, so we need to decode them
	// FIXME: inefficient copying of data
	std::string packet_str = packet_data;
	size_t len = gsf_base64_decode_simple((guint8*)(&packet_str[0]), packet_str.size());
	packet_str.resize(len);
	Packet* pPacket = _createPacket(packet_str, pBuddy);
	UT_return_if_fail(pPacket);

	AccountHandler::handleMessage(pPacket, pBuddy);
}

XMPPBuddyPtr XMPPAccountHandler::_getBuddy(const std::string& from_address)
{
	for (std::vector<BuddyPtr>::iterator it = getBuddies().begin(); it != getBuddies().end(); it++)
	{
		XMPPBuddyPtr pBuddy = boost::static_pointer_cast<XMPPBuddy>(*it);
		UT_continue_if_fail(pBuddy);
		if (pBuddy->getAddress() == from_address)
			return pBuddy;
	}
	return XMPPBuddyPtr();
}

