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

#ifndef __XMPPACCOUNTHANDLER__
#define __XMPPACCOUNTHANDLER__

#include <string>
#include <vector>
#include <map>

using std::string;
using std::map;

#include <loudmouth/loudmouth.h>
#include <libxml/tree.h>

#include <account/xp/AccountHandler.h>
#include "stdio.h"
#include "ut_string_class.h"
#include "ut_types.h"
#include "XMPPBuddy.h"

#define XMPP_RESOURCE "AbiCollab"

extern AccountHandlerConstructor XMPPAccountHandlerConstructor;
	
class XMPPAccountHandler : public AccountHandler
{
public:
	XMPPAccountHandler();
	virtual ~XMPPAccountHandler(void);

	// housekeeping
	static UT_UTF8String	getStaticStorageType();
	virtual UT_UTF8String	getStorageType()
		{ return getStaticStorageType(); }	
	virtual UT_UTF8String	getDescription();	
	virtual UT_UTF8String	getDisplayType();

	// connection management
	virtual ConnectResult	connect();
	virtual bool			disconnect(void);
	virtual bool			isOnline()
		{ return m_bLoggedIn; }		

	// asynchronous connection helper functions
	bool					authenticate();
	bool					setup();
	bool					tearDown();
	
	// dialog management
	virtual void			embedDialogWidgets(void* pEmbeddingParent) = 0;
	virtual void			removeDialogWidgets(void* pEmbeddingParent) = 0;
	virtual void			storeProperties() = 0;

	// user management
	virtual BuddyPtr		constructBuddy(const PropertyMap& vProps);
	virtual BuddyPtr		constructBuddy(const std::string& descriptor, BuddyPtr pBuddy);
	virtual bool			recognizeBuddyIdentifier(const std::string& identifier);
	virtual bool			allowsManualBuddies()
		{ return true; }
	virtual void			forceDisconnectBuddy(BuddyPtr) { /* TODO: implement me? */ }
	
	// session management
	virtual bool							allowsSessionTakeover()
		{ return false; } // no technical reason not to allow this; we just didn't implement session takeover for this backend yet	
	
		// packet management
	virtual bool			send(const Packet* pPacket);
	virtual bool 			send(const Packet* pPacket, BuddyPtr pBuddy);
	
	virtual void 			handleMessage(const gchar* packet_data, const std::string& from_address);

private:
	UT_UTF8String			_getNameFromFqa(const UT_UTF8String& fqa);
	bool					_send(const char* base64data, XMPPBuddyPtr pBuddy);
	XMPPBuddyPtr			_getBuddy(const std::string& from_address);
		
	// connection management
	LmConnection *			m_pConnection;
	LmMessageHandler *		m_pPresenceHandler;
	LmMessageHandler *		m_pStreamErrorHandler;
	LmMessageHandler *		m_pChatHandler;
	bool					m_bLoggedIn;
};

#endif /* __XMPPACCOUNTHANDLER__ */
