/* Copyright (C) 2006-2008 by Marc Maurer <uwog@uwog.net>
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

#ifndef __TCPACCOUNTHANDLER__
#define __TCPACCOUNTHANDLER__

#include <boost/shared_ptr.hpp>
#include <core/account/xp/AccountHandler.h>

#include "IOServerHandler.h"
#include "TCPBuddy.h"

#define DEFAULT_TCP_PORT 25509  /* log2(e + pi) * 10^4 */

extern AccountHandlerConstructor TCPAccountHandlerConstructor;

class TCPAccountHandler : public AccountHandler
{
public:
	TCPAccountHandler();
	virtual ~TCPAccountHandler();

	// housekeeping
	static UT_UTF8String					getStaticStorageType();
	virtual UT_UTF8String					getStorageType()
		{ return getStaticStorageType(); }	
	virtual UT_UTF8String					getDescription();
	virtual UT_UTF8String					getDisplayType();
	
	// dialog management 
	virtual void							storeProperties();

	// connection management
	virtual ConnectResult					connect();
	virtual bool							disconnect();
	virtual bool							isOnline();

	// user management
	virtual BuddyPtr						constructBuddy(const PropertyMap& props);
	virtual BuddyPtr						constructBuddy(const std::string& descriptor, BuddyPtr pBuddy);
	virtual bool							recognizeBuddyIdentifier(const std::string& identifier);
	virtual bool							allowsManualBuddies()
		{ return false; }
	virtual void							forceDisconnectBuddy(BuddyPtr buddy);

	// session management
	virtual bool							allowsSessionTakeover()
		{ return false; }

	// packet management
	virtual bool							send(const Packet* packet);
	virtual bool							send(const Packet*, BuddyPtr pBuddy);

	// event management
	void									handleEvent(Session& session);

private:
	void									_teardownAndDestroyHandler();
	void									_handleMessages(Session& session);

	// user management
	TCPBuddyPtr								_getBuddy(Session* pSession);
	//TCPBuddy*								_getBuddy(const TCPBuddy* pBuddy);

	// connection management
	virtual UT_sint32						_getPort(const PropertyMap& props);
	void									_handleAccept(IOServerHandler* pHandler, boost::shared_ptr<Session> session);
		
	asio::io_service						m_io_service;
	asio::io_service::work					m_work;
	asio::thread*							m_thread;
	bool									m_bConnected; // TODO: drop this, ask the IO handler
	IOServerHandler*						m_pDelegator;

	std::map<TCPBuddyPtr, boost::shared_ptr<Session> >		m_clients; // mapping buddies and their accompanying session
};

#endif /* __TCPACCOUNTHANDLER__ */
