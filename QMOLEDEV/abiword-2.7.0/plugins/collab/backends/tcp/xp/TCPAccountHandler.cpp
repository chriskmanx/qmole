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

#include "ut_timer.h"
#include <boost/lexical_cast.hpp>

#include "TCPAccountHandler.h"
#include "TCPBuddy.h"

#include <account/xp/AccountEvent.h>
#include <session/xp/AbiCollabSessionManager.h>

TCPAccountHandler::TCPAccountHandler()
	: AccountHandler(),
	m_io_service(),
	m_work(m_io_service),
	m_thread(0),
	m_bConnected(false),
	m_pDelegator(0)
{
}

TCPAccountHandler::~TCPAccountHandler()
{
	UT_DEBUGMSG(("~TCPAccountHandler()\n"));
	if (m_bConnected)
		disconnect();
}

UT_UTF8String TCPAccountHandler::getDescription()
{
	const std::string host = getProperty("server");
	const std::string port = getProperty("port");	
	
	if (host == "")
		return UT_UTF8String_sprintf("Listening on port %s", port.c_str());
	return UT_UTF8String_sprintf("%s:%s", host.c_str(), port.c_str());
}

UT_UTF8String TCPAccountHandler::getDisplayType()
{
	return "Direct Connection (TCP)";
}

UT_UTF8String TCPAccountHandler::getStaticStorageType()
{
	return "com.abisource.abiword.abicollab.backend.tcp";
}

void TCPAccountHandler::storeProperties()
{
	UT_DEBUGMSG(("TCPAccountHandler::storeProperties() - TODO: implement me\n"));
}

ConnectResult TCPAccountHandler::connect()
{
	UT_DEBUGMSG(("TCPAccountHandler::connect()\n"));

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, CONNECT_INTERNAL_ERROR);

	UT_return_val_if_fail(!m_pDelegator, CONNECT_INTERNAL_ERROR);
	UT_return_val_if_fail(!m_bConnected, CONNECT_ALREADY_CONNECTED);
	UT_return_val_if_fail(!m_thread, CONNECT_INTERNAL_ERROR);
	m_io_service.reset();
	m_thread = new asio::thread(boost::bind(&asio::io_service::run, &m_io_service));

	// set up the connection
	if (getProperty("server") == "")
	{
		UT_sint32 port = _getPort(getProperties());
		UT_DEBUGMSG(("Start accepting connections on port %d...\n", port));

		try
		{
			IOServerHandler* pDelegator = new IOServerHandler(port, 
						boost::bind(&TCPAccountHandler::_handleAccept, this, _1, _2),
						boost::bind(&TCPAccountHandler::handleEvent, this, _1), m_io_service);
			m_pDelegator = pDelegator;
			m_bConnected = true; // todo: ask it to the acceptor
			pDelegator->run();
		}
		catch (asio::system_error se)
		{
			UT_DEBUGMSG(("Failed to start accepting connections: %s\n", se.what()));
			_teardownAndDestroyHandler();
			return CONNECT_FAILED;
		}
		catch (...)
		{
			UT_DEBUGMSG(("Caught unhandled server exception!\n"));
			_teardownAndDestroyHandler();
			return CONNECT_FAILED;
		}
	}
	else
	{
		UT_DEBUGMSG(("Connecting to server %s on port %d...\n", getProperty("server").c_str(), _getPort(getProperties())));
		boost::shared_ptr<Session> session_ptr(new Session(m_io_service, boost::bind(&TCPAccountHandler::handleEvent, this, _1)));
		asio::ip::tcp::resolver resolver(m_io_service);
		asio::ip::tcp::resolver::query query(getProperty("server"), getProperty("port"));
		asio::ip::tcp::resolver::iterator iterator(resolver.resolve(query));
		try
		{
			session_ptr->getSocket().connect(*iterator);
			session_ptr->asyncReadHeader();
			m_bConnected = true; // todo: ask it to the socket
			// Add a buddy
			TCPBuddyPtr pBuddy = boost::shared_ptr<TCPBuddy>(new TCPBuddy(this, getProperty("server"), getProperty("port")));
			addBuddy(pBuddy);
			m_clients.insert(std::pair<TCPBuddyPtr, boost::shared_ptr<Session> >(pBuddy, session_ptr));
		}
		catch (asio::system_error se)
		{
			UT_DEBUGMSG(("Failed to connect to server: %s\n", se.what()));
			_teardownAndDestroyHandler();
			return CONNECT_FAILED;
		}
	}
	
	if (!m_bConnected)
		return CONNECT_FAILED;

	// we are connected now, time to start sending out messages (such as events)
	pManager->registerEventListener(this);
	// signal all listeners we are logged in
	AccountOnlineEvent event;
	// TODO: fill the event
	AbiCollabSessionManager::getManager()->signal(event);

	return CONNECT_SUCCESS;
}

bool TCPAccountHandler::disconnect()
{
	UT_DEBUGMSG(("TCPAccountHandler::disconnect()\n"));

	UT_return_val_if_fail(m_bConnected, false);

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);
	
	_teardownAndDestroyHandler();
	m_bConnected = false;

	// signal all listeners we are logged out
	AccountOfflineEvent event;
	// TODO: fill the event
	AbiCollabSessionManager::getManager()->signal(event);
	// we are disconnected now, no need to sent out messages (such as events) anymore
	pManager->unregisterEventListener(this);	
	return true;
}

void TCPAccountHandler::_teardownAndDestroyHandler()
{
	// stop accepting new connections
	UT_DEBUGMSG(("Stop accepting connections/packets\n"));
	m_io_service.stop();
	if (m_thread)
	{
		m_thread->join();
		DELETEP(m_thread);
	}

	// ... then tear down all client connections
	UT_DEBUGMSG(("Tearing down client connections\n"));
	for (std::map<TCPBuddyPtr, boost::shared_ptr<Session> >::iterator it = m_clients.begin(); it != m_clients.end();)
		(*it).second->disconnect();

	// ... then stop the IOServerhandler (if any)
	if (m_pDelegator)
	{
		UT_DEBUGMSG(("Stopping the IOServerhandler\n"));
		m_pDelegator->stop();

		// ... and finally kill off the IO handler alltogether
		UT_DEBUGMSG(("Deleting the IO handler\n"));
		DELETEP(m_pDelegator);
	}
}

void TCPAccountHandler::_handleMessages(Session& session)
{
	UT_DEBUGMSG(("TCPAccountHandler::_handleMessages()\n"));
	
	// handle all packets waiting in our queue
	int packet_size;
	char* packet_data;
	while (session.pop(packet_size, &packet_data))
	{
		// get the buddy for this session
		TCPBuddyPtr pBuddy = _getBuddy(&session);
		UT_continue_if_fail(pBuddy); // TODO: shouldn't we just disconnect here?

		// construct the packet
		// FIXME: inefficient copying of data
		std::string packet_str(' ', packet_size);
		memcpy(&packet_str[0], packet_data, packet_size);
		FREEP(packet_data);
		Packet* pPacket = _createPacket(packet_str, pBuddy);
		UT_continue_if_fail(pPacket); // TODO: shouldn't we just disconnect here?

		// handle!
		handleMessage(pPacket, pBuddy);
	}	
}

bool TCPAccountHandler::isOnline()
{
	return m_bConnected; // TODO: ask the IO handler
}

void TCPAccountHandler::_handleAccept(IOServerHandler* pHandler, boost::shared_ptr<Session> session)
{
	UT_DEBUGMSG(("TCPAccountHandler::handleAccept\n"));
	UT_return_if_fail(pHandler);
	UT_return_if_fail(session);
	
	// store this buddy/session
	UT_UTF8String name;
	UT_UTF8String_sprintf(name, "%s:%d", 
			session->getSocket().remote_endpoint().address().to_string().c_str(), 
			session->getSocket().remote_endpoint().port());
	TCPBuddyPtr pBuddy = boost::shared_ptr<TCPBuddy>(new TCPBuddy(this, 
								session->getSocket().remote_endpoint().address().to_string(), 
								boost::lexical_cast<std::string>(session->getSocket().remote_endpoint().port())));
	addBuddy(pBuddy);
	m_clients.insert(std::pair<TCPBuddyPtr, boost::shared_ptr<Session> >(pBuddy, session));
	
	// accept a new buddy/session
	pHandler->asyncAccept();	
}

BuddyPtr TCPAccountHandler::constructBuddy(const PropertyMap& props)
{
	UT_DEBUGMSG(("TCPAccountHandler::constructBuddy()\n"));

	PropertyMap::const_iterator hi = props.find("server");
	UT_return_val_if_fail(hi != props.end(), BuddyPtr());
	UT_return_val_if_fail(hi->second.size() > 0, BuddyPtr());

	UT_sint32 port = _getPort(props);
	UT_return_val_if_fail(port != -1, BuddyPtr());
	
	UT_DEBUGMSG(("Constructing TCP Buddy (host: %s, port: %d)\n", hi->second.c_str(), port));
	return boost::shared_ptr<TCPBuddy>(new TCPBuddy(this, hi->second, boost::lexical_cast<std::string>(port)));
}

BuddyPtr TCPAccountHandler::constructBuddy(const std::string& /*descriptor*/, BuddyPtr /*pBuddy*/)
{
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
	return TCPBuddyPtr();
}

void TCPAccountHandler::forceDisconnectBuddy(BuddyPtr buddy)
{
	UT_DEBUGMSG(("TCPAccountHandler::forceDisconnectBuddy()\n"));
	UT_return_if_fail(buddy);
	TCPBuddyPtr pBuddy = boost::static_pointer_cast<TCPBuddy>(buddy);

	// locate this buddy!
	std::map<TCPBuddyPtr, boost::shared_ptr<Session> >::iterator it = m_clients.find(pBuddy);
	if (it == m_clients.end()) 
	{
		for (it = m_clients.begin(); it != m_clients.end(); ++it) 
		{
			if ((*it).first->getServer() == pBuddy->getServer() &&
				(*it).first->getPort() == pBuddy->getPort()) 
				break;
		}
		UT_return_if_fail(it != m_clients.end());
	}
	
	// disconnect it
	UT_return_if_fail(it != m_clients.end());
	(*it).second->disconnect();
}

bool TCPAccountHandler::recognizeBuddyIdentifier(const std::string& /*identifier*/)
{
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
	return false;
}

void TCPAccountHandler::handleEvent(Session& session)
{
	UT_DEBUGMSG(("TCPAccountHandler::handleEvent()\n"));

	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);

	// make sure we have handled _all_ packets in the queue before checking
	// the disconnected status
	bool disconnected = !session.isConnected();
	_handleMessages(session);
	
	// check the connection status
	if (disconnected)
	{
		UT_DEBUGMSG(("Socket is not connected anymore!\n"));
		// drop all buddies that were on this connection
		std::map<TCPBuddyPtr, boost::shared_ptr<Session> >::iterator next;
		for (std::map<TCPBuddyPtr, boost::shared_ptr<Session> >::iterator it = m_clients.begin(); it != m_clients.end(); it = next)
		{
			next = it;
			next++;
		
			UT_continue_if_fail((*it).first);
			UT_continue_if_fail((*it).second);

			TCPBuddyPtr pB = (*it).first;
			
			if ((*it).second.get() == &session)
			{
				UT_DEBUGMSG(("Lost connection to %s buddy %s:%s\n", getProperty("server") == "" ? "client" : "server", pB->getServer().c_str(), pB->getPort().c_str()));
				// drop this buddy from all sessions
				pManager->removeBuddy(pB, false);
				
				// erase the buddy <-> session mapping
				m_clients.erase(it);
				
				deleteBuddy(pB);
			}
		}

		// if we were connected to a server, then we are basically disconnected now
		if (getProperty("server") != "")
			disconnect();
	}

	// check other things here if needed...
}

UT_sint32 TCPAccountHandler::_getPort(const PropertyMap& props)
{
	PropertyMap::const_iterator pi = props.find("port");

	UT_sint32 port = -1;
	if (pi == props.end()) // no port specified, use the default port
	{
		port = DEFAULT_TCP_PORT;
	}
	else
	{
		long portl = strtol(pi->second.c_str(), (char **)NULL, 10);
		if (portl == LONG_MIN || portl == LONG_MAX) // TODO: we should check errno here for ERANGE
			port = DEFAULT_TCP_PORT;
		else
			port = (UT_sint32)portl;	
	}
	
	return port;
}

bool TCPAccountHandler::send(const Packet* packet)
{
	UT_DEBUGMSG(("TCPAccountHandler::_send(const UT_UTF8String& packet)\n"));

	// don't bother creating a nice buffer if there's noone to send it to
	if (!m_clients.empty()) 
	{
		// make to-be-send-stream once
		std::string data;
		_createPacketStream(data, packet);
		
		// send it to everyone we know!
		for (std::map<TCPBuddyPtr, boost::shared_ptr<Session> >::iterator it = m_clients.begin(); it != m_clients.end(); it++)
		{
			std::pair<TCPBuddyPtr, boost::shared_ptr<Session> > pbs = *it;
			if (pbs.second)
			{
				pbs.second->asyncWrite(data.size(), data.c_str());
			}
			else
				UT_ASSERT(UT_NOT_REACHED);
		}
	}
	return true;
}

bool TCPAccountHandler::send(const Packet* packet, BuddyPtr pBuddy)
{
	UT_DEBUGMSG(("TCPAccountHandler::_send(const UT_UTF8String& packet, const Buddy& buddy)\n"));
	UT_return_val_if_fail(pBuddy, false);
	TCPBuddyPtr pB = boost::static_pointer_cast<TCPBuddy>(pBuddy);
	
	// find the session
	std::map<TCPBuddyPtr, boost::shared_ptr<Session> >::iterator pos = m_clients.find(pB);
	if (pos != m_clients.end())
	{
		boost::shared_ptr<Session> session_ptr = pos->second;
		UT_return_val_if_fail(session_ptr, false);
		
		std::string data;
		_createPacketStream(data, packet);

		session_ptr->asyncWrite(data.size(), data.c_str());
		return true;
	}
	else
	{
		UT_DEBUGMSG(("TCPAccountHandler::send: buddy %s:%s doesn't exist, hope you were dragging something ;)\n", pB->getServer().c_str(), pB->getPort().c_str()));
	}
	return false;
}

TCPBuddyPtr TCPAccountHandler::_getBuddy(Session* pSession)
{
	for (std::map<TCPBuddyPtr, boost::shared_ptr<Session> >::iterator it = m_clients.begin(); it != m_clients.end(); it++)
	{
		std::pair<TCPBuddyPtr, boost::shared_ptr<Session> > pbs = *it;
		if (pbs.second.get() == pSession)
			return pbs.first;
	}
	UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
	return TCPBuddyPtr();
}
