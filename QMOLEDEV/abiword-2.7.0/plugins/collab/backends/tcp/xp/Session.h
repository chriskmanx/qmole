/* Copyright (C) 2007,2008 by Marc Maurer <uwog@uwog.net>
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

#ifndef __SESSION__
#define __SESSION__

#include <boost/function.hpp>
#include <boost/bind.hpp>
#include <deque>
#include <sync/xp/lock.h>
#include <sync/xp/Synchronizer.h>

class TCPAccountHandler;

class Session : public Synchronizer, public boost::noncopyable
{
public:
	Session(asio::io_service& io_service, boost::function<void (Session&)> ef)
		: Synchronizer(boost::bind(&Session::_signal, this)),
		socket(io_service),
		queue_protector(),
		m_ef(ef)
	{
	}

	asio::ip::tcp::socket& getSocket()
	{	
		return socket;
	}

	void push(int size, char* data)
	{
		{
			abicollab::scoped_lock lock(queue_protector); 
			incoming.push_back( std::pair<int, char*>(size, data) );
		}
		Synchronizer::signal();
	}

	/*
		Only called fron the abiword main loop
	*/
	bool pop(int& size, char** data)
	{
		if (incoming.size() == 0)
			return false;
		{
			abicollab::scoped_lock lock(queue_protector); 
			std::pair<int, char*> p = incoming.front();
			size = p.first;
			*data = p.second;
			incoming.pop_front();
		}
		return true;
	}

	void asyncReadHeader()
	{
		UT_DEBUGMSG(("Session::asyncReadHeader()\n"));
		packet_data = 0; // just to be sure we'll never touch a datablock we might have read before
		asio::async_read(socket, 
			asio::buffer(&packet_size, 4),
			boost::bind(&Session::asyncReadHeaderHandler, this, asio::placeholders::error, asio::placeholders::bytes_transferred));
	}

	void asyncWrite(int size, const char* data)
	{
		// TODO: this is a race condition, mutex this
		bool writeInProgress = outgoing.size() > 0;

		// FIXME: inefficient memory copy
		char* store_data = reinterpret_cast<char*>(malloc(size));
		memcpy(store_data, data, size);
		outgoing.push_back(std::pair<int, char*>(size, store_data));

		if (!writeInProgress)
		{	
			packet_size_write = size;
			packet_data_write = store_data;
		
			UT_DEBUGMSG(("sending datablock of length: %d\n", packet_size_write));
			asio::async_write(socket, 
				asio::buffer(&packet_size_write, 4),
				boost::bind(&Session::asyncWriteHeaderHandler, this, asio::placeholders::error));
		}
	}

	/*
		Only called fron the abiword main loop
	*/
	bool isConnected()
	{
		return socket.is_open();
	}
	
	void disconnect()
	{
		UT_DEBUGMSG(("Session::disconnect()\n"));
		if (socket.is_open())
		{
			asio::error_code ecs;
			socket.shutdown(asio::ip::tcp::socket::shutdown_both, ecs);
			if (ecs) {
				UT_DEBUGMSG(("Error shutting down socket: %s\n", ecs.message().c_str()));
			}
			asio::error_code ecc;
			socket.close(ecc);
			if (ecc) {
				UT_DEBUGMSG(("Error closing socket: %s\n", ecc.message().c_str()));
			}
		}
		UT_DEBUGMSG(("Socket closed, signalling mainloop\n"));
		signal();
	}

private:
	void _signal()
	{
		UT_DEBUGMSG(("Session::_signal()\n"));
		m_ef(*this);
	}

	void asyncReadHeaderHandler(const asio::error_code& error,
		std::size_t bytes_transferred)
	{
		if (error)
		{
			UT_DEBUGMSG(("asyncReadHeaderHandler error: %s\n", error.message().c_str()));
			disconnect();
			return;
		}

		if (bytes_transferred != 4)
		{
			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
			disconnect(); // TODO: should not happen, handle this
			return;
		}
		
		UT_DEBUGMSG(("going to read datablock of length: %d\n", packet_size));
		// now continue reading the packet data
		packet_data = reinterpret_cast<char*>(malloc(packet_size));
		asio::async_read(socket,
			asio::buffer(packet_data, packet_size),
			boost::bind(&Session::asyncReadHandler, this, asio::placeholders::error, asio::placeholders::bytes_transferred));
	}

	void asyncReadHandler(const asio::error_code& error,
		std::size_t bytes_transferred)
	{
		if (error)
		{
			UT_DEBUGMSG(("asyncReadHandler generic error\n"));
			disconnect();
			return;
		}
		
		if (bytes_transferred != std::size_t(packet_size))
		{
			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
			disconnect(); // TODO: should not happen, handle this
			return;
		}
		
		push(packet_size, packet_data);
		// start over for a new packet
		asyncReadHeader();
	}
	
	void asyncWriteHeaderHandler(const asio::error_code& ec)
	{
		UT_DEBUGMSG(("Session::asyncWriteHeaderHandler()\n"));
		if (ec)
		{
			UT_DEBUGMSG(("asyncWriteHeaderHandler generic error\n"));
			disconnect();
			return;
		}
		
		// write the packet body
		asio::async_write(socket, 
			asio::buffer(packet_data_write, packet_size_write),
			boost::bind(&Session::asyncWriteHandler, this, asio::placeholders::error));
	}

	void asyncWriteHandler(const asio::error_code& ec)
	{
		UT_DEBUGMSG(("Session::asyncWriteHandler()\n"));
		FREEP(packet_data_write);
		if (ec)
		{
			UT_DEBUGMSG(("asyncWriteHandler generic error\n"));
			disconnect();
			return;			
		}

		// TODO: this is a race condition, mutex this
		outgoing.pop_front();
		if (outgoing.size() > 0)
		{
			std::pair<int, char*> p = outgoing.front();
			packet_size_write = p.first;
			packet_data_write = p.second;
			
			UT_DEBUGMSG(("sending datablock of length: %d\n", packet_size_write));

			asio::async_write(socket, 
				asio::buffer(&packet_size_write, 4),
				boost::bind(&Session::asyncWriteHeaderHandler, this, asio::placeholders::error));
		}
	}

	asio::ip::tcp::socket					socket;
	abicollab::mutex 							queue_protector;
	std::deque< std::pair<int, char*> >		incoming;
	std::deque< std::pair<int, char*> >		outgoing;

	int										packet_size; // state needed for async reads
	char*									packet_data; // state needed for async reads

	int										packet_size_write; // state needed for async writes
	char*									packet_data_write; // state needed for async writes
	
	boost::function<void (Session&)>		m_ef;
};

#endif /* __SESSION__ */
