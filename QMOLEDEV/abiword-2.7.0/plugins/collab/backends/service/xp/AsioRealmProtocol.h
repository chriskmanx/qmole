#ifndef __ASIO_REALM_PROTOCOL__
#define __ASIO_REALM_PROTOCOL__

#include "RealmProtocol.h"

namespace realm {

namespace protocolv1 {

	template <typename WriteHandler>
	static void send(const RoutingPacket& p, asio::ip::tcp::socket& socket, WriteHandler handler) {
		std::vector<asio::const_buffer> bufs(4);
		bufs.push_back(asio::buffer(&p.type(), 1));
		bufs.push_back(asio::buffer(&p.getPayloadSize(), 4)); // FIXME: not Big Endian safe!
		bufs.push_back(asio::buffer(&p.getAddressCount(), 1));
		bufs.push_back(asio::buffer(&(p.getConnectionIds()[0]), p.getConnectionIds().size()));
		bufs.push_back(asio::buffer(*p.getMessage()));
		asio::async_write(socket, bufs, handler); 
	}

	template <typename WriteHandler>
	static void send(const DeliverPacket& p, asio::ip::tcp::socket& socket, WriteHandler handler) {
		std::vector<asio::const_buffer> bufs(4);
		bufs.push_back(asio::buffer(&p.type(), 1));
		bufs.push_back(asio::buffer(&p.getPayloadSize(), 4)); // FIXME: not Big Endian safe!
		bufs.push_back(asio::buffer(&p.getConnectionId(), 1));
		bufs.push_back(asio::buffer(*p.getMessage()));
		asio::async_write(socket, bufs, handler); 
	}	
	
	template <typename WriteHandler>
	static void send(const UserJoinedPacket& p, asio::ip::tcp::socket& socket, WriteHandler handler) {
		std::vector<asio::const_buffer> bufs(4);
		bufs.push_back(asio::buffer(&p.type(), 1));
		bufs.push_back(asio::buffer(&p.getPayloadSize(), 4)); // FIXME: not Big Endian safe!
		bufs.push_back(asio::buffer(&p.getConnectionId(), 1));
		bufs.push_back(asio::buffer(&p.isMaster(), 1));
		bufs.push_back(asio::buffer(*p.getUserInfo()));
		asio::async_write(socket, bufs, handler); 
	}
	
	template <typename WriteHandler>
	static void send(const UserLeftPacket& p, asio::ip::tcp::socket& socket, WriteHandler handler) {
		std::vector<asio::const_buffer> bufs(2);
		bufs.push_back(asio::buffer(&p.type(), 1));
		bufs.push_back(asio::buffer(&p.getConnectionId(), 1));
		asio::async_write(socket, bufs, handler); 
	}

	template <typename WriteHandler>
	static void send(const SessionTakeOverPacket& p, asio::ip::tcp::socket& socket, WriteHandler handler) {
		asio::async_write(socket, asio::buffer(&p.type(), 1), handler); 
	}

}

}

#endif /* ASIO_REALM_PROTOCOL__ */
