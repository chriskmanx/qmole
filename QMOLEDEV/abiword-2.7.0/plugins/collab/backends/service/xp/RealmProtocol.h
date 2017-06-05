#ifndef __REALM_PROTOCOL__
#define __REALM_PROTOCOL__

#include <string>
#include <vector>
#include <stdint.h>
#include <boost/shared_ptr.hpp>
#include <asio.hpp>

namespace realm {

namespace protocol {

enum handshake_response {
	HANDSHAKE_RESERVED = 0,				/* 0x00: reserved */
	HANDSHAKE_OK,						/* 0x01: Login OK */
	HANDSHAKE_BAD_IDENTIFIER,			/* 0x02: Unrecognized protocol magic */
	HANDSHAKE_UNSUPPORTED_PROTOCOL,		/* 0x03: Unsupported protocol version */
	HANDSHAKE_INVALID_COOKIE			/* 0x04: Invalid cookie */
};

}
	
namespace protocolv1 {

class Packet;
typedef boost::shared_ptr<Packet> PacketPtr;
typedef boost::shared_ptr<asio::streambuf> StreamPtr;

// the packet type values must match up the corresponding body_size in 
// RealmProtocol.cpp (ugly, but it's fast)!
enum packet_type {
	PACKET_RESERVED = 0x00,
	PACKET_ROUTE, // 0x01
	PACKET_DELIVER, // 0x02
	PACKET_USERJOINED, // 0x03
	PACKET_USERLEFT, // 0x04
	PACKET_SESSIONTAKEOVER, // 0x05
	__LAST_PACKET__
};

class Packet {
public:
	virtual ~Packet() {}

	static PacketPtr construct(uint8_t type);

	static bool s_valid(unsigned char type);
	static uint32_t s_body_size(unsigned char type);

	// Should be overwritten when the packet size is variable
	// Returns: the minimal number of bytes additionally needed before we can
	// re-assess if the packet is complete or not; -1 on error.
	// Note that this several complete() calls on the same buffer may return
	// several times with a value > 0. Only when complete() returns 0 the
	// buffer contains enough information to call parse().
	virtual int complete(const char* buf, size_t size);

	// should be overwritten when the packet 'has a body'
	// returns: the number of bytes read, -1 on error
	virtual int parse(const char* /*buf*/, size_t /*size*/) {
		return 0;
	}
		
	const uint8_t & type() const {
		return m_type;
	}	

protected:
	Packet(uint8_t type);

private:
	uint8_t m_type;
};

class PayloadPacket : public Packet {
public:
	virtual int complete(const char* buf, size_t size);
	virtual int parse(const char* buf, size_t size);

	const uint32_t& getPayloadSize() const {
		return m_payload_size;
	}
protected:
	PayloadPacket(uint8_t type, uint32_t min_payload_size, uint32_t payload_size)
		: Packet(type),
		m_min_payload_size(min_payload_size),
		m_payload_size(payload_size)
	{}
	PayloadPacket(uint8_t type, uint32_t min_payload_size)
		: Packet(type),
		m_min_payload_size(min_payload_size),
		m_payload_size(0)
	{}
private:
	uint32_t m_min_payload_size;
	uint32_t m_payload_size;
};

class RoutingPacket : public PayloadPacket {
public:
	RoutingPacket();
	RoutingPacket(std::vector<uint8_t>& connection_ids, boost::shared_ptr<std::string> msg);
	virtual int parse(const char* buf, size_t size);

	const uint8_t& getAddressCount() const {
		return m_address_count;
	}

	const std::vector<uint8_t>& getConnectionIds() const {
		return m_connection_ids;
	}	
	
	boost::shared_ptr<std::string> getMessage() const {
		return m_msg;
	}
	
private:
	uint8_t					m_address_count;	// a bit redundant (as it can be derived from m_connection_ids.size()), 
												// but it's convenient to be able to get a reference to this value 
												// when sending this packet
	std::vector<uint8_t>	m_connection_ids;
	boost::shared_ptr<std::string> m_msg;
};

class DeliverPacket : public PayloadPacket {
public:
	DeliverPacket();
	DeliverPacket(uint8_t connection_id, boost::shared_ptr<std::string> msg);
	virtual int parse(const char* buf, size_t size);
	
	const uint8_t& getConnectionId() const {
		return m_connection_id;
	}
	
	boost::shared_ptr<std::string> getMessage() const {
		return m_msg;
	}
	
private:
	uint8_t			m_connection_id;
	boost::shared_ptr<std::string>	m_msg;	
};

class UserJoinedPacket : public PayloadPacket {
public:
	UserJoinedPacket();
	UserJoinedPacket(uint8_t connection_id, bool master, boost::shared_ptr<std::string> userinfo);
	virtual int parse(const char* buf, size_t size);
	
	const uint8_t& getConnectionId() const {
		return m_connection_id;
	}
	
	const uint8_t& isMaster() const {
		return m_master;
	}
	
	boost::shared_ptr<std::string> getUserInfo() const {
		return m_userinfo;
	}

private:
	uint8_t			m_connection_id;
	uint8_t			m_master;
	boost::shared_ptr<std::string>		m_userinfo;
};

class UserLeftPacket : public Packet {
public:
	UserLeftPacket();
	UserLeftPacket(uint8_t connection_id);
	virtual int parse(const char* buf, size_t size);
	
	const uint8_t& getConnectionId() const {
		return m_connection_id;
	}
	
private:
	uint8_t m_connection_id;
};

class SessionTakeOverPacket : public Packet {
public:
	SessionTakeOverPacket();
};

}
}

#endif /* __REALM_PROTOCOL__ */
