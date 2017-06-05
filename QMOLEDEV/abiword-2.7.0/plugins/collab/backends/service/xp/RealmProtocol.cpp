#include "RealmProtocol.h"

namespace realm {
	
namespace protocolv1 {

#define MAX_PACKET_DATA_SIZE 64*1024*1024
		
#define RPV1_PACKET_NONEXISTENT -2
#define RPV1_PACKET_VARIABLE -1
	
static uint32_t body_size[6] = {
	RPV1_PACKET_NONEXISTENT, /* 0: reserved */
	RPV1_PACKET_VARIABLE, /* route; variable size */
	RPV1_PACKET_VARIABLE, /* deliver; variable size */
	RPV1_PACKET_VARIABLE, /* user joined */
	1, /* user left */
	0  /* session takeover */
};

PacketPtr Packet::construct(uint8_t type) {
	switch (type) {
		case 0x00: /* reserved */
			return PacketPtr();
		case 0x01: /* route */
			return PacketPtr(new RoutingPacket());
		case 0x02: /* deliver */
			return PacketPtr(new DeliverPacket());
		case 0x03: /* user joined */
			return PacketPtr(new UserJoinedPacket());
		case 0x04: /* user left */
			return PacketPtr(new UserLeftPacket());
		case 0x05: /* session takeover */
			return PacketPtr(new SessionTakeOverPacket());
	}
	
	return PacketPtr();
}

Packet::Packet(uint8_t type)
	: m_type(type)
{}

bool Packet::s_valid(unsigned char type) {
	return type < __LAST_PACKET__;
}

uint32_t Packet::s_body_size(unsigned char type) {
	return body_size[type];
}

int Packet::complete(const char* /*buf*/, size_t size) {
	if (size >= body_size[m_type])
		return 0;
	return body_size[m_type]-size;
}

int PayloadPacket::complete(const char* buf, size_t size) {
	if (size < 4+m_min_payload_size)
		return 4+m_min_payload_size-size;	
	uint32_t payload_size = 0;
	memcpy(&payload_size, &buf[0], 4); // FIXME: not Big Endian safe!
	if (payload_size > MAX_PACKET_DATA_SIZE)
		return -1;
	if (size >= 4+payload_size)
		return 0;
	return 4+payload_size-size;	
}

int PayloadPacket::parse(const char* buf, size_t size) {
	// read and check the payload size
	if (size < 4+m_min_payload_size)
		return -1;
	uint32_t payload_size = 0;
	memcpy(&payload_size, &buf[0], 4); // FIXME: not Big Endian safe!
	if (size < 4 + payload_size || payload_size < m_min_payload_size || payload_size > MAX_PACKET_DATA_SIZE)
		return -1;
	m_payload_size = payload_size;
	return 4;
}

RoutingPacket::RoutingPacket()
	: PayloadPacket(PACKET_ROUTE, 2),
	m_address_count(0),
	m_connection_ids(),
	m_msg()
{}

RoutingPacket::RoutingPacket(std::vector<uint8_t>& connection_ids, boost::shared_ptr<std::string> msg)
	: PayloadPacket(PACKET_ROUTE, 2, 1 + connection_ids.size() + msg->size()),
	m_address_count(connection_ids.size()),
	m_connection_ids(connection_ids),
	m_msg(msg)
{}

int RoutingPacket::parse(const char* buf, size_t size) {
	int parsed = PayloadPacket::parse(buf, size);
	if (parsed == -1)
		return -1;
	// get the recipients
	m_address_count = buf[parsed];
	if ((uint32_t)m_address_count + 1 > getPayloadSize())
		return -1;
	m_connection_ids.resize(m_address_count);
	std::copy(buf+parsed+1, buf+parsed+1+m_address_count, m_connection_ids.begin());
	// get the message data
	uint32_t msg_start = parsed+1+m_address_count;
	uint32_t msg_size = getPayloadSize() - 1 - m_address_count;
	m_msg.reset(new std::string(msg_size, '\0'));
	std::copy(buf+msg_start, buf+msg_start+msg_size, (*m_msg).begin());
	return parsed + getPayloadSize();
}

DeliverPacket::DeliverPacket()
	: PayloadPacket(PACKET_DELIVER, 1),
	m_connection_id(0),
	m_msg()
{}

DeliverPacket::DeliverPacket(uint8_t connection_id, boost::shared_ptr<std::string> msg)
	: PayloadPacket(PACKET_DELIVER, 1, 1 + msg->size()),
	m_connection_id(connection_id),
	m_msg(msg)
{}

int DeliverPacket::parse(const char* buf, size_t size) {
	int parsed = PayloadPacket::parse(buf, size);
	if (parsed == -1)
		return -1;
	// get the sender
	m_connection_id = buf[parsed];
	// get the message data
	uint32_t msg_start = parsed + 1;
	uint32_t msg_size = getPayloadSize() - 1;
	m_msg.reset(new std::string(msg_size, '\0'));
	std::copy(buf+msg_start, buf+msg_start+msg_size, (*m_msg).begin());
	return parsed + getPayloadSize();
}

UserJoinedPacket::UserJoinedPacket()
	: PayloadPacket(PACKET_USERJOINED, 2),
	m_connection_id(0),
	m_master(0),
	m_userinfo()
{}

UserJoinedPacket::UserJoinedPacket(uint8_t connection_id, bool master, boost::shared_ptr<std::string> userinfo)
	: PayloadPacket(PACKET_USERJOINED, 2, 2 + userinfo->size()),
	m_connection_id(connection_id),
	m_master(static_cast<uint8_t>(master)),
	m_userinfo(userinfo)
{}

int UserJoinedPacket::parse(const char* buf, size_t size) {
	int parsed = PayloadPacket::parse(buf, size);
	if (parsed == -1)
		return -1;
	// get the user that joined
	m_connection_id = buf[parsed];
	// get the master information
	m_master = buf[parsed+1];
	// get the userinfo
	uint32_t msg_start = parsed + 2;
	uint32_t msg_size = getPayloadSize() - 2;
	m_userinfo.reset(new std::string(msg_size, '\0'));
	std::copy(buf+msg_start, buf+msg_start+msg_size, (*m_userinfo).begin());
	return parsed + getPayloadSize();
}

UserLeftPacket::UserLeftPacket()
	: Packet(PACKET_USERLEFT),
	m_connection_id(0)
{}

UserLeftPacket::UserLeftPacket(uint8_t connection_id)
	: Packet(PACKET_USERLEFT),
	m_connection_id(connection_id)
{}

int UserLeftPacket::parse(const char* buf, size_t size) {
	if (size < 1)
		return false;
	m_connection_id = buf[0];
	return 1;
}

SessionTakeOverPacket::SessionTakeOverPacket()
	: Packet(PACKET_SESSIONTAKEOVER)
{}

}
}
