/* Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
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
 
#ifndef __EVENTPACKET_H__
#define __EVENTPACKET_H__

#include <libxml/xmlreader.h>

#include "ut_types.h"
#include "ut_string_class.h"
#include "AbiCollab_Packet.h"

class EventPacket : public Packet
{
public:
	DECLARE_ABSTRACT_PACKET(EventPacket);
	
	virtual PacketType getType() const
		{ return PT_Event; }
};

#endif /* __EVENTPACKET_H__ */
