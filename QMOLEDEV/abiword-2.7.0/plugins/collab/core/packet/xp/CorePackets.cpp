/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
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

#include "AbiCollab_Packet.h"

/* session packets */
REGISTER_PACKET(ChangeRecordSessionPacket)
REGISTER_PACKET(Props_ChangeRecordSessionPacket)
REGISTER_PACKET(InsertSpan_ChangeRecordSessionPacket)
REGISTER_PACKET(ChangeStrux_ChangeRecordSessionPacket)
REGISTER_PACKET(DeleteStrux_ChangeRecordSessionPacket)
REGISTER_PACKET(Object_ChangeRecordSessionPacket)
REGISTER_PACKET(Data_ChangeRecordSessionPacket)
REGISTER_PACKET(Glob_ChangeRecordSessionPacket)
REGISTER_PACKET(GlobSessionPacket)
REGISTER_PACKET(SignalSessionPacket)
REGISTER_PACKET(RevertSessionPacket)
REGISTER_PACKET(RevertAckSessionPacket)

/* session takeover packets */
REGISTER_PACKET(SessionTakeoverRequestPacket)
REGISTER_PACKET(SessionTakeoverAckPacket)
REGISTER_PACKET(SessionFlushedPacket)
REGISTER_PACKET(SessionReconnectRequestPacket)
REGISTER_PACKET(SessionReconnectAckPacket)
