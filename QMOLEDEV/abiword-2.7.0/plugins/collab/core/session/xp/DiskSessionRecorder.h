/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007 One Laptop Per Child
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

#ifndef ABICOLLAB_DISK_RECORDER_H
#define ABICOLLAB_DISK_RECORDER_H

#include <vector>

#include <string>
#include <time.h>

#include "xap_App.h"
#include "ut_assert.h"
#include "ut_go_file.h"

#include <session/xp/AbiCollab.h>

class RecordedPacket
{
public:
	RecordedPacket(bool bIncoming,bool bHasBuddy, const UT_UTF8String& buddyName, UT_uint64 timestamp, Packet* pPacket)
		: m_bIncoming(bIncoming),
		m_bHasBuddy(bHasBuddy),
		m_buddyName(buddyName),
		m_timestamp(timestamp),
		m_pPacket(pPacket),
		m_bDeleteAtDestroy(false)
	{}
	
	~RecordedPacket()
	{
		DELETEP(m_pPacket);
	}
	
	bool			m_bIncoming;
	bool			m_bHasBuddy;
	UT_UTF8String	m_buddyName;
	UT_uint64		m_timestamp;
	Packet*			m_pPacket;
	
	bool			m_bDeleteAtDestroy;
};

class DiskSessionRecorder : public SessionRecorderInterface
{
public:
	DiskSessionRecorder(AbiCollab* pSession);
	~DiskSessionRecorder();		
		
	static const char* getTargetDirectory()
		{ return XAP_App::getApp()->getUserPrivateDirectory(); }

	static const char* getPrefix()
		{ return "Session-"; }
	
	static bool getPackets(const std::string& filename, bool& bLocallyControlled, std::vector<RecordedPacket*>& packets);
		
	static bool	dumpSession(const std::string& filename);

	void storeOutgoing(const Packet* pPacket)
		{ store(false, pPacket, BuddyPtr()); }

	void storeOutgoing(const Packet* pPacket, BuddyPtr toBuddy)
		{ store(false, pPacket, toBuddy); }

	void storeIncoming(const Packet* pPacket, BuddyPtr fromBuddy)
		{ store(true, pPacket, fromBuddy); }

	static const char* getHeader()
		{ return "DSR!"; }

protected:
	GsfOutput* 		m_GsfStream;
	GError*			m_Error;
	const char*		m_URI;
	
	void destroy();
	void store(bool incoming, const Packet* pPacket, BuddyPtr pBuddy);
	void write(const void* data, int count);
};

#endif /* ABICOLLAB_DISK_RECORDER_H */
