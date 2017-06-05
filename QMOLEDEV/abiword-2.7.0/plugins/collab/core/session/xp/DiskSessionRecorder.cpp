/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
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

#include <gsf/gsf-output-stdio.h>
#include <account/xp/Buddy.h>
#include <plugin/xp/AbiCollab_Plugin.h>
#include "DiskSessionRecorder.h"

DiskSessionRecorder::DiskSessionRecorder(AbiCollab* pSession)
	: SessionRecorderInterface(pSession)
{
	UT_DEBUGMSG(("DiskSessionRecorder::DiskSessionRecorder()\n"));
	
	std::string pidStr;
#ifndef WIN32
	pidStr = str(boost::format( "%1%" ) % int(getpid()) );
#endif
	gchar *s = g_build_filename( getTargetDirectory(), (std::string(getPrefix())+pSession->getSessionId().utf8_str()).c_str(), NULL );
	std::string fn = (char*)s;
	fn += ".";
	fn += pidStr;
	FREEP(s);
	
	FILE* file = fopen( fn.c_str(), "wb" );
	if (file) {
		setbuf( file, NULL ); // disable file caching
		
		m_URI = UT_go_filename_to_uri( fn.c_str() );
		m_Error = NULL;
		m_GsfStream = gsf_output_stdio_new_FILE( m_URI, file, FALSE );

		if (m_GsfStream)
		{
			UT_DEBUGMSG(("DiskSessionRecorder: writing to file %s\n", fn.c_str()));
			write( getHeader(), strlen(getHeader()) );
			UT_sint32 version = ABICOLLAB_PROTOCOL_VERSION;
			write( &version, sizeof(version) );
			char bLocallyControlled = pSession->isLocallyControlled();
			write( &bLocallyControlled, sizeof(bLocallyControlled) );
		}
		else
		{
			UT_DEBUGMSG(("DiskSessionRecorder: stopping, can't write to file %s\n", fn.c_str()));
		}
	} else {
		m_URI = NULL;
		m_Error = NULL;
		m_GsfStream = NULL;
	}
}

DiskSessionRecorder::~DiskSessionRecorder() 
{
	destroy();
}

bool DiskSessionRecorder::getPackets(const std::string& filename, bool& bLocallyControlled, vector<RecordedPacket*>& packets)
{
	// open file
	GsfInput* in = UT_go_file_open(filename.c_str(), NULL);
	UT_return_val_if_fail(in,false);
	
	// read contents
	size_t fileSize = gsf_input_size(in);
	guint8 const* contents = gsf_input_read(in, fileSize, NULL);
	if (!contents)
	{
		g_object_unref(G_OBJECT(in));
		UT_return_val_if_fail(false,false);
	}
	
	// ugly: copy to string, because our serialization code expects a string
	std::string buffer;
	buffer.resize( fileSize );
	memcpy( &buffer[0], contents, fileSize );
	
	// check header
	size_t headerLen = strlen( DiskSessionRecorder::getHeader() );
	if (memcmp( DiskSessionRecorder::getHeader(), &buffer[0], headerLen )) 
	{
		UT_DEBUGMSG(("%s does not seem to be a session file\n", filename.c_str() ));
		UT_return_val_if_fail(false,false);
	}
	
	// check version
	UT_sint32 version = ABICOLLAB_PROTOCOL_VERSION;
	if (memcmp( &version, &buffer[headerLen], sizeof(version) ))
	{
		UT_DEBUGMSG(("%s has wrong version\n", filename.c_str() ));
		UT_return_val_if_fail(false,false);
	}
	
	bLocallyControlled = false;
	memcpy(&bLocallyControlled, &buffer[headerLen + sizeof(version)], sizeof(bLocallyControlled));
	UT_DEBUGMSG(("Session is %s\n",bLocallyControlled?"locally controlled":"not locally controlled"));
	
	// create stream, and skip header (is non-serialized-data)
	IStrArchive is( buffer );
	is.Skip( headerLen + sizeof(version) + sizeof(bLocallyControlled) );
	
	// keep reading packets
	try
	{
		while (!is.EndOfFile()) 
		{
			// read data for packet
			char incoming;
			is << incoming;
			
			char hasBuddy;
			is << hasBuddy;
			
			UT_UTF8String buddyName;
			if (hasBuddy)
			{
				is << buddyName;
			}
			
			UT_uint64 timestamp;
			is << timestamp;
			
			unsigned char packetClass;
			is << packetClass;
			// NOTE: we can safely cast to a session packet, as only session packets are recorded
			// TODO: add a safety check
			SessionPacket* newPacket = static_cast<SessionPacket*>(Packet::createPacket( (PClassType)packetClass ));
			if (!newPacket)
			{
				UT_DEBUGMSG(("unknown packet class %d\n", packetClass ));
				UT_return_val_if_fail(false,false);
			}
			is << *newPacket;
			
			packets.push_back(new RecordedPacket(incoming, hasBuddy, buddyName, timestamp, newPacket));
		}
		return true;
	}
	catch(...)
	{
		UT_DEBUGMSG(("exception caught during packet serialization\n"));
	}
	return false;
}

bool DiskSessionRecorder::dumpSession(const std::string& filename)
{
	bool bLocallyControlled;
	vector<RecordedPacket*> packets;

	if (DiskSessionRecorder::getPackets(filename, bLocallyControlled, packets))
	{
		UT_DEBUGMSG(("Session is %s\n",bLocallyControlled?"locally controlled":"not locally controlled"));
		
		UT_uint32 packetCounter = 0;
		for (vector<RecordedPacket*>::const_iterator cit = packets.begin(); cit != packets.end(); cit++)
		{
			const RecordedPacket* rp = *cit;
			
			// display packet
			printf("--------------------------------------------------------------------------------\n");
#ifndef WIN32
			// could someone find the equivalent of gmtime_r on win32? (or better: an XP function) 
			time_t t = time_t(rp->m_timestamp);
			struct tm time;
			gmtime_r( &t, &time );				
			printf("@ %04d/%02d/%02d %02d:%02d:%02d\n", 1900+time.tm_year, time.tm_mon, time.tm_mday, time.tm_hour, time.tm_min, time.tm_sec);
#endif
			printf("[%06u] %s packet ", packetCounter++, rp->m_bIncoming?"INCOMING":"OUTGOING" );
			printf("%s ", rp->m_bIncoming?"from":"to");
			if (rp->m_bHasBuddy)
			{
				printf("<%s>", rp->m_buddyName.utf8_str());
			}
			else
			{
				printf("<all>");
			}
			printf(" of class %s\n", Packet::getPacketClassname(rp->m_pPacket->getClassType()));
			printf("--------------------------------------------------------------------------------\n");
			printf("%s\n", rp->m_pPacket->toStr().c_str());
			printf("--------------------------------------------------------------------------------\n");
			
			DELETEP(rp);
		}
	}
	return true;
}

void DiskSessionRecorder::destroy()
{
	UT_DEBUGMSG(("DiskSessionRecorder::destroy: closing file 0x%x\n", m_GsfStream));
	if (m_GsfStream) 
	{
		gsf_output_close(m_GsfStream);
		g_object_unref(G_OBJECT(m_GsfStream));
		m_GsfStream = NULL;	// necessary? /me is gsf n00b
	}
	if (m_URI) 
	{
		FREEP(m_URI);
	}
}

void DiskSessionRecorder::store(bool incoming, const Packet* pPacket, BuddyPtr pBuddy)
{
	UT_DEBUGMSG(("DiskSessionRecorder::store: %d 0x%x %s\n", incoming, pPacket, pBuddy ? pBuddy->getDescriptor().utf8_str() : "none"));
	UT_return_if_fail(pPacket);
	UT_return_if_fail(m_GsfStream);
	OStrArchive os;
	
	// store if we're incoming or outgoing
	char incomingC = incoming;
	os << incomingC;

	// store if we had a buddy, and who it was
	char haveBuddy = pBuddy ? 1 : 0;
	os << haveBuddy;
	if (haveBuddy)
	{
		// FIXME: this is not guaranteed to be unique; a single author can join a session multiple times
		UT_UTF8String descr = pBuddy->getDescriptor();
		os << descr;
	}
	
	// store timestamp, make it 64-bit value always
	UT_uint64 timestamp = UT_uint64( time(0) );
	os << timestamp;

	// store packet class
	unsigned char packetClass = pPacket->getClassType();
	os << packetClass;

	// store packet
	os << const_cast<Packet&>(*pPacket);

	// write to file!
	write(os.getData().c_str(), os.Size());
}

void DiskSessionRecorder::write(const void* data, int count)
{
	UT_DEBUGMSG(("DiskSessionRecorder::write: 0x%x %d\n", data,count));
	UT_return_if_fail(m_GsfStream);
	gsf_output_write(m_GsfStream, size_t(count), reinterpret_cast<const guint8*>(data));
}
