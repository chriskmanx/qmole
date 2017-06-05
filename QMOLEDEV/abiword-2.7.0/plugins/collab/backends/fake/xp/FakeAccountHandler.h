/* Copyright (C) 2007 One Laptop Per Child
 * Author: Marc Maurer <uwog@uwog.net>
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

#ifndef __FAKEACCOUNTHANDLER__
#define __FAKEACCOUNTHANDLER__

#include <account/xp/AccountHandler.h>
#include <account/xp/Buddy.h>
#include "FakeBuddy.h"

class RecordedPacket;
class AbiCollab;
class PD_Document;

class FakeAccountHandler : public AccountHandler
{
public:
	FakeAccountHandler(const UT_UTF8String& sSessionURI, XAP_Frame* pFrame);
	virtual ~FakeAccountHandler();

	// housekeeping
	static UT_UTF8String					getStaticStorageType();
	virtual UT_UTF8String					getStorageType()
		{ return getStaticStorageType(); }
	virtual UT_UTF8String					getDescription();
	virtual UT_UTF8String					getDisplayType();
	
	// dialog management 
	virtual void							storeProperties();
	virtual void							embedDialogWidgets(void* /*pEmbeddingParent*/)
		{ UT_ASSERT_HARMLESS(UT_NOT_REACHED); }
	virtual void							removeDialogWidgets(void* /*pEmbeddingParent*/)
		{ UT_ASSERT_HARMLESS(UT_NOT_REACHED); }

	// connection management
	virtual ConnectResult					connect();
	virtual bool							disconnect();
	virtual bool							isOnline();
	bool									isLocallyControlled()
		{ return false; }
	
	// user management
	FakeBuddyPtr							getBuddy(const UT_UTF8String& description);
	virtual BuddyPtr						constructBuddy(const PropertyMap& props);
	virtual BuddyPtr						constructBuddy(const std::string& descriptor, BuddyPtr pBuddy);
	virtual bool							allowsManualBuddies()
		{ return false; }
	virtual void							forceDisconnectBuddy(BuddyPtr pBuddy);
	virtual bool							recognizeBuddyIdentifier(const std::string& identifier);

	// session management
	virtual bool							allowsSessionTakeover()
		{ return false; } // not sure if sugar/tubes allow session takeover; should investigate - MARCM

	// packet management
	virtual bool							send(const Packet* pPacket);
	virtual bool							send(const Packet* pPacket, BuddyPtr pBuddy);
	
	// functions for the regression test
	bool									process();
		
	// functions for the debug test
	bool									getCurrentRev(UT_sint32& iLocalRev, UT_sint32& iRemoteRev);
	bool									stepToRemoteRev(UT_sint32 iRemoteRev);
	bool									canStep();
	bool									step(UT_sint32& iLocalRev);
	
	// misc. functions
	bool									initialize(UT_UTF8String* pForceSessionId);
	void									cleanup();
	XAP_Frame*								getFrame()
		{ return m_pFrame; }
	
private:
	bool									_loadDocument(UT_UTF8String* pForceSessionId);
	bool									_createSession();
	bool									_import(const RecordedPacket& rp);
	
	UT_UTF8String							m_sSessionURI;
	XAP_Frame*								m_pFrame;
	AbiCollab*								m_pSession;
	bool									m_bLocallyControlled;
	PD_Document*							m_pDoc;
	std::vector<RecordedPacket*>			m_packets;
	UT_sint32								m_iIndex;
	
	// variables for the debug test
	UT_sint32								m_iLocalRev;
	UT_sint32								m_iRemoteRev;
};

#endif /* __SUGARACCOUNTHANDLER__ */
