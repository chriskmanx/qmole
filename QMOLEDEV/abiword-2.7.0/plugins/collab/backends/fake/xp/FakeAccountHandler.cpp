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

#include <string.h>
#include <ev_EditMethod.h>
#include <xap_App.h>
#include <fv_View.h>
#include <ut_assert.h>
#include <ie_exp.h>

#ifndef WIN32
// FIXME: this breaks OSX
#include "ap_UnixFrame.h"
#else
// TODO: fix includes
#endif

#include <FakeAccountHandler.h>
#include <account/xp/Event.h>
#include <account/xp/AccountEvent.h>
#include <account/xp/SessionEvent.h>
#include <session/xp/AbiCollabSessionManager.h>
#include <session/xp/AbiCollab.h>
#include <session/xp/DiskSessionRecorder.h>

FakeAccountHandler::FakeAccountHandler(const UT_UTF8String& sSessionURI, XAP_Frame* pFrame)
	: AccountHandler(),
	m_sSessionURI(sSessionURI),
	m_pFrame(pFrame),
	m_pSession(NULL),
	m_bLocallyControlled(false),
	m_pDoc(NULL),
	m_iIndex(1),
	m_iLocalRev(0),
	m_iRemoteRev(0)
{
	UT_DEBUGMSG(("FakeAccountHandler::FakeAccountHandler()\n"));
}

FakeAccountHandler::~FakeAccountHandler()
{
}

UT_UTF8String FakeAccountHandler::getDescription()
{
	return "Fake AbiCollab Backend";
}

UT_UTF8String FakeAccountHandler::getDisplayType()
{
	return "Fake AbiCollab Backend";
}

UT_UTF8String FakeAccountHandler::getStaticStorageType()
{
	return "com.abisource.abiword.abicollab.backend.fake";
}

void FakeAccountHandler::storeProperties()
{
}

ConnectResult FakeAccountHandler::connect()
{
	UT_ASSERT_HARMLESS(UT_NOT_REACHED);
	return CONNECT_SUCCESS;
}

bool FakeAccountHandler::disconnect()
{
	UT_ASSERT_HARMLESS(UT_NOT_REACHED);
	return true;
}

bool FakeAccountHandler::isOnline()
{
	return true;
}

FakeBuddyPtr FakeAccountHandler::getBuddy(const UT_UTF8String& description)
{
	for (std::vector<BuddyPtr>::iterator it = getBuddies().begin(); it != getBuddies().end(); it++)
	{
		FakeBuddyPtr pBuddy = boost::static_pointer_cast<FakeBuddy>(*it);
		UT_continue_if_fail(pBuddy);
		if (pBuddy->getDescription() == description)
			return pBuddy;
	}
	return FakeBuddyPtr();
}

BuddyPtr FakeAccountHandler::constructBuddy(const PropertyMap& props)
{
	UT_DEBUGMSG(("FakeAccountHandler::constructBuddy()\n"));

	PropertyMap::const_iterator cit = props.find("name");
	UT_return_val_if_fail(cit != props.end(), FakeBuddyPtr());
	UT_return_val_if_fail(cit->second.size() > 0, FakeBuddyPtr());

	UT_DEBUGMSG(("Constructing FakeBuddy (name: %s)\n", cit->second.c_str()));
	return boost::shared_ptr<FakeBuddy>(new FakeBuddy(this, cit->second.c_str()));
}

BuddyPtr FakeAccountHandler::constructBuddy(const std::string& /*descriptor*/, BuddyPtr /*pBuddy*/)
{
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
	return FakeBuddyPtr();
}

void FakeAccountHandler::forceDisconnectBuddy(BuddyPtr pBuddy)
{
	UT_return_if_fail(pBuddy);
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
}

bool FakeAccountHandler::recognizeBuddyIdentifier(const std::string& /*identifier*/)
{
	UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
	return false;
}

bool FakeAccountHandler::send(const Packet* pPacket)
{
	UT_DEBUGMSG(("FakeAccountHandler::send(const Packet* pPacket)\n"));
	UT_return_val_if_fail(pPacket, false);

	// TODO: implement me

	return true;
}

bool FakeAccountHandler::send(const Packet* pPacket, BuddyPtr /*pBuddy*/)
{
	UT_DEBUGMSG(("FakeAccountHandler::send(const Packet* pPacket, const Buddy& buddy)\n"));
	UT_return_val_if_fail(pPacket, false);

	return true;
}

bool FakeAccountHandler::initialize(UT_UTF8String* pForceSessionId)
{
	UT_DEBUGMSG(("FakeAccountHandler::initialize()"));
	
	UT_DEBUGMSG(("Loading recorded session\n"));
	UT_return_val_if_fail(_loadDocument(pForceSessionId), false);

	UT_DEBUGMSG(("Starting new collaboration session\n"));
	UT_return_val_if_fail(_createSession(), false);

	// store a property unique to this handler, so different fake account
	// handlers won't be treated as "equal"
	addProperty("sessionid", m_pSession->getSessionId().utf8_str());

	return true;
}

void FakeAccountHandler::cleanup()
{
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);	
	
	UT_return_if_fail(m_pSession);
	pManager->destroySession(m_pSession); // TODO: if we want to really close properly, we'd close/disjoin the session first
}
		
bool FakeAccountHandler::_loadDocument(UT_UTF8String* pForceSessionId)
{
	UT_DEBUGMSG(("FakeAccountHandler::_loadDocument()\n"));
	
	UT_return_val_if_fail(!m_pDoc, false);
	
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);	
	
	m_packets.clear();
	UT_return_val_if_fail(DiskSessionRecorder::getPackets(m_sSessionURI.utf8_str(), m_bLocallyControlled, m_packets), false);
	
	if (m_packets.size() == 0)
	{
		// NOTE: ugly, but the first recorded packet is always the state of the document
		UT_DEBUGMSG(("No recorded m_packets, nothing to do\n"));
		return true;
	}	

	if (pForceSessionId)
	{
		// force the session id of the packets to the given ID
		UT_DEBUGMSG(("Forcing the session ID of all recorded packets to %s\n", pForceSessionId->utf8_str()));
		
		for (std::vector<RecordedPacket*>::const_iterator cit = ++m_packets.begin(); cit != m_packets.end(); cit++)
		{
			RecordedPacket* pRp = *cit;
			UT_continue_if_fail (pRp && pRp->m_pPacket && SessionPacket::isInstanceOf(*pRp->m_pPacket));
			SessionPacket* sp = static_cast<SessionPacket*>(pRp->m_pPacket);
			sp->setSessionId(*pForceSessionId);
		}
	}
	
	// grab the first packet, which always contains the intial state of the document
	UT_return_val_if_fail(m_packets[0]->m_pPacket->getClassType() == PCT_JoinSessionRequestResponseEvent, false);
	JoinSessionRequestResponseEvent& jsrre = *static_cast<JoinSessionRequestResponseEvent*>(m_packets[0]->m_pPacket);
	
	// create a new documenyt to replay our recorded session in
	UT_return_val_if_fail(AbiCollabSessionManager::deserializeDocument(&m_pDoc, jsrre.m_sZABW, false) == UT_OK, false);
	UT_return_val_if_fail(m_pDoc, false);
	m_pDoc->setFilename(strdup("void"));
	
	if (m_bLocallyControlled)
	{
		m_pDoc->setCRNumber(jsrre.m_iRev); // we need to make sure the document is in the exact same state as when we started the collaboration session
		m_iLocalRev = jsrre.m_iRev;
	}
	else
		m_iRemoteRev = jsrre.m_iRev; // hacky, but works
	
	if (!m_pFrame)
	{	
		GR_Graphics* pG = NULL;
	
#ifndef WIN32
		// FIXME: this breaks on OSX
		
		// create a new frame, view and layout for the document
		m_pFrame = new AP_UnixFrame();

#else
		m_pFrame = new AP_Win32Frame();
#endif

		pG = GR_Graphics::newNullGraphics();
	
		FL_DocLayout* pLayout = new FL_DocLayout(m_pDoc, static_cast<GR_Graphics *>(pG));
		FV_View* pView = new FV_View(XAP_App::getApp(), m_pFrame, pLayout);
		m_pFrame->setView(pView);
		m_pFrame->setDoc(m_pDoc);
		pLayout->fillLayouts();
		pView->setPoint(2);
	}
	else
	{
		UT_DEBUGMSG(("Loading document in exiting frame\n"));
		m_pFrame->loadDocument(m_pDoc);
	}
	
	return true;
}

bool FakeAccountHandler::_createSession()
{
	UT_return_val_if_fail(m_pDoc, false);
	UT_return_val_if_fail(!m_pSession, false);
	UT_return_val_if_fail(m_packets.size() > 1, false);	
	
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);
	
	UT_return_val_if_fail(m_packets[0]->m_pPacket->getClassType() == PCT_JoinSessionRequestResponseEvent, false);
	JoinSessionRequestResponseEvent& jsrre = *static_cast<JoinSessionRequestResponseEvent*>(m_packets[0]->m_pPacket);	
	
	// do a quick scan through the m_packets to find the session and document id's
	UT_UTF8String sSessionId("");
	UT_UTF8String sDocUUID("");
	for (UT_uint32 i = 1; i < m_packets.size(); i++)
	{
		UT_continue_if_fail (m_packets[i] && m_packets[i]->m_pPacket && SessionPacket::isInstanceOf(*m_packets[i]->m_pPacket));
		SessionPacket* sp = static_cast<SessionPacket*>(m_packets[i]->m_pPacket);
		sSessionId = sp->getSessionId();
		sDocUUID = sp->getDocUUID();
		UT_DEBUGMSG(("Got session %s, docuuid: %s\n", sSessionId.utf8_str(), sDocUUID.utf8_str()));
		break;
	}
	UT_return_val_if_fail(sSessionId != "" && sDocUUID != "", false);
	
	if (m_bLocallyControlled)
	{
		UT_DEBUGMSG(("Starting a locally controlled collaboration session: %s\n", sSessionId.utf8_str()));
		// FIXME: we need to set the proper collab id and master descriptor
		m_pSession = pManager->startSession(m_pDoc, sSessionId, NULL, "fake://master");
	}
	else
	{
		UT_DEBUGMSG(("Joining a collaboration session: %s\n", sSessionId.utf8_str()));
		
		// do a quick scan through the m_packets to find a remote buddy (which is
		// automatically the session controller then
		BuddyPtr pCollaborator = FakeBuddyPtr();
		for (UT_uint32 i = 1; i < m_packets.size(); i++)
		{
			if (m_packets[i]->m_bHasBuddy)
			{
				pCollaborator = boost::shared_ptr<FakeBuddy>(new FakeBuddy(this, m_packets[i]->m_buddyName));
				break;
			}
		}
		
		if (!pCollaborator)
		{
			UT_DEBUGMSG(("The session controller never sent a changerecord, so we don't know who he is; inventing a name\n"));
			pCollaborator = boost::shared_ptr<FakeBuddy>(new FakeBuddy(this, "Fake::NoName"));
		}
		
		addBuddy(pCollaborator);
		XAP_Frame* pFrame = XAP_App::getApp()->newFrame(); // FIXME: this is a memory leak
		m_pSession = new AbiCollab(sSessionId, m_pDoc, sDocUUID /* FIXME: this is the local doc uuid, is that valid?? */, jsrre.m_iRev, pCollaborator, pFrame);
		pManager->joinSession(m_pSession, pCollaborator);
	}

	return true;
}

bool FakeAccountHandler::process()
{
	UT_DEBUGMSG(("FakeAccountHandler::process()\n"));

	UT_return_val_if_fail(m_pDoc, false);
	UT_return_val_if_fail(m_pSession, false);
	UT_return_val_if_fail(m_packets.size() > 1, false);
	
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);	

	// replay all actions
	for (std::vector<RecordedPacket*>::const_iterator cit = ++m_packets.begin(); cit != m_packets.end(); cit++)
	{
		RecordedPacket* pRp = *cit;
		FakeAccountHandler::_import(*pRp);
		DELETEP(pRp);
	}

	pManager->destroySession(m_pDoc);
	
	// write out the resulting document
	UT_UTF8String abwFile(m_sSessionURI + ".abw");
	GError* e = NULL;
	UT_go_file_remove(abwFile.utf8_str(), NULL);
	GsfOutput* sink = UT_go_file_create(abwFile.utf8_str(), &e);
	if (sink && !e)
	{
		UT_DEBUGMSG(("Writing regression test result to %s\n", abwFile.utf8_str()));
		/*UT_Error result =*/ m_pDoc->saveAs(sink, IE_Exp::fileTypeForSuffix(".abw"), true);
		gsf_output_close(GSF_OUTPUT(sink));
	}
	else
	{
		UT_DEBUGMSG(("Error writing regression test result to %s\n", abwFile.utf8_str()));
	}
		
	// FIXME: do we leak the layout here?
	// ...

	return true;
}

bool FakeAccountHandler::_import(const RecordedPacket& rp)
{
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, false);	
	
	UT_return_val_if_fail(rp.m_pPacket && SessionPacket::isInstanceOf(*rp.m_pPacket), false);
	SessionPacket* sp = static_cast<SessionPacket*>(rp.m_pPacket);
	
	if (rp.m_bIncoming)
	{
		UT_DEBUGMSG(("-> Replaying incoming remote packet from %s\n", rp.m_bHasBuddy?rp.m_buddyName.utf8_str():"<unknown>"));

		if (m_bLocallyControlled)
		{
			if (rp.m_bHasBuddy)
			{
				if (!getBuddy(rp.m_buddyName))
				{
					UT_DEBUGMSG(("Adding new buddy to the collaboration session: %s\n", rp.m_buddyName.utf8_str()));
					FakeBuddyPtr pNewCollaborator = boost::shared_ptr<FakeBuddy>(new FakeBuddy(this, rp.m_buddyName));
					addBuddy(pNewCollaborator);
					m_pSession->addCollaborator(pNewCollaborator);
				}
			}
			else
				UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
		}
		
		// a remote m_packets has been received; let it walk the normal path for incoming m_packets
		pManager->processPacket(*this, sp, getBuddy(rp.m_buddyName));
	}
	else
	{
		UT_DEBUGMSG(("-> Replaying local packet\n"));
		
		// just dump the packet right into the importer; no collision checking is
		// needed, because this packet was created locally
		switch (sp->getClassType())
		{
			case PCT_SignalSessionPacket:
				UT_DEBUGMSG(("Ignoring locally generated Signal packet, as it will be recreated by a changerecord\n"));
				break;
			case PCT_RevertSessionPacket:
				if (m_bLocallyControlled)
					UT_DEBUGMSG(("Ignoring locally generated RevertSessionPacket, as it should have been recreated by an already handled incoming changerecord\n"));
				else
					UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
				break;
			case PCT_RevertAckSessionPacket:
				if (!m_bLocallyControlled)
					UT_DEBUGMSG(("Ignoring locally generated RevertAckSessionPacket, as it should have been recreated by an already handled incoming changerecord\n"));
				else
					UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
				break;
			default:
				if (AbstractChangeRecordSessionPacket::isInstanceOf(*sp))
				{
					ABI_Collab_Import* pImport = m_pSession->getImport();
					FakeBuddyPtr pBuddy = boost::shared_ptr<FakeBuddy>(new FakeBuddy(this, "NULL"));
					pImport->_import(*sp, 0, pBuddy);
				}
				else
					UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
				break;
		}
	}

	return true;
}

bool FakeAccountHandler::getCurrentRev(UT_sint32& iLocalRev, UT_sint32& iRemoteRev)
{
	UT_DEBUGMSG(("FakeAccountHandler::getCurrentRev()\n"));
	
	UT_return_val_if_fail(m_pDoc, false);
	UT_return_val_if_fail(m_pSession, false);
	UT_return_val_if_fail(m_packets.size() > 1, false);
	
	iLocalRev = m_iLocalRev;	// don't query the PD_Document for the version number, as it will
								// return an incompatible change number when globs are involved
	iRemoteRev = m_iRemoteRev;	// we could also query the importer, but this is simpler for now
	return true;
}

bool FakeAccountHandler::stepToRemoteRev(UT_sint32 iRemoteRev)
{
	UT_DEBUGMSG(("FakeAccountHandler::stepToRemoteRev()\n"));
	
	UT_return_val_if_fail(m_pDoc, false);
	UT_return_val_if_fail(m_pSession, false);
	UT_return_val_if_fail(m_packets.size() > 1, false);

	for (UT_uint32 i = m_iIndex; i < m_packets.size(); i++)
	{
		m_iIndex = i + 1;
		RecordedPacket& rp = *m_packets[i];
		UT_continue_if_fail(rp.m_pPacket && SessionPacket::isInstanceOf(*rp.m_pPacket));
		SessionPacket* sp = static_cast<SessionPacket*>(rp.m_pPacket);

		if (rp.m_bIncoming)
		{
			_import(rp);
			if (AbstractChangeRecordSessionPacket::isInstanceOf(*sp))
			{
				m_iRemoteRev = static_cast<AbstractChangeRecordSessionPacket*>(sp)->getRev();
				if (m_iRemoteRev == iRemoteRev)
					break;
			}
		}
		else
		{
			_import(rp);
			if (AbstractChangeRecordSessionPacket::isInstanceOf(*sp))
				m_iLocalRev = static_cast<AbstractChangeRecordSessionPacket*>(sp)->getRev();
		}
	}	
	
	return true;
}

bool FakeAccountHandler::canStep()
{
	UT_DEBUGMSG(("FakeAccountHandler::canStep()\n"));
	
	UT_return_val_if_fail(m_pDoc, false);
	UT_return_val_if_fail(m_pSession, false);
	UT_return_val_if_fail(m_packets.size() > 1, false);

	for (UT_uint32 i = m_iIndex; i < m_packets.size(); i++)
	{
		RecordedPacket& rp = *m_packets[i];
		UT_continue_if_fail(rp.m_pPacket && SessionPacket::isInstanceOf(*rp.m_pPacket));
		SessionPacket* sp = static_cast<SessionPacket*>(rp.m_pPacket);

		if (!rp.m_bIncoming && AbstractChangeRecordSessionPacket::isInstanceOf(*sp))
			return true;
	}	
	
	return false;
}

bool FakeAccountHandler::step(UT_sint32& iLocalRev)
{
	UT_DEBUGMSG(("FakeAccountHandler::step()\n"));

	UT_return_val_if_fail(m_pDoc, false);
	UT_return_val_if_fail(m_pSession, false);
	UT_return_val_if_fail(m_packets.size() > 1, false);

	for (UT_uint32 i = m_iIndex; i < m_packets.size(); i++)
	{
		m_iIndex = i + 1;
		RecordedPacket& rp = *m_packets[i];
		UT_continue_if_fail(rp.m_pPacket && SessionPacket::isInstanceOf(*rp.m_pPacket));
		SessionPacket* sp = static_cast<SessionPacket*>(rp.m_pPacket);

		if (rp.m_bIncoming)
		{
			_import(rp);
			if (AbstractChangeRecordSessionPacket::isInstanceOf(*sp))
				m_iRemoteRev = static_cast<AbstractChangeRecordSessionPacket*>(sp)->getRev();
		}
		else
		{
			_import(rp);
			if (AbstractChangeRecordSessionPacket::isInstanceOf(*sp))
			{
				m_iLocalRev = iLocalRev = static_cast<AbstractChangeRecordSessionPacket*>(sp)->getRev();
				return true;
			}
		}
	}
	
	return false;
}
