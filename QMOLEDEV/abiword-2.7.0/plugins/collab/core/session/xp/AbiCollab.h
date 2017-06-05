/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006,2007 by Marc Maurer <uwog@uwog.net>
 * Copyright (C) 2007 by One Laptop Per Child
 * Copyright (C) 2008 by AbiSource Corporation B.V.
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

#ifndef ABI_COLLAB_H
#define ABI_COLLAB_H

#include <map>
#include <vector>
#include "ev_EditBits.h"
#include "ev_MouseListener.h"
#include "ut_types.h"
#include "pt_Types.h"
#include "px_ChangeRecord.h"
#include "stdio.h"
#include "xav_Listener.h"
#include "pl_Listener.h"
#include "ut_string_class.h"
#include "ut_uuid.h"

#include <account/xp/Buddy.h>
#include <packet/xp/AbiCollab_Packet.h>
#include <session/xp/AbiCollab_Import.h>
#include <session/xp/AbiCollab_Export.h>

class FL_DocLayout;
class PD_Document;
class PX_ChangeRecord;
class CommandLine;
class Buddy;
class AccountHandler;

enum SessionTakeoverState
{
	STS_NONE,
	STS_SENT_TAKEOVER_REQUEST,
	STS_SENT_TAKEOVER_ACK,
	STS_SENT_SESSION_RECONNECT_REQUEST
};	

class ChangeAdjust
{
friend class AbiCollab_ImportRuleSet;
	
public:
	ChangeAdjust(const AbstractChangeRecordSessionPacket& packet, PT_DocPosition iOrigDocPos, const UT_UTF8String& sRemoteDocUUID);
	~ChangeAdjust();
	
	PT_DocPosition				getLocalPos() const	{ return m_iLocalPos; }
	void						setLocalPos(PT_DocPosition iLocalPos) { m_iLocalPos = iLocalPos; }
	UT_sint32					getLocalLength() const { return m_pPacket->getLength(); }
	UT_sint32					getLocalAdjust() const { return m_pPacket->getAdjust(); }
	UT_sint32					getLocalRev() const { return m_pPacket->getRev(); }
	
	PT_DocPosition				getRemoteDocPos() const	{ return m_iRemoteDocPos; }
	const UT_UTF8String&		getRemoteDocUUID() const { return m_sRemoteDocUUID; }
	
private:
	// locally generated data (possibly in response to remotely generated data)
	const AbstractChangeRecordSessionPacket*	m_pPacket;
	PT_DocPosition								m_iLocalPos;
	
	// remotely generated data
	PT_DocPosition								m_iRemoteDocPos;
	UT_UTF8String								m_sRemoteDocUUID;
};

class AbiCollab;
class SessionRecorderInterface
{
public:
	SessionRecorderInterface( AbiCollab* Session ) : m_pAbiCollab(Session) {}
	virtual ~SessionRecorderInterface() {}
	virtual void storeOutgoing(const Packet* pPacket ) = 0;
	virtual void storeOutgoing(const Packet* pPacket, BuddyPtr toBuddy) = 0;
	virtual void storeIncoming(const Packet* pPacket, BuddyPtr fromBuddy) = 0;
protected:
	AbiCollab*		m_pAbiCollab;
};

class AbiCollab : public EV_MouseListener
{
	friend class ABI_Collab_Export;

public:
	AbiCollab(PD_Document* pDoc, const UT_UTF8String& sSessionId, XAP_Frame* pFrame);
	AbiCollab(const UT_UTF8String& sSessionId, 
					PD_Document* pDoc, 
					const UT_UTF8String& docUUID,
					UT_sint32 iRev,
					BuddyPtr pControler,
					XAP_Frame* pFrame);
	virtual ~AbiCollab();

	// collaborator management
	void								addCollaborator(BuddyPtr pCollaborator);
	void								removeCollaborator(BuddyPtr pCollaborator);
	void								removeCollaboratorsForAccount(AccountHandler* pHandler);
	const std::vector<BuddyPtr>&		getCollaborators() const
		{ return m_vCollaborators; }
	bool								isController(BuddyPtr pCollaborator) const
		{ return m_pController == pCollaborator; }

	// import/export management
	ABI_Collab_Import*					getImport(void)
		{ return &m_Import; }
	ABI_Collab_Export*					getExport(void)
		{ return &m_Export; }
	void								push(SessionPacket* pPacket);
	bool								push(SessionPacket* pPacket, BuddyPtr collaborator);
	void								maskExport();
	virtual const std::vector<SessionPacket*>&	unmaskExport();
	bool								isExportMasked(void) const
		{ return m_bExportMasked; }
	void								import(SessionPacket* pPacket, BuddyPtr collaborator);
	void								addChangeAdjust(ChangeAdjust* pAdjust);
	const AbstractChangeRecordSessionPacket* getActivePacket() const
		{ return m_pActivePacket; }

	// document management
	PD_Document*						getDocument(void) const
		{ return m_pDoc; }
	XAP_Frame*							getFrame(void) const
		{ return m_pFrame; }

	const UT_UTF8String					getSessionId() const
		{ return m_sId; }
	bool								isLocallyControlled() const
		{ return m_pController == NULL; }
	
	void								setIsReverting(bool bIsReverting)
		{ m_bIsReverting = bIsReverting; }
	
	// session takeover
	void								initiateSessionTakeover(BuddyPtr pNewMaster);
	
	// session recording functionality
	bool								isRecording()
		{ return m_pRecorder != NULL; }
	void								startRecording(SessionRecorderInterface* pRecorder);
	void								stopRecording();

	// mouse listener functionality
	virtual void						signalMouse(EV_EditBits eb, UT_sint32 xPos, UT_sint32 yPos);

protected:
	// TODO: make all Packets shared pointers, so this isn't needed anymore
	class SessionPacketVector : public std::vector<SessionPacket*>
	{
	public:
		~SessionPacketVector() { clear(); }	// so it's autocleaned on destroy!
		void clear()
		{
			for (size_t i=0; i<size(); ++i)
			{
				DELETEP((*this)[i]);
			}
			std::vector<SessionPacket*>::clear();
		};
	} m_vecMaskedPackets; // packets that are generated during the import of a packet

private:
	// collaborator management
	void								_removeCollaborator(UT_sint32 index);

	// document management
	void								_setDocument(PD_Document* pDoc, XAP_Frame* pFrame);
	void								_setDocListenerId(UT_uint32 iDocListenerId)
		{ m_iDocListenerId = iDocListenerId; }
	
	void								_fillRemoteRev(Packet* pPacket, BuddyPtr pBuddy); 

	// mouse listener functionality
	void								_releaseMouseDrag();

	// session takeover
	bool								_handleSessionTakeover(AbstractSessionTakeoverPacket* pPacket, BuddyPtr collaborator);
	bool								_hasAckedSessionTakeover(BuddyPtr collaborator);
	bool								_allSlavesAckedSessionTakeover();
	void								_switchMaster();
	void								_becomeMaster();
	bool								_restartAsSlave(const UT_UTF8String& sDocUUID, UT_sint32 iRev);
	void								_shutdownAsMaster();
	bool								_allSlavesReconnected();
	void								_checkRestartAsMaster();
	void								_restartAsMaster();
	void								_pushOutgoingQueue();

	PD_Document *						m_pDoc;
	XAP_Frame*							m_pFrame;
	ABI_Collab_Import					m_Import;
	ABI_Collab_Export					m_Export;

	std::vector<BuddyPtr>				m_vCollaborators;
	UT_uint32							m_iDocListenerId;
	bool								m_bExportMasked;

	UT_UTF8String						m_sId;

	BuddyPtr							m_pController;

	CommandLine *						m_pCommandLine;
	bool								m_bCloseNow;

	const AbstractChangeRecordSessionPacket* m_pActivePacket;
	bool								m_bIsReverting;
	
	SessionRecorderInterface*			m_pRecorder;

	UT_sint32							m_iMouseLID;
	bool								m_bDoingMouseDrag;
	std::vector<std::pair<SessionPacket*,BuddyPtr> >
										m_vIncomingQueue;


	// session takeover functionality
	SessionTakeoverState				m_eTakeoveState;
	bool								m_bProposedController;
	BuddyPtr							m_pProposedController;
	std::map<std::string, bool>			m_vApprovedReconnectBuddies;
	std::map<BuddyPtr, bool>			m_mAckedSessionTakeoverBuddies; // only used by the session controller
	bool								m_bSessionFlushed;
	SessionPacketVector					m_vOutgoingQueue;
};

#endif /* ABI_COLLAB_H */
