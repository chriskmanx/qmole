/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006,2007 by Marc Maurer <uwog@uwog.net>
 * Copyright (C) 2007 by One Laptop Per Child
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

#ifndef ABI_COLLAB_IMPORT_H
#define ABI_COLLAB_IMPORT_H

#include <map>
#include <vector>
#include <string>
#include <deque>

#include "ut_types.h"
#include "pt_Types.h"
#include "px_ChangeRecord.h"
#include "ut_stack.h"
#include "xav_Listener.h"
#include "pl_Listener.h"
#include "ut_string_class.h"
#include <packet/xp/AbiCollab_Packet.h>
#include <account/xp/Buddy.h>

class FL_DocLayout;
class PD_Document;
class UT_Stack;
class ABI_xmpp;
class ChangeAdjust;

class ABI_Collab_Import
{
	friend class FakeAccountHandler;
	
public:
	ABI_Collab_Import(AbiCollab* pAbiCollab, PD_Document* doc);
	~ABI_Collab_Import();
	
	bool								import(const SessionPacket& sPacket, BuddyPtr collaborator);
	std::map<BuddyPtr, UT_sint32>&		getRemoteRevisions()
		{ return m_remoteRevs; }
	PT_DocPosition						getEndOfDoc();
	void								masterInit();
	void								slaveInit(BuddyPtr pBuddy, UT_sint32 iRev);

private:
	bool								_isOverlapping(UT_sint32 pos1, UT_sint32 length1, UT_sint32 pos2, UT_sint32 length2);
	void								_calculateCollisionSeqence(UT_sint32 iIncomingRemoteRev, 
											const UT_UTF8String& sIncomingDocUUID, UT_sint32& iStart, 
											UT_sint32& iEnd);
	UT_sint32							_getIncomingAdjustmentForState(
											const UT_GenericVector<ChangeAdjust *>* pExpAdjusts, 
											UT_sint32 iStart, UT_sint32 iEnd, UT_sint32 iIncomingPos, 
											UT_sint32 iIncomingLength, const UT_UTF8String& sIncomingUUID,
											std::deque<int>& impAdjs);
	bool								_checkForCollision(const AbstractChangeRecordSessionPacket& acrsp, UT_sint32& iRev, 
											UT_sint32& iImportAdjustment);
	bool								_handleCollision(UT_sint32 iIncommingRev, UT_sint32 iLocalRev, BuddyPtr pCollaborator);
	bool								_shouldIgnore(BuddyPtr pCollaborator);
	void								_disableUpdates(UT_GenericVector<AV_View *>& vecViews, bool bIsGlob);
	void								_enableUpdates(UT_GenericVector<AV_View *>& vecViews, bool bIsGlob);
	bool								_import(const SessionPacket& packet, UT_sint32 iImportAdjustment, BuddyPtr pCollaborator, bool inGlob = false);

	PD_Document*						m_pDoc;
	AbiCollab *							m_pAbiCollab;
	std::map<BuddyPtr, UT_sint32>		m_remoteRevs; // maintained by the importer, used by the exporter
	std::vector<std::pair<BuddyPtr, UT_sint32> > m_revertSet; // only used by the session owner
	std::deque<UT_sint32>				m_iAlreadyRevertedRevs; // only used by non-session owners
};

#endif /* ABI_COLLAB_IMPORT_H */
