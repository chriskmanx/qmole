/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiCollab- Code to enable the modification of remote documents.
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

#include "pd_Document.h"
#include "px_CR_SpanChange.h"
#include "px_CR_FmtMarkChange.h"  
#include "px_CR_SpanChange.h"
#include "px_CR_FmtMark.h"        
#include "px_CR_Span.h"
#include "px_CR_Glob.h"           
#include "px_CR_StruxChange.h"
#include "px_CR_ObjectChange.h"   
#include "px_CR_Strux.h"
#include "px_CR_Object.h"
#include "xap_App.h"
#include "pd_Style.h"
#include "xap_Frame.h"
#include "fv_View.h"
#include "pt_Types.h"

#include "AbiCollab_Import.h"
#include "AbiCollab_ImportRuleSet.h"
#include "AbiCollab.h"
#include <account/xp/Buddy.h>

ABI_Collab_Import::ABI_Collab_Import(AbiCollab* pAbiCollab, PD_Document* doc):
	m_pDoc(doc),
	m_pAbiCollab(pAbiCollab)
{
}

ABI_Collab_Import::~ABI_Collab_Import()
{
}

bool ABI_Collab_Import::_isOverlapping(UT_sint32 pos1, UT_sint32 length1, UT_sint32 pos2, UT_sint32 length2)
{
	if (pos1 == pos2)
		return true;
	else if (pos1 < pos2)
		return (pos1 + length1 - 1 >= pos2);
	else
		return (pos2 + length2 - 1 >= pos1);
}

void ABI_Collab_Import::_calculateCollisionSeqence(UT_sint32 iIncomingRemoteRev, const UT_UTF8String& sIncomingDocUUID, UT_sint32& iStart, UT_sint32& iEnd)
{
	UT_DEBUGMSG(("ABI_Collab_Import::_calculateCollisionSeqence() - iIncomingRemoteRev: %d\n", iIncomingRemoteRev));

	// initialization
	iStart = -1;
	iEnd = -1;

	ABI_Collab_Export* pExport = m_pAbiCollab->getExport();
	UT_return_if_fail(pExport);

	const UT_GenericVector<ChangeAdjust *>* pExpAdjusts = pExport->getAdjusts();
	UT_return_if_fail(pExpAdjusts);

	// worst case: the whole outgoing changerecord stack is the collision sequence
	iStart = 0;
	iEnd = pExpAdjusts->getItemCount();

	// scan back to find the changerecord in our export list the remote has seen,
	// maybe we can narrow the collision sequence down
	UT_sint32 i = 0;
	for (i = pExpAdjusts->getItemCount()-1; i >= 0; i--)
	{
	    ChangeAdjust * pChange = pExpAdjusts->getNthItem(i);
		if (pChange)
		{
		    UT_DEBUGMSG(("Looking at exported changerecord - rev: %d, pos: %d, length: %d adjust %d, queue pos: %d, orig uuid: %s\n", 
						pChange->getLocalRev(), pChange->getLocalPos(), pChange->getLocalLength(), pChange->getLocalAdjust(), i, pChange->getRemoteDocUUID().utf8_str()));

		    if (iIncomingRemoteRev >= pChange->getLocalRev())
		    {
				UT_DEBUGMSG(("Found the changerecord the remote side has already seen (iIncomingRemoteRev = %d)\n", iIncomingRemoteRev));

				// a new changerecord can't collide with a changerecord the 
				// remote side had already seen, hence the +1
				iStart = i+1;
				break;
		    }
		}
		else
			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
	}

	// now move upward, so we kill off the bottom where changes are from the same document as the incoming cr (you can not collide with your own cr's!)
	for (; iStart < UT_sint32(pExpAdjusts->getItemCount()); iStart++)
	{
		ChangeAdjust * pChange = pExpAdjusts->getNthItem(iStart);
		if (pChange->getRemoteDocUUID() != sIncomingDocUUID)
		{
			// not the same document anymore, we can stop
			break;
		}
		else
        {
			UT_DEBUGMSG(("Killing off matching change: %d\n", iStart));
        }
	}
}

UT_sint32 ABI_Collab_Import::_getIncomingAdjustmentForState(const UT_GenericVector<ChangeAdjust *>* pExpAdjusts, UT_sint32 iStart, UT_sint32 iEnd, UT_sint32 iIncomingPos, UT_sint32 iIncomingLength, const UT_UTF8String& sIncomingUUID, std::deque<int>& incAdjs)
{
	UT_DEBUGMSG(("ABI_Collab_Import::_getIncomingAdjustmentForState()\n"));
	UT_return_val_if_fail(pExpAdjusts, 0);

	UT_sint32 iAdjust = 0;
	for (UT_sint32 j = iEnd-1; j>=iStart; j--)
	{
		ChangeAdjust* pPrev = pExpAdjusts->getNthItem(j);
		if (sIncomingUUID == pPrev->getRemoteDocUUID())
		{
			UT_DEBUGMSG(("Looking at possible adjustment with queue pos: %d, -adjust: %d\n", j, -pPrev->getLocalAdjust()));

			if (static_cast<UT_sint32>(pPrev->getRemoteDocPos()) < iIncomingPos+iAdjust)
			{
				if (pPrev->getLocalAdjust() > 0)
				{
					if (_isOverlapping(pPrev->getRemoteDocPos(), pPrev->getLocalLength(), iIncomingPos+iAdjust, iIncomingLength))
					{
						// NOTE: if the position was in the middle of an insert done previously, 
						// then we only need to take the insertion adjust partially into account
						UT_DEBUGMSG(("ADJUST OVERLAP DETECTED with queue pos: %d, pPrev->getRemoteDocPos(): %d, pPrev->m_iLength: %d, iIncomingPos: %d, iAdjust: %d\n", 
									j, pPrev->getRemoteDocPos(), pPrev->getLocalLength(), iIncomingPos, iAdjust));
						iAdjust -= (iIncomingPos+iAdjust - pPrev->getRemoteDocPos());
						incAdjs.push_front(iIncomingPos+iAdjust - pPrev->getRemoteDocPos());
					}
					else
					{
						UT_DEBUGMSG(("ADJUSTMENT influenced normally by queue pos: %d\n", j));
						iAdjust -= pPrev->getLocalAdjust();
						incAdjs.push_front(pPrev->getLocalAdjust());
					}
				}
				else if (pPrev->getLocalAdjust() < 0)
				{
					// TODO: is the < 0 case correctly handled like this?
					UT_DEBUGMSG(("ADJUSTMENT influence by delete by queue pos: %d, pPrev->m_iProgDocPos: %d, pPrev->getRemoteDocPos()\n", j, pPrev->getRemoteDocPos(), pPrev->getRemoteDocPos()));
					iAdjust -= pPrev->getLocalAdjust();
					incAdjs.push_front(pPrev->getLocalAdjust());		
				}
				else
				{
					UT_DEBUGMSG(("ADJUSTMENT influence of 0 by queue pos: %d, pPrev->m_iProgDocPos: %d, pPrev->getRemoteDocPos()\n", j, pPrev->getRemoteDocPos(), pPrev->getRemoteDocPos()));
					incAdjs.push_front(0);
				}
			}
			else if (static_cast<UT_sint32>(pPrev->getRemoteDocPos()) > iIncomingPos+iAdjust)
			{
				UT_DEBUGMSG(("no ADJUSTMENT influence (insertion point smaller than checkpoint) by queue pos: %d, pPrev->m_iProgDocPos: %d, pPrev->getRemoteDocPos()\n", j, pPrev->getRemoteDocPos(), pPrev->getRemoteDocPos()));
				incAdjs.push_front(0);
			}
			else
			{
				UT_DEBUGMSG(("no ADJUSTMENT influence (insertion point equals checkpoint) by queue pos: %d, pPrev->m_iProgDocPos: %d, pPrev->getRemoteDocPos()\n", j, pPrev->getRemoteDocPos(), pPrev->getRemoteDocPos()));
				incAdjs.push_front(0);
			}
		}
	}
	return iAdjust;
}

/*!
 * Scan back through the CR's we've emitted since this remote CR was sent
 * and see if any overlap this one.
 * return true if there is a collision.
 */
bool ABI_Collab_Import::_checkForCollision(const AbstractChangeRecordSessionPacket& acrsp, UT_sint32& iRev, UT_sint32& iImportAdjustment)
{
	UT_DEBUGMSG(("ABI_Collab_Import::_checkForCollision() - pos: %d, length: %d, UUID: %s, remoterev: %d\n", 
				 acrsp.getPos(), acrsp.getLength(), acrsp.getDocUUID().utf8_str(), acrsp.getRemoteRev()));

	ABI_Collab_Export* pExport = m_pAbiCollab->getExport();
	UT_return_val_if_fail(pExport, false);

	const UT_GenericVector<ChangeAdjust *>* pExpAdjusts = pExport->getAdjusts();
	UT_return_val_if_fail(pExpAdjusts, false);

	iImportAdjustment = 0;

	// get the collision sequence (if any)
	UT_sint32 iStart = 0;
	UT_sint32 iEnd = 0;
	_calculateCollisionSeqence(acrsp.getRemoteRev(), acrsp.getDocUUID(), iStart, iEnd);
	UT_return_val_if_fail(iStart >= 0 && iEnd >= 0, false);
	if (iStart == iEnd)
	{
		UT_DEBUGMSG(("Empty collision sequence, no possible collision\n"));
		return false;
	}

	std::deque<int> incAdjs;
	UT_sint32 iIncomingStateAdjust = _getIncomingAdjustmentForState(pExpAdjusts, iStart, iEnd, acrsp.getPos(), acrsp.getLength(), acrsp.getDocUUID(), incAdjs);
	UT_DEBUGMSG(("IINCOMMINGSTATEADJUST: %d\n", iIncomingStateAdjust));

	// Now scan forward and look for an overlap of the new changerecord with the collision sequence
	UT_DEBUGMSG(("Checking collision sequence [%d..%d) for overlapping changerecords\n", iStart, iEnd));
	bool bDenied = false;
	for (UT_sint32 i = iStart; i < iEnd; i++)
	{
		ChangeAdjust* pChange = pExpAdjusts->getNthItem(i);
		if (pChange)
		{
			UT_DEBUGMSG(("Looking at pChange->getRemoteDocUUID(): %s\n", pChange->getRemoteDocUUID().utf8_str()));

			if (pChange->getRemoteDocUUID() != acrsp.getDocUUID())
			{
				if (_isOverlapping(acrsp.getPos()+iIncomingStateAdjust, acrsp.getLength(), pChange->getLocalPos(), pChange->getLocalLength()) &&
					!AbiCollab_ImportRuleSet::isOverlapAllowed(*pChange, acrsp, iIncomingStateAdjust))
				{
					UT_DEBUGMSG(("Fatal overlap detected for incoming pos: %d, incoming length: %d, pChange->getLocalPos(): %d, pChange->getLocalLength(): %d\n", 
							acrsp.getPos(), acrsp.getLength(), pChange->getLocalPos(), pChange->getLocalLength()));
					iRev = pChange->getLocalRev();
					bDenied = true;
					break;
				}
				else
                {
					UT_DEBUGMSG(("No (fatal) overlap detected for incoming pos: %d, incoming length: %d, pChange->getLocalPos(): %d, pChange->getLocalLength(): %d\n", 
							acrsp.getPos(), acrsp.getLength(), pChange->getLocalPos(), pChange->getLocalLength()));
                }
				
				if (pChange->getLocalPos() < acrsp.getPos()+iIncomingStateAdjust)
				{
					UT_DEBUGMSG(("Normal Upward influence detected\n"));
					iIncomingStateAdjust += pChange->getLocalAdjust();
				}
			}
			else
			{
				UT_DEBUGMSG(("Skipping overlap detection: changerecords came from the same document; incoming pos: %d, incoming length: %d, pChange->getLocalPos(): %d, pChange->getLocalLength(): %d\n", 
							acrsp.getPos(), acrsp.getLength(), pChange->getLocalPos(), pChange->getLocalLength()));
				if (!incAdjs.empty())
				{
					iIncomingStateAdjust += incAdjs.front();
					incAdjs.pop_front();
				}
				else
					UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
			}

			UT_DEBUGMSG(("Now: iIncomingStateAdjust: %d\n", iIncomingStateAdjust));
		}
		else
			UT_return_val_if_fail(false, false);
	}

	if (!bDenied && !incAdjs.empty())
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);

	while (!incAdjs.empty())
	{
		UT_DEBUGMSG(("Adding left-over incoming adjustment: %d\n", incAdjs.front()));
		iIncomingStateAdjust += incAdjs.front();
		incAdjs.pop_front();
	}

	iImportAdjustment = iIncomingStateAdjust;
	UT_DEBUGMSG(("Full import adjustment: %d\n", iImportAdjustment));

	return bDenied;
}

// returns true if the import can continue, false otherwise
bool ABI_Collab_Import::_handleCollision(UT_sint32 iIncomingRev, UT_sint32 iLocalRev, BuddyPtr pCollaborator)
{
	UT_DEBUGMSG(("_handleCollision() - incoming rev %d collides against local rev %d!!!\n", iIncomingRev, iLocalRev));
	UT_return_val_if_fail(pCollaborator, false);

	if (m_pAbiCollab->isLocallyControlled())
	{
		UT_DEBUGMSG(("We're controlling this session, refusing this changerecord from %s!\n", pCollaborator->getDescription().utf8_str()));
		// add this collaborator to our revert ack list, so we can ignore his packets
		// until we get an acknoledgement that he has reverted his local, colliding changes
		m_revertSet.push_back(std::make_pair(pCollaborator, iIncomingRev));
		// send the revert command to the collaborator
		RevertSessionPacket rsp(m_pAbiCollab->getSessionId(), m_pDoc->getOrigDocUUIDString(), iIncomingRev);
		m_pAbiCollab->push(&rsp, pCollaborator);
		return false;
	}
	else
	{
		UT_DEBUGMSG(("We're NOT controlling this session, reverting local changes and accepting changerecord!\n"));

		ABI_Collab_Export* pExport = m_pAbiCollab->getExport();
		UT_return_val_if_fail(pExport, false);

		UT_GenericVector<ChangeAdjust *>* pAdjusts = pExport->getAdjusts();
		UT_return_val_if_fail(pAdjusts, false);
		
		m_pAbiCollab->setIsReverting(true); // mask all changes in the exporter

		// undo our cool local changes, and nuke our exported packet list as well up to (and including) iLocalRev
		for (UT_sint32 i = pAdjusts->getItemCount() - 1; i >= 0; i--)
		{
			ChangeAdjust* pChange = pAdjusts->getNthItem(i);
			if (pChange)
			{
				if (pChange->getLocalRev() >= iLocalRev)
				{
					if (strcmp(m_pDoc->getOrigDocUUIDString(), pChange->getRemoteDocUUID().utf8_str()) == 0)
					{
						UT_DEBUGMSG(("UNDO-ING AND NUKING LOCAL CHANGE: EXPORT POSITION %d, pChange->m_iCRNumber: %d!\n", i, pChange->getLocalRev()));

						// undo the change locally
						m_pDoc->undoCmd(1);

						// fix up the positions on the change stack
						for (UT_sint32 j = i+1; j < pAdjusts->getItemCount(); j++)
						{
							ChangeAdjust* pC = pAdjusts->getNthItem(j);
							if (pC)
							{
								UT_DEBUGMSG(("Looking at fixing up the position of change pos %d\n", j));
								if (pChange->getLocalPos() < pC->getLocalPos())
								{
									UT_DEBUGMSG(("Adjusting change pos %d from m_iDocPos: %d to m_iDocPos: %d\n", j, pC->getLocalPos(), pC->getLocalPos() - pChange->getLocalAdjust()));
									pC->setLocalPos(pC->getLocalPos() - pChange->getLocalAdjust());
								}
								else
                                {
									UT_DEBUGMSG(("No need to adjust change pos %d\n", j));
                                }
							}
							else
                            {
								UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
                            }
						}
						
						// kill off the item
						pAdjusts->deleteNthItem(i);
						delete pChange;
					}
					else
                    {
						UT_DEBUGMSG(("Skipping undo of remote change\n"));
                    }
				}
				else
					break;
			}
			else
            {
				UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
            }
		}

		m_pAbiCollab->setIsReverting(false); // unmask all changes in the exporter

		UT_DEBUGMSG(("Pre-Acknowledging revert of revision %d\n", iLocalRev));
		// send the revert acknowledgement command to the session owner
		RevertAckSessionPacket rasp(m_pAbiCollab->getSessionId(), m_pDoc->getOrigDocUUIDString(), iLocalRev);
		m_pAbiCollab->push(&rasp, pCollaborator);

		m_iAlreadyRevertedRevs.push_back(iLocalRev);	
		
		return true;
	}
}

bool ABI_Collab_Import::_shouldIgnore(BuddyPtr pCollaborator)
{
	UT_return_val_if_fail(pCollaborator, false);

	if (m_pAbiCollab->isLocallyControlled())
	{
		UT_DEBUGMSG(("This session is locally controlled, check if we are waiting for a revert ack from buddy: %s\n", pCollaborator->getDescription().utf8_str()));
		// see if we are waiting for a revert ack packet from this collaborator;
		// if we do, then just drop all packets on the floor until we see it
		for (vector<std::pair<BuddyPtr, UT_sint32> >::iterator it = m_revertSet.begin(); it != m_revertSet.end(); it++)
		{
			if ((*it).first == pCollaborator)
			{
				UT_DEBUGMSG(("Found collaborator %s on our revert ack list for rev %d; changerecords should be ignored!\n", (*it).first->getDescription().utf8_str(), (*it).second));
				return true;
			}
		}
	}
	UT_DEBUGMSG(("%s is not on our revert ack list, don't ignore this packet...\n", pCollaborator->getDescription().utf8_str()));
	return false;
}

void ABI_Collab_Import::_disableUpdates(UT_GenericVector<AV_View *>& vecViews, bool bIsGlob)
{
	m_pDoc->getAllViews(&vecViews);

	for (UT_sint32 i=0; i < vecViews.getItemCount(); i++)
	{
		vecViews.getNthItem(i)->setActivityMask(false);
	}
	m_pDoc->notifyPieceTableChangeStart();
	
	if (bIsGlob)
	{
		// lock out all updates while processing the glob
		m_pDoc->disableListUpdates();
		m_pDoc->setDontImmediatelyLayout(true);
		m_pDoc->beginUserAtomicGlob();
	}
}

void ABI_Collab_Import::_enableUpdates(UT_GenericVector<AV_View *>& vecViews, bool bIsGlob)
{
	if (bIsGlob)
	{
		// allow updates again
		m_pDoc->enableListUpdates();
		m_pDoc->updateDirtyLists();
		m_pDoc->setDontImmediatelyLayout(false);
		m_pDoc->endUserAtomicGlob();
	}
	m_pDoc->notifyPieceTableChangeEnd();
	
	bool bDone = false;
	for (UT_sint32 i = 0; i<vecViews.getItemCount(); i++)
	{
		FV_View * pView = static_cast<FV_View *>( vecViews.getNthItem(i));
		if(pView && !bDone && pView->shouldScreenUpdateOnGeneralUpdate())
		{
			m_pDoc->signalListeners(PD_SIGNAL_UPDATE_LAYOUT);
			bDone = true;
		}
		if(pView)
		{
			pView->fixInsertionPointCoords();
			pView->setActivityMask(true);
		}
	}
}

bool ABI_Collab_Import::import(const SessionPacket& packet, BuddyPtr collaborator)
{
	UT_DEBUGMSG(("ABI_Collab_Import::import()\n"));

	UT_DEBUGMSG(("--------------------------\n"));
	UT_DEBUGMSG(("%s", packet.toStr().c_str()));
	UT_DEBUGMSG(("--------------------------\n"));

	UT_return_val_if_fail(collaborator, false);
	
	// check for collisions to see if we can import this packet at all;
	// NOTE: the position adjustment is calculated in the process, so we don't have to 
	// do that again on import (as calculating it for globs can be quite costly, and
	// we need to do it anyway to properly detect collisions)
	UT_sint32 iImportAdjustment = 0;
	switch (packet.getClassType())
	{
		case PCT_SignalSessionPacket:
			if (_shouldIgnore(collaborator))
				return false;
			// this packet can never cause a collision, let it pass
			break;
		case PCT_RevertSessionPacket:
		case PCT_RevertAckSessionPacket:
			// these packets can never cause collisions, let them pass
			break;
		default:
			if (AbstractChangeRecordSessionPacket::isInstanceOf(packet))
			{
				if (_shouldIgnore(collaborator))
					return false;

				// check for a collision and handle it if it occurred
				const AbstractChangeRecordSessionPacket& acrsp = static_cast<const AbstractChangeRecordSessionPacket&>(packet);
				UT_sint32 iLocalRev = 0;
				bool bCollide = _checkForCollision(acrsp, iLocalRev, iImportAdjustment);
				// make sure we revert all the way upto the first revision in this glob if a collision is detected
				bool continueImport = (bCollide ? _handleCollision(acrsp.getRev(), iLocalRev, collaborator) : true);
				if (!continueImport)
					return false;
			}
			else
			{
				UT_DEBUGMSG(("Unhandled packet class type: %d", packet.getClassType()));
				UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
			}
			break;
	}

	UT_DEBUGMSG(("Collision test succeeded, continuing to import this packet\n"));

	// set the temporary document UUID, so all generated changerecords during
	// import will inherit this UUID


	UT_UTF8String sRealDocname = m_pDoc->getOrigDocUUIDString();
	UT_DEBUGMSG(("Temp. setting document UUID to %s\n", packet.getDocUUID().utf8_str()));
	m_pDoc->setMyUUID(packet.getDocUUID().utf8_str());

	// disable layout/view updates
	UT_GenericVector<AV_View *> vecViews;
	_disableUpdates(vecViews, packet.getClassType() == PCT_GlobSessionPacket);

	// import the changes in the document
	bool bRes = _import(packet, iImportAdjustment, collaborator);	
	// UT_ASSERT_HARMLESS(bRes); // enable this check when stuff like tables don't incorrectly report a failed import due to formatting marks without props/attrs anymore

	// enable layout/view updates
	_enableUpdates(vecViews, packet.getClassType() == PCT_GlobSessionPacket);

	// restore our own document UUID
	m_pDoc->setMyUUID(sRealDocname.utf8_str());
	
	return bRes;
}

/*!
 * Take a packet contained with a UT_UTF8string, interpret it's
 * contents and apply the implied operations on the document.
 */
bool ABI_Collab_Import::_import(const SessionPacket& packet, UT_sint32 iImportAdjustment, BuddyPtr pCollaborator, bool inGlob)
{
	UT_DEBUGMSG(("ABI_Collab_Import::_import() - packet class type: %d, iImportAdjustment: %d\n", packet.getClassType(), iImportAdjustment));
	UT_return_val_if_fail(pCollaborator, false);

	switch (packet.getClassType())
	{
		case PCT_GlobSessionPacket:
			{
				const GlobSessionPacket* gp = static_cast<const GlobSessionPacket*>(&packet);
				UT_return_val_if_fail(gp->getPackets().size() > 0, false);

				// store the last seen revision from this collaborator (it is immediately used by the export)
				m_remoteRevs[pCollaborator] = gp->getRev();

				for (UT_uint32 j = 0; j < gp->getPackets().size(); j++)
				{
					SessionPacket* pGlobPacket = gp->getPackets()[j];
					if (pGlobPacket)
					{
						bool res = _import(*pGlobPacket, iImportAdjustment, pCollaborator, true); // yay for recursion :)
						if (!res)
							UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
					}
				}

				return true;
			}
		
		case PCT_SignalSessionPacket:
			{
				const SignalSessionPacket* sp = static_cast<const SignalSessionPacket*>(&packet);
				m_pDoc->signalListeners(sp->getSignalType());
				return true;
			}

		case PCT_RevertSessionPacket:
			{
				const RevertSessionPacket* rrp = static_cast<const RevertSessionPacket*>(&packet);
				UT_DEBUGMSG(("Revert packet seen on import for rev: %d\n", rrp->getRev()));

				if (m_iAlreadyRevertedRevs.size() == 0 || m_iAlreadyRevertedRevs.front() != rrp->getRev())
				{
					UT_DEBUGMSG(("Incoming revert for revision %d, which we didn't detect locally (m_iAlreadyRevertedRev: %d)!\n", rrp->getRev(), m_iAlreadyRevertedRevs.front()));
					UT_DEBUGMSG(("DOCUMENT OUT OF SYNC DETECTED!!!!\n"));
					UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
					return false;
				}	

				m_iAlreadyRevertedRevs.pop_front();
				return true;
			}
		case PCT_RevertAckSessionPacket:
			{
				UT_DEBUGMSG(("RevertAck packet seen on import for rev: %d\n", static_cast<const RevertAckSessionPacket*>(&packet)->getRev()));

				// remove this collaborator from our revert ack list; he can play again...
				for (vector<std::pair<BuddyPtr, UT_sint32> >::iterator it = m_revertSet.begin(); it != m_revertSet.end(); it++)
				{
					if ((*it).first == pCollaborator)
					{
						UT_DEBUGMSG(("Found collaborator %s on our revert ack list with rev %d! Removing him from the list...\n", (*it).first->getDescription().utf8_str(), (*it).second));
						UT_ASSERT_HARMLESS((*it).second == static_cast<const RevertAckSessionPacket*>(&packet)->getRev());
						m_revertSet.erase(it);
						return true;
					}
				}

				UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
				return false;
			}

		default:
			// silly C++ can't switch on ranges
			if (packet.getClassType() >= _PCT_FirstChangeRecord && packet.getClassType() <= _PCT_LastChangeRecord)
			{
				const ChangeRecordSessionPacket* crp = static_cast<const ChangeRecordSessionPacket*>(&packet);
				UT_DEBUGMSG(("It's safe to import this packet\n"));

				UT_DEBUGMSG(("For CR number %d requested point %d adjustment %d \n",  crp->getRev(), crp->getPos(), iImportAdjustment));

				PT_DocPosition pos = static_cast<PT_DocPosition>(crp->getPos() + iImportAdjustment);
				UT_ASSERT(pos <= getEndOfDoc());

				if (!inGlob)
				{
					// store the last seen revision from this collaborator (it is immediately used by the export)
					// NOTE: if this changerecord is part of a glob, then we don't do this; we'll have
					// already set the revision of the glob itself as the last seen one
					m_remoteRevs[pCollaborator] = crp->getRev();
				}

				// todo: remove these temp vars
				PT_DocPosition iPos2 = 0;

				// process the packet
				switch(crp->getPXType())
				{
					case PX_ChangeRecord::PXT_GlobMarker:
					{
						UT_DEBUGMSG(("Found GLOB marker (ignoring)\n"));
						return true;
					}
					case PX_ChangeRecord::PXT_InsertSpan:
					{
						const InsertSpan_ChangeRecordSessionPacket* icrsp = static_cast<const InsertSpan_ChangeRecordSessionPacket*>( crp );
						UT_UCS4String UCSChars = const_cast<UT_UTF8String&>(icrsp->m_sText).ucs4_str();	// ugly, ucs4_str should be const func!
						PP_AttrProp attrProp;
						attrProp.setAttributes(const_cast<const gchar**>(icrsp->getAtts()));
						attrProp.setProperties(const_cast<const gchar**>(icrsp->getProps()));
						m_pDoc->insertSpan(pos,UCSChars.ucs4_str(),UCSChars.length(), &attrProp);
						break;
					}
					case PX_ChangeRecord::PXT_DeleteSpan:
					{
						iPos2 = pos + crp->getLength();
						PP_AttrProp *p_AttrProp_Before = NULL;
						UT_uint32 icnt = 0;
						m_pDoc->deleteSpan(pos,iPos2,p_AttrProp_Before,icnt,true);
						break;
					}
					case PX_ChangeRecord::PXT_ChangeSpan:
					{
						const Props_ChangeRecordSessionPacket* pcrsp = static_cast<const Props_ChangeRecordSessionPacket*>( crp );
						gchar** szAtts = pcrsp->getAtts();
						gchar** szProps = pcrsp->getProps();
						iPos2 = pos + pcrsp->getLength();
						if((szProps == NULL) && (szAtts == NULL))
						{
							//
							// This happens if we remove all formats
							// we have to handle this seperately
							//
							// Get style of containing block
							//
							PL_StruxDocHandle sdh = NULL;
							m_pDoc->getStruxOfTypeFromPosition(pos,PTX_Block,&sdh);
							if(!sdh)
							{
								UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
								return false;
							}
							PD_Style * pStyle = m_pDoc->getStyleFromSDH(sdh);
							if(!pStyle)
							{
								UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
								return false;
							}
							const gchar * szName =  pStyle->getName();
							const gchar * atts[3] = {PT_STYLE_ATTRIBUTE_NAME,szName,NULL};
							m_pDoc->changeSpanFmt(PTC_SetExactly, pos, iPos2, atts, const_cast<const gchar**>( szProps ) );
						}
						else
						{
							m_pDoc->changeSpanFmt(PTC_SetExactly, pos, iPos2, const_cast<const gchar**>(szAtts), const_cast<const gchar**>( szProps ) );
						}
						break;
					}
					case PX_ChangeRecord::PXT_InsertStrux:
					{
						const ChangeStrux_ChangeRecordSessionPacket* pcrsp = static_cast<const ChangeStrux_ChangeRecordSessionPacket*>( crp );
						PTStruxType pts = pcrsp->m_eStruxType;
						gchar** szAtts = pcrsp->getAtts();
						gchar** szProps = pcrsp->getProps();
						
						if((szProps != NULL) || (szAtts != NULL))
						{
							m_pDoc->insertStrux( pos, pts, const_cast<const gchar**>( szAtts ), const_cast<const gchar**>( szProps ) );
						}
						else
						{
							m_pDoc->insertStrux(pos, pts);
						}
						break;
					}
					case PX_ChangeRecord::PXT_DeleteStrux:
					{
						const DeleteStrux_ChangeRecordSessionPacket* pcrsp = static_cast<const DeleteStrux_ChangeRecordSessionPacket*>( crp );
						PTStruxType pts = pcrsp->m_eStruxType;
						m_pDoc->deleteStrux(pos,pts,true);
						break;
					}
					case PX_ChangeRecord::PXT_ChangeStrux:
					{
						const ChangeStrux_ChangeRecordSessionPacket* pcrsp = static_cast<const ChangeStrux_ChangeRecordSessionPacket*>( crp );
						PTStruxType pts = pcrsp->m_eStruxType;
						gchar** szAtts = pcrsp->getAtts();
						gchar** szProps = pcrsp->getProps();
						UT_return_val_if_fail(szProps != NULL || szAtts != NULL, false);

						UT_DEBUGMSG(("Executing ChangeStrux pos= %d \n",pos));
						m_pDoc->changeStruxFmt(PTC_SetExactly, pos, pos, const_cast<const gchar**>( szAtts ), const_cast<const gchar**>( szProps ), pts);
						
						// TODO: this mask is waaaay to generic
						XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
						if (pFrame)
						{
							FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());
							if (pView)
								pView->notifyListeners(AV_CHG_TYPING | AV_CHG_FMTCHAR | AV_CHG_FMTBLOCK | AV_CHG_PAGECOUNT | AV_CHG_FMTSTYLE );
						}
						break;
					}
					case PX_ChangeRecord::PXT_InsertObject:
					{
						const Object_ChangeRecordSessionPacket* ocrsp = static_cast<const Object_ChangeRecordSessionPacket*>( crp );
						PTObjectType pto = ocrsp->m_eObjectType;
						gchar** szAtts = ocrsp->getAtts();
						gchar** szProps = ocrsp->getProps();
						
						if((szProps == NULL) && (szAtts == NULL))
						{
							UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
							return false;
						}
						m_pDoc->insertObject(pos, pto, const_cast<const gchar**>( szAtts ), const_cast<const gchar**>( szProps ) );
						break;
					}
					case PX_ChangeRecord::PXT_DeleteObject:
					{
						iPos2 = pos + 1;
						PP_AttrProp *p_AttrProp_Before = NULL;
						UT_uint32 icnt = 0;
						m_pDoc->deleteSpan(pos, iPos2, p_AttrProp_Before, icnt, true);
						break;
					}
					case PX_ChangeRecord::PXT_ChangeObject:
					{
						const Object_ChangeRecordSessionPacket* ccrsp = static_cast<const Object_ChangeRecordSessionPacket*>( crp );
						//PTObjectType pto = ccrsp->m_eObjectType;
						gchar** szAtts = ccrsp->getAtts();
						gchar** szProps = ccrsp->getProps();
						
						if ((szProps == NULL) && (szAtts == NULL))
						{
							UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
							return false;
						}
						m_pDoc->changeSpanFmt(PTC_SetExactly, pos, pos + 1, const_cast<const gchar**>( szAtts ), const_cast<const gchar**>( szProps ));
						break;
					}
					case PX_ChangeRecord::PXT_InsertFmtMark:
					{
						const Props_ChangeRecordSessionPacket* pcrsp = static_cast<const Props_ChangeRecordSessionPacket*>( crp );
						gchar** szAtts = pcrsp->getAtts();
						gchar** szProps = pcrsp->getProps();
						
						if((szProps == NULL) && (szAtts == NULL))
						{
							// nothing to do here, please move along
							
							// NOTE: why does this happen anyway? 
							// This happens when for example when sending over tables:
							UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
							return false;
						}
						return m_pDoc->changeSpanFmt(PTC_SetExactly, pos, pos, const_cast<const gchar**>( szAtts ), const_cast<const gchar**>( szProps ));
					}
					case PX_ChangeRecord::PXT_DeleteFmtMark:
					{
						return m_pDoc->deleteFmtMark(pos);
					}
					case PX_ChangeRecord::PXT_ChangeFmtMark:
					{
						const Props_ChangeRecordSessionPacket* pcrsp = static_cast<const Props_ChangeRecordSessionPacket*>( crp );
						gchar** szAtts = pcrsp->getAtts();
						gchar** szProps = pcrsp->getProps();
						
						if ((szProps == NULL) && (szAtts == NULL))
						{
							UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
							return false;
						}
						return m_pDoc->changeSpanFmt(PTC_SetExactly, pos, pos, const_cast<const gchar**>( szAtts ), const_cast<const gchar**>( szProps ));
					}
					case PX_ChangeRecord::PXT_ChangePoint:
					{
						UT_DEBUGMSG(("Change Point CR \n"));
						return m_pDoc->createAndSendCR(pos, crp->getPXType(), true, 0);
					}
					case PX_ChangeRecord::PXT_ListUpdate:
					{
						UT_DEBUGMSG(("ListUpdate CR \n"));
						return m_pDoc->createAndSendCR(pos, crp->getPXType(), true,0);
					}
					case PX_ChangeRecord::PXT_StopList:
					{
						UT_DEBUGMSG(("StopList CR \n"));
						return m_pDoc->createAndSendCR(pos, crp->getPXType(), true,0);
					}
					case PX_ChangeRecord::PXT_UpdateField:
					{
						UT_DEBUGMSG(("UpdateFiled CR \n"));
						return m_pDoc->createAndSendCR(pos, crp->getPXType(), true,0);
					}
					case PX_ChangeRecord::PXT_RemoveList:
					{
						UT_DEBUGMSG(("RemoveList CR \n"));
						return m_pDoc->createAndSendCR(pos, crp->getPXType(), true,0);
					}
					case PX_ChangeRecord::PXT_UpdateLayout:
					{
						UT_DEBUGMSG(("UpdateLayout CR \n"));
						return m_pDoc->createAndSendCR(pos, crp->getPXType(), true,0);
					}
					case PX_ChangeRecord::PXT_CreateDataItem:
					{
						const Data_ChangeRecordSessionPacket* dp = static_cast<const Data_ChangeRecordSessionPacket*>( crp );
						const char * szNameV = g_strdup(dp->getAttribute(PT_DATAITEM_ATTRIBUTE_NAME));
						void * pHandle = NULL;
						char * pToken = dp->m_bTokenSet ? g_strdup(dp->m_sToken.c_str()) : NULL;
						UT_ByteBuf * pBuf= new UT_ByteBuf();
						UT_DEBUGMSG(("PXT_CreateDataItem: append image buffer @ 0x%x, %u bytes, pToken %s\n", &dp->m_vecData[0], dp->m_vecData.size(), pToken?pToken:"NULL"));
						pBuf->append(reinterpret_cast<const UT_Byte *>( &dp->m_vecData[0] ), dp->m_vecData.size() );
						bool res = m_pDoc->createDataItem(szNameV,false,pBuf,pToken,&pHandle);
						delete pBuf;
						return res;
					}
					case PX_ChangeRecord::PXT_ChangeDocProp:
					{
						UT_DEBUGMSG(("ChangeDocProp CR \n"));
						const Props_ChangeRecordSessionPacket* pcrsp = static_cast<const Props_ChangeRecordSessionPacket*>( crp );
						//
						// Assemble the Attributes for different properties
						//
						const gchar** szAtts = const_cast<const gchar **>(pcrsp->getAtts());
						const gchar** szProps = const_cast<const gchar **>(pcrsp->getProps());
						//
						// Now direct the document to make the changes
						//
						return m_pDoc->changeDocPropeties(szAtts,szProps);
					}
					default:
					{
						UT_DEBUGMSG(("Unimplemented crp->getPXType(): %d\n", crp->getPXType()));
						UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
						break;
					}
				}
				
				return true;
			}
			else
			{
				UT_DEBUGMSG(("ABI_Collab_Import::import called with unhandled packet class type: %d!\n", packet.getClassType()));
				return false;
			}
			break;		
	}

	return false;
}

PT_DocPosition ABI_Collab_Import::getEndOfDoc(void)
{
        PT_DocPosition posEnd;
        m_pDoc->getBounds(true,posEnd);
        return posEnd;
}

void ABI_Collab_Import::masterInit()
{
	// NOTE: it's important that this function resets all state, as it can be
	// called in the middle of an already running collaboration session
	// (eg. when a session takeover happens)

	m_remoteRevs.clear();
	m_revertSet.clear();
	m_iAlreadyRevertedRevs.clear(); // only used by non-session owners, but it can't hurt to clear it
}

void ABI_Collab_Import::slaveInit(BuddyPtr pBuddy, UT_sint32 iRev)
{
	UT_return_if_fail(pBuddy);

	// NOTE: it's important that this function resets all state, as it can be
	// called in the middle of an already running collaboration session
	// (eg. when a session takeover happens)

	m_remoteRevs.clear();
	m_remoteRevs[pBuddy] = iRev;
	m_revertSet.clear(); // only used by the session owner, but it can't hurt to clear it
	m_iAlreadyRevertedRevs.clear();
}

