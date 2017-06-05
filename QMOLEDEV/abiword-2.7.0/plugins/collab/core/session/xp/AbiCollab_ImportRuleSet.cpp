/*
 * AbiCollab - Code to enable the modification of remote documents.
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

#include "AbiCollab.h"
#include "AbiCollab_ImportRuleSet.h"
#include <xp/AbiCollab_Packet.h>

bool AbiCollab_ImportRuleSet::isOverlapAllowed(const ChangeAdjust& ca, const AbstractChangeRecordSessionPacket& acrsp, UT_sint32 iRemotePosAdjust)
{
	return _isSaveInsert(ca, acrsp, iRemotePosAdjust) && 
			_isSafeFmtChange(ca, acrsp, iRemotePosAdjust)
			/* TODO: add more overlaps checks */;
}

bool AbiCollab_ImportRuleSet::_isSaveInsert(const ChangeAdjust& ca, const AbstractChangeRecordSessionPacket& acrsp, UT_sint32 iRemotePosAdjust)
{
	UT_return_val_if_fail(ca.m_pPacket, false);

	// if the packets share the same insertion position, then there is nothing we can do
	if (ca.getLocalPos() == acrsp.getPos())
		return false;
	
	// allowing overlapping deletions is _really_ tricky; for now, we just disallow it
	if (ca.getLocalLength() <= 0 || acrsp.getLength() <= 0)
		return false; 

	if (ca.m_pPacket->getClassType() != PCT_GlobSessionPacket && acrsp.getClassType() != PCT_GlobSessionPacket)
	{
		// overlapping inserts are just fine in the case of non-globs, as long as the start positions differ
		return ca.getLocalPos() != (acrsp.getPos()+iRemotePosAdjust);
	}

	//
	// if we get there, then at least one of the packets is a glob; this makes it a bit harder
	//
	
	// first, check that there are no 'delete' changerecords in the glob(s);
	// as stated above, deletes are really tricky, so we just disallow those
	if (ca.m_pPacket->getClassType() == PCT_GlobSessionPacket)
		for (std::vector<SessionPacket*>::const_iterator cit = static_cast<const GlobSessionPacket*>(ca.m_pPacket)->getPackets().begin(); cit != static_cast<const GlobSessionPacket*>(ca.m_pPacket)->getPackets().end(); cit++)
			if (AbstractChangeRecordSessionPacket::isInstanceOf(**cit) && 
				static_cast<AbstractChangeRecordSessionPacket*>(*cit)->getAdjust() < 0)
					return false;

	if (acrsp.getClassType() == PCT_GlobSessionPacket)
		for (std::vector<SessionPacket*>::const_iterator cit = static_cast<const GlobSessionPacket&>(acrsp).getPackets().begin(); cit != static_cast<const GlobSessionPacket&>(acrsp).getPackets().end(); cit++)
			if (AbstractChangeRecordSessionPacket::isInstanceOf(**cit) && 
				static_cast<AbstractChangeRecordSessionPacket*>(*cit)->getAdjust() < 0)
					return false;
	
	//
	// TODO: allow globs/insertions that really don't touch eachother, caused by the fact that
	// the 'first' insertion moves up the other packet's position
	//
	
	
	return false; // just to be on the save side
}

bool AbiCollab_ImportRuleSet::_isSafeFmtChange(const ChangeAdjust& /*ca*/, const AbstractChangeRecordSessionPacket& /*acrsp*/, UT_sint32 /*iRemotePosAdjust*/)
{
	// check for save formatting change overlaps; for example, it is perfectly fine if person A adds
	// a 'bold' property, while person B at the same time adds an 'italic' property
	
	// TODO: implement me
	
	return false;
}
