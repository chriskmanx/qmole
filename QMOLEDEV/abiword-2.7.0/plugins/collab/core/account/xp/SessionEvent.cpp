/* Copyright (C) 2006,2007 by Marc Maurer <uwog@uwog.net>
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

#include <string>
#include <vector>

#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xmlreader.h>

#include "ut_vector.h"
#include "pd_Document.h"
#include "px_ChangeRecord.h"
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
#include "DocHandle.h"
#include <xp/AbiCollab_Packet.h>
#include "Event.h"
#include "SessionEvent.h"

void StartSessionEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void GetSessionsEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void GetSessionsResponseEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
	ar << m_Sessions;
}

void JoinSessionEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
	ar << m_sSessionId;
}

std::string	JoinSessionEvent::toStr() const
{
	return 
		Event::toStr() + 
		str(boost::format("JoinSessionEvent: m_sSessionId: %1%\n") % m_sSessionId.utf8_str());
}

void JoinSessionRequestEvent::serialize(Archive & ar)
{
	JoinSessionEvent::serialize( ar );
}

void JoinSessionRequestResponseEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
	ar << m_sSessionId << m_sZABW << m_iRev << m_sDocumentId << m_sDocumentName << m_iAuthorId;
}

std::string	JoinSessionRequestResponseEvent::toStr() const
{
	return
		Event::toStr() +
		str(boost::format("JoinSessionRequestResponseEvent: m_sZABW: %1% bytes, m_iRev: %2%, m_sDocumentId: %3%, m_sDocumentName: %4%, m_iAuthorId: %5%\n") %
			m_sZABW.size() % m_iRev % m_sDocumentId.utf8_str() % m_sDocumentName.utf8_str() % m_iAuthorId );
}

void DisjoinSessionEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
	ar << m_sSessionId;
}

std::string	DisjoinSessionEvent::toStr() const
{
	return 
		Event::toStr() + 
		str(boost::format("DisjoinSessionEvent: m_sSessionId: %1%\n") % m_sSessionId.utf8_str());
}

void CloseSessionEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
	ar << m_sSessionId;
}

std::string	CloseSessionEvent::toStr() const
{
	return 
		Event::toStr() + 
		str(boost::format("CloseSessionEvent: m_sSessionId: %1%\n") % m_sSessionId.utf8_str());
}
