/* Copyright (C) 2006,2007 by Marc Maurer <uwog@uwog.net>
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

#include "DocHandle.h"
#include <xp/AbiCollab_Packet.h>
#include "Event.h"
#include "AccountEvent.h"

void AccountNewEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void AccountOnlineEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void AccountOfflineEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void AccountAddBuddyEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void AccountDeleteBuddyEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void AccountBuddyOnlineEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void AccountBuddyOfflineEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void AccountAddBuddyRequestEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
}

void AccountBuddyAddDocumentEvent::serialize(Archive & ar)
{
	Event::serialize( ar );
	char haveHandle;
	if (ar.isLoading()) {
		ar << haveHandle;
		if (haveHandle) {
			// create or find document with session and name?
			// XXX
			UT_UTF8String session, name;
			ar << session << name;
			m_pDocHandle = NULL;
		} else {
			m_pDocHandle = NULL;
		}
	} else {
		haveHandle = m_pDocHandle ? 1 : 0;
		ar << haveHandle;
		if (haveHandle) {
			ar << const_cast<UT_UTF8String&>( m_pDocHandle->getSessionId() );
			ar << const_cast<UT_UTF8String&>( m_pDocHandle->getName() );
		}
	}
}
