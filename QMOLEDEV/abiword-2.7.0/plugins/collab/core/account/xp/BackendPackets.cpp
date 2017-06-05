/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
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
#include "AccountEvent.h"
#include "SessionEvent.h"

REGISTER_PACKET(AccountNewEvent)
REGISTER_PACKET(AccountOnlineEvent)
REGISTER_PACKET(AccountOfflineEvent)
REGISTER_PACKET(AccountAddBuddyEvent)
REGISTER_PACKET(AccountDeleteBuddyEvent)
REGISTER_PACKET(AccountBuddyOnlineEvent)
REGISTER_PACKET(AccountBuddyOfflineEvent)
REGISTER_PACKET(AccountAddBuddyRequestEvent)
REGISTER_PACKET(AccountBuddyAddDocumentEvent)
REGISTER_PACKET(StartSessionEvent)
REGISTER_PACKET(GetSessionsEvent)
REGISTER_PACKET(GetSessionsResponseEvent)
REGISTER_PACKET(JoinSessionEvent)
REGISTER_PACKET(JoinSessionRequestEvent)
REGISTER_PACKET(JoinSessionRequestResponseEvent)
REGISTER_PACKET(DisjoinSessionEvent)
REGISTER_PACKET(CloseSessionEvent)
