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

#ifndef __SUGARACCOUNTHANDLER__
#define __SUGARACCOUNTHANDLER__

#include <set>

#include <account/xp/AccountHandler.h>
#include <dbus/dbus.h>
#include <dbus/dbus-glib.h>
#include <dbus/dbus-glib-lowlevel.h>
#include "SugarBuddy.h"

extern AccountHandlerConstructor SugarAccountHandlerConstructor;

class Session;
class FV_View;

class SugarAccountHandler : public AccountHandler
{
public:
	static SugarAccountHandler*				getHandler();
	SugarAccountHandler(); // TODO: this constructor shouldn't be public
	virtual ~SugarAccountHandler();

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
		{ return m_bLocallyControlled; }
	
	// user management
	virtual BuddyPtr						constructBuddy(const PropertyMap& props);
	virtual BuddyPtr						constructBuddy(const std::string& descriptor, BuddyPtr pBuddy);
	virtual bool							allowsManualBuddies()
		{ return false; }
	virtual void							forceDisconnectBuddy(BuddyPtr pBuddy);
	virtual bool							recognizeBuddyIdentifier(const std::string& identifier);

	// session management
	virtual bool							allowsSessionTakeover()
		{ return false; }

	// packet management
	virtual bool							send(const Packet* pPacket);
	virtual bool							send(const Packet* pPacket, BuddyPtr buddy);
	Packet*									createPacket(const std::string& packet, BuddyPtr pBuddy);
	
	// event management
	void									handleEvent(Session& pSession);

	// signal management
	virtual void							signal(const Event& event, BuddyPtr pSource);

	// tube & buddy management
	SugarBuddyPtr							getBuddy(const UT_UTF8String& dbusAddress);
	bool									offerTube(FV_View* pView, const UT_UTF8String& tubeDBusAddress);
	bool									joinTube(FV_View* pView, const UT_UTF8String& tubeDBusAddress);
	bool									joinBuddy(FV_View* pView, const UT_UTF8String& buddyDBusAddress);
	bool									disjoinBuddy(FV_View* pView, const UT_UTF8String& buddyDBusAddress);

	bool									isIgnoredBuddy(const UT_UTF8String& buddyName)
		{ return m_ignoredBuddies.find(buddyName) != m_ignoredBuddies.end(); }
	
protected:
	// control management
	void									_registerEditMethods();

private:
	static SugarAccountHandler* 			m_pHandler;
	DBusConnection*							m_pTube;
	bool									m_bLocallyControlled;
	std::set<UT_UTF8String>					m_ignoredBuddies;
};

#endif /* __SUGARACCOUNTHANDLER__ */
