/* Copyright (C) 2008 AbiSource Corporation B.V.
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

#ifndef __REALM_BUDDY__
#define __REALM_BUDDY__

#include <stdint.h>
#include <string>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/lexical_cast.hpp>
#include "ut_string_class.h"
#include <core/account/xp/Buddy.h>
#include <core/account/xp/AccountHandler.h>

class RealmConnection;

class RealmBuddy : public Buddy , public boost::enable_shared_from_this<RealmBuddy>
{
public:
	RealmBuddy(AccountHandler* handler, uint64_t user_id, const std::string& domain,
					UT_uint8 realm_connection_id, bool master, boost::shared_ptr<RealmConnection> connection)
		: Buddy(handler),
		m_user_id(user_id),
		m_domain(domain),
		m_realm_connection_id(realm_connection_id),
		m_master(master),
		m_connection(connection)
	{
		setVolatile(true);
	}
	
	virtual UT_UTF8String getDescriptor(bool include_session_info = false) const
	{
		return UT_UTF8String("acn://") + 
					boost::lexical_cast<std::string>(m_user_id).c_str() + 
					(include_session_info ? UT_UTF8String(":") + boost::lexical_cast<std::string>((uint32_t)m_realm_connection_id).c_str() : UT_UTF8String("")) +
					UT_UTF8String("@") + 
					m_domain.c_str();
	}
	
	virtual UT_UTF8String getDescription() const
	{
		return getDescriptor();
	}
	
	virtual const DocTreeItem* getDocTreeItems() const
	{
		return NULL;
	}

	boost::shared_ptr<RealmBuddy> ptr() {
		return shared_from_this();
	}

	boost::shared_ptr<RealmConnection> connection() {
		return m_connection;
	}

	uint64_t user_id() const {
		return m_user_id;
	}

	UT_uint8 realm_connection_id() const {
		return m_realm_connection_id;
	}
	
	bool master() const {
		return m_master;
	}
	
	void demote() {
		m_master = false;
	}

	void promote() {
		m_master = true;
	}
	
private:
	uint64_t			m_user_id;
	std::string			m_domain;
	UT_uint8			m_realm_connection_id;
	bool				m_master;
	boost::shared_ptr<RealmConnection>		m_connection;
};

typedef boost::shared_ptr<RealmBuddy> RealmBuddyPtr;

#endif /* __REALM_BUDDY__ */
