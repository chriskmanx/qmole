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

#ifndef __SUGARBUDDY_H__
#define __SUGARBUDDY_H__

#include <map>
#include <account/xp/Buddy.h>
#include <account/xp/DocTreeItem.h>
#include <account/xp/AccountHandler.h>

class DocHandle;

class SugarBuddy : public Buddy
{
public:
	SugarBuddy(AccountHandler* handler, const UT_UTF8String dbusAddress)
		: Buddy(handler),
		m_sDBusAddress(dbusAddress)
	{
	}

	virtual UT_UTF8String getDescriptor(bool /*include_session_info = false*/) const
	{
		return UT_UTF8String("sugar://") + m_sDBusAddress;
	}
	
	virtual UT_UTF8String		getDescription() const
		{ return m_sDBusAddress; }
		
	virtual const DocTreeItem* getDocTreeItems() const
	{
		UT_ASSERT_HARMLESS(UT_NOT_REACHED);
		return NULL;
	}

	const UT_UTF8String& getDBusAddress()
	{
		return m_sDBusAddress;
	}

private:

	UT_UTF8String		m_sDBusAddress;
};

typedef boost::shared_ptr<SugarBuddy> SugarBuddyPtr;

#endif /* SUGARBUDDY_H */
