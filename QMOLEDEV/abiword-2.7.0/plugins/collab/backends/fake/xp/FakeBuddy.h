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

#ifndef __FAKEBUDDY_H__
#define __FAKEBUDDY_H__

#include <map>
#include <account/xp/Buddy.h>
#include <account/xp/DocTreeItem.h>
#include <account/xp/AccountHandler.h>

class DocHandle;

class FakeBuddy : public Buddy
{
public:
	FakeBuddy(AccountHandler* handler, const UT_UTF8String& descriptor)
		: Buddy(handler),
		m_sDescriptor(descriptor)
	{
	}

	virtual UT_UTF8String getDescriptor(bool /*include_session_info = false*/) const
	{
		return UT_UTF8String("fake://") + m_sDescriptor;
	}
	
	virtual UT_UTF8String		getDescription() const
		{ return m_sDescriptor; }
		
	virtual const DocTreeItem* getDocTreeItems() const
	{
		UT_ASSERT_HARMLESS(UT_NOT_REACHED);
		return NULL;
	}

private:
	UT_UTF8String m_sDescriptor;
};

typedef boost::shared_ptr<FakeBuddy> FakeBuddyPtr;

#endif /* __FAKEBUDDY_H__ */
