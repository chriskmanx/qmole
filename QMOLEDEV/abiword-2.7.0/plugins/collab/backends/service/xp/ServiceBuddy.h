/* Copyright (C) 2006,2007 Marc Maurer <uwog@uwog.net>
 * Copyright (C) 2008 AbiSource Corporation B.V.
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

#ifndef __SERVICEBUDDY__
#define __SERVICEBUDDY__

#include <string>
#include <stdint.h>
#include <boost/lexical_cast.hpp>
#include <boost/shared_ptr.hpp>
#include "ut_string_class.h"
#include <core/account/xp/Buddy.h>
#include <core/account/xp/AccountHandler.h>

class ServiceBuddy : public Buddy
{
public:
	ServiceBuddy(AccountHandler* handler, const std::string& email, const std::string& domain)
		: Buddy(handler),
		m_email(email),
		m_domain(domain)
	{
		setVolatile(true);
	}
	
	virtual UT_UTF8String getDescriptor(bool include_session_info = false) const
	{
		if (include_session_info)
			UT_ASSERT_HARMLESS(UT_NOT_REACHED);
		return UT_UTF8String("acn://") + m_email.c_str() + UT_UTF8String("@") + m_domain.c_str();
	}
	
	virtual UT_UTF8String getDescription() const
		{ return m_email.c_str(); }
	
	const std::string& getEmail() const
		{ return m_email; }
	
	virtual const DocTreeItem* getDocTreeItems() const
	{
		const vector<DocHandle*>& docHandles = getDocHandles();
		DocTreeItem* first = 0;
		DocTreeItem* prev = 0;		
		for (vector<DocHandle*>::const_iterator pos = docHandles.begin(); pos != docHandles.end(); pos++)
		{
			DocTreeItem* item = new DocTreeItem();
			item->m_type = DOCTREEITEM_TYPE_DOCUMENT;
			item->m_docHandle = *pos;
			item->m_child = 0;
			item->m_next = 0;
			
			if (!first)
				first = item;
			if (prev)
				prev->m_next = item;
			prev = item;
		}
		return first;
	}
	
private:
	std::string		m_email;
	std::string		m_domain;
};

typedef boost::shared_ptr<ServiceBuddy> ServiceBuddyPtr;

#endif /* __SERVICEBUDDY__ */
