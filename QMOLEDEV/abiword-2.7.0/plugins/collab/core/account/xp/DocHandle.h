/* Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
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

#ifndef __DOCHANDLE_H__
#define __DOCHANDLE_H__

#include "ut_string_class.h"
class AbiCollab;

 class DocHandle
{
public:
	DocHandle(const UT_UTF8String& sSessionId, const UT_UTF8String& name) 
		: m_sSessionId(sSessionId),
	  m_name(name),
	  m_pAbiCollab(NULL)
	{
	}
	virtual ~DocHandle() {}
	
	const UT_UTF8String&		getSessionId() const
		{ return m_sSessionId; }
	const UT_UTF8String&		getName() const
		{ return m_name; }
	void setSession(AbiCollab * pAbiCollab)
		{ m_pAbiCollab = pAbiCollab; }

	AbiCollab *					getSession(void)
		{ return m_pAbiCollab;}
	  
private:
	const UT_UTF8String			m_sSessionId;
	const UT_UTF8String			m_name;
	AbiCollab *					m_pAbiCollab;
};

#endif /* DOCHANDLE_H */
