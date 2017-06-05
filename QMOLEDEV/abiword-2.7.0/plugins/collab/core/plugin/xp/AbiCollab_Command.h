/*
 * AbiCollab- Code to enable the modification of remote documents.
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

#ifndef ABI_COLLAB_COMMAND_H
#define ABI_COLLAB_COMMAND_H

class UT_UTF8String;
#ifdef ABICOLLAB_HANDLER_FAKE	
class FakeAccountHandler;
#endif

class AbiCollab_Command
{
public:
	AbiCollab_Command(const UT_UTF8String& argv);
	~AbiCollab_Command();

	bool					execute();

private:
	bool					_doCmdRegression(const UT_UTF8String& sSessionFile);	
	bool					_doCmdDebug(const UT_UTF8String& sServerSessionFile, const UT_UTF8String& sClientSessionFile, bool bSingleStep);	
	
#ifdef ABICOLLAB_HANDLER_FAKE	
	bool					_syncDocs(FakeAccountHandler* pServerHandler, FakeAccountHandler* pClientHandler, bool bSingleStep);
#endif
	
	UT_UTF8String			m_argv;	
};

#endif /* ABI_COLLAB_COMMAND_H */
