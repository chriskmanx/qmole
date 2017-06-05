/*
 * AbiCollab - Code to enable the modification of remote documents.
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

#ifndef ABI_COLLAB_IMPORTRULESET_H
#define ABI_COLLAB_IMPORTRULESET_H

#include "ut_types.h"

class ChangeAdjust;
class AbstractChangeRecordSessionPacket;

class AbiCollab_ImportRuleSet
{
public:
	static bool		isOverlapAllowed(const ChangeAdjust& ca, const AbstractChangeRecordSessionPacket& acrsp, UT_sint32 iRemotePosAdjust);

private:
	static bool		_isSaveInsert(const ChangeAdjust& ca, const AbstractChangeRecordSessionPacket& acrsp, UT_sint32 iRemotePosAdjust);
	static bool		_isSafeFmtChange(const ChangeAdjust& ca, const AbstractChangeRecordSessionPacket& acrsp, UT_sint32 iRemotePosAdjust);
};

#endif /* ABI_COLLAB_IMPORTRULESET_H */
