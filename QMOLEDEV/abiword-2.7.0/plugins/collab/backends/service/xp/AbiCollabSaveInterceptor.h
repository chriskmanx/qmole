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

#ifndef __ABICOLLAB_SAVE_INTERCEPTOR__
#define __ABICOLLAB_SAVE_INTERCEPTOR__

#include "ev_EditMethod.h"
#define SAVE_INTERCEPTOR_EM "com.abisource.abiword.abicollab.servicesaveinterceptor"

class AV_View;
class EV_EditMethodCallData;
class EV_EditMethod;
class PD_Document;
class AbiCollabSaveInterceptor
{
public:
	AbiCollabSaveInterceptor();
	
	bool intercept(AV_View * v, EV_EditMethodCallData * d);
	bool saveRemotely(PD_Document * pDoc);
	
private:
	void _save_cb(UT_Error error, AbiCollab* pSession);

	EV_EditMethod* m_pOldSaveEM;
};

#endif /* __ABICOLLAB_SAVE_INTERCEPTOR__ */
