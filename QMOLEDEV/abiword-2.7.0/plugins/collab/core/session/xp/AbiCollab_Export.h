/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006,2007 by Marc Maurer <uwog@uwog.net>
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

#ifndef ABI_COLLAB_EXPORT_H
#define ABI_COLLAB_EXPORT_H

#include "ut_types.h"
#include "pt_Types.h"
#include "px_ChangeRecord.h"
#include "xav_Listener.h"
#include "pl_Listener.h"
#include "ut_string_class.h"
#include <packet/xp/AbiCollab_Packet.h>

class FL_DocLayout;
class PD_Document;
class UT_Stack;
class PX_ChangeRecord;
class ChangeAdjust;
class AbiCollab;

class ABI_Collab_Export : public PL_DocChangeListener
{

friend class AbiCollab;

public:
	ABI_Collab_Export(AbiCollab* pAbiCollab, PD_Document* pDoc);
	virtual ~ABI_Collab_Export();

	virtual bool		populate(PL_StruxFmtHandle /*sfh*/,
								 const PX_ChangeRecord* /*pcr*/) { return true; }

	virtual bool		populateStrux(PL_StruxDocHandle /*sdh*/,
									  const PX_ChangeRecord* /*pcr*/,
									  PL_StruxFmtHandle* /*psfh*/) { return true; }

	virtual bool		change(PL_StruxFmtHandle sfh,
							   const PX_ChangeRecord* pcr);

	virtual void		deferNotifications(void) {}
	virtual void		processDeferredNotifications(void) {}

	virtual bool		insertStrux(PL_StruxFmtHandle sfh,
									const PX_ChangeRecord* pcr,
									PL_StruxDocHandle sdh,
									PL_ListenerId lid,
									void (*pfnBindHandles)(PL_StruxDocHandle sdhNew,
															PL_ListenerId lid,
															PL_StruxFmtHandle sfhNew));

	virtual bool		signal(UT_uint32 iSignal);

	virtual	PLListenerType getType() const;

	virtual void		setNewDocument(PD_Document * pDoc);
	virtual void		removeDocument(void);

	const UT_GenericVector<ChangeAdjust *> * getAdjusts(void) const 
		{ return & m_vecAdjusts;}
	UT_GenericVector<ChangeAdjust *> * getAdjusts(void)
		{ return & m_vecAdjusts;}
		
	void				masterInit();
	void				slaveInit(const UT_UTF8String& docUUID, UT_sint32 iRemoteRev);
	
private:

	ChangeRecordSessionPacket*		_buildPacket( const PX_ChangeRecord* pcr );
	void							_handleNewPacket( ChangeRecordSessionPacket* pPacket, const PX_ChangeRecord* pcr );
	bool							_isGlobEnd(UT_Byte istart, UT_Byte istop);
	void							_mapPropsAtts(
											UT_sint32 indx, 
											std::map<UT_UTF8String,UT_UTF8String>& props, std::map<UT_uint8,UT_UTF8String>& atts );

	void				_init();
	void				_cleanup();

	//! Document which is client of this DocListener
	PD_Document*		m_pDoc;

	UT_Stack			m_sLastContainerLayout;

	AV_ChangeMask		m_chgMaskCached;
	bool				m_bCacheChanges;

	UT_sint32			m_iBlockIndex;
	UT_sint32			m_iSectionIndex;

	AbiCollab *			m_pAbiCollab;
	UT_GenericVector<ChangeAdjust *>        m_vecAdjusts;
	GlobSessionPacket*	m_pGlobPacket;		// if set, we're in a glob
};

#endif /* ABI_COLLAB_EXPORT_H */
