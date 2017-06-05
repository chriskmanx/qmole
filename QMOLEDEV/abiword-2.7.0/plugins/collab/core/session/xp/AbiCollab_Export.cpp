/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiCollab- Code to enable the modification of remote documents.
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
 * Copyright (C) 2007 One Laptop Per Child
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

#include "pd_Document.h"
#include "px_CR_SpanChange.h"
#include "px_CR_FmtMarkChange.h"  
#include "px_CR_SpanChange.h"
#include "px_CR_FmtMark.h"        
#include "px_CR_Span.h"
#include "px_CR_Glob.h"           
#include "px_CR_StruxChange.h"
#include "px_CR_ObjectChange.h"   
#include "px_CR_Strux.h"
#include "px_CR_Object.h"
#include "ut_bytebuf.h"

#include "AbiCollab_Export.h"
#include "AbiCollab_Import.h"
#include "AbiCollab.h"
#include "AbiCollabSessionManager.h"

ABI_Collab_Export::ABI_Collab_Export(AbiCollab * pAbiCollab, PD_Document* pDoc) 
:	m_pDoc(pDoc),
	m_pAbiCollab(pAbiCollab)
{
	UT_DEBUGMSG(("Constructing AbiCollab Exporter \n"));
	_init();
}

ABI_Collab_Export::~ABI_Collab_Export()
{
	UT_DEBUGMSG(("AbiCollab Export deleted %x \n",this));
	_cleanup();
}

/*!
 * Helper function for buildPacket, used on 'real' change record packets only, so
 * PACKET_CHANGERECORD is always correct.
 */
template<class _PacketType>
struct PacketFactory 
{
	static _PacketType* create( const PX_ChangeRecord * pcr, AbiCollab * pAbiCollab, PD_Document* pDoc ) 
	{
		UT_DEBUGMSG(("Creating packet with doc UUID: %s\n", pDoc->getOrigDocUUIDString()));
		return new _PacketType(
				pAbiCollab->getSessionId(), 		/* session identifier */
				pcr->getType(), 					/* type of change */
				pDoc->getOrigDocUUIDString(),		/* document identifier */
				pcr->getPosition(), 				/* position */
				pcr->getCRNumber(),					/* local revision */
				-1									/* remote revision unknown at this point */
				);
	};
};

PLListenerType ABI_Collab_Export::getType() const
{
	return PTL_CollabExport;
}

/*!
 * This method converts a change record into a AbiCollab session packet
 */
ChangeRecordSessionPacket* ABI_Collab_Export::_buildPacket( const PX_ChangeRecord * pcr )
{
	UT_return_val_if_fail(pcr, false);
	
	UT_sint32 index = static_cast<UT_sint32>(pcr->getIndexAP());
	switch(pcr->getType()) {
		case PX_ChangeRecord::PXT_GlobMarker:
		{
			const PX_ChangeRecord_Glob* pcrg = reinterpret_cast<const PX_ChangeRecord_Glob*>(pcr);
			
			// build change record packet
			Glob_ChangeRecordSessionPacket* packet = PacketFactory<Glob_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			packet->setLength(0);
			packet->setAdjust(0);
			// set special properties
			packet->m_iGLOBType = pcrg->getFlags();
			return packet;
		}
	
		case PX_ChangeRecord::PXT_InsertSpan:
		{
			// build change record packet
			InsertSpan_ChangeRecordSessionPacket* packet = PacketFactory<InsertSpan_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			const PX_ChangeRecord_SpanChange * pcrc = static_cast<const PX_ChangeRecord_SpanChange *>(pcr);
			packet->setLength( pcrc->getLength() );
			packet->setAdjust( pcrc->getLength() );
			// set properties
			_mapPropsAtts( index, packet->getPropMap(), packet->getAttMap() );			
			// set special properties
			PT_BufIndex bi = pcrc->getBufIndex();
			const UT_UCS4Char* pChars = m_pDoc->getPointer(bi);
			packet->m_sText.appendUCS4(pChars, pcrc->getLength() );
			return packet;
		}
		
		case PX_ChangeRecord::PXT_DeleteSpan:
		{
			// build change record packet
			ChangeRecordSessionPacket* packet = PacketFactory<ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			const PX_ChangeRecord_SpanChange * pcrc = static_cast<const PX_ChangeRecord_SpanChange *>(pcr);
			packet->setLength( pcrc->getLength() );
			packet->setAdjust( -pcrc->getLength() );
			return packet;
		}
		
		case PX_ChangeRecord::PXT_ChangeSpan:
		{
			// build change record
			Props_ChangeRecordSessionPacket* packet = PacketFactory<Props_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length
			const PX_ChangeRecord_SpanChange * pcrc = static_cast<const PX_ChangeRecord_SpanChange *> (pcr);
			packet->setLength( pcrc->getLength() );
			packet->setAdjust( 0 );
			// set properties
			_mapPropsAtts( index, packet->getPropMap(), packet->getAttMap() );
			return packet;
		}
		
		case PX_ChangeRecord::PXT_InsertStrux:
		{
			// build change record packet
			ChangeStrux_ChangeRecordSessionPacket* packet = PacketFactory<ChangeStrux_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			const PX_ChangeRecord_Strux * pcrx = static_cast<const PX_ChangeRecord_Strux *> (pcr);
			packet->m_eStruxType = pcrx->getStruxType();
			packet->setLength( 1 );
			packet->setAdjust( 1 );
			// set properties
			// we do not always want to map our attrs/props (why not?? - MARCM)
			bool mapAttributes = false;
			switch (packet->m_eStruxType) 
			{
				case PTX_Block:
					if (index != m_iBlockIndex) 
					{
						m_iBlockIndex = index;
						mapAttributes = true;
					}
					break;
				case PTX_Section:
				case PTX_SectionHdrFtr:				// TODO; is this correct??? i have _no_ clue - MARCM
					if (index != m_iSectionIndex) 
					{
						m_iSectionIndex = index;
						mapAttributes = true;
					}
					break;
					
				case PTX_SectionFootnote:
				case PTX_SectionAnnotation:
				case PTX_SectionTOC:
				case PTX_SectionEndnote:
				case PTX_SectionTable:
				case PTX_SectionCell:
				case PTX_SectionFrame:
					mapAttributes = true;
					break;
				default:
					break;
			}
			if (mapAttributes) 
				_mapPropsAtts( index, packet->getPropMap(), packet->getAttMap() );
			return packet;
		}

		case PX_ChangeRecord::PXT_ChangeStrux:
		{
			// build change record packet
			ChangeStrux_ChangeRecordSessionPacket* packet = PacketFactory<ChangeStrux_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			const PX_ChangeRecord_StruxChange * pcrx = static_cast<const PX_ChangeRecord_StruxChange *> (pcr);
			packet->m_eStruxType = pcrx->getStruxType();
			packet->setLength( 1 );
			packet->setAdjust( 0 );
			// set properties
			_mapPropsAtts( index, packet->getPropMap(), packet->getAttMap() );
			return packet;
		}
		
		case PX_ChangeRecord::PXT_DeleteStrux:
		{
			// build change record packet
			DeleteStrux_ChangeRecordSessionPacket* packet = PacketFactory<DeleteStrux_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			packet->setLength( 1 );
			packet->setAdjust( -1 );
			// set properties
			const PX_ChangeRecord_Strux * pcrx = static_cast<const PX_ChangeRecord_Strux *> (pcr);
			packet->m_eStruxType = pcrx->getStruxType();
			return packet;
		}
		
		case PX_ChangeRecord::PXT_InsertObject:
		{
			// build change record packet
			Object_ChangeRecordSessionPacket* packet = PacketFactory<Object_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			packet->setLength( 1 ); // TODO: is this correct? - MARCM
			packet->setAdjust( 1 ); // TODO: is this correct? - MARCM
			// set properties
			const PX_ChangeRecord_Object * pcro = static_cast<const PX_ChangeRecord_Object *>(pcr);
			packet->m_eObjectType = pcro->getObjectType();
			_mapPropsAtts( index, packet->getPropMap(), packet->getAttMap() );
			return packet;
		}
		
		case PX_ChangeRecord::PXT_ChangeObject:
		{
			// build change record packet
			Object_ChangeRecordSessionPacket* packet = PacketFactory<Object_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			packet->setLength( 1 ); // TODO: is this correct? - MARCM
			packet->setAdjust( 0 ); // TODO: is this correct? - MARCM
			// set properties
			const PX_ChangeRecord_ObjectChange * pcro = static_cast<const PX_ChangeRecord_ObjectChange *>(pcr);
			packet->m_eObjectType = pcro->getObjectType();
			_mapPropsAtts( index, packet->getPropMap(), packet->getAttMap() );
			return packet;
		}
			
		case PX_ChangeRecord::PXT_DeleteObject:
		{
			// build change record packet
			Object_ChangeRecordSessionPacket* packet = PacketFactory<Object_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			packet->setLength( 1 );
			packet->setAdjust( -1 );
			// set properties
			const PX_ChangeRecord_ObjectChange * pcro = static_cast<const PX_ChangeRecord_ObjectChange *>(pcr);
			packet->m_eObjectType = pcro->getObjectType();
			return packet;
		}
		
		case PX_ChangeRecord::PXT_InsertFmtMark:
		case PX_ChangeRecord::PXT_ChangeFmtMark:
		{
			// build change record
			Props_ChangeRecordSessionPacket* packet = PacketFactory<Props_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set properties
			_mapPropsAtts( index, packet->getPropMap(), packet->getAttMap() );
			// set length and adjust
			packet->setLength( 0 );
			packet->setAdjust( 0 );			
			return packet;
		}
		
		case PX_ChangeRecord::PXT_DeleteFmtMark:
		case PX_ChangeRecord::PXT_ChangePoint:
		case PX_ChangeRecord::PXT_ListUpdate:
		case PX_ChangeRecord::PXT_StopList:
		case PX_ChangeRecord::PXT_UpdateField:
		case PX_ChangeRecord::PXT_RemoveList: // should this be here? - MARCM
		case PX_ChangeRecord::PXT_UpdateLayout:
		{
			// build change record
			ChangeRecordSessionPacket* packet = PacketFactory<ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set length and adjust
			packet->setLength( 0 );
			packet->setAdjust( 0 );
			return packet;
		}
		
		case PX_ChangeRecord::PXT_CreateDataItem:
		{
			// build change record

			Data_ChangeRecordSessionPacket* packet = PacketFactory<Data_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set properties
			_mapPropsAtts( index, packet->getPropMap(), packet->getAttMap() );
			// fetch data item data
			const PP_AttrProp * pAP = NULL;
			PT_AttrPropIndex indexAP = static_cast<PT_AttrPropIndex>(index);
			bool b = m_pDoc->getAttrProp(indexAP, &pAP);
			UT_return_val_if_fail(b,NULL);
			const gchar * pszDataName = NULL;
			(pAP)->getAttribute(PT_DATAITEM_ATTRIBUTE_NAME,pszDataName);
			if(pszDataName == NULL)
			{
				UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
				return NULL;
			}
			if(g_str_has_prefix(pszDataName,"snapshot-png-") == TRUE)
			{
				//
				// Drop the snapshot, let the remote document create it's own
				//
				delete packet;
				return NULL;
			}
			const UT_ByteBuf* pBuf=NULL;
			const void* pToken = NULL;
			void* pHandle = NULL;
			m_pDoc->getDataItemDataByName(pszDataName,&pBuf,&pToken,&pHandle);
			// put into our vector
			size_t length = pBuf->getLength();
			packet->m_vecData.resize( length );
			memcpy( &packet->m_vecData[0], pBuf->getPointer(0), length );
			if (pToken) 
			{
				packet->m_bTokenSet = true;
				packet->m_sToken = static_cast<const char*>( pToken );
			}
			else
			{
				packet->m_bTokenSet = false;
			}
			// set length and adjust
			packet->setLength( 0 );
			packet->setAdjust( 0 );
			return packet;
		}
		case PX_ChangeRecord::PXT_ChangeDocProp:
		{
			// build change record
			Props_ChangeRecordSessionPacket* packet = PacketFactory<Props_ChangeRecordSessionPacket>::create( pcr, m_pAbiCollab, m_pDoc );
			// set properties
			_mapPropsAtts( index, packet->getPropMap(), packet->getAttMap() );
			// set length and adjust
			// Document Properties have not size
			packet->setLength( 0 );
			packet->setAdjust( 0 );			
			return packet;

		}		
		default:
			UT_DEBUGMSG(("Unimplemented pcr->getType(): %d\n", pcr->getType()));
			UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
			return NULL;
	}
}

/*!
 * This method extracts all the attributes (including the property string
 * from the index provides and maps them to a utf-8 string.
 * The result can be placed inside an XML name space. 
 */
void ABI_Collab_Export::_mapPropsAtts( UT_sint32 indx, std::map<UT_UTF8String,UT_UTF8String>& props, std::map<UT_uint8,UT_UTF8String>& atts )
{
	// fetch data
	const PP_AttrProp * pAP = NULL;
	PT_AttrPropIndex indexAP = static_cast<PT_AttrPropIndex>(indx);
	bool b = m_pDoc->getAttrProp(indexAP, &pAP);
	UT_return_if_fail(b);
	
	// necessary vars
	UT_sint32 i = 0;
	const gchar * szName = NULL;
	const gchar * szVal = NULL;
	
	// map attributes
	atts.clear();
	UT_sint32 nAtts = static_cast<UT_sint32>(pAP->getAttributeCount());
	for(i=0; i<nAtts;i++)
	{
		pAP->getNthAttribute(i,szName,szVal);
		if (szName && szVal)
		{
			UT_sint16 ix = getPacket_PTName_Index( static_cast<const char*>( szName ) );
			if (ix!=-1)
			{
				UT_DEBUGMSG(("ABI_Collab_Export::mapPropsAtts: [%d] '%s' = '%s'\n", ix, szName, szVal));
				atts[ ix ] = static_cast<const char*>( szVal );
			}
			else
			{
				UT_DEBUGMSG(("ABI_Collab_Export::mapPropsAtts: unknown attribute '%s' = '%s'\n", szName, szVal));
			}
		}
	}
	
	// map properties
	props.clear();
	UT_sint32 nProps = static_cast<UT_sint32>(pAP->getPropertyCount());
	for(i=0; i<nProps;i++)
	{
		pAP->getNthProperty(i,szName,szVal);
		if (szName && szVal)
		{
			props[static_cast<const char*>( szName )] = static_cast<const char*>( szVal );
		}
	}
}

/*!
 * Returns true if the stop flag matches the start.
 */
bool ABI_Collab_Export::_isGlobEnd(UT_Byte istart, UT_Byte istop)
{
	if((istart == PX_ChangeRecord_Glob::PXF_UserAtomicStart) &&
	   (istop == PX_ChangeRecord_Glob::PXF_UserAtomicEnd))
		return true;
	
	if((istart == PX_ChangeRecord_Glob::PXF_MultiStepStart) &&
	   (PX_ChangeRecord_Glob::PXF_MultiStepEnd))
		return true;
	
	return false;
}

/*!
 * Implements the "change" method of the document listener class.
 * It takes the supplied change record and makes an XML-like representation
 * of it. Eventually these can either be stored in a file or sent over the
 * internet to a remote AbiWord where it can be translated back.
 */
bool ABI_Collab_Export::change(PL_StruxFmtHandle /*sfh*/,
				const PX_ChangeRecord * pcr)
{
	// build new packet
	ChangeRecordSessionPacket * newPacket = _buildPacket( pcr );
	UT_return_val_if_fail(newPacket, true); // unhandled change record type

	// update internal state
	if (pcr->getType() == PX_ChangeRecord::PXT_GlobMarker) 
	{
		const PX_ChangeRecord_Glob * pcrg = static_cast<const PX_ChangeRecord_Glob *>(pcr);
		if (m_pGlobPacket) 
		{
			UT_return_val_if_fail(m_pGlobPacket->getPackets().size() > 0, true);
			UT_return_val_if_fail(m_pGlobPacket->getPackets()[0]->getClassType() == PCT_Glob_ChangeRecordSessionPacket, true);

			Glob_ChangeRecordSessionPacket* m_pOpeningGlobMarker = static_cast<Glob_ChangeRecordSessionPacket*>(m_pGlobPacket->getPackets()[0]);
			if (_isGlobEnd(m_pOpeningGlobMarker->m_iGLOBType, pcrg->getFlags())) 
			{	
				m_pGlobPacket->addPacket( newPacket );
				
				// push packet
				m_pAbiCollab->push( m_pGlobPacket );
				m_pAbiCollab->addChangeAdjust( new ChangeAdjust(
					*m_pGlobPacket, 
					(m_pAbiCollab->getActivePacket() ? m_pAbiCollab->getActivePacket()->getPos() : -1), 
					m_pDoc->getMyUUIDString()) 
					);

				// reset glob state
				DELETEP(m_pGlobPacket);
				m_pGlobPacket = 0;
				return true;
				
			} 
			else 
			{
				// not end of glob? then this must be at least true
				UT_return_val_if_fail(pcrg->getFlags() != PX_ChangeRecord_Glob::PXF_UserAtomicStart, false);
			}
		} 
		else 
		{
			m_pGlobPacket = new GlobSessionPacket( newPacket->getSessionId(), newPacket->getDocUUID() );
		}
	}

	_handleNewPacket( newPacket, pcr );
	return true;
}

/*!
 * Handles new packet, by either adding it to a glob if we're doing a glob,
 * or by pushing it to the collab session directly
 */
void ABI_Collab_Export::_handleNewPacket(ChangeRecordSessionPacket* pPacket, const PX_ChangeRecord* /*pcr*/) 
{
	UT_return_if_fail( pPacket );
	
	if (m_pGlobPacket) 
	{
		m_pGlobPacket->addPacket( pPacket );
	}
	else 
	{
		m_pAbiCollab->push( pPacket );
		m_pAbiCollab->addChangeAdjust( new ChangeAdjust(
				*pPacket, 
				(m_pAbiCollab->getActivePacket() ? m_pAbiCollab->getActivePacket()->getPos() : -1), 
				m_pDoc->getMyUUIDString())
				);
		DELETEP(pPacket);
	}
}

/*!
 * Implements the "insertStrux" method of the document listener class.
 * It takes the supplied change record and makes an XML-like representation
 * of it. Eventually these can either be stored in a file or sent over the
 * internet to a remote AbiWord where it can be translated back.
 */
bool ABI_Collab_Export::insertStrux(PL_StruxFmtHandle /*sfh*/,
									const PX_ChangeRecord * pcr,
									PL_StruxDocHandle sdh,
									PL_ListenerId lid,
									void (* pfnBindHandles)(PL_StruxDocHandle sdhNew,
															PL_ListenerId lid,
															PL_StruxFmtHandle sfhNew))
{
	if(pfnBindHandles)
	{
		PL_StruxFmtHandle sfhNew = static_cast<PL_StruxFmtHandle>(this);
		pfnBindHandles(sdh,lid,sfhNew);
	}

	ChangeRecordSessionPacket* newPacket = _buildPacket( pcr );
	UT_return_val_if_fail(newPacket, true); // unhandled change record type
	_handleNewPacket( newPacket, pcr );
	return true;
}

/*!
 * Implements the signal() method of the Document listener class.
 */
bool ABI_Collab_Export::signal(UT_uint32 iSignal)
{
	UT_DEBUGMSG(("ABI_Collab_Export::signal()! %d \n",iSignal));
	
	if(iSignal == PD_SIGNAL_SAVEDOC)
	{
			return true;
	}
	SignalSessionPacket* ssp = new SignalSessionPacket(m_pAbiCollab->getSessionId(), m_pDoc->getOrigDocUUIDString(), iSignal);
	if(m_pGlobPacket)
	{
		m_pGlobPacket->addPacket( ssp );
	}
	else
	{
		m_pAbiCollab->push( ssp );
		delete ssp;
	}
	return true;
}

/*!
 * This virtual method is called from the AbiWord main tree upon doing a replace document with an attached
 * AbiCollab_Export connected to the document.
 *
 * Note: this is a really weird signal, coming from a PD_Document
 * Note: If anything, a Frame should emit this signal to its listeners
 */
void ABI_Collab_Export::setNewDocument(PD_Document * /*pDoc*/)
{
	UT_DEBUGMSG(("ABI_Collab_Export::setNewDocument() - ignored\n"));
	// we are connected to a session, we can't just replace the document!
	UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
	
	// TODO: inform the session manager to kill off this session, as it has seriously gone bad
	// ...
}

/*!
 * This virtual method is called if the attached document is deleted with an attached
 * AbiCollab_Export connected to the document.
 */
void ABI_Collab_Export::removeDocument(void)
{
	UT_DEBUGMSG(("ABI_Collab_Export::removeDocument()\n"));

	// inform the session manager that this session is being (forcefully) closed
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_if_fail(pManager);
	
	// NOTE: don't do anything after this line anymore, as this will self-destruct us!
	pManager->disconnectSession(m_pAbiCollab);
}

void ABI_Collab_Export::masterInit()
{
	UT_DEBUGMSG(("ABI_Collab_Export::masterInit()\n"));

	// NOTE: it's important that this function resets all state, as it can be
	// called in the middle of an already running collaboration session
	// (eg. when a session takeover happens)

	_cleanup();
	_init();
}

void ABI_Collab_Export::slaveInit(const UT_UTF8String& docUUID, UT_sint32 iRemoteRev)
{
	UT_DEBUGMSG(("ABI_Collab_Export::slaveInit() - docUUID: %s, iRev: %d\n", docUUID.utf8_str(), iRemoteRev));

	// NOTE: it's important that this function resets all state, as it can be
	// called in the middle of an already running collaboration session
	// (eg. when a session takeover happens)

	_cleanup();
	_init();

	// initialize the adjustment stack
	ChangeRecordSessionPacket voidPacket;
	voidPacket.setDocUUID(docUUID);
	voidPacket.setRev(iRemoteRev);
	m_pAbiCollab->addChangeAdjust(new ChangeAdjust(voidPacket, static_cast<PT_DocPosition>(0), docUUID));
}

void ABI_Collab_Export::_init()
{
	m_chgMaskCached = 0;
	m_bCacheChanges = false;
	m_iBlockIndex = -1;
	m_iSectionIndex = -1;
	m_pGlobPacket = NULL;
	m_vecAdjusts.clear();
}

void ABI_Collab_Export::_cleanup()
{
	UT_sint32 i = static_cast<UT_sint32>(m_vecAdjusts.getItemCount());
	while(i > 0)
	{
		delete m_vecAdjusts.getNthItem(i-1);
		i--;
	}
	DELETEP( m_pGlobPacket );
}
