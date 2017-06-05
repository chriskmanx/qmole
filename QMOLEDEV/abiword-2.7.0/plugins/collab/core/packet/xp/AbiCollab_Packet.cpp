/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2005 by Martin Sevior
 * Copyright (C) 2006 by Marc Maurer <uwog@uwog.net>
 * Copyright (C) 2007 One Laptop Per Child
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

#include <string>
#include <vector>

#include "ut_vector.h"
#include "pd_Document.h"
#include "px_ChangeRecord.h"
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
#include "AbiCollab_Packet.h"
#include "pt_Types.h"
#include <plugin/xp/AbiCollab_Plugin.h>
#include <account/xp/Buddy.h>

const gchar * szAbiCollab_Packet_PTName[] =
{
    PT_STYLE_ATTRIBUTE_NAME,
    PT_LEVEL_ATTRIBUTE_NAME,
    PT_LISTID_ATTRIBUTE_NAME,
    PT_PARENTID_ATTRIBUTE_NAME,
    PT_NAME_ATTRIBUTE_NAME,
    PT_TYPE_ATTRIBUTE_NAME,
    PT_BASEDON_ATTRIBUTE_NAME,
    PT_FOLLOWEDBY_ATTRIBUTE_NAME,
    PT_ID_ATTRIBUTE_NAME,
    PT_HEADER_ATTRIBUTE_NAME,
    PT_HEADEREVEN_ATTRIBUTE_NAME,
    PT_HEADERFIRST_ATTRIBUTE_NAME,
    PT_HEADERLAST_ATTRIBUTE_NAME,
    PT_FOOTER_ATTRIBUTE_NAME,
    PT_FOOTEREVEN_ATTRIBUTE_NAME,
    PT_FOOTERFIRST_ATTRIBUTE_NAME,
    PT_FOOTERLAST_ATTRIBUTE_NAME,
    PT_REVISION_ATTRIBUTE_NAME,
    PT_REVISION_DESC_ATTRIBUTE_NAME,
    PT_REVISION_TIME_ATTRIBUTE_NAME,
    PT_REVISION_VERSION_ATTRIBUTE_NAME,
    PT_DOCPROP_ATTRIBUTE_NAME,
    PT_ID_ATTRIBUTE_NAME,
    PT_STRUX_IMAGE_DATAID,
    PT_XID_ATTRIBUTE_NAME,
    PT_DATAITEM_ATTRIBUTE_NAME,
    PT_IMAGE_DATAID,
    PT_IMAGE_TITLE,
    PT_IMAGE_DESCRIPTION,
    PT_DATA_PREVIEW,
    PT_HYPERLINK_TARGET_NAME,
    PT_ANNOTATION_NUMBER,
    PT_AUTHOR_NAME
};

UT_sint16 getPacket_PTName_Index( const gchar* name )
{
	for (UT_uint8 i=0; i<sizeof(szAbiCollab_Packet_PTName)/sizeof(szAbiCollab_Packet_PTName[0]); ++i) 
	{
		if (!strcmp( name, szAbiCollab_Packet_PTName[i] ))
		{
			return UT_sint16(i);
		}
	}
	return -1;
}

/* ***************************************************** */
/* *                Packet                               */
/* ***************************************************** */

Packet::Packet()
: m_pSession( NULL )
, m_pParent( NULL )
{
}

Packet::Packet( AbiCollab* session ) 
: m_pSession( session )
, m_pParent( NULL )
{
}

void Packet::serialize(Archive& /*ar*/)
{
}

Packet* Packet::createPacket( PClassType eType ) 
{
	ClassMap& map = GetClassMap();
	ClassMap::iterator it = map.find( eType );
	if (it==map.end()) return NULL;	// not found, perfectly valid (in use by registerPacketClass)
	return (*it).second.StaticConstructor();
}

const char* Packet::getPacketClassname( PClassType eType )
{
	ClassMap& map = GetClassMap();
	ClassMap::iterator it = map.find( eType );
	if (it==map.end()) return "unknown";	// not found, perfectly valid (in use by registerPacketClass)
	return (*it).second.ClassName;
}

void Packet::registerPacketClass( PClassType eType, PacketCreateFuncType createFunc, const char* szClassName ) 
{
	UT_return_if_fail( !createPacket(eType) ); // if this fails, we have a duplicate eType registration
	ClassData cd;
	cd.StaticConstructor = createFunc;
	cd.ClassName = szClassName;
	GetClassMap()[eType] = cd;
}

Packet::ClassMap& Packet::GetClassMap() {
	
	static ClassMap _ClassMap;
	return _ClassMap;
}

std::string	Packet::toStr() const
{
	return str(boost::format("Packet: hasParent: %1%\n") % (m_pParent ? "yes" : "no"));
}

UT_sint32 Packet::getProtocolVersion() const
{ 
	return ABICOLLAB_PROTOCOL_VERSION; 
}

/* ***************************************************** */
/* *                   SessionPacket                     */
/* ***************************************************** */

SessionPacket::SessionPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID) 
	: Packet(),
	m_sSessionId(sSessionId),
	m_sDocUUID(sDocUUID)
{
}

void SessionPacket::serialize( Archive& ar )
{
	Packet::serialize( ar );
	
	// the state of m_pParent influences the way we serialize,
	// so when we load a glob, we need to set the glob as parent
	// of all its sub packets, BEFORE they are serialized, or serialization
	// will go wrong
	if (!m_pParent)
	{
		ar << m_sSessionId;
		ar << m_sDocUUID;
	}
}

bool SessionPacket::isInstanceOf(const Packet& packet)
{
	return (packet.getClassType() >= _PCT_FirstSessionPacket && packet.getClassType() <= _PCT_LastSessionPacket);
}

std::string SessionPacket::toStr() const
{
	return Packet::toStr() + 
		str(boost::format("SessionPacket: m_sSessionId: %1%, m_sDocUUID: %2%\n") % m_sSessionId.utf8_str() % m_sDocUUID.utf8_str());
}

/* ***************************************************** */
/* *            AbstractChangeRecordSessionPacket        */
/* ***************************************************** */

bool AbstractChangeRecordSessionPacket::isInstanceOf(const SessionPacket& packet)
{
	return (packet.getClassType() == PCT_GlobSessionPacket) || 
			(packet.getClassType() >= _PCT_FirstChangeRecord && packet.getClassType() <= _PCT_LastChangeRecord);
}

/* ***************************************************** */
/* *          ChangeRecordSessionPacket                  */
/* ***************************************************** */

ChangeRecordSessionPacket::ChangeRecordSessionPacket(
			const UT_UTF8String& sSessionId,
			PX_ChangeRecord::PXType cType, 
			const UT_UTF8String& sDocUUID, 
			PT_DocPosition iPos,
			int iRev,
			int iRemoteRev)
	: AbstractChangeRecordSessionPacket(sSessionId, sDocUUID),
	m_cType(cType),
	m_iLength(0),
	m_iAdjust(0),
	m_iPos(iPos),
	m_iRev(iRev),
	m_iRemoteRev(iRemoteRev)
{
}

void ChangeRecordSessionPacket::serialize( Archive& ar )
{
	SessionPacket::serialize( ar );
	ar << (int&)m_cType;
	ar << COMPACT_INT(m_iPos);	
	ar << COMPACT_INT(m_iLength);
	ar << COMPACT_INT(m_iAdjust);
	ar << COMPACT_INT(m_iRev);
	ar << COMPACT_INT(m_iRemoteRev);
}

static const std::string getPXTypeStr( PX_ChangeRecord::PXType t )
{
	UT_return_val_if_fail(t >= PX_ChangeRecord::PXT_GlobMarker && t <= PX_ChangeRecord::PXT_ChangeDocProp, str(boost::format( "<invalid value passed to getPXTypeStr: %d>" ) % int(t) ));
	static std::string pxTypeStrs[] = {
		"PXT_GlobMarker",
		"PXT_InsertSpan", 
		"PXT_DeleteSpan",
		"PXT_ChangeSpan", 
		"PXT_InsertStrux",
		"PXT_DeleteStrux", 
		"PXT_ChangeStrux",
		"PXT_InsertObject", 
		"PXT_DeleteObject",
		"PXT_ChangeObject", 
		"PXT_InsertFmtMark",
		"PXT_DeleteFmtMark", 
		"PXT_ChangeFmtMark",
		"PXT_ChangePoint", 
		"PXT_ListUpdate", 
		"PXT_StopList", 
		"PXT_UpdateField",
		"PXT_RemoveList",
		"PXT_UpdateLayout",
		"PXT_AddStyle",
		"PXT_RemoveStyle",
		"PXT_CreateDataItem",
		"PXT_ChangeDocProp",
	};
	return pxTypeStrs[ int(t)+1 ];
}

std::string ChangeRecordSessionPacket::toStr() const
{
	return SessionPacket::toStr() + 
		str(boost::format("ChangeRecordSessionPacket: m_cType: %1%(%2%), m_iLength: %3%, m_iAdjust: %4%, m_iPos: %5%, m_iRev: %6%, m_iRemoteRev: %7%\n") 
				% getPXTypeStr(m_cType).c_str() % m_cType % m_iLength % m_iAdjust % m_iPos % m_iRev % m_iRemoteRev );
}

/* ***************************************************** */
/* *            *_ChangeRecordSessionPacket              */
/* ***************************************************** */

Props_ChangeRecordSessionPacket::Props_ChangeRecordSessionPacket( const Props_ChangeRecordSessionPacket& Other )
: ChangeRecordSessionPacket( Other )
, m_szAtts( NULL )
, m_szProps( NULL )
, m_sAtts( Other.m_sAtts )
, m_sProps( Other.m_sProps )
{
	_fillProps();
	_fillAtts();
}

void Props_ChangeRecordSessionPacket::serialize( Archive& ar )
{
	ChangeRecordSessionPacket::serialize( ar );
	ar << m_sProps << m_sAtts;
	if (ar.isLoading())
	{
		_fillProps();
		_fillAtts();
	}
}

void Props_ChangeRecordSessionPacket::_freeProps()
{
	if (m_szProps == NULL)
		return;
	
	UT_sint32 i = 0;
	while(m_szProps[i] != NULL)
	{
		FREEP(m_szProps[i]);
		i++;
	}
	delete [] m_szProps;
	m_szProps = NULL;
}

void Props_ChangeRecordSessionPacket::_freeAtts()
{
	if (m_szAtts == NULL)
		return;
	
	UT_sint32 i = 0;
	while(m_szAtts[i] != NULL)
	{
		FREEP(m_szAtts[i]);
		i++;
	}
	delete [] m_szAtts;
	m_szAtts = NULL;
}

void Props_ChangeRecordSessionPacket::_fillProps()
{
	_freeProps();
	
	m_szProps = new gchar* [m_sProps.size()*2 + 1];
	UT_uint32 i = 0;
	for (std::map<UT_UTF8String,UT_UTF8String>::iterator it=m_sProps.begin(); it!=m_sProps.end(); ++it) 
	{
		m_szProps[i] = g_strdup((*it).first.utf8_str());
		m_szProps[i+1] = g_strdup((*it).second.utf8_str());
		i += 2;
	}
	m_szProps[i] = NULL;
}

void Props_ChangeRecordSessionPacket::_fillAtts()
{
	_freeAtts();
	
	m_szAtts = new gchar* [m_sAtts.size()*2 + 1];
	UT_uint32 i = 0;
	for (std::map<UT_uint8,UT_UTF8String>::iterator it=m_sAtts.begin(); it!=m_sAtts.end(); ++it) 
	{
		UT_uint8 attIndex = (*it).first;
		if (attIndex < (sizeof(szAbiCollab_Packet_PTName)/sizeof(szAbiCollab_Packet_PTName[0]))) 
		{
			m_szAtts[i] = g_strdup( szAbiCollab_Packet_PTName[attIndex] );
			m_szAtts[i+1] = g_strdup( (*it).second.utf8_str() );
			i += 2;
		}
		else
		{
			// invalid index, who sent this??
			UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
		}
	}
	m_szAtts[i] = NULL;
}

gchar* Props_ChangeRecordSessionPacket::getAttribute( const gchar* attr ) const
{
	UT_sint16 idx = getPacket_PTName_Index( attr );
	if (idx==-1) return NULL;	// guaranteed not in map
	std::map<UT_uint8,UT_UTF8String>::const_iterator it = m_sAtts.find( idx );
	if (it==m_sAtts.end()) return NULL;
	return static_cast<gchar*>( const_cast<char*>( (*it).second.utf8_str() ) );
}

std::string Props_ChangeRecordSessionPacket::toStr() const
{
	std::string propStr = ChangeRecordSessionPacket::toStr() + "Props_ChangeRecordSessionPacket: ";
	
	if (m_szAtts)
	{
		propStr += "attrs: ";
		UT_sint32 i = 0;
		while (m_szAtts[i] != NULL)
		{
			propStr += str(boost::format("%1%:%2%;") % m_szAtts[i] % m_szAtts[i+1]);
			i += 2;
		}
	}

	if (m_szProps)
	{
		propStr += " props: ";
		UT_sint32 i = 0;
		while (m_szProps[i] != NULL)
		{
			propStr += str(boost::format("%1%:%2%;") % m_szProps[i] % m_szProps[i+1]);
			i += 2;
		}
	}

	propStr += "\n";
	return propStr;
}


/* ***************************************************** */
/* *            *_ChangeRecordSessionPacket              */
/* ***************************************************** */

static const std::string getPTStruxTypeStr( PTStruxType p )
{
	UT_return_val_if_fail(p >= PTX_Section && p <= PTX_StruxDummy, str(boost::format( "<invalid value passed to getPTStruxTypeStr: %d>" ) % int(p) ));
	static std::string PacketSessionTypeStrs[] = {
		"PTX_Section",
		"PTX_Block",
		"PTX_SectionHdrFtr",
		"PTX_SectionEndnote",
		"PTX_SectionTable",
		"PTX_SectionCell",
		"PTX_SectionFootnote",
		"PTX_SectionMarginnote",
		"PTX_SectionAnnotation",
		"PTX_SectionFrame",
		"PTX_SectionTOC",
		"PTX_EndCell",
		"PTX_EndTable",
		"PTX_EndFootnote",
		"PTX_EndMarginnote",
		"PTX_EndEndnote",
		"PTX_EndAnnotation",
		"PTX_EndFrame",
		"PTX_EndTOC",
		"PTX_StruxDummy",
	};
	return PacketSessionTypeStrs[ int(p) ];
}

void InsertSpan_ChangeRecordSessionPacket::serialize( Archive& ar )
{
	Props_ChangeRecordSessionPacket::serialize( ar );
	ar << m_sText;
}

std::string InsertSpan_ChangeRecordSessionPacket::toStr() const
{
	return Props_ChangeRecordSessionPacket::toStr() +
			str(boost::format("InsertSpan_ChangeRecordSessionPacket: m_sText: %1%\n") % m_sText.utf8_str());
}

void ChangeStrux_ChangeRecordSessionPacket::serialize( Archive& ar )
{
	Props_ChangeRecordSessionPacket::serialize( ar );
	UT_ASSERT(sizeof(m_eStruxType)==4);
	ar << (int&)m_eStruxType;
}

std::string ChangeStrux_ChangeRecordSessionPacket::toStr() const
{
	return Props_ChangeRecordSessionPacket::toStr() +
		str(boost::format("ChangeStrux_ChangeRecordSessionPacket: m_eStruxType: %1%(%2%)\n") % getPTStruxTypeStr(m_eStruxType).c_str() % m_eStruxType );
}

void DeleteStrux_ChangeRecordSessionPacket::serialize( Archive& ar )
{
	ChangeRecordSessionPacket::serialize( ar );
	UT_ASSERT(sizeof(m_eStruxType)==4);
	ar << (int&)m_eStruxType;
}

std::string DeleteStrux_ChangeRecordSessionPacket::toStr() const
{
	return ChangeRecordSessionPacket::toStr() +
		str(boost::format("DeleteStrux_ChangeRecordSessionPacket: m_eStruxType: %1%(%2%)\n") % getPTStruxTypeStr(m_eStruxType).c_str() % m_eStruxType );
}

void Object_ChangeRecordSessionPacket::serialize( Archive& ar )
{
	Props_ChangeRecordSessionPacket::serialize( ar );
	UT_ASSERT(sizeof(m_eObjectType)==4);
	ar << (int&)m_eObjectType;
}

static const std::string getPTObjectTypeStr( PTObjectType p )
{
	UT_return_val_if_fail(p >= PTO_Image && p <= PTO_Embed, str(boost::format( "<invalid value passed to getPTObjectTypeStr: %d>" ) % int(p) ));
	static std::string PTObjectTypeStrs[] = {
		"PTO_Image", 
		"PTO_Field", 
		"PTO_Bookmark", 
		"PTO_Hyperlink", 
		"PTO_Math", 
		"PTO_Embed",
		"PTO_Annotation"
	};
	return PTObjectTypeStrs[ int(p) ];
} 

std::string Object_ChangeRecordSessionPacket::toStr() const
{
	return Props_ChangeRecordSessionPacket::toStr() +
		str(boost::format("Object_ChangeRecordSessionPacket: m_eObjectType: %1%\n") % getPTObjectTypeStr(m_eObjectType).c_str() );
}	

void Data_ChangeRecordSessionPacket::serialize( Archive& ar )
{
	Props_ChangeRecordSessionPacket::serialize( ar );
	ar << m_vecData << m_bTokenSet;
	if (m_bTokenSet) ar << m_sToken;
}

std::string Data_ChangeRecordSessionPacket::toStr() const
{
	return ChangeRecordSessionPacket::toStr() +
		str(boost::format("Data_ChangeRecordSessionPacket: m_vecData: %1%\n") % "[DATA]");
}	

void Glob_ChangeRecordSessionPacket::serialize( Archive& ar )
{
	ChangeRecordSessionPacket::serialize( ar );
	ar << m_iGLOBType;
}

std::string Glob_ChangeRecordSessionPacket::toStr() const
{
	return ChangeRecordSessionPacket::toStr() +
		str(boost::format("Glob_ChangeRecordSessionPacket: m_iGLOBType: %1%\n") % ((UT_sint32)m_iGLOBType));
}	

/* ***************************************************** */
/* *             GlobSessionPacket                       */
/* ***************************************************** */

GlobSessionPacket::GlobSessionPacket( const GlobSessionPacket& Other )
: AbstractChangeRecordSessionPacket( Other )
{
	UT_DEBUGMSG(("GlobSessionPacket::GlobSessionPacket: copying %u sub packets\n", Other.m_pPackets.size()));
	m_pPackets.resize( Other.m_pPackets.size() );
	for (size_t i=0; i<Other.m_pPackets.size(); ++i) {
		m_pPackets[i] = static_cast<SessionPacket*>( Other.m_pPackets[i]->clone() );
	}
}

GlobSessionPacket::~GlobSessionPacket()
{
	for (size_t i=0; i<m_pPackets.size(); ++i)
	{
		DELETEP( m_pPackets[i] );
	}
}

void GlobSessionPacket::addPacket(SessionPacket* pPacket)
{
	UT_return_if_fail(pPacket);
	
	m_pPackets.push_back(pPacket); 
	pPacket->setParent( this );
}

PT_DocPosition GlobSessionPacket::getPos() const
{
	PT_DocPosition  iGlobPos = 0;

	for (size_t i = 0; i < m_pPackets.size(); i++)
	{
		UT_continue_if_fail(m_pPackets[i]);
		SessionPacket* pPacket = m_pPackets[i];
		if (pPacket->getClassType() >= _PCT_FirstChangeRecord && pPacket->getClassType() <= _PCT_LastChangeRecord)
		{
			ChangeRecordSessionPacket* crp = static_cast<ChangeRecordSessionPacket*>( pPacket );
			if (crp->getPos() > 0) // TODO: check the types, instead of a '0' position check to see if this member contains a real position - MARCM
			{
				if (iGlobPos == 0 || crp->getPos() < iGlobPos)
				{
					iGlobPos = crp->getPos();
				}
			}
			
		}
	}

	return iGlobPos;
}

UT_sint32 GlobSessionPacket::getLength() const
{
//	UT_sint32 iGlobLength = 0;
	
	ChangeRecordSessionPacket* pFirstPacket = NULL;
	ChangeRecordSessionPacket* pLastPacket = NULL;
	
	for (size_t i = 0; i < m_pPackets.size(); i++)
	{
		SessionPacket* pPacket = m_pPackets[i];
		UT_continue_if_fail(pPacket);
		
		switch (pPacket->getClassType())
		{
			/* misc. session packets */
			case PCT_SignalSessionPacket:
				// these packets don't contribute to the length of a glob
				break;
			case PCT_RevertSessionPacket:
			case PCT_RevertAckSessionPacket:
				// these packets should never be in a glob
				UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
				break;
			case PCT_GlobSessionPacket:
				// we don't allow embedded globs in globs
				UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
				break;
			/* changerecord session packets */
			case PCT_ChangeRecordSessionPacket:
			case PCT_Props_ChangeRecordSessionPacket:
			case PCT_InsertSpan_ChangeRecordSessionPacket:
			case PCT_DeleteStrux_ChangeRecordSessionPacket:
			case PCT_Object_ChangeRecordSessionPacket:
			case PCT_Data_ChangeRecordSessionPacket:
			case PCT_ChangeStrux_ChangeRecordSessionPacket:
				{
					ChangeRecordSessionPacket* crp = static_cast<ChangeRecordSessionPacket*>(pPacket);
					
					UT_ASSERT_HARMLESS(crp->getLength() >= 0);
					
					if (!pFirstPacket ||
						crp->getPos() < pFirstPacket->getPos())
						pFirstPacket = crp;
					if (!pLastPacket || 
						crp->getPos() + crp->getLength() > pLastPacket->getPos() + pLastPacket->getLength())
						pLastPacket = crp;
				}
				break;
			case PCT_Glob_ChangeRecordSessionPacket:
				// this packet doesn't contribute to the length of a glob
				break;
			default:
				UT_ASSERT_HARMLESS(UT_NOT_IMPLEMENTED);
				break;
		}
	}
	
	if (!pFirstPacket)
		return 0;
	UT_return_val_if_fail(pLastPacket, 0);
	return pLastPacket->getPos() + pLastPacket->getLength() - pFirstPacket->getPos();
}

UT_sint32 GlobSessionPacket::getAdjust() const
{
	UT_sint32 iGlobAdjust = 0;
	
	for (size_t i = 0; i < m_pPackets.size(); i++)
	{
		UT_continue_if_fail(m_pPackets[i]);
		SessionPacket* pPacket = m_pPackets[i];
		if (pPacket->getClassType() >= _PCT_FirstChangeRecord && pPacket->getClassType() <= _PCT_LastChangeRecord)
		{
			ChangeRecordSessionPacket* crp = static_cast<ChangeRecordSessionPacket*>( pPacket );
			iGlobAdjust += crp->getAdjust();
		}
	}
	
	return iGlobAdjust;
}

UT_sint32 GlobSessionPacket::getRev() const
{
	for (size_t i = 0; i < m_pPackets.size(); i++)
	{
		UT_continue_if_fail(m_pPackets[i]);
		SessionPacket* pPacket = m_pPackets[i];
		if (pPacket->getClassType() >= _PCT_FirstChangeRecord && pPacket->getClassType() <= _PCT_LastChangeRecord)
		{
			ChangeRecordSessionPacket* crp = static_cast<ChangeRecordSessionPacket*>( pPacket );
			return crp->getRev();
		}
	}
	UT_return_val_if_fail(false, 0);
}

UT_sint32 GlobSessionPacket::getRemoteRev(void) const
{
	for (size_t i = 0; i < m_pPackets.size(); i++)
	{
		UT_continue_if_fail(m_pPackets[i]);
		SessionPacket* pPacket = m_pPackets[i];
		if (pPacket->getClassType() >= _PCT_FirstChangeRecord && pPacket->getClassType() <= _PCT_LastChangeRecord)
		{
			ChangeRecordSessionPacket* crp = static_cast<ChangeRecordSessionPacket*>( pPacket );
			return crp->getRemoteRev();
		}
	}
	UT_return_val_if_fail(false, 0);
}

void GlobSessionPacket::serialize( Archive& ar )
{
	SessionPacket::serialize( ar );
	
	unsigned int count;
	if (ar.isLoading()) 
	{
		// load packet count
		ar << COMPACT_INT(count);
		m_pPackets.resize( count, NULL );
		
		// load packets
		for (size_t i=0; i<m_pPackets.size(); ++i)
		{
			UT_uint8 classId;
			ar << classId;
			SessionPacket* newPacket = static_cast<SessionPacket*>( Packet::createPacket( (PClassType)classId ) );
			UT_ASSERT(newPacket);		// should this be safer?
			newPacket->setParent( this );
			ar << *newPacket;
			m_pPackets[i] = newPacket;
			// for efficiency reasons, childs of a glob don't serialize their session 
			// and document id's; therefor we set them now manually
			newPacket->setSessionId(getSessionId());
			newPacket->setDocUUID(getDocUUID());
		}
	} 
	else 
	{
		// save packet count
		count = m_pPackets.size();
		ar << COMPACT_INT(count);
		
		// save packets
		for (size_t i=0; i<m_pPackets.size(); ++i) 
		{
			// get and check packet
			SessionPacket* sp = m_pPackets[i];
			UT_ASSERT(sp);				// this really may NEVER fail or addPacket malfunctioned!
			UT_uint8 classId = sp->getClassType();
			ar << classId << *sp;
		}
	}
}

std::string GlobSessionPacket::toStr() const
{
	std::string globStr = SessionPacket::toStr() + "GlobSessionPacket:\n";

	for (std::vector<SessionPacket*>::const_iterator cit = m_pPackets.begin(); cit != m_pPackets.end(); cit++)
	{
		globStr += "\n* ";
		globStr += (*cit)->toStr();
		globStr += "\n";
	}

	globStr += str(boost::format("Glob functions: getPos(): %1%, getLength(): %2%, getAdjust(): %3%, getRev(): %4%, getRemoteRev(): %5%\n") 
					% getPos() % getLength() % getAdjust() % getRev() % getRemoteRev());
	
	return globStr;
}

/* ***************************************************** */
/* *             SignalSessionPacket                     */
/* ***************************************************** */

SignalSessionPacket::SignalSessionPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID, UT_uint32 iSignal)
	: SessionPacket(sSessionId, sDocUUID),
	m_iSignal(iSignal)
{
}

void SignalSessionPacket::serialize( Archive& ar )
{
	SessionPacket::serialize( ar );
	ar << COMPACT_INT(m_iSignal);
}

std::string SignalSessionPacket::toStr() const
{
	return SessionPacket::toStr() +
		str(boost::format("SignalSessionPacket: m_iSignal: %1%\n") % m_iSignal);
}

/* ***************************************************** */
/* *             RevertSessionPacket                     */
/* ***************************************************** */

RevertSessionPacket::RevertSessionPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID, UT_sint32 iRev)
	: SessionPacket(sSessionId, sDocUUID),
	m_iRev(iRev)
{
}

void RevertSessionPacket::serialize( Archive& ar )
{
	SessionPacket::serialize( ar );
	ar << COMPACT_INT(m_iRev);
}

std::string RevertSessionPacket::toStr() const
{
	return SessionPacket::toStr() +
		str(boost::format("RevertSessionPacket: m_iRev: %1%\n") % m_iRev);
}

/* ***************************************************** */
/* *             RevertAckSessionPacket                  */
/* ***************************************************** */

RevertAckSessionPacket::RevertAckSessionPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID, UT_sint32 iRev)
	: SessionPacket(sSessionId, sDocUUID),
	m_iRev(iRev)
{
}

void RevertAckSessionPacket::serialize( Archive& ar )
{
	SessionPacket::serialize( ar );
	ar << COMPACT_INT(m_iRev);
}

std::string RevertAckSessionPacket::toStr() const
{
	return SessionPacket::toStr() +
		str(boost::format("RevertAckSessionPacket: m_iRev: %1%\n") % m_iRev);
}

/* ***************************************************** */
/* *            AbstractSessionTakeoverPacket            */
/* ***************************************************** */

bool AbstractSessionTakeoverPacket::isInstanceOf(const SessionPacket& packet)
{
	return (packet.getClassType() >= _PCT_FirstSessionTakeoverPacket && packet.getClassType() <= _PCT_LastSessionTakeoverPacket);
}

/* ***************************************************** */
/* *             SessionTakeoverRequestPacket            */
/* ***************************************************** */

SessionTakeoverRequestPacket::SessionTakeoverRequestPacket(
		const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID,
		bool bPromote, const std::vector<std::string>& vBuddyIdentifiers
	) : AbstractSessionTakeoverPacket(sSessionId, sDocUUID),
	m_bPromote(bPromote),
	m_vBuddyIdentifiers(vBuddyIdentifiers)
{
}

void SessionTakeoverRequestPacket::serialize( Archive& ar )
{
	SessionPacket::serialize( ar );
	ar << m_bPromote;
	ar << m_vBuddyIdentifiers;
}

std::string SessionTakeoverRequestPacket::toStr() const
{
	std::string s = SessionPacket::toStr() + 
		"SessionTakeoverRequestPacket:\n  promote: ";
	s += m_bPromote ? "true" : "false";
	s += "\n";
	for (std::vector<std::string>::const_iterator it = m_vBuddyIdentifiers.begin(); it != m_vBuddyIdentifiers.end(); it++)
		s += std::string("  Buddy: ") + *it + "\n";
	return s;
}

/* ***************************************************** */
/* *             SessionTakeoverAckPacket                */
/* ***************************************************** */

void SessionTakeoverAckPacket::serialize(Archive& ar)
{
	SessionPacket::serialize(ar);
}

std::string SessionTakeoverAckPacket::toStr() const
{
	return SessionPacket::toStr() + "SessionTakeoverAckPacket\n";
}

/* ***************************************************** */
/* *             SessionFlushedPacket                    */
/* ***************************************************** */

void SessionFlushedPacket::serialize(Archive& ar)
{
	SessionPacket::serialize(ar);
}

std::string SessionFlushedPacket::toStr() const
{
	return SessionPacket::toStr() + "SessionFlushedPacket\n";
}

/* ***************************************************** */
/* *             SessionReconnectRequestPacket           */
/* ***************************************************** */

void SessionReconnectRequestPacket::serialize(Archive& ar)
{
	SessionPacket::serialize(ar);
}

std::string SessionReconnectRequestPacket::toStr() const
{
	return SessionPacket::toStr() + "SessionReconnectRequestPacket\n";
}

/* ***************************************************** */
/* *             SessionReconnectAckPacket               */
/* ***************************************************** */

SessionReconnectAckPacket::SessionReconnectAckPacket(
	const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID, UT_sint32 iRev)
	: AbstractSessionTakeoverPacket(sSessionId, sDocUUID),
	m_iRev(iRev)
{
}

void SessionReconnectAckPacket::serialize(Archive& ar)
{
	SessionPacket::serialize(ar);
	ar << m_iRev;
}

std::string SessionReconnectAckPacket::toStr() const
{
	return SessionPacket::toStr() +
		str(boost::format("SessionReconnectAckPacket: m_iRev: %1%\n") % m_iRev);
}
