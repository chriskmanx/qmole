/*
 * AbiCollab - Code to enable the modification of remote documents.
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
 
#ifndef ABICOLLAB_PACKET_H
#define ABICOLLAB_PACKET_H

#include <boost/format.hpp>

#include "ut_types.h"
#include "ut_string_class.h"
#include "px_ChangeRecord.h"
#include "ut_stack.h"
#include "Serialization.h"

#include <string>
#include <vector>

class Buddy;

enum PacketType
{
	PT_Session = 0,
	PT_Event,
	PT_Handler
};

enum PacketEventType
{
	PTE_AccountAddBuddyRequest,
	PTE_StartSession,
	PTE_JoinSession,
	PTE_DisjoinSession,
	PTE_CloseSession
};

enum PClassType // send over the net to identify classes
{
	// NOTE: do not reshuffle these values: it will make the protocol incompatible with previous versions
	
	//
	// base
	//
	PCT_Packet = 0,
	PCT_EventPacket,
	PCT_Event,
	PCT_ProtocolErrorPacket,
	
	//
	// session packets
	//
	/* misc. session packets */
	PCT_SignalSessionPacket = 0x10,				// update _PCT_FirstSessionPacket if you move this
	PCT_RevertSessionPacket,
	PCT_RevertAckSessionPacket,
	PCT_GlobSessionPacket,
	/* changerecord session packets */
	PCT_ChangeRecordSessionPacket,				// update _PCT_FirstChangeRecord if you move this
	PCT_Props_ChangeRecordSessionPacket,
	PCT_InsertSpan_ChangeRecordSessionPacket,
	PCT_ChangeStrux_ChangeRecordSessionPacket,
	PCT_DeleteStrux_ChangeRecordSessionPacket,
	PCT_Object_ChangeRecordSessionPacket,
	PCT_Data_ChangeRecordSessionPacket,
	PCT_Glob_ChangeRecordSessionPacket,			// update _PCT_LastChangeRecord if you move this
	/* session takeover packets */
	PCT_SessionTakeoverRequestPacket = 0x40,	// update _PCT_FirstSessionTakeoverPacket if you move this
	PCT_SessionTakeoverAckPacket,
	PCT_SessionFlushedPacket,
	PCT_SessionReconnectRequestPacket,
	PCT_SessionReconnectAckPacket,				// update _PCT_LastSessionTakeoverPacket and _PCT_LastSessionPacket if you move this

	//
	// events
	//
	PCT_AccountNewEvent = 0x80,
	PCT_AccountOnlineEvent,
	PCT_AccountOfflineEvent,
	PCT_AccountAddBuddyEvent,
	PCT_AccountDeleteBuddyEvent,
	PCT_AccountBuddyOnlineEvent,
	PCT_AccountBuddyOfflineEvent,
	PCT_AccountAddBuddyRequestEvent,
	PCT_AccountBuddyAddDocumentEvent,
	PCT_StartSessionEvent,
	PCT_JoinSessionEvent,
	PCT_JoinSessionRequestEvent,
	PCT_JoinSessionRequestResponseEvent,
	PCT_DisjoinSessionEvent,
	PCT_CloseSessionEvent,
	PCT_GetSessionsEvent,
	PCT_GetSessionsResponseEvent,
	
	//
	// meta values (KEEP THESE UPDATED WHEN ADDING NEW PACKET TYPES!!)
	//
	_PCT_FirstSessionPacket = PCT_SignalSessionPacket,
	_PCT_LastSessionPacket = PCT_SessionReconnectAckPacket,
	
	_PCT_FirstChangeRecord = PCT_ChangeRecordSessionPacket,
	_PCT_LastChangeRecord = PCT_Glob_ChangeRecordSessionPacket,
	
	_PCT_FirstSessionTakeoverPacket = PCT_SessionTakeoverRequestPacket,
	_PCT_LastSessionTakeoverPacket = PCT_SessionReconnectAckPacket,
	
	_PCT_FirstEvent = PCT_AccountNewEvent,
	_PCT_LastEvent = PCT_GetSessionsResponseEvent
};

class PX_ChangeRecord;
class SessionPacket;
class AbiCollab;
class AccountHandler;

extern const gchar * szAbiCollab_Packet_PTName[];
UT_sint16 getPacket_PTName_Index( const gchar* name );	// returns -1, or 0 <= x <= sizeof(szAbiCollab_Packet_PTName)

/*************************************************************
 * Packets                                                   *
 *************************************************************/

#define DECLARE_ABSTRACT_PACKET(Class)									\
	virtual PClassType getClassType() const { return PCT_##Class; }

#define DECLARE_SERIALIZABLE_PACKET										\
	virtual void serialize(Archive & ar);

#define DECLARE_PACKET(Class)											\
	DECLARE_ABSTRACT_PACKET(Class)										\
	DECLARE_SERIALIZABLE_PACKET											\
	virtual Packet* clone() const { return new Class( *this ); }		\
	static Packet* create() { return new Class(); }

#define REGISTER_PACKET(Class)											\
	struct PacketRegister##Class {										\
		PacketRegister##Class() {										\
			Packet::registerPacketClass( PCT_##Class, Class::create,	\
				#Class );												\
		}																\
	};																	\
	static PacketRegister##Class _PacketRegister##Class;

class Packet
{
public:
	DECLARE_ABSTRACT_PACKET(Packet);
	
	Packet();
	Packet( AbiCollab* session );
	virtual ~Packet() {}
	virtual Packet* clone() const = 0;

	const AbiCollab* getSession() const				{ return m_pSession; }
	AbiCollab* getSession() 						{ return m_pSession; }
	virtual UT_sint32 getProtocolVersion() const;
	
    virtual void serialize(Archive & ar);										// overridden automatically throught DECLARE_PACKET
	void setParent( Packet* pParent )		{ m_pParent = pParent; }
	Packet* getParent() 					{ return m_pParent; }
	
	virtual std::string	toStr() const;
	
protected:
	AbiCollab*			m_pSession;
	Packet*				m_pParent;
	
	/** Class reconstruction */
public:
	typedef Packet*(*PacketCreateFuncType)();
	static Packet* createPacket( PClassType eType );
	static const char* getPacketClassname( PClassType eType );
	static void registerPacketClass( PClassType eType, PacketCreateFuncType createFunc, const char* szClassName );
private:
	struct ClassData {
		PacketCreateFuncType	StaticConstructor;
		const char*				ClassName;
		ClassData() : StaticConstructor( NULL ), ClassName( NULL ) {}
	};
	typedef std::map<PClassType,ClassData> ClassMap;
	static ClassMap& GetClassMap();
};

/*************************************************************
 * SessionPackets                                            *
 *************************************************************/

class SessionPacket : public Packet
{
public:
	DECLARE_SERIALIZABLE_PACKET

	static bool isInstanceOf(const Packet& packet);
		
protected:
	SessionPacket() : m_sSessionId(""), m_sDocUUID("")  {}
	SessionPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID);

public:
	virtual const UT_UTF8String& getSessionId() const
		{ return m_sSessionId; }

	void setSessionId(const UT_UTF8String& sSessionId)
		{ m_sSessionId = sSessionId; }

	virtual const UT_UTF8String& getDocUUID() const
		{ return m_sDocUUID; }

	void setDocUUID(const UT_UTF8String& sDocUUID)
		{ m_sDocUUID = sDocUUID; }
		
	virtual std::string toStr() const;
	
private:
	UT_UTF8String			m_sSessionId;
	UT_UTF8String			m_sDocUUID;	
};

class AbstractChangeRecordSessionPacket : public SessionPacket
{
public:
	AbstractChangeRecordSessionPacket()
		: SessionPacket("", "")
		{}
		
	AbstractChangeRecordSessionPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID)
		: SessionPacket(sSessionId, sDocUUID)
		{}
	
	static bool isInstanceOf(const SessionPacket& packet);
	
	virtual PT_DocPosition getPos() const = 0;
	virtual UT_sint32 getLength() const = 0;
	virtual UT_sint32 getAdjust() const = 0;
	virtual UT_sint32 getRev() const = 0;
	virtual UT_sint32 getRemoteRev(void) const = 0;
};

class ChangeRecordSessionPacket : public AbstractChangeRecordSessionPacket
{
public:
	DECLARE_PACKET(ChangeRecordSessionPacket);
	ChangeRecordSessionPacket()
			: m_cType(PX_ChangeRecord::PXType(0)), 
			m_iLength(0),
			m_iAdjust(0),
			m_iPos(0),
			m_iRev(0), 
			m_iRemoteRev(0) {}
	ChangeRecordSessionPacket(
			const UT_UTF8String& sSessionId,
			PX_ChangeRecord::PXType cType, 
			const UT_UTF8String& sDocUUID, 
		 	PT_DocPosition iPos,
			int iRev, 
			int iRemoteRev);
	
	PX_ChangeRecord::PXType getPXType() const			{ return m_cType; }
	
	virtual PT_DocPosition getPos() const				{ return m_iPos; }
	virtual UT_sint32 getLength() const					{ return m_iLength; }
	virtual UT_sint32 getAdjust() const					{ return m_iAdjust; }
	virtual UT_sint32 getRev() const 					{ return m_iRev; }
	virtual UT_sint32 getRemoteRev(void) const			{ return m_iRemoteRev; }
	
	void setPos( UT_sint32 iPos )						{ m_iPos = iPos; }
	void setLength( UT_sint32 iLength )					{ m_iLength = iLength; }
	void setAdjust(UT_sint32 iAdjust)					{ m_iAdjust = iAdjust; }
	void setRev( UT_sint32 iRev )						{ m_iRev = iRev; }
	void setRemoteRev( UT_sint32 iRemoteRev )			{ m_iRemoteRev = iRemoteRev; }

	virtual std::string toStr() const;
	
private:
	PX_ChangeRecord::PXType		m_cType;

	UT_sint32					m_iLength;
	UT_sint32					m_iAdjust;

	PT_DocPosition				m_iPos;
	UT_sint32					m_iRev;
	UT_sint32					m_iRemoteRev;
};

class Props_ChangeRecordSessionPacket : public ChangeRecordSessionPacket {
public:
	DECLARE_PACKET(Props_ChangeRecordSessionPacket);
	Props_ChangeRecordSessionPacket() : m_szAtts(NULL), m_szProps(NULL) {}
	Props_ChangeRecordSessionPacket( const Props_ChangeRecordSessionPacket& );
	Props_ChangeRecordSessionPacket(
			const UT_UTF8String& sSessionId,
			PX_ChangeRecord::PXType cType, 
			const UT_UTF8String& sDocUUID, 
			PT_DocPosition iPos,
			int iRev, 
			int iRemoteRev)
	: ChangeRecordSessionPacket( sSessionId, cType, sDocUUID, iPos, iRev, iRemoteRev )
	, m_szAtts( NULL )
	, m_szProps( NULL )
	{}
	~Props_ChangeRecordSessionPacket() 
	{
		_freeProps();
		_freeAtts();
	}
	
	gchar** getProps() const										{ return m_szProps; }	
	const std::map<UT_UTF8String,UT_UTF8String>& getPropMap() const	{ return m_sProps; }
	std::map<UT_UTF8String,UT_UTF8String>& getPropMap() 			{ return m_sProps; }
	
	gchar** getAtts() const											{ return m_szAtts; }
	const std::map<UT_uint8,UT_UTF8String>& getAttMap() const		{ return m_sAtts; }
	std::map<UT_uint8,UT_UTF8String>& getAttMap() 					{ return m_sAtts; }
	gchar* getAttribute( const gchar* attr ) const;

	virtual std::string toStr() const;		
	
protected:
	gchar**									m_szAtts;
	gchar**									m_szProps;
	std::map<UT_uint8,UT_UTF8String>		m_sAtts;		// key is index into szAbiCollab_Packet_PTName
	std::map<UT_UTF8String,UT_UTF8String>	m_sProps;
	
	void _freeProps(); 
	void _freeAtts();
	void _fillProps();		// uses m_sProps to make m_szProps
	void _fillAtts();		// uses m_sAtts to make m_szAtts
};

class InsertSpan_ChangeRecordSessionPacket : public Props_ChangeRecordSessionPacket {
public:
	DECLARE_PACKET(InsertSpan_ChangeRecordSessionPacket);
	InsertSpan_ChangeRecordSessionPacket() : m_sText("") {}
	InsertSpan_ChangeRecordSessionPacket(
			const UT_UTF8String& sSessionId,
			PX_ChangeRecord::PXType cType, 
			const UT_UTF8String& sDocUUID, 
			PT_DocPosition iPos,
			int iRev, 
			int iRemoteRev)
	: Props_ChangeRecordSessionPacket( sSessionId, cType, sDocUUID, iPos, iRev, iRemoteRev )
	, m_sText("")
	{}
	
	virtual std::string toStr() const;
	
	// XXX: make proper setters/getters when done!
	UT_UTF8String				m_sText;
};

class ChangeStrux_ChangeRecordSessionPacket : public Props_ChangeRecordSessionPacket {
public:
	DECLARE_PACKET(ChangeStrux_ChangeRecordSessionPacket);
	ChangeStrux_ChangeRecordSessionPacket() : m_eStruxType(PTStruxType(0)) {}
	ChangeStrux_ChangeRecordSessionPacket(
			const UT_UTF8String& sSessionId,
			PX_ChangeRecord::PXType cType, 
			const UT_UTF8String& sDocUUID, 
			PT_DocPosition iPos,
			int iRev, 
			int iRemoteRev)
	: Props_ChangeRecordSessionPacket( sSessionId, cType, sDocUUID, iPos, iRev, iRemoteRev )
	{}
	
	virtual std::string toStr() const;

	// XXX: make proper setters/getters when done!
	PTStruxType					m_eStruxType;
};

class DeleteStrux_ChangeRecordSessionPacket : public ChangeRecordSessionPacket {
public:
	DECLARE_PACKET(DeleteStrux_ChangeRecordSessionPacket);
	DeleteStrux_ChangeRecordSessionPacket() {}
	DeleteStrux_ChangeRecordSessionPacket(
			const UT_UTF8String& sSessionId,
			PX_ChangeRecord::PXType cType, 
			const UT_UTF8String& sDocUUID, 
			PT_DocPosition iPos,
			int iRev, 
			int iRemoteRev)
	: ChangeRecordSessionPacket( sSessionId, cType, sDocUUID, iPos, iRev, iRemoteRev )
	{}
	
	virtual std::string toStr() const;	
	
	// XXX: make proper setters/getters when done!
	PTStruxType					m_eStruxType;
};

class Object_ChangeRecordSessionPacket : public Props_ChangeRecordSessionPacket {
public:
	DECLARE_PACKET(Object_ChangeRecordSessionPacket);
	Object_ChangeRecordSessionPacket() {}
	Object_ChangeRecordSessionPacket(
			const UT_UTF8String& sSessionId,
			PX_ChangeRecord::PXType cType, 
			const UT_UTF8String& sDocUUID, 
			PT_DocPosition iPos,
			int iRev, 
			int iRemoteRev)
	: Props_ChangeRecordSessionPacket( sSessionId, cType, sDocUUID, iPos, iRev, iRemoteRev )
	{}
	
	virtual std::string toStr() const;
	
	// XXX: make proper setters/getters when done!
	PTObjectType				m_eObjectType;
};

class Data_ChangeRecordSessionPacket : public Props_ChangeRecordSessionPacket {
public:
	DECLARE_PACKET(Data_ChangeRecordSessionPacket);
	Data_ChangeRecordSessionPacket()
	: m_bTokenSet(false)
	{}
	Data_ChangeRecordSessionPacket(
			const UT_UTF8String& sSessionId,
			PX_ChangeRecord::PXType cType, 
			const UT_UTF8String& sDocUUID, 
			PT_DocPosition iPos,
			int iRev, 
			int iRemoteRev)
	: Props_ChangeRecordSessionPacket( sSessionId, cType, sDocUUID, iPos, iRev, iRemoteRev )
	, m_bTokenSet(false)
	{}
	
	virtual std::string toStr() const;
	
	// XXX: make proper setters/getters when done!
	std::vector<char>			m_vecData;
	bool						m_bTokenSet;
	std::string					m_sToken;
};

class Glob_ChangeRecordSessionPacket : public ChangeRecordSessionPacket
{
public:
	DECLARE_PACKET(Glob_ChangeRecordSessionPacket);
	Glob_ChangeRecordSessionPacket() {}
	Glob_ChangeRecordSessionPacket( 
			const UT_UTF8String& sSessionId,
			PX_ChangeRecord::PXType cType, 
			const UT_UTF8String& sDocUUID, 
			PT_DocPosition iPos,
			int iRev, 
			int iRemoteRev)
	: ChangeRecordSessionPacket( sSessionId, cType, sDocUUID, iPos, iRev, iRemoteRev )
	{}

	virtual std::string toStr() const;
	
	// XXX: make proper setters/getters when done!
	UT_Byte							m_iGLOBType;
};

class GlobSessionPacket : public AbstractChangeRecordSessionPacket
{
public:
	DECLARE_PACKET(GlobSessionPacket);
	GlobSessionPacket() {}
	GlobSessionPacket( const GlobSessionPacket& Other );
	GlobSessionPacket( const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID )
		: AbstractChangeRecordSessionPacket(sSessionId, sDocUUID)
		{}
	~GlobSessionPacket();

	const std::vector<SessionPacket*>& getPackets() const	{ return m_pPackets; }

	void addPacket(SessionPacket* pPacket);
	
	virtual PT_DocPosition getPos() const;
	virtual UT_sint32 getLength() const;
	virtual UT_sint32 getAdjust() const;
	virtual UT_sint32 getRev() const;
	virtual UT_sint32 getRemoteRev(void) const;
	
	virtual std::string toStr() const;

private:
	std::vector<SessionPacket*>		m_pPackets;
};

class SignalSessionPacket : public SessionPacket
{
public:
	DECLARE_PACKET(SignalSessionPacket);
	SignalSessionPacket() {}
	SignalSessionPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID, UT_uint32 iSignal);
	
	UT_uint32 getSignalType() const
		{ return m_iSignal; }
	
	virtual std::string toStr() const;
		
private:
	UT_uint32	m_iSignal;
};

class RevertSessionPacket : public SessionPacket
{
public:
	DECLARE_PACKET(RevertSessionPacket);
	RevertSessionPacket() {}
	RevertSessionPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID, UT_sint32 iRev);

	UT_sint32			getRev() const
		{ return m_iRev; }
	
	virtual std::string toStr() const;

private:
	UT_sint32			m_iRev;
};

class RevertAckSessionPacket : public SessionPacket
{
public:
	DECLARE_PACKET(RevertAckSessionPacket);
	RevertAckSessionPacket() {}
	RevertAckSessionPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID, UT_sint32 iRev);

	UT_sint32			getRev() const
		{ return m_iRev; }
	
	virtual std::string toStr() const;

private:
	UT_sint32			m_iRev;
};


/*************************************************************
 * Session Takeover Packets                                  *
 *************************************************************/

class AbstractSessionTakeoverPacket : public SessionPacket
{
public:
	AbstractSessionTakeoverPacket() {}

	AbstractSessionTakeoverPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID)
		: SessionPacket(sSessionId, sDocUUID)
		{}
		
	static bool isInstanceOf(const SessionPacket& packet);
};

class SessionTakeoverRequestPacket : public AbstractSessionTakeoverPacket
{
public:
	DECLARE_PACKET(SessionTakeoverRequestPacket);
	SessionTakeoverRequestPacket() {}
	SessionTakeoverRequestPacket(
		const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID, 
		bool bPromote, const std::vector<std::string>& vBuddyIdentifiers
	);

	bool promote() const
		{ return m_bPromote; }

	const std::vector<std::string>& getBuddyIdentifiers() const
		{ return m_vBuddyIdentifiers; }
		
	virtual std::string toStr() const;

private:
	bool						m_bPromote;
	std::vector<std::string>	m_vBuddyIdentifiers;
};

class SessionTakeoverAckPacket : public AbstractSessionTakeoverPacket
{
public:
	DECLARE_PACKET(SessionTakeoverAckPacket);
	SessionTakeoverAckPacket() {}
	SessionTakeoverAckPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID)
		: AbstractSessionTakeoverPacket(sSessionId, sDocUUID) { }

	virtual std::string toStr() const;	
};

class SessionFlushedPacket : public AbstractSessionTakeoverPacket
{
public:
	DECLARE_PACKET(SessionFlushedPacket);
	SessionFlushedPacket() {}
	SessionFlushedPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID)
		: AbstractSessionTakeoverPacket(sSessionId, sDocUUID) { }

	virtual std::string toStr() const;
};

class SessionReconnectRequestPacket : public AbstractSessionTakeoverPacket
{
public:
	DECLARE_PACKET(SessionReconnectRequestPacket);
	SessionReconnectRequestPacket() {}
	SessionReconnectRequestPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID)
		: AbstractSessionTakeoverPacket(sSessionId, sDocUUID) { }

	virtual std::string toStr() const;
};

class SessionReconnectAckPacket : public AbstractSessionTakeoverPacket
{
public:
	DECLARE_PACKET(SessionReconnectAckPacket);
	SessionReconnectAckPacket() {}
	SessionReconnectAckPacket(const UT_UTF8String& sSessionId, const UT_UTF8String& sDocUUID,
		UT_sint32 iRev);

	UT_sint32					getRev() const
		{ return m_iRev; }
	
	virtual std::string toStr() const;

private:
	UT_sint32					m_iRev;	
};

#endif /* ABICOLLAB_PACKET_H */
