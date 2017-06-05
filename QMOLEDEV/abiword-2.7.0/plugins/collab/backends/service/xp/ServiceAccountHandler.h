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

#ifndef __SERVICEACCOUNTHANDLER__
#define __SERVICEACCOUNTHANDLER__

#include <string>
#include <vector>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>

#include "xap_Types.h"
#include "ut_string_class.h"

#include <core/account/xp/AccountHandler.h>
#include "AbiCollabSaveInterceptor.h"
#include "AsioRealmProtocol.h"
#include "pl_Listener.h"
#include "RealmConnection.h"
#include "RealmBuddy.h"
#include "RealmProtocol.h"
#include "ServiceBuddy.h"
#include "ServiceErrorCodes.h"
#include "soa.h"

namespace acs = abicollab::service;
namespace rpv1 = realm::protocolv1;

class PD_Document;
class GetSessionsResponseEvent;
class JoinSessionRequestResponseEvent;
class ServiceBuddy;
class AbiCollabService_Export;
class RealmBuddy;

extern AccountHandlerConstructor ServiceAccountHandlerConstructor;

typedef boost::shared_ptr< std::map<std::string, GetSessionsResponseEvent> > BuddySessionsPtr;

#define SERVICE_ACCOUNT_HANDLER_TYPE "com.abisource.abiword.abicollab.backend.service"

class ServiceAccountHandler : public AccountHandler
{
public:
	ServiceAccountHandler();
	virtual ~ServiceAccountHandler();

	static bool								askPassword(const std::string& email, std::string& password);

	// housekeeping
	static UT_UTF8String					getStaticStorageType();
	virtual UT_UTF8String					getStorageType()
		{ return getStaticStorageType(); }	
	virtual UT_UTF8String					getDescription();
	virtual UT_UTF8String					getDisplayType();

	// dialog management 
	virtual void							storeProperties();
	static XAP_Dialog_Id					getDialogGenericInputId();
	static XAP_Dialog_Id					getDialogGenericProgressId();

	// connection management
	virtual ConnectResult					connect();
	virtual bool							disconnect();
	virtual bool							isOnline();
	ConnectionPtr							getConnection(PD_Document* pDoc);

	// user management
	virtual BuddyPtr						constructBuddy(const PropertyMap& props);
	virtual BuddyPtr						constructBuddy(const std::string& descriptor, BuddyPtr pBuddy);
	virtual bool							allowsManualBuddies()
		{ return false; }
	virtual bool							recognizeBuddyIdentifier(const std::string& identifier);
	virtual void							forceDisconnectBuddy(BuddyPtr) { /* TODO: implement me? */ }

	// packet management
	virtual bool							send(const Packet* packet);
	virtual bool							send(const Packet* packet, BuddyPtr pBuddy);

	// session management
	virtual void							getSessionsAsync();
	virtual void							getSessionsAsync(const Buddy& buddy);
	virtual bool							hasSession(const UT_UTF8String& sSessionId);
	virtual void							joinSessionAsync(BuddyPtr pBuddy, DocHandle& docHandle);
	acs::SOAP_ERROR							openDocument(UT_uint64 doc_id, UT_uint64 revision, const std::string& session_id, PD_Document** pDoc, XAP_Frame* pFrame);
	UT_Error								saveDocument(PD_Document* pDoc, ConnectionPtr connection_ptr);
	void									removeExporter(void);
	
	// session management
	virtual bool							allowsSessionTakeover()
		{ return true; }

	// signal management
	virtual void							signal(const Event& event, BuddyPtr pSource);

	// misc functions
	static bool								parseUserInfo(const std::string& userinfo, uint64_t& user_id);
	
	static XAP_Dialog_Id		 			m_iDialogGenericInput;
	static XAP_Dialog_Id		 			m_iDialogGenericProgress;
	static AbiCollabSaveInterceptor			m_saveInterceptor;

private:
	ServiceBuddyPtr							_getBuddy(ServiceBuddyPtr pBuddy);

	template <class T>
	void _send(boost::shared_ptr<T> packet, RealmBuddyPtr recipient)
	{
		realm::protocolv1::send(*packet, recipient->connection()->socket(),
			boost::bind(&ServiceAccountHandler::_write_handler, this,
							asio::placeholders::error, asio::placeholders::bytes_transferred, recipient,
							boost::static_pointer_cast<rpv1::Packet>(packet)));
	}

	void									_write_handler(const asio::error_code& e, std::size_t bytes_transferred,
													boost::shared_ptr<const RealmBuddy> recipient, boost::shared_ptr<rpv1::Packet> packet);

	void									_write_result(const asio::error_code& e, std::size_t bytes_transferred,
													ConnectionPtr connection, boost::shared_ptr<rpv1::Packet> packet);

	acs::SOAP_ERROR							_listDocuments(const std::string uri, const std::string email, const std::string password, 
													bool verify_webapp_host, BuddySessionsPtr sessions_ptr);
	void									_listDocuments_cb(acs::SOAP_ERROR error, BuddySessionsPtr sessions_ptr);
	
	acs::SOAP_ERROR							_openDocumentMaster(ConnectionPtr connection, soa::CollectionPtr rcp, PD_Document** pDoc, XAP_Frame* pFrame, 
													const std::string& session_id, const std::string& filename);
	acs::SOAP_ERROR							_openDocumentSlave(ConnectionPtr connection, PD_Document** pDoc, XAP_Frame* pFrame, 
													const std::string& filename);
	
	void									_handleJoinSessionRequestResponse(
													JoinSessionRequestResponseEvent* jsre, BuddyPtr pBuddy, 
													XAP_Frame* pFrame, PD_Document** pDoc, const std::string& filename);
	void									_handleRealmPacket(ConnectionPtr connection);
	ConnectionPtr							_getConnection(const std::string& session_id);	
	void									_removeConnection(const std::string& session_id);
	void									_handleMessages(ConnectionPtr connection);
	void									_parseSessionFiles(soa::ArrayPtr files_array, GetSessionsResponseEvent& gsre);
	bool									_splitDescriptor(const std::string& descriptor, uint64_t& user_id, uint8_t& conn_id, std::string& domain);
	std::string								_getDomain();
	bool									_parseUserInfo(const std::string& userinfo, uint64_t& user_id);

	bool									m_bOnline;  // only used to determine if we are allowed to 
														// communicate with abicollab.net or not
	std::vector<ConnectionPtr>				m_connections;
	std::string								m_ssl_ca_file;
	PL_ListenerId             m_iListenerID;
	AbiCollabService_Export * m_pExport;
	  
};

#endif /* __SERVICEACCOUNTHANDLER__ */
