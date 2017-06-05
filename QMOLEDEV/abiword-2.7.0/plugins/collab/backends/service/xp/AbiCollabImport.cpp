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

#include <libxml/tree.h>
#include "xap_App.h"
#include "xap_DialogFactory.h"
#include "ServiceAccountHandler.h"
#include "ap_Dialog_GenericInput.h"
#include <core/session/xp/AbiCollabSessionManager.h>
#include "AbiCollabImport.h"
#include "ServiceErrorCodes.h"

class XmlDocDeleter {
public:
            void operator()(xmlDocPtr* doc) {
                    if (!doc || !*doc)
                            return;
                    xmlFreeDoc(*doc);
            }
};

IE_Imp_AbiCollabSniffer abicollab_sniffer; // FIXME: this is not really nice if we allow plugin-unloading in the future again

IE_Imp_AbiCollabSniffer::IE_Imp_AbiCollabSniffer()
	: IE_ImpSniffer("AbiWord::AbiCollab-1.0")
{
	IE_Imp::registerImporter(this);
}

IE_Imp_AbiCollabSniffer::~IE_Imp_AbiCollabSniffer()
{
	IE_Imp::unregisterImporter(this);
}

static IE_SuffixConfidence IE_Imp_AbiCollabSniffer__SuffixConfidence[] = {
	{ "abicollab", 	UT_CONFIDENCE_PERFECT 	},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_Imp_AbiCollabSniffer::getSuffixConfidence ()
{
	return IE_Imp_AbiCollabSniffer__SuffixConfidence;
}

UT_Confidence_t IE_Imp_AbiCollabSniffer::recognizeContents (const char * szBuf, UT_uint32 iNumbytes)
{
	std::string contents(szBuf, iNumbytes);
	if (contents.find("<abicollab>") != std::string::npos &&
		contents.find("<email>") != std::string::npos &&
		contents.find("<doc_id>") != std::string::npos &&
		contents.find("<revision>") != std::string::npos)
		return UT_CONFIDENCE_PERFECT;
	return UT_CONFIDENCE_ZILCH;
}

bool IE_Imp_AbiCollabSniffer::getDlgLabels (const char ** pszDesc, const char ** pszSuffixList, IEFileType * ft)
{
	// TODO: should we hide this from the menu ?
	*pszDesc = "Collaborative File Descriptor (.abicollab)";
	*pszSuffixList = "*.abicollab";
	*ft = getFileType();
	return true;
}

UT_Error IE_Imp_AbiCollabSniffer::constructImporter (PD_Document * pDocument, IE_Imp ** ppie)
{
	*ppie = new IE_Imp_AbiCollab(pDocument);
	return UT_OK;
}

// ******
// ** IE_Imp_AbiCollab
// ******

IE_Imp_AbiCollab::IE_Imp_AbiCollab(PD_Document* pDocument)
	: IE_Imp(pDocument)
{
	UT_DEBUGMSG(("IE_Imp_AbiCollab::IE_Imp_AbiCollab()\n"));
}

UT_Error IE_Imp_AbiCollab::_loadFile(GsfInput * input)
{
	UT_DEBUGMSG(("IE_Imp_AbiCollab::_loadFile()\n"));
	UT_return_val_if_fail(input, UT_ERROR);
	
	std::string email;
	std::string server;
	UT_sint64 doc_id;
	UT_sint64 revision;	

	// get the information needed to open the document
	if (!_parse(input, email, server, doc_id, revision))
		return UT_ERROR;
	
	// get an account handler to use for these settings
	ServiceAccountHandler* pAccount = _getAccount(email, server);
	UT_return_val_if_fail(pAccount, UT_ERROR);
	
	return _openDocument(input, pAccount, email, server, doc_id, revision);
}
	
UT_Error IE_Imp_AbiCollab::_openDocument(GsfInput * input, ServiceAccountHandler* pAccount,
					const std::string& email, const std::string& server, UT_sint64 doc_id, UT_sint64 revision)
{	
	UT_DEBUGMSG(("IE_Imp_AbiCollab::_openDocument()\n"));
	UT_return_val_if_fail(input, UT_ERROR);
	UT_return_val_if_fail(pAccount, UT_ERROR);
	
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, UT_ERROR);
	
	// NOTE: the document id is a valid session identifier, as abicollab.net 
	// has one ever lasting session per document really
	PD_Document* pDoc = getDoc();
	UT_return_val_if_fail(pDoc, UT_ERROR);
	UT_ASSERT_HARMLESS(XAP_App::getApp()->getLastFocussedFrame() != NULL);
	acs::SOAP_ERROR err = pAccount->openDocument(doc_id, revision,
										 boost::lexical_cast<std::string>(doc_id),
										  &pDoc, XAP_App::getApp()->getLastFocussedFrame());
	switch (err)
	{
		case acs::SOAP_ERROR_OK:
			return UT_OK;
		case acs::SOAP_ERROR_INVALID_PASSWORD:
			{
				// TODO: asking for user input is not really nice in an async function
				std::string password;
				if (ServiceAccountHandler::askPassword(email, password))
				{
					// try again with the new password
					pAccount->addProperty("password", password);
					pManager->storeProfile();
					return _openDocument(input, pAccount, email, server, doc_id, revision);	
				}
			}
			break;
		default:
			return UT_ERROR;
	}
	
	return UT_ERROR;
}

bool IE_Imp_AbiCollab::_parse(GsfInput * input, std::string& email, std::string& server, UT_sint64& doc_id, UT_sint64& revision)
{
	guint8 const* contents = gsf_input_read(input, gsf_input_size(input), NULL);
	UT_return_val_if_fail(contents, UT_ERROR);

	// FIXME: put this in a boost shared ptr
	xmlDocPtr reader = xmlReadMemory(reinterpret_cast<const char*>(contents), 
							strlen(reinterpret_cast<const char*>(contents)), 0, "UTF-8", 0);
	UT_return_val_if_fail(reader, UT_ERROR);
	boost::shared_ptr<xmlDocPtr> reader_ptr(&reader, XmlDocDeleter());
	
	xmlNode* root = xmlDocGetRootElement(*reader_ptr);
	UT_return_val_if_fail(root, false);
	UT_return_val_if_fail(strcmp(reinterpret_cast<const char*>(root->name), "abicollab") == 0, false);

	std::string doc_id_;
	std::string revision_;
	for (xmlNode* child = root->children; child; child = child->next)
	{
		if (child->type != XML_ELEMENT_NODE)
			continue;
		if (strcmp(reinterpret_cast<const char*>(child->name), "email") == 0)
			email = reinterpret_cast<const char*>(xmlNodeGetContent(child)); // FIXME: memory leak
		else if (strcmp(reinterpret_cast<const char*>(child->name), "server") == 0)
			server = reinterpret_cast<const char*>(xmlNodeGetContent(child)); // FIXME: memory leak
		else if (strcmp(reinterpret_cast<const char*>(child->name), "doc_id") == 0)
			doc_id_ = reinterpret_cast<const char*>(xmlNodeGetContent(child)); // FIXME: memory leak
		else if (strcmp(reinterpret_cast<const char*>(child->name), "revision") == 0)
			revision_ = reinterpret_cast<const char*>(xmlNodeGetContent(child)); // FIXME: memory leak
	}
	
	UT_return_val_if_fail(email != "" && server != "" && doc_id_ != "" && revision_ != "", false);
	
	try {
		doc_id = boost::lexical_cast<int64_t>(doc_id_);
	} catch (boost::bad_lexical_cast &) {
		UT_DEBUGMSG(("Error casting doc_id (%s) to an UT_sint64\n", doc_id_.c_str()));
		return false;
	}		

	try {
		revision = boost::lexical_cast<int64_t>(revision_);
	} catch (boost::bad_lexical_cast &) {
		UT_DEBUGMSG(("Error casting revision (%s) to an UT_sint64\n", revision_.c_str()));
		return false;
	}		
	
	return true;
}

ServiceAccountHandler* IE_Imp_AbiCollab::_getAccount(const std::string& email, const std::string& server)
{
	AbiCollabSessionManager* pManager = AbiCollabSessionManager::getManager();
	UT_return_val_if_fail(pManager, NULL);	
	
	// check if we already have an account handler for this server;
	// if not, we'll create it
	ServiceAccountHandler* pExistingServiceAccount = NULL;
	const std::vector<AccountHandler *> accounts = pManager->getAccounts();
	for (UT_uint32 i = 0; i < accounts.size(); i++)
	{
		AccountHandler* pAccount = accounts[i];
		UT_continue_if_fail(pAccount);
		// FIXME: don't hardcode this storage type; make it a static class variable
		if (pAccount->getStorageType() == "com.abisource.abiword.abicollab.backend.service")
		{
			ServiceAccountHandler* pServiceAccount = static_cast<ServiceAccountHandler*>(pAccount);
			// TODO: check trailing '/' for the server name
			if (pServiceAccount->getProperty("uri") == server &&
				pServiceAccount->getProperty("email") == email)
			{
				UT_DEBUGMSG(("Found an existing account for server %s, email: %s!\n", server.c_str(), email.c_str()));
				pExistingServiceAccount = pServiceAccount;
				break;
			}
		}
	}
	
	if (!pExistingServiceAccount)
	{
		UT_DEBUGMSG(("No existing account exists for server %s, email: %s\n", server.c_str(), email.c_str()));
		
		std::string password;
		if (!ServiceAccountHandler::askPassword(email, password))
			return NULL;

		// create the new account
		pExistingServiceAccount = static_cast<ServiceAccountHandler*>(ServiceAccountHandlerConstructor());
		pExistingServiceAccount->addProperty("email", email);
		pExistingServiceAccount->addProperty("password", password);
		pExistingServiceAccount->addProperty("uri", server);
		pExistingServiceAccount->addProperty("autoconnect", "true");
		bool success = pManager->addAccount(pExistingServiceAccount);
		if (success)
			pManager->storeProfile();
	}

	if (!pExistingServiceAccount->isOnline())
		pExistingServiceAccount->connect();
	
	return pExistingServiceAccount;
}
