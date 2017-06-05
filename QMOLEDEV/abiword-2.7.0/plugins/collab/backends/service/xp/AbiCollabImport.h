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

#ifndef __ABICOLLAB_IMPORT__
#define __ABICOLLAB_IMPORT__

#include "ie_imp.h"
#include "ut_types.h"

class IE_Imp_AbiCollabSniffer : public IE_ImpSniffer
{
public:
	IE_Imp_AbiCollabSniffer();
	virtual ~IE_Imp_AbiCollabSniffer();

	virtual const IE_SuffixConfidence * getSuffixConfidence ();
	virtual UT_Confidence_t recognizeContents(const char * szBuf, UT_uint32 iNumbytes);
	virtual const IE_MimeConfidence * getMimeConfidence () { return NULL; }
	virtual bool getDlgLabels (const char ** pszDesc, const char ** pszSuffixList, IEFileType * ft);
	virtual UT_Error constructImporter (PD_Document * pDocument, IE_Imp ** ppie);	
};

class IE_Imp_AbiCollab : public IE_Imp
{
public:
	IE_Imp_AbiCollab(PD_Document* pDocument);

protected:
	virtual UT_Error _loadFile(GsfInput * input);

private:
	UT_Error				_openDocument(GsfInput * input, ServiceAccountHandler* pAccount,
									const std::string& email, const std::string& server, UT_sint64 doc_id, UT_sint64 revision);
	bool					_parse(GsfInput * input, std::string& email, std::string& server, UT_sint64& doc_id, UT_sint64& revision);
	ServiceAccountHandler*  _getAccount(const std::string& email, const std::string& server);
};

#endif /* __ABICOLLAB_IMPORT__ */
