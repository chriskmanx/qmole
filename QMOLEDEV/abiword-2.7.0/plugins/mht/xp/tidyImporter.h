/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord: tidyImporter - plugin for Multipart [X]HTML
 * 
 * Copyright (C) 2002-2003 Francis James Franklin <fjf@alinameridon.com>
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

#include "ie_imp_XHTML.h"

class IE_Imp_Tidy_Sniffer : public IE_ImpSniffer
{
	friend class IE_Imp;

public:
	IE_Imp_Tidy_Sniffer();

	virtual ~IE_Imp_Tidy_Sniffer() {}

	virtual const IE_MimeConfidence * getMimeConfidence ();

	virtual UT_Confidence_t recognizeContents (const char * szBuf, UT_uint32 iNumbytes);
	virtual UT_Confidence_t recognizeSuffix (const char * szSuffix);

	virtual bool getDlgLabels (const char ** szDesc,
							   const char ** szSuffixList, IEFileType * ft);

	virtual UT_Error constructImporter (PD_Document * pDocument, IE_Imp ** ppie);
};

class IE_Imp_HTML : public IE_Imp_XHTML
{
public:
	IE_Imp_HTML (PD_Document * pDocument);

	virtual ~IE_Imp_HTML ();

protected:

	virtual UT_Error _loadFile (GsfInput * input);
};
