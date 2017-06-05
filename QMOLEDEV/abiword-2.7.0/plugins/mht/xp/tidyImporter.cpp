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

#include "tidyReader.h"
#include "tidyImporter.h"
#include "ie_impexp_HTML.h"

IE_Imp_Tidy_Sniffer::IE_Imp_Tidy_Sniffer ()
#ifdef XHTML_NAMED_CONSTRUCTORS
	: IE_ImpSniffer("AbiXHTML::HTML Tidy")
#endif
{
	// 
}

#ifdef XHTML_NAMED_CONSTRUCTORS

// supported mimetypes
static IE_MimeConfidence IE_Imp_Tidy_Sniffer__MimeConfidence[] = {
	{ IE_MIME_MATCH_FULL, 	IE_MIMETYPE_HTML, 	UT_CONFIDENCE_GOOD 	},
	{ IE_MIME_MATCH_BOGUS, 	"", 				UT_CONFIDENCE_ZILCH }
};

const IE_MimeConfidence * IE_Imp_Tidy_Sniffer::getMimeConfidence ()
{
	return IE_Imp_Tidy_Sniffer__MimeConfidence;
}

#endif /* XHTML_NAMED_CONSTRUCTORS */

UT_Confidence_t IE_Imp_Tidy_Sniffer::recognizeContents (const char * /*szBuf*/, UT_uint32 /*iNumbytes*/)
{
	return UT_CONFIDENCE_ZILCH;
}

UT_Confidence_t IE_Imp_Tidy_Sniffer::recognizeSuffix (const char * szSuffix)
{
	if (!(g_ascii_strcasecmp(szSuffix,".html")) || !(g_ascii_strcasecmp(szSuffix,".htm")))
		return UT_CONFIDENCE_GOOD;
	return UT_CONFIDENCE_ZILCH;
}

UT_Error IE_Imp_Tidy_Sniffer::constructImporter (PD_Document * pDocument, IE_Imp ** ppie)
{
	IE_Imp_XHTML * p = new IE_Imp_HTML(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Imp_Tidy_Sniffer::getDlgLabels (const char ** pszDesc,
										const char ** pszSuffixList,
										IEFileType * ft)
{
	*pszDesc = "HTML [via tidy] (.html, .htm)";
	*pszSuffixList = "*.html; *.htm";
	*ft = getFileType();
	return true;
}

IE_Imp_HTML::IE_Imp_HTML (PD_Document * pDocument) :
	IE_Imp_XHTML(pDocument)
{
	// 
}

IE_Imp_HTML::~IE_Imp_HTML ()
{
	// 
}

UT_Error IE_Imp_HTML::_loadFile (GsfInput * input)
{
	TidyReader wrapper;
	setReader (&wrapper);

	UT_Error e = IE_Imp_XHTML::_loadFile (input);

	return e;
}
