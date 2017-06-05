/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
 * Copyright (C) 2003 Tomas Frydrych <tomas@frydrych.uklinux.net> 
 * Copyright (C) 2004 Martin Sevior <msevior@physics.unimelb.edu.au> 
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

#include "ut_types.h"
#include "ut_assert.h"
#include "ut_debugmsg.h"
#include "ut_string.h"
#include "ut_iconv.h"
#include "ie_imp_GOComponent.h"
#include "pd_Document.h"

#include "xap_EncodingManager.h"


#include "ap_Dialog_Id.h"
#include "xap_DialogFactory.h"
#include "xap_Dlg_Encoding.h"
#include "ap_Prefs.h"
#include "ie_imp_Text.h"
#include "xap_Frame.h"
#include "xap_UnixFrameImpl.h"
#include "fv_View.h"
#include <goffice/component/go-component-factory.h>
#include <goffice/utils/go-file.h>

/*****************************************************************/
/*****************************************************************/

GSList *mime_types;

// supported mimetypes
static IE_MimeConfidence *IE_Imp_Component_Sniffer__MimeConfidence = NULL;

IE_Imp_Component_Sniffer::IE_Imp_Component_Sniffer ()
	: IE_ImpSniffer(IE_IMPEXPNAME_GOCOMPONENT, true)
{
	// 
}

IE_Imp_Component_Sniffer::~IE_Imp_Component_Sniffer ()
{
	if (IE_Imp_Component_Sniffer__MimeConfidence != NULL)
		delete[] IE_Imp_Component_Sniffer__MimeConfidence;
}

static UT_Confidence_t 
supports_mime (const char * szMIME)
{
#warning should return PERFECT only when priority is high
	if (g_slist_find_custom (mime_types, szMIME, (GCompareFunc) strcmp) != NULL)
	{
		 switch (go_components_get_priority (szMIME))
		 {
			 case GO_MIME_PRIORITY_DISPLAY:
				 return UT_CONFIDENCE_POOR;
			 case GO_MIME_PRIORITY_PRINT:
			 case GO_MIME_PRIORITY_PARTIAL:
				 return UT_CONFIDENCE_SOSO;
			 case GO_MIME_PRIORITY_FULL:
				 return UT_CONFIDENCE_GOOD;
			 case GO_MIME_PRIORITY_NATIVE:
				return UT_CONFIDENCE_PERFECT;
			 default:
				return UT_CONFIDENCE_ZILCH;
		 }
	}
	return UT_CONFIDENCE_ZILCH;
}

const IE_SuffixConfidence * IE_Imp_Component_Sniffer::getSuffixConfidence ()
{
	return NULL;
}

const IE_MimeConfidence * IE_Imp_Component_Sniffer::getMimeConfidence ()
{
	if (IE_Imp_Component_Sniffer__MimeConfidence == NULL)
	{
		IE_Imp_Component_Sniffer__MimeConfidence = new IE_MimeConfidence[g_slist_length(mime_types) + 1];
		int i;
		GSList *l;
		for (l = mime_types, i = 0; l != NULL; l= l->next, i++)
		{
			IE_Imp_Component_Sniffer__MimeConfidence[i].match = IE_MIME_MATCH_FULL;
			IE_Imp_Component_Sniffer__MimeConfidence[i].mimetype = static_cast<const char*>(l->data);
			IE_Imp_Component_Sniffer__MimeConfidence[i].confidence = supports_mime(static_cast<const gchar*>(l->data));
		}
		IE_Imp_Component_Sniffer__MimeConfidence[i].match = IE_MIME_MATCH_BOGUS;
		IE_Imp_Component_Sniffer__MimeConfidence[i].confidence = UT_CONFIDENCE_ZILCH;
	}
	return IE_Imp_Component_Sniffer__MimeConfidence; // IE_Imp_Component_Sniffer__MimeConfidence;
}

/*!
  Check if buffer contains data meant for this importer.
 */
UT_Confidence_t IE_Imp_Component_Sniffer::recognizeContents(const char * szBuf,
													   UT_uint32 iNumbytes)
{
	char const *mime = go_get_mime_type_for_data (szBuf, iNumbytes);
	if (mime != NULL)
	{
	   UT_Confidence_t confidence = supports_mime (mime);
	   FREEP(mime);
	   return confidence;
	}
	return UT_CONFIDENCE_ZILCH;
}

UT_Error IE_Imp_Component_Sniffer::constructImporter(PD_Document * pDocument,
												IE_Imp ** ppie)
{
	IE_Imp_Component * p = new IE_Imp_Component(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Imp_Component_Sniffer::getDlgLabels(G_GNUC_UNUSED const char ** pszDesc,
									   G_GNUC_UNUSED const char ** pszSuffixList,
									   G_GNUC_UNUSED IEFileType * ft)
{
	return false;
}

/*****************************************************************/
/*****************************************************************/

#define X_CleanupIfError(error,exp)	do { if (((error)=(exp)) != UT_OK) goto Cleanup; } while (0)

/*
  Import Component data from a plain text file
 \param szFilename Name of file to import
 Simply fills a UT_byteBuf with the contents of the Component
*/
UT_Error IE_Imp_Component::_loadFile(GsfInput * fp)
{
	ImportStreamFile * pStream = new ImportStreamFile(fp);
	UT_Error error;


	pStream->init(NULL);
	X_CleanupIfError(error,_parseStream(pStream));
	error = UT_OK;


Cleanup:
	delete pStream;
	return error;
}

#undef X_CleanupIfError

/*****************************************************************/
/*****************************************************************/

/*
  Construct text importer
 \param pDocument Document to import Component into

 Uses current document's encoding if it is set
*/
IE_Imp_Component::IE_Imp_Component(PD_Document * pDocument)
	: IE_Imp(pDocument),m_pByteBuf(NULL)
{
	m_pByteBuf = new UT_ByteBuf;

}

IE_Imp_Component::~IE_Imp_Component(void)
{
	DELETEP(m_pByteBuf);
}

/*****************************************************************/

#define X_ReturnIfFail(exp,error)		do { bool b = (exp); if (!b) return (error); } while (0)
#define X_ReturnNoMemIfError(exp)	X_ReturnIfFail(exp,UT_IE_NOMEMORY)


/*!
  Parse stream contents into the document
 \param stream Stream to import from

 This code is used for both files and the clipboard
 */
UT_Error IE_Imp_Component::_parseStream(ImportStream * pStream)
{
	UT_return_val_if_fail(pStream, UT_ERROR);
	XAP_Frame *pFrame = XAP_App::getApp()->getLastFocussedFrame();
    FV_View* pView = static_cast<FV_View*>(pFrame->getCurrentView());

	UT_UCSChar c;
	unsigned char uc;
	while (pStream->getChar(c))
	{
		uc = static_cast<unsigned char>(c);
		m_pByteBuf->append(&uc,1);
	}
	const char *mime_type = go_get_mime_type_for_data(m_pByteBuf->getPointer(0), m_pByteBuf->getLength());
 	if (g_slist_find_custom (mime_types, mime_type,
				(gint (*)(const void*, const void*))strcmp) == NULL) {
		return UT_IE_UNSUPTYPE;
	}
	UT_String Props=UT_String ("embed-type: GOComponent//") + mime_type;
	PT_DocPosition pos = pView->getPoint();
	pView->cmdInsertEmbed(m_pByteBuf,pos,mime_type,Props.c_str());
	pView->cmdSelect(pos,pos+1);

	return UT_OK;
}

bool IE_Imp_Component::pasteFromBuffer(PD_DocumentRange * pDocRange,
								  const unsigned char * pData, UT_uint32 lenData,
								  const char * /* encoding */)
{
	UT_return_val_if_fail(getDoc() == pDocRange->m_pDoc,false);
	UT_return_val_if_fail(pDocRange->m_pos1 == pDocRange->m_pos2,false);

	ImportStreamClipboard stream(pData, lenData);
	setClipboard (pDocRange->m_pos1);
	stream.init(NULL);
	UT_Error error = _parseStream(&stream);
	return error == UT_OK;
}
