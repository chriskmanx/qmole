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
#include "ie_imp_GOChart.h"
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

/*****************************************************************/
/*****************************************************************/

IE_Imp_Object_Sniffer::IE_Imp_Object_Sniffer ()
	: IE_ImpSniffer(IE_IMPEXPNAME_GOCHART, true)
{
	// 
}

// supported suffixes
static IE_SuffixConfidence IE_Imp_Object_Sniffer__SuffixConfidence[] = {
	{ "xml", 	UT_CONFIDENCE_GOOD 		},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_Imp_Object_Sniffer::getSuffixConfidence ()
{
	return IE_Imp_Object_Sniffer__SuffixConfidence;
}

// supported mimetypes
static IE_MimeConfidence IE_Imp_Object_Sniffer__MimeConfidence[] = {
	{ IE_MIME_MATCH_FULL, 	"application/x-goffice-graph", 	UT_CONFIDENCE_PERFECT	},
	{ IE_MIME_MATCH_FULL, 	"application/xml", 				UT_CONFIDENCE_GOOD 		},	
	{ IE_MIME_MATCH_BOGUS, 	"", 							UT_CONFIDENCE_ZILCH 	}
};

const IE_MimeConfidence * IE_Imp_Object_Sniffer::getMimeConfidence ()
{
	return IE_Imp_Object_Sniffer__MimeConfidence;
}

/*!
  Check if buffer contains data meant for this importer.

 We don't attmpt to recognize since other filetypes (HTML) can
 use the same encodings a text file can.
 We also don't want to steal recognition when user wants to use
 the Encoded Text importer.
 */
UT_Confidence_t IE_Imp_Object_Sniffer::recognizeContents(const char * szBuf,
													   G_GNUC_UNUSED UT_uint32 iNumbytes)
{
	const char * magic1 = "<?xml version=\"1.0\"";
	const char * magic2 = "<GogObject type=\"GogGraph\">";
	
	xxx_UT_DEBUGMSG(("magic1 = %s \n",magic1));
	xxx_UT_DEBUGMSG(("magic2 = %s \n",magic2));
	if((strstr(szBuf,magic1) != NULL) && (strstr(szBuf,magic2) != NULL))
	   return UT_CONFIDENCE_PERFECT;
	return UT_CONFIDENCE_ZILCH;
 

}

UT_Error IE_Imp_Object_Sniffer::constructImporter(PD_Document * pDocument,
												IE_Imp ** ppie)
{
	IE_Imp_Object * p = new IE_Imp_Object(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Imp_Object_Sniffer::getDlgLabels(G_GNUC_UNUSED const char ** pszDesc,
									   G_GNUC_UNUSED const char ** pszSuffixList,
									   G_GNUC_UNUSED IEFileType * ft)
{
/*	*pszDesc = "GNOME Office Chart (.xml)";
	*pszSuffixList = "*.xml";
	*ft = getFileType();	*/
	return false;
}

/*****************************************************************/
/*****************************************************************/

#define X_CleanupIfError(error,exp)	do { if (((error)=(exp)) != UT_OK) goto Cleanup; } while (0)

/*
  Import Object data from a plain text file
 \param szFilename Name of file to import
 Simply fills a UT_byteBuf with the contents of the Object
*/
UT_Error IE_Imp_Object::_loadFile(GsfInput * fp)
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
 \param pDocument Document to import Object into

 Uses current document's encoding if it is set
*/
IE_Imp_Object::IE_Imp_Object(PD_Document * pDocument)
	: IE_Imp(pDocument),m_pByteBuf(NULL)
{
	m_pByteBuf = new UT_ByteBuf;

}

IE_Imp_Object::~IE_Imp_Object(void)
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
UT_Error IE_Imp_Object::_parseStream(ImportStream * pStream)
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
	const char* mimetypeGOChart = "application/x-goffice-graph";
	const char * szProps="embed-type: GOChart";
	PT_DocPosition pos = pView->getPoint();
	pView->cmdInsertEmbed(m_pByteBuf,pView->getPoint(),mimetypeGOChart,szProps);
	pView->cmdSelect(pos,pos+1);

	return UT_OK;
}

bool IE_Imp_Object::pasteFromBuffer(PD_DocumentRange * pDocRange,
								  const unsigned char * pData, UT_uint32 lenData,
								  const char * /* encoding */)
{
	UT_return_val_if_fail(getDoc() == pDocRange->m_pDoc,false);
	UT_return_val_if_fail(pDocRange->m_pos1 == pDocRange->m_pos2,false);

	ImportStreamClipboard stream(pData, lenData);
	setClipboard (pDocRange->m_pos1);
	stream.init(NULL);
	_parseStream(&stream);
	return true;
}
