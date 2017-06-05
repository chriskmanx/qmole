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

#include <stdio.h>
#include <stdlib.h>
#include "ut_types.h"
#include "ut_assert.h"
#include "ut_debugmsg.h"
#include "ut_string.h"
#include "ut_iconv.h"
#include "ie_imp_MathML.h"
#include "pd_Document.h"

#include "xap_EncodingManager.h"


#include "ap_Dialog_Id.h"
#include "xap_DialogFactory.h"
#include "xap_Dlg_Encoding.h"
#include "ap_Prefs.h"
#include "ie_imp_Text.h"

/*****************************************************************/
/*****************************************************************/

static const AbiMathViewEntityMapItem mathmlEntitiesMap[] = {
#include "entitiesMap.inc"
};

static int compareEntities(const void * pSz, const void * pEnt)
{
	AbiMathViewEntityMapItem ** pE = NULL;

	pE  = reinterpret_cast<AbiMathViewEntityMapItem **>(reinterpret_cast<void **>(const_cast<void *>(pEnt)));

	const char * sz1 = reinterpret_cast<const char *>(pSz);
	const char * sz2 = (*pE)->szEntity;

	return strcmp(sz1, sz2);
}

static int sortEntities(const void * pEnt1, const void * pEnt2)
{
	AbiMathViewEntityMapItem ** pE1 = NULL;
	AbiMathViewEntityMapItem ** pE2 = NULL;

	pE1 = reinterpret_cast<AbiMathViewEntityMapItem **>(reinterpret_cast<void **>(const_cast<void *>(pEnt1)));
	pE2 = reinterpret_cast<AbiMathViewEntityMapItem **>(reinterpret_cast<void **>(const_cast<void *>(pEnt2)));

	const char * sz1 = (*pE1)->szEntity;
	const char * sz2 = (*pE2)->szEntity;

	return strcmp(sz1, sz2);
}

IE_Imp_MathML_EntityTable::IE_Imp_MathML_EntityTable ()
{
	UT_sint32 MapSize = sizeof(mathmlEntitiesMap) / sizeof(AbiMathViewEntityMapItem);

	for (UT_sint32 i = 0; i < MapSize - 1; i++)
	{
		AbiMathViewEntityMapItem * pEnt = const_cast<AbiMathViewEntityMapItem * >(&mathmlEntitiesMap[i]);
		m_vecEntityMap.addItem(pEnt);
	}
	m_vecEntityMap.qsort(sortEntities);
}

IE_Imp_MathML_EntityTable::~IE_Imp_MathML_EntityTable ()
{
	// 
}

/* caller's responsibility to free returned bytebuf
 */
bool IE_Imp_MathML_EntityTable::convert(const char * buffer, unsigned long length, UT_ByteBuf & To) const
{
	if (!buffer || !length)
	{
		UT_ASSERT(buffer && length);
		return false;
	}

	const char * ptr1 = buffer;
	const char * end  = buffer + length;

	bool bOkay = false;

	while (*ptr1 && (end - ptr1 > 6))
	{
		if (*ptr1 == '<')
		{
			if (strncmp(ptr1, "<math", 5) == 0)
			{
				bOkay = true;
				ptr1 += 5;
				break;
			}
		}
		++ptr1;
	}
	if (!bOkay)
	{
		return false;
	}

	const char * start = buffer;

	while (end - ptr1 > 7)
	{
		if (*ptr1 == 0)
		{
			break;
		}
		if (*ptr1 != '&')
		{
			++ptr1;
			continue;
		}
		if (ptr1 != start)
		{
			To.append(reinterpret_cast<const UT_Byte *>(start), ptr1 - start);
		}

		bool bEntity = true;

		const char * ptr2  = ptr1 + 1;

		while (end - ptr2 > 7)
		{
			if (*ptr2 == 0)
			{
				bEntity = false;
				break;
			}
			if (*ptr2 == ';')
			{
				break;
			}
			switch (*ptr2)
			{
			case '<': case '>': case '&': case '\'': case '"': case ' ': // non-exhaustive list of bad characters for entity; quick check only
				bEntity = false;
			default:
				break;
			}
			if (!bEntity)
			{
				break;
			}
			++ptr2;
		}
		if (bEntity)
		{
			if (*(ptr1 + 1) == '#') // unicode value; just leave alone...
			{
				++ptr2;
				To.append(reinterpret_cast<const UT_Byte *>(ptr1), ptr2 - ptr1);
				ptr1 = ptr2;
			}
			else
			{
				/* Make a copy of the string, excluding the & and ;
				 */
				UT_sint32 isize = static_cast<UT_sint32>(ptr2 - (ptr1 + 1));

				char * pszNew = new char[isize+1];

				for (UT_sint32 i = 0; i < isize; i++)
				{
					pszNew[i] = ptr1[1+i];
				}
				pszNew[isize] = 0;

				UT_sint32 pos = m_vecEntityMap.binarysearch(reinterpret_cast<void *>(pszNew), compareEntities);

				if (pos >= 0)
				{
					AbiMathViewEntityMapItem * pE = m_vecEntityMap.getNthItem(pos);

					const char * szEntVal = pE->szVal;

					UT_DEBUGMSG(("Replacing entity \"&%s;\" with \"%s\".\n", pszNew, szEntVal));

					To.append(reinterpret_cast<const UT_Byte *>(szEntVal), strlen(szEntVal));
					ptr1 = ptr2 + 1;
				}
				else
				{
					UT_DEBUGMSG(("Entity \"&%s;\" unmatched; leaving alone...\n", pszNew));

					++ptr2;
					To.append(reinterpret_cast<const UT_Byte *>(ptr1), ptr2 - ptr1);
					ptr1 = ptr2;
				}
				DELETEPV(pszNew);
			}
		}
		else // huh?
		{
			UT_DEBUGMSG(("Found unmatched \"&\" - replacing with \"&amp;\"!\n"));

			const char * amp = "&amp;";
			To.append(reinterpret_cast<const UT_Byte *>(amp), 5);
			++ptr1;
		}
		start = ptr1;
	}
	To.append(reinterpret_cast<const UT_Byte *>(start), end - start);

	return true; 
}

/*****************************************************************/
/*****************************************************************/

IE_Imp_MathML_Sniffer::IE_Imp_MathML_Sniffer (const IE_Imp_MathML_EntityTable & EntityTable) :
	IE_ImpSniffer(IE_IMPEXPNAME_MATHML, true),
	m_EntityTable(&EntityTable)
{
	// 
}

// supported suffixes
static IE_SuffixConfidence IE_Imp_MathML_Sniffer__SuffixConfidence[] = {
	{ "mathml",	UT_CONFIDENCE_PERFECT 	},
	{ "xml", 	UT_CONFIDENCE_GOOD 		},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_Imp_MathML_Sniffer::getSuffixConfidence ()
{
	return IE_Imp_MathML_Sniffer__SuffixConfidence;
}

// supported mimetypes
static IE_MimeConfidence IE_Imp_MathML_Sniffer__MimeConfidence[] = {
	{ IE_MIME_MATCH_FULL, 	"application/mathml+xml", 	UT_CONFIDENCE_GOOD 	},
	{ IE_MIME_MATCH_FULL, 	"application/mathml", 		UT_CONFIDENCE_GOOD 	},	
	{ IE_MIME_MATCH_CLASS, 	"text", 					UT_CONFIDENCE_SOSO 	},
	{ IE_MIME_MATCH_BOGUS,  "", 						UT_CONFIDENCE_ZILCH }
};

const IE_MimeConfidence * IE_Imp_MathML_Sniffer::getMimeConfidence ()
{
	return IE_Imp_MathML_Sniffer__MimeConfidence;
}

/*!
  Check if buffer contains data meant for this importer.

 We don't attmpt to recognize since other filetypes (HTML) can
 use the same encodings a text file can.
 We also don't want to steal recognition when user wants to use
 the Encoded Text importer.
 */
UT_Confidence_t IE_Imp_MathML_Sniffer::recognizeContents(const char * szBuf,
													   UT_uint32 /*iNumbytes*/)
{
	const char * magic = "<math";
	if(strncmp(szBuf,magic,strlen(magic) == 0))
	   return UT_CONFIDENCE_PERFECT;
	return UT_CONFIDENCE_ZILCH;
 

}

UT_Error IE_Imp_MathML_Sniffer::constructImporter(PD_Document * pDocument,
												IE_Imp ** ppie)
{
	IE_Imp_MathML * p = new IE_Imp_MathML(pDocument, *m_EntityTable);
	*ppie = p;
	return UT_OK;
}

bool IE_Imp_MathML_Sniffer::getDlgLabels(const char ** pszDesc,
									   const char ** pszSuffixList,
									   IEFileType * ft)
{
	*pszDesc = "MathML (.xml, .mathml)";
	*pszSuffixList = "*.xml; *.mathml";
	*ft = getFileType();
	return true;
}

/*****************************************************************/
/*****************************************************************/

#define X_CleanupIfError(error,exp)	do { if (((error)=(exp)) != UT_OK) goto Cleanup; } while (0)

/*
  Import MathML data from a plain text file
 \param szFilename Name of file to import
 Simply fills a UT_byteBuf with the contents of the MathML
*/
UT_Error IE_Imp_MathML::_loadFile(GsfInput * fp)
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
 \param pDocument Document to import MathML into

 Uses current document's encoding if it is set
*/
IE_Imp_MathML::IE_Imp_MathML(PD_Document * pDocument, const IE_Imp_MathML_EntityTable & EntityTable) :
	IE_Imp(pDocument),
	m_pByteBuf(new UT_ByteBuf),
	m_EntityTable(&EntityTable)
{
	// 
}

IE_Imp_MathML::~IE_Imp_MathML(void)
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
UT_Error IE_Imp_MathML::_parseStream(ImportStream * pStream)
{
	UT_return_val_if_fail(pStream, UT_ERROR);

	UT_ByteBuf BB;
	UT_UCSChar c;
	unsigned char uc;
	while (pStream->getChar(c))
	{
		uc = static_cast<unsigned char>(c);
		BB.append(&uc,1);
	}
	return m_EntityTable->convert(reinterpret_cast<const char *>(BB.getPointer(0)), BB.getLength(), *m_pByteBuf) ? UT_OK : UT_ERROR;
}

bool IE_Imp_MathML::pasteFromBuffer(PD_DocumentRange * pDocRange,
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

