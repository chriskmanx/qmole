/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
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
#include "ie_imp_MIF.h"
#include "pd_Document.h"
#include "ut_growbuf.h"
#include "xap_EncodingManager.h"

/*
 * Import MIF documents
 */

/*****************************************************************/
/*****************************************************************/

IE_Imp_MIF_Sniffer::IE_Imp_MIF_Sniffer (const char * _name) :
  IE_ImpSniffer(_name)
{
  // 
}

// supported suffixes
static IE_SuffixConfidence IE_Imp_MIF_Sniffer__SuffixConfidence[] = {
	{ "mif", 	UT_CONFIDENCE_PERFECT 	},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_Imp_MIF_Sniffer::getSuffixConfidence ()
{
	return IE_Imp_MIF_Sniffer__SuffixConfidence;
}

UT_Confidence_t IE_Imp_MIF_Sniffer::recognizeContents(const char * /*szBuf*/, 
										   UT_uint32 /*iNumbytes*/)
{
  // TODO: try to sensibly recognize the contents of the buffer
  return(UT_CONFIDENCE_ZILCH);
}

UT_Error IE_Imp_MIF_Sniffer::constructImporter(PD_Document * pDocument,
											   IE_Imp ** ppie)
{
	IE_Imp_MIF * p = new IE_Imp_MIF(pDocument);
	*ppie = p;
	return UT_OK;
}

bool	IE_Imp_MIF_Sniffer::getDlgLabels(const char ** pszDesc,
										 const char ** pszSuffixList,
										 IEFileType * ft)
{
	*pszDesc = "MIF (.mif)";
	*pszSuffixList = "*.mif";
	*ft = getFileType();
	return true;
}

/*****************************************************************/
/*****************************************************************/

#define X_CleanupIfError(error,exp)	do { if (((error)=(exp)) != UT_OK) goto Cleanup; } while (0)

UT_Error IE_Imp_MIF::_loadFile(GsfInput * fp)
{
	UT_Error error;

	X_CleanupIfError(error,_writeHeader(fp));
	X_CleanupIfError(error,_parseFile(fp));

	error = UT_OK;

Cleanup:
	return error;
}

#undef X_CleanupIfError

/*****************************************************************/
/*****************************************************************/

IE_Imp_MIF::~IE_Imp_MIF()
{
}

IE_Imp_MIF::IE_Imp_MIF(PD_Document * pDocument)
	: IE_Imp(pDocument)
{
}

/*****************************************************************/
/*****************************************************************/

#define X_ReturnIfFail(exp,error)		do { bool b = (exp); if (!b) return (error); } while (0)
#define X_ReturnNoMemIfError(exp)	X_ReturnIfFail(exp,UT_IE_NOMEMORY)

UT_Error IE_Imp_MIF::_writeHeader(GsfInput * /* fp */)
{
	X_ReturnNoMemIfError(appendStrux(PTX_Section, NULL));

	return UT_OK;
}

UT_Error IE_Imp_MIF::_parseFile(GsfInput * /*fp*/)
{
	return UT_OK;
}

#undef X_ReturnNoMemIfError
#undef X_ReturnIfFail
