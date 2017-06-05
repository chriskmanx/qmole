/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
 * Copyright (C) 2000 Hubert Figuière
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

#ifndef IE_IMP_CLARISWORKS_H
#define IE_IMP_CLARISWORKS_H

#include <stdio.h>
#include "ie_imp.h"
class PD_Document;


// The importer/reader for ClarisWorks/AppleWorks Files

class IE_Imp_ClarisWorks_Sniffer : public IE_ImpSniffer
{
	friend class IE_Imp;

public:
	IE_Imp_ClarisWorks_Sniffer();
	virtual ~IE_Imp_ClarisWorks_Sniffer() {}

	virtual const IE_SuffixConfidence * getSuffixConfidence ();
	virtual UT_Confidence_t recognizeContents (const char * szBuf, 
									UT_uint32 iNumbytes);
	virtual const IE_MimeConfidence * getMimeConfidence () { return NULL; }
	virtual bool getDlgLabels (const char ** szDesc,
							   const char ** szSuffixList,
							   IEFileType * ft);
	virtual UT_Error constructImporter (PD_Document * pDocument,
										IE_Imp ** ppie);

};

class IE_Imp_ClarisWorks : public IE_Imp
{
 public:
   IE_Imp_ClarisWorks(PD_Document * pDocument);
   ~IE_Imp_ClarisWorks();
  
 protected:
	virtual UT_Error _loadFile(GsfInput * fp);
   UT_Error			_parseFile(GsfInput * fp);
   UT_Error			_writeHeader(GsfInput * fp);
 private:

};

#endif /* IE_IMP_CLARISWORKS_H */
