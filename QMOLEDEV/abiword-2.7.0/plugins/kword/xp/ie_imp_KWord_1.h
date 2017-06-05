/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 2001 AbiSource, Inc.
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


#ifndef IE_IMP_KWORD_1_H
#define IE_IMP_KWORD_1_H

#include "ut_string_class.h"
#include "ie_imp_XML.h"

class PD_Document;

// The importer/reader for KWord 1.0 files.

class IE_Imp_KWord_1_Sniffer : public IE_ImpSniffer
{
	friend class IE_Imp;

public:
	IE_Imp_KWord_1_Sniffer(const char * name);
	virtual ~IE_Imp_KWord_1_Sniffer() {}

	virtual const IE_SuffixConfidence * getSuffixConfidence ();
	virtual const IE_MimeConfidence * getMimeConfidence ();
	virtual UT_Confidence_t recognizeContents (const char * szBuf, 
									UT_uint32 iNumbytes);
	virtual bool getDlgLabels (const char ** szDesc,
							   const char ** szSuffixList,
							   IEFileType * ft);
	virtual UT_Error constructImporter (PD_Document * pDocument,
										IE_Imp ** ppie);

};

class IE_Imp_KWord_1 : public IE_Imp_XML
{
public:
  IE_Imp_KWord_1(PD_Document * pDocument);
  virtual ~IE_Imp_KWord_1();

  void startElement(const gchar *name, const gchar **atts);
  void endElement(const gchar *name);
  void charData(const gchar*, int len); 

private:

  void _appendText ();

  UT_UCS4String m_szTextBuffer;
  UT_String m_szCharProps;
  UT_String m_szSectProps;
  UT_String m_ParaProps;
  bool m_bInText;
};

#endif /* IE_IMP_KWORD_1_H */
