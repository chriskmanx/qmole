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


#ifndef IE_IMP_DOCBOOK_H
#define IE_IMP_DOCBOOK_H

#include 	"ie_imp_XML.h"
#include	"ie_Table.h"
#include	"fl_AutoNum.h"

class PD_Document;

class IE_Imp_DocBook_Sniffer : public IE_ImpSniffer
{
	friend class IE_Imp;

public:
	IE_Imp_DocBook_Sniffer(const char * name);
	virtual ~IE_Imp_DocBook_Sniffer() {}

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

// The importer/reader for DocBook files.

class IE_Imp_DocBook : public IE_Imp_XML
{
public:
	IE_Imp_DocBook(PD_Document * pDocument);
	virtual ~IE_Imp_DocBook();

	static bool		RecognizeContents(const char * szBuf, 
						  UT_uint32 iNumbytes);
	static bool		RecognizeSuffix(const char * szSuffix);
	static UT_Error		StaticConstructor(PD_Document * pDocument,
						  IE_Imp ** ppie);
	static bool		GetDlgLabels(const char ** pszDesc,
					     const char ** pszSuffixList,
					     IEFileType * ft);
	static bool 		SupportsFileType(IEFileType ft);
	

	void			startElement(const gchar *name, 
					      const gchar **atts);
	void			endElement(const gchar *name);
	
	void charData(const gchar *s, int len);

protected:
	int m_iCurListID;
	int m_iBlockDepth;
	int m_iDataDepth;
	int m_iListDepth;
	int m_iFootnotes;
	int m_iImages;
	int m_iSectionDepth;
	int m_iTitleDepth;
	UT_sint32 m_iNoteID;
	UT_GenericVector<fl_AutoNum *> m_utvTitles;
	bool m_bMustAddTitle;
	bool m_bRequiredBlock;
	bool m_bTitleAdded;
	bool m_bMustNumber;
	bool m_bWroteBold;
	bool m_bWroteEntryPara;
	bool m_bInFrame;
	bool m_bInIndex;
	bool m_bInMath;
	bool m_bInMeta;
	bool m_bInNote;
	bool m_bInTable;
	bool m_bInTOC;
	bool m_bReadBook;
	UT_NumberStack m_utnsTagStack;
	UT_UTF8String m_sectionRole;
	ie_Table * m_TableHelperStack;

	void createList (void);
	void createTitle (void);
	void createImage (const char *name, const gchar **atts);
	void requireBlock(void);
	const gchar ** getCondition (const gchar **atts);
	UT_uint32 tagTop(void);
};

#endif /* IE_IMP_DocBook_H */
