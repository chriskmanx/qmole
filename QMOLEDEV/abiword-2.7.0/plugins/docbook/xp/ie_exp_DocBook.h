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


#ifndef IE_EXP_DOCBOOK_H
#define IE_EXP_DOCBOOK_H

#include	"ie_exp.h"
#include	"pl_Listener.h"
#include	"pp_AttrProp.h"
#include	"ut_stack.h"
#include "ie_Table.h"

class PD_Document;
class s_DocBook_Listener;

// The exporter/writer for DocBook

class IE_Exp_DocBook_Sniffer : public IE_ExpSniffer
{
	friend class IE_Exp;

public:
	IE_Exp_DocBook_Sniffer (const char * name);
	virtual ~IE_Exp_DocBook_Sniffer () {}

	virtual bool recognizeSuffix (const char * szSuffix);
	virtual bool getDlgLabels (const char ** szDesc,
							   const char ** szSuffixList,
							   IEFileType * ft);
	virtual UT_Error constructExporter (PD_Document * pDocument,
										IE_Exp ** ppie);
};


class IE_Exp_DocBook : public IE_Exp
{
public:
	IE_Exp_DocBook(PD_Document * pDocument);
	virtual ~IE_Exp_DocBook();
	
		void iwrite (const char *);
		void writeln (const char *);
		int indent (void);
		int unindent (void);

protected:
	virtual UT_Error	_writeDocument(void);
		int s_align;

private:	
	s_DocBook_Listener *	m_pListener;
};


class s_DocBook_Listener : public PL_Listener
{
public:
	s_DocBook_Listener(PD_Document * pDocument,
						IE_Exp_DocBook * pie);
	virtual ~s_DocBook_Listener();

	virtual bool		populate(PL_StruxFmtHandle sfh,
								 const PX_ChangeRecord * pcr);

	virtual bool		populateStrux(PL_StruxDocHandle sdh,
									  const PX_ChangeRecord * pcr,
									  PL_StruxFmtHandle * psfh);

	virtual bool		change(PL_StruxFmtHandle sfh,
							   const PX_ChangeRecord * pcr);

	virtual bool		insertStrux(PL_StruxFmtHandle sfh,
									const PX_ChangeRecord * pcr,
									PL_StruxDocHandle sdh,
									PL_ListenerId lid,
									void (* pfnBindHandles)(PL_StruxDocHandle sdhNew,
															PL_ListenerId lid,
															PL_StruxFmtHandle sfhNew));

	virtual bool		signal(UT_uint32 iSignal);
	virtual bool		_initFile(void);
	virtual void		_closeFile(void);
protected:

	void				_tagClose(UT_uint32 tagID, const UT_UTF8String & content, bool newline=true, bool indent=true, bool decrease=true);
	void				_tagOpen(UT_uint32 tagID, const UT_UTF8String & content, bool newline=true, bool indent=true, bool increase=true);
	void				_tagOpenClose(const UT_UTF8String & content, bool suppress, bool newline=true, bool indent=true);
	UT_uint32			_tagTop(void);
	void				_closeSection(int sub);
	void				_closeSectionTitle(void);
	void				_closeParagraph(void);
	void				_closeSpan(void);
	void				_closeList(void);
	void				_closeChapter (void);
	void				_closeChapterTitle (void);
	void				_openParagraph(PT_AttrPropIndex api);
	void				_openSection(PT_AttrPropIndex api, int, const UT_UTF8String & content);
	void				_openSectionTitle(void);
	void				_openSpan(PT_AttrPropIndex api);
	void				_openChapter (PT_AttrPropIndex api);
	void				_openChapterTitle (PT_AttrPropIndex api);
	void				_openList(PT_AttrPropIndex api);
	void				_openBlock (bool indent);
	void				_openPlainBlock ();
	void				_handleDocument(void);
	void				_handleMetaData(void);
	void				_handleRevisions(void);
	void				_handleImage(PT_AttrPropIndex api);
	void				_handlePositionedImage(PT_AttrPropIndex api);
	void				_handleMath(PT_AttrPropIndex api);
	void				_handleEmbedded(PT_AttrPropIndex api);
	void				_handleField(const PX_ChangeRecord_Object * pcro, PT_AttrPropIndex api);
	void				_handleTOC(PT_AttrPropIndex api);
	void				_handleHyperlink(PT_AttrPropIndex api);
	void				_handleBookmark(PT_AttrPropIndex api);
	void				_handleHdrFtr(PT_AttrPropIndex api);
	void				_handleFootnote(PT_AttrPropIndex api);
	void				_openTable(PT_AttrPropIndex api);
	void				_openNestedTable();
	void				_openCell();
	void				_openRow(void);
	void				_closeNestedTable(void);
	void				_closeTable(void);
	void				_closeCell(void);
	void				_closeRow(void);
	bool				_decideIndent(void);
	bool				_inFormattedSpan(void);
	bool				_inSectionStrux(void);

	void				_outputData(const UT_UCSChar * p, UT_uint32 length);
	void				_handleDataItems(void);
	void				_convertFontSize(char* szDest, const char* pszFontSize);
	void				_convertColor(char* szDest, const char* pszColor);

	const UT_UTF8String	_getProps(PT_AttrPropIndex api);

	PD_Document *		        m_pDocument;
	IE_Exp_DocBook *		m_pie;
private:

	bool				m_bInParagraph;
	bool				m_bInSection;
	bool				m_bInSpan;
	bool				m_bInChapter;
	bool				m_bInTable;
	bool				m_bInTitle;
	bool				m_bInFrame;
	bool				m_bInHdrFtr;
	bool				m_bInNote;
	int					m_iNestedTable;
	int					m_iTableDepth;
	int					m_iListDepth;
	int					m_iPreviousListDepth;
	int					m_iSectionDepth;
	int					m_iLastClosed;
	UT_UTF8String		m_sLastStyle;
	UT_UTF8String		m_sParentStyle;
	const PP_AttrProp*	        m_pAP_Span;

	// Need to look up proper type, and place to stick #defines...

	UT_uint16		m_iBlockType;	// BT_*
	bool                 m_bWasSpace;
	bool				m_bExternal;

	UT_Stack			m_utsListStack;
	UT_NumberStack		m_utnsTagStack;

	UT_Vector		m_utvDataIDs;	// list of data ids for image enumeration
	ie_Table mTableHelper;
};

#endif /* IE_EXP_DOCBOOK_H */
