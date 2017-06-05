/* -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
 * Copyright (C) 2004 Marc Maurer (uwog@uwog.net)
 * Copyright (C) 2008 Xun Sun (xun.sun.cn@gmail.com)
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

#include <stdlib.h>
#include <string.h>
#include <deque>
#include <stack>

#ifdef HAVE_LIBXSLT
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>
#endif

#include "fp_types.h"
#include "ut_debugmsg.h"
#include "ut_string.h"
#include "ut_bytebuf.h"
#include "ut_base64.h"
#include "ut_Language.h"
#include "ut_units.h"
#include "ut_mbtowc.h"
#include "ut_wctomb.h"
#include "pt_Types.h"
#include "ie_exp_LaTeX.h"
#include "pd_Document.h"
#include "pd_Style.h"
#include "pp_AttrProp.h"
#include "px_ChangeRecord.h"
#include "px_CR_Object.h"
#include "px_CR_Span.h"
#include "px_CR_Strux.h"
#include "xap_App.h"
#include "xap_EncodingManager.h"
#include "fd_Field.h"
#include "ie_Table.h"
#include "ut_locale.h"
#include "ut_string_class.h"
#include "xap_Module.h"
#include "ut_misc.h"

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_latex_register
#define abi_plugin_unregister abipgn_latex_unregister
#define abi_plugin_supports_version abipgn_latex_supports_version
// dll exports break static linking
#define ABI_BUILTIN_FAR_CALL extern "C"
#else
#define ABI_BUILTIN_FAR_CALL ABI_FAR_CALL
ABI_PLUGIN_DECLARE("LaTeX")
#endif

/*****************************************************************/
/*****************************************************************/

// completely generic code to allow this to be a plugin

// we use a reference-counted sniffer
static IE_Exp_LaTeX_Sniffer * m_sniffer = 0;

ABI_BUILTIN_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{

	if (!m_sniffer)
	{
		m_sniffer = new IE_Exp_LaTeX_Sniffer ();
	}

	mi->name = "LaTeX Exporter";
	mi->desc = "Export LaTeX Documents";
	mi->version = ABI_VERSION_STRING;
	mi->author = "Abi the Ant";
	mi->usage = "No Usage";

	IE_Exp::registerExporter (m_sniffer);
	return 1;
}

ABI_BUILTIN_FAR_CALL
int abi_plugin_unregister (XAP_ModuleInfo * mi)
{
	mi->name = 0;
	mi->desc = 0;
	mi->version = 0;
	mi->author = 0;
	mi->usage = 0;

	UT_return_val_if_fail (m_sniffer, 0);

	IE_Exp::unregisterExporter (m_sniffer);
	delete m_sniffer;
	m_sniffer = 0;

	return 1;
}

ABI_BUILTIN_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, 
				    UT_uint32 /*release*/)
{
  return 1;
}

/*****************************************************************/
/*****************************************************************/

IE_Exp_LaTeX_Sniffer::IE_Exp_LaTeX_Sniffer () :
  IE_ExpSniffer("AbiLaTeX::LaTeX")
{
  // 
}

bool IE_Exp_LaTeX_Sniffer::recognizeSuffix(const char * szSuffix)
{
	return (!g_ascii_strcasecmp(szSuffix,".tex") || !g_ascii_strcasecmp(szSuffix, ".latex"));
}

UT_Error IE_Exp_LaTeX_Sniffer::constructExporter(PD_Document * pDocument,
						 IE_Exp ** ppie)
{
	IE_Exp_LaTeX * p = new IE_Exp_LaTeX(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Exp_LaTeX_Sniffer::getDlgLabels(const char ** pszDesc,
					const char ** pszSuffixList,
					IEFileType * ft)
{
	*pszDesc = "LaTeX (.latex)";
	*pszSuffixList = "*.tex; *.latex";
	*ft = getFileType();
	return true;
}

/*****************************************************************/
/*****************************************************************/

//#define DEFAULT_SIZE "12pt"
#define EPSILON 0.1

enum JustificationTypes {
	JUSTIFIED,
	CENTER,
	RIGHT,
	LEFT
};

IE_Exp_LaTeX::IE_Exp_LaTeX(PD_Document * pDocument)
	: IE_Exp(pDocument)
{
	m_error = 0;
	m_pListener = NULL;
}

IE_Exp_LaTeX::~IE_Exp_LaTeX()
{
}

/*****************************************************************/
/*****************************************************************/
typedef UT_UCSChar U16;
static int wvConvertUnicodeToLaTeX(U16 char16, const char*& out);
static bool _convertLettersToSymbols(char c, const char *& subst);

#define BT_NORMAL		1
#define BT_HEADING1		2
#define BT_HEADING2		3
#define BT_HEADING3		4
#define BT_BLOCKTEXT	5
#define BT_PLAINTEXT	6

class LaTeX_Analysis_Listener : public PL_Listener
{
private:
	ie_Table *  m_pTableHelper;
public:
	bool m_hasEndnotes;
	bool m_hasTable;
	bool m_hasMultiRow;

	LaTeX_Analysis_Listener(PD_Document * pDocument,
							IE_Exp_LaTeX * /*pie*/)
		: m_hasEndnotes(false),
		  m_hasTable(false),
		  m_hasMultiRow(false)
	{
	    m_pTableHelper = new ie_Table(pDocument);
	}

	virtual ~LaTeX_Analysis_Listener()
	{
	    DELETEP(m_pTableHelper);
	}

	virtual bool		populate(PL_StruxFmtHandle /*sfh*/,
					    const PX_ChangeRecord * /*pcr*/)
	{
		return true;	
	}

	virtual bool		populateStrux(PL_StruxDocHandle sdh,
						const PX_ChangeRecord * pcr,
						PL_StruxFmtHandle * psfh)
	{
		UT_ASSERT(pcr->getType() == PX_ChangeRecord::PXT_InsertStrux);
		const PX_ChangeRecord_Strux * pcrx = static_cast<const PX_ChangeRecord_Strux *> (pcr);
		*psfh = 0;							// we don't need it.

		switch (pcrx->getStruxType())
		{
		    case PTX_SectionEndnote:
		    case PTX_EndEndnote:
			m_hasEndnotes = true;
			break;
		    case PTX_SectionTable:
		    {
			m_pTableHelper->OpenTable(sdh, pcr->getIndexAP());
			m_hasTable = true;
			break;
		    }
		    case PTX_EndTable:
		    {
			m_pTableHelper->CloseTable();
			break;
		    }
		    case PTX_SectionCell:
			m_pTableHelper->OpenCell(pcr->getIndexAP());
			if(m_pTableHelper->getBot() - m_pTableHelper->getTop() >1)
			    this->m_hasMultiRow = true;
			break;
		    case PTX_EndCell:
			m_pTableHelper->CloseCell();
			break;
		    default:
			break;
		}

		return true;
	}

	virtual bool		change(PL_StruxFmtHandle /*sfh*/,
					const PX_ChangeRecord * /*pcr*/)
	{
		UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
		return false;
	}

	virtual bool		insertStrux(PL_StruxFmtHandle /*sfh*/,
					    const PX_ChangeRecord * /*pcr*/,
					    PL_StruxDocHandle /*sdh*/,
					    PL_ListenerId /*lid*/,
					    void (* /*pfnBindHandles*/)(PL_StruxDocHandle sdhNew,
									PL_ListenerId lid,
									PL_StruxFmtHandle sfhNew))
	{
		UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
		return false;
	}

	virtual bool		signal(UT_uint32 /*iSignal*/)
	{
		UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
		return false;
	}
};

class s_LaTeX_Listener : public PL_Listener
{
public:
	s_LaTeX_Listener(PD_Document * pDocument,
			    IE_Exp_LaTeX * pie,
			    const LaTeX_Analysis_Listener& analysis);
	virtual ~s_LaTeX_Listener();

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
#ifdef HAVE_LIBXSLT	
	static	bool		convertMathMLtoLaTeX(const UT_UTF8String & sMathML,
							UT_UTF8String & sLaTeX);
#endif
protected:
	void				_closeBlock(void);
	void				_closeCell(void);
	void				_closeParagraph(void);
	void				_closeSection(void);
	void				_closeSpan(void);
	void				_closeTable(void);
	void                _closeList(void);
	void                _closeLists(void);
	void				_openCell(PT_AttrPropIndex api);
	void				_openParagraph(PT_AttrPropIndex api);
	void				_openSection(PT_AttrPropIndex api);
	void				_openSpan(PT_AttrPropIndex api);
	void				_openTable(PT_AttrPropIndex api);
	void				_outputBabelPackage(void);
	void				_outputData(const UT_UCSChar * p, UT_uint32 length);
	void				_handleDataItems(void);
	void				_convertFontSize(UT_String& szDest, const char* pszFontSize);
	void				_convertColor(UT_String& szDest, const char* pszColor);
	void				_writeImage (const UT_ByteBuf * pByteBuf,
								   const UT_UTF8String & imagedir,
								   const UT_UTF8String & filename);
	void				_handleImage(const PP_AttrProp * pAP);
	
	PD_Document *		m_pDocument;
	IE_Exp_LaTeX *		m_pie;
	bool				m_bInBlock;
	bool				m_bInCell;
	bool				m_bInSection;
	bool				m_bInSpan;
	bool				m_bInList;
	bool				m_bInScript;
	bool				m_bInHeading;
	bool				m_bInFootnote;
	bool				m_bBetweenQuotes;
	const PP_AttrProp*	m_pAP_Span;
	bool                m_bMultiCols;
	bool				m_bInSymbol;
	bool				m_bInEndnote;
	bool				m_bHaveEndnote;
	bool				m_bOverline;
  
	JustificationTypes  m_eJustification;
	bool				m_bLineHeight;
	int 				ChapterNumber;
	
	/* default font size for the current document, in pt,
	 * as defined by the Normal style
	 */
	int 				m_DefaultFontSize;
	
	int                 m_Indent;
	int		    m_NumCloseBrackets; // accessed by _openSpan() and _closeSpan()
	int		    m_TableWidth;
	int		    m_CellLeft;
	int		    m_CellRight;
	int		    m_CellTop;
	int		    m_CellBot;
	
// Type for the last-processed list  
	FL_ListType			list_type;
	std::stack<FL_ListType>	list_stack;
	// Need to look up proper type, and place to stick #defines...

	UT_uint16		m_iBlockType;	// BT_*
	UT_Wctomb		m_wctomb;
	
	// Table utility members
	
	ie_Table *			m_pTableHelper;

	int		    m_RowNuminTable; // the current row being handled, starting from 1
	int		    m_ExpectedLeft; // expected left-attach value for the next cell,
					    // to deal with cells spanning multiple columns;
					    // starting from 0

	std::deque<UT_Rect*>	*m_pqRect; // pointer to a UT_Rect (de)queque. each UT_Rect
					   // instance in this queue has a 1:1 correspondence
					   // to a cell spanning multiple rows. The queue is
					   // examined when a table row ends, to output a \hline
					   // or several \cline as appropriate
	unsigned int	    m_index; // (dynamic, increase only) index into m_pqRect; it is safe
				     // to skip anything before m_index
#ifdef HAVE_LIBXSLT
	static xsltStylesheet *cur;
#endif
};

#ifdef HAVE_LIBXSLT
	xsltStylesheet * s_LaTeX_Listener::cur = NULL;
#endif

void s_LaTeX_Listener::_closeParagraph(void)
{
	if ((!m_bInCell) && (!m_bInFootnote) && (!m_bInEndnote)) m_pie->write("\n");
	m_bInHeading = false;
	return;
}
        
void s_LaTeX_Listener::_closeList(void)
{
	switch (list_type) {
		case NUMBERED_LIST:
			m_pie->write("\\end{enumerate}\n");
			break;
		case BULLETED_LIST:
			m_pie->write("\\end{itemize}\n");
			break;
		default:
			;
	}
	list_stack.pop();
	if (!list_stack.empty())
	{
		list_type = list_stack.top();
	}
}

void s_LaTeX_Listener::_closeLists()
{
	do{
		_closeList();
	} while(!list_stack.empty());
	m_bInList = false;
}

void s_LaTeX_Listener::_closeSection(void)
{
	_closeBlock();
	if (!m_bInSection)
	{
		return;
	}

	if (m_bInList)
	{
		this->_closeLists();
	}
 
	if (m_bMultiCols)
	{
		m_pie->write("\\end{multicols}\n");
		m_bMultiCols = false;
	}

	m_bInSection = false;
	return;
}

void s_LaTeX_Listener::_closeBlock(void)
{ 
	_closeSpan();
	if(m_bInFootnote || m_bInEndnote)
		return;
	if (!m_bInBlock)
		return;
	// if(m_bInCell) m_pie->write("Block end");

	switch (m_iBlockType)
	{
	case BT_NORMAL:
		if (m_bLineHeight)
		  m_pie->write("\n\\end{spacing}");

		switch (m_eJustification)
		{
		case JUSTIFIED:
			break;
		case CENTER:
			m_pie->write("\n\\end{center}");
			break;
		case RIGHT:
			m_pie->write("\n\\end{flushright}");
			break;
		case LEFT:
			m_pie->write("\n\\end{flushleft}");
			break;
		}

		if(!m_bInCell) m_pie->write("\n\n");
		break;
	case BT_HEADING1:
	case BT_HEADING2:
	case BT_HEADING3:
		m_pie->write("}\n");
		break;
	case BT_BLOCKTEXT:
		m_pie->write("\n\\end{quote}\n"); // It's not correct, but I'll leave it by now...
		break;
	case BT_PLAINTEXT:
		m_pie->write("}\n");
		break;
	default:
		m_pie->write("%% oh, oh\n");
	}

	m_bInBlock = false;
	return;
}

void s_LaTeX_Listener::_openCell(PT_AttrPropIndex api)
{
	this->m_pTableHelper->OpenCell(api);
	m_CellLeft = this->m_pTableHelper->getLeft();
	m_CellTop = this->m_pTableHelper->getTop();
	m_CellRight = this->m_pTableHelper->getRight();
	m_CellBot = this->m_pTableHelper->getBot();
	m_bInCell = true;
	
	if (this->m_pTableHelper->isNewRow())
	{
	    m_ExpectedLeft = 0;
	    if(m_CellTop != 0)
			m_pie->write("\\\\");
	    m_pie->write("\n");
	    if(!m_pqRect || m_pqRect->empty())
			m_pie->write("\\hline");
	    else
	    {
			UT_Rect* p;
			int left=1;
			while(m_index < m_pqRect->size())
			{
				p = m_pqRect->at(m_index);
				if(p->top + p->height -1 > m_RowNuminTable)
					break;
				m_index++;
			}
			for(unsigned int i=m_index; i< m_pqRect->size(); i++)
			{
				p = m_pqRect->at(i);
				if(m_RowNuminTable < p->top)
					break;
				if(left < p->left)
				{
					UT_String str;
					UT_String_sprintf(str, "\\cline{%d-%d}", left, p->left-1);
					m_pie->write(str);
				}
		    
				left = p->left + p->width;
				if(left > this->m_TableWidth)
					break;
			}
			xxx_UT_DEBUGMSG(("left = %d \n", left));
			if(left <= m_TableWidth)
			{
				if(1 == left)
					m_pie->write("\\hline");
				else
				{
					UT_String str;
					UT_String_sprintf(str, "\\cline{%d-%d}", left, m_TableWidth);
					m_pie->write(str);
				}
			}
	    }
	    m_pie->write("\n");
	    m_RowNuminTable = m_CellTop + 1;
	}
	if (m_CellLeft != 0)
	{
	    int i = m_CellLeft - m_ExpectedLeft;
	    for(; i>0; i--)
			m_pie->write("&");
	}
	if(m_CellRight - m_CellLeft >1)
	{
	    UT_String str;
	    UT_String_sprintf(str, "\\multicolumn{%d}{|l|}{", m_CellRight - m_CellLeft);
	    m_pie->write(str);
	}
	if(m_CellBot - m_CellTop >1)
	{
	    UT_String str;
	    UT_String_sprintf(str, "\\multirow{%d}{*}{", m_CellBot - m_CellTop);
	    m_pie->write(str);
	    if(m_pqRect)
	    {
		UT_Rect * p = new UT_Rect(m_CellLeft+1, m_CellTop+1, 
			m_CellRight - m_CellLeft, m_CellBot - m_CellTop);
		if(p)
		    m_pqRect->push_back(p);
	    }
	}
}

void s_LaTeX_Listener::_openParagraph(PT_AttrPropIndex api)
{
	m_bLineHeight = false;

	if (!m_bInSection)
	{
		return;
	}
	
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	m_iBlockType = BT_NORMAL;
	
	if (bHaveProp && pAP)
	{
		const gchar * szValue;

		if ((pAP->getAttribute(PT_LISTID_ATTRIBUTE_NAME, szValue))
			&& (pAP->getAttribute(PT_STYLE_ATTRIBUTE_NAME, szValue))
			&& (0 == strcmp(szValue, "Normal")))
		{
			int indent = 0;
			bool bNewList = false;
			const gchar * szIndent, * szLeft, * szListStyle = NULL;
			FL_ListType this_list_type = NOT_A_LIST;
			pAP->getProperty("list-style", szListStyle);
			
			if(szListStyle)
			{
			    if (0 == strcmp(szListStyle, "Numbered List") )
				this_list_type = NUMBERED_LIST;
			    else if (0 == strcmp(szListStyle, "Bullet List") )
				this_list_type = BULLETED_LIST;
			}
			
			if (this_list_type == NOT_A_LIST)
			{
			    this_list_type = list_type;
			}
			
			if (pAP->getProperty("text-indent", szIndent) && pAP->getProperty("margin-left", szLeft))
			{
				indent = UT_convertToDimension(szIndent, DIM_MM) + UT_convertToDimension(szLeft, DIM_MM);
				if (m_bInList)
				{
					xxx_UT_DEBUGMSG(("      indent = %d, m_Indent = %d\n", indent, m_Indent));
					if(indent > this->m_Indent) //nested list
						bNewList = true;
					else if (indent < this->m_Indent)
					{
						this->_closeList();
					}
					else
					/*
					 * now we have indent == this->m_Indent,
					 * but it is possible that the current list item is
					 * of different style with the last one.
					 */                                    
					{
						if (this_list_type != list_type)
						{
							this->_closeList();
							bNewList = true;
						}
					}
				}
			}
			
			if (bNewList || !m_bInList) 
				//necessary to build a new (possibly nested) list
			{
			    list_type = this_list_type;
			    if (list_type == NUMBERED_LIST)
			    {
					m_pie->write("\\begin{enumerate}\n");
			    }
			    else if (list_type == BULLETED_LIST)
			    {
					m_pie->write("\\begin{itemize}\n");
			    }
			    
			    list_stack.push(list_type);
			    m_bInList = true;
			}
			
			if (szIndent && szLeft)
				this->m_Indent = indent;
			
			m_pie->write("\\item ");
		} else if (m_bInList) {
			this->_closeLists();
		}

		if (pAP->getAttribute(PT_STYLE_ATTRIBUTE_NAME, szValue))
		{
			if (strstr(szValue, "Heading"))
				m_bInHeading = true;
			if(0 == strcmp(szValue, "Heading 1")) 
			{
				m_iBlockType = BT_HEADING1;
				m_pie->write("\\section*{");
			}
			else if(0 == strcmp(szValue, "Heading 2")) 
			{
				m_iBlockType = BT_HEADING2;
				m_pie->write("\\subsection*{");
			}
			else if(0 == strcmp(szValue, "Heading 3")) 
			{
				m_iBlockType = BT_HEADING3;
				m_pie->write("\\subsubsection*{");
			}
			else if(0 == strcmp(szValue, "Numbered Heading 1")) 
			{
				m_iBlockType = BT_HEADING1;
				m_pie->write("\\section{");
			}
			else if(0 == strcmp(szValue, "Numbered Heading 2")) 
			{
				m_iBlockType = BT_HEADING2;
				m_pie->write("\\subsection{");
			}
			else if(0 == strcmp(szValue, "Numbered Heading 3")) 
			{
				m_iBlockType = BT_HEADING3;
				m_pie->write("\\subsubsection{");
			}
			else if (0 == strcmp(szValue, "Chapter Heading")) {
				// TODO: Clean this...
				char			szChapterNumber[6];
				m_iBlockType = BT_HEADING1;
				sprintf(szChapterNumber, "%d", ChapterNumber++);
				m_pie->write ("\n\\newpage \\section*{\\LARGE\\chaptername\\ ");
				m_pie->write(szChapterNumber);
				m_pie->write(" ");	  // \\newline");
        	}
			else if(0 == strcmp(szValue, "Block Text"))
			{
				m_iBlockType = BT_BLOCKTEXT;
				m_pie->write("\\begin{quote}\n");
			}
			else if(0 == strcmp(szValue, "Plain Text"))
			{
				m_iBlockType = BT_PLAINTEXT;
				m_pie->write("\\texttt{");
			}
		}
		
		/* Assumption: never get property set with h1-h3, block text, plain text. Probably true. */
		
		/* In LaTeX, a footnote is enclosed within a parapgraph, so we need to
		 * preserve values of the previous block, in particular m_iBlockType and
		 * m_eJustification, as they affect the behavior of _closeBlock()
		 */
		if (m_iBlockType == BT_NORMAL && !m_bInFootnote)
		{
			m_eJustification = JUSTIFIED;
			if (pAP->getProperty("text-align", szValue))
			{
				if (0 == strcmp(szValue, "center"))
				{
					m_pie->write("\\begin{center}\n");
					m_eJustification = CENTER;
				}
				if (0 == strcmp(szValue, "right"))
				{
					m_pie->write("\\begin{flushright}\n");
					m_eJustification = RIGHT;
				}
				if (0 == strcmp(szValue, "left"))
				{
					m_pie->write("\\begin{flushleft}\n");
					m_eJustification = LEFT;
				}
			}

			if (pAP->getProperty("line-height", szValue))
			{
				double height = atof(szValue);

				if (height < 0.9 || height > 1.1)
				{
				    	char strH[8];
					
					/* Assume $baselineskip/fontsize \approx 1.2$, reasonable in most cases */
					snprintf(strH, 8, "%.2f", height / 1.2);
					strH[7] = '\0';
					
					m_pie->write("\\begin{spacing}{");
					xxx_UT_DEBUGMSG(("m_bLineHeight = true\n"));
					m_bLineHeight = true;
					
					m_pie->write(strH);
					m_pie->write("}\n");
				}
			}
		}
	}
	
	m_bInBlock = true;
}

void s_LaTeX_Listener::_openSection(PT_AttrPropIndex api)
{
	const PP_AttrProp* pAP = NULL;
	const gchar* pszNbCols = NULL;

	m_bBetweenQuotes = false;
	m_bInList = false;
	m_bInFootnote = false;
	m_bMultiCols = false;

	if (m_pDocument->getAttrProp(api, &pAP) && pAP)
	{
		const gchar* pszPageMarginLeft = NULL;
		const gchar* pszPageMarginRight = NULL;

		pAP->getProperty("columns", pszNbCols);
		pAP->getProperty("page-margin-right", pszPageMarginLeft);
		pAP->getProperty("page-margin-left", pszPageMarginRight);

		if (pszNbCols != NULL && ((0 == strcmp(pszNbCols, "2"))
						|| (0 == strcmp(pszNbCols, "3"))))
		{
			m_bMultiCols = true;
		}
		if (pszPageMarginLeft != NULL)
		{
			m_pie->write("\\setlength{\\oddsidemargin}{");
			m_pie->write(static_cast<const char *> (pszPageMarginLeft));
			m_pie->write("-1in");
			m_pie->write("}\n");
		}
		if (pszPageMarginRight != NULL)
		{
			m_pie->write("\\setlength{\\textwidth}{\\paperwidth - ");
			m_pie->write(static_cast<const char *> (pszPageMarginRight));
			m_pie->write("-");
			m_pie->write(static_cast<const char *> (pszPageMarginLeft));
			m_pie->write("}\n");
		}
	}

	if (m_bMultiCols)
	{
		m_pie->write("\\begin{multicols}{");
		m_pie->write(static_cast<const char *> (pszNbCols));
		m_pie->write("}\n");
	}
}

void s_LaTeX_Listener::_convertColor(UT_String& szDest, const char* pszColor)
{
	char colors[3][3];
	for (int i=0;i<3;++i)
	{
		strncpy (colors[i],&pszColor[2*i],2);
		colors[i][2]=0;
	}
	UT_LocaleTransactor lt (LC_NUMERIC, "C");
	UT_String_sprintf (szDest, "%.3f,%.3f,%.3f",
			   strtol (&colors[0][0],NULL,16)/255.,
			   strtol (&colors[1][0],NULL,16)/255.,
			   strtol (&colors[2][0],NULL,16)/255.);
}

struct LaTeX_Font_Size
{
	guint8 tiny;
	guint8 scriptsize;
	guint8 footnotesize;
	guint8 small;
	/* int normalsize; */ 
	guint8 large;
	guint8 Large;
	guint8 LARGE;
	guint8 huge;
	guint8 Huge;
};

/*
 * These font sizes in the standard document classes are documented in 
 * "The (Not So) Short Introduction to LaTeX2e" and the following url:
 * http://en.wikibooks.org/wiki/LaTeX/Formatting
 */
static const LaTeX_Font_Size fontsizes[]=
{
	{5, 7, 8, 9, /*10,*/ 12, 14, 17, 20, 25}, // normalsize == 10pt
	{6, 8, 9, 10, /*11,*/ 12, 17, 17, 20, 25}, // normalsize == 11pt
	{6, 8, 10, 11, /*12,*/ 14, 17, 20, 25, 25} // normalsize == 12pt
};

void s_LaTeX_Listener::_convertFontSize(UT_String& szDest, const char* pszFontSize)
{
	double fSizeInPoints = UT_convertToPoints(pszFontSize);
	const LaTeX_Font_Size *fs = NULL;

	if(m_bInScript) {
		fSizeInPoints -= 4;
	}
	
	if (m_DefaultFontSize == 10)
	{
		fs = &fontsizes[0];
	}
	else if (m_DefaultFontSize == 11)
	{
		fs = &fontsizes[1];
	}
	else // m_DefaultFontSize == 12
	{
		fs = &fontsizes[2];
	}
	
	if (fSizeInPoints <= fs->tiny)
	{
		szDest = "tiny";
	}
	else if (fSizeInPoints <= fs->scriptsize)
	{
		szDest = "scriptsize";
	}
	else if (fSizeInPoints <= fs->footnotesize)
	{
		szDest = "footnotesize";
	}
	else if (fSizeInPoints <= fs->small)
	{
		szDest = "small";
	}
	else if (fSizeInPoints <= m_DefaultFontSize)
	{
		szDest = "normalsize";
	}
	else if (fSizeInPoints <= fs->large)
	{
		szDest = "large";
	}
	else if (fSizeInPoints <= fs->Large)
	{
		szDest = "Large";
	}
	else if (fSizeInPoints <= fs->LARGE)
	{
		szDest = "LARGE";
	}
	else if (fSizeInPoints <= fs->huge)
	{
		szDest = "huge";
	}
	else
	{
		szDest = "Huge";
	}
}

void s_LaTeX_Listener::_openSpan(PT_AttrPropIndex api)
{
	if (!m_bInBlock)
	{
		return;
	}
	
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	m_bOverline = false;
	m_NumCloseBrackets = 0;
	
	if (bHaveProp && pAP)
	{
		const gchar * szValue;

		if (pAP->getProperty("font-weight", szValue)
			&& !strcmp(szValue, "bold")
			)
		{
			m_pie->write("\\textbf{");
			m_NumCloseBrackets++;
		}
		
		if (pAP->getProperty("font-style", szValue)
			&& !strcmp(szValue, "italic")
			)
		{
			m_pie->write("\\emph{");
			m_NumCloseBrackets++;
		}
		
		if (pAP->getProperty("text-position", szValue))
		{
			if (!strcmp("superscript", szValue))
			{
				m_bInScript = true;
				m_pie->write("\\textsuperscript{");
				m_NumCloseBrackets++;
			}
			else if (!strcmp("subscript", szValue))
			{
				m_bInScript = true;
				m_pie->write("\\textsubscript{");
				m_NumCloseBrackets++;
			}
		}
		
		const gchar* pszColor = NULL;
		pAP->getProperty("color", pszColor);
		if (pszColor)
		{
		    if ((0 != strcmp("000000", pszColor)) &&
			(0 != strcmp("transparent", pszColor)))
		    {
				UT_String szColor;
				_convertColor(szColor,(const char*)pszColor);
				m_pie->write("\\textcolor[rgb]{");
				m_pie->write(szColor);
				m_pie->write("}{");
				m_NumCloseBrackets++;
		    }
		}
		
		const gchar* pszBgColor = NULL;
		pAP->getProperty("bgcolor", pszBgColor);

		if (pszBgColor)
		{
		  if ((0 != strcmp("000000", pszBgColor)) &&
		      (0 != strcmp("transparent", pszBgColor)))
		    {
		      UT_String szColor;
		      _convertColor(szColor,(const char*)pszBgColor);
		      m_pie->write("\\colorbox[rgb]{");
		      m_pie->write(szColor);
		      m_pie->write("}{");
		      m_NumCloseBrackets++;
		    }
		}

 		if (pAP->getProperty("font-size", szValue) && !m_bInHeading)
		{
			if (int(0.5 + UT_convertToPoints(szValue)) != m_DefaultFontSize)
			{
				m_pie->write("{\\");
				UT_String szSize;
				_convertFontSize(szSize, static_cast<const char*>(szValue));
				m_pie->write(szSize);
				m_pie->write(" ");
				m_NumCloseBrackets++;
			}
		}
		
		if (pAP->getProperty("font-family", szValue))
		{
			// TODO: Use a dynamic substitution table
			if (strstr(szValue, "Symbol") && !m_bInHeading)
				m_bInSymbol = true;
			if (strstr(szValue, "Courier") ||
				!strcmp("Luxi Mono",szValue)) {
				m_pie->write("\\texttt{");
				m_NumCloseBrackets++;
			}
			if (!strcmp("Arial", szValue) ||
				!strcmp("Helvetic", szValue) ||
				!strcmp("Luxi Sans",szValue)) {
				m_pie->write("\\textsf{");
				m_NumCloseBrackets++;
			}
			UT_DEBUGMSG (("Latex export: TODO: 'font-family' property\n"));
		}

		if (pAP->getProperty("text-decoration", szValue) && szValue && !m_bInHeading)
		{
			gchar* p = g_strdup(szValue);

			UT_return_if_fail(p);
			gchar*	q = strtok(p, " ");

			// See the ulem.sty documentation (available at www.ctan.org)
			// if you wish to include other kinds of underlines, such as
			// double underlines or wavy underlines
			while (q)
			{
			    if (0 == strcmp(q, "underline"))
			    {
					m_pie->write("\\uline{");
					m_NumCloseBrackets++;
			    }
			    else if(0 == strcmp(q, "overline"))
			    {
					m_bOverline = true;
			    }
			    else if(0 == strcmp(q, "line-through"))
			    {
					m_pie->write("\\sout{");
					m_NumCloseBrackets++;
			    }
			    q = strtok(NULL, " ");
			}
			
			/* This should be at the very last, in order to match
			 * the close brackets in _closeSpan().
			 */
			if (m_bOverline)
			    m_pie->write("$\\overline{\\textrm{");
			g_free(p);
		}

		m_bInSpan = true;
		m_pAP_Span = pAP;
	}
}

void s_LaTeX_Listener::_openTable(PT_AttrPropIndex /*api*/)
{
	UT_sint32 i = 0;

	m_pie->write("\n\n%");
	m_pie->write("\n% Table begins");
	m_pie->write("\n% ");
	m_pie->write("\n\\begin{table}[h]\\begin{tabular}{|");
	for(i = 0; i < m_pTableHelper->getNumCols(); i++) m_pie->write("l|");
	m_pie->write("}");
//	m_pie->write("\n\\hline\n");
	
	m_RowNuminTable = 1;
	m_ExpectedLeft = 0;
	m_index = 0;
}

void s_LaTeX_Listener::_closeCell(void)
{
	if (m_CellBot - m_CellTop >1)
	    m_pie->write("}");
	if (m_CellRight - m_CellLeft >1)
	    m_pie->write("}");
	m_bInCell = false;
	this->m_pTableHelper->CloseCell();
	if(m_CellRight == m_TableWidth)
	{
	    m_ExpectedLeft = 0;
	}
	else
	{
	    m_ExpectedLeft = m_CellRight;
	    m_pie->write("&");	    	    
	}
}

void s_LaTeX_Listener::_closeSpan(void)
{
	if (!m_bInSpan)
		return;

	if (m_bOverline)
	    m_pie->write("}}$");
	
	if (m_pAP_Span)
	{
		m_bInScript = false;
		if (m_bInSymbol)
		    m_bInSymbol = false;
		for(; m_NumCloseBrackets>0; m_NumCloseBrackets--)
			m_pie->write("}");

		m_pAP_Span = NULL;
	}

	m_bInSpan = false;
	return;
}

void s_LaTeX_Listener::_closeTable(void)
{
    if(m_pqRect)
    {
		for(unsigned int i=0; i<m_pqRect->size(); i++)
		{
			delete m_pqRect->at(i);
			m_pqRect->at(i) = NULL;
		}
		m_pqRect->clear();
    }
    m_pie->write("\\\\\n\\hline\n");
    m_pie->write("\\end{tabular}\n\\end{table}\n");
}

void s_LaTeX_Listener::_outputData(const UT_UCSChar * data, UT_uint32 length)
{
	if (!m_bInBlock)
	{
		return;
	}

	UT_String sBuf;
	const UT_UCSChar * pData;

	UT_ASSERT(sizeof(UT_Byte) == sizeof(char));

	for (pData = data; (pData < data + length); /**/)
	{
		const char* subst = "";

		if (m_bInSymbol)
		{
			if (_convertLettersToSymbols(*pData, subst))
			{
				while (*subst)
					sBuf += *subst++;
				pData++;
				continue;
			}
		}

		// If you don't know what code is a character,
		// this will print it for you...
		// printf("%c,%d\n",*pData,*pData);

		switch (*pData)
		{

		case ' ':
			if (m_bInScript)
			sBuf += '\\';
			sBuf += ' ';
			pData++;
			break;
			
		case '\\':
			sBuf += "\\ensuremath{\\backslash}";
			pData++;
			break;
			
		case '$':
			sBuf += '\\'; 
			sBuf += '$';
			pData++;
			break;

		case '%':
			sBuf += '\\'; 
			sBuf += '%';
			pData++;
			break;
			
		case '&':
			sBuf += '\\'; 
			sBuf += '&';
			pData++;
			break;

		case '#':
			sBuf += '\\'; 
			sBuf += '#';
			pData++;
			break;

		case '_':
			sBuf += '\\'; 
			sBuf += '_';
			pData++;
			break;

		case '{':
			sBuf += '\\'; 
			sBuf += '{';
			pData++;
			break;

		case '}':
			sBuf += '\\';
			sBuf += '}';
			pData++;
			break;

		case '~':
			sBuf += '\\';
			sBuf += '~';
			sBuf += '{';
			sBuf += '}';
			pData++;
			break;

		case '^':
			sBuf += '\\';
			sBuf += '^';
			sBuf += '{';
			sBuf += '}';
			pData++;
			break;

		case 34:
			(m_bBetweenQuotes = !m_bBetweenQuotes)? sBuf += "{``}" : sBuf += "''";
			pData++;
			break;

		case UCS_LF:					// LF -- representing a Forced-Line-Break
			sBuf += '\\';
			sBuf += '\\';
			pData++;
			break;

		case UCS_VTAB:					// VTAB -- representing a Forced-Column-Break -- TODO
			pData++;
			break;
			
		case UCS_FF:					// FF -- representing a Forced-Page-Break
			sBuf += '\\';
			sBuf += 'n';
			sBuf += 'e';
			sBuf += 'w';
			sBuf += 'p';
			sBuf += 'a';
			sBuf += 'g';
			sBuf += 'e';
			sBuf += '\n';
			pData++;
			break;
			
			
		default:
			int translated =  wvConvertUnicodeToLaTeX(*pData,subst);
			if (translated) 
			{
				while (*subst)
					sBuf += *subst++;
				pData++;
			}
			else 
			{
				char buf[30];
				int len;
				if (m_wctomb.wctomb(buf,len,*pData++)) {
				    for(int i=0;i<len;++i)
						sBuf += buf[i];
				};
			}
			break;
		}
	}

	m_pie->write(sBuf.c_str(),sBuf.size());
}

#define SUB(a,who) case a: subst = "\\(\\"who"\\)"; return true;
#define SUBd(a,who) case a: subst = who; return true;
static bool _convertLettersToSymbols(char c, const char *& subst)
{
	switch (c)
	{
		// only-if-amssymb
// 		SUB('\\', "therefore");

		SUB('\"', "forall");    SUB('$', "exists");
		SUB('\'', "ni");        SUB('@', "cong");
		SUB('^', "perp");       SUB('`', "overline{\\ }");
		SUB('a', "alpha");      SUBd('A', "A");
		SUB('b', "beta"); 	    SUBd('B', "B");
		SUB('c', "chi");  	    SUBd('C', "X");
		SUB('d', "delta");	    SUB('D', "Delta");
		SUB('e', "varepsilon"); SUBd('E', "E");
		SUB('f', "phi");  	    SUB('F', "Phi");
		SUB('g', "gamma");	    SUB('G', "Gamma");
		SUB('h', "eta");	    SUBd('H', "H");
		SUB('i', "iota"); 	    SUBd('I', "I"); 
		SUB('j', "varphi");     SUB('J', "vartheta");
		SUB('k', "kappa"); 	    SUBd('K', "K");
		SUB('l', "lambda");	    SUB('L', "Lambda");
		SUB('m', "mu");    	    SUBd('M', "M");
		SUB('n', "nu");    	    SUBd('N', "N");
		SUBd('o', "o");    	    SUBd('O', "O");
		SUB('p', "pi");    	    SUB('P', "Pi");
		SUB('q', "theta"); 	    SUB('Q', "Theta");
		SUB('r', "rho");   	    SUBd('R', "P");
		SUB('s', "sigma"); 	    SUB('S', "Sigma");
		SUB('t', "tau");   	    SUBd('T', "T");
		SUB('u', "upsilon");    SUBd('U', "Y");
 		SUB('v', "varpi");		SUB('V', "varsigma");
		SUB('w', "omega");      SUB('W', "Omega");
		SUB('x', "xi");         SUB('X', "Xi");
		SUB('y', "psi");        SUB('Y', "Psi");
		SUB('z', "zeta");       SUBd('Z', "Z");
// TODO all those fun upper-ascii letters
	default: return false;
	}
}

// _outputBabelPackage should be called only by the constructer, and only once
void s_LaTeX_Listener::_outputBabelPackage(void)
{
	// Language appears in <abiword> as property "lang",
	// es-ES, en-US, and so forth...
	
	const gchar * szLangCode = NULL;
	m_pDocument->getAttrProp()->getProperty("lang", szLangCode); // language code
	if(szLangCode && *szLangCode)
	{
	    UT_Language lang;
	    UT_uint32 indx = lang.getIndxFromCode(szLangCode);
	    if (indx > 0)
	    {
		char *strLangName = g_strdup(lang.getNthLangName(indx)); // language name
		if (strLangName)
		{
		    m_pie->write("%% Please revise the following command, if your babel\n");
		    m_pie->write("%% package does not support ");
		    m_pie->write(strLangName);
		    m_pie->write("\n");
		    
		    *strLangName = tolower(*strLangName);
		    
		    const char *q = strtok(strLangName, " ("); // retrieve the "significant" part
		    if (strcmp(q, "french") == 0)
			q="frenchb"; // frenchb.ldf
		    else if (strcmp(q, "german") == 0)
			q="germanb"; // germanb.ldf
		    else if (strcmp(q, "portuguese") == 0)
			q="portuges"; // portuges.ldf
		    else if (strcmp(q, "russian") == 0)
			q="russianb"; // russianb.ldf
		    else if (strcmp(q, "slovenian") == 0)
			q="slovene"; // slovene.ldf
		    else if (strcmp(q, "ukrainian") == 0)
			q="ukraineb"; // ukraineb.ldf
		    
		    m_pie->write("\\usepackage[");
		    m_pie->write(q);
		    m_pie->write("]{babel}\n");
		    
		    g_free(strLangName);
		}
	    }
	    
	}
}
s_LaTeX_Listener::s_LaTeX_Listener(PD_Document * pDocument, IE_Exp_LaTeX * pie, 
				    const LaTeX_Analysis_Listener& analysis)
  : m_pDocument(pDocument),
	m_pie(pie),
	m_bInBlock(false),
	m_bInCell(false),
	m_bInSection(false),
	m_bInSpan(false),
	m_bInScript(false),
	m_bInFootnote(false),
	m_bInSymbol(0),
	m_bInEndnote(false),
	m_bHaveEndnote(analysis.m_hasEndnotes),
	m_bOverline(false),
	m_DefaultFontSize(12),
	m_NumCloseBrackets(0),
	list_type(BULLETED_LIST),
	m_pqRect(NULL)
{
	m_pie->write("%% ================================================================================\n");
	m_pie->write("%% This LaTeX file was created by AbiWord.                                         \n");
	m_pie->write("%% AbiWord is a free, Open Source word processor.                                  \n");
	m_pie->write("%% More information about AbiWord is available at http://www.abisource.com/        \n");
	m_pie->write("%% ================================================================================\n");
	m_pie->write("\n");

	// If (documentclass == book), numbered headings begin with x.y.
	// If (documentclass == article), there are no chapter headings.
	// We redefine a "chapter" as a section*.

	m_pie->write("\\documentclass[");
	
	fp_PageSize::Predefined ps = pDocument->m_docPageSize.NameToPredefined(pDocument->m_docPageSize.getPredefinedName());
	switch(ps)
	{
	    case fp_PageSize::psA4:
		    m_pie->write("a4paper");
		    break;
	    case fp_PageSize::psA5:
		    m_pie->write("a5paper");
		    break;
	    case fp_PageSize::psB5:
		    m_pie->write("b5paper");
		    break;
	    case fp_PageSize::psLegal:
		    m_pie->write("legalpaper");
		    break;
	    case fp_PageSize::psLetter:
	    default:
		    m_pie->write("letterpaper");
		    break;
	}
	
	if(pDocument->m_docPageSize.isPortrait())
	    m_pie->write(",portrait");
	else
	    m_pie->write(",landscape");
	
	//retrieve the actual font size
	PD_Style * pStyle = NULL;
	pDocument->getStyle ("Normal", &pStyle);
	if(pStyle)
	{
	    const gchar * szValue = 0;
		pStyle->getProperty("font-size", szValue);
		if (szValue)
		{
			// rounding
			m_DefaultFontSize = int(0.5 + UT_convertToPoints(szValue));
			if (m_DefaultFontSize <= 10)
			{
				m_DefaultFontSize = 10;
				m_pie->write(",10pt");
			}
			else if (m_DefaultFontSize <= 11)
			{
				m_DefaultFontSize = 11;
				m_pie->write(",11pt");
			}
		}
	}
	if (m_DefaultFontSize == 12)
		m_pie->write(",12pt");

	m_pie->write("]{article}\n");
	// Better for ISO-8859-1 than previous: [T1] doesn't work very well
	// TODO: Use inputenc from .abw.
	m_pie->write("\\usepackage[latin1]{inputenc}\n");
	m_pie->write("\\usepackage{calc}\n");
	m_pie->write("\\usepackage{setspace}\n");
	m_pie->write("\\usepackage{fixltx2e}\n");  // for \textsubscript
	m_pie->write("\\usepackage{graphicx}\n");
	m_pie->write("\\usepackage{multicol}\n");
	m_pie->write("\\usepackage[normalem]{ulem}\n");

	_outputBabelPackage();
	
	m_pie->write("\\usepackage{color}\n");

	if (m_bHaveEndnote)
		m_pie->write("\\usepackage{endnotes}\n");

	if (analysis.m_hasTable && analysis.m_hasMultiRow)
	{
	    m_pie->write("\\usepackage{multirow}\n");
	    m_pqRect = new std::deque<UT_Rect*>;
	}
	// Must be as late as possible.
	m_pie->write("\\usepackage{hyperref}\n");

	{
	    const char* misc = XAP_EncodingManager::get_instance()->getTexPrologue();
	    if (misc)
		m_pie->write(misc);
	}
	m_pie->write("\n");
	ChapterNumber = 1;
	m_pie->write("\\begin{document}\n\n");
	
	m_pTableHelper = new ie_Table(pDocument);
}

s_LaTeX_Listener::~s_LaTeX_Listener()
{
	//if (!m_bInFootnote) return;
#ifdef HAVE_LIBXSLT
	if(cur)
	{
		xsltFreeStylesheet(cur);
		cur = NULL;
	}	
#endif
	_closeSection();
	_handleDataItems();
	DELETEP(m_pTableHelper);
	if(m_pqRect)
	{
	    for(unsigned int i=0; i<m_pqRect->size(); i++)
	    {
		delete m_pqRect->at(i);
		m_pqRect->at(i) = NULL;
	    }
	    delete m_pqRect;
	}
	if (m_bHaveEndnote)
		m_pie->write("\n\\theendnotes");
	m_pie->write("\n\\end{document}\n");
}

#ifdef HAVE_LIBXSLT
bool s_LaTeX_Listener::convertMathMLtoLaTeX(const UT_UTF8String & sMathML,
											UT_UTF8String & sLaTeX)
{
	//static xsltStylesheet *cur = NULL;
	xmlDocPtr doc, res;
	xmlChar * pLatex = NULL;
	int len;
	
	if (sMathML.empty())
		// Nothing has failed, but we have nothing to do anyway
		return false;
	if (!cur)
	{
		UT_UTF8String path(XAP_App::getApp()->getAbiSuiteLibDir());
		path += "/xsltml/mmltex.xsl";

		cur = xsltParseStylesheetFile((const xmlChar *)(path.utf8_str()));
		if (!cur)
		{
			UT_DEBUGMSG(("convertMathMLtoLaTeX: Parsing stylesheet failed\n"));
			return false;
		}
	}
	doc = xmlParseDoc((const xmlChar*)(sMathML.utf8_str()));
	if (!doc)
	{
		xxx_UT_DEBUGMSG(("convertMathMLtoLaTeX: Parsing MathML document failed\n"));
		return false;
	}	
	
	res = xsltApplyStylesheet(cur, doc, NULL);
	if (!res)
	{
		xxx_UT_DEBUGMSG(("convertMathMLtoLaTeX: Applying stylesheet failed\n"));
		xmlFreeDoc(doc);
		return false;
	}
	
	if (xsltSaveResultToString(&pLatex, &len, res, cur) != 0)
	{
		xmlFreeDoc(res);
		xmlFreeDoc(doc);
		return false;
	}
	sLaTeX.assign((const char*)pLatex, len);
	
	g_free(pLatex);
	xmlFreeDoc(res);
	xmlFreeDoc(doc);
	return true;
}
#endif

bool s_LaTeX_Listener::populate(PL_StruxFmtHandle /*sfh*/,
								   const PX_ChangeRecord * pcr)
{
	
	
	switch (pcr->getType())
	{
	case PX_ChangeRecord::PXT_InsertSpan:
		{
			const PX_ChangeRecord_Span * pcrs = static_cast<const PX_ChangeRecord_Span *> (pcr);

			PT_AttrPropIndex api = pcr->getIndexAP();
			if (api)
			{
				_openSpan(api);
			}
			
			PT_BufIndex bi = pcrs->getBufIndex();
			_outputData(m_pDocument->getPointer(bi),pcrs->getLength());

			if (api)
				_closeSpan();
			return true;
		}

	case PX_ChangeRecord::PXT_InsertObject:
		{
			PT_AttrPropIndex api = pcr->getIndexAP();
			const PX_ChangeRecord_Object * pcro = static_cast<const PX_ChangeRecord_Object *> (pcr);

			const PP_AttrProp * pAP = NULL;
			bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);

			const gchar* szValue = NULL;

			fd_Field* field = NULL;

			switch (pcro->getObjectType())
			{
			case PTO_Image:
				// LaTeX assumes images are EPS.
				// PDFLaTeX assumes images are PNG.
				// Currently we can create PNG images only
				// TODO: is it possible to create EPS images after the cairo integration? 
				if(bHaveProp)
					_handleImage(pAP);
				return true;

			case PTO_Field:

			  field = pcro->getField();
			  if(field->getValue())
			      m_pie->write(field->getValue());

				// we do nothing with computed fields.
				return true;

			case PTO_Hyperlink:
				_closeSpan () ;
				if(m_bInHeading) return true;
				if(bHaveProp && pAP && pAP->getAttribute("xlink:href", szValue))
				{
					m_pie->write("\\href{");
					m_pie->write(szValue);
					m_pie->write("}{");
				}
				else
				{
					m_pie->write("}");
				}
				return true;

			case PTO_Bookmark:
				if(m_bInHeading) return true;
				if(bHaveProp && pAP && pAP->getAttribute("type", szValue))
				{
					if(0 == strcmp("start",szValue))
					{
						if(pAP->getAttribute("name", szValue))
						{
							m_pie->write("\\hypertarget{");
							m_pie->write(szValue);
							m_pie->write("}{");
						}
					}
					else if(0 == strcmp("end",szValue)) m_pie->write("}");
				}
				else
				{
					m_pie->write("}");
				}
				return true;
				
			  case PTO_Math:
				_closeSpan () ;
				if(bHaveProp && pAP)
				{
					UT_UTF8String sLatex;
					const UT_ByteBuf * pByteBuf = NULL;
					UT_UCS4_mbtowc myWC;
					
					if(pAP->getAttribute("latexid", szValue) &&	szValue 
							&& *szValue)
					{					
						bool bFoundLatex = m_pDocument->getDataItemDataByName(szValue, 
						    &pByteBuf,
						    NULL, NULL);
						if(!bFoundLatex)
						{
							UT_DEBUGMSG(("Equation %s not found in document \n", szValue));
							return true;
						}
						sLatex.appendBuf(*pByteBuf, myWC);
						
						m_pie->write("$");
						m_pie->write(sLatex.utf8_str());
						m_pie->write("$");
					}
#ifdef HAVE_LIBXSLT
					else if(pAP->getAttribute("dataid", szValue) &&	szValue 
							&& *szValue)
					{
						UT_UTF8String sMathML;
						bool bFoundMathML = m_pDocument->getDataItemDataByName(szValue, 
						    &pByteBuf,
						    NULL, NULL);
						if(!bFoundMathML)
						{
							UT_DEBUGMSG(("Equation %s not found in document \n", szValue));
							return true;
						}
						
						sMathML.appendBuf(*pByteBuf, myWC);
						
						if(!convertMathMLtoLaTeX(sMathML, sLatex))
							return true;
						/*The converted sLatex already contains $s*/
						m_pie->write(sLatex.utf8_str());
					}
#endif
					else
						return true;
				}
				return true;

			default:
				UT_ASSERT_HARMLESS(0);
				return true;
			}
		}

	case PX_ChangeRecord::PXT_InsertFmtMark:
		return true;
		
	default:
		UT_ASSERT_HARMLESS(0);
		return false;
	}
}

bool s_LaTeX_Listener::populateStrux(PL_StruxDocHandle sdh,
										   const PX_ChangeRecord * pcr,
										   PL_StruxFmtHandle * psfh)
{
	UT_ASSERT(pcr->getType() == PX_ChangeRecord::PXT_InsertStrux);
	const PX_ChangeRecord_Strux * pcrx = static_cast<const PX_ChangeRecord_Strux *> (pcr);
	*psfh = 0;							// we don't need it.

	switch (pcrx->getStruxType())
	{
	case PTX_Section:
	{
		_closeSection();

		PT_AttrPropIndex indexAP = pcr->getIndexAP();
		const PP_AttrProp* pAP = NULL;
		if (m_pDocument->getAttrProp(indexAP, &pAP) && pAP)
		{
			const gchar* pszSectionType = NULL;
			pAP->getAttribute("type", pszSectionType);
			if (
				!pszSectionType
				|| (0 == strcmp(pszSectionType, "doc"))
				)
			{
				_openSection(pcr->getIndexAP());
				m_bInSection = true;
			}
			else
			{
				m_bInSection = false;
			}
		}
		else
		{
			m_bInSection = false;
		}
		
		return true;
	}

	case PTX_SectionHdrFtr:
	{
		_closeSection();

		PT_AttrPropIndex indexAP = pcr->getIndexAP();
		const PP_AttrProp* pAP = NULL;
		if (m_pDocument->getAttrProp(indexAP, &pAP) && pAP)
		{
			const gchar* pszSectionType = NULL;
			pAP->getAttribute("type", pszSectionType);
			if (
				!pszSectionType
				|| (0 == strcmp(pszSectionType, "doc"))
				)
			{
				_openSection(pcr->getIndexAP());
				m_bInSection = true;
			}
			else
			{
				m_bInSection = false;
			}
		}
		else
		{
			m_bInSection = false;
		}
		
		return true;
	}

	case PTX_Block:
	{
		_closeBlock();
		_closeParagraph();
		_openParagraph(pcr->getIndexAP());
		return true;
	}

	case PTX_SectionTable:
	{
		m_pTableHelper->OpenTable(sdh, pcr->getIndexAP());
		m_TableWidth = m_pTableHelper->getNumCols();
		_openTable(pcr->getIndexAP());
		return true;
	}

	case PTX_EndTable:
	{
		_closeTable();
		m_pTableHelper->CloseTable();
		return true;
	}

	case PTX_SectionCell:
	{
		_openCell(pcr->getIndexAP());
		return true;
	}

	case PTX_EndCell:
	{
		_closeCell();
		return true;
	}

	case PTX_EndFrame:
	case PTX_EndMarginnote:
	case PTX_EndFootnote:
	{
		m_bInFootnote = false;
		m_pie->write("} ");
		return true;
	}

	case PTX_SectionFrame:
	case PTX_SectionMarginnote:
	case PTX_SectionFootnote:
	{
		m_bInFootnote = true;
		m_pie->write("\\footnote{");
		return true;
	}
	
	case PTX_SectionTOC:
	{
		_closeBlock();
		/*
		_closeSection();
		 */
		m_pie->write("\\tableofcontents \n");
		return true;
	}
	case PTX_EndTOC:
		return true;
	
	case PTX_SectionEndnote:
	{
		m_bInEndnote = true;
		m_pie->write("\\endnote{");
		return true;
	}
	case PTX_EndEndnote:
	{
		m_bInEndnote = false;
		m_pie->write("} ");
		return true;
	}
	default:
		UT_ASSERT(UT_TODO);
		return true;
	}
}

bool s_LaTeX_Listener::change(PL_StruxFmtHandle /*sfh*/,
									const PX_ChangeRecord * /*pcr*/)
{
	UT_ASSERT(0);						// this function is not used.
	return false;
}

bool s_LaTeX_Listener::insertStrux(PL_StruxFmtHandle /*sfh*/,
									 const PX_ChangeRecord * /*pcr*/,
									 PL_StruxDocHandle /*sdh*/,
									 PL_ListenerId /* lid */,
									 void (* /*pfnBindHandles*/)(PL_StruxDocHandle /* sdhNew */,
																 PL_ListenerId /* lid */,
																 PL_StruxFmtHandle /* sfhNew */))
{
	UT_ASSERT(0);						// this function is not used.
	return false;
}

bool s_LaTeX_Listener::signal(UT_uint32 /* iSignal */)
{
	UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
	return false;
}


/*****************************************************************/
/*****************************************************************/

UT_Error IE_Exp_LaTeX::_writeDocument(void)
{
	LaTeX_Analysis_Listener analysis(getDoc(), this);
	if (!getDoc()->tellListener(&analysis))
		return UT_ERROR;

	m_pListener = new s_LaTeX_Listener(getDoc(),this, analysis);
	if (!m_pListener)
		return UT_IE_NOMEMORY;
	if (!getDoc()->tellListener(static_cast<PL_Listener *>(m_pListener)))
		return UT_ERROR;
	delete m_pListener;

	m_pListener = NULL;
	
	return ((m_error) ? UT_IE_COULDNOTWRITE : UT_OK);
}

/*****************************************************************/
/*****************************************************************/

void s_LaTeX_Listener::_handleDataItems(void)
{
}


/* Code taken from the HTML exporter
 *
 * imagedir is the name of the directory in which we'll write the image
 * filename is the name of the file to which we'll write the image
 */
void s_LaTeX_Listener::_writeImage (const UT_ByteBuf * pByteBuf,
									const UT_UTF8String & imagedir,
									const UT_UTF8String & filename)
{
	UT_go_directory_create(imagedir.utf8_str(), 0750, NULL);

	UT_UTF8String path(imagedir);
	path += "/";
	path += filename;

	GsfOutput * out = UT_go_file_create (path.utf8_str (), NULL);
	if (out)
	{
		gsf_output_write (out, pByteBuf->getLength (), (const guint8*)pByteBuf->getPointer (0));
		gsf_output_close (out);
		g_object_unref (G_OBJECT (out));
	}
}

void s_LaTeX_Listener::_handleImage(const PP_AttrProp * pAP)
{	
	/* Part of code taken from the HTML exporter */
	const UT_ByteBuf * pByteBuf;
	UT_ByteBuf decodedByteBuf;				
	const gchar *szHeight = NULL, *szWidth = NULL, *szDataID = NULL, *szMimeType = NULL;
	
	if (! pAP)
		return;
	if (! pAP->getAttribute("dataid", szDataID))
		return;

	if (! m_pDocument->getDataItemDataByName(szDataID, &pByteBuf, reinterpret_cast<const void**>(&szMimeType), NULL))
		return;
	if ((pByteBuf == 0) || (szMimeType == 0)) return; // ??

	if (strcmp (szMimeType, "image/png") != 0)
	{
		UT_DEBUGMSG(("Object not of MIME type image/png - ignoring...\n"));
		return;
	}

	gchar *imagedir = UT_go_dirname_from_uri(m_pie->getFileName(), true);
	
	UT_UTF8String filename(szDataID);
	filename += ".png";
	
	/* save the image as imagedir/filename */
	_writeImage (pByteBuf, imagedir, filename);
	FREEP(imagedir);
	
	m_pie->write("\\includegraphics");
	if (pAP->getProperty("height", szHeight) && pAP->getProperty("width", szWidth))
	{
		m_pie->write("[height=");
		m_pie->write(szHeight);
		m_pie->write(",width=");
		m_pie->write(szWidth);
		m_pie->write("]");
	}

	m_pie->write("{");
	m_pie->write(szDataID);
	m_pie->write("}\n");

	return;	
}
/*
  This is a copy from wv. Returns 1 if was translated, 0 if wasn't.
  It can convert to empty string.
*/
#undef printf
#define printf(x) out = (x); 

static int wvConvertUnicodeToLaTeX(U16 char16,const char*& out)
	{
	out = ""; //this is needed

	//DEBUG: printf("%d,%c\n",char16,char16);

	/* 
	german and scandinavian characters, MV 1.7.2000 
	See man iso_8859_1

 	This requires the inputencoding latin1 package,
 	see latin1.def. Chars in range 160...255 are just
	put through as these are legal iso-8859-1 symbols.
	(see above)
	
	Best way to do it until LaTeX is Unicode enabled 
	(Omega project).
	-- MV 4.7.2000 
	*/
	
	switch(char16)
		{
		/* Fix up these as math characters: */
		case 0xb1:
			printf("$\\pm$");
			return(1);
		case 0xb2:
			printf("$\\mathtwosuperior$");
			return(1);
		case 0xb3:
			printf("$\\maththreesuperior$");
			return(1);
		case 0xb5:
			printf("$\\mu$");
			return(1);
		case 0xb9:
			printf("$\\mathonesuperior$");
			return(1);
		case 0xdc:
			printf("\\ldots{}");
			return(1);
		case 37:
			printf("\\%%");
			return(1);
		case 11:
			printf("\\\\\n");
			return(1);
		case 30:
		case 31:
		
		case 12:
		case 13:
		case 14:
		case 7:
			return(1);
		case 45:
			printf("-");
			return(1);
		case 34:
			printf("\"");
			return(1);
		case 35:
			printf("\\#"); /* MV 14.8.2000 */
			return(1);
		case 36:
			printf("\\$"); /* MV 14.8.2000 */
			return(1);
		case 38:
			printf("\\&"); /* MV 1.7.2000 */
			return(1);
		case 60:
			printf("$<$");
			return(1);
		case 62:
			printf("$>$");
			return(1);

		case 0xF8E7:	
		/* without this, things should work in theory, but not for me */
			printf("_");
			return(1);

	/* Added some new Unicode characters. It's probably difficult
           to write these characters in AbiWord, though ... :(
           -- 2000-08-11 huftis@bigfoot.com */

		case 0x0100:
			printf("\\=A"); /* A with macron */
			return(1);
		case 0x0101:
			printf("\\=a");  /* a with macron */
			return(1);
		case 0x0102:
			printf("\\u{A}");  /* A with breve */
			return(1);
		case 0x0103:
			printf("\\u{a}");  /* a with breve */
			return(1);

		case 0x0106:
			printf("\\'C");  /* C with acute */
			return(1);
		case 0x0107:
			printf("\\'c");  /* c with acute */
			return(1);
		case 0x0108:
			printf("\\^C");  /* C with circumflex */
			return(1);
		case 0x0109:
			printf("\\^c");  /* c with circumflex */
			return(1);
		case 0x010A:
			printf("\\.C");  /* C with dot above */
			return(1);
		case 0x010B:
			printf("\\.c");  /* c with dot above */
			return(1);
		case 0x010C:
			printf("\\v{C}");  /* C with caron */
			return(1);
		case 0x010D:
			printf("\\v{c}");  /* c with caron */
			return(1);
		case 0x010E:
			printf("\\v{D}");  /* D with caron */
			return(1);
		case 0x010F:
			printf("\\v{d}");  /* d with caron */
			return(1);
		case 0x0110:
			printf("\\DJ{}");  /* D with stroke */
			return(1);
		case 0x0111:
			printf("\\dj{}");  /* d with stroke */
			return(1);
		case 0x0112:
			printf("\\=E");  /* E with macron */
			return(1);
		case 0x0113:
			printf("\\=e");  /* e with macron */
			return(1);
		case 0x0114:
			printf("\\u{E}");  /* E with breve */
			return(1);
		case 0x0115:
			printf("\\u{e}");  /* e with breve */
			return(1);
		case 0x0116:
			printf("\\.E");  /* E with dot above */
			return(1);
		case 0x0117:
			printf("\\.e");  /* e with dot above */
			return(1);

		case 0x011A:
			printf("\\v{E}");  /* E with caron */
			return(1);
		case 0x011B:
			printf("\\v{e}");  /* e with caron */
			return(1);
		case 0x011C:
			printf("\\^G");  /* G with circumflex */
			return(1);
		case 0x011D:
			printf("\\^g");  /* g with circumflex */
			return(1);
		case 0x011E:
			printf("\\u{G}");  /* G with breve */
			return(1);
		case 0x011F:
			printf("\\u{g}");  /* g with breve */
			return(1);
		case 0x0120:
			printf("\\.G");  /* G with dot above */
			return(1);
		case 0x0121:
			printf("\\u{g}");  /* g with dot above */
			return(1);
		case 0x0122:
			printf("^H");  /* H with circumflex */
			return(1);
		case 0x0123:
			printf("^h");  /* h with circumflex */
			return(1);

		case 0x0128:
			printf("\\~I");  /* I with tilde */
			return(1);
		case 0x0129:
			printf("\\~{\\i}");  /* i with tilde (dotless) */
			return(1);
		case 0x012A:
			printf("\\=I");  /* I with macron */
			return(1);
		case 0x012B:
			printf("\\={\\i}");  /* i with macron (dotless) */
			return(1);
		case 0x012C:
			printf("\\u{I}");  /* I with breve */
			return(1);
		case 0x012D:
			printf("\\u{\\i}");  /* i with breve */
			return(1);

		case 0x0130:
			printf("\\.I");  /* I with dot above */
			return(1);
		case 0x0131:
			printf("\\i{}");  /* dotless i */
			return(1);
		case 0x0132:
			printf("IJ");  /* IJ ligature */
			return(1);
		case 0x0133:
			printf("ij");  /* ij ligature  */
			return(1);
		case 0x0134:
			printf("\\^J");  /* J with circumflex (dotless) */
			return(1);
		case 0x0135:
			printf("\\^{\\j}");  /* j with circumflex (dotless) */
			return(1);
		case 0x0136:
			printf("\\c{K}");  /* K with cedilla */
			return(1);
		case 0x0137:
			printf("\\c{k}");  /* k with cedilla */
			return(1);

		case 0x0138:
			printf("k");  /* NOTE: Not the correct character (kra), but similar */
			return(1);

		case 0x0139:
			printf("\\'L");  /* L with acute */
			return(1);
		case 0x013A:
			printf("\\'l");  /* l with acute  */
			return(1);
		case 0x013B:
			printf("\\c{L}");  /* L with cedilla */
			return(1);
		case 0x013C:
			printf("\\c{l}");  /* l with cedilla */
			return(1);
		case 0x013D:
			printf("\\v{L}");  /* L with caron */
			return(1);
		case 0x013E:
			printf("\\v{l}");  /* l with caron */
			return(1);

		case 0x0141:
			printf("\\L{}");  /* L with stroke */
			return(1);
		case 0x0142:
			printf("\\l{}");  /* l with stroke  */
			return(1);
		case 0x0143:
			printf("\\'N");  /* N with acute */
			return(1);
		case 0x0144:
			printf("\\'n");  /* n with acute */
			return(1);
		case 0x0145:
			printf("\\c{N}");  /* N with cedilla */
			return(1);
		case 0x0146:
			printf("\\c{n}");  /* n with cedilla */
			return(1);
		case 0x0147:
			printf("\\v{N}");  /* N with caron */
			return(1);
		case 0x0148:
			printf("\\v{n}");  /* n with caron */
			return(1);
		case 0x0149:
			printf("'n");  /* n preceed with apostroph  */
			return(1);
		case 0x014A:
			printf("\\NG{}");  /* ENG character */
			return(1);
		case 0x014B:
			printf("\\ng{}");  /* eng character */
			return(1);
		case 0x014C:
			printf("\\=O");  /* O with macron */
			return(1);
		case 0x014D:
			printf("\\=o");  /* o with macron */
			return(1);
		case 0x014E:
			printf("\\u{O}");  /* O with breve */
			return(1);
		case 0x014F:
			printf("\\u{o}");  /* o with breve */
			return(1);
		case 0x0150:
			printf("\\H{O}");  /* O with double acute */
			return(1);
		case 0x0151:
			printf("\\H{o}");  /* o with double acute */
			return(1);
		case 0x0152:
			printf("\\OE{}");  /* OE ligature */
			return(1);
		case 0x0153:
			printf("\\oe{}");  /* oe ligature */
			return(1);
		case 0x0154:
			printf("\\'R");  /* R with acute */
			return(1);
		case 0x0155:
			printf("\\'r");  /* r with acute */
			return(1);
		case 0x0156:
			printf("\\c{R}");  /* R with cedilla */
			return(1);
		case 0x0157:
			printf("\\c{r}");  /* r with cedilla */
			return(1);
		case 0x0158:
			printf("\\v{R}");  /* R with caron */
			return(1);
		case 0x0159:
			printf("\\v{r}");  /* r with caron */
			return(1);
		case 0x015A:
			printf("\\'S");  /* S with acute */
			return(1);
		case 0x015B:
			printf("\\'s");  /* s with acute */
			return(1);
		case 0x015C:
			printf("\\^S");  /* S with circumflex */
			return(1);
		case 0x015D:
			printf("\\^s");  /* c with circumflex */
			return(1);
		case 0x015E:
			printf("\\c{S}");  /* S with cedilla */
			return(1);
		case 0x015F:
			printf("\\c{s}");  /* s with cedilla */
			return(1);
		case 0x0160:
			printf("\\v{S}");  /* S with caron */
			return(1);
		case 0x0161:
			printf("\\v{s}");  /* s with caron */
			return(1);
		case 0x0162:
			printf("\\c{T}");  /* T with cedilla */
			return(1);
		case 0x0163:
			printf("\\c{t}");  /* t with cedilla */
			return(1);
		case 0x0164:
			printf("\\v{T}");  /* T with caron */
			return(1);
		case 0x0165:
			printf("\\v{t}");  /* t with caron */
			return(1);

		case 0x0168:
			printf("\\~U");  /* U with tilde */
			return(1);
		case 0x0169:
			printf("\\~u");  /* u with tilde */
			return(1);
		case 0x016A:
			printf("\\=U");  /* U with macron */
			return(1);

		/* Greek (thanks Petr Vanicek!): */
		case 0x0391:
			printf("$\\Alpha$");
			return(1);
		case 0x0392:
			printf("$\\Beta$");
			return(1);
		case 0x0393:
			printf("$\\Gamma$");
			return(1);
		case 0x0394:
			printf("$\\Delta$");
			return(1);
		case 0x0395:
			printf("$\\Epsilon$");
			return(1);
		case 0x0396:
			printf("$\\Zeta$");
			return(1);
		case 0x0397:
			printf("$\\Eta$");
			return(1);
		case 0x0398:
			printf("$\\Theta$");
			return(1);
		case 0x0399:
			printf("$\\Iota$");
			return(1);
		case 0x039a:
			printf("$\\Kappa$");
			return(1);
		case 0x039b:
			printf("$\\Lambda$");
			return(1);
		case 0x039c:
			printf("$\\Mu$");
			return(1);
		case 0x039d:
			printf("$\\Nu$");
			return(1);
		case 0x039e:
			printf("$\\Xi$");
			return(1);
		case 0x039f:
			printf("$\\Omicron$");
			return(1);
		case 0x03a0:
			printf("$\\Pi$");
			return(1);
		case 0x03a1:
			printf("$\\Rho$");
			return(1);

		case 0x03a3:
			printf("$\\Sigma$");
			return(1);
		case 0x03a4:
			printf("$\\Tau$");
			return(1);
		case 0x03a5:
			printf("$\\Upsilon$");
			return(1);
		case 0x03a6:
			printf("$\\Phi$");
			return(1);
		case 0x03a7:
			printf("$\\Chi$");
			return(1);
		case 0x03a8:
			printf("$\\Psi$");
			return(1);
		case 0x03a9:
			printf("$\\Omega$");
			return(1);

		/* ...and lower case: */

		case 0x03b1:
			printf("$\\alpha$");
			return(1);
		case 0x03b2:
			printf("$\\beta$");
			return(1);
		case 0x03b3:
			printf("$\\gamma$");
			return(1);
		case 0x03b4:
			printf("$\\delta$");
			return(1);
		case 0x03b5:
			printf("$\\epsilon$");
			return(1);
		case 0x03b6:
			printf("$\\zeta$");
			return(1);
		case 0x03b7:
			printf("$\\eta$");
			return(1);
		case 0x03b8:
			printf("$\\theta$");
			return(1);
		case 0x03b9:
			printf("$\\iota$");
			return(1);
		case 0x03ba:
			printf("$\\kappa$");
			return(1);
		case 0x03bb:
			printf("$\\lambda$");
			return(1);
		case 0x03bc:
			printf("$\\mu$");
			return(1);
		case 0x03bd:
			printf("$\\nu$");
			return(1);
		case 0x03be:
			printf("$\\xi$");
			return(1);
		case 0x03bf:
			printf("$\\omicron$");
			return(1);
		case 0x03c0:
			printf("$\\pi$");
			return(1);
		case 0x03c1:
			printf("$\\rho$");
			return(1);

		case 0x03c3:
			printf("$\\sigma$");
			return(1);
		case 0x03c4:
			printf("$\\tau$");
			return(1);
		case 0x03c5:
			printf("$\\upsilon$");
			return(1);
		case 0x03c6:
			printf("$\\phi$");
			return(1);
		case 0x03c7:
			printf("$\\chi$");
			return(1);
		case 0x03c8:
			printf("$\\psi$");
			return(1);
		case 0x03c9:
			printf("$\\omega$");
			return(1);

	/* More math, typical inline: */
		case 0x2111:
			printf("$\\Im$");
			return(1);
		case 0x2118:
			printf("$\\wp$");   /* Weierstrass p */
			return(1);
		case 0x211c:
			printf("$\\Re$");
			return(1);
		case 0x2135:
			printf("$\\aleph$");
			return(1);

		case 0x2190:
			printf("$\\leftarrow$");
			return(1);
		case 0x2191:
			printf("$\\uparrow$");
			return(1);
		case 0x2192:
			printf("$\\rightarrow$");
			return(1);
		case 0x2193:
			printf("$\\downarrow$");
			return(1);
		case 0x21d0:
			printf("$\\Leftarrow$");
			return(1);
		case 0x21d1:
			printf("$\\Uparrow$");
			return(1);
		case 0x21d2:
			printf("$\\Rightarrow$");
			return(1);
		case 0x21d3:
			printf("$\\Downarrow$");
			return(1);
		case 0x21d4:
			printf("$\\Leftrightarrow$");
			return(1);

		case 0x2200:
			printf("$\\forall$");
			return(1);
		case 0x2202:
			printf("$\\partial$");
			return(1);
		case 0x2203:
			printf("$\\exists$");
			return(1);
		case 0x2205:
			printf("$\\emptyset$");
			return(1);
		case 0x2207:
			printf("$\\nabla$");
			return(1);
		case 0x2208:
			printf("$\\in$");   /* element of */
			return(1);
		case 0x2209:
			printf("$\\notin$");   /* not an element of */
			return(1);
		case 0x220b:
			printf("$\\ni$");   /* contains as member */
			return(1);
		case 0x221a:
			printf("$\\surd$"); 	/* sq root */
			return(1);
		case 0x2212:
			printf("$-$");		/* minus */
			return(1);
		case 0x221d:
			printf("$\\propto$");
			return(1);
		case 0x221e:
			printf("$\\infty$");
			return(1);
		case 0x2220:
			printf("$\\angle$");
			return(1);
		case 0x2227:
			printf("$\\land$"); /* logical and */
			return(1);
		case 0x2228:
			printf("$\\lor$");   /* logical or */
			return(1);
		case 0x2229:
			printf("$\\cap$"); /* intersection */
			return(1);
		case 0x222a:
			printf("$\\cup$"); /* union */
			return(1);
		case 0x223c:
			printf("$\\sim$"); /* similar to  */
			return(1);
		case 0x2248:
			printf("$\\approx$");
			return(1);
		case 0x2261:
			printf("$\\equiv$");
			return(1);
		case 0x2260:
			printf("$\\neq$");
			return(1);
		case 0x2264:
			printf("$\\leq$");
			return(1);
		case 0x2265:
			printf("$\\geq$");
			return(1);
		case 0x2282:
			printf("$\\subset$");
			return(1);
		case 0x2283:
			printf("$\\supset$");
			return(1);
		case 0x2284:
			printf("$\\notsubset$");
			return(1);
		case 0x2286:
			printf("$\\subseteq$");
			return(1);
		case 0x2287:
			printf("$\\supseteq$");
			return(1);
		case 0x2295:
			printf("$\\oplus$");   /* circled plus */
			return(1);
		case 0x2297:
			printf("$\\otimes$");
			return(1);
		case 0x22a5:
			printf("$\\perp$");	/* perpendicular */
			return(1);




		case 0x2660:
			printf("$\\spadesuit$");
			return(1);
		case 0x2663:
			printf("$\\clubsuit$");
			return(1);
		case 0x2665:
			printf("$\\heartsuit$");
			return(1);
		case 0x2666:
			printf("$\\diamondsuit$");
			return(1);


		case 0x01C7:
			printf("LJ");  /* the LJ letter */
			return(1);
		case 0x01C8:
			printf("Lj");  /* the Lj letter */
			return(1);
		case 0x01C9:
			printf("lj");  /* the lj letter */
			return(1);
		case 0x01CA:
			printf("NJ");  /* the NJ letter */
			return(1);
		case 0x01CB:
			printf("Nj");  /* the Nj letter */
			return(1);
		case 0x01CC:
			printf("nj");  /* the nj letter */
			return(1);
		case 0x01CD:
			printf("\\v{A}");  /* A with caron */
			return(1);
		case 0x01CE:
			printf("\\v{a}");  /* a with caron */
			return(1);
		case 0x01CF:
			printf("\\v{I}");  /* I with caron */
			return(1);
		case 0x01D0:
			printf("\\v{\\i}");  /* i with caron (dotless) */
			return(1);
		case 0x01D1:
			printf("\\v{O}");  /* O with caron */
			return(1);
		case 0x01D2:
			printf("\\v{o}");  /* o with caron */
			return(1);
		case 0x01D3:
			printf("\\v{U}");  /* U with caron */
			return(1);
		case 0x01D4:
			printf("\\v{u}");  /* u with caron */
			return(1);

		case 0x01E6:
			printf("\\v{G}");  /* G with caron */
			return(1);
		case 0x01E7:
			printf("\\v{g}");  /* g with caron */
			return(1);
		case 0x01E8:
			printf("\\v{K}");  /* K with caron */
			return(1);
		case 0x01E9:
			printf("\\v{k}");  /* k with caron */
			return(1);


		case 0x01F0:
			printf("\\v{\\j}");  /* j with caron (dotless) */
			return(1);
		case 0x01F1:
			printf("DZ");  /* the DZ letter */
			return(1);
		case 0x01F2:
			printf("Dz");  /* the Dz letter */
			return(1);
		case 0x01F3:
			printf("dz");  /* the dz letter */
			return(1);
		case 0x01F4:
			printf("\\'G");  /* G with acute */
			return(1);
		case 0x01F5:
			printf("\\'g");  /* g with acute */
			return(1);

		case 0x01FA:
			printf("\\'{\\AA}");  /* ? with acute */
			return(1);
		case 0x01FB:
			printf("\\'{\\aa}");  /* ? with acute */
			return(1);
		case 0x01FC:
			printf("\\'{\\AE}");  /* ? with acute */
			return(1);
		case 0x01FD:
			printf("\\'{\\ae}");  /* ? with acute */
			return(1);
		case 0x01FE:
			printf("\\'{\\O}");  /* ? with acute */
			return(1);
		case 0x01FF:
			printf("\\'{\\o}");  /* ? with acute */
			return(1);

		case 0x2010:
			printf("-"); /* hyphen */
			return(1);
		case 0x2011:
			printf("-"); /* non-breaking hyphen (is there a way to get this in LaTeX?) */
			return(1);
		case 0x2012:
			printf("--"); /* figure dash (similar to en-dash) */
			return(1);
		case 0x2013:
			/* 
			soft-hyphen? Or en-dash? I find that making 
			this a soft-hyphen works very well, but makes
			the occasional "hard" word-connection hyphen 
			(like the "-" in roller-coaster) disappear.
			(Are these actually en-dashes? Dunno.)
			How does MS Word distinguish between the 0x2013's
			that signify soft hyphens and those that signify
			word-connection hyphens? wvware should be able
			to as well. -- MV 8.7.2000
	
			U+2013 is the en-dash character and not a soft
			hyphen. Soft hyphen is U+00AD. Changing to
			"--". -- 2000-08-11 huftis@bigfoot.com
			*/
			printf("--"); 
			return(1);

		case 0x016B:
			printf("\\=u");  /* u with macron */
			return(1);
		case 0x016C:
			printf("\\u{U}");  /* U with breve */
			return(1);
		case 0x016D:
			printf("\\u{u}");  /* u with breve */
			return(1);
		case 0x016E:
			printf("\\r{U}");  /* U with ring above */
			return(1);
		case 0x016F:
			printf("\\r{u}");  /* u with ring above */
			return(1);
		case 0x0170:
			printf("\\H{U}");  /* U with double acute */
			return(1);
		case 0x0171:
			printf("\\H{u}");  /* u with double acute */
			return(1);

		case 0x0174:
			printf("\\^W");  /* W with circumflex */
			return(1);
		case 0x0175:
			printf("\\^w");  /* w with circumflex */
			return(1);
		case 0x0176:
			printf("\\^Y");  /* Y with circumflex */
			return(1);
		case 0x0177:
			printf("\\^y");  /* y with circumflex */
			return(1);
		case 0x0178:
			printf("\\\"Y");  /* Y with diaeresis */
			return(1);
		case 0x0179:
			printf("\\'Z");  /* Z with acute */
			return(1);
		case 0x017A:
			printf("\\'z");  /* z with acute */
			return(1);
		case 0x017B:
			printf("\\.Z");  /* Z with dot above */
			return(1);
		case 0x017C:
			printf("\\.z");  /* z with dot above */
			return(1);
		case 0x017D:
			printf("\\v{Z}");  /* Z with caron */
			return(1);
		case 0x017E:
			printf("\\v{z}");  /* z with caron */
			return(1);
	/* German and Spanish characters as well as some specials */
                case 0x00EB:
                        printf("\\\"{e}");
                        return(1);
                case 0x00CB:
                        printf("\\\"{E}");
                        return(1);
                case 0x00F6:
                        printf("\\\"{o}");
                        return(1);
                case 0x00E4:
                        printf("\\\"{a}");
                        return(1);
                case 0x00FC:
                        printf("\\\"{u}");
                        return(1);
                case 0x00C4:
                        printf("\\\"{A}");
                        return(1);
                case 0x00D6:
                        printf("\\\"{O}");
                        return(1);
#if 0
                case 0x00DC:
                        printf("\\\"{U}");
                        return(1);
#endif
                case 0x00DF:
                        printf("\\ss{}");
                        return(1);
                case 0x00E9: /* e with acute */
                        printf("\\\'{e}");
                        return(1);
                case 0x00C9: /* E with acute */
                        printf("\\\'{E}");
                        return(1);
                case 0x00E8: /* e with grave */
                        printf("\\`{e}");
                        return(1);
                case 0x00C8: /* E with grave */
                        printf("\\`{E}");
                        return(1);
                case 0x00FD: /* y with acute */
                        printf("\\\'{y}");
                        return(1);
                case 0x00DD: /* Y with acute */
                        printf("\\\'{Y}");
                        return(1);
                case 0x00F8: /* o with stroke */
                        printf("{\\o}");
                        return(1);
                case 0x00D8: /* O with stroke */
                        printf("{\\O}");
                        return(1);
                case 0x00E0: /* a with grave */
                        printf("\\`{a}");
                        return(1);
                case 0x00C0: /* A with grave */
                        printf("\\`{A}");
                        return(1);
                case 0x00E1: /* a with acute */
                        printf("\\\'{a}");
                        return(1);
                case 0x00ED: /* i with acute */
                        printf("\\\'{i}");
                        return(1);
                case 0x00FA: /* u with acute */
                        printf("\\\'{u}");
                        return(1);
                case 0x00F2: /* o with grave */
                        printf("\\`{o}");
                        return(1);
                case 0x00F3: /* o with acute */
                        printf("\\\'{o}");
                        return(1);
                case 0x00F1: /* n with tilde */
                        printf("\\~{n}");
                        return(1);
                case 0x00C1: /* A with acute */
                        printf("\\\'{A}");
                        return(1);
                case 0x00CD: /* I with acute */
                        printf("\\\'{I}");
                        return(1);
                case 0x00DA: /* U with acute */
                        printf("\\\'{U}");
                        return(1);
                case 0x00D3: /* O with acute */
                        printf("\\\'{O}");
                        return(1);
                case 0x00E7: /* c with cedilla */
                        printf("\\c{c}");
                        return(1);
                case 0x00C7: /* C with cedilla */
                        printf("\\c{C}");
                        return(1);
                case 0x00D1: /* N with tilde */
                        printf("\\~{N}");
                        return(1);
                case 0x00A1: /* inverted exclamation mark */
                        printf("!`");
                        return(1);
                case 0x00BF: /* inverted question mark */
                        printf("?`");
                        return(1);


	/* Windows specials (MV 4.7.2000). More could be added. 
	See http://www.hut.fi/u/jkorpela/www/windows-chars.html
	*/

		case 0x2014:
			printf("---"); /* em-dash */
			return(1);
		case 0x2018:
			printf("{`}");  /* left single quote, Win */
			return(1);
		case 0x2019:
			printf("'");  /* Right single quote, Win */
			return(1);
		case 0x201A:
			printf("\\quotesinglbase{}");  /* single low 99 quotation mark */
			return(1);
		case 0x201C:
			printf("{``}");  /* inverted double quotation mark */
			return(1);
		case 0x201D:
			printf("''");  /* double q.m. */
			return(1);
		case 0x201E:
			printf("\\quotedblbase{}");  /* double low 99 quotation mark */
			return(1);
		case 0x2020:
			printf("\\dag{}");  /* dagger */
			return(1);
		case 0x2021:
			printf("\\ddag{}");  /* double dagger */
			return(1);
		case 0x2022:
			printf("$\\bullet$");  /* bullet */
			return(1);
		case 0x2023:
			printf("$\\bullet$");  /* NOTE: Not a real triangular bullet */
			return(1);

		case 0x2024:
			printf(".");  /* One dot leader (for use in TOCs) */
			return(1);
		case 0x2025:
			printf("..");  /* Two dot leader (for use in TOCs) */
			return(1);
		case 0x2026:
			printf("\\ldots{}"); /* ellipsis */
			return(1);

		case 0x2039:
			printf("\\guilsinglleft{}");  /* single left angle quotation mark */
			return(1);
		case 0x203A:
			printf("\\guilsinglright{}"); /* single right angle quotation mark */
			return(1);

		case 0x203C:
			printf("!!"); /* double exclamation mark */
			return(1);

		case 0x2215:
			printf("$/$");  /* Division slash */
			return(1);

		case 0x2030:
			printf("o/oo");
			return(1);

		case 0x20ac:
			printf("\\euro");
                        /* No known implementation ;-)

			TODO
                        Shouldn't we use the package 'eurofont'?
                        -- 2000-08-15 huftis@bigfoot.com 
                        */
			return(1);

		case 0x2160:
			printf("I"); /* Roman numeral I */
			return(1);
		case 0x2161:
			printf("II"); /* Roman numeral II */
			return(1);
		case 0x2162:
			printf("III"); /* Roman numeral III */
			return(1);
		case 0x2163:
			printf("IV"); /* Roman numeral IV */
			return(1);
		case 0x2164:
			printf("V"); /* Roman numeral V */
			return(1);
		case 0x2165:
			printf("VI"); /* Roman numeral VI */
			return(1);
		case 0x2166:
			printf("VII"); /* Roman numeral VII */
			return(1);
		case 0x2167:
			printf("VIII"); /* Roman numeral VIII */
			return(1);
		case 0x2168:
			printf("IX"); /* Roman numeral IX */
			return(1);
		case 0x2169:
			printf("X"); /* Roman numeral X */
			return(1);
		case 0x216A:
			printf("XI"); /* Roman numeral XI */
			return(1);
		case 0x216B:
			printf("XII"); /* Roman numeral XII */
			return(1);
		case 0x216C:
			printf("L"); /* Roman numeral L */
			return(1);
		case 0x216D:
			printf("C"); /* Roman numeral C */
			return(1);
		case 0x216E:
			printf("D"); /* Roman numeral D */
			return(1);
		case 0x216F:
			printf("M"); /* Roman numeral M */
			return(1);
		case 0x2170:
			printf("i"); /* Roman numeral i */
			return(1);
		case 0x2171:
			printf("ii"); /* Roman numeral ii */
			return(1);
		case 0x2172:
			printf("iii"); /* Roman numeral iii */
			return(1);
		case 0x2173:
			printf("iv"); /* Roman numeral iv */
			return(1);
		case 0x2174:
			printf("v"); /* Roman numeral v */
			return(1);
		case 0x2175:
			printf("vi"); /* Roman numeral vi */
			return(1);
		case 0x2176:
			printf("vii"); /* Roman numeral vii */
			return(1);
		case 0x2177:
			printf("viii"); /* Roman numeral viii */
			return(1);
		case 0x2178:
			printf("ix"); /* Roman numeral ix */
			return(1);
		case 0x2179:
			printf("x"); /* Roman numeral x */
			return(1);
		case 0x217A:
			printf("xi"); /* Roman numeral xi */
			return(1);
		case 0x217B:
			printf("xiii"); /* Roman numeral xii */
			return(1);
		case 0x217C:
			printf("l"); /* Roman numeral l */
			return(1);
		case 0x217D:
			printf("c"); /* Roman numeral c */
			return(1);
		case 0x217E:
			printf("d"); /* Roman numeral d */
			return(1);
		case 0x217F:
			printf("m"); /* Roman numeral m */
			return(1);

		}
	/* Debugging aid: */
	return(0);
	}
#undef printf
