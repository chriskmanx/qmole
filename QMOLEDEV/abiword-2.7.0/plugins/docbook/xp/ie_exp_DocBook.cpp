/* AbiWord
 * Copyright (C) 1998 AbiSource, Inc.
 * Copyright (C) 2000-2002 Dom Lachowicz 
 * Copyright (C) 2002 Screetch
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

#include "ut_string.h"
#include "ut_bytebuf.h"
#include "ut_base64.h"
#include "ut_locale.h"
#include "ut_units.h"
#include "pt_Types.h"
#include "pd_Document.h"
#include "ie_impexp_DocBook.h"
#include "ie_exp_DocBook.h"
#include "pp_AttrProp.h"
#include "px_ChangeRecord.h"
#include "px_CR_Object.h"
#include "px_CR_Span.h"
#include "px_CR_Strux.h"
#include "xap_App.h"
#include "xap_EncodingManager.h"
#include "ap_Strings.h"
#include "fd_Field.h"

#include "ut_path.h"
#include "ut_string_class.h"

/*****************************************************************/
/*****************************************************************/

IE_Exp_DocBook_Sniffer::IE_Exp_DocBook_Sniffer (const char * _name) :
  IE_ExpSniffer(_name)
{
  // 
}

bool IE_Exp_DocBook_Sniffer::recognizeSuffix(const char * szSuffix)
{
	return (!g_ascii_strcasecmp(szSuffix,".dbk") ||!g_ascii_strcasecmp(szSuffix,".xml"));
}

UT_Error IE_Exp_DocBook_Sniffer::constructExporter(PD_Document * pDocument,
						   IE_Exp ** ppie)
{
	IE_Exp_DocBook * p = new IE_Exp_DocBook(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Exp_DocBook_Sniffer::getDlgLabels(const char ** pszDesc,
											const char ** pszSuffixList,
											IEFileType * ft)
{
	*pszDesc = "DocBook (.dbk, .xml)";
	*pszSuffixList = "*.dbk; *.xml";
	*ft = getFileType();
	return true;
}

/*****************************************************************/
/*****************************************************************/

IE_Exp_DocBook::IE_Exp_DocBook(PD_Document * pDocument)
	: IE_Exp(pDocument)
{
	m_error = 0;
	s_align = 0;
	m_pListener = NULL;
}

IE_Exp_DocBook::~IE_Exp_DocBook()
{
}

int IE_Exp_DocBook :: indent (void)
{
	return ++s_align;
}

int IE_Exp_DocBook :: unindent (void)
{
	if (--s_align < 0)
	{
		s_align = 0;
		UT_DEBUGMSG(("DocBook: trying to unindent at min indent.\n"));
	}
	return s_align;
}

void IE_Exp_DocBook :: iwrite (const char *txt)
{
	if (s_align)
	{
		char *tmpIndent = new char [s_align + 1];
		memset (tmpIndent, '\t', s_align);
		tmpIndent [s_align] = '\0';

		IE_Exp :: write (tmpIndent);
		DELETEPV(tmpIndent);
	}

	IE_Exp :: write (txt);
}

void IE_Exp_DocBook :: writeln (const char *txt)
{
	iwrite (txt);
	IE_Exp :: write ("\n");
}

/*****************************************************************/
/*****************************************************************/

/*!
   removes the suffix from a string by searching backwards for the specified 
   character delimiter. If the delimiter is not found, a copy of the original 
   string is returned
   
   eg. _stripSuffix("/home/user/file.png, '.') returns "/home/user/file" 
	   _stripSuffix("/home/user/foo_bar, '_') returns /home/user/foo 
	   _stripSuffix("/home/user/file.png, '_') returns /home/user/file.png"
   TODO: put this in UT_String somehow, it came from ie_exp_HTML.
*/
static char *_stripSuffix(const char* from, char delimiter)
{
	char * fremove_s = (char *)malloc(strlen(from)+1);
	strcpy(fremove_s, from);   

	char * p = fremove_s + strlen(fremove_s);
	while ((p >= fremove_s) && (*p != delimiter))
		p--;
	
	if (p >= fremove_s)
	*p = '\0';
	
	return fremove_s;
}

static char * _stripSuffix(const UT_UTF8String & from, char delimiter)
{
  return _stripSuffix(from.utf8_str(), delimiter);
}

/*****************************************************************/
/*****************************************************************/

#define BT_NORMAL		1
#define BT_PLAINTEXT	2

bool s_DocBook_Listener::_initFile(void)
{
	// write out the doctype descriptor
	m_pie->writeln("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
	m_pie->writeln("<!DOCTYPE book PUBLIC \"-//OASIS//DTD DocBook XML V4.2//EN\"");
	m_pie->writeln("\t\"http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd\">\n");
	m_pie->writeln("<!-- ================================================================================ -->");
	m_pie->writeln("<!-- This DocBook file was created by AbiWord.                                        -->");
	m_pie->writeln("<!-- AbiWord is a free, Open Source word processor.                                   -->");
	m_pie->writeln("<!-- You may obtain more information about AbiWord at http://www.abisource.com        -->");
	m_pie->writeln("<!-- ================================================================================ -->");
	m_pie->write("\n");
	_handleDocument();
	_handleMetaData();

	return true;
}

void s_DocBook_Listener::_closeFile(void)
{
	if(m_bInTitle && !m_bInSection) //in case the file contains a lone chapter heading
	{
		_closeChapterTitle();
		_tagOpenClose("section",false);
		_tagOpenClose("para",false);
	}
	_closeChapter();  //handles all of the section closing
	_handleDataItems();
	_tagClose(TT_DOCUMENT,"book");
}

void s_DocBook_Listener::_tagClose(UT_uint32 tagID, const UT_UTF8String & content, bool newline, bool indent, bool decrease)
{
	UT_uint32 i = 0;

	if(decrease)
		m_pie->unindent();

	if(indent)
		m_pie->iwrite("</");
	else
		m_pie->write("</");

	m_pie->write(content.utf8_str());
	m_pie->write(">");

	if(newline)
		m_pie->write("\n");

	m_utnsTagStack.pop((UT_sint32*)&i);
	m_iLastClosed = i;
	xxx_UT_DEBUGMSG(("Popping %d off of stack\n",i));

	if(i != tagID)
	{
		UT_DEBUGMSG(("DocBook export: possible mismatched tag. Requested: %d, Popped: %d\n",tagID,i));
	}
}

void s_DocBook_Listener::_tagOpen(UT_uint32 tagID, const UT_UTF8String & content, bool newline, bool indent, bool increase)
{
	if(indent)
		m_pie->iwrite("<");
	else
		m_pie->write("<");

	m_pie->write(content.utf8_str());
	m_pie->write(">");

	if(newline)
		m_pie->write("\n");

	if(increase)
		m_pie->indent();

	m_utnsTagStack.push(tagID);
	xxx_UT_DEBUGMSG(("Pushing %d onto stack\n",tagID));
}

void s_DocBook_Listener::_tagOpenClose(const UT_UTF8String & content, bool suppress, bool newline, bool indent)
{
	if(indent)
		m_pie->iwrite("<");
	else
		m_pie->write("<");		

	m_pie->write(content.utf8_str());

	if(suppress)
		m_pie->write("/>");
	else
	{
		m_pie->write("></");
		m_pie->write(content.utf8_str());
		m_pie->write(">");
	}

	if(newline)
		m_pie->write("\n");
}

UT_uint32 s_DocBook_Listener::_tagTop(void)
{
	UT_sint32 i = 0;

	if (m_utnsTagStack.viewTop (i))
		return (UT_uint32)i;
	return 0;
}

void s_DocBook_Listener :: _closeSection(int sub)
{
	_closeParagraph();  // this also prevents section titles from being left open, so keep it above the return

	if(_tagTop() == TT_FOOTNOTE)  // triggered by the .doc importer sometimes 
	{
		_tagClose(TT_FOOTNOTE,"footnote",false,false,false);
		m_bInNote = false;
		_closeParagraph();
	}

	if((!m_bInSection) || (sub > m_iSectionDepth) || (m_bInTable))
		return;

	while((sub < m_iSectionDepth) && (m_iSectionDepth > 0))
	{
		if(_tagTop() == TT_TITLE)
			_closeSectionTitle();
		if(m_iLastClosed == TT_TITLE)
			_tagOpenClose("para",false);  //we can't have empty sections

		if(_tagTop() != TT_SECTION)
			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);

		_tagClose(TT_SECTION,"section");
		m_iSectionDepth--;
	}

	if(m_iSectionDepth == 0)
		m_bInSection = false;
	if(m_bInHdrFtr)
		m_bInHdrFtr = false;
	m_sLastStyle = "";
}

void s_DocBook_Listener :: _closeSectionTitle()
{
	if(!m_bInTitle)
		return;

	if(_tagTop() != TT_TITLE)
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);

	_tagClose(TT_TITLE,"title",true,false);
	m_bInTitle = false;
}


void s_DocBook_Listener :: _closeParagraph(void)
{
	if((_tagTop() == TT_FOOTNOTE) || (!m_bInParagraph))
		return;

	_closeSpan();

	if(_tagTop() == TT_LINK)  // don't let links span paragraphs
		_tagClose(TT_LINK,"link",false,false,false);
	else if(_tagTop() == TT_ULINK)
		_tagClose(TT_ULINK,"ulink",false,false,false);

	if((m_iBlockType == BT_PLAINTEXT) || (_tagTop() == TT_PLAINTEXT))
	{
		m_iBlockType = BT_NORMAL;
		_tagClose(TT_PLAINTEXT,"literallayout",true,false,false);
	}
	else if((m_iBlockType == BT_NORMAL) || (_tagTop() == TT_BLOCK))
	{		
		bool deindent = true;
		if(m_bInTable)
			deindent = false;
		else if(m_bInNote)
			deindent = false;

		_tagClose(TT_BLOCK,"para",((!m_bInTable) && (!m_bInNote)),false,deindent);
	}
	else
	{
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
	}

	if(!m_bInNote)
		m_bInParagraph = false;
}

void s_DocBook_Listener :: _closeSpan(void)
{
	if(!m_bInSpan)
		return;

	const PP_AttrProp * pAP = m_pAP_Span;
	
	if (pAP)
	{
		const gchar * szValue = 0;

		if (pAP->getProperty(static_cast<const gchar *>("text-position"), szValue))
		{
			if (!strcmp("superscript", szValue))
			{
				_tagClose(TT_SUPERSCRIPT,"superscript",false,false,false);
			}
			else if (!strcmp("subscript", szValue))
			{
				_tagClose(TT_SUBSCRIPT,"subscript",false,false,false);
			}
		}

		if ((pAP->getProperty(static_cast<const gchar *>("font-style"), szValue)) && !strcmp(szValue, "italic"))
		{
			_tagClose(TT_EMPHASIS,"emphasis",false,false,false);
		}
		
		_tagClose(TT_PHRASE,"phrase",false,false,false);
		m_pAP_Span = NULL;
	}

	m_bInSpan = false;
}

void s_DocBook_Listener :: _closeChapter (void)
{
	if(!m_bInChapter)
		return;

	if(m_bInTable) // bad .doc import can lead to <section>s being closed in <cell>s; try to export valid XML regardless
	{
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
		_closeTable();
	}

	_closeSection(0); //close any open sections
	_tagClose(TT_CHAPTER,"chapter");
	m_bInChapter = false;
}

void s_DocBook_Listener :: _closeChapterTitle (void)
{
	if(!m_bInChapter || !m_bInTitle)
		return;

	if(_tagTop() != TT_TITLE)
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);

	_tagClose(TT_TITLE,"title",true,false);
	m_bInTitle = false;
}

void s_DocBook_Listener :: _openChapter (PT_AttrPropIndex api)
{
	_closeChapter(); // close any open chapters (and sections)

	if(_tagTop() != TT_DOCUMENT)
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);

	_tagOpen(TT_CHAPTER,"chapter");
	m_bInChapter = true;
	_openChapterTitle(api);
}

void s_DocBook_Listener :: _openChapterTitle (PT_AttrPropIndex /*api*/)
{
	if(_tagTop() == TT_CHAPTER)
	{
		_tagOpen(TT_TITLE,"title",false);
		m_bInTitle = true;
	}
	else
	{
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
	}
}

void s_DocBook_Listener :: _openList (PT_AttrPropIndex /*api*/)
{
/*
	if(_tagTop() != TT_ITEMIZEDLIST)
	{
		m_pie->write("\n");
		_tagOpen(TT_ITEMIZEDLIST,"itemizedlist");
	}

	_tagOpen(TT_LISTITEM,"listitem");
*/
}

void s_DocBook_Listener :: _openBlock(bool indent)
{
	if(m_bInTitle)
		return;

	UT_UTF8String buf = "para";

	_closeParagraph();
	_tagOpen(TT_BLOCK,buf,false,indent,indent);
	m_bInParagraph = true;
	m_iBlockType = BT_NORMAL;
}

void s_DocBook_Listener :: _openPlainBlock()
{
	if(m_bInTitle)
		return;

	UT_UTF8String buf = "literallayout";

	_closeParagraph();
	_tagOpen(TT_PLAINTEXT,buf,true,false,false);
	m_bInParagraph = true;
	m_iBlockType = BT_PLAINTEXT;
}

void s_DocBook_Listener :: _openParagraph(PT_AttrPropIndex api)
{
	if((m_iNestedTable == 0) || (m_iNestedTable == 2))
		return;  // we can't write before/after a nested table

	if(m_bInTable && (_tagTop() == TT_ROW)) //no <entry>, can happen on bad .doc import
	{
		_openCell();
	}

	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP), indent = false;
	UT_UTF8String buf("");

	if (bHaveProp && pAP)
	{
		const gchar * szValue = 0;

		if (pAP->getAttribute(static_cast<const gchar *>(PT_STYLE_ATTRIBUTE_NAME), szValue))
		{	
			if((!strcmp(szValue, "Heading 1")) || (!strcmp(szValue, "Numbered Heading 1")))
			{
				// <p style="Heading 1"> ...
				_closeChapterTitle();

				if((!_inSectionStrux()) && !m_bInTitle)
					_openSection(api, 1, szValue);
				else if(!m_bInTitle)
				{
					indent = _decideIndent();
					_openBlock(indent);
				}
				return;
			}
			else if((!strcmp(szValue, "Heading 2")) || (!strcmp(szValue, "Numbered Heading 2")))
			{
				// <p style="Heading 2"> ...
				_closeChapterTitle();

				if((!_inSectionStrux()) && !m_bInTitle)
				{
					_openSection(api, !strcmp(szValue, m_sParentStyle.utf8_str()) ? m_iSectionDepth : 2, szValue);
					m_sParentStyle = szValue;  //don't coalesce runs inside of a title
				}
				else if(!m_bInTitle)
				{
					indent = _decideIndent();
					_openBlock(indent);
					m_sParentStyle = szValue;
				}
				return;
			}
			else if((!strcmp(szValue, "Heading 3")) || (!strcmp(szValue, "Numbered Heading 3")))
			{
				// <p style="Heading 3"> ...
				_closeChapterTitle();

				if(!_inSectionStrux() && !m_bInTitle)
				{
					_openSection(api, !strcmp(szValue, m_sParentStyle.utf8_str()) ? m_iSectionDepth : 3, szValue);
					m_sParentStyle = szValue;
				}
				else
				{
					indent = _decideIndent();
					_openBlock(indent);
					m_sParentStyle = szValue;
				}
				return;
			}
			else if(!strcmp(szValue, "Heading 4")) 
			{
				// <p style="Heading 4"> ...
				_closeChapterTitle();

				if(!_inSectionStrux() && !m_bInTitle)
				{
					_openSection(api, !strcmp(szValue, m_sParentStyle.utf8_str()) ? m_iSectionDepth : 4, szValue);
					m_sParentStyle = szValue;
				}
				else
				{
					indent = _decideIndent();
					_openBlock(indent);
					m_sParentStyle = szValue;
				}
				return;
			}
			else if (!strcmp (szValue, "Chapter Heading"))
			{
				if(!_inSectionStrux() && !m_bInTitle)
				{
					_openChapter(api);
					m_sParentStyle = szValue;
				}
				else
				{
					indent = _decideIndent();
					_openBlock(indent);
					m_sParentStyle = szValue;
				}
				return;
			}
			else if (!strcmp (szValue, "Section Heading"))
			{
				_closeChapterTitle();

				if(!_inSectionStrux() && !m_bInTitle)
				{
					_openSection(api, 1, szValue);
					m_sParentStyle = szValue;
				}
				else
				{
					indent = _decideIndent();
					_openBlock(indent);
					m_sParentStyle = szValue;
				}
				return;
			}
			else if (!strcmp (szValue, "Plain Text"))
			{
				_closeChapterTitle();

				if (!m_iSectionDepth)
				{
					_openSection (api,1, szValue);
					_closeSectionTitle();  // no title
				}

				if(m_iLastClosed == TT_SECTION)
				{
					_openSection(api,m_iSectionDepth,szValue);
					_closeSectionTitle(); //no title
				}

				/* merge all plaintexts into 1 if possible */
				if ((!m_bInParagraph) || (!(m_iBlockType == BT_PLAINTEXT)))
				{
					indent = _decideIndent();
					_openPlainBlock();
				}
				else
					m_pie -> write ("\n");

				m_sParentStyle = szValue;
				return;
			}
			else if (!strcmp (szValue, "Normal"))
			{
				_closeChapterTitle();

				if (!m_iSectionDepth)
					_openSection (api,1, szValue);

				_closeSectionTitle();
				_closeParagraph();

				if(m_iLastClosed == TT_SECTION)
				{
					_openSection(api,m_iSectionDepth,szValue);
					_closeSectionTitle(); //no title
				}

				buf = "para";

				indent = _decideIndent();
				_tagOpen(TT_BLOCK,buf,false,indent,indent); //don't indent in tables
				m_iBlockType = BT_NORMAL;
			}
			else
			{
				/* unhandled style */
				_closeChapterTitle();

				if(!_inSectionStrux() && !m_bInTitle)
				{
					buf = "para";

					if(strcmp (szValue, m_sLastStyle.utf8_str()))  // not a coalescing run
					{
						_openSection(api,m_iSectionDepth,szValue);
						_closeSectionTitle(); //no title
					}

					_closeParagraph();
					indent = _decideIndent();
					_tagOpen(TT_BLOCK,buf,false,indent,indent);
					m_bInParagraph = true;
					m_iBlockType = BT_NORMAL;
					m_sLastStyle = szValue;
				}
				else
				{
					indent = _decideIndent();
					_openBlock(indent);
				}
				return;
			}
		}
		else 
		{
			// <p> with no style attribute ...
			if (!m_iSectionDepth)
				_openSection (api,1, "");

			_closeSectionTitle();
			_closeParagraph();
			m_iBlockType = BT_NORMAL;

			if(m_iLastClosed == TT_SECTION)
			{
				_openSection(api,m_iSectionDepth,szValue);
				_closeSectionTitle();
			}

			buf = "para";
			indent = _decideIndent();
			_tagOpen(TT_BLOCK,buf,false,indent,indent); //don't indent in table
		}
	}
	else 
	{
		// <p> with no style attribute, and no properties either
		if (!m_iSectionDepth)
			_openSection (api,1,"");

		_closeSectionTitle();
		_closeParagraph ();
		m_iBlockType = BT_NORMAL;

		if(m_iLastClosed == TT_SECTION)
		{
			_openSection(api,m_iSectionDepth,"");
			_closeSectionTitle();
		}

		indent = _decideIndent();
		_tagOpen(TT_BLOCK,"para",false,indent,indent); //don't indent in table
	}
	m_bInParagraph = true;
}

void s_DocBook_Listener :: _openSection (PT_AttrPropIndex api, int sub, const UT_UTF8String & content)
{
	if((m_bInTable) || (m_bInFrame) || (m_bInHdrFtr))
		return;

	if(!m_bInChapter)
		_openChapter(api);

	if(!m_bInSection)
		_closeChapterTitle();

	_closeSection(sub - 1);

	if(_tagTop() == TT_TITLE)
		_closeSectionTitle();

	UT_UTF8String section = "section", escaped = "";
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	const gchar * szValue = 0;

	if(content.length())
	{
		escaped = content;
		escaped.escapeXML();
		section += " role=\"";
		section += escaped;
		section += "\"";
	}

	_tagOpen(TT_SECTION,section);
	m_iSectionDepth++;
	m_bInSection = true;
	_openSectionTitle();

	if(pAP && bHaveProp && (pAP->getAttribute("strux-image-dataid", szValue)))
	{
		_closeSectionTitle(); // no title
		_handlePositionedImage(api);
	}
}

void s_DocBook_Listener :: _openSectionTitle(void)
{
	if((_tagTop() != TT_SECTION) || m_bInTitle)
		return;

	_tagOpen(TT_TITLE,"title",false);
	m_bInTitle = true;
}

void s_DocBook_Listener :: _openSpan(PT_AttrPropIndex api)
{
	if ((!m_bInParagraph && !m_bInTitle))
		return;

	if(m_bInSpan)
		_closeSpan();
	
	UT_UTF8String buf = "phrase";
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	
	if (bHaveProp && pAP)
	{
		const gchar * szValue = 0;

		if ((pAP->getAttribute(static_cast<const gchar *>("revision"), szValue)))
		{
			buf += " revision=\"";
			buf += szValue;
			buf += "\"";
		}
		if ((pAP->getProperty(static_cast<const gchar *>("lang"), szValue)))
		{
			buf += " lang=\"";
			buf += szValue;
			buf += "\"";
		}
		if ((pAP->getProperty(static_cast<const gchar *>("font-weight"), szValue)) && !strcmp(szValue, "bold"))
		{
			buf += " role=\"strong\"";
		}

		_tagOpen(TT_PHRASE,buf,false,false,false);

		if ((pAP->getProperty(static_cast<const gchar *>("font-style"), szValue)) && !strcmp(szValue, "italic"))
		{
			_tagOpen(TT_EMPHASIS,"emphasis",false,false,false);
		}
		if (pAP->getProperty(static_cast<const gchar *>("text-position"), szValue))
		{
			if (!strcmp("superscript", szValue))
			{
				_tagOpen(TT_SUPERSCRIPT,"superscript",false,false,false);
			}
			else if (!strcmp("subscript", szValue))
			{
				_tagOpen(TT_SUBSCRIPT,"subscript",false,false,false);
			}
		}
		m_bInSpan = true;
		m_pAP_Span = pAP;
	}
}

void s_DocBook_Listener::_outputData(const UT_UCSChar * data, UT_uint32 length)
{
	if ((!m_bInParagraph) && (!m_bInTitle))
	{
		return;
	}
	m_bWasSpace = false;
	
	UT_UTF8String sBuf = "";
	const UT_UCSChar * pData;

	UT_ASSERT(sizeof(UT_Byte) == sizeof(char));

	for (pData=data; (pData<data+length); /**/)
	{
		switch (*pData)
		{
		case UCS_FF: // page break
			if ((!m_bInTitle) && (m_bInParagraph)) // we don't put tags in <title>
			{
				if(_inFormattedSpan())
					_closeSpan();
				sBuf += "<beginpage/>";
			}
			else
				UT_ASSERT_HARMLESS(UT_TODO);

			pData++;
			break;
		case '<':
			sBuf += "&lt;";
			pData++;
			break;
			
		case '>':
			sBuf += "&gt;";
			pData++;
			break;
			
		case '&':
			sBuf += "&amp;";
			pData++;
			break;

		case UCS_VTAB:					// column break
		case UCS_LF:					// LF -- representing a Forced-Line-Break
			if (m_iBlockType == BT_PLAINTEXT)
				sBuf += "\n";
			else
				UT_ASSERT_HARMLESS(UT_TODO); // <br/> isn't valid in docbook
			pData++;
			break;

		case ' ':
		  // try to honor multiple spaces
		  // except in PLAINTEXT
		  if (m_iBlockType != BT_PLAINTEXT)
		  {
		  	if(m_bWasSpace)
				{
					// not defined in dbk, nor in abw...
//					sBuf += "&nbsp;";
					pData++;
				}
			  else
				{
				  // just tack on a single space to the textrun
				  m_bWasSpace = true;
				  sBuf += " ";
				  pData++;
				}
			  break;
		  }
		  else
		  {
			  // plain text: allowing multiple spaces and so
			  sBuf.appendUCS4(pData,1);
			  pData++;
			  break;
		  }
		case UCS_TAB:
		  // try to honor multiple spaces
		  // except in PLAINTEXT
		  if (m_iBlockType != BT_PLAINTEXT)
		  {
		  	if(m_bWasSpace)
				{
					// not defined in dbk, nor in abw...
//					sBuf += "&nbsp;";
					pData++;
				}
			  else
				{
				  // just tack on a single space to the textrun
				  m_bWasSpace = true;
				  sBuf += "\t";
				  pData++;
				}
			  break;
		  }
		  else
		  {
			  // plain text: allowing multiple spaces and so
			  sBuf.appendUCS4(pData,1);
			  pData++;
			  break;
		  }

		default:

		  // reset this variable
			m_bWasSpace = false;
			if(*pData < 0x20)  //invalid xml chars
				pData++;
			else
			{
				sBuf.appendUCS4(pData, 1);
				pData++;
			}
			break;
		}
	}

	m_pie->write(sBuf.utf8_str(),sBuf.byteLength());
}

s_DocBook_Listener::s_DocBook_Listener(PD_Document * pDocument,
				       IE_Exp_DocBook * pie)
 : mTableHelper(pDocument)
{
	m_pDocument = pDocument;
	m_pie = pie;
	m_bInParagraph = false;
	m_bInSection = false;
	m_bInSpan = false;
	m_bInChapter = false;
	m_bInTable = false;
	m_bInTitle = false;
	m_bInFrame = false;
	m_bInHdrFtr = false;
	m_bInNote = false;
	m_iBlockType = 0;
	m_iNestedTable = -1;
	m_iTableDepth = 0;
	m_iListDepth = 0;
	m_iPreviousListDepth = 0;
	m_iSectionDepth = 0;
	m_iLastClosed = 0;
	m_sLastStyle = "";  //only used for non-handled styles
	m_sParentStyle = "";
	m_bExternal = false;
}

s_DocBook_Listener::~s_DocBook_Listener()
{
	UT_VECTOR_FREEALL(char *, m_utvDataIDs);
}

bool s_DocBook_Listener::populate(PL_StruxFmtHandle /*sfh*/,
								   const PX_ChangeRecord * pcr)
{
	switch (pcr -> getType ())
	{
		case PX_ChangeRecord :: PXT_InsertSpan:
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
			const PX_ChangeRecord_Object * pcro = static_cast<const PX_ChangeRecord_Object *> (pcr);
			PT_AttrPropIndex api = pcr->getIndexAP();

			switch (pcro->getObjectType())
			{
			case PTO_Image:
				_handleImage(api);
				return true;

			case PTO_Math:
				_handleMath(api);
				return true;

			case PTO_Embed:
				_handleEmbedded(api);
				return true;

			case PTO_Field:
				_handleField(pcro, api);
				return true;

			case PTO_Hyperlink:
				_handleHyperlink(api);
				return true;
			
			case PTO_Bookmark:
				_handleBookmark(api);
				return true;

			default:
				UT_ASSERT_HARMLESS(UT_TODO);
				return true;
			}
			return false;
		}

		default:
			return true;
	}
}

/* http://www.docbook.org/tdg/en/html/table.html */

void s_DocBook_Listener::_openTable(PT_AttrPropIndex api)
{
	if(m_bInTitle)
		_closeSectionTitle();

	if(m_bInTable)
	{
		_openNestedTable();
		return;
	}

	UT_UTF8String buf("");
	UT_sint32 nCols = mTableHelper.getNumCols();

	if(!m_bInSection) //tables as first elements
	{
		_openSection(api,1,"");
		_closeSectionTitle(); //no title in this instance, so just close it
	}

	if(m_iLastClosed == TT_SECTION)
	{
		_openSection(api,m_iSectionDepth,"");
		_closeSectionTitle(); //no title
	}

	buf = "informaltable frame=\"all\"";

	_tagOpen(TT_TABLE,buf);

	UT_UTF8String tgroup(UT_UTF8String_sprintf("tgroup cols='%d' align='left' colsep='1' rowsep='1'", nCols));
	_tagOpen(TT_TGROUP,tgroup,true,true,false);

	for (int i = 0; i < nCols; i++)
	{
		UT_UTF8String colspec (UT_UTF8String_sprintf("colspec colname='c%d'", i+1));
		_tagOpenClose(colspec,true);
	}

	_tagOpen(TT_TBODY,"tbody");
	m_bInTable = true;
}

void s_DocBook_Listener::_openNestedTable()
{
	if(m_iNestedTable != 0) //docbook only allows one level of nesting
		return;

	if(_tagTop() != TT_ROW)
		_openRow();

	UT_sint32 nCols = mTableHelper.getNumCols();

	UT_UTF8String entrytbl(UT_UTF8String_sprintf("entrytbl cols='%d' align='left' colsep='1' rowsep='1'", nCols));

	_tagOpen(TT_ENTRYTBL,entrytbl);
	_tagOpen(TT_TBODY,"tbody");
	m_iNestedTable = 1;
}

void s_DocBook_Listener::_openCell()
{
	UT_sint32 rowspan = 1, colspan = 1;
	UT_UTF8String entry ("entry");
  
	rowspan = mTableHelper.getBot() - mTableHelper.getTop();
	colspan = mTableHelper.getRight() - mTableHelper.getLeft();
  
	_openRow();

	if (rowspan > 1)
		entry += UT_UTF8String_sprintf(" morerows='%d'", rowspan-1);
	if (colspan > 1)
		entry += UT_UTF8String_sprintf(" namest='c%d' nameend='c%d'", mTableHelper.getLeft()+1, mTableHelper.getRight());

	_tagOpen(TT_ENTRY,entry,false,true,true);
}

void s_DocBook_Listener::_openRow(void)
{
	if (mTableHelper.isNewRow())
	{
		_closeCell();
		_closeRow();
		_tagOpen(TT_ROW,"row");
	}
}

void s_DocBook_Listener::_closeTable(void)
{
	if(!m_bInTable)
		return;

	if(m_iNestedTable == 1)
	{
		_closeNestedTable();
		return;
	}

	_closeCell();
	_closeRow();
	_tagClose(TT_TBODY,"tbody");
	_tagClose(TT_TGROUP,"tgroup",true,true,false); //keep tgroup and tbody at the same indent level
	_tagClose(TT_TABLE,"informaltable");
	m_bInTable = false;
}

void s_DocBook_Listener::_closeNestedTable(void)
{
	if(m_iNestedTable != 1)
		return;

	_closeCell();
	_closeRow();
	_tagClose(TT_TBODY,"tbody");
	_tagClose(TT_ENTRYTBL,"entrytbl");
	m_iNestedTable = 2;
}

void s_DocBook_Listener::_closeCell(void)
{
	_closeParagraph();

	if(_tagTop() == TT_ENTRY)
	{
		_tagClose(TT_ENTRY,"entry",true,false,true);
	}

}

void s_DocBook_Listener::_closeRow(void)
{
	_closeCell();

	if (_tagTop() == TT_ROW)
	{
		_tagClose(TT_ROW,"row");
	}
}

void s_DocBook_Listener::_handleDocument(void)
{
	UT_UTF8String buf("book");
	PT_AttrPropIndex docApi = m_pDocument->getAttrPropIndex();
	const PP_AttrProp * pDAP = NULL;
	const gchar* szValue = 0;

	m_pDocument->getAttrProp (docApi, &pDAP);

	if(pDAP && pDAP->getProperty("lang", szValue))
	{
		buf += " lang=\"";
		buf += szValue;
		buf+= "\"";
	}

	_tagOpen(TT_DOCUMENT,buf);
}

void s_DocBook_Listener::_handleMetaData(void)
{
	UT_UTF8String metaProp(""), escaped("");

	_tagOpen(TT_BOOKINFO,"bookinfo");
	_tagOpen(TT_BIBLIOMISC,"bibliomisc",false,true,false);
	_tagOpen(TT_APPLICATION,"application class=\"software\"",false,false,false);
	m_pie->write("AbiWord");
	_tagClose(TT_APPLICATION,"application",false,false,false);
	_tagClose(TT_BIBLIOMISC,"bibliomisc",true,false,false);

	if (m_pDocument->getMetaDataProp (PD_META_KEY_TITLE, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_TITLE,"title",false,true,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_TITLE,"title",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_CREATOR, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_AUTHOR,"author",false,true,false);
		_tagOpen(TT_OTHERNAME,"othername role=\"full\"",false,false,false);
		m_pie->write(metaProp.utf8_str());
		_tagClose(TT_OTHERNAME,"othername",false,false,false);
		_tagClose(TT_AUTHOR,"author",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_SUBJECT, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_SUBJECTSET,"subjectset",false,true,false);
		_tagOpen(TT_SUBJECT,"subject",false,false,false);
		_tagOpen(TT_SUBJECTTERM,"subjectterm",false,false,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_SUBJECTTERM,"subjectterm",false,false,false);
		_tagClose(TT_SUBJECT,"subject",false,false,false);
		_tagClose(TT_SUBJECTSET,"subjectset",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_DESCRIPTION, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_ABSTRACT,"abstract",false,true,false);
		_tagOpen(TT_BLOCK,"para",false,false,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_BLOCK,"para",false,false,false);
		_tagClose(TT_ABSTRACT,"abstract",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_PUBLISHER, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_PUBLISHER,"publisher",false,true,false);
		_tagOpen(TT_PUBLISHERNAME,"publishername",false,false,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_PUBLISHERNAME,"publishername",false,false,false);
		_tagClose(TT_PUBLISHER,"publisher",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_CONTRIBUTOR, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_COLLAB,"collab",false,true,false);
		_tagOpen(TT_COLLABNAME,"collabname",false,false,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_COLLABNAME,"collabname",false,false,false);
		_tagClose(TT_COLLAB,"collab",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_DATE, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_DATE,"date",false,true,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_DATE,"date",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_SOURCE, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_BIBLIOSOURCE,"bibliosource",false,true,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_BIBLIOSOURCE,"bibliosource",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_RELATION, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_BIBLIORELATION,"bibliorelation",false,true,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_BIBLIORELATION,"bibliorelation",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_COVERAGE, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_BIBLIOCOVERAGE,"bibliocoverage",false,true,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_BIBLIOCOVERAGE,"bibliocoverage",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_RIGHTS, metaProp) && metaProp.size())
	{
		escaped = metaProp.escapeXML();
		_tagOpen(TT_LEGALNOTICE,"legalnotice",false,true,false);
		_tagOpen(TT_BLOCK,"para",false,false,false);
		m_pie->write(escaped.utf8_str());
		_tagClose(TT_BLOCK,"para",false,false,false);
		_tagClose(TT_LEGALNOTICE,"legalnotice",true,false,false);
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_KEYWORDS, metaProp) && metaProp.size())
	{
		UT_UTF8String buf = "";
		UT_UCS4String keyword = metaProp.utf8_str();

		for(UT_uint32 i = 0;i < keyword.length(); i++)
		{
			if(keyword[i] != ' ')
			{
				buf += keyword[i];
			}
			else
			{
				if(buf.empty())  //only blank space encountered
				{
					continue;
				}
				if(_tagTop() == TT_BOOKINFO)
					_tagOpen(TT_KEYWORDSET,"keywordset");			

				_tagOpen(TT_KEYWORD,"keyword",false,true,false);
				buf.escapeXML();
				m_pie->write(buf.utf8_str());
				_tagClose(TT_KEYWORD,"keyword",true,false,false);
				buf.clear();
			}
		}

		if(buf.length())
		{
			if(_tagTop() == TT_BOOKINFO)
				_tagOpen(TT_KEYWORDSET,"keywordset");

			_tagOpen(TT_KEYWORD,"keyword",false,true,false);
			buf.escapeXML();
			m_pie->write(buf.utf8_str());
			_tagClose(TT_KEYWORD,"keyword",true,false,false);
		}

		if(_tagTop() == TT_KEYWORDSET)
			_tagClose(TT_KEYWORDSET,"keywordset");
	}
	if (m_pDocument->getMetaDataProp (PD_META_KEY_DATE_LAST_CHANGED, metaProp) && metaProp.size())
	{
		UT_ASSERT_HARMLESS(UT_TODO);
	}

	_handleRevisions();
	_tagClose(TT_BOOKINFO,"bookinfo");
}

void s_DocBook_Listener::_handleRevisions(void)
{
	const AD_Revision * pRev = NULL;
	const UT_GenericVector<AD_Revision*> & vRevisions = m_pDocument->getRevisions();

	UT_sint32 k = 0;
	for (k=0; k < vRevisions.getItemCount(); k++)
	{
		if(k == 0)
			_tagOpen(TT_REVHISTORY,"revhistory");

		pRev = vRevisions.getNthItem(k);
		if(!pRev)
			continue;

		UT_UTF8String s;
		UT_UCS4String s4;

		UT_UTF8String_sprintf(s, "%d", pRev->getId());
		_tagOpen(TT_REVISION,"revision");
		_tagOpen(TT_REVNUMBER,"revnumber",false);
		m_pie->write(s.utf8_str());
		_tagClose(TT_REVNUMBER,"revnumber",true,false);
		s.clear();

		UT_UTF8String_sprintf(s, "%d", pRev->getStartTime());
		_tagOpen(TT_DATE,"date",false);
		m_pie->write(s.utf8_str());
		_tagClose(TT_DATE,"date",true,false);
		s4 = pRev->getDescription();

		if(s4.length())
		{
			_tagOpen(TT_REVREMARK,"revremark",false);
			s.clear();
			s = s4.utf8_str();
			s.escapeXML();
			m_pie->write(s.utf8_str());
			_tagClose(TT_REVREMARK,"revremark",true,false);
		}
		_tagClose(TT_REVISION,"revision");
	}

	if(_tagTop() == TT_REVHISTORY)
		_tagClose(TT_REVHISTORY,"revhistory");
}

void s_DocBook_Listener::_handleImage(PT_AttrPropIndex api)
{
	UT_UTF8String buf(""), escaped("");
	const gchar* szValue = 0;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	UT_LocaleTransactor t(LC_NUMERIC, "C");

	if(!m_bInSection) // an image might be in a chapter heading
	{
		_closeChapterTitle();
		_openSection(api,1,"");
	}
	if(!m_bInParagraph) // an image might also be in a section heading
	{
		_closeSectionTitle();
		_openBlock(true);
	}

	if(bHaveProp && pAP && pAP->getAttribute("dataid", szValue))
	{
		char* dataid = strdup(const_cast<char*>(szValue));
		char * temp = _stripSuffix(UT_go_basename(szValue), '_');
		char * fstripped = _stripSuffix(temp, '.');
		UT_UTF8String_sprintf(buf, "%s.png", fstripped);
		m_utvDataIDs.push_back(dataid);

		FREEP(temp);
		FREEP(fstripped);

		_tagOpen(TT_FIGURE,"figure",false,false,false);
		_tagOpen(TT_TITLE,"title",false,false,false);

		if(pAP->getAttribute("title", szValue))  //use the image's title instead of its file name, if it exists
		{
			escaped = szValue;
			escaped.escapeXML();
			m_pie->write(escaped.utf8_str());
		}
		else  //fall back to the filename
		{
			escaped = buf.escapeXML();
			m_pie->write(escaped.utf8_str());
		}

		_tagClose(TT_TITLE,"title",false,false,false);
		_tagOpen(TT_MEDIAOBJECT,"mediaobject",false,false,false);
		_tagOpen(TT_IMAGEOBJECT,"imageobject",false,false,false);

		escaped.clear();
		escaped = "imagedata fileref=\"";
		escaped += UT_go_basename(m_pie->getFileName());
		escaped += "_data/";
		escaped += buf.escapeXML();
		escaped += "\" format=\"PNG\"";

		if(pAP->getProperty("height", szValue))
		{
			escaped += " depth=\"";
			escaped += szValue;
			escaped += "\"";
		}
		if(pAP->getProperty("width", szValue))
		{
			escaped += " width=\"";
			escaped += szValue;
			escaped += "\"";
		}

		_tagOpenClose(escaped,true,false,false);
		_tagClose(TT_IMAGEOBJECT,"imageobject",false,false,false);

		if(pAP->getAttribute("alt", szValue))  //use the image's alt
		{
			buf.clear();
			buf = szValue;
			buf.escapeXML();
			_tagOpen(TT_TEXTOBJECT,"textobject",false,false,false);
			_tagOpen(TT_BLOCK,"para",false,false,false);
			m_pie->write(buf.utf8_str());
			_tagClose(TT_BLOCK,"para",false,false,false);
			_tagClose(TT_TEXTOBJECT,"textobject",false,false,false);
		}
		_tagClose(TT_MEDIAOBJECT,"mediaobject",false,false,false);
		_tagClose(TT_FIGURE,"figure",false,false,false);
	}
}

void s_DocBook_Listener::_handlePositionedImage(PT_AttrPropIndex api)
{
	UT_UTF8String buf(""), escaped("");
	const gchar* szValue = 0;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	UT_LocaleTransactor t(LC_NUMERIC, "C");

	if(bHaveProp && pAP && pAP->getAttribute("strux-image-dataid", szValue))
	{
		char* dataid = strdup(const_cast<char*>(szValue));
		char * temp = _stripSuffix(UT_go_basename(szValue), '_');
		char * fstripped = _stripSuffix(temp, '.');
		UT_UTF8String_sprintf(buf, "%s.png", fstripped);
		m_utvDataIDs.push_back(dataid);

		FREEP(temp);
		FREEP(fstripped);

		_tagOpen(TT_FIGURE,"figure",false,true,false);
		_tagOpen(TT_TITLE,"title",false,false,false);

		if(pAP->getAttribute("title", szValue))  //use the image's title instead of its file name, if it exists
		{
			escaped = szValue;
			escaped.escapeXML();
			m_pie->write(escaped.utf8_str());
		}
		else  //fall back to the filename
		{
			escaped = buf.escapeXML();
			m_pie->write(escaped.utf8_str());
		}

		_tagClose(TT_TITLE,"title",false,false,false);
		_tagOpen(TT_MEDIAOBJECT,"mediaobject",false,false,false);
		_tagOpen(TT_IMAGEOBJECT,"imageobject",false,false,false);

		escaped.clear();
		escaped = "imagedata fileref=\"";
		escaped += UT_go_basename(m_pie->getFileName());
		escaped += "_data/";
		escaped += buf.escapeXML();
		escaped += "\" format=\"PNG\"";

		if(pAP->getProperty("frame-height", szValue))
		{
			escaped += " depth=\"";
			escaped += szValue;
			escaped += "\"";
		}
		if(pAP->getProperty("frame-width", szValue))
		{
			escaped += " width=\"";
			escaped += szValue;
			escaped += "\"";
		}

		_tagOpenClose(escaped,true,false,false);
		_tagClose(TT_IMAGEOBJECT,"imageobject",false,false,false);

		if(pAP->getAttribute("alt", szValue))  //use the image's alt
		{
			buf.clear();
			buf = szValue;
			buf.escapeXML();
			_tagOpen(TT_TEXTOBJECT,"textobject",false,false,false);
			_tagOpen(TT_BLOCK,"para",false,false,false);
			m_pie->write(buf.utf8_str());
			_tagClose(TT_BLOCK,"para",false,false,false);
			_tagClose(TT_TEXTOBJECT,"textobject",false,false,false);
		}
		_tagClose(TT_MEDIAOBJECT,"mediaobject",false,false,false);
		_tagClose(TT_FIGURE,"figure",true,false,false);
	}
}

void s_DocBook_Listener::_handleMath(PT_AttrPropIndex api)
{
	UT_UTF8String buf(""), escaped("");
	const gchar* szValue = 0;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	UT_LocaleTransactor t(LC_NUMERIC, "C");

	if(!m_bInSection) // an equation might be in a chapter heading
	{
		_closeChapterTitle();
		_openSection(api,1,"");
	}
	if(!m_bInParagraph) // an equation might also be in a section heading
	{
		_closeSectionTitle();
		_openBlock(true);
	}

	if(bHaveProp && pAP && pAP->getAttribute("dataid", szValue))
	{
		buf = "snapshot-png-";
		buf += szValue;
		char* dataid = strdup(buf.utf8_str());
		m_utvDataIDs.push_back(dataid);
		buf += ".png";

		_tagOpen(TT_INLINEEQUATION,"inlineequation",false,false,false);

		escaped = "graphic fileref=\"";
		escaped += UT_go_basename(m_pie->getFileName());
		escaped += "_data/";
		escaped += buf.escapeXML();
		escaped += "\" format=\"PNG\"";

		if(pAP->getProperty("height", szValue))
		{
			double dInch = static_cast<double>(atoi(szValue))/UT_LAYOUT_RESOLUTION;
			buf.clear();
			UT_UTF8String_sprintf(buf,"%fin",dInch);
			escaped += " depth=\"";
			escaped += buf;
			escaped += "\"";
		}
		if(pAP->getProperty("width", szValue))
		{
			double dInch = static_cast<double>(atoi(szValue))/UT_LAYOUT_RESOLUTION;
			buf.clear();
			UT_UTF8String_sprintf(buf,"%fin",dInch);
			escaped += " width=\"";
			escaped += buf;
			escaped += "\"";
		}
		if(pAP->getProperty("lang", szValue))
		{
			escaped += " lang=\"";
			escaped += szValue;
			escaped += "\"";
		}

		_tagOpenClose(escaped,true,false,false);

/*  TODO: save mathml somehow

		if(pAP->getAttribute("latexid", szValue))
		{
			escaped.clear();
			escaped = szValue;
			escaped.escapeXML();
			_tagOpen(TT_MATHPHRASE,"mathphrase",false);
			m_pie->write(escaped.utf8_str());	//TODO: investigate mml support in docbook instead
			_tagClose(TT_MATHPHRASE,"mathphrase");
		}
*/
		_tagClose(TT_INLINEEQUATION,"inlineequation", false, false, false);
	}
}

void s_DocBook_Listener::_handleEmbedded(PT_AttrPropIndex api)
{
	UT_UTF8String buf(""), escaped("");
	const gchar* szValue = 0;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	UT_LocaleTransactor t(LC_NUMERIC, "C");

	if(!m_bInSection) // a chart might be in a chapter heading
	{
		_closeChapterTitle();
		_openSection(api,1,"");
	}
	if(!m_bInParagraph) // a chart might also be in a section heading
	{
		_closeSectionTitle();
		_openBlock(true);
	}

	if(bHaveProp && pAP && pAP->getAttribute("dataid", szValue))
	{
		buf = "snapshot-png-";
		buf += szValue;
		char* dataid = strdup(buf.utf8_str());
		m_utvDataIDs.push_back(dataid);
		buf += ".png";

		_tagOpen(TT_INFORMALFIGURE,"informalfigure",false,false,false);
		_tagOpen(TT_MEDIAOBJECT,"mediaobject",false,false,false);
		_tagOpen(TT_IMAGEOBJECT,"imageobject",false,false,false);

		escaped = "imagedata fileref=\"";
		escaped += UT_go_basename(m_pie->getFileName());
		escaped += "_data/";
		escaped += buf.escapeXML();
		escaped += "\" format=\"PNG\"";

		if(pAP->getProperty("height", szValue))
		{
			escaped += " depth=\"";
			escaped += szValue;
			escaped += "\"";
		}
		if(pAP->getProperty("width", szValue))
		{
			escaped += " width=\"";
			escaped += szValue;
			escaped += "\"";
		}
		if(pAP->getProperty("lang", szValue))
		{
			escaped += " lang=\"";
			escaped += szValue;
			escaped += "\"";
		}

		_tagOpenClose(escaped,true,false,false);
		_tagClose(TT_IMAGEOBJECT,"imageobject",false,false,false);
		_tagClose(TT_MEDIAOBJECT,"mediaobject",false,false,false);
		_tagClose(TT_INFORMALFIGURE,"informalfigure",false,false,false);
	}
}

void s_DocBook_Listener::_handleField(const PX_ChangeRecord_Object * pcro, PT_AttrPropIndex api)
{
	UT_UTF8String buf(""), escaped("");
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);
	const gchar * szValue = 0, * szStyle = 0;

	if(!m_bInParagraph)
		_openBlock(false);

	m_pie->populateFields ();

	if(bHaveProp && pAP && pAP->getAttribute ("type", szValue))
	{
		if (!strcmp (szValue, "list_label"))
		{
			_openList(api);
			return;
		}

		fd_Field * field = pcro->getField();
		buf = "phrase role=\"";  //save the field type
		buf += szValue;
		buf += "\"";

		if (!strcmp (szValue, "endnote_anchor"))  //give the endnote <para> a unique id
		{
			if(pAP->getAttribute("endnote-id", szStyle))
			{
				buf += " id=\"endnote-id-";
				buf += szStyle;
				buf += "\"";
			}
		}

		_tagOpen(TT_PHRASE,buf,false,false,false);
		buf.clear();

		if (!strcmp (szValue, "footnote_ref"))
		{
			buf = "footnoteref linkend=\"footnote-id-";

			if(pAP->getAttribute("footnote-id", szValue))
			{
				buf += szValue;
				buf += "\"";
			}

			_tagOpenClose(buf,true,false,false);
		}
		else if (!strcmp (szValue, "endnote_ref"))
		{
			buf = "xref linkend=\"endnote-id-";

			if(pAP->getAttribute("endnote-id", szValue))
			{
				buf += szValue;
				buf += "\"";
			}

			_tagOpenClose(buf,true,false,false);
		}

		buf.clear();
		buf = field->getValue();
		if(buf.length())
		{
			buf.escapeXML();
			m_pie->write(buf.utf8_str());
		}

		_tagClose(TT_PHRASE,"phrase",false,false,false);
	}
}

void s_DocBook_Listener::_handleTOC(PT_AttrPropIndex api)
{
	UT_UTF8String buf(""), content("toc");
	const gchar* szValue = 0;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);

	/* TOCs are supposed to be at the beginning or end of a section, so we
	   open a new section to prevent the loss of its position in the text */

	_closeParagraph();
	_closeSection(m_iSectionDepth);
	_tagOpen(TT_SECTION,"section role=\"abi-toc\"");

	if(bHaveProp && pAP && pAP->getProperty("toc-heading", szValue)) //user-defined
	{
		buf = szValue;
		buf.escapeXML();
	}
	else  // get the default
	{
		XAP_App::getApp()->getStringSet()->getValueUTF8(AP_STRING_ID_TOC_TocHeading, buf);
	}

	// TODO: populate the TOC

	_tagOpen(TT_TITLE,"title",false);
	m_pie->write(buf.utf8_str());
	_tagClose(TT_TITLE,"title",true,false);
	_tagOpen(TT_TOC,content,false);
	_tagClose(TT_TOC,"toc",true,false);
	_tagOpenClose("para",false);
	_tagClose(TT_SECTION,"section");
}

void s_DocBook_Listener::_handleHyperlink(PT_AttrPropIndex api)
{
	UT_UTF8String buf(""), escaped("");
	const gchar* szValue = 0;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);

	if(bHaveProp && pAP && pAP->getAttribute("xlink:href", szValue))
	{
		if(szValue && (szValue[0] == '#'))
		{
			/* anchor */
			escaped = szValue + 1;  // skip '#'
			escaped.escapeURL();
			buf = "link linkend=\"";
			buf += escaped;
			buf += "\"";
			_tagOpen(TT_LINK,buf,false,false,false);
			m_bExternal = false;
		}
		else if(szValue)
		{
			/* external */
			escaped = szValue;
			escaped.escapeURL();
			buf = "ulink url=\"";
			buf += escaped;
			buf += "\"";
			_tagOpen(TT_ULINK,buf,false,false,false);
			m_bExternal = true;
		}
	}
	else
	{
		if (m_bExternal && (_tagTop() == TT_ULINK))
			_tagClose(TT_ULINK,"ulink",false,false,false);
		else if(!m_bExternal && (_tagTop() == TT_LINK))
			_tagClose(TT_LINK,"link",false,false,false);
	}
}

void s_DocBook_Listener::_handleBookmark(PT_AttrPropIndex api)
{
	UT_UTF8String buf(""), escaped("");
	const gchar* szValue = 0;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);

	if(bHaveProp && pAP && pAP->getAttribute("type", szValue))
	{
		if (!strcmp (szValue, "start") && pAP->getAttribute("name", szValue))
		{
			buf = "anchor id=\"";
			escaped = szValue;
			escaped.escapeXML();
			buf += escaped;
			buf += "\"";
			_tagOpenClose(buf,true,false,false);
		}
	}
}

void s_DocBook_Listener::_handleHdrFtr(PT_AttrPropIndex api)
{
	UT_UTF8String buf("abi-");  //prefix the type to prevent confusion on import (e.g. if there's a 'footer' style)
	const gchar* szValue = 0;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);

	if(bHaveProp && pAP && pAP->getAttribute("type", szValue))
		buf += szValue;

	_openSection(api,1,buf);
	_closeSectionTitle(); //no title
}

void s_DocBook_Listener::_handleFootnote(PT_AttrPropIndex api)
{
	UT_UTF8String buf("footnote id=\"footnote-id-");  //make it an NCNAME to prevent validation errors
	const gchar* szValue = 0;
	const PP_AttrProp * pAP = NULL;
	bool bHaveProp = m_pDocument->getAttrProp(api,&pAP);

	if(bHaveProp && pAP && pAP->getAttribute("footnote-id", szValue))
		buf += szValue;
	buf += "\"";

	if(m_bInTitle && !m_bInSection) //in a chapter title
	{
		_openSection(api,1,"");
	}

	if(m_bInTitle)  //in a section or chapter title
	{
		_closeSectionTitle();
		_openBlock(true);		
	}

	_tagOpen(TT_FOOTNOTE,buf,false,false,false);
}

bool s_DocBook_Listener::_decideIndent(void)
{
	if(m_bInTable)  // don't indent in tables
		return false;

	if(m_iBlockType == BT_PLAINTEXT)  // <literallayout>
		return false;

	if((_tagTop() == TT_FOOTNOTE) && ((m_iLastClosed == TT_PHRASE) || 
		(m_iLastClosed == TT_BLOCK) || (m_iLastClosed == TT_TITLE)))  // the <para> right after <footnote>
		return false;

	if(m_bInNote && ((m_iLastClosed == TT_PHRASE) || (m_iLastClosed == TT_BLOCK)))
		return false;

	if(m_bInHdrFtr)
		return true;

	return true;
}

bool s_DocBook_Listener::_inFormattedSpan(void)
{
	return ((_tagTop() == TT_SUPERSCRIPT) || (_tagTop() == TT_SUBSCRIPT) || (_tagTop() == TT_EMPHASIS));
}

bool s_DocBook_Listener::_inSectionStrux(void)
{
	return ((m_bInTable) || (m_bInFrame) || (m_bInHdrFtr) || (m_bInNote));
}

bool s_DocBook_Listener::populateStrux(PL_StruxDocHandle sdh,
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
			/* open a chapter, _not_ a section; changing this causes the creation of an extra section at the top
			   of every document */
			_openChapter(pcr->getIndexAP());
			return true;
		}

        case PTX_SectionEndnote:
		{
			// don't do anything - handled in _handleField()
			m_bInNote = true;
			return true;
		}
		case PTX_EndEndnote:
		{
			m_bInNote = false;
			return true;
		}

		case PTX_SectionHdrFtr:
		{
			_closeSection(0);
			_handleHdrFtr(pcr->getIndexAP());
			m_bInHdrFtr = true;
			return true;
		}

		case PTX_Block:
		{
			_openParagraph (pcr -> getIndexAP ());
			return true;
		}

		case PTX_SectionTable:
		{
			m_iTableDepth++;
			if(m_iTableDepth <= 2)
			{
				_closeParagraph();
				mTableHelper.OpenTable(sdh,pcr->getIndexAP()) ;
				_openTable(pcr->getIndexAP());
			}
			return true;
		}

		case PTX_SectionCell:
		{
			if(m_iTableDepth > 2)
				return true;

			if((m_iNestedTable == 2) && (m_iTableDepth == 1)) //the last cell had a nested table; reset the value
				m_iNestedTable = -1;

			PL_StruxDocHandle nextTable = NULL, nextCell = NULL;
			bool bNextTable = m_pDocument->getNextStruxOfType(sdh, PTX_SectionTable, &nextTable);
			bool bEndCell = m_pDocument->getNextStruxOfType(sdh, PTX_EndCell, &nextCell);

			if(bNextTable && bEndCell && (m_iNestedTable == -1))
			{
				if(m_pDocument->getStruxPosition(nextTable) < m_pDocument->getStruxPosition(nextCell)) //nested table
				{
					_closeParagraph();
					mTableHelper.OpenCell(pcr->getIndexAP());
					m_iNestedTable = 0;  //pending, so don't allow any writing
					return true;
				}
			}

			if((m_iNestedTable == -1) || (m_iNestedTable == 1))
			{
				// regular cell
				_closeParagraph();
				mTableHelper.OpenCell(pcr->getIndexAP());
				_openCell();
			}
			return true;
		}

		case PTX_EndTable:
		{	    
			// m_iTableDepth will be 1 if a nested table was closed
			// or 0 if a regular table was closed
			m_iTableDepth--;
			if(m_iTableDepth > 1)
				return true;

			_closeParagraph();
			_closeRow();
			_closeTable();
			mTableHelper.CloseTable();

			if(m_iNestedTable != 2) //don't allow any content after an </entrytbl>
				m_iNestedTable = -1;
			return true;
		}

		case PTX_EndCell:
		{
			if(m_iTableDepth > 2)
				return true;

			_closeParagraph();
			_closeCell();
			mTableHelper.CloseCell();
			return true;
		}

		case PTX_SectionFootnote:
		{
			_handleFootnote(pcr->getIndexAP());
			m_bInNote = true;
			return true;
		}

		case PTX_EndFootnote:
		{
			_closeParagraph();
			if(m_iLastClosed == TT_PHRASE)  //<footnote> was empty - see bug 9890
				_tagOpenClose("para",false,false,false);

			if(m_bInNote)  // we might've closed a footnote early to workaround .doc import bugs, so check first
				_tagClose(TT_FOOTNOTE,"footnote",false,false,false);

			m_bInNote = false;
			return true;
		}

		case PTX_SectionTOC:
		{
			_handleTOC(pcr->getIndexAP());
			return true;
		}
	
		case PTX_EndTOC:
		{
			// don't do anything - already handled in _handleTOC()
			return true;
		}

		case PTX_SectionFrame:
		{
			_closeSectionTitle();
			_openSection(pcr->getIndexAP(), m_iSectionDepth+1, "abi-frame");
			m_bInFrame = true;  //make sure this remains after the openSection() call
			return true;
		}
		case PTX_EndFrame:
		{
			_closeSection(m_iSectionDepth-1);
			m_bInFrame = false;
			return true;
		}

		case PTX_EndMarginnote:
		case PTX_SectionMarginnote:
		default:
			UT_ASSERT_HARMLESS(UT_TODO);
			return true;
	}
}

bool s_DocBook_Listener::change(PL_StruxFmtHandle /*sfh*/,
									const PX_ChangeRecord * /*pcr*/)
{
	UT_ASSERT(0);						// this function is not used.
	return false;
}

bool s_DocBook_Listener::insertStrux(PL_StruxFmtHandle /*sfh*/,
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

bool s_DocBook_Listener::signal(UT_uint32 /* iSignal */)
{
	UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
	return false;
}


/*****************************************************************/
/*****************************************************************/

UT_Error IE_Exp_DocBook::_writeDocument(void)
{
	m_pListener = new s_DocBook_Listener(getDoc(),this);

	if (!m_pListener)
		return UT_IE_NOMEMORY;
	m_pListener -> _initFile ();
	if (!getDoc()->tellListener(static_cast<PL_Listener *>(m_pListener)))
		return UT_ERROR;
	m_pListener -> _closeFile ();

	DELETEP(m_pListener);
	
	return ((m_error) ? UT_IE_COULDNOTWRITE : UT_OK);
}

/*****************************************************************/
/*****************************************************************/

void s_DocBook_Listener::_handleDataItems(void)
{
	// Lifted from HTML listener
 	const char * szName = 0;
	const char * szMimeType = 0;
	const UT_ByteBuf * pByteBuf;
	
	for (UT_uint32 k=0; (m_pDocument->enumDataItems(k,NULL,&szName,&pByteBuf,reinterpret_cast<const void **>(&szMimeType))); k++)
	{
		UT_sint32 loc = -1;
		for (UT_sint32 i = 0; i < m_utvDataIDs.getItemCount(); i++)
		{
			if(strcmp(const_cast<char*>(reinterpret_cast<const char*>(m_utvDataIDs[i])), szName) == 0)
			{
				loc = i;
				break;
			}
		}
		
		if(loc > -1)
		{
			UT_UTF8String fname;
			
			UT_UTF8String_sprintf(fname, "%s_data", m_pie->getFileName());
			/* int result = */
			UT_go_directory_create(fname.utf8_str(), 0750, NULL);
			
			if (!strcmp(szMimeType, "image/svg+xml"))
				UT_UTF8String_sprintf(fname, "%s/%s_%d.svg", fname.utf8_str(), szName, loc);
			if (!strcmp(szMimeType, "application/mathml+xml"))
				UT_UTF8String_sprintf(fname, "%s/%s_%d.mathml", fname.utf8_str(), szName, loc);
			else // PNG Image
			{  
			  char * temp = _stripSuffix(UT_go_basename(szName), '_');
			  char * fstripped = _stripSuffix(temp, '.');
			  FREEP(temp);
			  UT_UTF8String_sprintf(fname, "%s/%s.png", fname.utf8_str(), fstripped);
			  FREEP(fstripped);
			}
			
			
			GsfOutput *fp = UT_go_file_create (fname.utf8_str(), NULL);
			
			if(!fp)
			  continue;
			
			gsf_output_write(fp, pByteBuf->getLength(), (const guint8*)pByteBuf->getPointer(0));			
			gsf_output_close(fp);
			g_object_unref (G_OBJECT (fp));
		}
	}
}
