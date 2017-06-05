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
#include <string.h>
#include "ut_types.h"
#include "ut_assert.h"
#include "ut_debugmsg.h"
#include "ut_path.h"
#include "ut_string.h"
#include "xap_App.h"
#include "xap_Frame.h"
#include "ie_impexp_DocBook.h"
#include "ie_imp_DocBook.h"
#include "ie_impGraphic.h"
#include "ie_types.h"
#include "fg_GraphicRaster.h"
#include "pd_Document.h"
#include "ut_growbuf.h"
#include "ut_png.h"

/*****************************************************************/
/*****************************************************************/

IE_Imp_DocBook_Sniffer::IE_Imp_DocBook_Sniffer (const char * _name) :
  IE_ImpSniffer(_name)
{
  // 
}

// supported suffixes
static IE_SuffixConfidence IE_Imp_DocBook_Sniffer__SuffixConfidence[] = {
	{ "dbk", 	UT_CONFIDENCE_PERFECT 	},
	{ "xml", 	UT_CONFIDENCE_PERFECT 	},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_Imp_DocBook_Sniffer::getSuffixConfidence ()
{
	return IE_Imp_DocBook_Sniffer__SuffixConfidence;
}

UT_Confidence_t IE_Imp_DocBook_Sniffer::recognizeContents(const char * szBuf, 
											   UT_uint32 /*iNumbytes*/)
{
  // TODO: scan the first few lines

  if(strstr(szBuf, "PUBLIC \"-//OASIS//DTD DocBook XML") == NULL) 
    return UT_CONFIDENCE_ZILCH;

  return UT_CONFIDENCE_PERFECT;
}

UT_Error IE_Imp_DocBook_Sniffer::constructImporter(PD_Document * pDocument,
												   IE_Imp ** ppie)
{
	IE_Imp_DocBook * p = new IE_Imp_DocBook(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Imp_DocBook_Sniffer::getDlgLabels(const char ** pszDesc,
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

IE_Imp_DocBook::~IE_Imp_DocBook()
{
	//DELETEP(m_TableHelperStack);
}

IE_Imp_DocBook::IE_Imp_DocBook(PD_Document * pDocument)
	: IE_Imp_XML(pDocument, false),
	m_iCurListID(AUTO_LIST_RESERVED),
	m_iBlockDepth(0),
	m_iDataDepth(0),
	m_iListDepth(0),
	m_iFootnotes(0),
	m_iImages(0),
	m_iSectionDepth(0),
	m_iTitleDepth(0), // this counter doesn't include 'sections' like prefaces or dedications
	m_iNoteID(-1),
	m_utvTitles(7,1,false),
	m_bMustAddTitle(false),
	m_bRequiredBlock(false),
	m_bWroteBold(false),
	m_bWroteEntryPara(false),
	m_bInFrame(false),
	m_bInIndex(false),
	m_bInMath(false),
	m_bInMeta(false),
	m_bInNote(false),
	m_bInTable(false),
	m_bInTOC(false),
	m_bReadBook(false)
{
//	m_TableHelperStack(new IE_Imp_TableHelperStack());
	for(int i = 0; i < 7; i++)
	{
		m_utvTitles.addItem((fl_AutoNum *)NULL);
	}
}

/*****************************************************************/
/*****************************************************************/


struct _TokenTable
{
	const char *	m_name;
	int             m_type;
};

static struct xmlToIdMapping s_Tokens[] =
{
	{	"abbrev",			TT_ABBREVIATION		},
	{	"abstract",			TT_ABSTRACT			},
	{	"ackno",			TT_ACKNO			},
	{	"acronym",			TT_ACRONYM			},
	{	"alt",				TT_ALT				},
	{	"anchor",			TT_BOOKMARK			},
	{	"appendix",			TT_APPENDIX			},
	{	"appendixinfo",		TT_APPENDIXINFO		},
	{	"application",		TT_APPLICATION		},
	{	"area",				TT_AREA				},
	{	"areaset",			TT_AREASET			},
	{	"areaspec",			TT_AREASPEC			},
	{	"article",			TT_ARTICLE			},
	{	"articleinfo",		TT_ARTICLEINFO		},
	{	"artpagenums",		TT_ARTPAGENUMS		},
	{	"attribution",		TT_ATTRIBUTION		},
	{	"author",			TT_AUTHOR			},
	{	"authorblurb",		TT_AUTHORBLURB		},
	{	"authorgroup",		TT_AUTHORGROUP		},
	{	"authorinitials",	TT_AUTHORINITIALS	},
	{	"beginpage",		TT_PAGEBREAK		},
	{	"bibliocoverage",	TT_BIBLIOCOVERAGE	},
	{	"bibliodiv",		TT_BIBLIODIV		},
	{	"biblioentry",		TT_BIBLIOENTRY		},
	{	"bibliography",		TT_BIBLIOGRAPHY		},
	{	"bibliomisc",		TT_BIBLIOMISC		},
	{	"bibliomixed",		TT_BIBLIOMIXED		},
	{	"bibliomset",		TT_BIBLIOMSET		},
	{	"bibliorelation",	TT_BIBLIORELATION	},
	{	"biblioset",		TT_BIBLIOSET		},
	{	"bibliosource",		TT_BIBLIOSOURCE		},
	{	"blockquote",		TT_BLOCKQUOTE		},
	{	"book",				TT_DOCUMENT			},
	{	"bookinfo",			TT_BOOKINFO			},
	{	"bridgehead",		TT_BRIDGEHEAD		},
	{	"chapter",			TT_CHAPTER			},
	{	"chapterinfo",		TT_CHAPTERINFO		},
	{	"citetitle",		TT_CITETITLE		},
	{	"cmdsynopsis",		TT_CMDSYNOPSIS		},
	{	"col",				TT_COL				},
	{	"collab",			TT_COLLAB			},
	{	"collabname",		TT_COLLABNAME		},
	{	"colophon",			TT_COLOPHON			},
	{	"colspec",			TT_COLSPEC			},
	{	"command",			TT_COMMAND			},
	{	"copyright",		TT_COPYRIGHT		},
	{	"date",				TT_DATE				},
	{	"dedication",		TT_DEDICATION		},
	{	"edition",			TT_EDITION			},
	{	"editor",			TT_EDITOR			},
	{	"email",			TT_EMAIL			},
	{	"emphasis",			TT_EMPHASIS			},
	{	"entry",			TT_ENTRY			},
	{	"entrytbl",			TT_ENTRYTBL			},
	{	"epigraph",			TT_EPIGRAPH			},
	{	"equation",			TT_EQUATION			},
	{	"figure",			TT_FIGURE			},
	{	"firstname",		TT_FIRSTNAME		},
	{	"footnote",			TT_FOOTNOTE			},
	{	"footnoteref",		TT_FOOTNOTEREF		},
	{	"formalpara",		TT_FORMALPARA		},
	{	"funcsynopsis",		TT_FUNCSYNOPSIS		},
	{	"glossary",			TT_GLOSSARY			},
	{	"graphic",			TT_GRAPHIC			},
	{	"graphicco",		TT_GRAPHICCO		},
	{	"holder",			TT_HOLDER			},
	{	"honorific",		TT_HONORIFIC		},
	{	"imagedata",		TT_IMAGEDATA		},
	{	"imageobject",		TT_IMAGEOBJECT		},
	{	"imageobjectco",	TT_IMAGEOBJECTCO	},
	{	"index",			TT_INDEX			},
	{	"indexdiv",			TT_INDEXDIV			},
	{	"indexentry",		TT_INDEXENTRY		},
	{	"indexinfo",		TT_INDEXINFO		},
	{	"indexterm",		TT_INDEXTERM		},
	{	"informalequation",	TT_EQUATION			},
	{	"informalfigure",	TT_FIGURE			},
	{	"informaltable",	TT_TABLE			},
	{	"inlineequation",	TT_EQUATION			},
	{	"inlinemediaobject",TT_MEDIAOBJECT		},
	{	"invpartnumber",	TT_INVPARTNUMBER	},
	{	"issuenum",			TT_ISSUENUM			},
	{	"itemizedlist",		TT_ITEMIZEDLIST		},
	{	"itermset",			TT_ITERMSET			},
	{	"keyword",			TT_KEYWORD			},
	{	"keywordset",		TT_KEYWORDSET		},
	{	"legalnotice",		TT_LEGALNOTICE		},
	{	"lineage",			TT_LINEAGE			},
	{	"link",				TT_LINK				},
	{	"listitem",			TT_LISTITEM			},
	{	"literallayout",	TT_PLAINTEXT		},
	{	"mediaobject",		TT_MEDIAOBJECT		},
	{	"mediaobjectco",	TT_MEDIAOBJECTCO	},
	{	"orderedlist",		TT_ORDEREDLIST		},
	{	"othername",		TT_OTHERNAME		},
	{	"para",				TT_BLOCK			},
	{	"part",				TT_PART				},
	{	"partinfo",			TT_PARTINFO			},
	{	"partintro",		TT_PARTINTRO		},
	{	"phrase",			TT_PHRASE			},
	{	"preface",			TT_PREFACE			},
	{	"prefaceinfo",		TT_PREFACEINFO		},
	{	"primary",			TT_PRIMARY			},
	{	"primaryie",		TT_PRIMARYIE		},
	{	"printhistory",		TT_PRINTHISTORY		},
	{	"productname",		TT_PRODUCTNAME		},
	{	"productnumber",	TT_PRODUCTNUMBER	},
	{	"programlisting",	TT_PLAINTEXT		},
	{	"pubdate",			TT_PUBDATE			},
	{	"publisher",		TT_PUBLISHER		},
	{	"publishername",	TT_PUBLISHERNAME	},
	{	"quote",			TT_QUOTE			},
	{	"refentry",			TT_REFENTRY			},
	{	"revdescription",	TT_REVDESCRIPTION	},
	{	"revhistory",		TT_REVHISTORY		},
	{	"revision",			TT_REVISION			},
	{	"revnumber",		TT_REVNUMBER		},
	{	"revremark",		TT_REVREMARK		},
	{	"row",				TT_ROW				},
	{	"screen",			TT_SCREEN			},
	{	"screeninfo",		TT_SCREENINFO		},
	{	"screenshot",		TT_SCREENSHOT		},
	{	"secondary",		TT_SECONDARY		},
	{	"secondaryie",		TT_SECONDARYIE		},
	{	"sect1",			TT_SECTION			},
	{	"sect1info",		TT_SECTIONINFO		},
	{	"sect2",			TT_SECTION			},
	{	"sect2info",		TT_SECTIONINFO		},
	{	"sect3",			TT_SECTION			},
	{	"sect3info",		TT_SECTIONINFO		},
	{	"sect4",			TT_SECTION			},
	{	"sect4info",		TT_SECTIONINFO		},
	{	"sect5",			TT_SECTION			},
	{	"sect5info",		TT_SECTIONINFO		},
	{	"section",			TT_SECTION			},
	{	"sectioninfo",		TT_SECTIONINFO		},
	{	"see",				TT_SEE				},
	{	"seealso",			TT_SEEALSO			},
	{	"seeie",			TT_SEEIE			},
	{	"seg",				TT_SEG				},
	{	"seglistitem",		TT_SEGLISTITEM		},
	{	"segmentedlist",	TT_SEGMENTEDLIST	},
	{	"set",				TT_SET				},
	{	"setindex",			TT_INDEX			},
	{	"sgmltag",			TT_SGMLTAG			},
	{	"simpara",			TT_BLOCK			},
	{	"subject",			TT_SUBJECT			},
	{	"subjectset",		TT_SUBJECTSET		},
	{	"subjectterm",		TT_SUBJECTTERM		},
	{	"subscript",		TT_SUBSCRIPT		},
	{	"superscript",		TT_SUPERSCRIPT		},
	{	"surname",			TT_SURNAME			},
	{	"synopsis",			TT_SYNOPSIS			},
	{	"table",			TT_TABLE			},
	{	"tbody",			TT_TBODY			},
	{	"tertiary",			TT_TERTIARY			},
	{	"tertiaryie",		TT_TERTIARYIE		},
	{	"textobject",		TT_TEXTOBJECT		},
	{	"tfoot",			TT_TFOOT			},
	{	"tgroup",			TT_TGROUP			},
	{	"thead",			TT_THEAD			},
	{	"tip",				TT_TIP				},
	{	"title",			TT_TITLE			},
	{	"toc",				TT_TOC				},
	{	"tocback",			TT_TOCBACK			},
	{	"tocchap",			TT_TOCCHAP			},
	{	"tocfront",			TT_TOCFRONT			},
	{	"toclevel1",		TT_TOCLEVEL1		},
	{	"toclevel2",		TT_TOCLEVEL2		},
	{	"toclevel3",		TT_TOCLEVEL3		},
	{	"toclevel4",		TT_TOCLEVEL4		},
	{	"toclevel5",		TT_TOCLEVEL5		},
	{	"tocpart",			TT_TOCPART			},
	{	"ulink",			TT_ULINK			},
	{	"variablelist",		TT_VARIABLELIST		},
	{	"varname",			TT_VARNAME			},
	{	"videodata",		TT_VIDEODATA		},
	{	"videoobject",		TT_VIDEOOBJECT		},
	{	"volumenum",		TT_VOLUMENUM		},
	{	"xref",				TT_LINK				},
	{	"year",				TT_YEAR				}
};

#define TokenTableSize	((sizeof(s_Tokens)/sizeof(s_Tokens[0])))

/*****************************************************************/	
/*****************************************************************/	

#define X_TestParseState(ps)	((m_parseState==(ps)))

#define X_VerifyParseState(ps)	do {  if (!(X_TestParseState(ps)))			\
									  { UT_DEBUGMSG(("DOM: X_VerifyParseState failed: %s\n", #ps)); \
									    m_error = UT_IE_BOGUSDOCUMENT;	\
										 return; } } while (0)
										 
#define X_CheckDocument(b)		do {  if (!(b))								\
									  { UT_DEBUGMSG(("DOM: X_CheckDocument failed: %s\n", #b)); \
									    m_error = UT_IE_BOGUSDOCUMENT;	\
										 return; } } while (0)

#define X_CheckError(v)			do {  if (!(v))								\
									  { UT_DEBUGMSG(("DOM: X_CheckError failed: %s\n", #v)); \
									    m_error = UT_ERROR;			\
										 return; } } while (0)

#define	X_EatIfAlreadyError()	do {  if (m_error) { UT_DEBUGMSG(("Already failed...\n")); return; } } while (0)

#define	CHAPTER_HEADING		1
#define	SECTION1_HEADING	2
#define	SECTION2_HEADING	3
#define	SECTION3_HEADING	4
#define	SECTION4_HEADING	5
#define	SECTION5_HEADING	6
#define	SECTION6_HEADING	7

/*****************************************************************/
/*****************************************************************/

void IE_Imp_DocBook::startElement(const gchar *name,
				   const gchar **atts)
{
	UT_DEBUGMSG(("DocBook import: startElement: %s\n", name));

	// xml parser keeps running until buffer consumed
	X_EatIfAlreadyError();
	
	UT_uint32 tokenIndex = _mapNameToToken (name, s_Tokens, TokenTableSize);
	bool bPush = true;

	if (m_bMustAddTitle)
		createTitle ();
	m_bMustAddTitle = false;

	switch (tokenIndex)
	{
	case TT_SET:
	{
		X_VerifyParseState(_PS_Init);
		break;
	}

	case TT_ARTICLE:
	case TT_DOCUMENT:
	{
		/* starts the document */
		X_VerifyParseState(_PS_Init);
		m_parseState = _PS_Doc;
		X_CheckError(appendStrux(PTX_Section,static_cast<const gchar **>(NULL)));

		m_iSectionDepth = 0;	/* not in a section, nor a chapter */

		if(tokenIndex == TT_ARTICLE)
			m_iSectionDepth = 1;

		break;
	}

	case TT_PART:
	{
		X_VerifyParseState(_PS_Doc);
		X_CheckError(tagTop() == TT_DOCUMENT);
		break;
	}

	case TT_CHAPTER:
	{
		X_VerifyParseState(_PS_Doc);
		X_CheckError (m_iSectionDepth == 0);
		m_iSectionDepth = 1;
		m_iTitleDepth = 1;
		/* we'll have to add a number */
		m_bMustNumber = true;

		break;
	}

	case TT_SECTION:
	{
		if(m_iBlockDepth > 0)
			m_parseState = _PS_Sec;

		X_CheckError((m_parseState ==_PS_Doc) || (m_parseState ==_PS_Sec));
		// we must at least be in a chapter or article

		m_parseState = _PS_Sec;
		X_CheckError (m_iSectionDepth > 0);
		m_iSectionDepth++;
		m_iTitleDepth++;

		const gchar *p_val = NULL;
		p_val = _getXMLPropValue(static_cast<const gchar *>("role"), atts);
		m_bMustNumber = false;

		if(!p_val)
			break;

		if(!strcmp(p_val,"abi-frame"))
		{
			X_CheckError(appendStrux(PTX_SectionFrame, NULL));

			m_iTitleDepth--;
			m_bInFrame = true;
		}
		else if(!strcmp(p_val,"abi-toc"))
		{
			m_iTitleDepth--;
			m_bInTOC = true;  //so we can ignore the TOC title
		}
		else if(!strcmp(p_val,"numbered")) // for backward compatibility
		{
			m_bMustNumber = true;
		}
		else
		{
			m_sectionRole = p_val;
		}

		break;
	}

	case TT_ACKNO:
	{
		X_CheckError(tagTop() == TT_ARTICLE);
		m_parseState = _PS_Sec;
		break;
	}

	case TT_REFENTRY:
	{
		X_VerifyParseState(_PS_Sec);
		break;
	}

	case TT_REFSYNOPSISDIV:
	{
		X_VerifyParseState(_PS_Sec);
		X_CheckError(tagTop() == TT_REFENTRY);
		m_parseState = _PS_Block;
		m_iBlockDepth++;
		break;
	}

	case TT_FUNCSYNOPSIS:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_Sec));
		requireBlock();
		break;
	}

	case TT_CMDSYNOPSIS:
	case TT_EPIGRAPH:
	{
		X_CheckError(m_iSectionDepth > 0);
		m_parseState = _PS_Block;
		m_iBlockDepth++;
		break;
	}

	case TT_BIBLIOGRAPHY:
	case TT_GLOSSARY:
	{
		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_Doc));
		m_parseState = _PS_Sec;
		m_iSectionDepth++;
		break;
	}

	case TT_BIBLIODIV:
	{
		X_VerifyParseState(_PS_Sec);
		X_CheckError(tagTop() == TT_BIBLIOGRAPHY);
		m_parseState = _PS_Block;
		m_iBlockDepth++;
		break;
	}
	case TT_BIBLIOMIXED:
	{
		X_VerifyParseState(_PS_Sec);
		m_parseState = _PS_MetaData;
		m_bInMeta = true;
		break;
	}

	case TT_BIBLIOMSET:
	case TT_BIBLIOSET:
	{
		X_VerifyParseState(_PS_MetaData);
		m_bInMeta = true;
		break;
	}

	case TT_BIBLIOENTRY:
	{
		X_VerifyParseState(_PS_Sec);
		m_parseState = _PS_MetaData;
		m_bInMeta = true;
		break;
	}

	case TT_APPENDIX:
	case TT_COLOPHON:
	case TT_DEDICATION:
	case TT_PARTINTRO:
	case TT_PREFACE:
	{
		X_VerifyParseState(_PS_Doc);
		m_parseState = _PS_Sec;
		m_iSectionDepth++;
		break;
	}

	// Metadata elements:

	case TT_APPENDIXINFO:
	{
		X_VerifyParseState(_PS_Sec);
		X_CheckError(tagTop() == TT_APPENDIX);
		m_parseState = _PS_MetaData;
		m_bInMeta = true;
		break;
	}

	case TT_PREFACEINFO:
	{
		X_VerifyParseState(_PS_Sec);
		X_CheckError(tagTop() == TT_PREFACE);
		m_parseState = _PS_MetaData;
		m_bInMeta = true;
		break;
	}

	case TT_CHAPTERINFO:
	{
		X_VerifyParseState(_PS_Doc);
		X_CheckError(tagTop() == TT_CHAPTER);
		m_parseState = _PS_MetaData;
		m_bInMeta = true;
		break;
	}

	case TT_ARTICLEINFO:
	{
		X_VerifyParseState(_PS_Doc);
		X_CheckError(tagTop() == TT_ARTICLE);
		m_parseState = _PS_MetaData;
		m_bInMeta = true;
		break;
	}

	case TT_PARTINFO:
	{
		X_VerifyParseState(_PS_Doc);
		X_CheckError(tagTop() == TT_PART);
		m_parseState = _PS_MetaData;
		m_bInMeta = true;
		break;
	}

	case TT_BOOKINFO:
	{
		X_VerifyParseState(_PS_Doc);
		X_CheckError(tagTop() == TT_DOCUMENT);
		m_parseState = _PS_MetaData;
		m_bInMeta = true;
		break;
	}

	case TT_SECTIONINFO:
	{
		X_VerifyParseState(_PS_Sec);
		m_parseState = _PS_MetaData;
		m_bInMeta = true;
		break;
	}

	case TT_ARTPAGENUMS:
	case TT_BIBLIOCOVERAGE:
	case TT_BIBLIOMISC:
	case TT_BIBLIORELATION:
	case TT_BIBLIOSOURCE:
	case TT_COLLAB:
	case TT_COPYRIGHT:
	case TT_EDITION:
	case TT_ISSUENUM:
	case TT_ITERMSET:
	case TT_KEYWORDSET:
	case TT_LEGALNOTICE:
	case TT_PRINTHISTORY:
	case TT_PUBDATE:
	case TT_PUBLISHER:
	case TT_SUBJECTSET:
	case TT_VOLUMENUM:
	{
		X_VerifyParseState(_PS_MetaData);
		m_parseState = _PS_Meta;
		break;
	}

	case TT_AUTHOR:
	{
		X_CheckError((m_parseState == _PS_MetaData) || (m_parseState == _PS_Block));
		break;
	}

	case TT_APPLICATION:
	case TT_COMMAND:
	{
		X_CheckError((m_parseState == _PS_Meta) || (m_parseState == _PS_Block));
		break;
	}

	case TT_AUTHORGROUP:
	case TT_EDITOR:
	case TT_INVPARTNUMBER:
	{
		X_VerifyParseState(_PS_MetaData);
		break;
	}

	case TT_HOLDER:
	case TT_YEAR:
	{
		X_VerifyParseState(_PS_Meta);
		X_CheckError(tagTop() == TT_COPYRIGHT);
		break;
	}

	case TT_DATE:
	{
		X_CheckError((m_parseState == _PS_Meta) || (m_parseState == _PS_Revision));
		break;
	}

	case TT_AUTHORBLURB:
	{
		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_MetaData) || (m_parseState == _PS_Block));
		m_parseState = _PS_Sec;
		break;
	}

	case TT_AUTHORINITIALS:
	{
		X_CheckError((m_parseState == _PS_Meta) || (m_parseState == _PS_MetaData) || (m_parseState == _PS_Block) || (m_parseState == _PS_Revision));
		break;
	}

	case TT_FIRSTNAME:
	case TT_HONORIFIC:
	case TT_LINEAGE:
	case TT_OTHERNAME:
	case TT_SURNAME:
	{
		X_CheckError((m_parseState == _PS_Meta) || (m_parseState == _PS_MetaData) || (m_parseState == _PS_Block));
		break;
	}

	case TT_PUBLISHERNAME:
	{
		X_CheckError((m_parseState == _PS_MetaData) || (m_parseState == _PS_Meta));
		m_parseState = _PS_Meta;
		break;
	}

	case TT_COLLABNAME:
	case TT_KEYWORD:
	case TT_SUBJECT:
	case TT_SUBJECTTERM:
	{
		X_VerifyParseState(_PS_Meta);
		break;
	}

	case TT_ABSTRACT:
	{
		X_CheckError((m_parseState ==_PS_MetaData) || (m_iSectionDepth > 0));

		if(m_parseState == _PS_MetaData)
			m_parseState = _PS_Meta;
		else
			m_parseState = _PS_Sec;

		break;
	}

	case TT_FORMALPARA:
	{
		X_CheckError((m_parseState ==_PS_List) || (m_parseState ==_PS_Sec));
		m_parseState = _PS_Sec;
		break;
	}

	case TT_BLOCK:
	{
		X_CheckError((m_iSectionDepth > 0) || (m_parseState == _PS_Meta));

		if(m_bInTOC)
			break;

		if(m_parseState != _PS_Meta)
		{
			m_parseState = _PS_Block;

			X_CheckError(appendStrux(PTX_Block, NULL));

			m_iBlockDepth++;
		}
		else
		{
			bPush = false;
		}
		break;
	}
		
	case TT_BRIDGEHEAD:
	{
        X_VerifyParseState(_PS_Sec);
		m_parseState = _PS_Block;
		m_iBlockDepth++;

		const gchar *buf[3];
		buf[2] = NULL;

		const gchar *p_val = NULL;
		p_val = _getXMLPropValue(static_cast<const gchar *>("renderas"), atts);
		gchar style_att[10] = "Heading a";

		if(p_val)
		{
			if(!strcmp(p_val, "sect1") || !strcmp(p_val, "sect2") || !strcmp(p_val, "sect3") || !strcmp(p_val, "sect4"))
			{
				char num = p_val[4];
				style_att[8] = num;
			}
			else if(!strcmp(p_val, "sect5"))
			{
				char num = '4';
				style_att[8] = num; // Heading 5 doesn't exist in AbiWord; use Heading 4 instead
			}
			else // renderas could be "other"
			{
				char num = '1';
				style_att[8] = num;  // default to Heading 1
			}
		}
		else
		{
			char num = '1';
			style_att[8] = num;
		}

		X_CheckError(appendStrux(PTX_Block, NULL));
		buf[0] = PT_STYLE_ATTRIBUTE_NAME;
		buf[1] = g_strdup(style_att);
		X_CheckError(appendFmt(const_cast<const gchar **>(buf)));
		FREEP(buf[1]);

		break;
	}

	case TT_ITEMIZEDLIST:
	case TT_ORDEREDLIST:
	case TT_SEGMENTEDLIST:
	case TT_VARIABLELIST:
	{
		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_Block) || (m_parseState == _PS_List)); //nested lists are possible
		m_parseState = _PS_ListSec;
		m_iListDepth++;
		break;
	}

	case TT_SEGLISTITEM:
	{
		X_VerifyParseState(_PS_ListSec);
		X_CheckError(tagTop() == TT_SEGMENTEDLIST);
		m_parseState = _PS_List;
		break;
	}

	case TT_SEG:
	{
		X_VerifyParseState(_PS_List);
		X_CheckError(tagTop() == TT_SEGLISTITEM);
		m_parseState = _PS_Block;
		m_iBlockDepth++;
		break;
	}

	case TT_LISTITEM:
	{
		X_VerifyParseState(_PS_ListSec);
		m_parseState = _PS_List;
		break;
	}

	case TT_BLOCKQUOTE:
	{
		X_VerifyParseState(_PS_Sec);
		m_parseState = _PS_Block;
		m_iBlockDepth++;

		const gchar *buf[3];
		buf[0] = PT_STYLE_ATTRIBUTE_NAME;
		buf[1] = "Block Text";
		buf[2] = NULL;

		X_CheckError(appendStrux(PTX_Block, NULL));
		X_CheckError(appendFmt(const_cast<const gchar **>(buf)));
		break;
	}

	case TT_PLAINTEXT:
	{
		m_parseState = _PS_Block;
		m_iBlockDepth++;

		const gchar *buf[3];
		buf[0] = PT_STYLE_ATTRIBUTE_NAME;
		buf[1] = "Plain Text";
		buf[2] = NULL;

		X_CheckError(appendStrux(PTX_Block, const_cast<const gchar **>(buf)));
		m_bWhiteSignificant = true;
		break;
	}

	case TT_PHRASE:
	{
		X_VerifyParseState(_PS_Block);

		const gchar *p_val = NULL;
		p_val = _getXMLPropValue(static_cast<const gchar *>("role"), atts);

		const gchar *buf[7];
		buf[0] = NULL;
		buf[1] = NULL;
		buf[2] = NULL;
		buf[3] = NULL;
		buf[4] = NULL;
		buf[5] = NULL;
		buf[6] = NULL;

		if(p_val)
		{
			if(!strcmp(p_val, "strong"))
			{
				buf[0] = PT_PROPS_ATTRIBUTE_NAME;
				buf[1] = "font-weight:bold";
				X_CheckError(_pushInlineFmt(const_cast<const gchar **>(buf)));
				X_CheckError(appendFmt(&m_vecInlineFmt));
				m_bWroteBold = true;
			}
			else  //possible field
			{
				if(!strcmp(p_val, "footnote_ref"))
				{
					break; //handled with TT_FOOTNOTEREF
				}
				else if(!strcmp(p_val, "footnote_anchor"))
				{
					X_CheckError(m_bInNote);
					UT_UTF8String noteID;
					UT_UTF8String_sprintf(noteID,"%i",m_iNoteID);

					buf[2] = "footnote-id";
					buf[3] = (gchar*)g_strdup(noteID.utf8_str());
					buf[4] = PT_PROPS_ATTRIBUTE_NAME;
					buf[5] = "text-position:superscript";
				}
				buf[0] = PT_TYPE_ATTRIBUTE_NAME;
				buf[1] = (gchar*)p_val;

				X_CheckError(appendObject(PTO_Field,const_cast<const gchar **>(buf)));
				m_parseState = _PS_Field;
				FREEP(buf[3]);
			}
		}

		break;
	}

	case TT_EMPHASIS:
	case TT_SUPERSCRIPT:
	case TT_SUBSCRIPT:
	{
		X_VerifyParseState(_PS_Block);

		const gchar *buf[3];
		buf[0] = PT_PROPS_ATTRIBUTE_NAME;
		buf[1] = NULL;
		buf[2] = NULL;

		switch(tokenIndex)
		{
			case TT_EMPHASIS: 
				buf[1] = "font-style:italic";
				break;
			case TT_SUPERSCRIPT: 
				buf[1] = "text-position:superscript";
				break;
			case TT_SUBSCRIPT: 
				buf[1] = "text-position:subscript";
			    break;
			default:
				UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
				break;
		}
		
		X_CheckError(_pushInlineFmt(const_cast<const gchar **>(buf)));
		X_CheckError(appendFmt(&m_vecInlineFmt));
		break;
	}

	case TT_TITLE:
	{
		X_CheckError ((m_parseState == _PS_Doc) || (m_parseState == _PS_Sec) || 
			(m_parseState == _PS_MetaData) || (m_parseState == _PS_Block) || 
			(m_parseState == _PS_DataSec) || (m_parseState == _PS_Init) || 
			(m_parseState == _PS_Table) || (m_parseState == _PS_ListSec));

		m_bTitleAdded = false;
		m_bMustAddTitle = true;

		if ((m_parseState == _PS_Table) || (m_parseState == _PS_ListSec) || (m_parseState == _PS_Cell) ||
			(m_parseState == _PS_Init) || (m_parseState == _PS_Cell) || (m_bInTOC))
		{
			m_bMustAddTitle = false;
		}
		if(((m_iSectionDepth == 1) || (m_iSectionDepth == 0)) && (m_parseState == _PS_Doc) && (m_iTitleDepth == 0))
		{
			m_bMustAddTitle = false; // this is for <book> and <article> titles
		}
		else if(m_parseState == _PS_MetaData)
		{
			m_bMustAddTitle = false;
			m_parseState = _PS_Meta;
		}

		break;
	}

	case TT_PAGEBREAK:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_iSectionDepth > 0));

		if(m_parseState == _PS_Block)
		{
			UT_UCSChar ucs = UCS_FF;
			appendSpan(&ucs,1);
		}
		else
		{
			bPush = false;
		}

		break;
	}

	case TT_QUOTE:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState = _PS_Meta));
		if(m_parseState == _PS_Block)
		{
			UT_UCSChar ucs = UCS_LDBLQUOTE;
			appendSpan(&ucs,1);
		}
		else
		{
			UT_ASSERT_HARMLESS(UT_TODO);
		}
		break;
	}

	case TT_CITETITLE:
	case TT_PRODUCTNAME:
	case TT_PRODUCTNUMBER:
	{
		X_CheckError ((m_parseState == _PS_Block) || (m_parseState == _PS_MetaData));
		break;
	}

	case TT_ABBREVIATION:
	case TT_ACRONYM:
	case TT_VARNAME:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_MetaData));
		break;
	}

	case TT_ULINK:  //an external link
	{
		const gchar *buf[3];
		buf[2] = NULL;

		const gchar *p_val = NULL;
		p_val = _getXMLPropValue(static_cast<const gchar *>("url"), atts);

		if(p_val)
		{
			buf[0] = "xlink:href";
			buf[1] = (gchar*)p_val;
			X_CheckError(appendObject(PTO_Hyperlink, const_cast<const gchar **>(buf)));
		}
		else
		{
			UT_ASSERT_HARMLESS(UT_TODO);
		}

		break;
	}
	
	case TT_LINK:  //an internal link
	{
		const gchar *buf[3];
		buf[2] = NULL;

		const gchar *p_val = NULL;
		p_val = _getXMLPropValue(static_cast<const gchar *>("linkend"), atts);

		if(p_val)
		{
			UT_UTF8String link = "#";
			link += p_val;

			buf[0] = "xlink:href";
			buf[1] = (gchar*)link.utf8_str();
			X_CheckError(appendObject(PTO_Hyperlink, const_cast<const gchar **>(buf)));
		}
		else
		{
			UT_ASSERT_HARMLESS(UT_TODO);
		}
		break;
	}

	case TT_ATTRIBUTION:
	case TT_EMAIL:
	case TT_SGMLTAG:
	{
		X_VerifyParseState(_PS_Block);
		break;
	}

	case TT_BOOKMARK:
	{
		X_VerifyParseState(_PS_Block);
		const gchar *buf[5];
		buf[4] = NULL;

		const gchar *p_val = NULL;
		p_val = _getXMLPropValue(static_cast<const gchar *>("id"), atts);

		if(p_val)
		{
			buf[0] = PT_TYPE_ATTRIBUTE_NAME;
			buf[1] = "start";
			buf[2] = PT_NAME_ATTRIBUTE_NAME;
			buf[3] = (gchar*)p_val;
			X_CheckError(appendObject(PTO_Bookmark, const_cast<const gchar **>(buf)));
			buf[1] = "end";
			X_CheckError(appendObject(PTO_Bookmark, const_cast<const gchar **>(buf)));
		}
		else
		{
			UT_ASSERT_HARMLESS(UT_TODO);
		}
		break;
	}

	case TT_FOOTNOTE:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_Cell));
		m_parseState = _PS_Sec;
		m_bInNote = true;
		m_iFootnotes++;

		const gchar *buf[3];
		UT_UTF8String noteID;

		if(m_iNoteID == -1)
		{
			UT_UTF8String id;

			m_iNoteID = m_iFootnotes;
			UT_UTF8String_sprintf(id,"%i",m_iNoteID);

			const gchar *ref[7];
			ref[0] = PT_TYPE_ATTRIBUTE_NAME;
			ref[1] = "footnote_ref";
			ref[2] = "footnote-id";
			ref[3] = (gchar*)g_strdup(id.utf8_str());
			ref[4] = PT_PROPS_ATTRIBUTE_NAME;
			ref[5] = "text-position:superscript";
			ref[6] = NULL;
			X_CheckError(appendObject(PTO_Field,const_cast<const gchar **>(ref)));
			FREEP(ref[3]);
		}

		UT_UTF8String_sprintf(noteID,"%i",m_iNoteID);
		buf[0] = "footnote-id";
		buf[1] = (gchar*)g_strdup(noteID.utf8_str());
		buf[2] = NULL;

		X_CheckError(appendStrux(PTX_SectionFootnote,const_cast<const gchar **>(buf)));
		FREEP(buf[1]);

		break;
	}

	case TT_FOOTNOTEREF:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_Field) || (m_parseState == _PS_Cell));
		m_parseState = _PS_Field;

		const gchar *buf[7];
		buf[0] = PT_TYPE_ATTRIBUTE_NAME;
		buf[1] = "footnote_ref";
		buf[2] = NULL;
		buf[3] = NULL;
		buf[4] = NULL;
		buf[5] = NULL;
		buf[6] = NULL;

		const gchar *p_val = NULL;
		p_val = _getXMLPropValue(static_cast<const gchar *>("linkend"), atts);

		if(p_val)
		{
			if(strstr(p_val,"footnote-id-") != NULL)
			{
				p_val += 12;
			}
			else
			{
				p_val = "-1";
			}
			m_iNoteID = atoi(p_val);

			buf[2] = "footnote-id";
			buf[3] = (gchar*)p_val;
			buf[4] = PT_PROPS_ATTRIBUTE_NAME;
			buf[5] = "text-position:superscript";
			buf[6] = NULL;
			X_CheckError(appendObject(PTO_Field,const_cast<const gchar **>(buf)));
		}
		else
		{
			UT_ASSERT_HARMLESS(UT_TODO);
		}
		break;
	}

	// TOC elements:

	case TT_TOC:
	{
		X_VerifyParseState(_PS_Sec);
		requireBlock();

		X_CheckError(appendStrux(PTX_SectionTOC, NULL));

		m_bInTOC = true;
		break;
	}

	case TT_TOCBACK:
	case TT_TOCCHAP:
	case TT_TOCFRONT:
	case TT_TOCLEVEL1:
	case TT_TOCLEVEL2:
	case TT_TOCLEVEL3:
	case TT_TOCLEVEL4:
	case TT_TOCLEVEL5:
	case TT_TOCPART:
	{
		X_CheckError(m_bInTOC);
		break;
	}

	// Index elements (not imported):

	case TT_INDEX:
	{
		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_Doc));
		break;
	}

	case TT_INDEXTERM:
	{
		UT_ASSERT_HARMLESS(UT_TODO);
		break;
	}

	case TT_PRIMARY:
	case TT_SECONDARY:
	case TT_SEE:
	case TT_SEEALSO:
	case TT_TERTIARY:
	{
		X_CheckError(tagTop() == TT_INDEXTERM);
		break;
	}

	case TT_INDEXDIV:
	case TT_INDEXENTRY:
	case TT_INDEXINFO:
	case TT_PRIMARYIE:
	case TT_SECONDARYIE:
	case TT_SEEIE:
	case TT_TERTIARYIE:
	{
		X_CheckError(m_bInIndex);
		break;
	}

	// Table elements:

	case TT_TABLE:
	{
		if((m_parseState == _PS_Doc) && (tagTop() == TT_ARTICLE))
		{
			m_parseState = _PS_Sec;
		}

		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_List));
		m_parseState = _PS_Table;
		//X_CheckError(m_TableHelperStack->OpenTable (getDoc(),static_cast<const char *>(NULL)));
		X_CheckError(appendStrux(PTX_SectionTable, NULL));

		m_bInTable = true;

		break;
	}

	case TT_ENTRY: //table cell
	{
		X_VerifyParseState(_PS_Table);
		m_parseState = _PS_Cell;

		//TODO: use the table helper
		X_CheckError(appendStrux(PTX_SectionCell, NULL));

		break;
	}

	case TT_ENTRYTBL: //nested table
	{
		X_VerifyParseState(_PS_Table);
		X_CheckError(appendStrux(PTX_SectionCell,static_cast<const gchar **>(NULL)));
		requireBlock();

		X_CheckError(appendStrux(PTX_SectionTable, NULL));

		m_parseState = _PS_Table; //requireBlock() will reset this
		break;
	}

	case TT_COL:
	case TT_COLSPEC:
	case TT_ROW:
	case TT_TBODY:
	case TT_TFOOT:
	case TT_TGROUP:
	case TT_THEAD:
	{
		X_VerifyParseState(_PS_Table);  //just make sure these elements are in a table
		break;
	}

	// Revision elements:

	case TT_REVHISTORY:
	{
		X_VerifyParseState(_PS_MetaData);
		m_parseState = _PS_RevisionSec;
		break;
	}

	case TT_REVISION:
	{
		X_VerifyParseState(_PS_RevisionSec);
		m_parseState = _PS_Revision;
		break;
	}

	case TT_REVREMARK:
	case TT_REVDESCRIPTION:
	case TT_REVNUMBER:
	{
		//TODO: import revisions
		X_VerifyParseState(_PS_Revision);
		break;
	}

	case TT_EQUATION:
	{
		X_VerifyParseState(_PS_Block);
		m_bInMath = true;
		break;
	}

	case TT_ALT:
	{
		X_CheckError(m_bInMath);
		break;
	}

	case TT_TEXTOBJECT:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_DataSec));
		m_parseState = _PS_Block;
		m_iBlockDepth++;
		break;
	}

	case TT_GRAPHICCO:
	{
		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_Block));
		requireBlock();
		m_parseState = _PS_Block;
		break;
	}

	case TT_MEDIAOBJECTCO:
	{
		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_Block) || (m_parseState == _PS_DataSec));
		requireBlock();
		m_parseState = _PS_DataSec;
		m_iDataDepth++;
		break;
	}

	case TT_AREA:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_DataSec));
		break;		
	}

	case TT_AREASET:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_DataSec));
		X_CheckError(tagTop() == TT_AREASPEC);
		break;
	}

	case TT_AREASPEC:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_DataSec));
		break;
	}

	case TT_SCREENSHOT:
	{
		if(m_parseState == _PS_Meta)
			return; //don't handle images embedded in metadata

		requireBlock();
		X_VerifyParseState(_PS_Block);
		m_parseState = _PS_DataSec;
		m_iDataDepth++;
		break;
	}

	case TT_SCREEN:
	case TT_SYNOPSIS:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_ListSec) || (m_parseState == _PS_List) || (m_parseState == _PS_Sec));
		requireBlock();
		m_parseState = _PS_Block;
		break;
	}

	case TT_SCREENINFO:
	{
		X_VerifyParseState(_PS_DataSec);
		X_CheckError(tagTop() == TT_SCREENSHOT);
		m_parseState = _PS_Meta;
		m_iDataDepth++;
		break;
	}

	case TT_GRAPHIC:
	{
		if(m_parseState == _PS_Meta)
			return; //don't handle images embedded in metadata

		requireBlock();
		X_CheckError ((m_parseState == _PS_Block) || (m_parseState == _PS_DataSec) || (m_bInMath));
		m_parseState = _PS_DataItem;
		m_iDataDepth++;

		const gchar *p_val = NULL;
		p_val = _getXMLPropValue(static_cast<const gchar *>("fileref"), atts);

		if(p_val)
			createImage (p_val, atts);

		break;
	}

	case TT_FIGURE:
	{
		requireBlock();
		X_VerifyParseState(_PS_Block);
		m_parseState = _PS_DataSec;
		m_iDataDepth++;
		break;
	}

	case TT_VIDEOOBJECT:
	{
		X_VerifyParseState(_PS_DataSec);
		m_parseState = _PS_DataSec;
		m_iDataDepth++;
		break;
	}

	case TT_VIDEODATA:
	{
		X_CheckError(tagTop() == TT_VIDEOOBJECT);
		X_VerifyParseState(_PS_DataSec);
		m_parseState = _PS_DataItem;
		m_iDataDepth++;
		break;
	}

	case TT_MEDIAOBJECT:
	{
		requireBlock();
		X_CheckError ((m_parseState == _PS_Meta) || (m_parseState == _PS_Block) || (m_parseState == _PS_DataSec));
		m_parseState = _PS_DataSec;
		m_iDataDepth++;
		break;
	}

	case TT_IMAGEOBJECT:
	{
		X_CheckError ((m_parseState == _PS_Meta) || (m_parseState == _PS_Block) || (m_parseState == _PS_DataSec));
		m_parseState = _PS_DataSec;
		m_iDataDepth++;
		break;
	}

	case TT_IMAGEDATA:
	{
		X_CheckError(tagTop() == TT_IMAGEOBJECT);
		m_parseState = _PS_DataItem;
		m_iDataDepth++;

		const gchar *p_val = NULL;
		p_val = _getXMLPropValue(static_cast<const gchar *>("fileref"), atts);

		if(p_val)
			createImage (p_val, atts);

		break;
	}
	  
	case TT_OTHER:
	default:
		UT_ASSERT_HARMLESS(UT_TODO);
	    UT_DEBUGMSG(("Unknown or knowingly unhandled tag [%s]\n",name));
	    break;
	}

	if(bPush)
	{
		m_utnsTagStack.push(tokenIndex);
		xxx_UT_DEBUGMSG(("Pushing %d onto stack\n",tokenIndex));
	}
}

void IE_Imp_DocBook::endElement(const gchar *name)
{
  
    UT_DEBUGMSG(("DocBook import: endElement: %s\n", name));

    // xml parser keeps running until buffer consumed
	X_EatIfAlreadyError();
	
   	UT_uint32 tokenIndex = _mapNameToToken (name, s_Tokens, TokenTableSize);
	bool bPop = true;

	switch (tokenIndex)
	{
	case TT_SET:
	{
		X_VerifyParseState(_PS_Init);
		break;
	}

	case TT_DOCUMENT:
	{
		X_VerifyParseState(_PS_Doc);
		m_parseState = _PS_Init;
		// we've already imported a <book>, so we don't want to set any more metadata (a <set> can
		// contain more than one <book>)
		m_bReadBook = true;
		break;
	}

	case TT_PART:
	{
		X_VerifyParseState(_PS_Doc);
		break;
	}

	case TT_ARTICLE:
	{
		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_Doc));
		m_parseState = _PS_Doc;
		m_iSectionDepth = 0;
		break;
	}

	case TT_CHAPTER:
	{
		X_VerifyParseState(_PS_Sec);
		m_parseState = _PS_Doc;
		m_iSectionDepth = 0;
		m_iTitleDepth = 0;
		m_bMustNumber = false;
		break;
	}

	case TT_SECTION:
	{
		if(m_iBlockDepth > 0)  //can happen if requireBlock() was used
		{
			m_parseState = _PS_Sec;
			m_iBlockDepth = 0;
		}

		X_VerifyParseState(_PS_Sec);
		m_iSectionDepth--;

		if(!m_bInTOC && !m_bInFrame)
			m_iTitleDepth--;

		if(m_iSectionDepth == 0)
			m_parseState = _PS_Doc;

		if(m_bInFrame)
		{
			X_CheckError(appendStrux(PTX_EndFrame,NULL));
			m_bInFrame = false;
		}
		else if(m_bInTOC)
		{
			m_bInTOC = false;
		}

		m_bMustNumber = false;
		break;
	}

	case TT_ACKNO:
	{
		X_VerifyParseState(_PS_Sec);
		m_parseState = _PS_Doc;
		break;
	}

	case TT_REFENTRY:
	{
		X_VerifyParseState(_PS_Sec);
		break;
	}

	case TT_FUNCSYNOPSIS:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_Sec));

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		if(m_iBlockDepth == 0)
			m_parseState = _PS_Sec;

		break;
	}

	case TT_CMDSYNOPSIS:
	{
		X_VerifyParseState(_PS_Block);
		m_iBlockDepth--;

		if(m_iBlockDepth == 0)
			m_parseState = _PS_Sec;

		break;
	}

	case TT_EPIGRAPH:
	case TT_REFSYNOPSISDIV:
	{
		X_VerifyParseState(_PS_Block);
		m_parseState = _PS_Sec;
		m_iBlockDepth--;
		break;
	}

	case TT_BIBLIOGRAPHY:
	case TT_GLOSSARY:
	{
		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_Doc));
		m_iSectionDepth--;

		if(m_iSectionDepth == 0)
			m_parseState = _PS_Doc;

		break;
	}

	case TT_BIBLIODIV:
	{
		X_VerifyParseState(_PS_Sec);
		m_iBlockDepth--;
		break;
	}

	case TT_BIBLIOMIXED:
	{
		X_VerifyParseState(_PS_MetaData);
		m_bInMeta = false;
		m_parseState = _PS_Sec;
		break;
	}

	case TT_BIBLIOENTRY:
	{
		X_VerifyParseState(_PS_MetaData);
		m_bInMeta = false;
		m_parseState = _PS_Sec;

		break;
	}

	case TT_APPENDIX:
	case TT_COLOPHON:
	case TT_DEDICATION:
	case TT_PARTINTRO:
	case TT_PREFACE:
	{
		X_VerifyParseState(_PS_Sec);
		m_parseState = _PS_Doc;
		m_iSectionDepth--;
		break;
	}

	// Metadata elements:

	case TT_APPENDIXINFO:
	case TT_ARTICLEINFO:
	case TT_BOOKINFO:
	case TT_CHAPTERINFO:
	case TT_PARTINFO:
	case TT_PREFACEINFO:
	{
		X_VerifyParseState(_PS_MetaData);
		m_parseState = _PS_Doc;
		m_bInMeta = false;
		break;
	}

	case TT_SECTIONINFO:
	{
		X_VerifyParseState(_PS_MetaData);
		m_parseState = _PS_Sec;
		m_bInMeta = false;
		break;
	}

	case TT_ARTPAGENUMS:
	case TT_BIBLIOCOVERAGE:
	case TT_BIBLIOMISC:
	case TT_BIBLIORELATION:
	case TT_BIBLIOSOURCE:
	case TT_COLLAB:
	case TT_COPYRIGHT:
	case TT_EDITION:
	case TT_ISSUENUM:
	case TT_ITERMSET:
	case TT_KEYWORDSET:
	case TT_LEGALNOTICE:
	case TT_PRINTHISTORY:
	case TT_PUBDATE:
	case TT_PUBLISHERNAME:
	case TT_SUBJECTSET:
	case TT_VOLUMENUM:
	{
		X_VerifyParseState(_PS_Meta);
		m_parseState = _PS_MetaData;
		break;
	}

	case TT_AUTHORGROUP:
	case TT_BIBLIOMSET:
	case TT_BIBLIOSET:
	case TT_EDITOR:
	case TT_INVPARTNUMBER:
	case TT_PUBLISHER:
	{
		X_VerifyParseState(_PS_MetaData);
		break;
	}

	case TT_AUTHOR:
	{
		X_CheckError((m_parseState == _PS_MetaData) || (m_parseState == _PS_Block));
		break;
	}

	case TT_APPLICATION:
	case TT_COMMAND:
	{
		X_CheckError((m_parseState == _PS_Meta) || (m_parseState == _PS_Block));
		break;
	}

	case TT_DATE:
	{
		X_CheckError((m_parseState == _PS_Meta) || (m_parseState == _PS_Revision));
		break;
	}

	case TT_AUTHORBLURB:
	{
		X_VerifyParseState(_PS_Sec);

		if(m_bInMeta)
			m_parseState = _PS_MetaData;
		else if(m_iBlockDepth > 0)
			m_parseState = _PS_Block;

		break;
	}

	case TT_AUTHORINITIALS:
	{
		X_CheckError((m_parseState == _PS_Meta) || (m_parseState == _PS_MetaData) || (m_parseState == _PS_Block) || (m_parseState == _PS_Revision));
		break;
	}

	case TT_FIRSTNAME:
	case TT_HONORIFIC:
	case TT_LINEAGE:
	case TT_OTHERNAME:
	case TT_SURNAME:
	{
		X_CheckError((m_parseState == _PS_Meta) || (m_parseState == _PS_MetaData) || (m_parseState == _PS_Block));
		break;
	}

	case TT_COLLABNAME:
	case TT_HOLDER:
	case TT_KEYWORD:
	case TT_SUBJECT:
	case TT_SUBJECTTERM:
	case TT_YEAR:
	{
		X_VerifyParseState(_PS_Meta);
		break;
	}

	case TT_ABSTRACT:
	{
		X_CheckError((m_parseState ==_PS_Meta) || (m_iSectionDepth > 0));

		if(m_parseState == _PS_Meta)
			m_parseState = _PS_MetaData;

		break;
	}

	case TT_ITEMIZEDLIST:
	case TT_ORDEREDLIST:
	case TT_SEGMENTEDLIST:
	case TT_VARIABLELIST:
	{
		X_VerifyParseState(_PS_ListSec);
		m_iListDepth--;

		if(m_iListDepth > 0) //nested lists are possible
			m_parseState = _PS_List;
		else if(m_iBlockDepth > 0)
			m_parseState = _PS_Block;
		else if(m_iListDepth == 0)
			m_parseState = _PS_Sec;

		break;
	}

	case TT_LISTITEM:
	case TT_SEGLISTITEM:
	{
		X_VerifyParseState(_PS_List);
		m_parseState = _PS_ListSec;
		break;
	}

	case TT_SEG:
	{
		X_VerifyParseState(_PS_Block);
		m_parseState = _PS_List;
		m_iBlockDepth--;
		break;
	}

	case TT_BRIDGEHEAD:
	{
		UT_ASSERT_HARMLESS(m_lenCharDataSeen==0);

		X_VerifyParseState(_PS_Block);
		m_parseState = _PS_Sec;
		m_iBlockDepth--;

		X_CheckDocument(_getInlineDepth()==0);
		_popInlineFmt();
		X_CheckError(appendFmt(&m_vecInlineFmt));

		break;
	}

	case TT_BLOCKQUOTE:
	{
		UT_ASSERT_HARMLESS(m_lenCharDataSeen==0);

		X_VerifyParseState(_PS_Block);
		m_parseState = _PS_Sec;
		m_iBlockDepth--;

		X_CheckDocument(_getInlineDepth()==0);
		_popInlineFmt();
		X_CheckError(appendFmt(&m_vecInlineFmt));

		break;
	}

	case TT_PLAINTEXT:
	{
		UT_ASSERT_HARMLESS(m_lenCharDataSeen==0);

		X_VerifyParseState(_PS_Block);
		m_iBlockDepth--;

		if(m_bInTable)
			m_parseState = _PS_Cell;
		else if(m_iBlockDepth == 0)
			m_parseState = _PS_Sec;
		else if(m_iDataDepth)
			m_parseState = _PS_DataSec;
		else
			m_parseState = _PS_Block;

		m_bWhiteSignificant = false;
		break;
	}

	case TT_FORMALPARA:
	{
		X_CheckError((m_parseState ==_PS_List) || (m_parseState ==_PS_Sec));

		if(m_iListDepth > 0)
			m_parseState = _PS_List;
		else
			m_parseState = _PS_Sec;

		break;
	}

	case TT_BLOCK:
	{
		UT_ASSERT_HARMLESS(m_lenCharDataSeen==0);

		if(m_bInTOC)
			break;

		if(m_parseState != _PS_Meta)
		{
			X_CheckError((m_parseState == _PS_Cell) || (m_parseState == _PS_Block));
			X_CheckDocument(_getInlineDepth()==0);

			m_iBlockDepth--;

			if(m_bInTable)
				m_parseState = _PS_Cell;
			else if(m_iListDepth > 0)
				m_parseState = _PS_List;
			else if(m_iDataDepth > 0)
				m_parseState = _PS_DataSec;
			else if(m_iBlockDepth == 0)
				m_parseState = _PS_Sec;
		}
		else
		{
			bPop = false;
		}
		break;
	}
		
	case TT_PHRASE:
	{
		X_CheckError((m_parseState ==_PS_Field) || (m_parseState ==_PS_Block) || (m_parseState == _PS_Cell));

		if(m_parseState ==_PS_Field)
			m_parseState = _PS_Block;

		if(m_bWroteBold)
		{
			m_bWroteBold = false;
			X_CheckDocument(_getInlineDepth()>0);
		    _popInlineFmt();
			X_CheckError(appendFmt(&m_vecInlineFmt));
		}

		break;
	}

	case TT_EMPHASIS:
	case TT_SUPERSCRIPT:
	case TT_SUBSCRIPT:
	{
		UT_ASSERT_HARMLESS(m_lenCharDataSeen==0);

		X_VerifyParseState(_PS_Block);
		X_CheckDocument(_getInlineDepth()>0);
		_popInlineFmt();
		X_CheckError(appendFmt(&m_vecInlineFmt));

		break;
	}

	case TT_TITLE:
	{
		if (m_bTitleAdded)
		{
			UT_ASSERT_HARMLESS(m_lenCharDataSeen==0);
			X_VerifyParseState(_PS_Block);
			m_parseState = _PS_Sec;
			X_CheckDocument(_getInlineDepth()==0);
		}
		else if(m_parseState == _PS_DataSec)
		{
			m_parseState = _PS_Block;
		}
		else if(m_parseState == _PS_Meta)
		{
			m_parseState = _PS_MetaData;
		}
		else if(m_parseState == _PS_Block)
		{
			m_parseState = _PS_Sec;
		}

		m_bTitleAdded = false;
		m_bMustAddTitle = false;
		break;
	}

	case TT_ATTRIBUTION:
	case TT_SGMLTAG:
	{
		X_VerifyParseState(_PS_Block);
		break;
	}

	case TT_EMAIL: // an email address
	{
		X_VerifyParseState(_PS_Block);
		X_CheckError(appendObject(PTO_Hyperlink, NULL));
		break;
	}

	case TT_BOOKMARK:
	{
		/* all has been taken care of in startElement. */
		break;
	}

	case TT_LINK:
	case TT_ULINK:
	    /* end of the link */
	  {
	    UT_ASSERT_HARMLESS(m_lenCharDataSeen==0);
		X_CheckError(appendObject(PTO_Hyperlink, NULL));
	  }
		break;

	case TT_PAGEBREAK:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_iSectionDepth > 0));
		break;
	}

	case TT_QUOTE:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState = _PS_Meta));
		if(m_parseState == _PS_Block)
		{
			UT_UCSChar ucs = UCS_RDBLQUOTE;
			appendSpan(&ucs,1);
		}
		else
		{
			UT_ASSERT_HARMLESS(UT_TODO);
		}
		break;
	}

	case TT_CITETITLE:
	case TT_PRODUCTNAME:
	case TT_PRODUCTNUMBER:
	{
		X_CheckError ((m_parseState == _PS_Block) || (m_parseState == _PS_MetaData));
		break;
	}

	case TT_ABBREVIATION:
	case TT_ACRONYM:
	case TT_VARNAME:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_MetaData));
		break;
	}

	case TT_FOOTNOTE:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_Cell));
		X_CheckError(appendStrux(PTX_EndFootnote,static_cast<const gchar **>(NULL)));

		if(m_bInTable)
			m_parseState = _PS_Cell;
		else
			m_parseState = _PS_Block;

		m_bInNote = false;
		m_iNoteID = -1;
		break;
	}

	case TT_FOOTNOTEREF:
	{
		X_VerifyParseState(_PS_Field);

		if(m_bInTable)
			m_parseState = _PS_Cell;
		else
			m_parseState = _PS_Block;

		break;
	}

	// TOC elements:

	case TT_TOC:
	{
		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
			m_parseState = _PS_Sec;
		}

		X_VerifyParseState(_PS_Sec);
		X_CheckError(appendStrux(PTX_EndTOC,static_cast<const gchar **>(NULL)));
		X_CheckError(appendStrux(PTX_Block,static_cast<const gchar **>(NULL)));
		break;
	}

	case TT_TOCBACK:
	case TT_TOCCHAP:
	case TT_TOCFRONT:
	case TT_TOCLEVEL1:
	case TT_TOCLEVEL2:
	case TT_TOCLEVEL3:
	case TT_TOCLEVEL4:
	case TT_TOCLEVEL5:
	case TT_TOCPART:
	{
		X_CheckError(m_bInTOC);
		break;
	}

	// Index elements (not imported):

	case TT_INDEX:
	{
		X_CheckError((m_parseState == _PS_Sec) || (m_parseState == _PS_Doc));
		m_bInIndex = false;
		break;
	}

	case TT_INDEXTERM:
	case TT_PRIMARY:
	case TT_SECONDARY:
	case TT_SEE:
	case TT_SEEALSO:
	case TT_TERTIARY:
	{
		UT_ASSERT_HARMLESS(UT_TODO);
		break;
	}

	case TT_INDEXDIV:
	case TT_INDEXENTRY:
	case TT_INDEXINFO:
	case TT_PRIMARYIE:
	case TT_SECONDARYIE:
	case TT_SEEIE:
	case TT_TERTIARYIE:
	{
		X_CheckError(m_bInIndex);
		break;
	}

	// Table elements:

	case TT_TABLE:
	{
		X_VerifyParseState(_PS_Table);
		X_CheckError(appendStrux(PTX_EndTable,static_cast<const gchar **>(NULL)));

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		if(m_iListDepth > 0)
			m_parseState = _PS_List;
		else
			m_parseState = _PS_Sec;

		m_bInTable = false;
		break;
	}

	case TT_ENTRY: //table cell
	{
		if(m_bWroteEntryPara)
		{
			m_parseState = _PS_Cell;
			m_iBlockDepth--;
			m_bWroteEntryPara = false;
		}

		X_VerifyParseState(_PS_Cell);
		X_CheckError(appendStrux(PTX_EndCell,NULL));
		m_parseState = _PS_Table;

		break;
	}

	case TT_ENTRYTBL: //nested table
	{
		X_VerifyParseState(_PS_Table);
		X_CheckError(appendStrux(PTX_EndTable,NULL));
		X_CheckError(appendStrux(PTX_EndCell,NULL));

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		break;
	}

	case TT_COL:
	case TT_COLSPEC:
	case TT_ROW:
	case TT_TBODY:
	case TT_TFOOT:
	case TT_TGROUP:
	case TT_THEAD:
	{
		X_VerifyParseState(_PS_Table);
		break;
	}

	// Revision elements:

	case TT_REVHISTORY:
	{
		X_VerifyParseState(_PS_RevisionSec);
		m_parseState = _PS_MetaData;
		break;
	}

	case TT_REVISION:
	{
		X_VerifyParseState(_PS_Revision);
		m_parseState = _PS_RevisionSec;
		break;
	}

	case TT_REVREMARK:
	case TT_REVDESCRIPTION:
	case TT_REVNUMBER:
	{
		//TODO: import revisions
		X_VerifyParseState(_PS_Revision);
		break;
	}

	case TT_EQUATION:
	{
		X_VerifyParseState(_PS_Block);
		m_bInMath = false;
		break;
	}

	case TT_ALT:
	{
		X_CheckError(m_bInMath);
		break;
	}

	case TT_TEXTOBJECT:
	{
		if(m_iDataDepth > 0)
			m_parseState = _PS_DataSec;

		break;
	}

	case TT_GRAPHICCO:
	{
		X_VerifyParseState(_PS_Block);

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		if(m_iBlockDepth == 0)
			m_parseState = _PS_Sec;

		break;
	}

	case TT_MEDIAOBJECTCO:
	{
		X_VerifyParseState(_PS_DataSec);
		m_iDataDepth--;

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		if(m_iDataDepth == 0)
			m_parseState = _PS_Sec;

		break;
	}

	case TT_AREA:
	case TT_AREASET:
	case TT_AREASPEC:
	{
		X_CheckError((m_parseState == _PS_Block) || (m_parseState == _PS_DataSec));
		break;
	}

	case TT_SCREENSHOT:
	{
		X_VerifyParseState(_PS_DataSec);
		m_parseState = _PS_Block;
		m_iDataDepth--;

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		if(m_iDataDepth > 0)
			m_parseState = _PS_DataSec;
		else if(m_iBlockDepth)
			m_parseState = _PS_Block;
		else
			m_parseState = _PS_Sec;

		break;
	}

	case TT_SCREEN:
	case TT_SYNOPSIS:
	{
		X_VerifyParseState(_PS_Block);

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		if(m_iBlockDepth == 0)
			m_parseState = _PS_Sec;

		break;
	}

	case TT_SCREENINFO:
	{
		X_VerifyParseState(_PS_Meta);
		m_parseState = _PS_DataSec;
		m_iDataDepth--;
		break;
	}

	case TT_GRAPHIC:
	{
		X_VerifyParseState(_PS_DataItem);
		m_iDataDepth--;

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		if(m_iDataDepth > 0)
			m_parseState = _PS_DataSec;
		else
			m_parseState = _PS_Block;

		break;
	}

	case TT_FIGURE:
	{
		X_VerifyParseState(_PS_DataSec);
		m_iDataDepth--;

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		if(m_iBlockDepth == 0)
			m_parseState = _PS_Sec;
		else if(m_iDataDepth == 0)
			m_parseState = _PS_Block;

		break;
	}

	case TT_VIDEOOBJECT:
	{
		X_VerifyParseState(_PS_DataSec);
		m_iDataDepth--;
		break;
	}

	case TT_VIDEODATA:
	{
		X_VerifyParseState(_PS_DataItem);
		m_parseState = _PS_DataSec;
		m_iDataDepth--;
		break;
	}

	case TT_MEDIAOBJECT:
	{
		X_VerifyParseState(_PS_DataSec);
		m_iDataDepth--;

		if(m_bRequiredBlock)
		{
			m_bRequiredBlock = false;
			m_iBlockDepth--;
		}

		if(m_iListDepth > 0)
			m_parseState = _PS_List;
		else if(m_bInMath)
			m_parseState = _PS_Block;
		else if(m_iDataDepth == 0)
			m_parseState = _PS_Sec;

		break;
	}

	case TT_IMAGEOBJECT:
	{
		X_VerifyParseState(_PS_DataSec);
		m_iDataDepth--;
		break;
	}

	case TT_IMAGEDATA:
	{
		X_VerifyParseState(_PS_DataItem);
		m_parseState = _PS_DataSec;
		m_iDataDepth--;
		break;
	}
	
	case TT_OTHER:
	default:
	    UT_DEBUGMSG(("Unknown or intentionally unhandled end tag [%s]\n",name));
		break;
	}

	if(bPop)
	{
		UT_uint32 i = 0;

		m_utnsTagStack.pop((UT_sint32*)&i);
		xxx_UT_DEBUGMSG(("Popping %d off of stack\n",i));

		if(i != tokenIndex) {
			UT_DEBUGMSG(("DocBook: Parse error!\n"));
        }
	}
}


/*
 * we redefine this function, so that it intercepts title
 * if a title has to been added, we first check that there is something to add
 * if so, we add the heading
 * if not, we skip the heading.
 * then, it calls its parent's charData.
 */
void IE_Imp_DocBook::charData(const gchar *s, int len)
{
	if (m_bMustAddTitle && (len > 0))
	{
		createTitle();
	}
	else if((m_parseState == _PS_Meta) && m_bReadBook)
	{
		return; //only set metadata once
	}
	else if((m_parseState == _PS_Meta) && (len > 0))
	{
		UT_UTF8String metaProp, updatedProp = "";

		switch(tagTop())
		{
			case TT_TITLE:
			{
				getDoc()->setMetaDataProp("dc.title",s);
				break;
			}

			case TT_AUTHOR:
			{
				getDoc()->setMetaDataProp("dc.creator",s);
				break;
			}

			case TT_FIRSTNAME:
			case TT_LINEAGE:
			case TT_OTHERNAME:
			case TT_SURNAME:
			{
				UT_ASSERT_HARMLESS(UT_TODO);
				break;
			}

			case TT_LEGALNOTICE:
			{
				getDoc()->setMetaDataProp("dc.rights",s);
				break;
			}

			case TT_PUBLISHERNAME:
			{
				getDoc()->setMetaDataProp("dc.publisher",s);
				break;
			}

			case TT_COLLABNAME:
			{
				getDoc()->setMetaDataProp("dc.contributor",s);
				break;
			}

			case TT_SUBJECTTERM:
			{
				getDoc()->setMetaDataProp("dc.subject",s);
				break;
			}

			case TT_KEYWORD:
			{
				if(getDoc()->getMetaDataProp (PD_META_KEY_KEYWORDS, metaProp) && metaProp.size())
				{
					updatedProp = metaProp;
					updatedProp += " "; //space the keywords
				}
				updatedProp += s;
				getDoc()->setMetaDataProp("abiword.keywords",(gchar*)updatedProp.utf8_str());
				break;
			}

			case TT_ABSTRACT:
			{
				getDoc()->setMetaDataProp("dc.description",s);
				break;
			}

			case TT_BIBLIOSOURCE:
			{
				getDoc()->setMetaDataProp("dc.source",s);
				break;
			}

			case TT_BIBLIOCOVERAGE:
			{
				getDoc()->setMetaDataProp("dc.coverage",s);
				break;
			}

			case TT_BIBLIORELATION:
			{
				getDoc()->setMetaDataProp("dc.relation",s);
				break;
			}

			case TT_APPLICATION:
			case TT_ARTPAGENUMS:
			case TT_AUTHORGROUP:
			case TT_COPYRIGHT:
			case TT_EDITION:
			case TT_ISSUENUM:
			case TT_KEYWORDSET:
			case TT_PRINTHISTORY:
			case TT_PUBDATE:
			case TT_VOLUMENUM:
			case TT_YEAR:
			{
				break; //these can be safely ignored
			}

			default:
			{
				UT_ASSERT_HARMLESS(UT_TODO);
				UT_DEBUGMSG(("Unhandled metadata in docbook importer: %d\n",tagTop()));
				break;
			}
		}
	}
	else if((m_parseState == _PS_Cell) && (len > 0))
	{
		requireBlock();
	}
	else if((m_parseState == _PS_Field) || m_bInTOC)
	{
		return;  //ignore field text since it should be regenerated
	}
	else if(m_parseState == _PS_MetaData)
	{
		return;
	}
	else if((m_parseState == _PS_Block) && (len >0))
	{
		if(tagTop() == TT_EMAIL)
		{
			const gchar *buf[3];
			buf[2] = NULL;

			UT_UTF8String link = "mailto:";
			link += s;

			buf[0] = "xlink:href";
			buf[1] = (gchar*)link.utf8_str();
			X_CheckError(appendObject(PTO_Hyperlink, const_cast<const gchar **>(buf)));
		}
	}

	IE_Imp_XML :: charData (s, len);
}
/*****************************************************************************/


/*****************************************************************************/
/*
 * creates a new title in the doc.
 */
void IE_Imp_DocBook :: createTitle (void)
{
	UT_return_if_fail(m_iTitleDepth > 0);

	if (m_parseState == _PS_DataSec)
	{
		UT_ASSERT_HARMLESS(UT_TODO);
		return;
	}

	m_parseState = _PS_Block;

	/* list of attributes */
	const gchar *buf[11];
	memset(buf, 0, sizeof(buf));

	if(m_iTitleDepth > m_utvTitles.getItemCount())
	{
		m_utvTitles.addItem((fl_AutoNum *)NULL);
	}

	bool foundStyle = false;

	if(m_sectionRole.length())
	{
		foundStyle = true;
		if(!strcmp(m_sectionRole.utf8_str(), "Heading 1") || !strcmp(m_sectionRole.utf8_str(), "Heading 2") ||
			!strcmp(m_sectionRole.utf8_str(), "Heading 3") || !strcmp(m_sectionRole.utf8_str(), "Heading 4") ||
			!strcmp(m_sectionRole.utf8_str(), "Section Heading"))
		{
			buf[1] = g_strdup(m_sectionRole.utf8_str());
		}
		else if(!strcmp(m_sectionRole.utf8_str(), "Numbered Heading 1") || !strcmp(m_sectionRole.utf8_str(), "Numbered Heading 2") ||
				!strcmp(m_sectionRole.utf8_str(), "Numbered Heading 3") || !strcmp(m_sectionRole.utf8_str(), "Chapter Heading"))
		{
			buf[1] = g_strdup(m_sectionRole.utf8_str());
			m_bMustNumber = true;
		}
		else
		{
			foundStyle = false;
		}
	}

	if(!foundStyle)
	{
		switch (m_iTitleDepth)
		{
			case CHAPTER_HEADING:
			{
				/* we must add a chapter heading */
				buf[1] = "Chapter Heading";
				break;
			}

			case SECTION1_HEADING:
			{
				/* we must add a section heading */
				buf[1] = "Section Heading";
				break;
			}

			case SECTION2_HEADING:
			{
				/* we must add a heading 1 */
				if (m_bMustNumber)
				{
					buf[1] = "Numbered Heading 1";
				}
				else
				{
					buf[1] = "Heading 1";
				}
				break;
			}

			case SECTION3_HEADING:
			{
				/* we must add a heading 2 */
				if (m_bMustNumber)
				{
					buf[1] = "Numbered Heading 2";
				}
				else
				{
					buf[1] = "Heading 2";
				}
				break;
			}

			case SECTION4_HEADING:
			{
				/* we must add a heading 3 */
				if (m_bMustNumber)
				{
					buf[1] = "Numbered Heading 3";
				}
				else
				{
					buf[1] = "Heading 3";
				}
				break;
			}

			case SECTION5_HEADING:
			case SECTION6_HEADING:
			default:			
			{
				if (m_bMustNumber)
				{
					buf[1] = "Numbered Heading 3"; //there's no Numbered Heading 4
				}
				else
				{
					buf[1] = "Heading 4";
				}
				break;
			}
		}
	}

	if (m_bMustNumber)
	{
		/*
		 * we must add a numbered heading; that means that we must put 
		 * it into a list
		 */
		/* deletes previous lists of same level and above */
		for (UT_sint32 i = (m_iTitleDepth - 1); i < m_utvTitles.getItemCount(); i++)
		{
			if (i == 0) //always keep the first chapter title
				continue;

			fl_AutoNum * temp = m_utvTitles.getNthItem(i);
			DELETEP(temp);
		}
		buf[8] = PT_PROPS_ATTRIBUTE_NAME;

		if((m_utvTitles.getNthItem(m_iTitleDepth-1) == NULL))
		{
			// if a list doesn't exist at this depth, create it
			createList();
			buf[9] = "start-value:1; list-style:Numbered List";
		}
		else
		{
			buf[9] = "list-style:Numbered List";
		}

		/* ok now it's created, we should add the id and the parent id */

		buf[2] = PT_LEVEL_ATTRIBUTE_NAME;

		UT_UTF8String val;

		if(m_utvTitles[m_iTitleDepth - 1])
			UT_UTF8String_sprintf (val, "%d", m_utvTitles[m_iTitleDepth - 1]->getLevel());
		else
			val = "1";

		buf[3] = (gchar *)g_strdup(val.utf8_str());
		buf[4] = PT_LISTID_ATTRIBUTE_NAME;

		if(m_utvTitles[m_iTitleDepth - 1])
			UT_UTF8String_sprintf (val, "%d", m_utvTitles[m_iTitleDepth - 1]->getID());
		else
			UT_UTF8String_sprintf (val, "%d", ++m_iCurListID);

		buf[5] = (gchar *)g_strdup(val.utf8_str());
		buf[6] = PT_PARENTID_ATTRIBUTE_NAME;

		if(m_utvTitles[m_iTitleDepth - 1])
			UT_UTF8String_sprintf (val, "%d", m_utvTitles[m_iTitleDepth - 1]->getParentID());
		else
			val = "0";

		buf[7] = (gchar *)g_strdup(val.utf8_str());

	}

	buf[0] = PT_STYLE_ATTRIBUTE_NAME;

	if(buf[1] == NULL) //preventive code for appendStrux() below
		buf[0] = NULL;

	X_CheckError(appendStrux(PTX_Block, const_cast<const gchar **>(buf)));
	if (m_bMustNumber)
	{
		/* adds field */
		const gchar * buf2 [3];
		buf2[0] = PT_TYPE_ATTRIBUTE_NAME;
		buf2[1] = "list_label";
		buf2[2] = NULL;

		X_CheckError ( appendObject (PTO_Field, const_cast<const gchar **>(buf2)));
		X_CheckError ( appendFmt (const_cast<const gchar **>(buf2)));
		UT_UCSChar ucs = UCS_TAB;
		appendSpan(&ucs,1);
		_popInlineFmt();
	}
	X_CheckError ( appendFmt (static_cast<const gchar **>(NULL)));

	m_bMustAddTitle = false;
	m_bTitleAdded = true;

	if(foundStyle)
		FREEP(buf[1]);

	FREEP(buf[3]);
	FREEP(buf[5]);
	FREEP(buf[7]);
}
/*****************************************************************************/


/*****************************************************************************/
/*
 * this function creates a new list in the document, and insert it in the
 * list table
 */
void IE_Imp_DocBook :: createList (void)
{
	UT_return_if_fail(m_iTitleDepth);

	int pid = 0;

	if (m_iTitleDepth > 1)
	{
		for (int i = (m_iTitleDepth - 2); i >= 0; i--)
		{
			/* retrieves parent id, if available */
			if (m_utvTitles[i])
			{
				pid = m_utvTitles [i] -> getID ();
				break;
			}
		}
	}

	const gchar * lDelim = "";

	if(m_iTitleDepth == 1)
		lDelim = "Chapter %L.";
	else if(m_iTitleDepth == 2)
		lDelim = "Section %L.";
	else
		lDelim = "%L.";

	/* creates the new list */
	fl_AutoNum *an = new fl_AutoNum (
			m_iCurListID,
			pid,
			NUMBERED_LIST,
			1,
			(const gchar *)lDelim,
			(const gchar *)"",
			getDoc (),
			NULL
		);
	getDoc()->addList(an);

	/* register it in the vector */
	if(m_utvTitles.setNthItem((m_iTitleDepth - 1), an, NULL) == -1)
		UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);

	/* increment the id counter, so that it is unique */
	m_iCurListID++;
}
/*****************************************************************************/

/*****************************************************************************/
/*
 * this function creates a new image in the document
 */
void IE_Imp_DocBook :: createImage (const char *name, const gchar **atts)
{
	char * relative_file = UT_go_url_resolve_relative(m_szFileName, name);
	if(!relative_file)
		return;

	UT_UTF8String filename(relative_file);
	g_free(relative_file);

	FG_Graphic * pfg = 0;
	if (IE_ImpGraphic::loadGraphic (filename.utf8_str(), IEGFT_Unknown, &pfg) != UT_OK)
		return;

	const UT_ByteBuf * pBB = static_cast<FG_GraphicRaster *>(pfg)->getRaster_PNG();
	X_CheckError(pBB);

	UT_UTF8String dataid;
	UT_UTF8String_sprintf (dataid, "image%u", static_cast<unsigned int>(m_iImages++));

	const char *mime = g_strdup("image/png");
	X_CheckError (getDoc()->createDataItem (dataid.utf8_str(), false, pBB, reinterpret_cast<void *>(const_cast<char *>(mime)), NULL));

	const gchar *buf[5];
	buf[0] = "dataid";
	buf[1] = (gchar*)dataid.utf8_str();
	buf[2] = NULL;
	buf[4] = NULL;

	UT_UTF8String props;
	const gchar *p_val = NULL;

	p_val = _getXMLPropValue(static_cast<const gchar *>("depth"), atts);

	if(p_val)
	{
		props = "height:";
		props+= p_val;
	}

	p_val = _getXMLPropValue(static_cast<const gchar *>("width"), atts);

	if(p_val)
	{
		if(props.length()) //the image might not have a depth attribute
			props+= "; ";

		props+= "width:";
		props+= p_val;
	}

	if(props.length())
	{
		buf[2] = PT_PROPS_ATTRIBUTE_NAME;
		buf[3] = (gchar*)props.utf8_str();
	}

	X_CheckError(appendObject(PTO_Image, const_cast<const gchar **>(buf)));
	DELETEP(pfg);
}
/*****************************************************************************/

UT_uint32 IE_Imp_DocBook::tagTop(void)
{
	UT_sint32 i = 0;

	if (m_utnsTagStack.viewTop (i))
		return (UT_uint32)i;
	return 0;
}

void IE_Imp_DocBook::requireBlock(void)
{
	if(!m_iBlockDepth)
	{
		m_iBlockDepth = 1;
		X_CheckError(appendStrux(PTX_Block,static_cast<const gchar **>(NULL)));

		if(m_parseState == _PS_Cell)
			m_bWroteEntryPara = true;
		else
			m_bRequiredBlock = true;

		m_parseState = _PS_Block;
	}
}
