/* -*- c-basic-offset: 4; tab-width: 4; indent-tabs-mode: t -*- */

/* AbiWord
 * Copyright (C) 2001 AbiSource, Inc.
 * Copyright (C) 2001 Dom Lachowicz
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

#include <string.h>
#include "ut_string.h"
#include "ut_bytebuf.h"
#include "ut_base64.h"
#include "pt_Types.h"
#include "ie_exp_Applix.h"
#include "pd_Document.h"
#include "pp_AttrProp.h"
#include "px_ChangeRecord.h"
#include "px_CR_Object.h"
#include "px_CR_Span.h"
#include "px_CR_Strux.h"
#include "ut_wctomb.h"
#include "xap_EncodingManager.h"
#include "ut_string_class.h"
#include "ie_impexp_Applix.h"

/**
 * TODO:
 *
 * All that this export filter handles now is plain (unformatted) text
 * It shouldn't be too hard to add paragraph and text formatting though
 * (rather trivial, actually). Also needed is a modified outputdata
 * method to support applix-special charaters (signified by ^blah)
 * Image support would also be nice, as would better handling of styles
 *
 * This would make a great POW. When you do the POW, please remove this TODO
 */

#define APPLIX_LINE 80 // Applix only allows 80 chars per line

/*****************************************************************/
/*****************************************************************/

IE_Exp_Applix_Sniffer::IE_Exp_Applix_Sniffer (const char * _name) :
  IE_ExpSniffer(_name)
{
  // 
}

UT_Confidence_t IE_Exp_Applix_Sniffer::supportsMIME (const char * szMIME)
{
	if (strcmp (szMIME, IE_MIMETYPE_Applix) == 0)
		{
			return UT_CONFIDENCE_GOOD;
		}
	return UT_CONFIDENCE_ZILCH;
}

bool IE_Exp_Applix_Sniffer::recognizeSuffix(const char * szSuffix)
{
	return (g_ascii_strcasecmp(szSuffix,".aw") == 0);
}

UT_Error IE_Exp_Applix_Sniffer::constructExporter(PD_Document * pDocument,
													 IE_Exp ** ppie)
{
	IE_Exp_Applix * p = new IE_Exp_Applix(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Exp_Applix_Sniffer::getDlgLabels(const char ** pszDesc,
											const char ** pszSuffixList,
											IEFileType * ft)
{
	*pszDesc = "Applix Words (.aw)";
	*pszSuffixList = "*.aw";
	*ft = getFileType();
	return true;
}

/*****************************************************************/
/*****************************************************************/

//////////////////////////////////////////////////////////////////
// a private listener class to help us translate the document
// into a Applix stream.  code is at the bottom of this file.
//////////////////////////////////////////////////////////////////

class s_Applix_Listener : public PL_Listener
{
public:
	s_Applix_Listener(PD_Document * pDocument,
		       IE_Exp_Applix * pie);
	virtual ~s_Applix_Listener();

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

protected:
	void				_closeBlock(void);
	void				_outputData(const UT_UCSChar * p, UT_uint32 length);
        void                            _write (const char *);
        void                            _write (const char * src, int len);
        void                            _writeln (const char *);
        void                            _openTag (const char *);
        void                            _closeTag (void);
        void                            _flush (void);

        void                            _openParagraph (PT_AttrPropIndex api);
        void                            _openSpan (PT_AttrPropIndex api);
        void                            _closeSpan (PT_AttrPropIndex api);

        void                            _writePreamble (void);
        void                            _writePostamble (void);
        void _resetBuffer (void);

	PD_Document *		m_pDocument;
	IE_Exp_Applix *		m_pie;
	bool				m_bInBlock;
        char m_buf[APPLIX_LINE + 1]; // not evil, applix does 80 chars per line
        int m_pos;
        bool m_bInSpan;
};

/*****************************************************************/
/*****************************************************************/

IE_Exp_Applix::IE_Exp_Applix(PD_Document * pDocument)
	: IE_Exp(pDocument)
{
	m_error = 0;
	m_pListener = NULL;
}

IE_Exp_Applix::~IE_Exp_Applix()
{
}

/*****************************************************************/
/*****************************************************************/

UT_Error IE_Exp_Applix::_writeDocument(void)
{
	m_pListener = new s_Applix_Listener(getDoc(),this);
	if (!m_pListener)
		return UT_IE_NOMEMORY;

	if (getDocRange())
		getDoc()->tellListenerSubset(static_cast<PL_Listener *>(m_pListener),getDocRange());
	else
		getDoc()->tellListener(static_cast<PL_Listener *>(m_pListener));
	DELETEP(m_pListener);
	
	return ((m_error) ? UT_IE_COULDNOTWRITE : UT_OK);
}

/*****************************************************************/
/*****************************************************************/

void s_Applix_Listener::_outputData(const UT_UCSChar * data, UT_uint32 length)
{
	const UT_UCSChar * pData;
	UT_String sBuf;

	UT_ASSERT(sizeof(char) == sizeof(UT_Byte));

	if (!m_bInBlock)
	{
		return;
	}

	for (pData=data; (pData<data+length); /**/)
	{
		switch (*pData)
		{
			
		default:
			if (*pData > 0x007f)
			{
				/*
				Try to convert to native encoding and if
				character fits into byte, output raw byte. This 
				is somewhat essential for single-byte non-latin
				languages like russian or polish - since
				tools like grep and sed can be used then for
				these files without any problem.
				Networks and mail transfers are 8bit clean
				these days.  - VH
				*/
				UT_UCSChar c = XAP_EncodingManager::get_instance()->try_UToNative(*pData);
				if (c==0 || c>255)
				{
					sBuf += UT_String_sprintf("&#x%x;",*pData++);
				}
				else
				{
					sBuf += static_cast<char>(c);
					pData++;
				}
			}
			else
			{
				sBuf += static_cast<char>(*pData++);
			}
			break;
		}
	}

	_write(sBuf.c_str(),sBuf.size());	
}

s_Applix_Listener::s_Applix_Listener(PD_Document * pDocument,
				     IE_Exp_Applix * pie)
{
	m_pDocument = pDocument;
	m_pie = pie;

	// when we are going to the clipboard, we should implicitly
	// assume that we are starting in the middle of a block.
	// when going to a file we should not.
	m_bInBlock = false;
	m_bInSpan = false;

	_resetBuffer (); // initialize the buffer
	_writePreamble ();
}

s_Applix_Listener::~s_Applix_Listener()
{
	_closeBlock();
	_writePostamble ();
	_flush ();
}

bool s_Applix_Listener::populate(PL_StruxFmtHandle /*sfh*/,
								  const PX_ChangeRecord * pcr)
{
	switch (pcr->getType())
	{
	case PX_ChangeRecord::PXT_InsertSpan:
		{
			const PX_ChangeRecord_Span * pcrs = static_cast<const PX_ChangeRecord_Span *> (pcr);

			PT_AttrPropIndex api = pcr->getIndexAP();
			_openSpan(api);

			PT_BufIndex bi = pcrs->getBufIndex();
			_outputData(m_pDocument->getPointer(bi),pcrs->getLength());
			
			_closeSpan(api);

			return true;
		}

	case PX_ChangeRecord::PXT_InsertObject:
		{
#if 0
			// TODO decide how to indicate objects in Applix output.
			
			const PX_ChangeRecord_Object * pcro = static_cast<const PX_ChangeRecord_Object *> (pcr);
			PT_AttrPropIndex api = pcr->getIndexAP();
			switch (pcro->getObjectType())
			{
			case PTO_Image:
				return true;

			case PTO_Field:
				return true;

				// todo: support these
			case PTO_Hyperlink:
			case PTO_Bookmark:
			  return true;

			default:
				UT_ASSERT(0);
				return false;
			}
#else
			return true;
#endif
		}

	case PX_ChangeRecord::PXT_InsertFmtMark:
		return true;

	default:
		UT_ASSERT(0);
		return false;
	}
}

bool s_Applix_Listener::populateStrux(PL_StruxDocHandle /*sdh*/,
									   const PX_ChangeRecord * pcr,
									   PL_StruxFmtHandle * psfh)
{
	UT_ASSERT(pcr->getType() == PX_ChangeRecord::PXT_InsertStrux);
	const PX_ChangeRecord_Strux * pcrx = static_cast<const PX_ChangeRecord_Strux *> (pcr);
	*psfh = 0;							// we don't need it.

	switch (pcrx->getStruxType())
	{
	case PTX_SectionEndnote:
	case PTX_SectionHdrFtr:
	case PTX_Section:
		{
			return true;
		}

	case PTX_Block:
		{
			_closeBlock();
			_openParagraph (pcr->getIndexAP());
			m_bInBlock = true;
			return true;
		}

	case PTX_SectionTable:
	case PTX_EndTable:
	case PTX_SectionCell:
	case PTX_EndCell:
	  return true;

	case PTX_EndFrame:
	case PTX_EndMarginnote:
	case PTX_EndFootnote:
	case PTX_SectionFrame:
	case PTX_SectionMarginnote:
	case PTX_SectionFootnote:
	case PTX_EndEndnote:
	default:
		UT_ASSERT_NOT_REACHED();
		return false;
	}
}

// this method has been very carefully hand-crafted
// to produce 80 chars per line output. don't mess with it
// -Dom
void s_Applix_Listener::_write (const char * src, int len)
{
  if (!src || !len) // short-circuit
    return;

  for (int i = 0; i < len; i++)
    {
      if (src[i] == '\n')
	{
	  _flush ();                     // flush and reset the buffer
	  m_pie->write ("\n", 1);        // write the newline
	}
      else // not a newline
	{
	  if (m_pos < (APPLIX_LINE - 2)) // plenty of room to append
	    {
	      m_buf[m_pos++] = src[i];
	    }
	  else // (m_pos == (APPLIX_LINE - 1))
	    {
	      if (i < (len - 1)) // more chars to write
		{
		  m_buf[m_pos++] = src[i]; // append the character
		  m_buf[m_pos++] = '\\';   // append a trailing '\' 
		  _flush ();               // flush the buffer
		  m_pie->write ("\n", 1);  // append a newline
		  m_buf[m_pos++] = ' ';    // append a space
		}
	      else
		{
		  m_buf[m_pos++] = src[i];
		}
	    }
	}
    }
}

void s_Applix_Listener::_flush (void)
{
  // flush the internal buffers
  m_pie->write (m_buf, m_pos); // write out the contents of the buffer
  _resetBuffer (); // reset the buffer and count
}

void s_Applix_Listener::_write (const char * src)
{
  if (src)
    _write (src, strlen (src));
}

void s_Applix_Listener::_writeln (const char * src)
{
  _write (src);
  _write ("\n");
}

void s_Applix_Listener::_openTag(const char * tag)
{
  _write ("<");
  _write (tag);
  _write (" ");
}

void s_Applix_Listener::_closeTag (void)
{
  _writeln (">");
}

bool s_Applix_Listener::change(PL_StruxFmtHandle /*sfh*/,
								const PX_ChangeRecord * /*pcr*/)
{
	UT_ASSERT(0);						// this function is not used.
	return false;
}

bool s_Applix_Listener::insertStrux(PL_StruxFmtHandle /*sfh*/,
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

bool s_Applix_Listener::signal(UT_uint32 /* iSignal */)
{
	UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
	return false;
}

void s_Applix_Listener::_writePreamble(void)
{
  // global stuff
  _writeln ("*BEGIN WORDS VERSION=430/320 ENCODING=7BIT");
  _writeln ("<Applix Words>");
  _writeln ("<Globals levelIndent:0 hyphMethod:0 headerMargin:500 footerMargin:394 changeBar Pos:0>");

  // styles - TODO: auto-generate these based on our styles
  _writeln ("<start_styles>");
  _write ("<style \"Normal\" nextStyle \"Normal\" no-pageBreak no-keepWith no-block justifyLeft "
	    "indentToLevel spellcheck firstIndent:0 leftIndent:0 rightIndent:0 lineSpacing:0 ");
  _write ("preParaSpacing:0 postParaSpacing:0 level:0 hyphZone:0 hyphMinFrag:0  no-bold "
	    "no-italic no-strikethru no-hidden no-caps no-underline hyphenate color:\"Black\" ");
  _write ("face:\"Palatino\" size:12 position:0 tag:\"\"  lB:0:0:\"\" rB:0:0:\"\" tB:0:0:\"\" "
	    "bB:0:0:\"\" hB:0:0:\"\" vB:0:0:\"\" shading:18:\"\":\"\":\"\" horizontalMargin:0 ");
  _writeln ("verticalMargin:0 dropShadow:0  localTabs lT:394  xposColumnRelative xpos:0 "
	    "yposParaRelative ypos:1 leftFrameMargin:126 rightFrameMargin:126 topFrameMargin:0 "
	    "bottomFrameMargin:0  >");
  _writeln ("<style \"footer\" parent \"Normal\" nextStyle \"footer\" indentToLevel level:0  "
	    "color:\"Black\"  localTabs cT:3347 rT:6299  >");
  _writeln ("<style \"header\" parent \"Normal\" nextStyle \"header\" indentToLevel level:0 "
	    "color:\"Black\"  localTabs cT:3347 rT:6299  >");
  _writeln ("<style \"heading 1\" parent \"Normal\" nextStyle \"heading_1\" indentToLevel "
	    "preParaSpacing:167 level:0  bold  >");
  _writeln ("<style \"heading 2\" parent \"heading 1\" nextStyle \"heading_2\" indentToLevel "
	    "level:0  size:14  >");
  _writeln ("<style \"heading 3\" parent \"Normal\" nextStyle \"Normal indent\" indentToLevel "
	    "level:0  bold  >");
  _writeln ("<style \"Normal indent\" parent \"Normal\" nextStyle \"Normal indent\" "
	    "indentToLevel firstIndent:394 leftIndent:394 level:0  color:\"Black\"  >");
  _writeln ("<style \"heading_1\" parent \"Normal\" >");

  // colors - these are usually localized to the user's environment
  // eg. - Schwarz, Blau, Wiess, Gelb, etc... we don't need to do that
  _writeln ("<color \"Black0\":0:0:0:255>");
  _writeln ("<color \"Black\":0:0:0:255>");
  _writeln ("<color \"Blue\":255:255:0:0>");
  _writeln ("<color \"Cyan\":255:0:0:0>");
  _writeln ("<color \"Green\":255:0:255:0>");
  _writeln ("<color \"Magenta\":0:255:0:0>");
  _writeln ("<color \"Red\":0:255:255:0>");
  _writeln ("<color \"Yellow\":0:0:255:0>");
  _writeln ("<color \"White\":0:0:0:0>");
  _writeln ("<color \"Dark Blue\":127:127:0:128>");
  _writeln ("<color \"Dark Cyan\":127:0:0:128>");
  _writeln ("<color \"Dark Green\":127:0:127:128>");
  _writeln ("<color \"Dark Magenta\":0:127:0:128>");
  _writeln ("<color \"Dark Red\":0:127:127:128>");
  _writeln ("<color \"Dark Yellow\":0:0:127:128>");
  _writeln ("<color \"Dark Gray\":0:0:0:128>");
  _writeln ("<color \"Light Gray\":0:0:0:63>");
  _writeln ("<color \"HtmlLinkDefault@\":255:255:0:0>");

  // end styles
  _writeln ("<end_styles>");

  // begin the document
  _writeln ("<start_flow>");
  _writeln ("<WP400 \"This file must be filtered to be read in WP 3.11\">");
}

void s_Applix_Listener::_writePostamble(void)
{
  // end of document
  _writeln ("<end_flow>");

  // this might have to get more interesting
  _writeln ("<start_vars>");
  _writeln ("<end_vars>");

  _writeln ("<end_document>");
  _writeln ("*END WORDS");
}

void s_Applix_Listener::_resetBuffer (void)
{
  memset (m_buf, 0, sizeof (m_buf));
  m_pos = 0;
}

void s_Applix_Listener::_openSpan(PT_AttrPropIndex /* always ignored */)
{
  _openTag ("T");
  _write ("\""); // begin text
  m_bInSpan = true;
}

void s_Applix_Listener::_openParagraph (PT_AttrPropIndex /*api*/)
{
  // TODO: this should get more complex, but this is a 1st rev
  _openTag ("P");
  _closeTag ();
}

void s_Applix_Listener::_closeSpan(PT_AttrPropIndex api)
{ 
  _write ("\""); // end text

  if (!api)
    _closeTag(); // just close the "<T" tag
  else
    {
      // TODO: this should get more interesting if api != 0
      _closeTag ();
    }

  m_bInSpan = false;
}

void s_Applix_Listener::_closeBlock(void)
{
	if (m_bInBlock)
	  m_bInBlock = false;
}
