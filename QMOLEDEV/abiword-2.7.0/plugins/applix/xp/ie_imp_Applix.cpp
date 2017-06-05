/* -*- c-basic-offset: 4; tab-width: 4; indent-tabs-mode: t -*- */

/* AbiWord
 * Copyright (C) 1998-2001 AbiSource, Inc.
 * Copyright (C) 2001 Hubert Figuiere, Dom Lachowicz
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
#include "ie_imp_Applix.h"
#include "pd_Document.h"
#include "ut_growbuf.h"
#include "ut_bytebuf.h"
#include "xap_EncodingManager.h"
#include "ie_impexp_Applix.h"

// these should both be case insensitive
#define AX_STR_CMP(x, y) strcmp((x), (y))
#define AX_STRN_CMP(x, y, z) strncmp((x), (y), (z))

#define APPLIX_LINE_LENGTH 80 // applix does 80 to a line
#define APPLIX_MAX_LINE_LENGTH 4096 // applix does does recommend reader to use 4KB buffer to prevent
                                    // broken writers to overflow the readers

#define nAxWords (sizeof(IE_Imp_Applix::axwords) / sizeof(IE_Imp_Applix::axwords[0]))


/*****************************************************************/
/*****************************************************************/

IE_Imp_Applix_Sniffer::IE_Imp_Applix_Sniffer (const char * _name) :
  IE_ImpSniffer(_name)
{
  // 
}

// supported suffixes
static IE_SuffixConfidence IE_Imp_Applix_Sniffer__SuffixConfidence[] = {
	{ "aw", 	UT_CONFIDENCE_PERFECT 	},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_Imp_Applix_Sniffer::getSuffixConfidence ()
{
	return IE_Imp_Applix_Sniffer__SuffixConfidence;
}

// supported mimetypes
static IE_MimeConfidence IE_Imp_Applix_Sniffer__MimeConfidence[] = {
	{ IE_MIME_MATCH_FULL, 	IE_MIMETYPE_Applix, UT_CONFIDENCE_GOOD 	},
	{ IE_MIME_MATCH_BOGUS, 	"", 				UT_CONFIDENCE_ZILCH }
};

const IE_MimeConfidence * IE_Imp_Applix_Sniffer::getMimeConfidence ()
{
	return IE_Imp_Applix_Sniffer__MimeConfidence;
}

UT_Confidence_t IE_Imp_Applix_Sniffer::recognizeContents(const char * szBuf, 
											  UT_uint32 iNumbytes)
{
	UT_uint32 iLinesToRead = 2;  // Only examine the first few lines of the file
	UT_uint32 iBytesScanned = 0;
	const char *p = szBuf;
	const char *magic;

	while( iLinesToRead-- )
	{
		magic = "<Applix Words>";
		if ( (iNumbytes - iBytesScanned) < strlen(magic) ) return(UT_CONFIDENCE_ZILCH);
		if ( strncmp(p, magic, strlen(magic)) == 0 ) return(UT_CONFIDENCE_PERFECT);

		/*  Seek to the next newline:  */
		while ( *p != '\n' && *p != '\r' )
		{
			iBytesScanned++; p++;
			if( iBytesScanned+2 >= iNumbytes ) return(UT_CONFIDENCE_ZILCH);
		}
		/*  Seek past the next newline:  */
		if ( *p == '\n' || *p == '\r' )
		{
			iBytesScanned++; p++;
			if ( *p == '\n' || *p == '\r' )
			{
				iBytesScanned++; p++;
			}
		}
	}
	return UT_CONFIDENCE_ZILCH;
}

UT_Error IE_Imp_Applix_Sniffer::constructImporter(PD_Document * pDocument,
												  IE_Imp ** ppie)
{
	IE_Imp_Applix * p = new IE_Imp_Applix(pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Imp_Applix_Sniffer::getDlgLabels(const char ** pszDesc,
										 const char ** pszSuffixList,
										 IEFileType * ft)
{
	*pszDesc = "Applix Word (.aw)";
	*pszSuffixList = "*.aw";
	*ft = getFileType();
	return true;
}

/*****************************************************************/
/*****************************************************************/

/*
 * Import Applix Word documents
 * 
 * One field per line, interesting text fields start after the <WP
 * Tag. Text fields ("<T") are immediately preceded by <P fields
 * 
 * For now, we only care about the stuff in-between the <start_flow>
 * And <end_flow> tags
 *
 * Interested in Plain-text import only right now. More complex import
 * To come
 *
 * Please, someone pick up on this and do a POW! - Dom
 */

/*****************************************************************/
/*****************************************************************/

#define X_CleanupIfError(error,exp)	do { if (((error)=(exp)) != UT_OK) goto Cleanup; } while (0)

UT_Error IE_Imp_Applix::_loadFile(GsfInput * fp)
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

IE_Imp_Applix::~IE_Imp_Applix()
{
}

IE_Imp_Applix::IE_Imp_Applix(PD_Document * pDocument)
	: IE_Imp(pDocument), m_textBuf(1024), m_axContext(axCtxVar)
{
	
}

/*****************************************************************/
/*****************************************************************/

#define X_ReturnIfFail(exp,error)		do { bool b = (exp); if (!b) return (error); } while (0)
#define X_ReturnNoMemIfError(exp)	X_ReturnIfFail(exp,UT_IE_NOMEMORY)

UT_Error IE_Imp_Applix::_writeHeader(GsfInput * /* fp */)
{
	X_ReturnNoMemIfError(appendStrux(PTX_Section, NULL));
	X_ReturnNoMemIfError(appendStrux(PTX_Block, NULL));

	return UT_OK;
}


IE_Imp_Applix::Applix_mapping_t IE_Imp_Applix::axwords[] =
{
    {"Applix", APPLIX_T},
    {"Globals", GLOBALS_T},
    {"start_styles", START_STYLES_T},
    {"end_styles", END_STYLES_T},
    {"style", STYLE_T},
    {"color", COLOR_T},
    {"start_flow", START_FLOW_T},
    {"end_flow", END_FLOW_T},
    {"WP400", WP400_T},
    {"text", TEXT_T},
    {"T", TEXT_T},
	{"page_break", PAGE_BREAK_T},
    {"para", PARA_T},
    {"P", PARA_T},
    {"start_vars", START_VARS_T},
    {"variable", VARIABLE_T},
    {"V", VARIABLE_T},
    {"end_vars", END_VARS_T},
    {"end_document", END_DOCUMENT_T},
    {"object", OBJECT_T},
    {"picture", PICTURE_T},
    {"section", SECTION_T},
    {"marker", MARKER_T},
	{"start_field", START_FIELD_T},
	{"S_F", START_FIELD_T},
	{"end_field", END_FIELD_T},
	{"E_F", END_FIELD_T},
	{"field_value", FIELD_VALUE_T},
	{"FV", FIELD_VALUE_T},
};



/*!
  Convert a tag string to a tag value. For tokenization.
  \param name tag string
  \param n size of content
  \return the tag value
  Convert a tag string to a tag value. For tokenization. It helps by 
  providing a code value that will helps dispatching to the right methods
  quickly.
  \fixme TODO: make it faster by using hash table or something else
  \see IE_Imp_Applix::s_getTagName
  \see IE_Imp_Applix::axwords
 */
IE_Imp_Applix::Applix_tag_t
IE_Imp_Applix::s_name_2_tag (const char *name, size_t n)
{
	if (!name || !n)
		return NOT_A_TAG;
	
	// simple lookup. TODO: make me faster (HT + BTree probably)
	for (size_t i = 0; i < nAxWords; i++)
	{
		if (!AX_STRN_CMP (name, axwords[i].name, n))
		{
			xxx_UT_DEBUGMSG(("DOM: applix tag is %s\n", axwords[i].name));
			return axwords[i].tag;
		}
	}
	
	return tag_Unknown;
}

/*!
  Get the tag from the buffer and return the tag value.
  \param str the buffer
  \param len the length of the buffer content
  \return the Applix_tag_t associated with tag
  Get the tag from the buffer and return the tag value. After extracting the
  tag from the line, call s_name_2_tag to get the tag value
  \see IE_Imp_Applix::s_name_2_tag
 */
// must free returned string (???? hub, 20010502)
IE_Imp_Applix::Applix_tag_t
IE_Imp_Applix::s_getTagName(const char *str, size_t len)
{
    Applix_tag_t tag;
    
	if (!len || !str)
		return NOT_A_TAG;

	// innocent, just for fast pointer comparison
	const char * ptr = static_cast<const char *>(str);
	
	UT_DEBUGMSG(("DOM: Applix string: %s (%d)\n", str, len));
	
	if(*ptr == '<')
	{
		ptr++;
		while(*ptr && !UT_UCS4_isspace(*ptr) && (*ptr != '>'))
		{
			ptr++;
		}
		if (*ptr)
		{
			char buf [APPLIX_LINE_LENGTH + 1];

			size_t n = ptr - str - 1;
			strncpy (buf, str+1, n);
			buf[n] = 0;

			xxx_UT_DEBUGMSG(("DOM: calling with %s\n", buf));

			tag = s_name_2_tag(buf, n);
			if ((tag == NOT_A_TAG) || (tag == tag_Unknown))
			{
			    UT_DEBUGMSG(("Tag %s is unknown\n", buf));
			}
			
			return tag;
		}
	}
	return NOT_A_TAG;
}

/*!
  Dispatch an ^yxx sequence to the right decoder
  \param str the buffer containing the sequence without ^ in head
  \param len the actual length of the buffer not includin the trailing 0. Must be at least 2 
  \retval c the output char
  \return the number of bytes read
  \note '^' is invalid and must be handle before
 */
short IE_Imp_Applix::s_decodeToUCS (const char *str, size_t len, UT_UCSChar * c)
{
	short ret;
	
	if ((*str >= 'a') && (*str <= 'p'))
	{
		ret = s_8bitsToUCS (str, len, c);
	}
	else if (((*str >= ' ') && (*str <= '_')) || (*str == '`'))
	{
		ret = s_16bitsToUCS (str, len, c);
	}
	else 
	{
		*c = 0;
		UT_DEBUGMSG (("invalid escape sequence\n"));
		ret = 0;
	}
	return ret;
}


/*!
  Converts an ^xx sequence for 8 bits chars to an UCSChar
  \param str the buffer containing the sequence without ^ in head
  \param len the actual length of the buffer not includin the trailing 0. Must be at least 2 
  \retval c the output char
  \return the number of bytes read
  \note the documentation is vague on this point, but I suspect that the charset
  \note is ISO-8859-1
 */
short IE_Imp_Applix::s_8bitsToUCS (const char *str, size_t len, UT_UCSChar * c)
{
    short decoded = 0;
    
    UT_ASSERT (len >= 2);
    UT_ASSERT (str);
    UT_ASSERT (c);
    UT_ASSERT (*str != '^');

    *c = 0;
    if (*str == '^')
    {
		return 0;
    }
    if (len >= 2) 
    {
		decoded = ((str[0] - 'a') << 4) + (str[1] - 'a');
		*c = decoded;
    }
	return 2;
}


/*!
  Converts an ^yxx sequence for 8 bits chars to an UCSChar
  \param str the buffer containing the sequence without ^ in head
  \param len the actual length of the buffer not includin the trailing 0. Must be at least 3
  \retval c the output char
  \return the number of bytes read
  \note the documentation is vague on this point, but I suspect that the charset is UNICODE encoded in UCS-2
 */
short IE_Imp_Applix::s_16bitsToUCS (const char *str, size_t len, UT_UCSChar * c)
{
    short decoded = 0;
	char x, y, z;
    
    UT_ASSERT (len >= 3);
    UT_ASSERT (str);
    UT_ASSERT (c);
    UT_ASSERT (*str != '^');

    *c = 0;
    if (*str == '^')
    {
		return 0;
    }
    if (len >= 3) 
    {
		// the ` us used instead of ". to simply we just replace.
		x = str[0];
		y = str[1];
		z = str[2];

		if (x == '`')
			x = '"';
		if (y == '`')
			y = '"';
		if (z == '`')
			z = '"';
		   
		decoded = ((x - ' ') << 10) + ((y - ' ') << 5) + (z - ' ');
		*c = decoded;
    }
	return 3;
}


UT_Error IE_Imp_Applix::_parseFile(GsfInput * fp)
{
    UT_ByteBuf buf (APPLIX_LINE_LENGTH + 1);
    size_t len = 0;
    Applix_tag_t tag;
	bool ok = true;
    
    while (!gsf_input_eof (fp))
    {
	    ok  = _applixGetLine (&buf, fp);
		if (ok)
		{
			len = strlen(reinterpret_cast<const char *>(buf.getPointer (0)));
			// todo: make me more robust
			// grammars? we don't need no stinkin' grammars! ;-(
			tag = s_getTagName(reinterpret_cast<const char *>(buf.getPointer(0)), len);
			if (tag != NOT_A_TAG) 
			{
				_dispatchTag (tag, reinterpret_cast<const char *>(buf.getPointer(0)), len);
			}
		}
    }

    return UT_OK;
}

#undef X_ReturnNoMemIfError
#undef X_ReturnIfFail

// borrowed from LGPL'd uclibc implementation
// http://buildroot.uclibc.org/cgi-bin/viewcvs.cgi/trunk/uClibc/libc/stdio/stdio.c
// Copyright (C) 1996 Robert de Bath <rdebath@cix.compulink.co.uk>
static char * fgets(char *s, int count, GsfInput * stream)
{
	int ch;
	char *p;
	
	p = s;
	while (count-- > 1) {		/* Guard against count arg == INT_MIN. */
		if(!gsf_input_read(stream, 1, (guint8*)&ch)) {
			if (gsf_input_eof(stream)) {
				break;
			} else {
				return 0;
			}
		}
		*p++ = ch;
		if (ch == '\n') {
			break;
		}
	}
	if ((s == p)) {
		return 0;
	}
	*p = 0;
	return s;
}

/*!
  Read an ApplixLine
  \retvalue pBuf a byte buffer
  \param fp the file to read from
  \return true if success.
  Read an Applix line. Reassemble it if need since it is able to analyse
  that the line is wrapped.
 */
bool
IE_Imp_Applix::_applixGetLine (UT_ByteBuf* pBuf, GsfInput *fp)
{
	UT_ASSERT (pBuf);
	char temp [APPLIX_MAX_LINE_LENGTH];
	char *res = NULL;
	size_t contentLen;
	char eol;
	bool done = false;
	short lineCount = 0;

	pBuf->truncate (0);
		
	do 
	{
		res = fgets (temp, APPLIX_MAX_LINE_LENGTH, fp);
		if (res == NULL) 
		{
			UT_DEBUGMSG (("Applix: fgets failed !\n"));
			return false;
		}
		// strip CRLF pairs
		contentLen = strlen (temp);
		eol = temp [contentLen - 1];
		while (((eol == '\n') || (eol == '\r')) && (contentLen > 0))
		{
			contentLen--;
			temp [contentLen] = 0;
			eol = temp [contentLen - 1];
		}
		
		if (lineCount > 0) 
		{
			if (temp [0] != ' ')
			{
				UT_DEBUGMSG(("Applix: continued line is not a space\n"));
				// assume end of line
				done = true;
				break;
			}
			// ZAP the space
			pBuf->append (reinterpret_cast<UT_Byte*>(&temp[1]), contentLen - 1);
		}
		else 
		{
			pBuf->append (reinterpret_cast<UT_Byte*>(&temp[0]), contentLen);
		}
		if (eol == '\\')
		{
			// line wrap
			lineCount++;
		}
		else 
		{
			// no \ so this is a single line
			done = true;
		}
	}
	while (!done);

	UT_DEBUGMSG (("Applix: read %d lines\n", lineCount));
	pBuf->append (reinterpret_cast<const UT_Byte*>(""), 1);  // add terminal 0
	return true;
}

/*!
  Dispatch the tag to the right handler.
  \param tag is the Applix tag value
  \param buf is the complete line for parsing. 
  \param len buffer content length
  \return void
  Dispatch the tag to the right handler.
  \note that _dispatchTag may itself read a line for line continuation.
  Since line continuation is predictable, this is not a problem.
  \see IE_Imp_Applix::s_getTagName
*/ 
void IE_Imp_Applix::_dispatchTag (Applix_tag_t tag, const char *buf, size_t len)
{
    switch (tag)
    {
	case PAGE_BREAK_T:
		_applixPageBreak (buf, len);
		break;
	case PARA_T:
		_applixNewPara (buf, len);
		break;
    case TEXT_T:
		if (m_axContext == axCtxFlow) 
		{
			_applixDecodeText (buf, len);
		}
		break;
	case START_STYLES_T:
		m_axContext = axCtxDef;
		break;
	case START_FLOW_T:
		m_axContext = axCtxFlow;
		break;
	case END_STYLES_T:
	case END_FLOW_T:
		m_axContext = axCtxNone;
		break;
    default:
		UT_DEBUGMSG (("Applix: unknown tag\n"));
		break;
    }
}

/*!
  Decode a Text tag.
  \param buf the buffer containing current reassembled line (ie not wrapped)
  \param len length of the content of buf
  \return void
  Decode a Text tag and send the output to the document as text spans.
  It will perform span attribute decoding too and char escape descoding.
  Format is 
  <T "text" attr>
  where T is T or Text, and "text" the text span encoded. 
 */
void IE_Imp_Applix::_applixDecodeText (const char *buf, size_t len)
{
	size_t pos = 0;
	UT_UCS4Char wc;
	UT_Byte    ch;
	UT_UCSChar c;	

	// be sure to discard buffer
	m_textBuf.truncate(0);
	while (buf [pos] != '"')
	{
		pos++;
		if (pos >= len) 
		{
			UT_DEBUGMSG (("buffer overflow\n"));
			break;
		}
	}
	//skip the opening "
	pos++;
	do 
	{
		ch = buf [pos];
		switch (ch)
		{
		case '\\':
			// handle escape
			pos++;
			ch = buf [pos];
			break;
		case '^':
			// handle encoded char
			pos++;
			ch = buf [pos];
			if (ch != '^')
			{
				short n;
				n = s_decodeToUCS (&buf [pos], len - pos, &c);
				pos += n - 1;    // -1 because we move to the last byte of encoding, not after. 
				m_textBuf.append(reinterpret_cast<const UT_GrowBufElement *>(&c), 1);
				ch = 0;
			}
			break;
		}
		// here if ch == 0 then don't add anything. NUL gets ignored
		if (ch) 
		{
			m_mbtowc.mbtowc(wc, ch);
			c = static_cast<UT_UCSChar>(wc);
			m_textBuf.append(reinterpret_cast<const UT_GrowBufElement *>(&c), 1);
		}
		pos++;
	}
	while ((pos < len) && (buf [pos] != '"'));

	// read the char props

	if (m_textBuf.getLength() > 0) 
	{
		appendSpan (reinterpret_cast<const UT_UCSChar *>(m_textBuf.getPointer (0)), m_textBuf.getLength ());
		m_textBuf.truncate(0);
	}
}

/*!
  Insert a new paragraph tag.
  \param buf the buffer containing current reassembled line (ie not wrapped)
  \param len length of the content of buf
  \return void
  Insert a new paragraph
  \todo TODO handle the style and paragraph attributes.
*/
void IE_Imp_Applix::_applixNewPara (const char */*buf*/, size_t /*len*/)
{
	// flush the current run
	UT_uint32 runlen;
	runlen = m_textBuf.getLength ();
	if (runlen > 0) 
	{
		xxx_UT_DEBUGMSG(("applix: flushing\n"));
		appendSpan (reinterpret_cast<const UT_UCSChar *>(m_textBuf.getPointer (0)), runlen);
	}
	
	xxx_UT_DEBUGMSG(("applix: new para\n"));
	appendStrux (PTX_Block, NULL);
}


/*!
  Decode a Page Break tag.
  \param buf the buffer containing current reassembled line (ie not wrapped)
  \param len length of the content of buf
  \return void
  Insert a page break in the current flow
  \todo TODO handle even/odd page, currently ignored by Abiword
*/
void IE_Imp_Applix::_applixPageBreak (const char * /*buf*/, size_t /*len*/)
{
	UT_UCSChar c;
	c = UCS_FF;
	m_textBuf.append(reinterpret_cast<const UT_GrowBufElement *>(&c), 1);
	appendSpan (reinterpret_cast<const UT_UCSChar *>(m_textBuf.getPointer (0)), m_textBuf.getLength ());
	m_textBuf.truncate(0);
}


/*****************************************************************/
/*****************************************************************/
