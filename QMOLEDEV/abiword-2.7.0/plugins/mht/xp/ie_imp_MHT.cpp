/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord: ie_imp_MHT - plugin for Multipart [X]HTML
 * 
 * Copyright (C) 2002 Francis James Franklin <fjf@alinameridon.com>
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
#include <string.h>
#include <ctype.h>
#include <fcntl.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

// AbiWord includes

#include "ut_assert.h"
#include "ut_debugmsg.h"
#include "ut_base64.h"
#include "ut_bytebuf.h"
#include "ut_hash.h"
#include "ut_vector.h"

#include "pd_Document.h"

#include "ie_impGraphic.h"
#include "ie_imp_MHT.h"
#include "ie_impexp_HTML.h"

#ifdef XHTML_HTML_TIDY_SUPPORTED
#include "tidyReader.h"
#endif
#ifdef XHTML_HTML_XML2_SUPPORTED
#include "ut_html.h"
#endif

/*****************************************************************/
/*****************************************************************/

IE_Imp_MHT_Sniffer::IE_Imp_MHT_Sniffer () :
	IE_ImpSniffer("AbiMHT::Multipart HTML")
{
	// 
}

// supported suffixes
static IE_SuffixConfidence IE_Imp_MHT_Sniffer__SuffixConfidence[] = {
	{ "mht", 	UT_CONFIDENCE_GOOD 		},
	{ "", 	UT_CONFIDENCE_ZILCH 	}
};

const IE_SuffixConfidence * IE_Imp_MHT_Sniffer::getSuffixConfidence ()
{
	return IE_Imp_MHT_Sniffer__SuffixConfidence;
}

// supported mimetypes
static IE_MimeConfidence IE_Imp_MHT_Sniffer__MimeConfidence[] = {
	{ IE_MIME_MATCH_FULL, 	IE_MIMETYPE_RELATED, 	UT_CONFIDENCE_GOOD 	},
	{ IE_MIME_MATCH_BOGUS, 	"", 					UT_CONFIDENCE_ZILCH }
};

const IE_MimeConfidence * IE_Imp_MHT_Sniffer::getMimeConfidence ()
{
	return IE_Imp_MHT_Sniffer__MimeConfidence;
}

static const char * s_strnstr (const char * haystack, UT_uint32 iNumbytes, const char * needle)
{
	UT_uint32 needle_length = static_cast<UT_uint32>(strlen (needle));
	UT_uint32 i = 0;

	if (needle_length > iNumbytes) return NULL;

	const char * ptr = haystack;
	const char * match = NULL;

	while (i < (iNumbytes - needle_length))
		{
			if (*ptr == *needle)
				if (strncmp (ptr, needle, needle_length) == 0)
					{
						match = ptr;
						break;
					}
			ptr++;
			i++;
		}
	return match;
}

UT_Confidence_t IE_Imp_MHT_Sniffer::recognizeContents (const char * szBuf, UT_uint32 iNumbytes)
{
	if (s_strnstr (szBuf, iNumbytes, IE_MIMETYPE_RELATED))
		if (s_strnstr (szBuf, iNumbytes, IE_MIMETYPE_HTML) ||
			s_strnstr (szBuf, iNumbytes, IE_MIMETYPE_XHTML))
			{
				return UT_CONFIDENCE_GOOD;
			}
	return UT_CONFIDENCE_ZILCH;
}

UT_Error IE_Imp_MHT_Sniffer::constructImporter (PD_Document * pDocument, IE_Imp ** ppie)
{
	IE_Imp_MHT * p = new IE_Imp_MHT (pDocument);
	*ppie = p;
	return UT_OK;
}

bool IE_Imp_MHT_Sniffer::getDlgLabels (const char ** pszDesc, const char ** pszSuffixList,
										IEFileType * ft)
{
	*pszDesc = "Multipart HTML (.mht)";
	*pszSuffixList = "*.mht";
	*ft = getFileType ();
	return true;
}

/*****************************************************************/
/*****************************************************************/

IE_Imp_MHT::IE_Imp_MHT (PD_Document * pDocument) :
	IE_Imp_XHTML(pDocument),
	m_document(0),
	m_parts(new UT_Vector),
	m_eps(0)
{
	// 
}

IE_Imp_MHT::~IE_Imp_MHT ()
{
	UT_VECTOR_PURGEALL(UT_Multipart *,(*m_parts));
	DELETEP(m_parts);
}

UT_Error IE_Imp_MHT::importFile (const char * szFilename)
{
	int fd_in = open (szFilename, O_RDONLY);
	if (fd_in < 0) return UT_ERROR;

	m_eps = eps_begin (INTERFACE_STREAM, &fd_in);
	if (m_eps == 0)
		{
			close (fd_in);
			return UT_ERROR;
		}

	bool bValid = false;

	for (header_t * h = eps_next_header (m_eps); h; h = eps_next_header (m_eps))
		{
			const char * name = reinterpret_cast<char *>(h->name);
			const char * data = reinterpret_cast<char *>(h->data);

			if (name && data)
				if (g_ascii_strcasecmp (name, "content-type") == 0)
					{
						UT_uint32 length = static_cast<UT_uint32>(strlen (data));
						if (s_strnstr (data, length, IE_MIMETYPE_RELATED))
							if (s_strnstr (data, length, IE_MIMETYPE_HTML) ||
								s_strnstr (data, length, IE_MIMETYPE_XHTML))
								{
									bValid = true;
								}
					}
			eps_header_free (m_eps);
		}

	UT_Error import_status = UT_OK;

	if (bValid)
		{
			while (eps_next_line (m_eps))
				{
					// nothing interesting here
				}
			int parts = 0;
			while ((!(m_eps->u->b->eof)) && (m_eps->content_type & CON_MULTI))
				{
					UT_Multipart * part = importMultipart ();
					if (part == 0) break;

					if (part->isXHTML () || part->isHTML4 ())
						{
							if (m_document)
								{
									UT_DEBUGMSG(("Multipart HTML document has multiple HTML regions!\n"));
									DELETEP(part);
									import_status = UT_IE_BOGUSDOCUMENT;
									break;
								}
							m_document = part;
						}
					if (m_parts->addItem (part) < 0)
						{
							UT_DEBUGMSG(("Multipart HTML: error appending part!\n"));
							DELETEP(part);
							import_status = UT_OUTOFMEM;
							break;
						}
				}
		}
	eps_end (m_eps);
	close (fd_in);

	if (m_document == 0)
		{
			UT_DEBUGMSG(("Multipart HTML document has no HTML regions!\n"));
			import_status = UT_IE_BOGUSDOCUMENT;
		}
	if (import_status == UT_OK)
		{
			if (m_document->isXHTML ())
				{
					import_status = importXHTML (szFilename);
				}
			else if (m_document->isHTML4 ())
				{
					import_status = importHTML4 (szFilename);
				}
			else import_status = UT_ERROR;
		}
	return import_status;
}

FG_Graphic * IE_Imp_MHT::importImage (const gchar * szSrc)
{
	bool bContentID = (strncmp ((const char *) szSrc, "cid:", 4) == 0);

	const UT_Multipart * part = 0;

	UT_uint32 count = m_parts->getItemCount ();
	for (UT_uint32 i = 0; i < count; i++)
		{
			const UT_Multipart * ptr = reinterpret_cast<const UT_Multipart *>((*m_parts)[i]);
			if (!ptr->isImage ()) continue;

			if (bContentID)
				{
					if (ptr->contentID ())
						if (strncmp (reinterpret_cast<const char *>(szSrc) + 4, ptr->contentID () + 1, strlen (static_cast<const char *> (szSrc)) - 4) == 0)
							{
								part = ptr;
								break;
							}
				}
			else
				{
					if (ptr->contentLocation ())
						if (strcmp (reinterpret_cast<const char *>(szSrc), ptr->contentLocation ()) == 0)
							{
								part = ptr;
								break;
							}
				}
		}
	if (part == 0)
		{
			UT_DEBUGMSG(("Multipart HTML: importImage: `%s' not an image, or not in archive\n",szSrc));
			return 0;
		}

	const UT_ByteBuf * pBB = part->getBuffer ();
	if (pBB == 0)
		{
			UT_DEBUGMSG(("Multipart HTML: importImage: `%s' - image in archive but not (or no longer?) loaded!\n",szSrc));
			return 0;
		}
	if (pBB->getLength () == 0)
		{
			UT_DEBUGMSG(("Multipart HTML: importImage: `%s' - image in archive but has no size!\n",szSrc));
			return 0;
		}

	IE_ImpGraphic * pieg = 0;
	if (IE_ImpGraphic::constructImporter (pBB, IEGFT_Unknown, &pieg) != UT_OK)
		{
			UT_DEBUGMSG(("unable to construct image importer!\n"));
			return 0;
		}
	if (pieg == 0) return 0;

	UT_Multipart * vol_part = const_cast<UT_Multipart *>(part);

	FG_Graphic * pfg = 0;
	UT_Error import_status = pieg->importGraphic (vol_part->detachBuffer (), &pfg);
	delete pieg;
	if (import_status != UT_OK)
		{
			UT_DEBUGMSG(("unable to import image!\n"));
			return 0;
		}
	UT_DEBUGMSG(("image loaded successfully\n"));

	return pfg;
}

UT_Error IE_Imp_MHT::importXHTML (const char * szFilename)
{
	const UT_Byte * buffer = m_document->getBuffer()->getPointer (0);
	UT_uint32 length = m_document->getBuffer()->getLength ();

	MultiReader wrapper(buffer,length);

	setReader (&wrapper);

	return IE_Imp_XHTML::importFile (szFilename);
}

UT_Error IE_Imp_MHT::importHTML4 (const char * szFilename)
{
	UT_Error e = UT_ERROR;

#ifdef XHTML_HTML_TIDY_SUPPORTED
	const UT_Byte * buffer = m_document->getBuffer()->getPointer (0);
	UT_uint32 length = m_document->getBuffer()->getLength ();

	TidyReader wrapper(buffer,length);
	setReader (&wrapper);

	e = IE_Imp_XHTML::importFile (szFilename);

	setReader (0);
#endif
#ifdef XHTML_HTML_XML2_SUPPORTED
	const UT_Byte * buffer = m_document->getBuffer()->getPointer (0);
	UT_uint32 length = m_document->getBuffer()->getLength ();

	UT_XML_BufReader wrapper(reinterpret_cast<const char *>(buffer),length);
	setReader (&wrapper);

	UT_HTML parser;
	setParser (&parser);

	e = IE_Imp_XHTML::importFile (szFilename);

	setParser (0);
	setReader (0);
#endif
	return e;
}

UT_Multipart * IE_Imp_MHT::importMultipart ()
{
	if (!mime_init_stream (m_eps)) return 0;

	UT_Multipart * part = new UT_Multipart;
	if (part == 0) return 0;

	for (header_t * h = mime_next_header (m_eps); h; h = mime_next_header (m_eps))
		{
			const char * name = reinterpret_cast<char *>(h->name);
			const char * data = reinterpret_cast<char *>(h->data);

			if (name && data) part->insert (name, data);

			header_kill (h);
		}
	bool bLoad = (part->isImage () || part->isXHTML () || part->isHTML4 ());

	for (unsigned char * l = mime_next_line (m_eps); l; l = mime_next_line (m_eps))
		{
			char * line = reinterpret_cast<char *>(l);
			UT_uint32 length = static_cast<UT_uint32>(strlen (line));

			if (bLoad && length) part->append (line, length);
		}
	return part;
}

UT_Multipart::UT_Multipart () :
	m_map(new UT_StringPtrMap),
	m_buf(new UT_ByteBuf),
	m_location(0),
	m_id(0),
	m_type(0),
	m_encoding(0),
	m_cte(cte_other),
	m_ct(ct_other),
	m_b64length(0)
{
	// 
}

UT_Multipart::~UT_Multipart ()
{
	clear ();

	DELETEP(m_map);
	DELETEP(m_buf);
}

bool UT_Multipart::insert (const char * name, const char * value)
{
	if (( name == 0) || ( value == 0)) return false;
	if ((*name == 0) || (*value == 0)) return false;

	char * new_value = g_strdup (value);
	if (new_value == 0) return false;

	if (!m_map->insert (name, new_value))
		{
			FREEP(new_value);
			return false;
		}

	if (g_ascii_strcasecmp (name, "content-transfer-encoding") == 0)
		{
			m_encoding = new_value;

			if (g_ascii_strcasecmp (new_value, "base64") == 0)
				{
					m_cte = cte_base64;
				}
			else if (g_ascii_strcasecmp (new_value, "quoted-printable") == 0)
				{
					m_cte = cte_quoted;
				}
			else m_cte = cte_other;
		}
	else if (g_ascii_strcasecmp (name, "content-location") == 0)
		{
			m_location = new_value;
		}
	else if (g_ascii_strcasecmp (name, "content-id") == 0)
		{
			m_id = new_value;
		}
	else if (g_ascii_strcasecmp (name, "content-type") == 0)
		{
			m_type = new_value;

			if (strncmp (new_value, IE_MIMETYPE_HTML, strlen (IE_MIMETYPE_HTML)) == 0)
				{
					m_ct = ct_html4;
				}
			else if (strncmp (new_value, IE_MIMETYPE_XHTML, strlen (IE_MIMETYPE_XHTML)) == 0)
				{
					m_ct = ct_xhtml;
				}
			else if (strncmp (new_value, "image/", 6) == 0)
				{
					m_ct = ct_image;
				}
			else m_ct = ct_other;
		}
	return true;
}

const char * UT_Multipart::lookup (const char * name)
{
	if ( name == 0) return 0;
	if (*name == 0) return 0;

	const void * vptr = m_map->pick (name);
	return reinterpret_cast<const char *>(vptr);
}

bool UT_Multipart::append (const char * buffer, UT_uint32 length)
{
	static const char * s_newline = "\n";

	if (m_buf == 0) return false;

	if ((buffer == 0) || (length == 0)) return true; // ??

	if (isBase64 ()) return append_Base64 (buffer, length);
	if (isQuoted ()) return append_Quoted (buffer, length);

	return (m_buf->append (reinterpret_cast<const UT_Byte *>(buffer), length) && m_buf->append (reinterpret_cast<const UT_Byte *>(s_newline), 1));
}

bool UT_Multipart::append_Base64 (const char * buffer, UT_uint32 length)
{
	bool success = true;

	char binbuffer[60];

	const char * bufptr = buffer;
	for (UT_uint32 i = 0; i < length; i++)
		{
			char c = *bufptr++;
			bool bEnd = (c == '=');

			unsigned char u = static_cast<unsigned char>(c);
			if (!isspace ((int) u)) m_b64buffer[m_b64length++] = c;

			if (bEnd || (m_b64length == 80) || ((i + 1 == length) && m_b64length && ((m_b64length & 0x03) == 0)))
				{
					const char * b64bufptr = m_b64buffer;

					char * binbufptr = binbuffer;
					size_t binlength = 60;

					UT_UTF8_Base64Decode (binbufptr, binlength, b64bufptr, m_b64length);
					if (m_b64length) memmove (m_b64buffer, b64bufptr, m_b64length);

					if (m_b64length > 3)
						{
							UT_DEBUGMSG(("Multipart HTML: append_Base64: oddness while decoding!\n"));
							success = false;
						}
					if (binlength < 60)
						if (!m_buf->append (reinterpret_cast<UT_Byte *>(binbuffer), 60 - binlength)) success = false;
				}
			if (bEnd || !success) break;
		}
	return success;
}

bool UT_Multipart::append_Quoted (const char * buffer, UT_uint32 length)
{
	char * str = 0;

	if (length > 78) // shouldn't be
		{
			str = (char *) malloc (length + 2);
			if (str == 0) return false;
		}
	else str = m_b64buffer;

	char hexbuf[3];
	hexbuf[2] = 0;

	bool suppressNewLine = false;

	const char * bufptr = buffer;
	const char * bufend = buffer + length;

	char * strptr = str;

	while ((bufptr < bufend) && !suppressNewLine)
		switch (*bufptr)
			{
			case '=':
				if (bufptr + 1 == bufend) suppressNewLine = true;
				else
					{
						bufptr++;
						hexbuf[0] = *bufptr++;
						hexbuf[1] = *bufptr++;

						unsigned int escape;
						if (sscanf (hexbuf, "%x", &escape) == 1) *strptr++ = static_cast<char>(escape & 0xff);
					}
				break;
			default:
				*strptr++ = *bufptr++;
				break;
			}
	if (!suppressNewLine) *strptr++ = '\n';
	*strptr = 0;

	bool success = m_buf->append (reinterpret_cast<UT_Byte *>(str), strlen (str));

	if (length > 80) FREEP(str);
	return success;
}

UT_ByteBuf * UT_Multipart::detachBuffer ()
{
	UT_ByteBuf * bufret = m_buf;
	m_buf = 0;
	return bufret;
}

void UT_Multipart::clear ()
{
	//UT_HASH_PURGEDATA (char *, m_map,  free);
	m_map->purgeData();
	m_map->clear ();

	if (m_buf) m_buf->truncate (0);
}

MultiReader::MultiReader (const UT_Byte * buffer, UT_uint32 length) :
	m_buffer(buffer),
	m_bufptr(buffer),
	m_length(length)
{
	// 
}

MultiReader::~MultiReader ()
{
	// 
}

bool MultiReader::openFile (const char * /* szFilename */)
{
	m_bufptr = m_buffer;
	return (m_buffer && m_length);
}

UT_uint32 MultiReader::readBytes (char * buffer, UT_uint32 length)
{
	UT_uint32 length_remaining = m_length - (m_bufptr - m_buffer);
	UT_uint32 length_copy = (length > length_remaining) ? length_remaining : length;

	if (buffer) memcpy (buffer, m_bufptr, length_copy);

	m_bufptr += length_copy;

	return length_copy;
}

void MultiReader::closeFile ()
{
	m_bufptr = m_buffer + m_length;
}
