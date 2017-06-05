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


#ifndef IE_IMP_MHT_H
#define IE_IMP_MHT_H

#include <stdio.h>

extern "C" {
#include <eps/eps.h>
}

#include "ut_string.h"

#include "ie_imp_XHTML.h"

#define IE_MIMETYPE_RELATED			"multipart/related"

//class UT_ByteBuf;
//class UT_StringPtrMap;
//class UT_Vector;

class UT_Multipart
{
	enum ContentTransferEncoding
	{
		cte_other,
		cte_base64,
		cte_quoted
	};
	enum ContentType
	{
		ct_other,
		ct_image,
		ct_html4,
		ct_xhtml
	};

public:
	UT_Multipart ();

	~UT_Multipart ();

	bool				insert (const char * name, const char * value);
	const char *		lookup (const char * name);

	bool				append (const char * buffer, UT_uint32 length);

	const UT_ByteBuf *	getBuffer () const { return m_buf; }
	UT_ByteBuf *		detachBuffer ();

	void				clear ();

	const char *		contentLocation () const { return m_location; }
	const char *		contentID ()       const { return m_id; }
	const char *		contentType ()     const { return m_type; }
	const char *		contentEncoding () const { return m_encoding; }

	bool				isBase64 () const { return (m_cte == cte_base64); }
	bool				isQuoted () const { return (m_cte == cte_quoted); }

	bool				isImage () const { return (m_ct == ct_image); }
	bool				isHTML4 () const { return (m_ct == ct_html4); }
	bool				isXHTML () const { return (m_ct == ct_xhtml); }

private:
	bool				append_Base64 (const char * buffer, UT_uint32 length);
	bool				append_Quoted (const char * buffer, UT_uint32 length);

	UT_StringPtrMap *	m_map;
	UT_ByteBuf *		m_buf;

	const char *		m_location;
	const char *		m_id;
	const char *		m_type;
	const char *		m_encoding;

	ContentTransferEncoding	m_cte;
	ContentType				m_ct;

	size_t				m_b64length;
	char				m_b64buffer[80];
};

class IE_Imp_MHT_Sniffer : public IE_ImpSniffer
{
	friend class IE_Imp;

public:
	IE_Imp_MHT_Sniffer ();
	~IE_Imp_MHT_Sniffer () { }

	virtual const IE_SuffixConfidence * getSuffixConfidence ();

	virtual const IE_MimeConfidence * getMimeConfidence ();

	UT_Confidence_t recognizeContents (const char * szBuf, UT_uint32 iNumbytes);

	bool getDlgLabels (const char ** szDesc, const char ** szSuffixList, IEFileType * ft);

	UT_Error constructImporter (PD_Document * pDocument, IE_Imp ** ppie);
};

class IE_Imp_MHT : public IE_Imp_XHTML
{
public:
	IE_Imp_MHT (PD_Document * pDocument);

	~IE_Imp_MHT ();

	UT_Error		importFile (const char * szFilename);

private:
	FG_Graphic *	importImage (const gchar * szSrc);

	UT_Error		importXHTML (const char * szFilename);
	UT_Error		importHTML4 (const char * szFilename);

	UT_Multipart *	importMultipart ();

	UT_Multipart *	m_document;
	UT_Vector *		m_parts;

	eps_t *	m_eps;
};

class MultiReader : public UT_XML::Reader
{
public:
	MultiReader (const UT_Byte * buffer, UT_uint32 length);
	virtual ~MultiReader ();

	virtual bool      openFile (const char * szFilename);
	virtual UT_uint32 readBytes (char * buffer, UT_uint32 length);
	virtual void      closeFile (void);

private:
	const UT_Byte * const	m_buffer;
	const UT_Byte *			m_bufptr;
	const UT_uint32			m_length;
};

#endif /* IE_IMP_MHT_H */
