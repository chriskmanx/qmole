/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord: tidyReader - plugin for Multipart [X]HTML
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


#ifndef TIDYREADER_H
#define TIDYREADER_H

#include <tidy/tidy.h>
#include <tidy/buffio.h>

#include "ut_xml.h"

class TidyReader : public UT_XML::Reader
{
public:
	TidyReader ();
	TidyReader (const UT_Byte * buffer, UT_uint32 length);

	virtual ~TidyReader ();

	virtual bool      openFile (const char * szFilename);
	virtual UT_uint32 readBytes (char * buffer, UT_uint32 length);
	virtual void      closeFile (void);

private:
	const UT_Byte * const	m_buffer;
	const UT_uint32			m_length;

	TidyDoc		m_tidy;

	TidyBuffer	m_outbuf;
	TidyBuffer	m_errbuf;
};

#endif
