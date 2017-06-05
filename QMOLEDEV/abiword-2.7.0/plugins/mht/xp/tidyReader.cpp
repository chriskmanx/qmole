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


#include <string.h>

#include "ut_debugmsg.h"

#include "tidyReader.h"

TidyReader::TidyReader () :
	m_buffer(0),
	m_length(0),
	m_tidy(0)
{
	memset (&m_outbuf, 0 , sizeof (TidyBuffer));
	memset (&m_errbuf, 0 , sizeof (TidyBuffer));
}

TidyReader::TidyReader (const UT_Byte * buffer, UT_uint32 length) :
	m_buffer(buffer),
	m_length(length),
	m_tidy(0)
{
	memset (&m_outbuf, 0 , sizeof (TidyBuffer));
	memset (&m_errbuf, 0 , sizeof (TidyBuffer));
}

TidyReader::~TidyReader ()
{
	if (m_tidy) closeFile ();
}

bool TidyReader::openFile (const char * szFilename)
{
	UT_DEBUGMSG(("using libtidy to parse HTML...\n"));

	m_tidy = tidyCreate ();
	if (m_tidy == 0) return false;

	if (tidyOptSetBool (m_tidy, TidyXhtmlOut, yes) == 0)
		{
			UT_DEBUGMSG(("tidyOptSetBool failed!\n"));
			closeFile ();
			return false;
		}
#ifndef DEBUG
	tidySetErrorBuffer (m_tidy, &m_errbuf);
#endif

	int parse_status;
	if (m_buffer && m_length)
		{
			UT_DEBUGMSG(("parse HTML in buffer...\n"));

			UT_Byte * buffer = const_cast<UT_Byte *>(m_buffer); // grr.

			TidyBuffer inbuf;

			tidyBufInit (&inbuf);
			tidyBufAttach (&inbuf, buffer, static_cast<unsigned int>(m_length));

			parse_status = tidyParseBuffer (m_tidy, &inbuf);

			tidyBufDetach (&inbuf);
		}
	else
		{
			UT_DEBUGMSG(("parse HTML in file: %s\n",szFilename));
			parse_status = tidyParseFile (m_tidy, szFilename);
		}
	if (parse_status < 0)
		{
			UT_DEBUGMSG(("tidyParseBuffer/File failed!\n"));
			closeFile ();
			return false;
		}

	parse_status = tidyCleanAndRepair (m_tidy);
	if (parse_status < 0)
		{
			UT_DEBUGMSG(("tidyCleanAndRepair failed!\n"));
			closeFile ();
			return false;
		}

	parse_status = tidyRunDiagnostics (m_tidy);
	if (parse_status < 0)
		{
			UT_DEBUGMSG(("tidyRunDiagnostics failed!\n"));
			closeFile ();
			return false;
		}

	if (parse_status > 1)
		{
			parse_status = (tidyOptSetBool (m_tidy, TidyForceOutput, yes) ? parse_status : -1);
		}
	if (parse_status < 0)
		{
			UT_DEBUGMSG(("tidyOptSetBool failed!\n"));
			closeFile ();
			return false;
		}

	parse_status = tidySaveBuffer (m_tidy, &m_outbuf);
	if (parse_status < 0)
		{
			UT_DEBUGMSG(("tidySaveBuffer failed!\n"));
			closeFile ();
			return false;
		}
	UT_DEBUGMSG(("tidy succeeded!\n"));
#ifdef DEBUG
	fputs ("================================================================\n", stderr);
	fputs ((const char *) m_outbuf.bp, stderr);
	fputs ("================================================================\n", stderr);
#endif
	m_outbuf.next = 0;

	return true;
}

UT_uint32 TidyReader::readBytes (char * buffer, UT_uint32 length)
{
	if (m_tidy == 0) return 0;

	unsigned int length_remaining = m_outbuf.size - m_outbuf.next;
	unsigned int length_copy = (length > length_remaining) ? length_remaining : length;

	if (buffer) memcpy (buffer, m_outbuf.bp + m_outbuf.next, length_copy);

	m_outbuf.next += length_copy;

	return (UT_uint32) length_copy;
}

void TidyReader::closeFile (void)
{
	if (m_tidy)
		{
			tidyBufFree (&m_outbuf);
#ifndef DEBUG
			tidyBufFree (&m_errbuf);
#endif
			tidyRelease (m_tidy);
			m_tidy = 0;
		}
	memset (&m_outbuf, 0 , sizeof (TidyBuffer));
	memset (&m_errbuf, 0 , sizeof (TidyBuffer));
}
