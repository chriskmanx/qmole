/* -*- mode: C++; tab-width: 4; c-basic-offset: 4; -*- */

/* AbiWord
 * Copyright (C) 1998-2003 AbiSource, Inc.
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

#ifdef ABI_PLUGIN_BUILTIN
#define abi_plugin_register abipgn_xhtml_register
#define abi_plugin_unregister abipgn_xhtml_unregister
#define abi_plugin_supports_version abipgn_xhtml_supports_version
#endif

#include "xap_Module.h"

#ifdef XHTML_MULTIPART_SUPPORTED
#include "ie_imp_MHT.h"
#endif
#ifdef XHTML_HTML_TIDY_SUPPORTED
#include "tidyImporter.h"
#endif

ABI_PLUGIN_DECLARE("HTML")

/* we use a reference-counted sniffer
 */

#ifdef XHTML_MULTIPART_SUPPORTED
static IE_Imp_MHT_Sniffer * m_mht_sniffer = 0;
#endif

#ifdef XHTML_HTML_TIDY_SUPPORTED
static IE_Imp_Tidy_Sniffer * m_tidy_sniffer = 0;
#endif

ABI_FAR_CALL
int abi_plugin_register (XAP_ModuleInfo * mi)
{
#ifdef XHTML_MULTIPART_SUPPORTED
	if (!m_mht_sniffer)
	{
		m_mht_sniffer = new IE_Imp_MHT_Sniffer ();
	}
#endif
#ifdef XHTML_HTML_TIDY_SUPPORTED
	if (!m_tidy_sniffer)
	{
		m_tidy_sniffer = new IE_Imp_Tidy_Sniffer ();
	}
#endif

#ifdef XHTML_MULTIPART_SUPPORTED
	mi->name = "Multipart HTML Importer";
	mi->desc = "Import Multipart HTML Documents";
#else
	mi->name = "HTML Importer";
	mi->desc = "Import HTML Documents";
#endif
	mi->version = ABI_VERSION_STRING;
	mi->author = "Abi the Ant";
	mi->usage = "No Usage";

#ifdef XHTML_MULTIPART_SUPPORTED
	IE_Imp::registerImporter (m_mht_sniffer);
#endif
#ifdef XHTML_HTML_TIDY_SUPPORTED
	IE_Imp::registerImporter (m_tidy_sniffer);
#endif
	return 1;
}

ABI_FAR_CALL
int abi_plugin_unregister (XAP_ModuleInfo * mi)
{
	mi->name = 0;
	mi->desc = 0;
	mi->version = 0;
	mi->author = 0;
	mi->usage = 0;

#ifdef XHTML_MULTIPART_SUPPORTED
	if (m_mht_sniffer)
		{
			IE_Imp::unregisterImporter (m_mht_sniffer);
			delete m_mht_sniffer;
			m_mht_sniffer = 0;
		}
#endif
#ifdef XHTML_HTML_TIDY_SUPPORTED
	if (m_tidy_sniffer)
		{
			IE_Imp::unregisterImporter (m_tidy_sniffer);
			delete m_tidy_sniffer;
			m_tidy_sniffer = 0;
		}
#endif
	return 1;
}

ABI_FAR_CALL
int abi_plugin_supports_version (UT_uint32 /*major*/, UT_uint32 /*minor*/, UT_uint32 /*release*/)
{
  return 1;
}
