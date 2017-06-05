/* AbiSource
 * 
 * Copyright (C) 2005 INdT
 * Author: Daniel d'Andrada T. de Carvalho <daniel.carvalho@indt.org.br>
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
 
// Class definition include
#include "ODe_Style_MasterPage.h"

// Internal includes
#include "ODe_Common.h"

// AbiWord includes
#include <pp_AttrProp.h>
#include <gsf/gsf-output-memory.h>
#include <gsf/gsf-input-memory.h>


/**
 * Constructor
 */
ODe_Style_MasterPage::ODe_Style_MasterPage (const gchar* pName,
                                            const gchar* pPageLayoutName) {

    m_name = pName;
    m_pageLayoutName = pPageLayoutName;

    m_pFooterContentTemp = gsf_output_memory_new ();
    m_pHeaderContentTemp = gsf_output_memory_new ();
}


/**
 * Destructor
 */
ODe_Style_MasterPage::~ODe_Style_MasterPage() {
    if (m_pHeaderContentTemp != NULL) {
        ODe_gsf_output_close(m_pHeaderContentTemp);
    }
    
    if (m_pFooterContentTemp != NULL) {
        ODe_gsf_output_close(m_pFooterContentTemp);
    }
}


/**
 * Fetches info from an AbiWord <section> tag (from its attributes as properties).
 */
void ODe_Style_MasterPage::fetchAttributesFromAbiSection(const PP_AttrProp* pAP) {
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getAttribute("header", pValue);
    if (ok && pValue != NULL) {
        m_abiHeaderId = pValue;
    }
    
    ok = pAP->getAttribute("footer", pValue);
    if (ok && pValue != NULL) {
        m_abiFooterId = pValue;
    }
}


/**
 * 
 */
bool ODe_Style_MasterPage::write(GsfOutput* pODT) const {
    
    UT_UTF8String output;
    
    UT_UTF8String_sprintf(output,
        "  <style:master-page style:name=\"%s\" style:page-layout-name=\"%s\">\n",
        m_name.utf8_str(), m_pageLayoutName.utf8_str());
        
    ODe_writeUTF8String(pODT, output);
    
    if (!m_abiHeaderId.empty()) {
        // It has a header
        ODe_writeUTF8String(pODT, "   <style:header>\n");

	ODe_gsf_output_write(pODT, gsf_output_size (m_pHeaderContentTemp), 
			     gsf_output_memory_get_bytes (GSF_OUTPUT_MEMORY (m_pHeaderContentTemp)));
        
        ODe_writeUTF8String(pODT, "   </style:header>\n");
    }

    if (!m_abiFooterId.empty()) {
        // It has a footer
        ODe_writeUTF8String(pODT, "   <style:footer>\n");

	ODe_gsf_output_write(pODT, gsf_output_size (m_pFooterContentTemp), 
			     gsf_output_memory_get_bytes (GSF_OUTPUT_MEMORY (m_pFooterContentTemp)));

        ODe_writeUTF8String(pODT, "   </style:footer>\n");
    }


    ODe_writeUTF8String(pODT, "  </style:master-page>\n");

    return true;
}
