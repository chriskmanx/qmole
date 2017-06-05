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
#include "ODe_HeadingSearcher_Listener.h"

// Internal includes
#include "ODe_AuxiliaryData.h"

// AbiWord includes
#include <pp_AttrProp.h>


/**
 * Constructor
 */
ODe_HeadingSearcher_Listener::ODe_HeadingSearcher_Listener(
                                    ODe_AuxiliaryData& rAuxiliaryData)
                                    :
                                    m_rAuxiliaryData(rAuxiliaryData) {
}


/**
 * 
 */
void ODe_HeadingSearcher_Listener::openTOC(const PP_AttrProp* pAP) {
    
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getProperty("toc-source-style1", pValue);
    if (ok && pValue != NULL) {
        m_rAuxiliaryData.m_headingStyles.addStyleName(pValue, 1);
    }
    
    ok = pAP->getProperty("toc-source-style2", pValue);
    if (ok && pValue != NULL) {
        m_rAuxiliaryData.m_headingStyles.addStyleName(pValue, 2);
    }
    
    ok = pAP->getProperty("toc-source-style3", pValue);
    if (ok && pValue != NULL) {
        m_rAuxiliaryData.m_headingStyles.addStyleName(pValue, 3);
    }
    
    ok = pAP->getProperty("toc-source-style4", pValue);
    if (ok && pValue != NULL) {
        m_rAuxiliaryData.m_headingStyles.addStyleName(pValue, 4);
    }
}
