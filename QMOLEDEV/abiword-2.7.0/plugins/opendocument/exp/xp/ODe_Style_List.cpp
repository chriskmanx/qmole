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
#include "ODe_Style_List.h"

// Internal includes
#include "ODe_Common.h"
#include "ODe_ListLevelStyle.h"

// AbiWord includes
#include <pp_AttrProp.h>


/**
 * Destructor
 */
ODe_Style_List::~ODe_Style_List() {
    UT_GenericVector<ODe_ListLevelStyle*>* pVector;
    
    pVector = m_levelStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_ListLevelStyle*, (*pVector));
    
    m_levelStyles.clear();
}


/**
 * Write the <text:list-style> element.
 */
bool ODe_Style_List::write(GsfOutput* pODT,
                           const UT_UTF8String& rSpacesOffset) const {
                            
    UT_uint32 i, count;
    UT_UTF8String subElementSpacesOffset;
    UT_UTF8String output;
    UT_GenericVector<ODe_ListLevelStyle*>* pVector;
    bool ok;
    
    UT_UTF8String_sprintf(output, "%s<text:list-style style:name=\"%s\">\n",
                          rSpacesOffset.utf8_str(), m_name.utf8_str());
    ODe_writeUTF8String(pODT, output);
    
    subElementSpacesOffset = rSpacesOffset;
    subElementSpacesOffset += " ";
    
    pVector = m_levelStyles.enumerate();
    count = pVector->getItemCount();
    for (i=0; i<count; i++) {
        ok = (*pVector)[i]->write(pODT, subElementSpacesOffset);
        if (!ok) {
            return false;
        }
    }
    
    UT_UTF8String_sprintf(output, "%s</text:list-style>\n",
                          rSpacesOffset.utf8_str());
    ODe_writeUTF8String(pODT, output);
    
    return true;
}


/**
 * 
 */
void ODe_Style_List::setLevelStyle(UT_uint8 level, const PP_AttrProp& rBlockAP) {
    
    UT_UTF8String levelString;
    ODe_ListLevelStyle* pLevelStyle;
    const gchar* pValue = NULL;
    bool ok;
    
    UT_UTF8String_sprintf(levelString, "%u", level);
    
    pLevelStyle = m_levelStyles.pick(levelString.utf8_str());
    
    if (pLevelStyle != NULL) {
        // This level style aws already set. There's nothing to be done.
        return;
    }
    
    ok = rBlockAP.getProperty("list-style", pValue);
    UT_return_if_fail(ok && pValue);
    
    if (!strcmp(pValue, "Numbered List") ||
        !strcmp(pValue, "Lower Case List") ||
        !strcmp(pValue, "Upper Case List") ||
        !strcmp(pValue, "Lower Roman List") ||
        !strcmp(pValue, "Upper Roman List") ||
        !strcmp(pValue, "Hebrew List") ||
        !strcmp(pValue, "Arabic List")) {
            
        pLevelStyle = new ODe_Numbered_ListLevelStyle();

    } else if (!strcmp(pValue, "Bullet List") ||
               !strcmp(pValue, "Dashed List") ||
               !strcmp(pValue, "Square List") ||
               !strcmp(pValue, "Triangle List") ||
               !strcmp(pValue, "Diamond List") ||
               !strcmp(pValue, "Star List") ||
               !strcmp(pValue, "Tick List") ||
               !strcmp(pValue, "Box List") ||
               !strcmp(pValue, "Hand List") ||
               !strcmp(pValue, "Heart List") ||
               !strcmp(pValue, "Implies List")) {
                
        pLevelStyle = new ODe_Bullet_ListLevelStyle();
        
        
    } else {
        UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
        return;
    }


    m_levelStyles.insert(levelString.utf8_str(), pLevelStyle);
    pLevelStyle->fetchAttributesFromAbiBlock(rBlockAP);
}


/**
 * 
 */
const ODe_ListLevelStyle* ODe_Style_List::getLevelStyle(UT_uint8 level) const {
    UT_UTF8String levelString;
    
    UT_UTF8String_sprintf(levelString, "%u", level);
    
    return m_levelStyles.pick(levelString.utf8_str());
}
