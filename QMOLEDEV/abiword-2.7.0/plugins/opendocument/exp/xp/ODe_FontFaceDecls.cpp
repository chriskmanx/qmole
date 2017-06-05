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
#include "ODe_FontFaceDecls.h"

// Internal includes
#include "ODe_Common.h"


/**
 * Destructor
 */
ODe_FontFaceDecls::~ODe_FontFaceDecls() {
    UT_GenericVector<UT_UTF8String*>* pVector;
    UT_uint32 i, count;
    
    pVector = m_fontDecls.enumerate();
    count = pVector->getItemCount();
    for (i=0; i<count; i++) {
        delete (*pVector)[i];
    }
    DELETEP(pVector);
}


/**
 * Add a font face declaration
 */
void ODe_FontFaceDecls::addFont(const UT_UTF8String& rFontName) {
    
    UT_UTF8String* pFontDecl = NULL;
    
    if ( !rFontName.empty() &&
         !m_fontDecls.contains(rFontName.utf8_str(), pFontDecl) ) {
        
        pFontDecl = new UT_UTF8String();
        UT_UTF8String_sprintf(*pFontDecl,
            "  <style:font-face style:name=\"%s\" svg:font-family=\"%s\"/>\n",
            rFontName.utf8_str(),
            rFontName.utf8_str());
        
        // TODO: Do something useful here instead of just this name-family
        //       mapping.
        
        m_fontDecls.insert(rFontName.utf8_str(), pFontDecl);
    }
}


/**
 * Write the <office:font-face-decls> element.
 */
bool ODe_FontFaceDecls::write(GsfOutput* pODT) const {
    UT_GenericVector<UT_UTF8String*>* pVector;
    UT_uint32 i, count;

    pVector = m_fontDecls.enumerate();
    count = pVector->getItemCount();
    
    if (count > 0) {
    
        ODe_writeUTF8String(pODT, " <office:font-face-decls>\n");
    
        for (i=0; i<count; i++) {
            ODe_writeUTF8String(pODT, *((*pVector)[i]));
        }
        
        ODe_writeUTF8String(pODT, " </office:font-face-decls>\n");
    
    } else {
        ODe_writeUTF8String(pODT, " <office:font-face-decls/>\n");
    }
	return true;
}
