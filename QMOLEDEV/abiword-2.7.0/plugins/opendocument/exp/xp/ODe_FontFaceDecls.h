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

#ifndef ODE_FONTFACEDECLS_H_
#define ODE_FONTFACEDECLS_H_

// AbiWord includes
#include <ut_hash.h>

// External includes
#include <gsf/gsf-output.h>

// AbiWord classes
class UT_UTF8String;

/**
 * This class represents a <office:font-face-decls> element.
 */
class ODe_FontFaceDecls {
public:

    virtual ~ODe_FontFaceDecls();

    void addFont(const UT_UTF8String& rFontName);
    
    // Write the <office:font-face-decls> element.
    bool write(GsfOutput* pODT) const;
    
private:

    UT_GenericStringMap<UT_UTF8String*> m_fontDecls;
};

#endif /*ODE_FONTFACEDECLS_H_*/
