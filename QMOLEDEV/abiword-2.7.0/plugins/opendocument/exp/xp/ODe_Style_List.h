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

#ifndef ODE_STYLE_LIST_H_
#define ODE_STYLE_LIST_H_

// AbiWord includes
#include <ut_hash.h>
#include <ut_string_class.h>

// External includes
#include <gsf/gsf-output.h>

// Internal classes
class ODe_ListLevelStyle;

// AbiWord classes
class PP_AttrProp;

/**
 * Represents a <text:list-style> element.
 */
class ODe_Style_List {
public:

    virtual ~ODe_Style_List();
    
    // Write this <text:list-style> element.
    bool write(GsfOutput* pODT, const UT_UTF8String& rSpacesOffset) const;
    
    void setName(const UT_UTF8String& rName) { m_name = rName; }
    const UT_UTF8String& getName() const {return m_name;}
    
    void setLevelStyle(UT_uint8 level, const PP_AttrProp& rBlockAP);
    const ODe_ListLevelStyle* getLevelStyle(UT_uint8 level) const;
    
    UT_GenericVector<ODe_ListLevelStyle*>* getListLevelStyles() {
        return m_levelStyles.enumerate();
    }

private:
    // style:name attribute
    UT_UTF8String m_name;

    // text:consecutive-numbering attribute
    bool m_bConsecutiveNumbering;

    UT_GenericStringMap<ODe_ListLevelStyle*> m_levelStyles;
};

#endif /*ODE_STYLE_LIST_H_*/
