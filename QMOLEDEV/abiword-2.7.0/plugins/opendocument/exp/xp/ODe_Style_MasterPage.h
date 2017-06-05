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

#ifndef _ODE_STYLE_MASTERPAGE_H_
#define _ODE_STYLE_MASTERPAGE_H_

// AbiWord includes
#include <ut_string_class.h>

// External includes
#include <gsf/gsf-output.h>
#include <stdio.h>

// AbiWord classes
class PP_AttrProp;

/**
 * A <style:master-page> element.
 */
class ODe_Style_MasterPage {
public:

    ODe_Style_MasterPage (const gchar* pName, const gchar* pPageLayoutName);
    
    virtual ~ODe_Style_MasterPage();

    void fetchAttributesFromAbiSection(const PP_AttrProp* pAP);

    void setName(const UT_UTF8String& rName) {
        m_name = rName;
    }
    
    // Write the <style:master-page> element.
    bool write(GsfOutput* pODT) const;
    
    GsfOutput* getHeaderContentTempFile() const {return m_pHeaderContentTemp;}
    GsfOutput* getFooterContentTempFile() const {return m_pFooterContentTemp;}
    
    const UT_UTF8String& getAbiHeaderId() const {return m_abiHeaderId;}
    const UT_UTF8String& getAbiFooterId() const {return m_abiFooterId;}
    
private:
    UT_UTF8String m_name;           // style:name
    UT_UTF8String m_pageLayoutName; // style:page-layout-name
    
    // <section header="2" ... >
    UT_UTF8String m_abiHeaderId;
    
    // <section footer="5" ... >
    UT_UTF8String m_abiFooterId;
    
    // Temporary files that will hold header and footer content.
    GsfOutput* m_pHeaderContentTemp;
    GsfOutput* m_pFooterContentTemp;
};

#endif //_ODE_STYLE_MASTERPAGE_H_
