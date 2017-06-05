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

#ifndef _ODE_STYLE_PAGELAYOUT_H_
#define _ODE_STYLE_PAGELAYOUT_H_

// AbiWord includes
#include "ut_string_class.h"

// External includes
#include <gsf/gsf-output.h>

// AbiWord classes
class PD_Document;
class PP_AttrProp;

/**
 * A <style:page-layout> element
 */
class ODe_Style_PageLayout {
public:

    void setName (const gchar* pName) {
        m_name = pName;
    }
    
    void setName (const UT_UTF8String& rName) {
        m_name = rName;
    }
    
    const gchar* getName() const {return m_name.utf8_str();}
    
    void fetchAttributesFromAbiDoc(PD_Document* pAbiDoc);
    void fetchAttributesFromAbiSection(const PP_AttrProp* pAP);
    
    static bool hasPageLayoutInfo(const PP_AttrProp* pAP);
    
    // Write the <style:page-layout> element.
    bool write(GsfOutput* pODT, const UT_UTF8String& rSpacesOffset) const;

    const char * getPageMarginTop(void)
    { return m_marginTop.utf8_str();}
    const char * getPageMarginLeft(void)
    { return m_marginLeft.utf8_str();}
    const char * getPageMarginHeader(void)
    { return m_headerHeight.utf8_str();}

private:

    bool _haveHeaderInfo() const {return !m_headerHeight.empty();}
    bool _haveFooterInfo() const {return !m_footerHeight.empty();}

    // <style:page-layout>
    UT_UTF8String m_name; // style:name
    
    // <style:page-layout-properties>
    UT_UTF8String m_pageWidth;        // fo:page-width
    UT_UTF8String m_pageHeight;       // fo:page-height
    UT_UTF8String m_printOrientation; // style:print-orientation
    UT_UTF8String m_marginTop;        // fo:margin-top
    UT_UTF8String m_marginBottom;     // fo:margin-bottom
    UT_UTF8String m_marginLeft;       // fo:margin-left
    UT_UTF8String m_marginRight;      // fo:margin-right
    UT_UTF8String m_backgroundColor;  // fo:background-color
    
    // <style:header-style>
    //   <style:header-footer-properties>
    UT_UTF8String m_headerHeight; // svg:height
    
    // <style:footer-style>
    //   <style:header-footer-properties>
    UT_UTF8String m_footerHeight; // svg:height

    // <style:background-image>
    UT_UTF8String m_backgroundImage;  // xlink:href
};

#endif //_ODE_STYLE_PAGELAYOUT_H_
