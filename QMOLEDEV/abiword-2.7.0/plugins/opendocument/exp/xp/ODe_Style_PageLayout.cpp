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
#include "ODe_Style_PageLayout.h"

// Internal includes
#include "ODe_Common.h"

// AbiWord includes
#include <ut_units.h>
#include <pd_Document.h>
#include <ut_string_class.h>
#include <pp_AttrProp.h>
#include <ut_locale.h>


/**
 * 
 */
void ODe_Style_PageLayout::fetchAttributesFromAbiDoc(PD_Document* pAbiDoc) {
    
    const gchar* pDimensionName;
    UT_Dimension docUnit;
    UT_LocaleTransactor t(LC_NUMERIC, "C");    

    docUnit = pAbiDoc->m_docPageSize.getDims();
    pDimensionName = UT_dimensionName(docUnit);
    
    UT_UTF8String_sprintf(m_pageWidth, "%f%s",
                          pAbiDoc->m_docPageSize.Width(docUnit),
                          pDimensionName);
                          
    UT_UTF8String_sprintf(m_pageHeight, "%f%s",
                          pAbiDoc->m_docPageSize.Height(docUnit),
                          pDimensionName);
                          
    if(pAbiDoc->m_docPageSize.isPortrait() == true) {
            m_printOrientation = "portrait";
    } else {
            m_printOrientation = "landscape";
    }
    
    UT_UTF8String_sprintf(m_marginTop, "%f%s",
                          pAbiDoc->m_docPageSize.MarginTop(docUnit),
                          pDimensionName);
                          
    UT_UTF8String_sprintf(m_marginBottom, "%f%s",
                          pAbiDoc->m_docPageSize.MarginBottom(docUnit),
                          pDimensionName);
                          
    UT_UTF8String_sprintf(m_marginLeft, "%f%s",
                          pAbiDoc->m_docPageSize.MarginLeft(docUnit),
                          pDimensionName);
                          
    UT_UTF8String_sprintf(m_marginRight, "%f%s",
                          pAbiDoc->m_docPageSize.MarginRight(docUnit),
                          pDimensionName);
}


/**
 * Fetch attributes from an AbiWord <section> tag. Mostly page margins.
 */
void ODe_Style_PageLayout::fetchAttributesFromAbiSection(const PP_AttrProp* pAP) {
    const gchar* pValue;
    bool ok;
    bool hasAbiHeaderMargin = false;
    bool hasAbiFooterMargin = false;
    double abiHeaderMarginCM = 0.0;
    double abiFooterMarginCM = 0.0;
    double abiTopMarginCM;
    double abiBottomMarginCM;
    UT_LocaleTransactor t(LC_NUMERIC, "C");    

    /*
     * The way on how margins (page, header and footer) are measured differs
     * from AbiWord and OpenDocument.
     * 
     * Some examples:
     * 
     * OpenDocument's top margin == AbiWord's header margin
     * 
     * OpenDocument's header height == AbiWord's top margin -
     *                                 AbiWord's header margin
     */

    ok = pAP->getProperty("page-margin-top", pValue);
    if (ok && pValue != NULL) {
        
        if (hasAbiHeaderMargin) {
            abiTopMarginCM = UT_convertToDimension(pValue, DIM_CM);
            
            UT_UTF8String_sprintf(m_headerHeight, "%fcm",
                                  abiTopMarginCM - abiHeaderMarginCM);
        } else {
            m_marginTop = pValue;
        }
    }
    if(m_marginTop.size() == 0)
    {
	m_marginTop = "1.0in";
    }
    
    ok = pAP->getProperty("page-margin-header", pValue);
    if (ok && pValue != NULL) {
        
        abiHeaderMarginCM = UT_convertToDimension(pValue, DIM_CM);
        
        // Set the header height
        abiTopMarginCM = UT_convertToDimension(m_marginTop.utf8_str(), DIM_CM);
        UT_UTF8String_sprintf(m_headerHeight, "%fcm",
                              abiTopMarginCM - abiHeaderMarginCM);
        
        // Redefine the top margin
        hasAbiHeaderMargin = true;
        UT_UTF8String_sprintf(m_marginTop, "%fcm",abiHeaderMarginCM);
    }

    ok = pAP->getProperty("page-margin-bottom", pValue);
    if (ok && pValue != NULL) {
        
        if (hasAbiFooterMargin) {
            abiBottomMarginCM = UT_convertToDimension(pValue, DIM_CM);
            
            UT_UTF8String_sprintf(m_footerHeight, "%fcm",
                                  abiBottomMarginCM - abiFooterMarginCM);
        } else {
            m_marginBottom = pValue;
        }
    }
    if(m_marginBottom.size() == 0)
    {
	m_marginBottom = "1.0in";
    }

    ok = pAP->getProperty("page-margin-footer", pValue);
    if (ok && pValue != NULL) {
        
        abiFooterMarginCM = UT_convertToDimension(pValue, DIM_CM);
        
        // Set the footer height
        abiBottomMarginCM = UT_convertToDimension(m_marginBottom.utf8_str(), DIM_CM);
        UT_UTF8String_sprintf(m_footerHeight, "%fcm",
                              abiBottomMarginCM - abiFooterMarginCM);
        
        // Redefine the bottom margin
        hasAbiFooterMargin = true;
        UT_UTF8String_sprintf(m_marginBottom, "%fcm",abiFooterMarginCM);
    }

    ok = pAP->getProperty("page-margin-left", pValue);
    if (ok && pValue != NULL) {
        m_marginLeft = pValue;
	UT_DEBUGMSG(("Found MarginLeft of section %s \n",pValue));
    }
    if(m_marginLeft.size() == 0)
    {
	m_marginLeft = "1.0in";
    }

    ok = pAP->getProperty("page-margin-right", pValue);
    if (ok && pValue != NULL) {
        m_marginRight = pValue;
    }
    if(m_marginRight.size() == 0)
    {
	m_marginRight = "1.0in";
    }

    ok = pAP->getProperty("background-color", pValue);
    if (ok && pValue && *pValue) {
        // TODO: handle transparent?
        m_backgroundColor = UT_colorToHex(pValue, true);
    }

    ok = pAP->getAttribute("strux-image-dataid", pValue);
    if (ok && pValue != NULL) {
        m_backgroundImage = pValue;
    }
}


/**
 * 
 */
bool ODe_Style_PageLayout::hasPageLayoutInfo(const PP_AttrProp* pAP) {
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getProperty("page-margin-header", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("page-margin-footer", pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    ok = pAP->getProperty("page-margin-top", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("page-margin-bottom", pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    ok = pAP->getProperty("page-margin-left", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("page-margin-right", pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    ok = pAP->getProperty("background-color", pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    ok = pAP->getAttribute("strux-image-dataid", pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    return false;
}


/**
 * Write the <style:page-layout> element.
 */
bool ODe_Style_PageLayout::write(GsfOutput* pODT,
                                 const UT_UTF8String& rSpacesOffset) const {
                                    
    UT_UTF8String output;

    UT_UTF8String_sprintf(output, "%s<style:page-layout style:name=\"%s\">\n",
        rSpacesOffset.utf8_str(), m_name.utf8_str());

    ODe_writeUTF8String(pODT, output);
    
    
    ////
    // <style:page-layout-properties>

    UT_UTF8String_sprintf(output, "%s <style:page-layout-properties",
        rSpacesOffset.utf8_str());

    ODe_writeAttribute(output, "fo:page-width", m_pageWidth);
    ODe_writeAttribute(output, "fo:page-height", m_pageHeight);
    ODe_writeAttribute(output, "style:print-orientation", m_printOrientation);
    ODe_writeAttribute(output, "fo:margin-top", m_marginTop);
    ODe_writeAttribute(output, "fo:margin-bottom", m_marginBottom);
    ODe_writeAttribute(output, "fo:margin-left", m_marginLeft);
    ODe_writeAttribute(output, "fo:margin-right", m_marginRight);
    ODe_writeAttribute(output, "fo:background-color", m_backgroundColor);

    if(m_backgroundImage.length()) {
        output += ">\n";
        output += UT_UTF8String_sprintf("%s  <style:background-image ",rSpacesOffset.utf8_str());
        output += "xlink:href=\"Pictures/";
        output += m_backgroundImage;
        output += ".png\" xlink:type=\"simple\" xlink:show=\"embed\" xlink:actuate=\"onLoad\" style:repeat=\"stretch\"/>\n";
        output += UT_UTF8String_sprintf("%s </style:page-layout-properties>\n",rSpacesOffset.utf8_str());
    } else {
        output += "/>\n";
    }

    ODe_writeUTF8String(pODT, output);
    
    
    ////
    // <style:header-style>
    //   <style:header-footer-properties>
    if (_haveHeaderInfo()) {
        
        // Write <style:header-style> tag        
        UT_UTF8String_sprintf(output, "%s <style:header-style>\n",
                              rSpacesOffset.utf8_str());
        ODe_writeUTF8String(pODT, output);
        
        // Write <style:header-footer-properties/> tag
        UT_UTF8String_sprintf(output, "%s  <style:header-footer-properties",
                              rSpacesOffset.utf8_str());
        ODe_writeAttribute(output, "svg:height", m_headerHeight);
        output += "/>\n";
        ODe_writeUTF8String(pODT, output);
        
        // Write </style:header-style> tag.
        UT_UTF8String_sprintf(output, "%s </style:header-style>\n",
                              rSpacesOffset.utf8_str());
        ODe_writeUTF8String(pODT, output);
    }
    
    ////
    // <style:footer-style>
    //   <style:header-footer-properties>
    if (_haveFooterInfo()) {
        
        // Write <style:footer-style> tag.
        UT_UTF8String_sprintf(output, "%s <style:footer-style>\n",
                              rSpacesOffset.utf8_str());
        ODe_writeUTF8String(pODT, output);
        
        // Write <style:header-footer-properties/> tag.
        UT_UTF8String_sprintf(output, "%s  <style:header-footer-properties",
                              rSpacesOffset.utf8_str());
        ODe_writeAttribute(output, "svg:height", m_footerHeight);
        output += "/>\n";
        ODe_writeUTF8String(pODT, output);
        
        // Write </style:footer-style> tag.
        UT_UTF8String_sprintf(output, "%s </style:footer-style>\n",
                              rSpacesOffset.utf8_str());
        ODe_writeUTF8String(pODT, output);
    }
    
    
    
    // Close the <style:page-layout> element.
    UT_UTF8String_sprintf(output, "%s</style:page-layout>\n",
        rSpacesOffset.utf8_str(), m_name.utf8_str());

    ODe_writeUTF8String(pODT, output);
    
    return true;
}
