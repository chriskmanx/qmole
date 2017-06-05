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
#include "ODe_AutomaticStyles.h"

// Internal includes
#include "ODe_Common.h"

// Internal classes
#include "ODe_Style_Style.h"
#include "ODe_Style_PageLayout.h"
#include "ODe_Style_List.h"


/**
 * Destructor
 */
ODe_AutomaticStyles::~ODe_AutomaticStyles() {
    UT_GenericVector<ODe_Style_Style*>* pStyleVector;
    UT_GenericVector<ODe_Style_PageLayout*>* pPageLayoutVector;
    UT_GenericVector<ODe_Style_List*>* pListStyleVector;
    
    pStyleVector = m_textStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_Style*, (*pStyleVector));
    
    pStyleVector = m_paragraphStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_Style*, (*pStyleVector));
    
    pStyleVector = m_sectionStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_Style*, (*pStyleVector));
    
    pStyleVector = m_tableStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_Style*, (*pStyleVector));
    
    pStyleVector = m_tableColumnStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_Style*, (*pStyleVector));
    
    pStyleVector = m_tableRowStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_Style*, (*pStyleVector));
    
    pStyleVector = m_tableCellStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_Style*, (*pStyleVector));
    
    pStyleVector = m_graphicStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_Style*, (*pStyleVector));
    DELETEP(pStyleVector);

    pPageLayoutVector = m_pageLayouts.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_PageLayout*, (*pPageLayoutVector));
    DELETEP(pPageLayoutVector);

    pListStyleVector = m_listStyles.enumerate();
    UT_VECTOR_PURGEALL(ODe_Style_List*, (*pListStyleVector));
    DELETEP(pListStyleVector);
}


/**
 * See ODe_AutomaticStyles::_storeStyle
 */
void ODe_AutomaticStyles::storeTextStyle(ODe_Style_Style*& rpTextStyle) {
    _storeStyle(rpTextStyle, m_textStyles, "T");
}


/**
 * See ODe_AutomaticStyles::_storeStyle
 */
void ODe_AutomaticStyles::storeParagraphStyle(ODe_Style_Style*& rpParagraphStyle) {
    _storeStyle(rpParagraphStyle, m_paragraphStyles, "P");
}


/**
 * See ODe_AutomaticStyles::_storeStyle
 */
void ODe_AutomaticStyles::storeSectionStyle(ODe_Style_Style*& rpSectionStyle) {
    _storeStyle(rpSectionStyle, m_sectionStyles, "Sect");
}


/**
 * See ODe_AutomaticStyles::_storeStyle
 */
void ODe_AutomaticStyles::storeGraphicStyle(ODe_Style_Style*& rpGraphicStyle) {
    _storeStyle(rpGraphicStyle, m_graphicStyles, "graphic");
}


/**
 * 
 */
ODe_Style_Style* ODe_AutomaticStyles::addTableStyle(
                                            const UT_UTF8String& rStyleName) {
                                                
    ODe_Style_Style* pStyle;
   
    pStyle = new ODe_Style_Style();
    pStyle->setStyleName(rStyleName);
    pStyle->setFamily("table");
    
    m_tableStyles.insert(rStyleName.utf8_str(), pStyle);
    
    return pStyle;
}


/**
 * 
 */
ODe_Style_Style* ODe_AutomaticStyles::addTableColumnStyle(
                                            const UT_UTF8String& rStyleName) {
                                                
    ODe_Style_Style* pStyle;
   
    pStyle = new ODe_Style_Style();
    pStyle->setStyleName(rStyleName);
    pStyle->setFamily("table-column");
    
    m_tableColumnStyles.insert(rStyleName.utf8_str(), pStyle);
    
    return pStyle;
}


/**
 * 
 */
ODe_Style_Style* ODe_AutomaticStyles::addTableRowStyle(
                                            const UT_UTF8String& rStyleName) {
                                                
    ODe_Style_Style* pStyle;
   
    pStyle = new ODe_Style_Style();
    pStyle->setStyleName(rStyleName);
    pStyle->setFamily("table-row");
    
    m_tableRowStyles.insert(rStyleName.utf8_str(), pStyle);
    
    return pStyle;
}


/**
 * 
 */
ODe_Style_Style* ODe_AutomaticStyles::addTableCellStyle(
                                            const UT_UTF8String& rStyleName) {
    ODe_Style_Style* pStyle;
   
    pStyle = new ODe_Style_Style();
    pStyle->setStyleName(rStyleName);
    pStyle->setFamily("table-cell");
    
    m_tableCellStyles.insert(rStyleName.utf8_str(), pStyle);
    
    return pStyle;
}


/**
 * 
 */
ODe_Style_PageLayout* ODe_AutomaticStyles::addPageLayout() {
    ODe_Style_PageLayout* pStyle;
    UT_UTF8String styleName;
   
    UT_UTF8String_sprintf(styleName, "PLayout%d", m_pageLayouts.size() + 1);
    
    pStyle = new ODe_Style_PageLayout();
    pStyle->setName(styleName);
    
    m_pageLayouts.insert(styleName.utf8_str(), pStyle);
    
    return pStyle;
}


/**
 * 
 */
ODe_Style_List* ODe_AutomaticStyles::addListStyle() {
    ODe_Style_List* pStyle;
    UT_UTF8String styleName;
   
    UT_UTF8String_sprintf(styleName, "L%d", m_listStyles.size() + 1);
    
    pStyle = new ODe_Style_List();
    pStyle->setName(styleName);
    
    m_listStyles.insert(styleName.utf8_str(), pStyle);
    
    return pStyle;
}


/**
 * 
 */
void ODe_AutomaticStyles::addPageLayout(ODe_Style_PageLayout*& pPageLayout) {
    m_pageLayouts.insert(pPageLayout->getName(), pPageLayout);
}


/**
 * Writes <office:automatic-styles> element.
 */
void ODe_AutomaticStyles::write(GsfOutput* pContentStream) const {

    UT_GenericVector<ODe_Style_Style*>* pStyleVector;
    UT_GenericVector<ODe_Style_PageLayout*>* pPageLayoutVector;
    UT_GenericVector<ODe_Style_List*>* pListStyleVector;
    UT_uint32 i, count;
    UT_UTF8String spacesOffset = "  ";

    ODe_writeUTF8String(pContentStream, " <office:automatic-styles>\n");
    
#define ODE_WRITE_STYLES(styleMap) \
    pStyleVector = styleMap.enumerate(); \
    count = pStyleVector->getItemCount(); \
    for (i=0; i<count; i++) { \
        (*pStyleVector)[i]->write(pContentStream, spacesOffset); \
    } \
    DELETEP(pStyleVector);
    
    
    ODE_WRITE_STYLES (m_textStyles);
    ODE_WRITE_STYLES (m_paragraphStyles);
    ODE_WRITE_STYLES (m_sectionStyles);
    ODE_WRITE_STYLES (m_tableStyles);
    ODE_WRITE_STYLES (m_tableColumnStyles);
    ODE_WRITE_STYLES (m_tableRowStyles);
    ODE_WRITE_STYLES (m_tableCellStyles);
    ODE_WRITE_STYLES (m_graphicStyles);
    
#undef ODE_WRITE_STYLES
 
    pPageLayoutVector = m_pageLayouts.enumerate();
    count = pPageLayoutVector->getItemCount();
    for (i=0; i<count; i++) {
        (*pPageLayoutVector)[i]->write(pContentStream, spacesOffset);
    }
    
    pListStyleVector = m_listStyles.enumerate();
    count = pListStyleVector->getItemCount();
    for (i=0; i<count; i++) {
        (*pListStyleVector)[i]->write(pContentStream, spacesOffset);
    }
    
    ODe_writeUTF8String(pContentStream, " </office:automatic-styles>\n");
}


/**
 * Store the style in this automatic styles holder. As the specified
 * style get's stored here, this class takes care of freeing its memory later, so
 * you don't have to worry about freeing the memory of the stored style.
 * 
 * The style also get's it's unique name on this method.
 * 
 * After calling this method you may end up with your style pointer pointing to
 * a different style. It happens when there is already a stored style equivalent
 * to the one that you sent to be stored. The one that was passed is deleted.
 */
void ODe_AutomaticStyles::_storeStyle(ODe_Style_Style*& rpStyle,
                     UT_GenericStringMap<ODe_Style_Style*>& rStyles,
                     const char* pNamingPrefix) {
                        
    UT_GenericVector<ODe_Style_Style*>* pStyleVector;
    ODe_Style_Style* pStyle;
    bool isDuplicated;
    UT_uint32 i, count;
    
    pStyleVector = rStyles.enumerate();
    count = pStyleVector->getItemCount();
    
    for (i=0, isDuplicated=false; i<count && isDuplicated==false; i++) {
        
        pStyle = pStyleVector->getNthItem(i);
        if ( pStyle->isEquivalentTo(*rpStyle) ) {
            isDuplicated = true; // exit the loop
            delete rpStyle; // We don't want a duplicated style.
            rpStyle = pStyle;
        }
    }
    
    
    if (!isDuplicated) {
        // Let's name and store this style.
        UT_UTF8String styleName;
   
        UT_UTF8String_sprintf(styleName, "%s%d", pNamingPrefix, count+1);
        
        rpStyle->setStyleName(styleName);
        rStyles.insert(styleName.utf8_str(), rpStyle);
    }
}
