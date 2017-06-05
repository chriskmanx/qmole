/* AbiSource
 * 
 * Copyright (C) 2005 INdT
 * Author: Daniel d'Andrada T. de Carvalho <daniel.carvalho@indt.org.br>
 * Copyright 2009 AbiSource Corporation B.V.
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
#include "ODe_Style_Style.h"

// Internal includes
#include "ODe_Common.h"

// AbiWord includes
#include <pp_AttrProp.h>
#include <pt_Types.h>
#include <ut_locale.h>

// External includes
#include <ctype.h>


/*******************************************************************************
 * ODe_Style_Style
 ******************************************************************************/


/**
 * Constructor
 */
ODe_Style_Style::ODe_Style_Style() :
    m_pSectionProps(NULL),
    m_pParagraphProps(NULL),
    m_pTextProps(NULL),
    m_pTableProps(NULL),
    m_pColumnProps(NULL),
    m_pRowProps(NULL),
    m_pCellProps(NULL),
    m_pGraphicProps(NULL)
{
}


/**
 * Destructor
 */
ODe_Style_Style::~ODe_Style_Style() {
    DELETEP(m_pSectionProps);
    DELETEP(m_pParagraphProps);
    DELETEP(m_pTextProps);
    DELETEP(m_pTableProps);
    DELETEP(m_pColumnProps);
    DELETEP(m_pRowProps);
    DELETEP(m_pCellProps);
    DELETEP(m_pGraphicProps);
}


/**
 * Writes <style:style> and its subelements.
 * 
 * @param rSpacesOffset Space characters written at the beginning of
 *                      each new line.
 */
bool ODe_Style_Style::write(GsfOutput* pODT, const UT_UTF8String& rSpacesOffset) const {
    UT_UTF8String output;
    UT_UTF8String subOffset;
    UT_UTF8String escape;
    
    output += rSpacesOffset;
    output += "<style:style";
    
    if (m_name.empty()) {
        UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
        return false;
    }
    escape = m_name;
    output += " style:name=\"";
    output += escape.escapeXML();
    output += "\"";
    
    if (m_family.empty()) {
        UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
        return false;
    }
    output += " style:family=\"";
    output += m_family;
    output += "\"";
    
    escape = m_parentStyleName;
    escape.escapeXML();
    ODe_writeAttribute(output, "style:parent-style-name", escape);
    escape = m_nextStyleName;
    escape.escapeXML();
    ODe_writeAttribute(output, "style:next-style-name", escape);
    escape = m_masterPageName;
    escape.escapeXML();
    ODe_writeAttribute(output, "style:master-page-name", escape);

    
    if (isEmpty()) {
        // This style has no props at all.
        output += "/>\n";
        ODe_writeUTF8String(pODT, output);
        return true;
    } else {
        output += ">\n";
    }
    
    
    // Increase the offset for sub elements.
    subOffset = rSpacesOffset;
    subOffset += " ";

#define ODE_WRITE_STYLE_PROPS(pStyleProps) \
    if (pStyleProps) { pStyleProps->write(output, subOffset); }
    
    ODE_WRITE_STYLE_PROPS(m_pSectionProps);
    ODE_WRITE_STYLE_PROPS(m_pParagraphProps);
    ODE_WRITE_STYLE_PROPS(m_pTextProps);
    ODE_WRITE_STYLE_PROPS(m_pTableProps);
    ODE_WRITE_STYLE_PROPS(m_pColumnProps);
    ODE_WRITE_STYLE_PROPS(m_pRowProps);
    ODE_WRITE_STYLE_PROPS(m_pCellProps);
    ODE_WRITE_STYLE_PROPS(m_pGraphicProps);

#undef ODE_WRITE_STYLE_PROPS
    
    output += rSpacesOffset;
    output += "</style:style>\n";
    
    ODe_writeUTF8String(pODT, output);
    return true;
}


/**
 * Returns true if the specified PP_AttrProp contains properties that belongs to
 * <style:text-properties> elements
 */
bool ODe_Style_Style::hasTextStyleProps(const PP_AttrProp* pAP) {
    
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getProperty("color", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("text-decoration", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("text-position", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("font-family", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("font-size", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("lang", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("font-style", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("font-weight", pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    ok = pAP->getProperty("display", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    return false;
}


/**
 * Returns true if the specified PP_AttrProp contains properties that belongs to
 * <style:paragraph-properties> elements
 */
bool ODe_Style_Style::hasParagraphStyleProps(const PP_AttrProp* pAP) {
    
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getProperty("bgcolor", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("line-height", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("text-align", pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    ok = pAP->getProperty("text-indent", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("widows", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("orphans", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getAttribute("listid", pValue);
    if (ok && pValue != NULL) {
        // This block is a list item, so, its margin-left property is not
        // valid as paragraph attribute.
        // So, we don't check for it.
    } else {
        ok = pAP->getProperty("margin-left", pValue);
        if (ok && pValue != NULL) {
            return true;
        }
    }
    
    ok = pAP->getProperty("margin-right", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("margin-top", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("margin-bottom", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("keep-with-next", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    return false;
}


/**
 * 
 */
bool ODe_Style_Style::hasSectionInfo(const PP_AttrProp* pAP) {
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getProperty("columns", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("column-gap", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    ok= pAP->getProperty("page-margin-top",pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    ok= pAP->getProperty("page-margin-left",pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    ok= pAP->getProperty("page-margin-right",pValue);
    if (ok && pValue != NULL) {
        return true;
    }

    ok= pAP->getProperty("page-margin-bottom",pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("page-margin-header", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("page-margin-footer", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    return false;
}


/**
 * It does not take style names into consideration.
 * Read it like: "is style "T1" equivalent to style "T2"
 * It is *NOT* like: "is style A equal to style B"
 */
bool ODe_Style_Style::isEquivalentTo(const ODe_Style_Style& rStyle) {
    bool isEqual;
    
    isEqual =
         m_family          == rStyle.m_family &&
         m_parentStyleName == rStyle.m_parentStyleName &&
         m_nextStyleName   == rStyle.m_nextStyleName &&
         m_masterPageName  == rStyle.m_masterPageName;

    if (!isEqual) {return false;}


#define ODE_EQUAL_STYLE_PROPS(m_pProps) \
    if (m_pProps == NULL && rStyle.m_pProps == NULL) { \
        isEqual = true; \
    } else if (m_pProps != NULL && rStyle.m_pProps != NULL) { \
        isEqual = (*m_pProps) == (*rStyle.m_pProps); \
    } else { \
        isEqual = false; \
    } \
    if (!isEqual) {return false;}


    ODE_EQUAL_STYLE_PROPS(m_pSectionProps);
    ODE_EQUAL_STYLE_PROPS(m_pParagraphProps);
    ODE_EQUAL_STYLE_PROPS(m_pTextProps);
    ODE_EQUAL_STYLE_PROPS(m_pTableProps);
    ODE_EQUAL_STYLE_PROPS(m_pColumnProps);
    ODE_EQUAL_STYLE_PROPS(m_pRowProps);
    ODE_EQUAL_STYLE_PROPS(m_pCellProps);
    ODE_EQUAL_STYLE_PROPS(m_pGraphicProps);

#undef ODE_EQUAL_STYLE_PROPS
    
    // If not returned until here it's because they're equal.
    return true;
}


/**
 * 
 */
bool ODe_Style_Style::isEmpty() const {
    
#define ODE_IS_STYLE_PROPS_EMPTY(pStyleProps) \
    if (pStyleProps) { \
        if(!pStyleProps->isEmpty()) { return false; } \
    }
    
    ODE_IS_STYLE_PROPS_EMPTY(m_pSectionProps);
    ODE_IS_STYLE_PROPS_EMPTY(m_pParagraphProps);
    ODE_IS_STYLE_PROPS_EMPTY(m_pTextProps);
    ODE_IS_STYLE_PROPS_EMPTY(m_pTableProps);
    ODE_IS_STYLE_PROPS_EMPTY(m_pColumnProps);
    ODE_IS_STYLE_PROPS_EMPTY(m_pRowProps);
    ODE_IS_STYLE_PROPS_EMPTY(m_pCellProps);
    ODE_IS_STYLE_PROPS_EMPTY(m_pGraphicProps);

#undef ODE_IS_STYLE_PROPS_EMPTY
           
    return true;
}


/**
 * 
 */
ODe_Style_Style& ODe_Style_Style::operator=(const ODe_Style_Style& rStyle) {

#define ODE_COPY_STYLE_PROPS(m_pStyleProps, StyleProps) \
    if (rStyle.m_pStyleProps) { \
        if (m_pStyleProps == NULL) { \
            m_pStyleProps = new StyleProps(); \
        } \
        *m_pStyleProps = *(rStyle.m_pStyleProps); \
    } else { \
        DELETEP(m_pStyleProps); \
    }
    
    
    ODE_COPY_STYLE_PROPS(m_pSectionProps, SectionProps);
    ODE_COPY_STYLE_PROPS(m_pParagraphProps, ParagraphProps);
    ODE_COPY_STYLE_PROPS(m_pTextProps, TextProps);
    ODE_COPY_STYLE_PROPS(m_pTableProps, TableProps);
    ODE_COPY_STYLE_PROPS(m_pColumnProps, ColumnProps);
    ODE_COPY_STYLE_PROPS(m_pRowProps, RowProps);
    ODE_COPY_STYLE_PROPS(m_pCellProps, CellProps);
    ODE_COPY_STYLE_PROPS(m_pGraphicProps, GraphicProps);
    
#undef ODE_COPY_STYLE_PROPS

    return *this;
}


/**
 * Defines the style from attributes and properties of an AbiWord style.
 * 
 * @return "false" if an error ocurred.
 */
bool ODe_Style_Style::fetchAttributesFromAbiStyle(const PP_AttrProp* pAP) {

    const gchar* pValue;
    bool ok;
    
    ok = pAP->getAttribute(PT_NAME_ATTRIBUTE_NAME, pValue);
    if (!ok) {return false;}
    m_name = pValue;
    
    ok = pAP->getAttribute(PT_FOLLOWEDBY_ATTRIBUTE_NAME, pValue);
    if (ok && pValue != NULL) {
        
        if (strcmp("Current Settings", pValue) != 0) {
            m_nextStyleName = pValue;
        }
    }
    
    ok = pAP->getAttribute(PT_BASEDON_ATTRIBUTE_NAME, pValue);
    if (ok && pValue != NULL) {
        if (strcmp(pValue, "None") != 0) {
            // OpenDocument don't use a "None" to say that is has no parent.
            // To say that it simply doesn't define this attribute.
            m_parentStyleName = pValue;
        }
    }
    
    
    if (m_pTextProps == NULL) {
        m_pTextProps = new TextProps();
    }
    m_pTextProps->fetchAttributesFromAbiProps(*pAP);
    
    if (m_pParagraphProps == NULL) {
        m_pParagraphProps = new ParagraphProps();
    }
    m_pParagraphProps->fetchAttributesFromAbiProps(*pAP);
    
    return true;
}


/**
 * Defines the style from attributes and properties of an AbiWord <span>.
 */
void ODe_Style_Style::fetchAttributesFromAbiSpan(const PP_AttrProp* pAP) {
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getAttribute("style", pValue);
    if (ok && pValue != NULL) {
        m_parentStyleName = pValue;
    }
    
    
    if (m_pTextProps == NULL) {
        m_pTextProps = new TextProps();
    }
    m_pTextProps->fetchAttributesFromAbiProps(*pAP);
}


/**
 * Fetch attributes from an AbiWord <p> tag. Usually paragraph style attributes.
 */
void ODe_Style_Style::fetchAttributesFromAbiBlock(const PP_AttrProp* pAP) {
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getAttribute("style", pValue);
    if (ok && pValue != NULL) {
        m_parentStyleName = pValue;
    }
    
    if (m_pTextProps == NULL) {
        m_pTextProps = new TextProps();
    }
    m_pTextProps->fetchAttributesFromAbiProps(*pAP);
    
    if (m_pParagraphProps == NULL) {
        m_pParagraphProps = new ParagraphProps();
    }
    m_pParagraphProps->fetchAttributesFromAbiProps(*pAP);
    
    ok = pAP->getAttribute("listid", pValue);
    if (ok && pValue != NULL) {
        // This block is a list item, so, it's margin-left property is not
        // valid as paragraph attribute.
        m_pParagraphProps->m_marginLeft.clear();
    }
}


/**
 * Fetch attributes from an AbiWord <section> tag. Usually column info for 
 * an OpenDocument <style:style style:family="section"> element.
 */
void ODe_Style_Style::fetchAttributesFromAbiSection(const PP_AttrProp* pAP) {
    if (m_pSectionProps == NULL) {
        m_pSectionProps = new SectionProps();
    }
    m_pSectionProps->fetchAttributesFromAbiProps(*pAP);
}


/**
 * Defines the style from attributes and properties of an AbiWord <table>.
 */
void ODe_Style_Style::fetchAttributesFromAbiTable(const PP_AttrProp* pAP) {
    if (m_pTableProps == NULL) {
        m_pTableProps = new TableProps();
    }
    m_pTableProps->fetchAttributesFromAbiProps(*pAP);
}


/**
 * Defines the style from attributes and properties of an AbiWord <cell>.
 */
void ODe_Style_Style::fetchAttributesFromAbiCell(const PP_AttrProp* pAP) {
    if (m_pCellProps == NULL) {
        m_pCellProps = new CellProps();
    }
    m_pCellProps->fetchAttributesFromAbiProps(*pAP);
}


/**
 * Defines the style from attributes and properties of an AbiWord <frame>.
 */
void ODe_Style_Style::fetchAttributesFromAbiFrame(const PP_AttrProp& rAP) {
    if (m_pGraphicProps == NULL) {
        m_pGraphicProps = new GraphicProps();
    }
    m_pGraphicProps->fetchAttributesFromAbiProps(rAP);
}


/**
 * 
 */
void ODe_Style_Style::setBreakBefore(const gchar* pBreakBefore) {
    if (m_pParagraphProps == NULL) {
        m_pParagraphProps = new ParagraphProps();
    }
    m_pParagraphProps->m_breakBefore = pBreakBefore;
}


/**
 * 
 */
const UT_UTF8String& ODe_Style_Style::getFontName() {
    if (m_pTextProps == NULL) {
        m_pTextProps = new TextProps();
    }
    return m_pTextProps->m_fontName;
}


/**
 * 
 */
void ODe_Style_Style::setColumnWidth(const gchar* pColumnWidth) {
    if (m_pColumnProps == NULL) {
        m_pColumnProps = new ColumnProps();
    }
    m_pColumnProps->m_columnWidth = pColumnWidth;
}


/**
 * 
 */
void ODe_Style_Style::setRowHeight(const gchar* pRowHeight) {
    if (m_pRowProps == NULL) {
        m_pRowProps = new RowProps();
    }
    m_pRowProps->m_rowHeight = pRowHeight;
}


/**
 * 
 */
void ODe_Style_Style::setMinRowHeight(const gchar* pMinRowHeight) {
    if (m_pRowProps == NULL) {
        m_pRowProps = new RowProps();
    }
    m_pRowProps->m_minRowHeight = pMinRowHeight;
}


/**
 * 
 */
bool ODe_Style_Style::hasTableStyleProps(const PP_AttrProp* pAP) {
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getProperty("background-color", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    ok = pAP->getProperty("table-column-props", pValue);
    if (ok && pValue != NULL) {
        return true;
    }
    
    // If we reached this point it's because there are no table props at all
    // on this AbiWord element attributes and properties.
    return false;
}


/**
 * 
 */
void ODe_Style_Style::inheritTableCellProperties(const ODe_Style_Style& tableStyle) {
    UT_return_if_fail(tableStyle.m_pCellProps);
    if (m_pCellProps == NULL) {
        m_pCellProps = new CellProps();
    }

    // the following properties are always inherited by AbiWord cells
    m_pCellProps->m_leftThickness = tableStyle.m_pCellProps->m_leftThickness;
    m_pCellProps->m_leftColor = tableStyle.m_pCellProps->m_leftColor;
    m_pCellProps->m_rightThickness = tableStyle.m_pCellProps->m_rightThickness;
    m_pCellProps->m_rightColor = tableStyle.m_pCellProps->m_rightColor;
    m_pCellProps->m_topThickness = tableStyle.m_pCellProps->m_topThickness;
    m_pCellProps->m_topColor = tableStyle.m_pCellProps->m_topColor;
    m_pCellProps->m_bottomThickness = tableStyle.m_pCellProps->m_bottomThickness;
    m_pCellProps->m_bottomColor = tableStyle.m_pCellProps->m_bottomColor;

    // Table background colors are not inherited in AbiWord and an OpenDocument
    // table can have its own background color as well, so we don't inherit
    // this property
}


/**
 * 
 */
void ODe_Style_Style::setWrap(const UT_UTF8String& rWrap) {
    if (m_pGraphicProps == NULL) {
        m_pGraphicProps = new GraphicProps();
    }
    
    m_pGraphicProps->m_wrap = rWrap;
}


/**
 * 
 */
void ODe_Style_Style::setRunThrough(const UT_UTF8String& rRunThrough) {
    if (m_pGraphicProps == NULL) {
        m_pGraphicProps = new GraphicProps();
    }
    
    m_pGraphicProps->m_runThrough = rRunThrough;
}


/**
 * 
 */
void ODe_Style_Style::setPadding(const UT_UTF8String& rPadding) {
    if (m_pGraphicProps == NULL) {
        m_pGraphicProps = new GraphicProps();
    }
    
    m_pGraphicProps->m_padding = rPadding;
}


/**
 * 
 */
void ODe_Style_Style::setHorizontalPos(const UT_UTF8String& rHorizontalPos) {
    if (m_pGraphicProps == NULL) {
        m_pGraphicProps = new GraphicProps();
    }
    
    m_pGraphicProps->m_horizontalPos = rHorizontalPos;
}


/**
 * 
 */
void ODe_Style_Style::setVerticalPos(const UT_UTF8String& rVerticalPos) {
    if (m_pGraphicProps == NULL) {
        m_pGraphicProps = new GraphicProps();
    }
    
    m_pGraphicProps->m_verticalPos = rVerticalPos;
}


/*******************************************************************************
 * SectionProps
 ******************************************************************************/


/**
 * 
 */
bool ODe_Style_Style::SectionProps::isEmpty() const {
    return m_columnCount.empty() && m_columnGap.empty();
}


/**
 * 
 */
void ODe_Style_Style::SectionProps::
fetchAttributesFromAbiProps(const PP_AttrProp& rAP) {
    const gchar* pValue;
    bool ok;
    
    ok = rAP.getProperty("columns", pValue);
    if (ok && pValue != NULL) {
        m_columnCount = pValue;
    }
    
    ok = rAP.getProperty("column-gap", pValue);
    if (ok && pValue != NULL) {
        m_columnGap = pValue;
    }
}


/**
 * 
 */
void ODe_Style_Style::SectionProps::
write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const {

    if (isEmpty()) {
        return;
    }
    
    rOutput += rSpacesOffset;
    rOutput += "<style:section-properties>\n";
    
    rOutput += rSpacesOffset;
    rOutput += " <style:columns";
    
    ODe_writeAttribute(rOutput, "fo:column-count", m_columnCount);
    ODe_writeAttribute(rOutput, "fo:column-gap", m_columnGap);
    
    rOutput += "/>\n";
    
    rOutput += rSpacesOffset;
    rOutput += "</style:section-properties>\n";
}


/**
 * 
 */
ODe_Style_Style::SectionProps& ODe_Style_Style::SectionProps::operator=(
                                            const SectionProps& rSectionProps) {
                 
    m_columnCount = rSectionProps.m_columnCount;
    m_columnGap = rSectionProps.m_columnGap;
    return *this;
}


/**
 * 
 */
bool ODe_Style_Style::SectionProps::operator==(
                     const ODe_Style_Style::SectionProps& rSectionProps) const {
                            
    return m_columnCount == rSectionProps.m_columnCount &&
           m_columnGap   == rSectionProps.m_columnGap;
}


/*******************************************************************************
 * ParagraphProps
 ******************************************************************************/


/**
 * 
 */
bool ODe_Style_Style::ParagraphProps::isEmpty() const {
    return ( m_textAlign.empty() &&
             m_textIndent.empty() &&
             m_lineHeight.empty() &&
             m_lineHeightAtLeast.empty() &&
             m_backgroundColor.empty() &&
             m_widows.empty() &&
             m_orphans.empty() &&
             m_marginLeft.empty() &&
             m_marginRight.empty() &&
             m_marginTop.empty() &&
             m_marginBottom.empty() &&
             m_keepWithNext.empty() &&
             m_breakBefore.empty() &&
             m_writingMode.empty());
}

/**
 * 
 */
void ODe_Style_Style::ParagraphProps::
fetchAttributesFromAbiProps(const PP_AttrProp& rAP) {
    const gchar* pValue;
    bool ok;
    
    ok = rAP.getProperty("bgcolor", pValue);
    if (ok && pValue && *pValue) {
        if (!strcmp("transparent", pValue)) { 
            m_backgroundColor = pValue;
        } else {
            m_backgroundColor = UT_colorToHex(pValue, true);
        }
    }
    
    ok = rAP.getProperty("line-height", pValue);
    if (ok && pValue != NULL) {
        UT_LocaleTransactor t(LC_NUMERIC, "C");
        if(strstr(pValue, "+")) { // "at least" spacing
            int len = strlen(pValue);

            if((len > 1) && (pValue[len - 1] == '+')) {
               gchar* temp = (gchar*)pValue;
               temp[len-1] = '\0';
               m_lineHeightAtLeast = UT_UTF8String_sprintf("%fin", UT_convertToDimension(temp, DIM_IN));
               m_lineHeight.clear(); // make sure this is empty
            } else {
                UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
            }
        } else if(strstr(pValue, "pt")) { // "exactly" spacing
            m_lineHeight = UT_UTF8String_sprintf("%fin", UT_convertToDimension(pValue, DIM_IN));
            m_lineHeightAtLeast.clear(); // make sure this is empty
        } else { //"single", "double", "1.5 lines", or "multiple" spacing
            m_lineHeight = UT_UTF8String_sprintf("%.0f%%", atof(pValue) * 100);
            m_lineHeightAtLeast.clear(); // make sure this is empty
        }
    }
    
    ok = rAP.getProperty("text-align", pValue);
    if (ok && pValue != NULL) {
        if(!strcmp(pValue, "right")) {
            m_textAlign = "end"; //see Bug 10719
        } else {
            m_textAlign = pValue;
        }
    }

    ok = rAP.getProperty("text-indent", pValue);
    if (ok && pValue != NULL) {
        m_textIndent = pValue;
    }

    ok = rAP.getProperty("dom-dir", pValue);
    if (ok && pValue != NULL) {
        if(!strcmp(pValue, "rtl")) {
            m_writingMode = "rl";
        } else {
            m_writingMode = "lr";
        }
    }
    
    ok = rAP.getProperty("widows", pValue);
    if (ok && pValue != NULL) {
        m_widows = pValue;
    }
    
    ok = rAP.getProperty("orphans", pValue);
    if (ok && pValue != NULL) {
        m_orphans = pValue;
    }
    
    ok = rAP.getProperty("margin-left", pValue);
    if (ok && pValue != NULL) {
        m_marginLeft = pValue;
    }
    
    ok = rAP.getProperty("margin-right", pValue);
    if (ok && pValue != NULL) {
        m_marginRight = pValue;
    }
    
    ok = rAP.getProperty("margin-top", pValue);
    if (ok && pValue != NULL) {
        m_marginTop = pValue;
    }
    
    ok = rAP.getProperty("margin-bottom", pValue);
    if (ok && pValue != NULL) {
        m_marginBottom = pValue;
    }
    
    ok = rAP.getProperty("keep-with-next", pValue);
    if (ok && pValue != NULL) {
        if (!strcmp(pValue, "yes")) {
            m_keepWithNext = "always";
        } else {
            m_keepWithNext = "auto";
        }
    }
}


/**
 * 
 */
void ODe_Style_Style::ParagraphProps::
write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const {
    
    if (isEmpty()) {
        return;
    }
    
    rOutput += rSpacesOffset;
    rOutput += "<style:paragraph-properties";
    
    ODe_writeAttribute(rOutput, "fo:text-align", m_textAlign);
    ODe_writeAttribute(rOutput, "fo:text-indent", m_textIndent);
    ODe_writeAttribute(rOutput, "fo:line-height", m_lineHeight);
    ODe_writeAttribute(rOutput, "style:line-height-at-least", m_lineHeightAtLeast);
    ODe_writeAttribute(rOutput, "fo:background-color", m_backgroundColor);
    ODe_writeAttribute(rOutput, "fo:widows", m_widows);
    ODe_writeAttribute(rOutput, "fo:orphans", m_orphans);
    ODe_writeAttribute(rOutput, "fo:margin-left", m_marginLeft);
    ODe_writeAttribute(rOutput, "fo:margin-right", m_marginRight);
    ODe_writeAttribute(rOutput, "fo:margin-top", m_marginTop);
    ODe_writeAttribute(rOutput, "fo:margin-bottom", m_marginBottom);
    ODe_writeAttribute(rOutput, "fo:keep-with-next", m_keepWithNext);
    ODe_writeAttribute(rOutput, "fo:break-before", m_breakBefore);
    ODe_writeAttribute(rOutput, "style:writing-mode", m_writingMode);

    rOutput += "/>\n";
}


/**
 * 
 */
ODe_Style_Style::ParagraphProps& ODe_Style_Style::ParagraphProps::operator=(
                                       const ParagraphProps& rParagraphProps) {
                                        
    m_textAlign = rParagraphProps.m_textAlign;
    m_textIndent = rParagraphProps.m_textIndent;
    m_lineHeight = rParagraphProps.m_lineHeight;
    m_lineHeightAtLeast = rParagraphProps.m_lineHeightAtLeast;
    m_backgroundColor = rParagraphProps.m_backgroundColor;
    m_widows = rParagraphProps.m_widows;
    m_orphans = rParagraphProps.m_orphans;
    m_marginLeft = rParagraphProps.m_marginLeft;
    m_marginRight = rParagraphProps.m_marginRight;
    m_marginTop = rParagraphProps.m_marginTop;
    m_marginBottom = rParagraphProps.m_marginBottom;
    m_keepWithNext = rParagraphProps.m_keepWithNext;
    m_breakBefore = rParagraphProps.m_breakBefore;
    m_writingMode = rParagraphProps.m_writingMode;
    
    return *this;
}


/**
 * 
 */
bool ODe_Style_Style::ParagraphProps::operator==(
                 const ODe_Style_Style::ParagraphProps& rParagraphProps) const {

    return                    
        m_textAlign       == rParagraphProps.m_textAlign &&
        m_textIndent       == rParagraphProps.m_textIndent &&
        m_lineHeight      == rParagraphProps.m_lineHeight &&
        m_lineHeightAtLeast == rParagraphProps.m_lineHeightAtLeast &&
        m_backgroundColor == rParagraphProps.m_backgroundColor &&
        m_widows          == rParagraphProps.m_widows &&
        m_orphans         == rParagraphProps.m_orphans &&
        m_marginLeft      == rParagraphProps.m_marginLeft &&
        m_marginRight     == rParagraphProps.m_marginRight &&
        m_marginTop       == rParagraphProps.m_marginTop &&
        m_marginBottom    == rParagraphProps.m_marginBottom &&
        m_keepWithNext    == rParagraphProps.m_keepWithNext &&
        m_breakBefore     == rParagraphProps.m_breakBefore &&
        m_writingMode     == rParagraphProps.m_writingMode;
}


/*******************************************************************************
 * TextProps
 ******************************************************************************/


/**
 * 
 */
bool ODe_Style_Style::TextProps::isEmpty() const {
    return m_color.empty() &&
           m_underlineType.empty() &&
           m_lineThroughType.empty() &&
           m_textPosition.empty() &&
           m_fontName.empty() &&
           m_fontSize.empty() &&
           m_language.empty() &&
           m_country.empty() &&
           m_fontStyle.empty() &&
           m_fontWeight.empty() &&
           m_backgroundColor.empty() &&
           m_display.empty();
}


/**
 * 
 */
void ODe_Style_Style::TextProps::
fetchAttributesFromAbiProps(const PP_AttrProp& rAP) {
    const gchar* pValue;
    bool ok;
    
    ok = rAP.getProperty("color", pValue);
    if (ok && pValue && *pValue) {
        // TODO: handle transparent?
        m_color = UT_colorToHex(pValue, true);
    }
    
    ok = rAP.getProperty("text-decoration", pValue);
    if (ok && pValue != NULL) {
        if (strstr(pValue, "underline"))
            m_underlineType = "single";

        if (strstr(pValue, "line-through"))
            m_lineThroughType = "single";
    }
    
    
    ok = rAP.getProperty("text-position", pValue);
    if (ok && pValue != NULL) {
        if (!strcmp("subscript", pValue)) {
            // Hard coded, it's ugly but I can't do otherwise.
            m_textPosition = "-33%";
        } else if (!strcmp("superscript", pValue)) {
            // Hard coded, it's ugly but I can't do otherwise.
            m_textPosition = "33%";
        } else {
            UT_ASSERT( !strcmp("normal", pValue) );
            m_textPosition.clear();
        }
    }
    
    
    ok = rAP.getProperty("font-family", pValue);
    if (ok && pValue != NULL) {
        m_fontName = pValue;
    }
    
    
    ok = rAP.getProperty("font-size", pValue);
    if (ok && pValue != NULL) {
        m_fontSize = pValue;
    }
    
    
    ok = rAP.getProperty("lang", pValue);
    if (ok && pValue != NULL) {
        if (!strcmp(pValue, "-none-")) {
            m_language = "none";
            m_country = "none";
        } else {
            gchar strLanguage[4];
            gchar strCountry[3];

            int len = strlen(pValue);
            bool bLong = (len == 6);
            // Note: not all language codes are 5 characters (e.g. cop-EG)
            if ((len == 5) || bLong) {
                strLanguage[0] = pValue[0];
                strLanguage[1] = pValue[1];

                if(bLong) {
                    strLanguage[2] = pValue[2];
                    // pValue[3] == '-'
                    strCountry[0] = pValue[4];
                    strCountry[1] = pValue[5];
                } else {
                    strLanguage[2] = 0;
                    // pValue[2] == '-'
                    strCountry[0] = pValue[3];
                    strCountry[1] = pValue[4];
                }
                
                strLanguage[3] = 0;
                strCountry[2] = 0;
               
                m_language = strLanguage;
                m_country = strCountry;
            } else {
                UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN);
            }
        }
    }
    
    
    ok = rAP.getProperty("font-style", pValue);
    if (ok && pValue != NULL) {
        if (!strcmp(pValue, "italic")) {
            m_fontStyle = "italic";
        }
    }
    
    
    ok = rAP.getProperty("font-weight", pValue);
    if (ok && pValue != NULL) {
        if (!strcmp(pValue, "bold")) {
            m_fontWeight = "bold";
        } else if (!strcmp(pValue, "normal")) {
            m_fontWeight = "normal";
        }
    }


    ok = rAP.getProperty("bgcolor", pValue);
    if (ok && pValue && *pValue) {
        if (!strcmp("transparent", pValue)) {
            m_backgroundColor = pValue;
        } else {
            m_backgroundColor = UT_colorToHex(pValue, true);
        }
    }


    ok = rAP.getProperty("display", pValue);
    if (ok && pValue != NULL) {
        if (!strcmp(pValue, "none")) {
            m_display = "none";
        } else {
            m_display = "true";
        }
    }
}


/**
 * 
 */
void ODe_Style_Style::TextProps::
write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const {
    
    if (isEmpty()) {
        return;
    }
    
    rOutput += rSpacesOffset;
    rOutput += "<style:text-properties";
    
    ODe_writeAttribute(rOutput, "fo:color", m_color);
    ODe_writeAttribute(rOutput, "style:text-underline-type", m_underlineType);
    ODe_writeAttribute(rOutput, "style:text-line-through-type", m_lineThroughType);
    ODe_writeAttribute(rOutput, "style:text-position", m_textPosition);
    ODe_writeAttribute(rOutput, "style:font-name", m_fontName);
    ODe_writeAttribute(rOutput, "fo:font-size", m_fontSize);
    ODe_writeAttribute(rOutput, "fo:language", m_language);
    ODe_writeAttribute(rOutput, "fo:country", m_country);
    ODe_writeAttribute(rOutput, "fo:font-style", m_fontStyle);
    ODe_writeAttribute(rOutput, "fo:font-weight", m_fontWeight);
    ODe_writeAttribute(rOutput, "fo:background-color", m_backgroundColor);
    ODe_writeAttribute(rOutput, "text:display", m_display);
    
    rOutput += "/>\n";
}


/**
 * 
 */
ODe_Style_Style::TextProps& ODe_Style_Style::TextProps::operator=(
                                                const TextProps& rTextProps) {
    
    m_color = rTextProps.m_color;
    m_underlineType = rTextProps.m_underlineType;
    m_lineThroughType = rTextProps.m_lineThroughType;
    m_textPosition = rTextProps.m_textPosition;
    m_fontName = rTextProps.m_fontName;
    m_fontSize = rTextProps.m_fontSize;
    m_language = rTextProps.m_language;
    m_country = rTextProps.m_country;
    m_fontStyle = rTextProps.m_fontStyle;
    m_fontWeight = rTextProps.m_fontWeight;
    m_backgroundColor = rTextProps.m_backgroundColor;
    m_display = rTextProps.m_display;
    
    return *this;
}


/**
 * 
 */
bool ODe_Style_Style::TextProps::operator==(
                           const ODe_Style_Style::TextProps& rTextProps) const {
                            
    return
        m_color           == rTextProps.m_color &&
        m_underlineType   == rTextProps.m_underlineType &&
        m_lineThroughType == rTextProps.m_lineThroughType &&
        m_textPosition    == rTextProps.m_textPosition &&
        m_fontName        == rTextProps.m_fontName &&
        m_fontSize        == rTextProps.m_fontSize &&
        m_language        == rTextProps.m_language &&
        m_country         == rTextProps.m_country &&
        m_fontStyle       == rTextProps.m_fontStyle &&
        m_fontWeight      == rTextProps.m_fontWeight &&
        m_backgroundColor == rTextProps.m_backgroundColor &&
        m_display         == rTextProps.m_display;
}


/*******************************************************************************
 * TableProps
 ******************************************************************************/


/**
 * 
 */
bool ODe_Style_Style::TableProps::isEmpty() const {
    return m_width.empty() &&
           m_backgroundColor.empty() &&
           m_align.empty() &&
           m_marginLeft.empty() &&
           m_marginRight.empty();
}


/**
 * 
 */
void ODe_Style_Style::TableProps::
fetchAttributesFromAbiProps(const PP_AttrProp& rAP) {
    const gchar* pValue;
    bool ok;
    
    ok = rAP.getProperty("background-color", pValue);
    if (ok && pValue && *pValue) {
        // TODO: handle transparent?
        m_backgroundColor = UT_colorToHex(pValue, true);
    }
    
    ok = rAP.getProperty("table-column-props", pValue);
    if (ok && pValue != NULL) {
        std::string buffer;
        double tableWidth = 0.0;
        UT_Dimension dimension = DIM_none;
        bool gotDimension = false;
        
        // The table width is the sum of all column widths.
        
        while (*pValue != 0) {
            if (*pValue == '/') {
                // We've reached the end of a column width
                if (!gotDimension) {
                    dimension = UT_determineDimension(buffer.c_str(), DIM_none);
                    gotDimension = true;
                }
                
                tableWidth += UT_convertDimensionless(buffer.c_str());
                buffer.clear();
            } else {
                // Store the character in the buffer
                buffer += *pValue;
            }
            pValue++;
        }
        
        UT_LocaleTransactor t(LC_NUMERIC, "C");
        UT_UTF8String_sprintf(m_width, "%f%s",
                              tableWidth, UT_dimensionName(dimension) );
    }
    
    
    ok = rAP.getProperty("table-column-leftpos", pValue);
    if (ok && pValue != NULL) {
        m_align = "margins";
        m_marginLeft = pValue;
    } else {
        m_align = "left";
    }
}


/**
 * 
 */
void ODe_Style_Style::TableProps::write(UT_UTF8String& rOutput,
                                        const UT_UTF8String& rSpacesOffset) const {
        
    rOutput += rSpacesOffset;
    rOutput += "<style:table-properties";
    
    ODe_writeAttribute(rOutput, "style:width", m_width);
    ODe_writeAttribute(rOutput, "fo:background-color", m_backgroundColor);
    ODe_writeAttribute(rOutput, "table:align", m_align);
    ODe_writeAttribute(rOutput, "fo:margin-left", m_marginLeft);
    ODe_writeAttribute(rOutput, "fo:margin-right", m_marginRight);
    
    rOutput += "/>\n";
}


/**
 * 
 */
ODe_Style_Style::TableProps& ODe_Style_Style::TableProps::operator=(
                                              const TableProps& rTableProps) {

    m_width = rTableProps.m_width;
    m_backgroundColor = rTableProps.m_backgroundColor;
    m_align = rTableProps.m_align;
    m_marginLeft = rTableProps.m_marginLeft;
    m_marginRight = rTableProps.m_marginRight;
    
    return *this;
}


/**
 * 
 */
bool ODe_Style_Style::TableProps::operator==(
                         const ODe_Style_Style::TableProps& rTableProps) const {
                            
    return
        m_width           == rTableProps.m_width &&
        m_backgroundColor == rTableProps.m_backgroundColor &&
        m_align           == rTableProps.m_align &&
        m_marginLeft      == rTableProps.m_marginLeft &&
        m_marginRight     == rTableProps.m_marginRight;
}


/*******************************************************************************
 * ColumnProps
 ******************************************************************************/


/**
 * 
 */
bool ODe_Style_Style::ColumnProps::isEmpty() const {
    return m_columnWidth.empty();
}


/**
 * 
 */
void ODe_Style_Style::ColumnProps::write(UT_UTF8String& rOutput,
                        const UT_UTF8String& rSpacesOffset) const {
    if (isEmpty()) {
        return;
    }
    
    rOutput += rSpacesOffset;
    rOutput += "<style:table-column-properties";
    
    ODe_writeAttribute(rOutput, "style:column-width", m_columnWidth);
    
    rOutput += "/>\n";
}


/**
 * 
 */
ODe_Style_Style::ColumnProps& ODe_Style_Style::ColumnProps::operator=(
                                            const ColumnProps& rColumnProps) {
                                                
    m_columnWidth = rColumnProps.m_columnWidth;
    return *this;
}


/**
 * 
 */
bool ODe_Style_Style::ColumnProps::operator==(
                      const ODe_Style_Style::ColumnProps& rColumnProps) const {
                        
    return m_columnWidth == rColumnProps.m_columnWidth;
}

/*******************************************************************************
 * RowProps
 ******************************************************************************/


/**
 * 
 */
bool ODe_Style_Style::RowProps::isEmpty() const {
    return m_rowHeight.empty() && 
           m_minRowHeight.empty();
}


/**
 * 
 */
void ODe_Style_Style::RowProps::write(UT_UTF8String& rOutput,
                        const UT_UTF8String& rSpacesOffset) const {
    if (isEmpty()) {
        return;
    }
    
    rOutput += rSpacesOffset;
    rOutput += "<style:table-row-properties";
    
    ODe_writeAttribute(rOutput, "style:row-height", m_rowHeight);
    ODe_writeAttribute(rOutput, "style:min-row-height", m_minRowHeight);
    
    rOutput += "/>\n";
}


/**
 * 
 */
ODe_Style_Style::RowProps& ODe_Style_Style::RowProps::operator=(
                                                const RowProps& rRowProps) {
    m_rowHeight = rRowProps.m_rowHeight;
    m_minRowHeight = rRowProps.m_minRowHeight;
    return *this;
}


/**
 * 
 */
bool ODe_Style_Style::RowProps::operator==(
                            const ODe_Style_Style::RowProps& rRowProps) const {
    return m_rowHeight == rRowProps.m_rowHeight &&
           m_minRowHeight == rRowProps.m_minRowHeight;
}


/*******************************************************************************
 * CellProps
 ******************************************************************************/


/**
 * 
 */
bool ODe_Style_Style::CellProps::isEmpty() const {
    return m_leftThickness.empty() &&
           m_leftColor.empty() &&
           m_rightThickness.empty() &&
           m_rightColor.empty() &&
           m_topThickness.empty() &&
           m_topColor.empty() &&
           m_bottomThickness.empty() &&
           m_bottomColor.empty() &&
           m_backgroundColor.empty();
}


/**
 * 
 */
void ODe_Style_Style::CellProps::
fetchAttributesFromAbiProps(const PP_AttrProp& rAP) {
    const gchar* pValue;
    bool ok;
    
    // NOTE: Contrary to OpenDocument, AbiWord allows setting line properties on the
    // table itself. Table line colors default to #000000 in AbiWord if unset, and cells 
    // inherit the table line color if they have no line color defined themselves.
    // Table line thickness defaults to 0.72pt, and again cells inherit the
    // table line thickness if they have no line thickness defined themselves.
    // 
    // The default table background color is transparent, and cells cells inherit
    // this property if they have no background color set
    //
    // See fp_TableContainer for details.

    // Be aware that some cell properties can have a value set already by inheritance 
    
    // Left border

    ok = rAP.getProperty("left-thickness", pValue);
    if (ok && pValue != NULL) {
        m_leftThickness = pValue;
    } else if (m_leftThickness.empty()) {
        m_leftThickness = "0.72pt";
    }
    
    ok = rAP.getProperty("left-color", pValue);
    if (ok && pValue != NULL) {
	    m_leftColor = pValue;
    } else if (m_leftColor.empty()) {
        m_leftColor = "000000";
    }

    // Right border

    ok = rAP.getProperty("right-thickness", pValue);
    if (ok && pValue != NULL) {
        m_rightThickness = pValue;
    } else if (m_rightThickness.empty()) {
        m_rightThickness = "0.72pt";
    }
    
    ok = rAP.getProperty("right-color", pValue);
    if (ok && pValue != NULL) {
	    m_rightColor = pValue;
    } else if (m_rightColor.empty()) {
        m_rightColor = "000000";
    }

    // Top border

    ok = rAP.getProperty("top-thickness", pValue);
    if (ok && pValue != NULL) {
        m_topThickness = pValue;
    } else if (m_topThickness.empty()) {
        m_topThickness = "0.72pt";
    }
    
    ok = rAP.getProperty("top-color", pValue);
    if (ok && pValue != NULL) {
	    m_topColor = pValue;
    } else if (m_topColor.empty()) {
        m_topColor = "000000";
    }
    
    // Bottom border

    ok = rAP.getProperty("bot-thickness", pValue);
    if (ok && pValue != NULL) {
        m_bottomThickness = pValue;
    } else if (m_bottomThickness.empty()) {
        m_bottomThickness = "0.72pt";
    }
    
    ok = rAP.getProperty("bot-color", pValue);
    if (ok && pValue != NULL) {
	    m_bottomColor = pValue;
    } else if (m_bottomColor.empty()) {
        m_bottomColor = "000000";
    }
    
    // Background color
    
    ok = rAP.getProperty("background-color", pValue);
    if (ok && pValue && *pValue) {
        // TODO: handle transparent?
        m_backgroundColor = UT_colorToHex(pValue, true);
    }
}


/**
 * 
 */
void ODe_Style_Style::CellProps::
write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const {
    
    if (isEmpty()) {
        return;
    }
    
    rOutput += rSpacesOffset;
    rOutput += "<style:table-cell-properties";
    
    ODe_writeAttribute(rOutput, "fo:border-left", m_leftThickness + " solid #" + m_leftColor);
    ODe_writeAttribute(rOutput, "fo:border-right",m_rightThickness + " solid #" + m_rightColor);
    ODe_writeAttribute(rOutput, "fo:border-top", m_topThickness + " solid #" + m_topColor);
    ODe_writeAttribute(rOutput, "fo:border-bottom", m_bottomThickness + " solid #" + m_bottomColor);
    ODe_writeAttribute(rOutput, "fo:background-color", m_backgroundColor);
    
    rOutput += "/>\n";
}


/**
 * 
 */
ODe_Style_Style::CellProps& ODe_Style_Style::CellProps::operator=(
                                                const CellProps& rCellProps) {
    
    m_leftThickness = rCellProps.m_leftThickness;
    m_leftColor = rCellProps.m_leftColor;
    m_rightThickness = rCellProps.m_rightThickness;
    m_rightColor = rCellProps.m_rightColor;
    m_topThickness = rCellProps.m_topThickness;
    m_topColor = rCellProps.m_topColor;
    m_bottomThickness = rCellProps.m_bottomThickness;
    m_bottomColor = rCellProps.m_bottomColor;
    m_backgroundColor = rCellProps.m_backgroundColor;
    
    return *this;
}


/**
 * 
 */
bool ODe_Style_Style::CellProps::operator==(
                           const ODe_Style_Style::CellProps& rCellProps) const {
    return
        m_leftThickness      == rCellProps.m_leftThickness &&
        m_leftColor          == rCellProps.m_leftColor &&
        m_rightThickness     == rCellProps.m_rightThickness &&
        m_rightColor         == rCellProps.m_rightColor &&
        m_topThickness       == rCellProps.m_topThickness &&
        m_topColor           == rCellProps.m_topColor &&
        m_bottomThickness    == rCellProps.m_bottomThickness &&
        m_bottomColor        == rCellProps.m_bottomColor &&
        m_backgroundColor    == rCellProps.m_backgroundColor;
}


/*******************************************************************************
 * GraphicProps
 ******************************************************************************/


/**
 * 
 */ 
bool ODe_Style_Style::GraphicProps::isEmpty() const {
    return
        m_backgroundColor.empty() &&
        m_borderLeft.empty() &&
        m_borderRight.empty() &&
        m_borderTop.empty() &&
        m_borderBottom.empty() &&
        m_wrap.empty() &&
        m_runThrough.empty() &&
        m_verticalPos.empty() &&
        m_verticalRel.empty() &&
        m_horizontalPos.empty() &&
        m_horizontalRel.empty() &&
        m_padding.empty();
}


/**
 * 
 */
void ODe_Style_Style::GraphicProps::
fetchAttributesFromAbiProps(const PP_AttrProp& rAP) {
    const gchar* pValue = NULL;
    bool ok;
    
    // Left border

    ok = rAP.getProperty("left-style", pValue);
    UT_ASSERT (ok && pValue != NULL);

    if (pValue && (*pValue == '0')) {
        m_borderLeft = "none";
    } else {
        ok = rAP.getProperty("left-thickness", pValue);
        if (ok && pValue != NULL) {
            m_borderLeft = pValue;
        }
        
        ok = rAP.getProperty("left-color", pValue);
        if (ok && pValue != NULL) {
            if (!m_borderLeft.empty()) {
                m_borderLeft += " ";
            }
            m_borderLeft += "solid #";
            m_borderLeft += pValue;
        }
    }
    
    
    // Right border

    ok = rAP.getProperty("right-style", pValue);
    UT_ASSERT (ok && pValue != NULL);
    
    if (pValue && (*pValue == '0')) {
        m_borderRight = "none";
    } else {
        ok = rAP.getProperty("right-thickness", pValue);
        if (ok && pValue != NULL) {
            m_borderRight = pValue;
        }
        
        ok = rAP.getProperty("right-color", pValue);
        if (ok && pValue != NULL) {
            if (!m_borderRight.empty()) {
                m_borderRight += " ";
            }
            m_borderRight += "solid #";
            m_borderRight += pValue;
        }
    }
   
    
    // Top border
    
    ok = rAP.getProperty("top-style", pValue);
    UT_ASSERT (ok && pValue != NULL);
    
    if (pValue && (*pValue == '0')) {
        m_borderTop = "none";
    } else {
        ok = rAP.getProperty("top-thickness", pValue);
        if (ok && pValue != NULL) {
            m_borderTop = pValue;
        }
        
        ok = rAP.getProperty("top-color", pValue);
        if (ok && pValue != NULL) {
            if (!m_borderTop.empty()) {
                m_borderTop += " ";
            }
            m_borderTop += "solid #";
            m_borderTop += pValue;
        }
    }
    
    
    // Bottom border
    
    
    ok = rAP.getProperty("bot-style", pValue);
    UT_ASSERT (ok && pValue != NULL);
    
    if (pValue && (*pValue == '0')) {
        m_borderBottom = "none";
    } else {
        ok = rAP.getProperty("bot-thickness", pValue);
        if (ok && pValue != NULL) {
            m_borderBottom = pValue;
        }
        
        ok = rAP.getProperty("bot-color", pValue);
        if (ok && pValue != NULL) {
            if (!m_borderBottom.empty()) {
                m_borderBottom += " ";
            }
            
            m_borderBottom += "solid #";
            m_borderBottom += pValue;
        }
    }
    
    
    // Background color
    
    ok = rAP.getProperty("background-color", pValue);
    if (ok && pValue && *pValue) {
        // TODO: handle transparent?
        m_backgroundColor = UT_colorToHex(pValue, true);
    }
    
    
    
    
    ok = rAP.getProperty("wrap-mode", pValue);
    if (ok && pValue != NULL) {
        if (!strcmp(pValue, "above-text")) {
            m_wrap = "run-through";
            m_runThrough = "foreground";
        } else if (!strcmp(pValue, "wrapped-both")) {
            m_wrap = "parallel";
        } else {
            UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
        }
    }
    
    
    ok = rAP.getProperty("position-to", pValue);
    UT_ASSERT (ok && pValue != NULL);

    if (!strcmp(pValue, "block-above-text")) {
        m_horizontalRel = "paragraph";
        m_verticalRel = "paragraph";
    } else {
        // Everything else (column and page) will be treated as page
        // anchored.
        m_horizontalRel = "page";
        m_verticalRel = "page";
    }
}


/**
 * 
 */
void ODe_Style_Style::GraphicProps::write(UT_UTF8String& rOutput,
                                     const UT_UTF8String& rSpacesOffset) const {
    if (isEmpty()) {
        return;
    }
    
    rOutput += rSpacesOffset;
    rOutput += "<style:graphic-properties";

    ODe_writeAttribute(rOutput, "fo:background-color", m_backgroundColor);
    ODe_writeAttribute(rOutput, "fo:border-left", m_borderLeft);
    ODe_writeAttribute(rOutput, "fo:border-right", m_borderRight);
    ODe_writeAttribute(rOutput, "fo:border-top", m_borderTop);
    ODe_writeAttribute(rOutput, "fo:border-bottom", m_borderBottom);
    ODe_writeAttribute(rOutput, "style:wrap", m_wrap);
    ODe_writeAttribute(rOutput, "style:run-through", m_runThrough);
    ODe_writeAttribute(rOutput, "style:vertical-pos", m_verticalPos);
    ODe_writeAttribute(rOutput, "style:vertical-rel", m_verticalRel);
    ODe_writeAttribute(rOutput, "style:horizontal-pos", m_horizontalPos);
    ODe_writeAttribute(rOutput, "style:horizontal-rel", m_horizontalRel);
    ODe_writeAttribute(rOutput, "fo:padding", m_padding);
    
    rOutput += "/>\n";
}


/**
 * 
 */
ODe_Style_Style::GraphicProps& ODe_Style_Style::GraphicProps::operator=(
                                            const GraphicProps& rGraphicProps) {
    m_backgroundColor = rGraphicProps.m_backgroundColor;
    m_borderLeft      = rGraphicProps.m_borderLeft;
    m_borderRight     = rGraphicProps.m_borderRight;
    m_borderTop       = rGraphicProps.m_borderTop;
    m_borderBottom    = rGraphicProps.m_borderBottom;
    m_wrap            = rGraphicProps.m_wrap;
    m_runThrough      = rGraphicProps.m_runThrough;
    m_verticalPos     = rGraphicProps.m_verticalPos;
    m_verticalRel     = rGraphicProps.m_verticalRel;
    m_horizontalPos   = rGraphicProps.m_horizontalPos;
    m_horizontalRel   = rGraphicProps.m_horizontalRel;
    m_padding         = rGraphicProps.m_padding;
	return *this;
}


/**
 * 
 */
bool ODe_Style_Style::GraphicProps::operator==(
                                      const GraphicProps& rGraphicProps) const {
    return
        m_backgroundColor == rGraphicProps.m_backgroundColor &&
        m_borderLeft      == rGraphicProps.m_borderLeft &&
        m_borderRight     == rGraphicProps.m_borderRight &&
        m_borderTop       == rGraphicProps.m_borderTop &&
        m_borderBottom    == rGraphicProps.m_borderBottom &&
        m_wrap            == rGraphicProps.m_wrap &&
        m_runThrough      == rGraphicProps.m_runThrough &&
        m_verticalPos     == rGraphicProps.m_verticalPos &&
        m_verticalRel     == rGraphicProps.m_verticalRel &&
        m_horizontalPos   == rGraphicProps.m_horizontalPos &&
        m_horizontalRel   == rGraphicProps.m_horizontalRel &&
        m_padding         == rGraphicProps.m_padding;
}
