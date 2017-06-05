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

#ifndef _ODE_STYLE_STYLE_H_
#define _ODE_STYLE_STYLE_H_


// AbiWord includes
#include <ut_string_class.h>

// External includes
#include <gsf/gsf.h>

// AbiWord classes
class PP_AttrProp;

/**
 * Class representing an OpenDocument <style:style> element.
 */
class ODe_Style_Style {
public:

    ODe_Style_Style();
    virtual ~ODe_Style_Style();
    
    // Write the <style:style> element.
    bool write(GsfOutput* pODT, const UT_UTF8String& rSpacesOffset) const;
    
    static bool hasTextStyleProps(const PP_AttrProp* pAP);
    static bool hasParagraphStyleProps(const PP_AttrProp* pAP);
    static bool hasSectionInfo(const PP_AttrProp* pAP);
    static bool hasTableStyleProps(const PP_AttrProp* pAP);    

    // It does not take style names into consideration.
    // Read it like: "is style "T1" equivalent to style "T2"
    // It is *NOT* like: "is style A equal to style B"
    bool isEquivalentTo(const ODe_Style_Style& rStyle);
    
    bool isEmpty() const;
    
    ODe_Style_Style& operator=(const ODe_Style_Style& rStyle);
    
    // Defines the style from attributes and properties of an AbiWord <s>.
    bool fetchAttributesFromAbiStyle(const PP_AttrProp* pAP);
    
    // Defines the style from attributes and properties of an AbiWord <span>.
    void fetchAttributesFromAbiSpan(const PP_AttrProp* pAP);
    
    // Defines the style from attributes and properties of an AbiWord <p>.
    void fetchAttributesFromAbiBlock(const PP_AttrProp* pAP);
    
    // Defines the style from attributes and properties of an AbiWord <section>.
    void fetchAttributesFromAbiSection(const PP_AttrProp* pAP);
    
    // Defines the style from attributes and properties of an AbiWord <table>.
    void fetchAttributesFromAbiTable(const PP_AttrProp* pAP);
    
    // Defines the style from attributes and properties of an AbiWord <cell>.
    void fetchAttributesFromAbiCell(const PP_AttrProp* pAP);
    
    // Defines the style from attributes and properties of an AbiWord <frame>.
    void fetchAttributesFromAbiFrame(const PP_AttrProp& rAP);
    
    void setStyleName(const UT_UTF8String& rStyleName) {
        m_name = rStyleName;
    }
    
    const UT_UTF8String& getName() const {
        return m_name;
    }
    
    void setFamily(const gchar* pFamily) {
        m_family = pFamily;
    }
    
    void setMasterPageName(const UT_UTF8String& rMasterPageName) {
        m_masterPageName = rMasterPageName;
    }
    
    const UT_UTF8String& getFontName();

    void setBreakBefore(const gchar* pBreakBefore);    
    void setColumnWidth(const gchar* pColumnWidth);
    void setRowHeight(const gchar* pRowHeight);
    void setMinRowHeight(const gchar* pMinRowHeight);
    void inheritTableCellProperties(const ODe_Style_Style& tableStyle);
    void setWrap(const UT_UTF8String& rWrap);
    void setRunThrough(const UT_UTF8String& rRunThrough);
    void setPadding(const UT_UTF8String& rPadding);
    void setHorizontalPos(const UT_UTF8String& rHorizontalPos);
    void setVerticalPos(const UT_UTF8String& rVerticalPos);
    
private:
    
    ////
    // <style:style> attributes
    UT_UTF8String m_name;            // text:style-name
    UT_UTF8String m_family;          // style:family
    UT_UTF8String m_parentStyleName; // style:parent-style-name
    UT_UTF8String m_nextStyleName;   // style:next-style-name
    UT_UTF8String m_masterPageName;  // style:master-page-name


    ////
    // <style:section-properties> attributes
    class SectionProps {
        public:
        
        bool isEmpty() const;
        void fetchAttributesFromAbiProps(const PP_AttrProp& rAP);
        void write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const ;
        SectionProps& operator=(const SectionProps& rSectionProps);
        bool operator==(const SectionProps& rSectionProps) const;
        
        ////
        // <style:columns> attributes
        UT_UTF8String m_columnCount;     // fo:column-count
        UT_UTF8String m_columnGap;       // fo:column-gap
    } *m_pSectionProps;


    ////
    // <style:paragraph-properties> attributes
    class ParagraphProps {
        public:
        
        bool isEmpty() const;
        void fetchAttributesFromAbiProps(const PP_AttrProp& rAP);
        void write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const ;
        ParagraphProps& operator=(const ParagraphProps& rParagraphProps);
        bool operator==(const ParagraphProps& rParagraphProps) const;
        
        UT_UTF8String m_textAlign;       // fo:text-align
        UT_UTF8String m_textIndent;      // fo:text-indent
        UT_UTF8String m_lineHeight;      // fo:line-height    
        UT_UTF8String m_lineHeightAtLeast; // style:line-height-at-least
        UT_UTF8String m_backgroundColor; // fo:background-color
        UT_UTF8String m_widows;          // fo:widows
        UT_UTF8String m_orphans;         // fo:orphans
        UT_UTF8String m_marginLeft;      // fo:margin-left
        UT_UTF8String m_marginRight;     // fo:margin-right
        UT_UTF8String m_marginTop;       // fo:margin-top
        UT_UTF8String m_marginBottom;    // fo:margin-bottom
        UT_UTF8String m_keepWithNext;    // fo:keep-with-next
        UT_UTF8String m_breakBefore;     // fo:break-before
        UT_UTF8String m_writingMode;     // style:writing-mode
    } *m_pParagraphProps;
    
    
    ////
    // <style:text-properties> attributes
    class TextProps {
        public:
        
        bool isEmpty() const;
        void fetchAttributesFromAbiProps(const PP_AttrProp& rAP);
        void write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const ;
        TextProps& operator=(const TextProps& rTextProps);
        bool operator==(const TextProps& rTextProps) const;
        
        UT_UTF8String m_color;           // fo:color
        UT_UTF8String m_underlineType;   // style:text-underline-type
        UT_UTF8String m_lineThroughType; // style:text-line-through-type
        UT_UTF8String m_textPosition;    // style:text-position
        UT_UTF8String m_fontName;        // style:font-name
        UT_UTF8String m_fontSize;        // fo:font-size
        UT_UTF8String m_language;        // fo:language
        UT_UTF8String m_country;         // fo:country
        UT_UTF8String m_fontStyle;       // fo:font-style
        UT_UTF8String m_fontWeight;      // fo:font-weight
        UT_UTF8String m_backgroundColor; // fo:background-color
        UT_UTF8String m_display;         // text:display
    } *m_pTextProps;


    ////
    // <style:table-properties> attributes:
    class TableProps {
        public:
        
        bool isEmpty() const;
        void fetchAttributesFromAbiProps(const PP_AttrProp& rAP);
        void write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const ;
        TableProps& operator=(const TableProps& rTableProps);
        bool operator==(const TableProps& rTableProps) const;
        
        UT_UTF8String m_width;           // style:width
        UT_UTF8String m_backgroundColor; // fo:background-color
        UT_UTF8String m_align;           // table:align
        UT_UTF8String m_marginLeft;      // fo:margin-left
        UT_UTF8String m_marginRight;     // fo:margin-right
    } *m_pTableProps;
    

    ////
    // <style:table-column-properties> attributes:
    class ColumnProps {
        public:
        
        bool isEmpty() const;
        void write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const ;
        ColumnProps& operator=(const ColumnProps& rColumnProps);
        bool operator==(const ColumnProps& rColumnProps) const;
        
        UT_UTF8String m_columnWidth;     // style:column-width
    } *m_pColumnProps;
    
    
    ////
    // <style:table-row-properties> attributes:
    class RowProps {
        public:
        
        bool isEmpty() const;
        void write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const ;
        RowProps& operator=(const RowProps& rRowProps);
        bool operator==(const RowProps& rRowProps) const;
        
        UT_UTF8String m_rowHeight;       // style:row-height
        UT_UTF8String m_minRowHeight;    // style:min-row-height
    } *m_pRowProps;


    ////
    // <style:table-cell-properties> attributes:
    class CellProps {
        public:
        
        bool isEmpty() const;
        void fetchAttributesFromAbiProps(const PP_AttrProp& rAP);
        void write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const ;
        CellProps& operator=(const CellProps& rCellProps);
        bool operator==(const CellProps& rCellProps) const;

        UT_UTF8String m_leftThickness;   // part of fo:border-left
        UT_UTF8String m_leftColor;       // part of fo:border-left
        UT_UTF8String m_rightThickness;  // part of fo:border-right
        UT_UTF8String m_rightColor;      // part of fo:border-right
        UT_UTF8String m_topThickness;    // part of fo:border-top
        UT_UTF8String m_topColor;        // part of fo:border-top
        UT_UTF8String m_bottomThickness; // part of fo:border-bottom
        UT_UTF8String m_bottomColor;     // part of fo:border-bottom
        UT_UTF8String m_backgroundColor; // fo:background-color

        // TODO: support line styles
    } *m_pCellProps;
    
    
    ////
    // <style:graphic-properties> attributes:
    class GraphicProps {
        public:
        
        bool isEmpty() const;
        void fetchAttributesFromAbiProps(const PP_AttrProp& rAP);
        void write(UT_UTF8String& rOutput, const UT_UTF8String& rSpacesOffset) const ;
        GraphicProps& operator=(const GraphicProps& rGraphicProps);
        bool operator==(const GraphicProps& rGraphicProps) const;
        
        UT_UTF8String m_backgroundColor; // fo:background-color
        UT_UTF8String m_borderLeft;      // fo:border-left
        UT_UTF8String m_borderRight;     // fo:border-right
        UT_UTF8String m_borderTop;       // fo:border-top
        UT_UTF8String m_borderBottom;    // fo:border-bottom
        UT_UTF8String m_wrap;            // style:wrap
        UT_UTF8String m_runThrough;      // style:run-through
        UT_UTF8String m_verticalPos;     // style:vertical-pos
        UT_UTF8String m_verticalRel;     // style:vertical-rel
        UT_UTF8String m_horizontalPos;   // style:horizontal-pos
        UT_UTF8String m_horizontalRel;   // style:horizontal-rel
        UT_UTF8String m_padding;         // fo:padding
    } *m_pGraphicProps;
};


#endif //_ODE_STYLE_STYLE_H_
