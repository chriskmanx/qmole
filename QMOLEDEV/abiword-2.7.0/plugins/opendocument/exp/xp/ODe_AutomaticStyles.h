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


#ifndef _ODE_AUTOMATICSTYLES_H_
#define _ODE_AUTOMATICSTYLES_H_

// AbiWord includes
#include <ut_hash.h>

// External includes
#include <gsf/gsf-output.h>

// Internal classes
class ODe_Style_Style;
class ODe_Style_PageLayout;
class ODe_Style_List;

/**
 * Represents a <office:automatic-styles> element.
 */
class ODe_AutomaticStyles {
	
public:

    ~ODe_AutomaticStyles();
    
    void storeTextStyle(ODe_Style_Style*& rpTextStyle);
    void storeParagraphStyle(ODe_Style_Style*& rpParagraphStyle);
    void storeSectionStyle(ODe_Style_Style*& rpSectionStyle);
    void storeGraphicStyle(ODe_Style_Style*& rpGraphicStyle);

    ODe_Style_Style* addTableStyle(const UT_UTF8String& rStyleName);
    ODe_Style_Style* addTableColumnStyle(const UT_UTF8String& rStyleName);
    ODe_Style_Style* addTableRowStyle(const UT_UTF8String& rStyleName);
    ODe_Style_Style* addTableCellStyle(const UT_UTF8String& rStyleName);
	ODe_Style_PageLayout* addPageLayout();
    ODe_Style_List* addListStyle();
    
    void addPageLayout(ODe_Style_PageLayout*& pPageLayout);
    
    ODe_Style_PageLayout* getPageLayout(const gchar* pName) {
        return m_pageLayouts.pick(pName);
    };
    
    ODe_Style_PageLayout* getMasterPage(const gchar* pName) {
        return m_pageLayouts.pick(pName);
    };

	UT_uint32 getSectionStylesCount() const {
        return m_sectionStyles.size();
    }
    
    UT_GenericVector<ODe_Style_Style*>* getParagraphStyles() {
        return m_paragraphStyles.enumerate();
    }
    
    UT_GenericVector<ODe_Style_Style*>* getTextStyles() {
        return m_textStyles.enumerate();
    }
    
    UT_GenericVector<ODe_Style_List*>* getListStyles() {
        return m_listStyles.enumerate();
    }
    
    // Writes <office:automatic-styles> element.
    void write(GsfOutput* pContentStream) const;

private:
    void _storeStyle(ODe_Style_Style*& rpStyle,
                     UT_GenericStringMap<ODe_Style_Style*>& rStyles,
                     const char* pNamingPrefix);

    UT_GenericStringMap<ODe_Style_Style*> m_textStyles;
    UT_GenericStringMap<ODe_Style_Style*> m_paragraphStyles;
    UT_GenericStringMap<ODe_Style_Style*> m_sectionStyles;
    UT_GenericStringMap<ODe_Style_Style*> m_tableStyles;
    UT_GenericStringMap<ODe_Style_Style*> m_tableColumnStyles;
    UT_GenericStringMap<ODe_Style_Style*> m_tableRowStyles;
    UT_GenericStringMap<ODe_Style_Style*> m_tableCellStyles;
    UT_GenericStringMap<ODe_Style_Style*> m_graphicStyles;
    UT_GenericStringMap<ODe_Style_PageLayout*> m_pageLayouts;
    UT_GenericStringMap<ODe_Style_List*> m_listStyles;
};

#endif //_ODE_AUTOMATICSTYLES_H_
