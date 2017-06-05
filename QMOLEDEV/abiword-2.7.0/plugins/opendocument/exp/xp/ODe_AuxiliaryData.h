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

#ifndef ODE_AUXILIARYDATA_H_
#define ODE_AUXILIARYDATA_H_

// AbiWord includes
#include <ut_vector.h>
#include <ut_string_class.h>


/**
 * All paragraph styles used to define the chapter levels of a document are
 * called heading styles. Paragraphs that uses heading styles are the ones that
 * appears on a table of contents.
 * 
 * This class stores the name and the respective outline level of all those
 * styles. It's necessary to do this because, in an OpenDocument document,
 * a standard paragraph is <text:p [...]>, but a heading paragraph is a
 * <text:h text:outline-level="x" [...]>
 * 
 * So, when translating an AbiWord paragraph, we must know wheter it will map
 * into an OpenDocument <text:p> or into a <text:h>.
 */
class ODe_HeadingStyles {
public:

    virtual ~ODe_HeadingStyles();

    /**
     * Given a paragraph style name, this method returns its outline level.
     * 0 (zero) is returned it the style name is not used by heading paragraphs.
     */
    UT_uint8 getHeadingOutlineLevel(const UT_UTF8String& rStyleName) const;
    
    void addStyleName(const gchar* pStyleName, UT_uint8 outlineLevel);
    
private:
    UT_GenericVector<UT_UTF8String*> m_styleNames;
    UT_GenericVector<UT_uint8> m_outlineLevels;
};


/**
 * Auxiliary data used and shared by all listener implementations.
 */
class ODe_AuxiliaryData {
public:
    ODe_AuxiliaryData();

    ODe_HeadingStyles m_headingStyles;
    
    // The number of tables already added to the document.
    UT_uint32 m_tableCount;
    
    // The number of frames already added to the document.
    UT_uint32 m_frameCount;
    
    // The number of notes (footnotes and endnotes) already added to the document.
    UT_uint32 m_noteCount;
    
    // The number of TOCs (Table of Confents) already added to the document.
    UT_uint32 m_TOCCount;
};

#endif /*ODE_AUXILIARYDATA_H_*/
