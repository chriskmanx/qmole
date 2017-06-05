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

#ifndef _ODE_STYLES_H_
#define _ODE_STYLES_H_

// AbiWord includes
#include <ut_hash.h>

// External includes
#include <gsf/gsf-output.h>

// Internal classes
class ODe_Style_Style;
class ODe_Style_MasterPage;
class ODe_Style_PageLayout;

// AbiWord classes
class PD_Document;
class PP_AttrProp;


/**
 * This class stores all normal and automatic styles.
 */
class ODe_Styles {
public:

    ~ODe_Styles();

    // Fetch all regular <style:style> elements (the ones that will be defined
    // inside <office:styles>).
    bool fetchRegularStyleStyles(PD_Document* pAbiDoc);

    // Writes the <office:styles> element.
    bool write(GsfOutput* pODT) const;
    
    UT_GenericVector<ODe_Style_Style*>* getParagraphStyles() {
        return m_paragraphStyles.enumerate();
    }
    
    UT_GenericVector<ODe_Style_Style*>* getTextStyles() {
        return m_textStyles.enumerate();
    }

private:
    bool _addStyle(const PP_AttrProp* pAP);

    UT_GenericStringMap<ODe_Style_Style*> m_textStyles;
    UT_GenericStringMap<ODe_Style_Style*> m_paragraphStyles;
};

#endif //_ODE_STYLES_H_
