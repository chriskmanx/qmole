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

#ifndef ODE_DOCUMENTDATA_H_
#define ODE_DOCUMENTDATA_H_

// Internal includes
#include "ODe_AutomaticStyles.h"
#include "ODe_Styles.h"
#include "ODe_FontFaceDecls.h"

// External includes
#include <gsf/gsf-output.h>
#include <stdio.h>

// AbiWord classes
class PD_Document;


/**
 * Stores data of the OpenDocument that is being built (exported from Abi).
 * This data will be used to write the document file itself later on.
 */
class ODe_DocumentData {
public:

    ODe_DocumentData();
    virtual ~ODe_DocumentData();
    
    bool init();
    
    // Do all necessary work before starting to listen the AbiWord document.
    bool doPreListeningWork(PD_Document* pAbiDoc);
    
    ////    
    // Post listening methods
    
    // Do all necessary work after having read the AbiWord document.
    bool doPostListeningWork();
    
    bool writeStylesXML(GsfOutfile* pOdt) const;
    bool writeContentXML(GsfOutfile* pOdt);



    // <office:automatic-styles> for <office:document-styles>
    ODe_AutomaticStyles m_stylesAutoStyles;
    
    // <office:automatic-styles> for <office:document-content>
    ODe_AutomaticStyles m_contentAutoStyles;
    
    // <office:styles> (regular styles)
    ODe_Styles m_styles;
    
    // <office:master-styles> (master page styles)
    UT_GenericStringMap<ODe_Style_MasterPage*> m_masterStyles;
    
    ODe_FontFaceDecls m_stylesXMLFontDecls;
    ODe_FontFaceDecls m_contentXMLFontDecls;
    
    // Temp file holding the content of <office:text>
    //
    // It is used because I only have the complete <office:automatic-styles>
    // after having written the entire <office:text>, but, on
    // content.xml, <office:automatic-styles> must appear *before* <office:text>.
    GsfOutput* m_pOfficeTextTemp;
};

#endif /*ODE_DOCUMENTDATA_H_*/
