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
#include "ODe_Common.h"
#include "ODe_DocumentData.h"
#include "ODe_ListLevelStyle.h"
#include "ODe_Style_List.h"
#include "ODe_Style_MasterPage.h"
#include "ODe_Style_PageLayout.h"
#include "ODe_Style_Style.h"
#include "ut_misc.h"

// External includes
#include <gsf/gsf-outfile.h>
#include <gsf/gsf-output-memory.h>

/**
 * Constructor
 */
ODe_DocumentData::ODe_DocumentData() :
    m_pOfficeTextTemp(NULL)
{
}


/**
 * Destructor
 */
ODe_DocumentData::~ODe_DocumentData() {
    UT_GenericVector<ODe_Style_MasterPage*>* pMasterPageVector;
    UT_uint32 count, i;
    
    pMasterPageVector = m_masterStyles.enumerate();
    count = pMasterPageVector->getItemCount();
    for (i=0; i<count; i++) {
        delete (*pMasterPageVector)[i];
    }    
    DELETEP(pMasterPageVector);
    
    if (m_pOfficeTextTemp != NULL) {
        ODe_gsf_output_close(m_pOfficeTextTemp);
    }
}


/**
 * Do all necessary work before starting to listen the AbiWord document.
 */
bool ODe_DocumentData::doPreListeningWork(PD_Document* pAbiDoc) {
    bool ok;
    
    ok = m_styles.fetchRegularStyleStyles(pAbiDoc);
    if (!ok) {
        return false;
    }


    /////
    // Creates a <style:page-layout> from the info that goes on the <pagesize>
    // tag of abw files.
    ODe_Style_PageLayout* pPageLayout;
    
    pPageLayout = new ODe_Style_PageLayout();
    pPageLayout->setName("Standard");
    
    m_stylesAutoStyles.addPageLayout(pPageLayout);

    pPageLayout->fetchAttributesFromAbiDoc(pAbiDoc);
    
    
    
    // Create the "Standard" master page style
    ODe_Style_MasterPage* pMPStyle;
    pMPStyle = new ODe_Style_MasterPage("Standard", "Standard");
    m_masterStyles.insert("Standard", pMPStyle);
    
    
    m_pOfficeTextTemp = gsf_output_memory_new();
    if (m_pOfficeTextTemp == NULL) {
        return false;
    }
    
    // If we've reached this point, everything is fine.
    return true;
}


/**
 * Do all necessary work after having read the AbiWord document.
 */
bool ODe_DocumentData::doPostListeningWork() {
    
    UT_GenericVector<ODe_Style_Style*>* pStylesVector;
    UT_GenericVector<ODe_Style_List*>* pListStyles;
    UT_GenericVector<ODe_ListLevelStyle*>* pListLevelStyles;
    UT_uint32 i, j, count, count2;
    
    ////
    // Build the <office:font-face-decls> element for the Styles XML file.
    
    pStylesVector = m_stylesAutoStyles.getParagraphStyles();
    count = pStylesVector->getItemCount();
    for (i=0; i<count; i++) {
        m_stylesXMLFontDecls.addFont( (*pStylesVector)[i]->getFontName() );
    }
    
    pStylesVector = m_stylesAutoStyles.getTextStyles();
    count = pStylesVector->getItemCount();
    for (i=0; i<count; i++) {
        m_stylesXMLFontDecls.addFont( (*pStylesVector)[i]->getFontName() );
    }
    
    pStylesVector = m_styles.getParagraphStyles();
    count = pStylesVector->getItemCount();
    for (i=0; i<count; i++) {
        m_stylesXMLFontDecls.addFont( (*pStylesVector)[i]->getFontName() );
    }
    
    pStylesVector = m_styles.getTextStyles();
    count = pStylesVector->getItemCount();
    for (i=0; i<count; i++) {
        m_stylesXMLFontDecls.addFont( (*pStylesVector)[i]->getFontName() );
    }
    
    ////
    // Build the <office:font-face-decls> element for the Content XML file.
    
    pStylesVector = m_contentAutoStyles.getParagraphStyles();
    count = pStylesVector->getItemCount();
    for (i=0; i<count; i++) {
        m_stylesXMLFontDecls.addFont( (*pStylesVector)[i]->getFontName() );
    }
    
    pStylesVector = m_contentAutoStyles.getTextStyles();
    count = pStylesVector->getItemCount();
    for (i=0; i<count; i++) {
        m_contentXMLFontDecls.addFont( (*pStylesVector)[i]->getFontName() );
    }
    
    pListStyles = m_contentAutoStyles.getListStyles();
    count = pListStyles->getItemCount();
    for (i=0; i<count; i++) {
        pListLevelStyles = pListStyles->getNthItem(i)->getListLevelStyles();
        count2 = pListLevelStyles->getItemCount();
        for (j=0; j<count2; j++) {
            m_contentXMLFontDecls.addFont((*pListLevelStyles)[j]->getFontName());
        }
    }
    
    return true;
}


/**
 * 
 */
bool ODe_DocumentData::writeStylesXML(GsfOutfile* pOdt) const {
    GsfOutput* pStylesStream;
    UT_GenericVector<ODe_Style_MasterPage*>* pMasterPageVector;    
    bool ok;
    UT_uint32 count, i;
    
    pStylesStream = gsf_outfile_new_child (pOdt, "styles.xml", FALSE);
    
    const char * const preamble [] = {
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "\n",
    "<office:document-styles"
    " xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\""
    " xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\""
    " xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\""
    " xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\""
    " xmlns:draw=\"urn:oasis:names:tc:opendocument:xmlns:drawing:1.0\""
    " xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\""
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
    " xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
    " xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\""
    " xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\""
    " xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\""
    " xmlns:chart=\"urn:oasis:names:tc:opendocument:xmlns:chart:1.0\""
    " xmlns:dr3d=\"urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0\""
    " xmlns:math=\"http://www.w3.org/1998/Math/MathML\""
    " xmlns:form=\"urn:oasis:names:tc:opendocument:xmlns:form:1.0\""
    " xmlns:script=\"urn:oasis:names:tc:opendocument:xmlns:script:1.0\""
    " xmlns:ooo=\"http://openoffice.org/2004/office\""
    " xmlns:ooow=\"http://openoffice.org/2004/writer\""
    " xmlns:oooc=\"http://openoffice.org/2004/calc\""
    " xmlns:dom=\"http://www.w3.org/2001/xml-events\""
    " office:version=\"1.0\">\n"};
    
    ODe_writeToStream(pStylesStream, preamble, G_N_ELEMENTS(preamble));
    
    m_stylesXMLFontDecls.write(pStylesStream);
    
    m_styles.write(pStylesStream);
    m_stylesAutoStyles.write(pStylesStream);
    
    ODe_writeUTF8String(pStylesStream, " <office:master-styles>\n");
    
    pMasterPageVector = m_masterStyles.enumerate();
    count = pMasterPageVector->getItemCount();
    for (i=0; i<count; i++) {
        ok = (*pMasterPageVector)[i]->write(pStylesStream);
        
        if (!ok) {
            return false;
        }
    }    
    
    ODe_writeUTF8String(pStylesStream, " </office:master-styles>\n");
    
    ODe_writeUTF8String(pStylesStream, "</office:document-styles>");
    
    ODe_gsf_output_close(pStylesStream);
    
    return true;
}


/**
 * 
 */
bool ODe_DocumentData::writeContentXML(GsfOutfile* pOdt) {
    GsfOutput* pContentStream;
    
    pContentStream = gsf_outfile_new_child (pOdt, "content.xml", FALSE);
    
    const char * const preamble [] = {
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "\n",
    "<office:document-content"
    " xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\""
    " xmlns:style=\"urn:oasis:names:tc:opendocument:xmlns:style:1.0\""
    " xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\""
    " xmlns:table=\"urn:oasis:names:tc:opendocument:xmlns:table:1.0\""
    " xmlns:draw=\"urn:oasis:names:tc:opendocument:xmlns:drawing:1.0\""
    " xmlns:fo=\"urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0\""
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\""
    " xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
    " xmlns:meta=\"urn:oasis:names:tc:opendocument:xmlns:meta:1.0\""
    " xmlns:number=\"urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0\""
    " xmlns:svg=\"urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0\""
    " xmlns:chart=\"urn:oasis:names:tc:opendocument:xmlns:chart:1.0\""
    " xmlns:dr3d=\"urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0\""
    " xmlns:math=\"http://www.w3.org/1998/Math/MathML\""
    " xmlns:form=\"urn:oasis:names:tc:opendocument:xmlns:form:1.0\""
    " xmlns:script=\"urn:oasis:names:tc:opendocument:xmlns:script:1.0\""
    " xmlns:ooo=\"http://openoffice.org/2004/office\""
    " xmlns:ooow=\"http://openoffice.org/2004/writer\""
    " xmlns:oooc=\"http://openoffice.org/2004/calc\""
    " xmlns:dom=\"http://www.w3.org/2001/xml-events\""
    " xmlns:xforms=\"http://www.w3.org/2002/xforms\""
    " xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\""
    " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""
    " office:version=\"1.0\">\n"};
    
    ODe_writeToStream(pContentStream, preamble, G_N_ELEMENTS(preamble));
    
    m_contentXMLFontDecls.write(pContentStream);
    
    m_contentAutoStyles.write(pContentStream);
    
    ODe_writeUTF8String(pContentStream, " <office:body>\n"
                                        "  <office:text>\n");
       
    ODe_gsf_output_write(pContentStream, gsf_output_size (m_pOfficeTextTemp),
			 gsf_output_memory_get_bytes (GSF_OUTPUT_MEMORY (m_pOfficeTextTemp)));
    
    ODe_gsf_output_close (m_pOfficeTextTemp);
    m_pOfficeTextTemp = NULL;
    
    ODe_writeUTF8String(pContentStream,
        "  </office:text>\n"
        " </office:body>\n"
        "</office:document-content>");
        
    ODe_gsf_output_close(pContentStream);
    
    return true;
}
