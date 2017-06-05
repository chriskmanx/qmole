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
#include "ODe_Main_Listener.h"

// Internal includes
#include "ODe_Common.h"
#include "ODe_Style_MasterPage.h"
#include "ODe_Style_PageLayout.h"
#include "ODe_Style_Style.h"
#include "ODe_ListenerAction.h"
#include "ODe_Text_Listener.h"
#include "ODe_DocumentData.h"

// AbiWord includes
#include <pp_AttrProp.h>
#include <gsf/gsf-output-memory.h>

/**
 * Constructor
 */
ODe_Main_Listener::ODe_Main_Listener(ODe_DocumentData& rDocumentData,
                                     ODe_AuxiliaryData& rAuxiliaryData)
							: m_rDocumentData(rDocumentData),
                              m_rAuxiliaryData(rAuxiliaryData),
                              m_onHeaderFooterSection(false),
                              m_openedODSection(false),
                              m_isFirstSection(true)
{
}


/**
 * Destructor
 */
ODe_Main_Listener::~ODe_Main_Listener() {
}


/**
 * Override of ODe_AbiDocListenerImpl::openSection
 */
void ODe_Main_Listener::openSection(const PP_AttrProp* pAP,
                                          ODe_ListenerAction& rAction) {

    if (_isHeaderFooterSection(pAP)) {
        _openHeaderFooterSection(pAP, rAction);
        return;
    }

    // We have a problem with sections. part of an Abi <section> translates into
    // an OpenDocument <style:master-page>/<style:page-layout> and part into
    // an OpenDocument <text:section>.
    //
    // For example:
    // Info about headers and footers goes into OpenDocument <style:master-page>.
    // Into about columns goes into OpenDocument <text:section>.

    ODe_Style_MasterPage* pMPStyle = NULL;
    bool pendingMasterPageStyleChange = false;
    UT_UTF8String masterPageStyleName;
    ODe_Text_Listener* pTextListener;

    if (ODe_Style_PageLayout::hasPageLayoutInfo(pAP)) {

        ODe_Style_PageLayout* pPageLayout;        

        if (m_isFirstSection) {
            // Use stantard master page and page layout.

            pPageLayout = m_rDocumentData.m_stylesAutoStyles.getPageLayout("Standard");
            pMPStyle = m_rDocumentData.m_masterStyles.pick("Standard");
            UT_DEBUGMSG(("Got PageLayout %x AutoStyles %x \n",pPageLayout,&m_rDocumentData.m_stylesAutoStyles));
            m_isFirstSection = false;

        } else {

            UT_UTF8String styleName;
            UT_UTF8String layoutName;
    		
    		UT_UTF8String_sprintf(styleName, "MasterStyle%d",
                                  m_rDocumentData.m_masterStyles.size());
    		

        	pPageLayout = m_rDocumentData.m_stylesAutoStyles.addPageLayout();
        	layoutName = pPageLayout->getName();
            
            pMPStyle = new ODe_Style_MasterPage(styleName.utf8_str(),
                                                layoutName.utf8_str());

            m_rDocumentData.m_masterStyles.insert(styleName.utf8_str(),pMPStyle);
            pendingMasterPageStyleChange = true;
            masterPageStyleName = styleName;
        }
        
        pMPStyle->fetchAttributesFromAbiSection(pAP);
        pPageLayout->fetchAttributesFromAbiSection(pAP);
	//
	// OK Set up a "standard" default set of properties
	//
	ODe_Style_PageLayout* pStandard = new ODe_Style_PageLayout();
	UT_UTF8String sName = "Standard";
	pStandard->setName(sName);
        m_rDocumentData.m_contentAutoStyles.addPageLayout(pStandard);
	pStandard->fetchAttributesFromAbiSection(pAP);
    } else {
        // Without this, '!strcmp(pId, pValue)' fails in _isHeaderFooterSection()
        // below, which ultimately leads to a crash due to an uninitialized
        // FILE* being passed to fwrite() (see bug 9798 for a sample).
        pMPStyle = m_rDocumentData.m_masterStyles.pick("Standard");
        pMPStyle->fetchAttributesFromAbiSection(pAP);
    }
    

    if (ODe_Style_Style::hasSectionInfo(pAP)) {
        
        ODe_Style_Style* pSectionStyle;
        pSectionStyle = new ODe_Style_Style();
        pSectionStyle->setFamily("section");
        
        pSectionStyle->fetchAttributesFromAbiSection(pAP);
        m_rDocumentData.m_contentAutoStyles.storeSectionStyle(pSectionStyle);

        ODe_Style_PageLayout* pPageLayout = m_rDocumentData.m_contentAutoStyles.addPageLayout();
	pPageLayout->fetchAttributesFromAbiSection(pAP);
       
        UT_UTF8String output;
        
        UT_UTF8String_sprintf(output,
        	"   <text:section text:style-name=\"%s\" text:name=\"Section%u\">\n",
        	pSectionStyle->getName().utf8_str(),
            m_rDocumentData.m_contentAutoStyles.getSectionStylesCount());
        
        ODe_writeToFile(m_rDocumentData.m_pOfficeTextTemp, output);
        
        m_openedODSection = true;
    }
    
    if (pendingMasterPageStyleChange) {
        pTextListener = new ODe_Text_Listener(
                                          m_rDocumentData.m_contentAutoStyles,
                                          m_rDocumentData.m_pOfficeTextTemp,
                                          m_rAuxiliaryData,
                                          0, 3,
                                          masterPageStyleName);
    } else {
        pTextListener = new ODe_Text_Listener(
                                          m_rDocumentData.m_contentAutoStyles,
                                          m_rDocumentData.m_pOfficeTextTemp,
                                          m_rAuxiliaryData,
                                          0, 3);
    }
    rAction.pushListenerImpl(pTextListener, true);
}


/**
 * Override of ODe_AbiDocListenerImpl::closeSection
 */
void ODe_Main_Listener::closeSection(ODe_ListenerAction& /*rAction*/) 
{
	if (m_openedODSection) {

		ODe_writeToFile(m_rDocumentData.m_pOfficeTextTemp, "   </text:section>\n");
		m_openedODSection = false;
        
	} else if (m_onHeaderFooterSection) {
        m_onHeaderFooterSection = false;
    }
}


/**
 * 
 */
bool ODe_Main_Listener::_isHeaderFooterSection(const PP_AttrProp* pAP) const {
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getAttribute("type", pValue);
    if (ok && pValue != NULL) {
        if (!strcmp(pValue, "header") || !strcmp(pValue, "footer")) {
            return true;
        }
    }

    return false;
}


/**
 * 
 */
void ODe_Main_Listener::_openHeaderFooterSection(
                                                  const PP_AttrProp* pAP,
                                                  ODe_ListenerAction& rAction) {
    const gchar* pValue;
    const gchar* pId = NULL;
    bool ok;
    UT_GenericVector<ODe_Style_MasterPage*>* pMasterPageVector;
    UT_uint32 count, i;
    const ODe_Style_MasterPage* pMPageStyle;
    GsfOutput* pTextOutput = NULL;
    
    pMasterPageVector = m_rDocumentData.m_masterStyles.enumerate();
    count = pMasterPageVector->getItemCount();

    
    ok = pAP->getAttribute("id", pValue);
    if (ok && pValue != NULL) {
        pId = pValue;
    } else {
        UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
    }
    

    ok = pAP->getAttribute("type", pValue);
    if (!ok || pValue == NULL) {
        UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
    }
    
    ok = false;
    if (!strcmp("header", pValue)) {
        
        for (i=0; i<count && !ok; i++) {
            pMPageStyle = (*pMasterPageVector)[i];
            pValue = pMPageStyle->getAbiHeaderId().utf8_str();
            if (!strcmp(pId, pValue)) {
                ok = true; // found it. get out of this "for" loop
                pTextOutput =  pMPageStyle->getHeaderContentTempFile();
            }
        }
        
    } else {
        // It's a footer
        
        for (i=0; i<count && !ok; i++) {
            pMPageStyle = (*pMasterPageVector)[i];
            pValue = pMPageStyle->getAbiFooterId().utf8_str();
            if (!strcmp(pId, pValue)) {
                ok = true; // found it. get out of this "for" loop
                pTextOutput = pMPageStyle->getFooterContentTempFile();
            }
        }
    }

    if (!ok) {
        UT_ASSERT(UT_SHOULD_NOT_HAPPEN); // We should have found this header/footer.
        pTextOutput = gsf_output_memory_new ();
    }

    m_openedODSection = false;
    m_onHeaderFooterSection = true;
    rAction.pushListenerImpl(new ODe_Text_Listener(
                                     m_rDocumentData.m_stylesAutoStyles,
                                     pTextOutput,
                                     m_rAuxiliaryData,
                                     0, 4),
                             true);
}
