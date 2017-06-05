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
#include "ODe_Frame_Listener.h"

// Internal includes
#include "ODe_AutomaticStyles.h"
#include "ODe_AuxiliaryData.h"
#include "ODe_Common.h"
#include "ODe_Style_Style.h"
#include "ODe_Text_Listener.h"
#include "ODe_ListenerAction.h"
#include "ODe_Style_PageLayout.h"

// AbiWord includes
#include <pp_AttrProp.h>
#include <ut_units.h>

/**
 * Constructor
 */
ODe_Frame_Listener::ODe_Frame_Listener(ODe_AutomaticStyles& rAutomatiStyles,
                                       GsfOutput* pTextOutput,
                                       ODe_AuxiliaryData& rAuxiliaryData,
                                       UT_uint8 zIndex,
                                       UT_uint8 spacesOffset)
                                       :
                                       ODe_AbiDocListenerImpl(spacesOffset),
                                       m_rAutomatiStyles(rAutomatiStyles),
                                       m_pTextOutput(pTextOutput),
                                       m_rAuxiliaryData(rAuxiliaryData),
                                       m_zIndex(zIndex)
{
}


/**
 * 
 */
void ODe_Frame_Listener::openFrame(const PP_AttrProp* pAP,
                                   ODe_ListenerAction& rAction) {
    bool ok = false;
    const gchar* pValue = NULL;
    
    ok = pAP->getProperty("frame-type", pValue);
    
    if (pValue && !strcmp(pValue, "textbox")) {
        _openODTextbox(*pAP, rAction);
    }
}


/**
 * 
 */
void ODe_Frame_Listener::closeFrame(ODe_ListenerAction& rAction) {
        UT_UTF8String output;
        
        m_spacesOffset--;
        _printSpacesOffset(output);
        output += "</draw:text-box>\n";
        
        m_spacesOffset--;
        _printSpacesOffset(output);
        output += "</draw:frame>";
    
        ODe_writeToFile(m_pTextOutput, output);
        
        rAction.popListenerImpl();
}


/**
 * 
 */
void ODe_Frame_Listener::openTable(const PP_AttrProp* /*pAP*/,
                                   ODe_ListenerAction& rAction) {
    ODe_Text_Listener* pTextListener;
    pTextListener = new ODe_Text_Listener(m_rAutomatiStyles,
                                          m_pTextOutput,
                                          m_rAuxiliaryData,
                                          m_zIndex+1,
                                          m_spacesOffset);
    rAction.pushListenerImpl(pTextListener, true);
}


/**
 * 
 */
void ODe_Frame_Listener::openBlock(const PP_AttrProp* /*pAP*/,
                                   ODe_ListenerAction& rAction) {
    ODe_Text_Listener* pTextListener;
    pTextListener = new ODe_Text_Listener(m_rAutomatiStyles,
                                          m_pTextOutput,
                                          m_rAuxiliaryData,
                                          m_zIndex+1,
                                          m_spacesOffset);
    rAction.pushListenerImpl(pTextListener, true);
}


/**
 * 
 */
void ODe_Frame_Listener::_openODTextbox(const PP_AttrProp& rAP,
                                        ODe_ListenerAction& /*rAction*/) 
{
    UT_UTF8String output;
    UT_UTF8String str;
    bool ok;
    const gchar* pValue = NULL;
    ODe_Style_Style* pStyle;
    
    pStyle = new ODe_Style_Style();
    pStyle->setFamily("graphic");
    pStyle->fetchAttributesFromAbiFrame(rAP);
    
    // Abi frames have no padding
    // (no margin between frame borders and its content)
    pStyle->setPadding("0cm");
    
    // Abi frames are aways positioned from its top-left corner.
    pStyle->setHorizontalPos("from-left");
    pStyle->setVerticalPos("from-top");
    
    m_rAutomatiStyles.storeGraphicStyle(pStyle);

    ////
    // Write <draw:frame>
    
    _printSpacesOffset(output);
    output += "<draw:frame";

    UT_UTF8String_sprintf(str, "Frame%u", m_rAuxiliaryData.m_frameCount+1);
    ODe_writeAttribute(output, "draw:name", str);
    m_rAuxiliaryData.m_frameCount++;
    
    ODe_writeAttribute(output, "draw:style-name", pStyle->getName());

    UT_UTF8String_sprintf(str, "%u", m_zIndex);
    ODe_writeAttribute(output, "draw:z-index", str);


    ok = rAP.getProperty("position-to", pValue);

    if (pValue && !strcmp(pValue, "block-above-text")) {

        ODe_writeAttribute(output, "text:anchor-type", "paragraph");

        ok = rAP.getProperty("xpos", pValue);
        UT_ASSERT(ok && pValue != NULL);
        ODe_writeAttribute(output, "svg:x", pValue);

        ok = rAP.getProperty("ypos", pValue);
        UT_ASSERT(ok && pValue != NULL);
        ODe_writeAttribute(output, "svg:y", pValue);
    } else {
        // Everything else (column and page) will be treated as page
        // anchored.
        
        ODe_writeAttribute(output, "text:anchor-type", "page");

	if(pValue && !strcmp(pValue, "column-above-text"))
	{
	  //
	  // Get the most recent page style so we can do the arithmetic
	  // Won't work for x in multi-columned docs
	  //
	    UT_uint32 numPStyles =  m_rAutomatiStyles.getSectionStylesCount();
	    UT_UTF8String stylePName;
	    UT_UTF8String_sprintf(stylePName, "PLayout%d", numPStyles + 1);
	    ODe_Style_PageLayout * pPageL = m_rAutomatiStyles.getPageLayout(stylePName.utf8_str());

	    ok = rAP.getProperty("frame-col-xpos", pValue);
	    UT_ASSERT(ok && pValue != NULL);
	    double xCol =  UT_convertToInches(pValue);
	    const gchar* pSVal= pPageL->getPageMarginLeft();
	    double xPageL = UT_convertToInches(pSVal);
	    double xTot = xPageL + xCol;
	    pValue = UT_convertInchesToDimensionString(DIM_IN,xTot,"4");
	    ODe_writeAttribute(output, "svg:x", pValue);
        
	    ok = rAP.getProperty("frame-col-ypos", pValue);
	    UT_ASSERT(ok && pValue != NULL);
	    double yCol =  UT_convertToInches(pValue);
	    pSVal= pPageL->getPageMarginTop();
	    double yPageL = UT_convertToInches(pSVal);
	    double yTot = yPageL + yCol;
	    pValue = UT_convertInchesToDimensionString(DIM_IN,yTot,"4");
	    ODe_writeAttribute(output, "svg:y", pValue);	  
	}
	else
	{
	    ok = rAP.getProperty("frame-page-xpos", pValue);
	    UT_ASSERT(ok && pValue != NULL);
	    ODe_writeAttribute(output, "svg:x", pValue);
        
	    ok = rAP.getProperty("frame-page-ypos", pValue);
	    UT_ASSERT(ok && pValue != NULL);
	    ODe_writeAttribute(output, "svg:y", pValue);
	}
    }
    
    
    ok = rAP.getProperty("frame-width", pValue);
    if (ok && pValue != NULL) {
        ODe_writeAttribute(output, "svg:width", pValue);
    }
    
    output += ">\n";
    
    ODe_writeToFile(m_pTextOutput, output);
    m_spacesOffset++;
    
    ////
    // Write <draw:text-box>
    
    output.clear();
    _printSpacesOffset(output);
    output += "<draw:text-box";
    
    ok = rAP.getProperty("frame-height", pValue);
    if (ok && pValue != NULL) {
        ODe_writeAttribute(output, "fo:min-height", pValue);
    }
    
    output += ">\n";
    
    ODe_writeToFile(m_pTextOutput, output);
    m_spacesOffset++;
}
