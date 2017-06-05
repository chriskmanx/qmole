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
#include "ODe_Text_Listener.h"

// Internal includes
#include "ODe_AutomaticStyles.h"
#include "ODe_AuxiliaryData.h"
#include "ODe_Common.h"
#include "ODe_Frame_Listener.h"
#include "ODe_ListenerAction.h"
#include "ODe_ListLevelStyle.h"
#include "ODe_Note_Listener.h"
#include "ODe_Style_List.h"
#include "ODe_Style_Style.h"
#include "ODe_Table_Listener.h"
#include "ODe_Style_PageLayout.h"

// AbiWord includes
#include <pp_AttrProp.h>
#include <gsf/gsf-output-memory.h>
#include <ut_units.h>

// External includes
#include <stdlib.h>


/**
 * Constructor
 * 
 * @param pTextOutput Handle to the file (often a temp one) that will receive 
 *                    the ODT output produced by this listener.
 */
ODe_Text_Listener::ODe_Text_Listener(ODe_AutomaticStyles& rAutomatiStyles,
                                     GsfOutput* pTextOutput,
                                     ODe_AuxiliaryData& rAuxiliaryData,
                                     UT_uint8 zIndex,
                                     UT_uint8 spacesOffset) :
                        ODe_AbiDocListenerImpl(spacesOffset),
                        m_openedODParagraph(false),
                        m_openedODSpan(false),
                        m_isFirstCharOnParagraph(true),
                        m_openedODTextboxFrame(false),
                        m_openedODNote(false),
                        m_pParagraphContent(NULL),
                        m_currentListLevel(0),
                        m_pCurrentListStyle(NULL),
                        m_pendingColumnBrake(false),
                        m_pendingPageBrake(false),
                        m_pendingMasterPageStyleChange(false),
                        m_rAutomatiStyles(rAutomatiStyles),
                        m_pTextOutput(pTextOutput),
                        m_rAuxiliaryData(rAuxiliaryData),
                        m_zIndex(zIndex)
{
}


/**
 * Constructor
 * 
 * @param pTextOutput Handle to the file (often a temp one) that will receive 
 *                    the ODT output produced by this listener.
 */
ODe_Text_Listener::ODe_Text_Listener(
                             ODe_AutomaticStyles& rAutomatiStyles,
                             GsfOutput* pTextOutput,
                             ODe_AuxiliaryData& rAuxiliaryData,
                             UT_uint8 zIndex,
                             UT_uint8 spacesOffset,
                             const UT_UTF8String& rPendingMasterPageStyleName) :
                        ODe_AbiDocListenerImpl(spacesOffset),
                        m_openedODParagraph(false),
                        m_openedODSpan(false),
                        m_isFirstCharOnParagraph(true),
                        m_openedODTextboxFrame(false),
                        m_openedODNote(false),
                        m_pParagraphContent(NULL),
                        m_currentListLevel(0),
                        m_pCurrentListStyle(NULL),
                        m_pendingColumnBrake(false),
                        m_pendingPageBrake(false),
                        m_pendingMasterPageStyleChange(true),
                        m_masterPageStyleName(rPendingMasterPageStyleName),
                        m_rAutomatiStyles(rAutomatiStyles),
                        m_pTextOutput(pTextOutput),
                        m_rAuxiliaryData(rAuxiliaryData),
                        m_zIndex(zIndex)
{
}


/**
 * 
 */
ODe_Text_Listener::~ODe_Text_Listener() {
    // Check if there is nothing being left unfinished.
    
    UT_ASSERT_HARMLESS(!m_openedODParagraph);
    UT_ASSERT_HARMLESS(!m_openedODSpan);

    UT_ASSERT_HARMLESS(m_currentListLevel == 0);
    UT_ASSERT_HARMLESS(m_pCurrentListStyle == NULL);
}


/**
 * 
 */
void ODe_Text_Listener::openTable(const PP_AttrProp* /*pAP*/,
                                  ODe_ListenerAction& rAction) {
    _closeODParagraph();
    _closeODList();

    rAction.pushListenerImpl(new ODe_Table_Listener(m_rAutomatiStyles,
                                                    m_pTextOutput,
                                                    m_rAuxiliaryData,
                                                    0,
                                                    m_spacesOffset),
                             true);
}


/**
 * Override of ODe_AbiDocListenerImpl::openBlock
 */
void ODe_Text_Listener::openBlock(const PP_AttrProp* pAP,
                                  ODe_ListenerAction& /*rAction*/) {

    _closeODParagraph();

    // We first handle the list info of that paragraph, if it exists.
    _openODListItem(pAP);
    
    // Then we try to open an OpenDocument paragraph out of this AbiWord block.
    _openODParagraph(pAP);

}


/**
 * 
 */
void ODe_Text_Listener::closeBlock() {
    if (m_openedODParagraph) {
        if (m_isHeadingParagraph) {
            ODe_writeToFile(m_pParagraphContent, "</text:h>\n");
        } else {
            ODe_writeToFile(m_pParagraphContent, "</text:p>\n");
        }
    }
}


/**
 * 
 */
void ODe_Text_Listener::openSpan(const PP_AttrProp* pAP) {
    UT_UTF8String styleName;
    bool ok;
    const gchar* pValue;
    
    if ( ODe_Style_Style::hasTextStyleProps(pAP) ) {
            
        // Need to create a new automatic style to hold those paragraph
        // properties.
        
        ODe_Style_Style* pStyle;
        pStyle = new ODe_Style_Style();
        pStyle->setFamily("text");
        
        pStyle->fetchAttributesFromAbiSpan(pAP);
        
        m_rAutomatiStyles.storeTextStyle(pStyle);
        styleName = pStyle->getName();
        
    } else {
        ok = pAP->getAttribute("style", pValue);
        if (ok) {
            styleName = pValue;
        }
    }
    
    if (!styleName.empty()) {
        UT_UTF8String output;
        
        UT_UTF8String_sprintf(output, "<text:span text:style-name=\"%s\">",
                              styleName.escapeXML().utf8_str());
                              
        ODe_writeToFile(m_pParagraphContent, output);
        m_openedODSpan = true;
    }
}


/**
 * 
 */
void ODe_Text_Listener::closeSpan() {
    if (m_openedODSpan) {
        ODe_writeToFile(m_pParagraphContent, "</text:span>");
        m_openedODSpan = false;
    }
}


/**
 * 
 */
void ODe_Text_Listener::openFrame(const PP_AttrProp* pAP,
                                  ODe_ListenerAction& rAction) {
    bool ok = false;
    const gchar* pValue = NULL;
    
    ok = pAP->getProperty("frame-type", pValue);
    
    if (pValue && !strcmp(pValue, "textbox")) {
        ODe_Frame_Listener* pFrameListener;
            
        // Paragraph anchored textboxes goes inside the paragraph that
        // it's anchored to, right before the paragraph contents.
        //
        // Page anchored textboxes goes inside the closest previous paragraph.

        pFrameListener = new ODe_Frame_Listener(m_rAutomatiStyles,
                                                m_pTextOutput,
                                                m_rAuxiliaryData,
                                                m_zIndex,
                                                m_spacesOffset);
                                                
        // Make the frame element appear on a new line
        ODe_writeToFile(m_pTextOutput, "\n");
                                                
        rAction.pushListenerImpl(pFrameListener, true);
        m_openedODTextboxFrame = true;
    } else if (pValue && !strcmp(pValue, "image")) {
        ok = pAP->getAttribute("strux-image-dataid", pValue);
        if(ok && pValue)
            insertPositionedImage(pValue, pAP);

        m_openedODTextboxFrame = true;
    }
}


/**
 * 
 */
void ODe_Text_Listener::closeFrame(ODe_ListenerAction& rAction) {

    if (m_openedODTextboxFrame) {
        m_openedODTextboxFrame = false;
    } else {
        // We are inside a textbox.        
        _closeODParagraph();
        rAction.popListenerImpl();
    }
}


/**
 * 
 */
void ODe_Text_Listener::openField(const fd_Field* field, const UT_UTF8String& fieldType, const UT_UTF8String& fieldValue) {
    UT_return_if_fail(field && fieldType.length());

    UT_UTF8String escape = fieldValue;
    escape.escapeXML();

    if(!strcmp(fieldType.utf8_str(),"list_label")) {
        return;  // don't do anything with list labels
    } else if(!strcmp(fieldType.utf8_str(),"page_number")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:page-number>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"page_count")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:page-count>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"meta_creator")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:author-name>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"meta_title")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:title>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"meta_description")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:description>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"meta_subject")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:subject>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"meta_keywords")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:keywords>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"char_count")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:character-count>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"word_count")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:word-count>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"para_count")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:paragraph-count>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"file_name")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:file-name>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"time")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:time>%s",escape.utf8_str()));
    } else if(!strcmp(fieldType.utf8_str(),"date")) {
        ODe_writeToFile(m_pParagraphContent, UT_UTF8String_sprintf("<text:date>%s",escape.utf8_str()));
    } else {
        UT_DEBUGMSG(("openField(): Unhandled field in the ODT exporter: %s\n", fieldType.utf8_str()));
    }
}

/**
 * 
 */

void ODe_Text_Listener::closeField(const UT_UTF8String& fieldType) {
    UT_return_if_fail(fieldType.length());

    if(!strcmp(fieldType.utf8_str(),"list_label")) {
        return;  // don't do anything with list labels
    } else if(!strcmp(fieldType.utf8_str(),"page_number")) {
        ODe_writeToFile(m_pParagraphContent, "</text:page-number>");
    } else if(!strcmp(fieldType.utf8_str(),"page_count")) {
        ODe_writeToFile(m_pParagraphContent, "</text:page-count>");
    } else if(!strcmp(fieldType.utf8_str(),"meta_creator")) {
        ODe_writeToFile(m_pParagraphContent, "</text:author-name>");
    } else if(!strcmp(fieldType.utf8_str(),"meta_title")) {
        ODe_writeToFile(m_pParagraphContent, "</text:title>");
    } else if(!strcmp(fieldType.utf8_str(),"meta_description")) {
        ODe_writeToFile(m_pParagraphContent, "</text:description>");
    } else if(!strcmp(fieldType.utf8_str(),"meta_subject")) {
        ODe_writeToFile(m_pParagraphContent, "</text:subject>");
    } else if(!strcmp(fieldType.utf8_str(),"meta_keywords")) {
        ODe_writeToFile(m_pParagraphContent, "</text:keywords>");
    } else if(!strcmp(fieldType.utf8_str(),"char_count")) {
        ODe_writeToFile(m_pParagraphContent, "</text:character-count>");
    } else if(!strcmp(fieldType.utf8_str(),"word_count")) {
        ODe_writeToFile(m_pParagraphContent, "</text:word-count>");
    } else if(!strcmp(fieldType.utf8_str(),"para_count")) {
        ODe_writeToFile(m_pParagraphContent, "</text:paragraph-count>");
    } else if(!strcmp(fieldType.utf8_str(),"file_name")) {
        ODe_writeToFile(m_pParagraphContent, "</text:file-name>");
    } else if(!strcmp(fieldType.utf8_str(),"time")) {
        ODe_writeToFile(m_pParagraphContent, "</text:time>");
    } else if(!strcmp(fieldType.utf8_str(),"date")) {
        ODe_writeToFile(m_pParagraphContent, "</text:date>");
    } else {
        UT_DEBUGMSG(("closeField(): Unhandled field in the ODT exporter: %s\n", fieldType.utf8_str()));
    }
}

/**
 * 
 */


void ODe_Text_Listener::openFootnote(const PP_AttrProp* /*pAP*/,
                                     ODe_ListenerAction& rAction) {
    ODe_Note_Listener* pNoteListener;
    
    pNoteListener = new ODe_Note_Listener(m_rAutomatiStyles,
                                          m_pParagraphContent,
                                          m_rAuxiliaryData,
                                          m_spacesOffset);
    
    rAction.pushListenerImpl(pNoteListener, true);
    m_openedODNote = true;
}


/**
 * 
 */
void ODe_Text_Listener::closeFootnote(ODe_ListenerAction& rAction) {
    if (m_openedODNote) {
        // We had a footnote.        
        m_openedODNote = false;
    } else {
        // We were inside a footnote.
        _closeODParagraph();
        _closeODList();
        rAction.popListenerImpl();
    }
}


/**
 * 
 */
void ODe_Text_Listener::openEndnote(const PP_AttrProp* /*pAP*/,
                                    ODe_ListenerAction& rAction) {
    ODe_Note_Listener* pNoteListener;
    
    pNoteListener = new ODe_Note_Listener(m_rAutomatiStyles,
                                          m_pParagraphContent,
                                          m_rAuxiliaryData,
                                          m_spacesOffset);
    
    rAction.pushListenerImpl(pNoteListener, true);
    m_openedODNote = true;
}


/**
 * 
 */
void ODe_Text_Listener::closeEndnote(ODe_ListenerAction& rAction) {
    if (m_openedODNote) {
        // We had a endnote.        
        m_openedODNote = false;
    } else {
        // We were inside an endnote.
        _closeODParagraph();
        _closeODList();
        rAction.popListenerImpl();
    }
}


/**
 * 
 */
void ODe_Text_Listener::openAnnotation(const PP_AttrProp* pAP) {

    UT_UTF8String output = "<office:annotation>", escape;

    const gchar* pValue = NULL;

    if(pAP && pAP->getProperty("annotation-author",pValue) && pValue && *pValue) {
        escape = pValue;
        escape.escapeXML();

        output += "<dc:creator>";
        output += escape;
        output += "</dc:creator>";
    }

    if(pAP && pAP->getProperty("annotation-date",pValue) && pValue && *pValue) {
        escape = pValue;
        escape.escapeXML();

        // TODO: is our property a valid date value?
        output += "<dc:date>";
        output += escape;
        output += "</dc:date>";
    }

    // TODO: export annotation-title somehow?

    ODe_writeToFile(m_pParagraphContent, output);
}

/**
 * 
 */
void ODe_Text_Listener::closeAnnotation() {
    UT_UTF8String output = "</office:annotation>";
    ODe_writeToFile(m_pParagraphContent, output);
}



/**
 * 
 */
void ODe_Text_Listener::openTOC(const PP_AttrProp* pAP) {
    UT_UTF8String output;
    bool ok;
    const gchar* pValue = 0;
    UT_uint8 outlineLevel;
    UT_UTF8String str;
    
    _closeODParagraph();
    _closeODList();
    
    m_rAuxiliaryData.m_TOCCount++;
    
    ////
    // Write <text:table-of-content> and <text:table-of-content-source>
    
    str.clear();
    _printSpacesOffset(str);
    
    UT_UTF8String_sprintf(output,
        "%s<text:table-of-content text:protected=\"true\""
        " text:name=\"Table of Contents%u\">\n",
        str.utf8_str(), m_rAuxiliaryData.m_TOCCount);
   
    ODe_writeToFile(m_pTextOutput, output);
    m_spacesOffset++;
    output.assign("");
    
    
    _printSpacesOffset(output);
    output += "<text:table-of-content-source text:outline-level=\"4\">\n";
    
    ODe_writeToFile(m_pTextOutput, output);
    m_spacesOffset++;
    output.assign("");
    
    ////
    // Write <text:index-title-template>
    
    ok = pAP->getProperty("toc-has-heading", pValue);
    UT_ASSERT_HARMLESS(ok && pValue != NULL);

    if (pValue && (*pValue == '1')) {
    
        _printSpacesOffset(output);
        output += "<text:index-title-template text:style-name=\"";
        
        ok = pAP->getProperty("toc-heading-style", pValue);
        UT_ASSERT_HARMLESS(ok && pValue != NULL);
        if (ok && pValue) {
            UT_UTF8String escape = pValue;
            output += escape.escapeXML();
        }
        
        output += "\">";
        
        ok = pAP->getProperty("toc-heading", pValue);
        UT_ASSERT_HARMLESS(ok && pValue != NULL);
        if (ok && pValue) {
            UT_UTF8String escape = pValue;
            output += escape.escapeXML();
        }
        
        output += "</text:index-title-template>\n";
        
        ODe_writeToFile(m_pTextOutput, output);
        output.assign("");
    
    }
    
    
    ////
    // Write all <text:table-of-content-entry-template>
    
    for (outlineLevel=1; outlineLevel<=4; outlineLevel++) {

        str.assign("");
        _printSpacesOffset(str);
        
        UT_UTF8String_sprintf(output,
            "%s<text:table-of-content-entry-template"
            " text:outline-level=\"%u\" text:style-name=\"",
            str.utf8_str(), outlineLevel);


        UT_UTF8String_sprintf(str, "toc-dest-style%u", outlineLevel);
        ok = pAP->getProperty(str.utf8_str(), pValue);
        UT_ASSERT_HARMLESS(ok && pValue != NULL);
        
        if (ok && pValue) {
            UT_UTF8String escape = pValue;
            output += escape.escapeXML();
        }
        
        output += "\">\n";
        m_spacesOffset++;
        
        // Fixed TOC structure (at least for now).
        // [chapter][text]............[page-number]
        
        _printSpacesOffset(output);
        output += "<text:index-entry-chapter/>\n";
        
        _printSpacesOffset(output);
        output += "<text:index-entry-text/>\n";
        
        _printSpacesOffset(output);
        output += "<text:index-entry-tab-stop style:type=\"right\""
                  " style:leader-char=\".\"/>\n";
                  
        _printSpacesOffset(output);
        output += "<text:index-entry-page-number/>\n";
        
        m_spacesOffset--;
        _printSpacesOffset(output);
        output += "</text:table-of-content-entry-template>\n";
        
        ODe_writeToFile(m_pTextOutput, output);
        output.assign("");
    }
    

    m_spacesOffset--;
    _printSpacesOffset(output);
    output += "</text:table-of-content-source>\n";
    ODe_writeToFile(m_pTextOutput, output);
}


/**
 * 
 */
void ODe_Text_Listener::closeTOC() {
    UT_UTF8String output;
    
    m_spacesOffset--;
    _printSpacesOffset(output);
    output += "</text:table-of-content>\n";
    ODe_writeToFile(m_pTextOutput, output);
}


/**
 * 
 */
void ODe_Text_Listener::openBookmark(const PP_AttrProp* pAP) {
    UT_return_if_fail(pAP);

    UT_UTF8String output = "<text:bookmark-start text:name=\"", escape;
    const gchar* pValue = NULL;

    if(pAP->getAttribute("type",pValue) && pValue && (strcmp(pValue, "start") == 0)) {
        if(pAP->getAttribute("name",pValue) && pValue) {
            escape = pValue;
            escape.escapeXML();

            if(escape.length()) {
                output+= escape;
                output+="\"/>";
                ODe_writeToFile(m_pParagraphContent, output);
            }
        }
    }
}

/**
 * 
 */
void ODe_Text_Listener::closeBookmark(const PP_AttrProp* pAP) {
    UT_return_if_fail(pAP);

    UT_UTF8String output = "<text:bookmark-end text:name=\"", escape;
    const gchar* pValue = NULL;

    if(pAP->getAttribute("type",pValue) && pValue && (strcmp(pValue, "end") == 0)) {
        if(pAP->getAttribute("name",pValue) && pValue) {
            escape = pValue;
            escape.escapeXML();

            if(escape.length()) {
                output+= escape;
                output+="\"/>";
                ODe_writeToFile(m_pParagraphContent, output);
            }
        }
    }
}


/**
 * 
 */
void ODe_Text_Listener::closeBookmark(UT_UTF8String &sBookmarkName) {
    UT_return_if_fail(sBookmarkName.length());

    UT_UTF8String output = "<text:bookmark-end text:name=\"", escape;
    escape = sBookmarkName;
    escape.escapeXML();

    if(escape.length()) {
        output+= escape;
        output+="\"/>";
        ODe_writeToFile(m_pParagraphContent, output);
    }
}


/**
 * 
 */
void ODe_Text_Listener::openHyperlink(const PP_AttrProp* pAP) {
    UT_return_if_fail(pAP);

    UT_UTF8String output = "<text:a ", escape;
    const gchar* pValue = NULL;

    if(pAP->getAttribute("xlink:href",pValue) && pValue) {
        escape = pValue;
        escape.escapeURL();

        if(escape.length()) {
            output+="xlink:href=\"";
            output+= escape;
            output+="\">";
            ODe_writeToFile(m_pParagraphContent, output);
        }
    }
}

/**
 * 
 */
void ODe_Text_Listener::closeHyperlink() {
    UT_UTF8String output = "</text:a>";
    ODe_writeToFile(m_pParagraphContent, output);
}

/**
 * 
 */
void ODe_Text_Listener::insertText(const UT_UTF8String& rText) {
    ODe_writeToFile(m_pParagraphContent, rText);
    m_isFirstCharOnParagraph = false;
}


/**
 * 
 */
void ODe_Text_Listener::closeCell(ODe_ListenerAction& rAction) {
    _closeODParagraph();
    _closeODList(); // Close the current list, if there is one.
    rAction.popListenerImpl();
}


/**
 * 
 */
void ODe_Text_Listener::closeSection(ODe_ListenerAction& rAction) {
    _closeODParagraph();
    _closeODList(); // Close the current list, if there is one.
    rAction.popListenerImpl();
}


/**
 *
 */
void ODe_Text_Listener::insertLineBreak() {
    ODe_writeToFile(m_pParagraphContent, "<text:line-break/>");
}


/**
 *
 */
void ODe_Text_Listener::insertColumnBreak() {
    _closeODList();
    m_pendingColumnBrake = true;
}


/**
 *
 */
void ODe_Text_Listener::insertPageBreak() {
    _closeODList();
    m_pendingPageBrake = true;
}


/**
 *
 */
void ODe_Text_Listener::insertTabChar() {
    // We will not write the tab char that abi inserts right after each
    // list item bullet/number.
    if (!m_isFirstCharOnParagraph || m_currentListLevel == 0) {
        ODe_writeToFile(m_pParagraphContent, "<text:tab/>");
    }

    m_isFirstCharOnParagraph = false;
}


/**
 *
 */
void ODe_Text_Listener::insertInlinedImage(const gchar* pImageName,
                                                 const PP_AttrProp* pAP) {
    UT_UTF8String output;
    UT_UTF8String str;
    UT_UTF8String escape;
    ODe_Style_Style* pStyle;
    const gchar* pValue;
    bool ok;

    
    pStyle = new ODe_Style_Style();
    pStyle->setFamily("graphic");
    pStyle->setWrap("run-through");
    pStyle->setRunThrough("foreground");
    m_rAutomatiStyles.storeGraphicStyle(pStyle);    
    
    output = "<draw:frame text:anchor-type=\"as-char\"";

    UT_UTF8String_sprintf(str, "%u", m_zIndex);
    ODe_writeAttribute(output, "draw:z-index", str);
    ODe_writeAttribute(output, "draw:style-name", pStyle->getName());

    ok = pAP->getProperty("width", pValue);
    if (ok && pValue != NULL) {
        ODe_writeAttribute(output, "svg:width", pValue);
    }
    
    ok = pAP->getProperty("height", pValue);
    if (ok && pValue != NULL) {
        ODe_writeAttribute(output, "svg:height", pValue);
    }
    
    output += "><draw:image xlink:href=\"Pictures/";
    output += pImageName;
    output += ".png\" xlink:type=\"simple\" xlink:show=\"embed\""
              " xlink:actuate=\"onLoad\"/>";

    ok = pAP->getAttribute("alt", pValue);
    if (ok && pValue != NULL) {
       escape = pValue;
       escape.escapeXML();
       if(escape.length()) {
           output += "<svg:desc>";
           output += escape.utf8_str();
           output += "</svg:desc>";
       }
       escape.clear();
    }

    ok = pAP->getAttribute("title", pValue);
    if (ok && pValue != NULL) {
       escape = pValue;
       escape.escapeXML();
       if(escape.length()) {
           output += "<svg:title>";
           output += escape.utf8_str();
           output += "</svg:title>";
       }
    }

    output += "</draw:frame>";

    ODe_writeToFile(m_pParagraphContent, output);
}


void ODe_Text_Listener::insertPositionedImage(const gchar* pImageName,
                                                 const PP_AttrProp* pAP) {
    UT_UTF8String output = "<text:p>";
    UT_UTF8String str;
    UT_UTF8String escape;
    ODe_Style_Style* pStyle;
    const gchar* pValue;
    bool ok;
   
    pStyle = new ODe_Style_Style();
    pStyle->setFamily("graphic");

    //set wrapping
    ok = pAP->getProperty("wrap-mode", pValue);
    if(ok && pValue && !strcmp(pValue, "wrapped-to-right")) {
        pStyle->setWrap("right");
    }
    else if(ok && pValue && !strcmp(pValue, "wrapped-to-left")) {
        pStyle->setWrap("left");
    }
    else if(ok && pValue && !strcmp(pValue, "wrapped-both")) {
        pStyle->setWrap("parallel");
    }
    else { //this handles the above-text case and any other unforeseen ones
        pStyle->setWrap("run-through");
        pStyle->setRunThrough("foreground");
    }

    m_rAutomatiStyles.storeGraphicStyle(pStyle);    
    
    output += "<draw:frame text:anchor-type=\"";
    ok = pAP->getProperty("position-to", pValue);
    if(ok && pValue && !strcmp(pValue, "column-above-text")) {
        output+="page\""; //the spec doesn't seem to handle column anchoring
	// we work around it
	ok = pAP->getProperty("pref-page", pValue);
 	if(ok)
	{
	    UT_sint32 iPage = atoi(pValue)+1;
	    UT_UTF8String sPage;
	    UT_UTF8String_sprintf(sPage,"%d",iPage);
	    ODe_writeAttribute(output, "text:anchor-page-number", sPage.utf8_str());
	}
	else
	{
	    ODe_writeAttribute(output, "text:anchor-page-number", "1");
	}
	//
	// Get the most recent page style so we can do the arithmetic
	// Won't work for x in multi-columned docs
	//

	UT_DEBUGMSG(("InsertPosionedObject TextListener %x AutoStyle %x \n",this,&m_rAutomatiStyles));
	ODe_Style_PageLayout * pPageL = NULL;
	UT_uint32 numPStyles =  m_rAutomatiStyles.getSectionStylesCount();
	UT_UTF8String stylePName;
	UT_DEBUGMSG(("Number PageLayoutStyles %d \n",numPStyles));
	UT_UTF8String_sprintf(stylePName, "PLayout%d", numPStyles + 1);
	pPageL = m_rAutomatiStyles.getPageLayout(stylePName.utf8_str());
	if(pPageL == NULL)
	{
	    pPageL = m_rAutomatiStyles.getPageLayout("Standard");
	}
	UT_DEBUGMSG(("Got PageLayoutStyle %x \n",pPageL));
	double xPageL = 0.;
	double yPageL = 0.;
	
	ok = pAP->getProperty("frame-col-xpos", pValue);
	UT_ASSERT(ok && pValue != NULL);
	double xCol =  UT_convertToInches(pValue);
	const gchar* pSVal= NULL;
	if(pPageL)
	{
	    pSVal = pPageL->getPageMarginLeft();
	    xPageL = UT_convertToInches(pSVal);
	}
	double xTot = xPageL + xCol;
	pValue = UT_convertInchesToDimensionString(DIM_IN,xTot,"4");
	ODe_writeAttribute(output, "svg:x", pValue);
        
	ok = pAP->getProperty("frame-col-ypos", pValue);
	UT_ASSERT(ok && pValue != NULL);
	double yCol =  UT_convertToInches(pValue);
	if(pPageL)
	{
	    pSVal = pPageL->getPageMarginTop();
	    yPageL = UT_convertToInches(pSVal);
	    pSVal = pPageL->getPageMarginHeader();
	    yPageL += UT_convertToInches(pSVal);
	    UT_DEBUGMSG(("PageMarginTop %s Margin in %f8.4\n",pSVal,yPageL));
	}
	double yTot = yPageL + yCol;
	UT_DEBUGMSG(("Col %f8.4 Total in %f8.4\n",yCol,yTot));
	pValue = UT_convertInchesToDimensionString(DIM_IN,yTot,"4");
	ODe_writeAttribute(output, "svg:y", pValue);
    }
    else if(ok && pValue && !strcmp(pValue, "page-above-text")) {
        output+="page\"";
	ok = pAP->getProperty("frame-page-xpos", pValue);
	UT_ASSERT(ok && pValue != NULL);
        ODe_writeAttribute(output, "svg:x", pValue);
        
	ok = pAP->getProperty("frame-page-ypos", pValue);
	UT_ASSERT(ok && pValue != NULL);
        ODe_writeAttribute(output, "svg:y", pValue);
    }
    else { //this handles the block-above-text case and any other unforeseen ones
        output+="paragraph\"";
	ok = pAP->getProperty("xpos", pValue);
	UT_ASSERT(ok && pValue != NULL);
        ODe_writeAttribute(output, "svg:x", pValue);
        
	ok = pAP->getProperty("ypos", pValue);
	UT_ASSERT(ok && pValue != NULL);
        ODe_writeAttribute(output, "svg:y", pValue);
    }

    UT_UTF8String_sprintf(str, "%u", m_zIndex);
    ODe_writeAttribute(output, "draw:z-index", str);
    ODe_writeAttribute(output, "draw:style-name", pStyle->getName());

    ok = pAP->getProperty("frame-width", pValue);
    if (ok && pValue != NULL) {
        ODe_writeAttribute(output, "svg:width", pValue);
    }
    
    ok = pAP->getProperty("frame-height", pValue);
    if (ok && pValue != NULL) {
        ODe_writeAttribute(output, "svg:height", pValue);
    }
    
    output += "><draw:image xlink:href=\"Pictures/";
    output += pImageName;
    output += ".png\" xlink:type=\"simple\" xlink:show=\"embed\""
              " xlink:actuate=\"onLoad\"/>";

    ok = pAP->getAttribute("alt", pValue);
    if (ok && pValue != NULL) {
       escape = pValue;
       escape.escapeXML();
       if(escape.length()) {
           output += "<svg:desc>";
           output += escape.utf8_str();
           output += "</svg:desc>";
       }
       escape.clear();
    }

    ok = pAP->getAttribute("title", pValue);
    if (ok && pValue != NULL) {
       escape = pValue;
       escape.escapeXML();
       if(escape.length()) {
           output += "<svg:title>";
           output += escape.utf8_str();
           output += "</svg:title>";
       }
    }

    output += "</draw:frame></text:p>";
    
    ODe_writeToFile(m_pParagraphContent, output);
}


/**
 * Returns true if the properties belongs to a plain paragraph, false otherwise.
 * An AbiWord <p> tag (block) can be, for instance, a list item if it has
 * a "listid" and/or "level" attribute.
 */
bool ODe_Text_Listener::_blockIsPlainParagraph(const PP_AttrProp* pAP) const {
    const gchar* pValue;
    bool ok;
    
    ok = pAP->getAttribute("level", pValue);
    if (ok && pValue != NULL) {
        return false;
    }
    
    ok = pAP->getAttribute("listid", pValue);
    if (ok && pValue != NULL) {
        return false;
    }
    
    return true;
}


/**
 * Open a <text:list-item>, in some cases along with a  preceding <text:list>
 */
void ODe_Text_Listener::_openODListItem(const PP_AttrProp* pAP) {
    int level;
    const gchar* pValue;
    bool ok;
    UT_UTF8String output;

   
    ok = pAP->getAttribute("level", pValue);
    if (ok && pValue != NULL) {
        level = atoi(pValue);
    } else {
        level = 0; // The list will be completely closed.
    }
    

    // This list item may belong to a new list.
    // If so, we must close the current one (if there is a current one at all).
    if (level == 1 && m_currentListLevel > 0) {
        // OBS: An Abi list must start with a level 1 list item.
        
        const ODe_ListLevelStyle* pListLevelStyle;
        pListLevelStyle = m_pCurrentListStyle->getLevelStyle(1);
        
        ok = pAP->getAttribute("listid", pValue);
        UT_ASSERT_HARMLESS(ok && pValue!=NULL);
                
        if (pValue && pListLevelStyle && (strcmp(pListLevelStyle->getAbiListID().utf8_str(), pValue) != 0)) {
            // This list item belongs to a new list.
            _closeODList(); // Close the current list to start a new one later on.
        }
    }


    if (level > m_currentListLevel) {
        // Open a new sub-list


        output.clear();
        _printSpacesOffset(output);
        
        if(m_currentListLevel == 0) {
            // It's a "root" list.
            
            UT_ASSERT(m_pCurrentListStyle == NULL);
            
            m_pCurrentListStyle = m_rAutomatiStyles.addListStyle();
            
            output += "<text:list text:style-name=\"";
            output += m_pCurrentListStyle->getName();
            output += "\">\n";
            
        } else {
            // It's a sub (nested) list, it will inherit the style of its
            // parent (root).
            output += "<text:list>\n";
        }
        
        ODe_writeToFile(m_pTextOutput, output);
        
        m_spacesOffset++;
        
        // It's possibly a new list level style.
        // Update our list style with info regarding this level.
        m_pCurrentListStyle->setLevelStyle(level, *pAP);
        
        m_currentListLevel++;
        
    } else if (level < m_currentListLevel) {
        // Close lists until reach the desired list level.
        
        // Levels increase step-by-step but may, nevertheless, decrease
        // many at once.

        // Note that list items are never closed alone. They are aways closed
        // together with a list end tag (</text:list>).
        
        while (m_currentListLevel > level) {
            // Close the current item and its list
            
            output.clear();

            m_spacesOffset--;            
            _printSpacesOffset(output);
            output += "</text:list-item>\n";

            m_spacesOffset--;
            _printSpacesOffset(output);
            output += "</text:list>\n";
            
            ODe_writeToFile(m_pTextOutput, output);
            m_currentListLevel--;
        }
        
        
        if (m_currentListLevel > 0) {
            // And, finnaly, close the item that is hold that table hierarchy
            output.clear();
            m_spacesOffset--;
            _printSpacesOffset(output);
            output += "</text:list-item>\n";
            
            ODe_writeToFile(m_pTextOutput, output);
        }
        
    } else if (m_currentListLevel > 0) {
        // Same level, just close the current list item.
        output.clear();
        m_spacesOffset--;
        _printSpacesOffset(output);
        output += "</text:list-item>\n";
        
        ODe_writeToFile(m_pTextOutput, output);
    }
    
    if (m_currentListLevel  > 0) {
        // Yes, we are inside a list item (so let's create one).
        
        // Note that list items are never closed alone. they are aways closed
        // together with a list end tag (</text:list>).
    
        output.clear();
        _printSpacesOffset(output);
        output += "<text:list-item>\n";
    
        ODe_writeToFile(m_pTextOutput, output);
        
        m_spacesOffset++;
    } else {
        m_pCurrentListStyle = NULL;
    }
}


/**
 * 
 */
void ODe_Text_Listener::_openODParagraph(const PP_AttrProp* pAP) {
    UT_UTF8String styleName;
    UT_UTF8String output;
    UT_UTF8String str;
    UT_UTF8String escape;
    const gchar* pValue;
    bool ok;
    
    ////
    // Figure out the paragraph style
    
    if (ODe_Style_Style::hasParagraphStyleProps(pAP) ||
        ODe_Style_Style::hasTextStyleProps(pAP) ||
        m_pendingMasterPageStyleChange ||
        m_pendingColumnBrake ||
        m_pendingPageBrake) {
            
        // Need to create a new automatic style to hold those paragraph
        // properties.
        
        ODe_Style_Style* pStyle;
        pStyle = new ODe_Style_Style();
        pStyle->setFamily("paragraph");
        
        pStyle->fetchAttributesFromAbiBlock(pAP);
        
        if (m_pendingMasterPageStyleChange) {
            pStyle->setMasterPageName(m_masterPageStyleName);
            m_pendingMasterPageStyleChange = false;
            m_masterPageStyleName.clear();
        }
        
        // Can't have both breaks
        UT_ASSERT(
            !(m_pendingColumnBrake==true && m_pendingPageBrake==true) );
        
        if (m_pendingColumnBrake) {
            pStyle->setBreakBefore("column");
            m_pendingColumnBrake = false;
        }
        
        if (m_pendingPageBrake) {
            pStyle->setBreakBefore("page");
            m_pendingPageBrake = false;
        }
        
        m_rAutomatiStyles.storeParagraphStyle(pStyle);
        styleName = pStyle->getName();
        
    } else {
        ok = pAP->getAttribute("style", pValue);
        if (ok) {
            styleName = pValue;
        }
    }
    
    
    ////
    // Write the output string
    
    output.clear();
    _printSpacesOffset(output);
    
    if (styleName.empty()) {
        output += "<text:p>";
        m_isHeadingParagraph = false;
    } else {
        UT_uint8 outlineLevel;
        
        outlineLevel = m_rAuxiliaryData.m_headingStyles.
                            getHeadingOutlineLevel(styleName);
        
        if (outlineLevel > 0) {
            // It's a heading.
            
            UT_UTF8String_sprintf(str, "%u", outlineLevel);
            
            escape = styleName;
            output += "<text:h text:style-name=\"";
            output += escape.escapeXML();
            output += "\" text:outline-level=\"";
            output += str;
            output += "\">";
            
            m_isHeadingParagraph = true;
            
        } else {
            // It's a regular paragraph.
            escape = styleName;
            output += "<text:p text:style-name=\"";
            output += escape.escapeXML();
            output += "\">";
            
            m_isHeadingParagraph = false;
        }
    }
    
    ////
    // Write output string to file and update related variables.
    
    ODe_writeToFile(m_pTextOutput, output);
    m_openedODParagraph = true;
    m_isFirstCharOnParagraph = true;
    m_spacesOffset++;
    
    // The paragraph content will be stored in a separate temp file.
    // It's done that way because we may have to write a textbox (<draw:frame>)
    // inside this paragraph, before its text content, which, in AbiWord, comes
    // before the textbox.
    UT_ASSERT(m_pParagraphContent==NULL);
    m_pParagraphContent = gsf_output_memory_new();
}


/**
 * Close an OpenDocument list if there is one currently open.
 */
void ODe_Text_Listener::_closeODList() {
    if (m_currentListLevel == 0) {
        // There is nothing to be done
        return;
    }
    
    UT_uint8 i;
    UT_UTF8String output;
    
    for (i=m_currentListLevel; i>0; i--) {
        output.clear();
        
        m_spacesOffset--;
        _printSpacesOffset(output);
        output += "</text:list-item>\n";
        
        m_spacesOffset--;
        _printSpacesOffset(output);
        output += "</text:list>\n";
	
        ODe_writeToFile(m_pTextOutput, output);
    }
    
    m_currentListLevel = 0;
    m_pCurrentListStyle = NULL;
}


/**
 * 
 */
void ODe_Text_Listener::_closeODParagraph() {

    if (m_openedODParagraph) {         
        gsf_output_write(m_pTextOutput, gsf_output_size(m_pParagraphContent),
			 gsf_output_memory_get_bytes(GSF_OUTPUT_MEMORY(m_pParagraphContent)));

        ODe_gsf_output_close(m_pParagraphContent);
        m_pParagraphContent = NULL;
    
        m_openedODParagraph = false;
        m_spacesOffset--;
    }
}
