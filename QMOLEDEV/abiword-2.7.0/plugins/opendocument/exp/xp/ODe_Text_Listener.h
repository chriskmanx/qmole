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

#ifndef ODE_TEXT_LISTENER_H_
#define ODE_TEXT_LISTENER_H_

// Internal includes
#include "ODe_AbiDocListenerImpl.h"

// Abiword includes
#include <ut_string_class.h>

// External includes
#include <stdio.h>

// Internal classes
class ODe_AutomaticStyles;
class ODe_AuxiliaryData;
class ODe_ListenerAction;
class ODe_Style_List;

// AbiWord classes
class PP_AttrProp;


/**
 * Writes basic stuff like paragraphs, inlined images, etc.
 */
class ODe_Text_Listener : public ODe_AbiDocListenerImpl {
public:

    ODe_Text_Listener(ODe_AutomaticStyles& rAutomatiStyles,
                      GsfOutput* pTextOutput,
                      ODe_AuxiliaryData& rAuxiliaryData,
                      UT_uint8 zIndex,
                      UT_uint8 spacesOffset);

    ODe_Text_Listener(ODe_AutomaticStyles& rAutomatiStyles,
                      GsfOutput* pTextOutput,
                      ODe_AuxiliaryData& rAuxiliaryData,
                      UT_uint8 zIndex,
                      UT_uint8 spacesOffset,
                      const UT_UTF8String& rPendingMasterPageStyleName);
                      
    ~ODe_Text_Listener();

    // Listener methods

    virtual void openTable(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);

    virtual void closeCell(ODe_ListenerAction& rAction);
    virtual void closeSection(ODe_ListenerAction& rAction);

    virtual void openBlock(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
    virtual void closeBlock();

    virtual void openSpan(const PP_AttrProp* pAP);
    virtual void closeSpan();
    
    virtual void openFrame(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
    virtual void closeFrame(ODe_ListenerAction& rAction);

    virtual void openField(const fd_Field* field, const UT_UTF8String& fieldType, const UT_UTF8String& fieldValue);
    virtual void closeField(const UT_UTF8String& fieldType);
    
    virtual void openFootnote(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
    virtual void closeFootnote(ODe_ListenerAction& rAction);
    
    virtual void openEndnote(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
    virtual void closeEndnote(ODe_ListenerAction& rAction);

    virtual void openAnnotation(const PP_AttrProp* pAP);
    virtual void closeAnnotation();

    virtual void openTOC(const PP_AttrProp* pAP);
    virtual void closeTOC();

    virtual void openBookmark(const PP_AttrProp* pAP);
    virtual void closeBookmark(const PP_AttrProp* pAP);
    virtual void closeBookmark(UT_UTF8String &sBookmarkName);

    virtual void openHyperlink(const PP_AttrProp* pAP);
    virtual void closeHyperlink();

    virtual void insertText(const UT_UTF8String& rText);

    virtual void insertLineBreak();
    virtual void insertColumnBreak();
    virtual void insertPageBreak();
    virtual void insertTabChar();

    virtual void insertInlinedImage(const gchar* pImageName,
                                    const PP_AttrProp* pAP);

    virtual void insertPositionedImage(const gchar* pImageName,
                                    const PP_AttrProp* pAP);

private:
    bool _blockIsPlainParagraph(const PP_AttrProp* pAP) const;
    void _openODListItem(const PP_AttrProp* pAP);
    void _openODParagraph(const PP_AttrProp* pAP);
    void _closeODParagraph();
    void _closeODList();

    
    bool m_openedODParagraph;
    bool m_openedODSpan;
    bool m_isFirstCharOnParagraph;
    bool m_isHeadingParagraph;
    bool m_openedODTextboxFrame;
    bool m_openedODNote;

    // Content of the current paragraph.
    GsfOutput* m_pParagraphContent;
    
    // The number of currently nested <text:list> tags
    // (meaning the current list level).
    UT_uint8 m_currentListLevel;
    ODe_Style_List* m_pCurrentListStyle;
    
    bool m_pendingColumnBrake;
    bool m_pendingPageBrake;
    
    bool m_pendingMasterPageStyleChange;
    UT_UTF8String m_masterPageStyleName;
    
    ODe_AutomaticStyles& m_rAutomatiStyles;
    GsfOutput* m_pTextOutput;
    ODe_AuxiliaryData& m_rAuxiliaryData;
    UT_uint8 m_zIndex;
};

#endif /*ODE_TEXT_LISTENER_H_*/
