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
#include "ODe_Note_Listener.h"

// Internal includes
#include "ODe_AuxiliaryData.h"
#include "ODe_Common.h"
#include "ODe_ListenerAction.h"
#include "ODe_Text_Listener.h"

// AbiWord includes
#include <pp_AttrProp.h>


/**
 * Constructor
 */
ODe_Note_Listener::ODe_Note_Listener(ODe_AutomaticStyles& rAutomatiStyles,
                                     GsfOutput* pTextOutput,
                                     ODe_AuxiliaryData& rAuxiliaryData,
                                     UT_uint8 spacesOffset)
                                     :
                                     ODe_AbiDocListenerImpl(spacesOffset),
                                     m_rAutomatiStyles(rAutomatiStyles),
                                     m_pTextOutput(pTextOutput),
                                     m_rAuxiliaryData(rAuxiliaryData)
{
}


/**
 * 
 */
void ODe_Note_Listener::openFootnote(const PP_AttrProp* pAP,
                                     ODe_ListenerAction& rAction) {
    bool ok;
    const gchar* pValue = NULL;
    UT_UTF8String str;
    
    ok = pAP->getAttribute("footnote-id", pValue);

    if (ok && pValue)
        _openNote("footnote", pValue, rAction);
}


/**
 * 
 */
void ODe_Note_Listener::closeFootnote(ODe_ListenerAction& rAction) {
    _closeNote(rAction);
}


/**
 * 
 */
void ODe_Note_Listener::openEndnote(const PP_AttrProp* pAP,
                                    ODe_ListenerAction& rAction) {
    bool ok;
    const gchar* pValue = NULL;
    UT_UTF8String str;
    
    ok = pAP->getAttribute("endnote-id", pValue);

    if (ok && pValue)
        _openNote("endnote", pValue, rAction);
}


/**
 * 
 */
void ODe_Note_Listener::closeEndnote(ODe_ListenerAction& rAction) {
    _closeNote(rAction);
}


/**
 * 
 */
void ODe_Note_Listener::openBlock(const PP_AttrProp* /*pAP*/,
                                  ODe_ListenerAction& rAction) {
    ODe_Text_Listener* pTextListener;
    pTextListener = new ODe_Text_Listener(m_rAutomatiStyles,
                                          m_pTextOutput,
                                          m_rAuxiliaryData,
                                          0,
                                          m_spacesOffset);
    rAction.pushListenerImpl(pTextListener, true);
}


/**
 * 
 */
void ODe_Note_Listener::_openNote(const gchar* pNoteClass,
                                  const gchar* pNoteId,
                                  ODe_ListenerAction& /*rAction*/) {
    UT_uint32 noteCitation;
    UT_UTF8String str;
    UT_UTF8String output;

    UT_return_if_fail(pNoteId);
    
    // The note citation will be id+1
    // So id=0 will have a citation "1", and so on.
    noteCitation = atoi(pNoteId) + 1;
    
    output += "<text:note text:id=\"note";
    
    UT_UTF8String_sprintf(str, "%u", m_rAuxiliaryData.m_noteCount+1);
    output += str;
    
    output += "\" text:note-class=\"";
    output += pNoteClass;
    output += "\"><text:note-citation>";
    
    UT_UTF8String_sprintf(str, "%u", noteCitation);
    output += str;
    
    output += "</text:note-citation><text:note-body>";                                    
    
    ODe_writeToFile(m_pTextOutput, output);
    
    m_rAuxiliaryData.m_noteCount++;
}


/**
 * 
 */
void ODe_Note_Listener::_closeNote(ODe_ListenerAction& rAction) {
    ODe_writeToFile(m_pTextOutput, "</text:note-body></text:note>");
    rAction.popListenerImpl();
}
