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


#ifndef ODE_NOTE_LISTENER_H_
#define ODE_NOTE_LISTENER_H_

// Internal includes
#include "ODe_AbiDocListenerImpl.h"

// Abiword includes
#include <ut_types.h>

// External includes
#include <stdio.h>

// Internal classes
class ODe_AutomaticStyles;
class ODe_AuxiliaryData;

// AbiWord classes
class PP_AttrProp;


/**
 * Writes footnotes and endnotes.
 */
class ODe_Note_Listener : public ODe_AbiDocListenerImpl {
public:

    ODe_Note_Listener(ODe_AutomaticStyles& rAutomatiStyles,
                      GsfOutput* pTextOutput,
                      ODe_AuxiliaryData& rAuxiliaryData,
                      UT_uint8 spacesOffset);


    virtual void openFootnote(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
    virtual void closeFootnote(ODe_ListenerAction& rAction);
    
    virtual void openEndnote(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
    virtual void closeEndnote(ODe_ListenerAction& rAction);

    virtual void openBlock(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
  
private:
    void _openNote(const gchar* pNoteClass, const gchar* pNoteId,
                   ODe_ListenerAction& rAction);
    void _closeNote(ODe_ListenerAction& rAction);

    ODe_AutomaticStyles& m_rAutomatiStyles;
    GsfOutput* m_pTextOutput;
    ODe_AuxiliaryData& m_rAuxiliaryData;

};

#endif /*ODE_NOTE_LISTENER_H_*/
