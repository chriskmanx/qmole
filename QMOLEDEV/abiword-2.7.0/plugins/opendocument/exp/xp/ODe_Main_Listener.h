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

#ifndef _ODE_MAIN_LISTENER_H_
#define _ODE_MAIN_LISTENER_H_

// Internal includes
#include "ODe_AutomaticStyles.h"
#include "ODe_Styles.h"
#include "ODe_AbiDocListenerImpl.h"
#include "ODe_FontFaceDecls.h"

// Internal classes
class ODe_AuxiliaryData;
class ODe_ListenerAction;
class ODe_DocumentData;

// External includes
#include <stdio.h>

// AbiWord classes
class PD_Document;
class PP_AttrProp;


/**
 * Responsible for gathering all necessary info to write 
 * <office:document-styles> (styles.xml) and 
 * <office:document-content> (content.xml).
 */
class ODe_Main_Listener : public ODe_AbiDocListenerImpl {
   
public:

	ODe_Main_Listener(ODe_DocumentData& rDocumentData,
                      ODe_AuxiliaryData& rAuxiliaryData);
	virtual ~ODe_Main_Listener();

    // Listener methods
    
    virtual void openSection(const PP_AttrProp* pAP, ODe_ListenerAction& rAction);
    virtual void closeSection(ODe_ListenerAction& rAction);
    
private:

    bool _isHeaderFooterSection(const PP_AttrProp* pAP) const;
    
    void _openHeaderFooterSection(const PP_AttrProp* pAP,
                                  ODe_ListenerAction& rAction);

    ODe_DocumentData& m_rDocumentData;
    ODe_AuxiliaryData& m_rAuxiliaryData;

    bool m_onHeaderFooterSection;
    bool m_openedODSection;

    bool m_isFirstSection;
};

#endif //_ODE_MAIN_LISTENER_H_
