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

#ifndef ODE_HEADINGSEARCHER_LISTENER_H_
#define ODE_HEADINGSEARCHER_LISTENER_H_

// Internal includes
#include "ODe_AbiDocListenerImpl.h"

// Internal classes
class ODe_AuxiliaryData;

// AbiWord classes
class PP_AttrProp;


/**
 * Searches all TOCs for its heading styles, i.e.: the paragraph styles that are
 * used to build the document structure (chapters, sections, etc).
 */
class ODe_HeadingSearcher_Listener: public ODe_AbiDocListenerImpl {
public:
    ODe_HeadingSearcher_Listener(ODe_AuxiliaryData& rAuxiliaryData);
    
    virtual void openTOC(const PP_AttrProp* pAP);
    
private:
    ODe_AuxiliaryData& m_rAuxiliaryData;
};

#endif /*ODE_HEADINGSEARCHER_LISTENER_H_*/
