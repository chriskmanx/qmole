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

#ifndef ODE_LISTENERACTION_H_
#define ODE_LISTENERACTION_H_

// AbiWord includes
#include <ut_types.h>

// Internal classes
class ODe_AbiDocListenerImpl;


/**
 * Stores the action that the current ODe_AbiDocListenerImpl issues to
 * ODe_AbiDocListener.
 */
class ODe_ListenerAction {
public:

    enum {
      ACTION_NONE       = 0,
      ACTION_PUSH       = 1,
      ACTION_POP        = 2
    };

    void pushListenerImpl(ODe_AbiDocListenerImpl* pListenerImpl, bool _deleteWhenPop) {
        m_action = ACTION_PUSH;
        m_pListenerImpl = pListenerImpl;
        m_deleteWhenPop = _deleteWhenPop;
    }
    
    void popListenerImpl() {
        m_action = ACTION_POP;
    }
    
    UT_uint8 getAction() const {
        return m_action;
    }

    ODe_AbiDocListenerImpl* getListenerImpl() const {
        return m_pListenerImpl;
    }
    
    bool deleteWhenPop() const {return m_deleteWhenPop;}

    void reset() {
        m_action = ACTION_NONE;
        m_pListenerImpl = NULL;
    }
    
private:
    UT_uint8 m_action;
    ODe_AbiDocListenerImpl* m_pListenerImpl;
    bool m_deleteWhenPop;
};

#endif /*ODE_LISTENERACTION_H_*/
