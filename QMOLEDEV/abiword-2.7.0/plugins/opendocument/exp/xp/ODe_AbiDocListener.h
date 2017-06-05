/* AbiSource
 * 
 * Copyright (C) 2002 Dom Lachowicz <cinamod@hotmail.com>
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


#ifndef _ODE_ABIDOCLISTENER_H_
#define _ODE_ABIDOCLISTENER_H_

// Internal includes
#include "ODe_ListenerAction.h"

// Abiword includes
#include <pl_Listener.h>
#include <ut_vector.h>
#include <ut_string_class.h>

// Internal classes
class ODe_AbiDocListenerImpl;
class ODe_DocumentData;

// AbiWord classes
class PD_Document;
class fd_Field;
class PX_ChangeRecord_Object;

/**
 * Handles the mess involved in PL_Listener event handling translating then
 * into simple and clean calls to ODe_AbiDocListenerImpl.
 */
class ODe_AbiDocListener : public PL_Listener
{
public:

    ODe_AbiDocListener(PD_Document* pDocument,
                       ODe_AbiDocListenerImpl* pListenerImpl,
                       bool deleteWhenPop);
                       
    virtual ~ODe_AbiDocListener();

    virtual bool populate(PL_StruxFmtHandle sfh, const PX_ChangeRecord * pcr);
    
    virtual bool populateStrux(PL_StruxDocHandle sdh,
        const PX_ChangeRecord * pcr, PL_StruxFmtHandle * psfh);
        
    virtual bool change(PL_StruxFmtHandle sfh, const PX_ChangeRecord * pcr);
    
    virtual bool insertStrux(PL_StruxFmtHandle sfh,
                const PX_ChangeRecord * pcr,
                PL_StruxDocHandle sdh,
                PL_ListenerId lid,
                void (* pfnBindHandles)(PL_StruxDocHandle sdhNew,
                            PL_ListenerId lid,
                            PL_StruxFmtHandle sfhNew));
                            
    virtual bool signal(UT_uint32 iSignal);

    void finished();

private:

    void _openSpan(PT_AttrPropIndex api);
    void _closeSpan();
    
    void _openBlock(PT_AttrPropIndex api);
    void _closeBlock();
    
    void _openSection(PT_AttrPropIndex api, bool recursiveCall = false);
    void _closeSection(bool recursiveCall = false);
    
    void _openField(const PX_ChangeRecord_Object* pcro, PT_AttrPropIndex api);
    void _closeField();
    
    void _openTable(PT_AttrPropIndex api, bool recursiveCall = false);
    void _closeTable(bool recursiveCall = false);
    
    void _openCell(PT_AttrPropIndex api, bool recursiveCall = false);
    void _closeCell(bool recursiveCall = false);
    
    void _openFootnote(PT_AttrPropIndex api);
    void _closeFootnote();
    
    void _openEndnote(PT_AttrPropIndex api);
    void _closeEndnote();

    void _openAnnotation(PT_AttrPropIndex api);
    void _closeAnnotation();
    
    void _openFrame(PT_AttrPropIndex api);
    void _closeFrame();
    
    void _openTOC(PT_AttrPropIndex api);
    void _closeTOC();

    void _openBookmark(PT_AttrPropIndex api);
    void _closeBookmark(PT_AttrPropIndex api);
    void _closeBookmark(UT_UTF8String &sBookmarkName);
    
    void _openHyperlink(PT_AttrPropIndex api);
    void _closeHyperlink();
    
    void _insertInlinedImage(PT_AttrPropIndex api);

    void _insertEmbeddedImage(PT_AttrPropIndex api);
    
    void _insertMath(PT_AttrPropIndex api);

    void _outputData(const UT_UCSChar* pData, UT_uint32 length);
    void _appendSpaces(UT_UTF8String* sBuf, UT_uint32 count);

    const gchar* _getObjectKey(const PT_AttrPropIndex& api,
                                  const gchar* key);
                                  
    void _handleListenerImplAction();
    
    ////
    // Abi processing vars

    fd_Field* m_pCurrentField;
    UT_UTF8String m_currentFieldType;
    UT_UTF8String m_bookmarkName;
    PT_AttrPropIndex    m_apiLastSpan;
    
    bool m_bInSpan;
    bool m_bInBlock;    
    bool m_bInBookmark;
    bool m_bInHyperlink;
    bool m_bInSection;
    bool m_bInAnnotation;
    
    UT_sint32 m_iInTable;
    UT_sint32 m_iInCell;
    
    PD_Document* m_pDocument;

    ////
    // Listener implementation vars
    
    class StackCell {
    public:
        StackCell() {m_pListenerImpl=NULL; m_deleteWhenPop=false;}
        StackCell(ODe_AbiDocListenerImpl* pListenerImpl, bool deleteWhenPop) {
            m_deleteWhenPop = deleteWhenPop;
            m_pListenerImpl = pListenerImpl;
        }
        // Work around the "return 0" issue of the UT_GenericVector::getNhItem()
        StackCell(UT_uint32 /*i*/) 
			{
				m_pListenerImpl=NULL; 
				m_deleteWhenPop=false;
			}
        
        StackCell& operator=(const StackCell& sc) {
            this->m_deleteWhenPop = sc.m_deleteWhenPop;
            this->m_pListenerImpl = sc.m_pListenerImpl;
            
            return *this;
        }
        
        bool m_deleteWhenPop;
        ODe_AbiDocListenerImpl* m_pListenerImpl;
    };

    UT_GenericVector <ODe_AbiDocListener::StackCell> m_implStack;    
    
    ODe_AbiDocListenerImpl* m_pCurrentImpl;
    bool m_deleteCurrentWhenPop;
    
    ODe_ListenerAction m_listenerImplAction;
};

#endif //_ODE_ABIDOCLISTENER_H_
