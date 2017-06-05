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


#ifndef _ODE_ABIDOCLISTENERIMPL_H_
#define _ODE_ABIDOCLISTENERIMPL_H_

// Abiword includes
#include <ut_types.h>
#include <fd_Field.h>

// Internal classes
class ODe_ListenerAction;

// AbiWord classes
class PP_AttrProp;
class UT_UTF8String;

/**
 * 
 */
class ODe_AbiDocListenerImpl {
public:
    ODe_AbiDocListenerImpl() {m_spacesOffset = 0;}
    ODe_AbiDocListenerImpl(UT_uint8 spacesOffset) : m_spacesOffset(spacesOffset) {}
    
    virtual ~ODe_AbiDocListenerImpl() {}
    
    virtual void insertText(const UT_UTF8String& /*rText*/) {}
    
    virtual void insertLineBreak() {}
    virtual void insertColumnBreak() {}
    virtual void insertPageBreak() {}
    virtual void insertTabChar() {}
    
    virtual void openSpan(const PP_AttrProp* /*pAP*/) {}
    virtual void closeSpan() {}
    
    virtual void openBlock(const PP_AttrProp* /*pAP*/, ODe_ListenerAction& /*rAction*/) {}
    virtual void closeBlock() {}
    
    virtual void openSection(const PP_AttrProp* /*pAP*/, ODe_ListenerAction& /*rAction*/) {}
    virtual void closeSection(ODe_ListenerAction& /*rAction*/) {}
    
    virtual void openField(const fd_Field* /*field*/, const UT_UTF8String& /*fieldType*/, const UT_UTF8String& /*fieldValue*/) {}
    virtual void closeField(const UT_UTF8String& /*fieldType*/) {}
    
    virtual void openTable(const PP_AttrProp* /*pAP*/, ODe_ListenerAction& /*rAction*/) {}
    virtual void closeTable(ODe_ListenerAction& /*rAction*/) {}
    
    virtual void openCell(const PP_AttrProp* /*pAP*/, ODe_ListenerAction& /*rAction*/) {}
    virtual void closeCell(ODe_ListenerAction& /*rAction*/) {}
    
    virtual void openFootnote(const PP_AttrProp* /*pAP*/, ODe_ListenerAction& /*rAction*/) {}
    virtual void closeFootnote(ODe_ListenerAction& /*rAction*/) {}
    
    virtual void openEndnote(const PP_AttrProp* /*pAP*/, ODe_ListenerAction& /*rAction*/) {}
    virtual void closeEndnote(ODe_ListenerAction& /*rAction*/) {}

    virtual void openAnnotation(const PP_AttrProp* /*pAP*/) {}
    virtual void closeAnnotation() {}
    
    virtual void openFrame(const PP_AttrProp* /*pAP*/, ODe_ListenerAction& /*rAction*/) {}
    virtual void closeFrame(ODe_ListenerAction& /*rAction*/) {}
    
    virtual void openTOC(const PP_AttrProp* /*pAP*/) {}
    virtual void closeTOC() {}

    virtual void openBookmark(const PP_AttrProp* /*pAP*/) {}
    virtual void closeBookmark(const PP_AttrProp* /*pAP*/) {}
    virtual void closeBookmark(UT_UTF8String & /*sBookmarkName*/) {}
    
    virtual void openHyperlink(const PP_AttrProp* /*pAP*/) {}
    virtual void closeHyperlink() {}
    
    virtual void insertInlinedImage(const gchar* /*pImageName*/,
                                    const PP_AttrProp* /*pAP*/) {}
                                    
protected:

    void _printSpacesOffset(UT_UTF8String& rOutput);
    
    UT_uint8 m_spacesOffset;
};

#endif // _ODE_ABIDOCLISTENERIMPL_H_
