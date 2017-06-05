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

#ifndef ODE_LISTLEVELSTYLE_H_
#define ODE_LISTLEVELSTYLE_H_

// External includes
#include <gsf/gsf-output.h>

// AbiWord includes
#include <ut_string_class.h>

// AbiWord classes
class PP_AttrProp;


/**
 * Abstract class.
 * Represents a <text:list-level-style-*> element
 */
class ODe_ListLevelStyle {
public:
	virtual ~ODe_ListLevelStyle()
		{
		}
    virtual void fetchAttributesFromAbiBlock(const PP_AttrProp& rAP);

    // Write this <text:list-style> element.
    virtual bool write(GsfOutput* pODT,
                       const UT_UTF8String& rSpacesOffset) const = 0;
    
    const UT_UTF8String& getAbiListID() const {return m_AbiListId;}

    const UT_UTF8String& getFontName() const {return m_fontName;}

protected:

    void _writeTextProperties(GsfOutput* pODT,
                         const UT_UTF8String& rSpacesOffset) const;
                         
    void _writeListLevelProperties(GsfOutput* pODT,
                              const UT_UTF8String& rSpacesOffset) const;

    // listid attribute fom AbiWord <p> tag.
    UT_UTF8String m_AbiListId;
    
    UT_UTF8String m_level; // text:level
    
    ////
    // <style:text-properties> sub element
    UT_UTF8String m_fontName; // style:font-name
    
    ////
    // <style:list-level-properties> sub element
    UT_UTF8String m_minLabelWidth; // text:min-label-width
    UT_UTF8String m_spaceBefore;   // text:space-before
};


/**
 * Represents a <text:list-level-style-bullet> element.
 */
class ODe_Bullet_ListLevelStyle : public ODe_ListLevelStyle {
public:
	virtual ~ODe_Bullet_ListLevelStyle()
		{}

    virtual void fetchAttributesFromAbiBlock(const PP_AttrProp& rAP);

    virtual bool write(GsfOutput* pODT,
                       const UT_UTF8String& rSpacesOffset) const;

private:
    UT_UTF8String m_bulletChar; // text:bullet-char
};


/**
 * Represents a <text:list-level-style-number> element.
 */
class ODe_Numbered_ListLevelStyle : public ODe_ListLevelStyle {
public:
	virtual ~ODe_Numbered_ListLevelStyle()
		{}
    virtual void fetchAttributesFromAbiBlock(const PP_AttrProp& rAP);

    virtual bool write(GsfOutput* pODT,
                       const UT_UTF8String& rSpacesOffset) const;
private:
    UT_UTF8String m_startValue;    // text:start-value
    UT_UTF8String m_numFormat;     // style:num-format
    UT_UTF8String m_displayLevels; // text:display-levels
};

#endif /*ODE_LISTLEVELSTYLE_H_*/
