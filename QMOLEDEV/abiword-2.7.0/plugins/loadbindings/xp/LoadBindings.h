/*
 * LoadBindings - Simple parser to read keybindings for AbiWord
 * Copyright (C) 2007 by Martin Sevior
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

#ifndef PLUGIN_LOADBINDINGS_H
#define PLUGIN_LOADBINGINGS_H
#include "ut_string_class.h"
#include "ev_EditEventMapper.h"
#include "ev_EditEventMapper.h"
#include "ev_EditBinding.h"

class AV_View;
class FL_DocLayout;
class XAP_App;
class EV_EditMethodCallData;

enum {
	DONT_UNBIND_MOUSECONTEXTS	= 0x01,
	DONT_UNBIND_KEYSTROKES		= 0x02,
		
	DONT_UNBIND_ANYTHING		= 0x03
};

enum _FROM_URI { FROM_URI };
enum _FROM_MEMORY { FROM_MEMORY };

typedef std::map<std::string,UT_uint8> UnbindMap;
typedef std::map<UT_uint32,std::string> BindingMap;

class LoadBindings
{
	public:
		LoadBindings(EV_EditMethodCallData * d, _FROM_URI);
		LoadBindings(EV_EditMethodCallData * d, _FROM_MEMORY);
		~LoadBindings();
		bool Load();
		bool Set() const;
		const std::string& GetName() const { return m_sName; }
		
	protected:
		XAP_App*			m_pApp;
		xmlDocPtr 			m_pXMLDoc;
		std::string			m_sName;
		bool				m_bReplace;
		BindingMap			m_BindMap;
		UnbindMap			m_UnbindMap;
		
	
		int strcmp( const char* s1, const char* s2 ) {			return ::strcmp( s1, s2 ); }
		int strcmp( const char* s1, const xmlChar* s2 ) { 		return ::strcmp( s1, reinterpret_cast<const char*>(s2) ); }
		int strcmp( const xmlChar* s1, const xmlChar* s2 ) {	return ::strcmp( reinterpret_cast<const char*>(s1), reinterpret_cast<const char*>(s2) ); }
		int strcmp( const xmlChar* s1, const char* s2 ) { 		return ::strcmp( reinterpret_cast<const char*>(s1), s2 ); }
	
		/** Parses the control, shift and alt attribute's (if present)
	 	 *	and converts them to a EV_EditModifierState value.
		 */
		EV_EditModifierState GetModifiers( xmlNodePtr node );
	
		/** Searches the specified node's attributes for a certain
	 	 *	named attribute and returns its value, if present.
		 */
		const char* FindAttribute( xmlNodePtr node, const char* name );
	
		/** Adds a new mapping to the internal binding map */
		bool AddMapping( UT_uint32 binding, const char* command );
	
		/** Adds a new mapping removal */
		bool RemoveMapping( const char* command, UT_uint8 unbinding );
	
		/** Reports an error. */
		void ReportError( const char* format, ... ) const;
	
		/** Reports a warning. */
		void ReportWarning( const char* format, ... ) const;
};

class EV_NamedVirtualKey
{
public:
	static const char *	getName(EV_EditBits eb);
	static EV_EditBits	getEB(const char * szName);
};

#endif /* PLUGIN_LOADBINDINGS_H */
