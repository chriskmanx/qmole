/*
 * AiksaurusApp - A Win32 interface to the AikSaurus library
 * Copyright (C) 2001 by Jared Davis, Michael D. Pritchett
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

#ifndef AIKAPP
#define AIKAPP

#ifdef _MSC_VER	
#pragma warning(disable: 4786)
#endif

#include <windows.h>
#include <string>
using namespace std;

class AiksaurusApp
{
public:
	AiksaurusApp();
	virtual ~AiksaurusApp();

	void setTitle( char * title );
	void setInitialMessage( char * msg );
	const char* runThesaurus( char * word );

	void initialize();
	HINSTANCE getInstance() { return m_hInstance; }
	void setInstance(HINSTANCE hI) { m_hInstance = hI; }
	void setLookup( char * word );

private:
	HINSTANCE m_hInstance;
	string lookup;
	string replace;
};

#endif
