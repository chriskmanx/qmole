/*
 * AiksaurusDlg - A Win32 interface to the AikSaurus library
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


#ifndef AIKDLG
#define AIKDLG

#ifdef _MSC_VER	
#pragma warning(disable: 4786)
#endif

#include <windows.h>
#include <commctrl.h>
#include <string>
#include <list>
#include <vector>
#include "Aiksaurus.h"
using namespace std;

#define MAX_WORD_LENGTH 50

class AiksaurusApp;

class AiksaurusDlg
{
public:
	AiksaurusDlg();
	virtual ~AiksaurusDlg();
	void runModal(AiksaurusApp * pApp);
	void setSearch(string word);
	string getReplacement();

	static BOOL CALLBACK s_dlgProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

	BOOL _onInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL _onCommand(HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL _onSetCursor(HWND hWnd, WPARAM wParam, LPARAM lParam);
	BOOL _onDeltaPos(NM_UPDOWN * pnmud);
	HWND m_hDlg;

private:
	Aiksaurus thesaurus;
	HINSTANCE m_hInstance;
	HWND      m_hSearch;
	HWND      m_hBack;
	HWND      m_hForward;
	HWND      m_hMeanings;
	HWND      m_hSynonyms;
	HWND      m_hCombo;
	HWND	  m_hReplace;
	HWND      m_hSynText;
	HICON     m_hOrigIcon;

	list<string> listBack;
	list<string> listForward;
	vector< list<string> > synonyms;

	string m_szLookupWord;
	string m_szReplacementWord;
	bool m_bSearchBtnChanged;
	bool m_bBackBtnChanged;
	bool m_bForwardBtnChanged;
	bool m_bWordFound;

	void _setNormalButtons();
	void _doSearch();
	void _onBack();
	void _onForward();
	void _addComboString( string str );
	void _clearListBoxes();
	void _clearSynonyms();
	void _clearMeanings();
	void _showAlternatives();
	void _showMeanings();
	void _showSynonyms( int index );
	void _copyToClipboard();
};

#endif
