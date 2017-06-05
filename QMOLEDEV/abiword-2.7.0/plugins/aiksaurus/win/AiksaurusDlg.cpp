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

#include "AiksaurusApp.h"
#include "AiksaurusDlg.h"
#include "resource.h"

// This is a static function.
BOOL CALLBACK AiksaurusDlg::s_dlgProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	AiksaurusDlg * pThis;
	
	switch (msg)
	{
	case WM_INITDIALOG:
		pThis = (AiksaurusDlg *)lParam;
		pThis->m_hDlg = hWnd;
		//_assertValidDlgHandle(hWnd);
		SetWindowLong(hWnd,DWL_USER,lParam);
		return pThis->_onInitDialog(hWnd,wParam,lParam);

	case WM_SETCURSOR:
		pThis = (AiksaurusDlg *)GetWindowLong(hWnd,DWL_USER);
		if(pThis)
			return pThis->_onSetCursor(hWnd,wParam,lParam);
		else
			return 0;

		
	case WM_COMMAND:
		pThis = (AiksaurusDlg *)GetWindowLong(hWnd,DWL_USER);
		if(pThis)
			return pThis->_onCommand(hWnd,wParam,lParam);
		else
			return 0;

	case WM_NOTIFY:
		pThis = (AiksaurusDlg *)GetWindowLong(hWnd,DWL_USER);
		switch (((LPNMHDR)lParam)->code)
		{
			case UDN_DELTAPOS:		return pThis->_onDeltaPos((NM_UPDOWN *)lParam);
			default:				return 0;
		}

	default:
		return 0;
	}
}

AiksaurusDlg::AiksaurusDlg() : m_hDlg(NULL)
{
	m_hSearch      = NULL;
	m_hBack        = NULL;
	m_hForward     = NULL;
	m_hMeanings    = NULL;
	m_hSynonyms    = NULL;
	m_hCombo       = NULL;
	m_hReplace     = NULL;
	m_hSynText     = NULL;

	m_bSearchBtnChanged  = true;
	m_bBackBtnChanged    = true;
	m_bForwardBtnChanged = true;
	m_bWordFound	     = false;
}

AiksaurusDlg::~AiksaurusDlg()
{
}

void AiksaurusDlg::runModal(AiksaurusApp * pApp)
{
	LPCTSTR lpTemplate = MAKEINTRESOURCE(ID_DIALOG_AIKSAURUS);
	m_hInstance = pApp->getInstance();

	int result = DialogBoxParam( m_hInstance, 
                                 lpTemplate,
								 NULL,
								 (DLGPROC)s_dlgProc, 
                                 (LPARAM)this );

}

void AiksaurusDlg::setSearch(string word)
{
	m_szLookupWord = word;
}

string AiksaurusDlg::getReplacement()
{
	if( m_szReplacementWord.empty() )
		return m_szLookupWord;
	else
		return m_szReplacementWord;
}

BOOL AiksaurusDlg::_onInitDialog(HWND hWnd, WPARAM wParam, LPARAM lParam)
{

	m_hSearch   = GetDlgItem( hWnd, ID_BTN_SEARCH );
	m_hBack     = GetDlgItem( hWnd, ID_BTN_BACK );
	m_hForward  = GetDlgItem( hWnd, ID_BTN_FORWARD );
	m_hMeanings = GetDlgItem( hWnd, ID_LBX_MEANING );
	m_hSynonyms = GetDlgItem( hWnd, ID_LBX_SYNONYMS );
	m_hCombo    = GetDlgItem( hWnd, ID_CBX_SEARCH );
	m_hReplace  = GetDlgItem( hWnd, ID_BTN_REPLACEMENT );
	m_hSynText  = GetDlgItem( hWnd, ID_EBX_REPLACEMENT );

	// Set Initial State
	_setNormalButtons();

	EnableWindow( m_hForward, false );
	EnableWindow( m_hBack, false );

	if( !m_szLookupWord.empty() )
	{
		SetWindowText( m_hCombo, m_szLookupWord.c_str());
		_doSearch();
	}

	// Set the Icon of the Dialog
	m_hOrigIcon = (HICON) SetClassLong( hWnd,
                  GCL_HICON,
				  (LONG)LoadIcon(m_hInstance, MAKEINTRESOURCE(ID_ICON_AIK)) ); 

	// Center Window
	RECT rectDesktop, rectDlg;
	HWND hDesktop = GetDesktopWindow();
	GetWindowRect( hWnd, &rectDlg );
	GetWindowRect( hDesktop, &rectDesktop );
	int Width = rectDlg.right - rectDlg.left;
	int Height = rectDlg.bottom - rectDlg.top;
	MoveWindow( hWnd, 
		        (rectDesktop.right - Width) / 2, 
				(rectDesktop.bottom - Height) / 2, 
				Width, 
				Height,
				true );

	return 1;
}

BOOL AiksaurusDlg::_onCommand(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	WORD wNotifyCode = HIWORD(wParam);
	WORD wId = LOWORD(wParam);
	HWND hWndCtrl = (HWND)lParam;

	switch (wId)
	{
	case IDCANCEL:
		SetWindowText( m_hSynText, "");
		// Reset Icon and end Dialog
		SetClassLong( hWnd, GCL_HICON, (LONG)m_hOrigIcon );
		EndDialog(hWnd,0);
		return 1;

	case ID_CBX_SEARCH:
		return 1;

	case ID_LBX_MEANING:
		{
			int nSelect = SendMessage( m_hMeanings, LB_GETCURSEL, 0, 0);
			if( nSelect != CB_ERR )
			{
				if( wNotifyCode == LBN_DBLCLK )
				{
					char buf[MAX_WORD_LENGTH];
					SendMessage( m_hMeanings, LB_GETTEXT, nSelect, (LPARAM)buf );
					SetWindowText( m_hCombo, buf );
					_doSearch();
				}
				else
				{
					if( m_bWordFound )
					{
						_clearSynonyms();
						_showSynonyms( nSelect );
					}
				}
			}
		}
		return 1;

	case ID_LBX_SYNONYMS:
		{
			int nSelect = SendMessage( m_hSynonyms, LB_GETCURSEL, 0, 0);
			if( nSelect != CB_ERR )
			{
				char buf[MAX_WORD_LENGTH];
				SendMessage( m_hSynonyms, LB_GETTEXT, nSelect, (LPARAM)buf );
				if( wNotifyCode == LBN_DBLCLK )
				{
					SetWindowText( m_hCombo, buf );
					_doSearch();
				}
				else
				{
					SetWindowText( m_hSynText, buf);
				}
			}
		}
		return 1;

	case ID_BTN_REPLACEMENT:
		{
			char buf[MAX_WORD_LENGTH];
			GetWindowText( m_hSynText, buf, MAX_WORD_LENGTH );
			m_szReplacementWord = buf;
		}
		// Reset Icon and end Dialog
		SetClassLong( hWnd, GCL_HICON, (LONG)m_hOrigIcon );
		EndDialog(hWnd,0);

	case ID_BTN_SEARCH:
		_doSearch();
		return 1;

	case ID_BTN_FORWARD:
		_onForward();
		return 1;

	case ID_BTN_BACK:
		_onBack();
		return 1;
		
	default:							// we did not handle this notification
		//TRACE(("WM_Command for id %ld\n",wId));
		return 0;						// return zero to let windows take care of it.
	}
}

BOOL AiksaurusDlg::_onSetCursor(HWND hWnd, WPARAM wParam, LPARAM lParam)
{
	_setNormalButtons();
	if( wParam == (WPARAM) m_hSearch )
	{
		m_bSearchBtnChanged = true;
		SendMessage( m_hSearch,
			         BM_SETIMAGE,
					 IMAGE_ICON,
					 (LPARAM) LoadImage( m_hInstance, MAKEINTRESOURCE(ID_ICON_SEARCH_HOVER), IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR) );
	}
	else if( wParam == (WPARAM) m_hBack )
	{
		m_bBackBtnChanged = true;
		SendMessage( m_hBack,
			         BM_SETIMAGE,
					 IMAGE_ICON,
					 (LPARAM) LoadImage( m_hInstance, MAKEINTRESOURCE(ID_ICON_BACK_HOVER), IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR) );
	}
	else if( wParam == (WPARAM) m_hForward )
	{
		m_bForwardBtnChanged = true;
		SendMessage( m_hForward,
			         BM_SETIMAGE,
					 IMAGE_ICON,
					 (LPARAM) LoadImage( m_hInstance, MAKEINTRESOURCE(ID_ICON_FORWARD_HOVER), IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR) );
	}

	return 0;
}


BOOL AiksaurusDlg::_onDeltaPos(NM_UPDOWN * pnmud)
{
	return 1;
}


void AiksaurusDlg::_setNormalButtons()
{
	// Set Icon Images
	if( m_bSearchBtnChanged )
	{
		m_bSearchBtnChanged = false;
		SendMessage( m_hSearch,
			         BM_SETIMAGE,
					 IMAGE_ICON,
					 (LPARAM) LoadImage( m_hInstance, MAKEINTRESOURCE(ID_ICON_SEARCH), IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR) );
	}

	if( m_bForwardBtnChanged )
	{
		m_bForwardBtnChanged = false;
		SendMessage( m_hForward,
			         BM_SETIMAGE,
					 IMAGE_ICON,
					 (LPARAM) LoadImage( m_hInstance, MAKEINTRESOURCE(ID_ICON_FORWARD), IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR) );
	}

	if( m_bBackBtnChanged )
	{
		m_bBackBtnChanged = false;
		SendMessage( m_hBack,
			         BM_SETIMAGE,
					 IMAGE_ICON,
					 (LPARAM) LoadImage( m_hInstance, MAKEINTRESOURCE(ID_ICON_BACK), IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR) );
	}

}

void AiksaurusDlg::_doSearch()
{
	_clearListBoxes();

	char buf[MAX_WORD_LENGTH];
	GetWindowText( m_hCombo, buf, MAX_WORD_LENGTH );
	_addComboString( buf );

	// Add to Back List
	listBack.push_front( buf );
	if( listBack.size() > 1 )
	{
		EnableWindow( m_hBack, true);
	}

	if( thesaurus.error()[0] )
	{
		MessageBox( m_hDlg, thesaurus.error(), "Thesaurus Error", MB_OK );
		return;
	}

	if( thesaurus.find( buf ) )
	{
		m_bWordFound = true;
		_showMeanings();
	}
	else
	{
		m_bWordFound = false;
		_showAlternatives();
	}
}

void AiksaurusDlg::_showAlternatives()
{
	_clearListBoxes();
	SendMessage( m_hMeanings, LB_ADDSTRING, 0, (LPARAM)"No synonyms found");
	SendMessage( m_hMeanings, LB_ADDSTRING, 0, (LPARAM)" Nearby words are:");
	for( const char* s = thesaurus.similar(); s[0]; s = thesaurus.similar() )
	{
		SendMessage( m_hSynonyms, LB_ADDSTRING, 0, (LPARAM)s);
	}
}

void AiksaurusDlg::_showMeanings()
{
	synonyms.clear();
	list<string> syns;
	int meaning, prevMeaning=-1;
	for( const char* s = thesaurus.next(meaning); s[0]; s = thesaurus.next(meaning) )
	{
		if( meaning != prevMeaning )
		{
			if( prevMeaning != -1 )
			{
				synonyms.push_back( syns );
				syns.clear();
			}
			string opt1 = s;
			string opt2 = thesaurus.next(meaning);
			if( !strcmp( thesaurus.word(), opt1.c_str() ) )
				SendMessage( m_hMeanings, LB_ADDSTRING, 0, (LPARAM)opt2.c_str());
			else
				SendMessage( m_hMeanings, LB_ADDSTRING, 0, (LPARAM)opt1.c_str());
			s = thesaurus.next(meaning);
		}

		prevMeaning = meaning;
		// Store in List Structure Tied to Meaning List Selection
		syns.push_back( s );
	}
	synonyms.push_back( syns );
	SendMessage( m_hMeanings, LB_SETCURSEL, 0, 0);
	_showSynonyms( 0 );
}

void AiksaurusDlg::_showSynonyms( int index )
{
	list<string>::iterator count;
	list<string> syn = synonyms[index];
	for( count = syn.begin(); count != syn.end(); count++)
	{
		SendMessage( m_hSynonyms, LB_ADDSTRING, 0, (LPARAM) (*count).c_str() );
	}
}

void AiksaurusDlg::_onBack()
{
	// Add to Forward List Current Item
	std::string s = listBack.front();
	listForward.push_front( s );
	listBack.pop_front();
	EnableWindow( m_hForward, true);

	// Set Current Item to this
	s = listBack.front();
	SetWindowText( m_hCombo, s.c_str() );

	// Remove from Back List
	listBack.pop_front();
	// If Back List empty diaable Button
	if( listBack.empty() )
	{
		EnableWindow( m_hBack, false );
	}

	_doSearch();
}

void AiksaurusDlg::_onForward()
{
	std::string s = listForward.front();
	SetWindowText( m_hCombo, s.c_str() );	

	// Remove From Forward List
	listForward.pop_front();

	// If Forward List empty diaable Button
	if( listForward.empty() )
	{
		EnableWindow( m_hForward, false );
	}
	_doSearch();

}


void AiksaurusDlg::_clearListBoxes()
{
	_clearSynonyms();
	_clearMeanings();
}

void AiksaurusDlg::_clearSynonyms()
{
	SendMessage( m_hSynonyms, LB_RESETCONTENT, 0, 0);
}

void AiksaurusDlg::_clearMeanings()
{
	SendMessage( m_hMeanings, LB_RESETCONTENT, 0, 0);
}

void AiksaurusDlg::_addComboString( string str )
{
	int nPos = SendMessage( m_hCombo, CB_FINDSTRING, -1, (LPARAM)str.c_str() );
	if( nPos == CB_ERR )
	{
		SendMessage( m_hCombo, CB_ADDSTRING, 0, (LPARAM) str.c_str() );
	}
}

void AiksaurusDlg::_copyToClipboard()
{
	char buf[MAX_WORD_LENGTH];
	GetWindowText( m_hSynText, buf, MAX_WORD_LENGTH );
	
    if( !OpenClipboard(NULL) ) return; 
    EmptyClipboard(); 
	
	int nBufSize = strlen( buf );
	HGLOBAL hMem = GlobalAlloc(GMEM_MOVEABLE, nBufSize + 1 );
	if( hMem == NULL )
	{
		CloseClipboard();
		return;
	}

    LPTSTR lptstrCopy = (LPTSTR) GlobalLock(hMem); 
    memcpy( lptstrCopy, buf, nBufSize );
	lptstrCopy[nBufSize] = '\0';   
	GlobalUnlock(hMem); 
 
	SetClipboardData(CF_TEXT, hMem); 
}	
