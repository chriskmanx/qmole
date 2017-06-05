////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file operations with large file support (64-bit size)
////////////////////////////////////////////////////////////////////////////

#include "TrayIcon.h"

void ShowTrayMenu();		//external
void OnTrayDefaultAction(); //external

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

LRESULT CALLBACK WindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

TrayIcon::TrayIcon()
{
	m_bVisible		= FALSE;
	m_tnd.cbSize	= sizeof(NOTIFYICONDATA);
	m_tnd.hWnd		= NULL;
	m_tnd.szTip[0]	= '\0';
	m_tnd.hIcon		= NULL;
	m_tnd.uCallbackMessage = WM_USER + 3456;
}

TrayIcon::~TrayIcon()
{
	Hide();

	if( ::IsWindow(m_tnd.hWnd) )
		DestroyWindow(m_tnd.hWnd);
}

//protected - pomoæna funkcija ( kreira prozor objekt	)
void TrayIcon::Create()
{
	if( ::IsWindow(m_tnd.hWnd) ) // already created
		return;
	
     // Create an invisible window - just to receive/handle messages
	 WNDCLASS wc;
	 ZeroMemory(&wc, sizeof(wc));
	 wc.lpszClassName	= "tray_icon";
	 wc.lpfnWndProc		= WindowProc;
	 RegisterClass(&wc);

	 m_tnd.hWnd = CreateWindow("tray_icon", "Tray", WS_POPUP, 0,0,10,10, NULL,NULL,NULL,NULL); 
}

void TrayIcon::Show()
{
	if( m_bVisible )
		return;
	
	Create();	//if not created
	
	if(!m_bVisible)
	{
		m_tnd.uFlags = NIF_MESSAGE | NIF_ICON | NIF_TIP;
		ShellNotify( NIM_ADD );
    }
}

void TrayIcon::Hide()
{
	if( !m_bVisible )
		return;

    if (m_bVisible){
        m_tnd.uFlags = NIF_ICON;
        ShellNotify(NIM_DELETE);
    }
}

void TrayIcon::MoveToRight()
{
    Hide();
    Show();
}

bool TrayIcon::SetTooltip(const char *pszTip)
{
	if( pszTip != NULL )
		strncpy( m_tnd.szTip, pszTip, sizeof(m_tnd.szTip) - 1); //kopira se samo onoliko koliko stane u buffer 
	     
	if( !m_bVisible ) 
		 return TRUE;

    m_tnd.uFlags = NIF_TIP;
    return ShellNotify(NIM_MODIFY);
}

const char *TrayIcon::GetTooltip() const
{
    return m_tnd.szTip;
}

void TrayIcon::SetIcon( HICON hIcon )
{
	m_tnd.hIcon = hIcon;
}

LRESULT CALLBACK WindowProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) 
{
	switch (lParam)
	{
		case WM_RBUTTONUP:
			ShowTrayMenu();
			break;

		case WM_LBUTTONUP:
			OnTrayDefaultAction();
			break;

		default:
			break;
	}

	return DefWindowProc(hWnd, message, wParam, lParam);
}

bool TrayIcon::ShellNotify(DWORD dwMessage)
{
   bool bSuccess = (0 != Shell_NotifyIcon( dwMessage, &m_tnd ));
   switch ( dwMessage )
   {
	case NIM_ADD:    m_bVisible = bSuccess;
					 break;
	case NIM_DELETE: m_bVisible = !bSuccess;
					 break;
	case NIM_MODIFY: //m_bVisible = TRUE; ?
					 break;
	default:
		; //wrong message
   }

   return bSuccess;
}	

