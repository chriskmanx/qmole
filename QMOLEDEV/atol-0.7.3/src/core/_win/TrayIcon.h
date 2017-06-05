////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file operations with large file support (64-bit size)
////////////////////////////////////////////////////////////////////////////

#ifndef __TRAYICON_H
#define __TRAYICON_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <windows.h>

class TrayIcon
{
public:
	TrayIcon();
	virtual ~TrayIcon();

public:
	bool IsVisible(){  return m_bVisible; };

	void Show();
	void Hide();
	void MoveToRight(); //make right-most icon
	void SetIcon( HICON hIcon );

	bool SetTooltip(const char *pszTooltip);
	const char *GetTooltip() const;

protected:
	NOTIFYICONDATA  m_tnd;	 	  // tray notification structure
	bool            m_bVisible;   // Is the icon in tray
	UINT			m_uNotifyID;  // ID of the task bar icon
		
	void Create();  //create window
	bool ShellNotify(DWORD dwMessage); //wrapper
};

#endif // __TRAYICON_H
