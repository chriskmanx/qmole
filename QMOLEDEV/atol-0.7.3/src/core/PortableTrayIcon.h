////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements portable tray icon class
//////////////////////////////////////////////////////////////////////////// 

#ifndef PORTABLETRAYICON_H__
#define PORTABLETRAYICON_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifdef _WIN32
 #include "_win/TrayIcon.h"
#else
 #include "_unx/eggtrayicon.h"
 #include <gtk/gtk.h>
#endif

class PortableTrayIcon  
{
public:
	PortableTrayIcon();
	virtual ~PortableTrayIcon();

	void Show();
	void Hide();
	void SetTooltip(const char *szTip);

protected:
  #ifdef _WIN32
	TrayIcon m_tray;
  #else
	EggTrayIcon *m_tray_icon; 
	GtkTooltips *m_tips;
  #endif
};

#endif // PORTABLETRAYICON_H__
