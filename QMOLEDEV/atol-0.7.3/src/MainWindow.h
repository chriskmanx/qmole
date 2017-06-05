////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: main window implementation
//////////////////////////////////////////////////////////////////////////// 

#ifndef MAINWINDOW_H__
#define MAINWINDOW_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <gtk/gtk.h>

class MainWindow  
{
public:
	MainWindow();
	virtual ~MainWindow();

	void Create(const char *szLeftPath = NULL, const char *szRightPath = NULL);
	void OnCreate(const char *szLeftPath = NULL, const char *szRightPath = NULL);
	void AddToCommandLine(const char *szText);

public:
	GtkWidget *m_pWidget;
	GtkWidget *m_pHistoryPrev;
	GtkWidget *m_pHistoryNext;
	GtkWidget *m_pConnectionClose;
	GtkWidget *m_pTransferMode;
	GtkWidget *m_pCmdLineWidget;

protected:
	GtkWidget* create_atol_main ();
	GtkWidget* create_toolbar ();
};

#endif // MAINWINDOW_H__
