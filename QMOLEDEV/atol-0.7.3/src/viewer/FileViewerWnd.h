////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Main window for file viewer, implements file drawing
//////////////////////////////////////////////////////////////////////////// 

#ifndef FILEVIEWERWND_H__
#define FILEVIEWERWND_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <gtk/gtk.h>
#include "Storage.h"

class FileViewerWnd
{
public:
	FileViewerWnd();
	virtual ~FileViewerWnd();
	
	void Create();
	void Invalidate();

	void FileLoad(const char *szPath);
	const char *GetFileName();
	void FileClose();

public:
	GtkWidget *m_pWidget;
	CStorage m_data;
	bool	 m_bDeleteOnClose;

	int m_nTopLine;			//first visible line, scroll position
	int m_nNumLinesPerPage;	//defined by the font and window size
	int m_nTotalLines;		//number of lines in document
	int m_nLineHeight;		//height in pixels (font + vert. spacing)
	int m_nLeftOffset;		//horizontal scrolling offset (in pixels)

	//text mode viewer
	int m_nTimer;

protected:
	GtkWidget *create_viewer_window();
	void create_menu_bar(GtkWidget *viewer_window, GtkWidget *parent, GtkAccelGroup *accel_group);
};

#endif // FILEVIEWERWND_H__
