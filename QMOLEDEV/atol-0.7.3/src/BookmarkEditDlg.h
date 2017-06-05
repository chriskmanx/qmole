////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Window to show/edit file system bookmrks
//////////////////////////////////////////////////////////////////////////// 

#ifndef BOOKMARKEDITDLG_H__
#define BOOKMARKEDITDLG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/BrowseBookmarkList.h"

class BookmarkEditDlg : public Dialog  
{
public:
	BookmarkEditDlg();
	virtual ~BookmarkEditDlg();

	void Create();
	void RebuildList();
	int GetSelectedIdx();

protected:
	GtkWidget* create_bookmark_manager_dialog ();

public:
	GtkListStore *m_store;
	BrowseBookmarkList m_lstBooks;
};

#endif // BOOKMARKEDITDLG_H__
