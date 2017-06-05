////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define options for a file merge operation
//////////////////////////////////////////////////////////////////////////// 

#ifndef FILEMERGEDIALOG_H__
#define FILEMERGEDIALOG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"

//TOFIX RecalcOutputName() method

class FileMergeDialog : public Dialog  
{
public:
	FileMergeDialog();
	virtual ~FileMergeDialog();

	void Create();
	void AddFile(const char *szName);

	int GetFileCount();
	const char *GetFileName(int nIdx);

	const char *GetDestFile();

protected:
	GtkWidget* create_file_merge_dialog ();

protected:
	GtkListStore *m_store;
};

#endif // FILEMERGEDIALOG_H__
