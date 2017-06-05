////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to display results of hash calculations for one or more files
//////////////////////////////////////////////////////////////////////////// 

#ifndef HASHRESULTDIALOG_H__
#define HASHRESULTDIALOG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

//TOFIX add popup menu option to save result list to file

#include "Dialog.h"

class HashResultDialog : public Dialog  
{
public:
	HashResultDialog();
	virtual ~HashResultDialog();

	virtual void Create();

protected:
	GtkWidget* create_hash_result_dialog ();

protected:
	GtkListStore *m_store;
};

#endif // HASHRESULTDIALOG_H__
