////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define options for a file split operation
//////////////////////////////////////////////////////////////////////////// 

#ifndef FILESPLITDIALOG_H__
#define FILESPLITDIALOG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"

class FileSplitDialog : public Dialog  
{
public:
	FileSplitDialog();
	virtual ~FileSplitDialog();

	void Create();

	//TOFIX use INT64 !
	typedef struct {
        const char *szName;
        unsigned int nSize;
    } SPLIT_DEF; 

	int GetSelectedSize();
};

#endif // FILESPLITDIALOG_H__
