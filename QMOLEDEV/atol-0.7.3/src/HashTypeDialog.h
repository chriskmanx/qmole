////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to select type of hash algorithm (md5,crc32 or sha1)
//////////////////////////////////////////////////////////////////////////// 

#ifndef HASHTYPEDIALOG_H__
#define HASHTYPEDIALOG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"

class HashTypeDialog : public Dialog  
{
public:
	HashTypeDialog();
	virtual ~HashTypeDialog();

	virtual void Create();

public:
	int m_nHashType;
};

#endif // HASHTYPEDIALOG_H__
