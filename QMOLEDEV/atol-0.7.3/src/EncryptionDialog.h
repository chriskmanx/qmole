////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Dialog to define file encryption/decryption operation
//////////////////////////////////////////////////////////////////////////// 

#ifndef ENCRYPTIONDIALOG_H__
#define ENCRYPTIONDIALOG_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Dialog.h"
#include "core/String.h"

class EncryptionDialog : public Dialog  
{
public:
	EncryptionDialog();
	virtual ~EncryptionDialog();

	void Create();

protected:
	GtkWidget* create_encryption_dialog();

public:
	bool m_bSelected;
	bool m_bEncrypt;
	bool m_bDeleteOriginal;
	String	m_strPassword;
};

#endif // ENCRYPTIONDIALOG_H__
