////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: <TOFIX>
////////////////////////////////////////////////////////////////////////////

#ifndef OPDECRYPT_H_
#define OPDECRYPT_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Op.h"

class OpDecrypt : public Op  
{
public:
	OpDecrypt();
	virtual ~OpDecrypt();

	String m_strPassword;
	bool m_bDeleteOriginal;

protected:
    virtual bool OpExecute();
};

#endif // OPDECRYPT_H_
