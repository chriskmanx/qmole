////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: <TOFIX>
////////////////////////////////////////////////////////////////////////////

#ifndef OPENCRYPT_H_
#define OPENCRYPT_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Op.h"

class OpEncrypt : public Op  
{
public:
	OpEncrypt();
	virtual ~OpEncrypt();

	String m_strPassword;
	bool m_bDeleteOriginal;

protected:
    virtual bool OpExecute();
};

#endif // OPENCRYPT_H_
