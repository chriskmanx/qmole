////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: <TOFIX>
////////////////////////////////////////////////////////////////////////////

#ifndef OPHASH_H__
#define OPHASH_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Op.h"
#include <vector>

typedef struct
{
    String strFile;
    String strHash;
} tFileHash;

class OpHash : public Op
{
public:
    OpHash();
    virtual ~OpHash();

    //hash op data
    int m_nHashType;
    std::vector<tFileHash> m_lstHashResults;

protected:
    void CalculateHash();
	void SingleFileHash(const char *szPath);

	virtual bool OpExecute();
};

#endif // OPHASH_H__

