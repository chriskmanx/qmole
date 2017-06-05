////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file split operation on a local file system
////////////////////////////////////////////////////////////////////////////

#ifndef OPSPLIT_H__
#define OPSPLIT_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Op.h"
#include "VfsListing.h"

class OpSplit : public Op  
{
public:
	OpSplit();
	virtual ~OpSplit();

	unsigned int m_nSplitSize;
    
protected:
	void SplitFile();
    virtual bool OpExecute();
};

#endif // OPSPLIT_H__
