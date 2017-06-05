////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Merge operation implementation
////////////////////////////////////////////////////////////////////////////

#ifndef OPMERGE_H_
#define OPMERGE_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Op.h"
#include "String.h"

class OpMerge : public Op  
{
public:
	OpMerge();
	virtual ~OpMerge();
	
	String m_strOutFile;

protected:
	virtual bool OpExecute();
    void MergeFiles();
};

#endif // OPMERGE_H_
