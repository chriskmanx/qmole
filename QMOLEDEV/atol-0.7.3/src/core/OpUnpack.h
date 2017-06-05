////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This class implements file decompress operation on a virtual file system (VFS)
////////////////////////////////////////////////////////////////////////////

#ifndef OPUNPACK_H_
#define OPUNPACK_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "OpCopy.h"

class OpUnpack : public OpCopy  
{
public:
	OpUnpack();
	virtual ~OpUnpack();

protected:
	bool Unpack();

	virtual bool OpExecute();
};

#endif // OPUNPACK_H_
