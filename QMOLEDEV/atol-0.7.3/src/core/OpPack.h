////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This class implements file compress operation on a virtual file system (VFS)
////////////////////////////////////////////////////////////////////////////

#ifndef OPPACK_H_
#define OPPACK_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "OpCopy.h"

class OpPack : public OpCopy  
{
public:
    OpPack();
    virtual ~OpPack();

    String m_strArchive;
	bool   m_bMove;

protected:
    bool Pack();

    virtual bool OpExecute();
};

#endif // OPPACK_H_
