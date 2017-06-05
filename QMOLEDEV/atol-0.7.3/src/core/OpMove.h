////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file move operation on a virtual file system (VFS)
////////////////////////////////////////////////////////////////////////////

#ifndef OPMOVE_H__
#define OPMOVE_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "OpCopy.h"
#include "VfsListing.h"

class OpMove : public OpCopy
{
public:
    OpMove();
    virtual ~OpMove();

protected:
    void MoveRecursive(VfsSelectionItem &item);
    bool SingleFileMove(VfsSelectionItem &item);

    virtual bool OpExecute();

    bool m_bMoveDirect;
};

#endif // OPMOVE_H__

