////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file delete operation on a virtual file system (VFS)
////////////////////////////////////////////////////////////////////////////

#ifndef OPDELETE_H__
#define OPDELETE_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Op.h"
#include "VfsListing.h"

class OpDelete : public Op  
{
public:
	OpDelete();
	virtual ~OpDelete();

	bool m_bRecycleBin;
	int m_nOpSettings;    //flags

protected:
	void DeleteRecursive(VfsSelectionItem &item);
	virtual bool OpExecute();

	VfsListing m_lstSrcDir;
};

#endif // OPDELETE_H__


