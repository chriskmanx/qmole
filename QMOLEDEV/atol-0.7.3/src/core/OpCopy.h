////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This class implements file copy operation on a virtual file system (VFS)
////////////////////////////////////////////////////////////////////////////

#ifndef OPCOPY_H__
#define OPCOPY_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Op.h"
#include "VfsListing.h"

class OpCopy : public Op
{
public:
	OpCopy();
	virtual ~OpCopy();

	//if copying single root item (can be directory) this contains full destination name
	//if copying multiple root items this contains destination directory
	String  m_strDstPattern;
	String  m_strName;            //name extracted from m_strDstPattern
	String  m_strOrigDest;
	bool    m_bSingleRootItem;
	bool    m_bLocalDestination;

protected:
	virtual bool OpExecute();

	bool IsDirectCopy(unsigned int dwSrcType, unsigned int dwDstType);
	bool PrepareInitialPath();
    
	bool Copy();
	void CopyRecursive(VfsSelectionItem &item);
	bool SingleFileCopy(VfsSelectionItem &item);
	String GetFullPath(Vfs *pVfs);

	int m_nOpSettings;    //flags
	VfsListing    m_lstDstDir;
};

#endif // OPCOPY_H__

