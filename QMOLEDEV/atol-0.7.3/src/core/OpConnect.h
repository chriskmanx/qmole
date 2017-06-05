////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This class implements connect operation on a virtual file system (VFS)
////////////////////////////////////////////////////////////////////////////

#ifndef OPCONNECT_H_
#define OPCONNECT_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Op.h"
//#include "BlockingDlg.h"
#include "VfsListing.h"

class OpConnect : public Op  
{
public:
	OpConnect();
	virtual ~OpConnect();

    //CBlockingDlg    *m_pDlg;

protected:
    virtual void OpInit();
    virtual void OpCleanup();
    virtual bool OpExecute();
};

#endif // OPCONNECT_H_
