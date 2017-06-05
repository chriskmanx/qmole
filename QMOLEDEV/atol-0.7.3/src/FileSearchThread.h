////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Defines thread for a file search operation
//////////////////////////////////////////////////////////////////////////// 

#ifndef FILESEARCHTHREAD_H__
#define FILESEARCHTHREAD_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "core/Thread.h"
#include "core/String.h"
#include "core/FilterDesc.h"
#include "core/Vfs.h"
#include "core/VfsListing.h"

class FileSearchThread : public Thread  
{
public:
	FileSearchThread();
	virtual ~FileSearchThread();

	void Abort();

public:
	//search description
	FilterDesc	m_objInfo;      //search info
	String		m_strDirectory; //list of directories
	bool		m_bRecursive;   //recurse into subdirs?

	//other info
	void *m_pCaller;   //pointer to the calling dialog object
	bool  m_bAbort;    //abort request
	bool  m_bDone;     //

protected:
	virtual void MainMethod();
	void RecursiveList(Vfs *pVFS, VfsListing &lstRoot);
};

#endif // FILESEARCHTHREAD_H__
