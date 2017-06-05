////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: 
//////////////////////////////////////////////////////////////////////////// 

#ifndef EXECUTIONTHREAD_H_
#define EXECUTIONTHREAD_H_

#include "core/Thread.h"
#include "core/String.h"
#include "core/VfsItem.h"

class FileList;
class Vfs_Archive;


class ExecutionThread : public Thread
{
public:
	ExecutionThread(FileList *pFileList, const char *pTempFile, const char *pProgram = NULL);
	virtual ~ExecutionThread() {}

protected:
	String m_strProgram;
	String m_strTempFile;
	FileList *m_pFileList;

	virtual void MainMethod();
	bool CopyBack(Vfs *pStrDst, const String& strDir, VfsItem& item);
	Vfs_Archive *CreateVfsArchive(const String& strArchivePath);
};

#endif // EXECUTIONTHREAD_H_
