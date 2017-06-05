////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements file split operation on a local file system
////////////////////////////////////////////////////////////////////////////

#include "OpSplit.h"
#include "PathName.h"
#include <stdio.h>

#ifndef min
 #define min(a,b) ((a)<(b))?(a):(b)
#endif

static bool SaveSegment(FILE *pSrcFile, unsigned int nStart, unsigned int uSize, const char *szSegFile, OpState *pInfo);

OpSplit::OpSplit()
{
	m_nSplitSize = 0;
	m_nOpType = OP_SPLIT;
}

OpSplit::~OpSplit()
{
}

bool OpSplit::OpExecute()
{
	if(m_pVfsSrc)
		SplitFile();

	return 1;
}

void OpSplit::SplitFile()
{
    if(m_nSplitSize < 1)    //TOFIX ?
      return;

    if(m_objSrcItems.GetRootCount() < 1)  //TOFIX error code, messagebox
        return;
    if(m_objSrcItems.m_lstRootItems[0].IsDir()) //TOFIX error code, messagebox
        return;

	//TOFIX automatic split size?
	//TOFIX check for disk size (especially removable disk)
    //radi samo s diska nad disk
    if(m_pVfsSrc->GetType() == Vfs::LOCAL || m_pVfsSrc->GetType() == Vfs::NET)
    {
        if(m_pVfsDst->GetType() == Vfs::LOCAL || m_pVfsDst->GetType() == Vfs::NET)
        {
            String strFile =  m_objSrcItems.m_lstRootItems[0].GetName();
            String strSrcPath = m_pVfsSrc->GetDir();
			PathName::EnsureTerminated(strSrcPath);
			strSrcPath += strFile;

            //TOFIX CFile64
            FILE *pSrcFile = fopen(strSrcPath.c_str(), "rb");
            if(NULL == pSrcFile)
                return; //TOFIX report error? FALSE;

			//get file length
			fseek(pSrcFile, 0, SEEK_END);
			unsigned int nTotal = ftell(pSrcFile);	//TOFIX 64 bit
			fseek(pSrcFile, 0, SEEK_SET);

            //report initial size - total progress
            m_pStat->InitTotalProgress(nTotal);

            int nSegmentID = 1;
            unsigned int nSegmentStart = 0;

            while(nSegmentStart < nTotal)
            {
			     //check for abort, ...
				if(m_pStat->IsAborted())
					break;

		String strDir(m_pVfsDst->GetDir());
		PathName::EnsureTerminated(strDir);

		String strDest;
        strDest.Printf("%s%s.%03d", strDir.c_str(), strFile.c_str(), nSegmentID);
//TOFIX		m_pVfsDst->FixPath(strDest);

                unsigned int nSize = min(nTotal-nSegmentStart, m_nSplitSize);

                //TRACE("Save file segment: %s\n", strDest);

                if(!SaveSegment(pSrcFile, nSegmentStart, nSize, strDest.c_str(), m_pStat))
                    return; //TOFIX report error

                nSegmentID ++;
                nSegmentStart += m_nSplitSize;
            }

            //cleaning
            fclose(pSrcFile);
        }
    }
}

//TOFIX move to separte class
//TOFIX CFile64
bool SaveSegment(FILE *pSrcFile, unsigned int nStart, unsigned int uSize, const char *szSegFile, OpState *pInfo)
{
    //TOFIX CFile64
    FILE *pDestFile = fopen(szSegFile, "wb");
    if(NULL == pDestFile)
    {
        //TRACE("Segment file open error.\n");
        fclose(pSrcFile);
        return false;
    }

    unsigned int buffer[1000];

    //get file length
    unsigned int total  = uSize;    //TOFIX 64 bit
    unsigned int copied = 0;
    unsigned int read   = 0;

    fseek(pSrcFile, nStart, SEEK_SET);

    //report initial size
    if(NULL != pInfo){
		//NOTE do this before initsingle
		pInfo->InitCurrentFiles(NULL, szSegFile);
        pInfo->InitCurrentProgress(0, total);
	}

    unsigned int nToGo = uSize;
    while(0 != (read = fread(buffer, 1, min(sizeof(buffer), nToGo), pSrcFile)))
    {
        nToGo -= read;

        if(    (pInfo && pInfo->IsAborted())
			|| (read != fwrite(buffer, 1, read, pDestFile)))
        {
            fclose(pSrcFile);
            fclose(pDestFile);
            return false;
        }

        copied += read;

        //report progress
        if(NULL != pInfo)
            pInfo->SetPos(copied);
    }

    fclose(pDestFile);
    return true;
}



