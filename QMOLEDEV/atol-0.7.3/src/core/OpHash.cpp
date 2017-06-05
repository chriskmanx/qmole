////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: <TOFIX>
////////////////////////////////////////////////////////////////////////////

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "_crypt/SHA1.h"	// must be the top one, because its defines can cause linker error
#include "OpHash.h"
#include "PathName.h"
#include "_crypt/md5.h"
#include "_crypt/Crc32.h"

int OpHashSaveDlgThreadsafe (void);

//TOFIX do not use global objects for result
int g_nHashType;
std::vector<tFileHash> g_lstHashResults;


OpHash::OpHash ()
{
	m_nHashType = 1;
	m_nOpType = OP_HASH;
}

OpHash::~OpHash ()
{
}

bool OpHash::OpExecute()
{
    if(!m_pVfsSrc)
		return false;

    CalculateHash();

	//store hash results
	g_nHashType = m_nHashType;
	g_lstHashResults = m_lstHashResults;

	return true;
}

void OpHash::CalculateHash()
{
    m_lstHashResults.clear();

	//init total progress size
    m_pStat->m_nTotBytesMax = m_objSrcItems.GetTotalSize();

    String strRootDir (m_pVfsSrc->GetDir ());
    PathName::EnsureTerminated(strRootDir);

    //TOFIX 2. for each file selected
    int nRootCount = m_objSrcItems.m_lstRootItems.size ();

    for (int i=0; i<nRootCount; i++)
    {
        //check for abort, ...
        if (m_pStat->IsAborted())
            break;

        String strFile = m_objSrcItems.m_lstRootItems[i].GetName ();
        String strPath  = strRootDir;
			   strPath += strFile;

		SingleFileHash(strPath);
    }
}

void OpHash::SingleFileHash(const char *szPath)
{
    //calculate file hash (with progress)
    String strHash;
    char szBuffer[512]="";
    int nRead = 0;

    FILE *pInput = fopen (szPath, "rb");
    if (NULL == pInput)
        return; //TOFIX return error code

	//get file size
	unsigned long ulFileSize = 0;
	fseek (pInput, 0, SEEK_END);
	ulFileSize = ftell (pInput);
	fseek (pInput, 0, SEEK_SET);

	//init current file progress
	m_pStat->InitCurrentFiles(szPath, "");
	m_pStat->InitCurrentProgress(0, ulFileSize);

    switch (m_nHashType)
    {
        case 1: //CRC
        {
            unsigned long value = 0;
            CCrc32 hash;

            hash.Init();
            while ((nRead = fread(szBuffer, 1, sizeof(szBuffer), pInput)) > 0)
            {
                hash.Update((UINT_8 *)szBuffer, nRead);

				//progress
                m_pStat->StepPos(nRead);
                if (m_pStat->IsAborted())
                    break;
            }
            hash.Final();
            value = hash.GetDigest();

            if (m_pStat->IsAborted())
                strHash = "Aborted";
            else
            {
                unsigned char *pData = (unsigned char *)&value;
                strHash.Printf("%02X%02X%02X%02X",pData[3],pData[2],pData[1],pData[0]);
            }
          }
          break;

        case 2: //md5
        {
            CMD5 hash;
            hash.MD5Init();
            while((nRead = fread(szBuffer, 1, sizeof(szBuffer), pInput))>0)
            {
                hash.MD5Update((unsigned char *)szBuffer, nRead);
                
				//progress
                m_pStat->StepPos(nRead);
                if (m_pStat->IsAborted())
                    break;
            }
            hash.MD5Final();

            if (m_pStat->IsAborted())
                strHash = "Aborted";
            else
                strHash = hash.GetDigest();

        }
        break;

        case 3: //sha1
        {
            CSHA1 hash;
            hash.Reset();
            while ((nRead = fread(szBuffer, 1, sizeof (szBuffer), pInput)) > 0)
            {
                hash.Update ((unsigned char *)szBuffer, nRead);

				//progress
				m_pStat->StepPos(nRead);
                if (m_pStat->IsAborted())
                    break;
            }
            hash.Final();

            if (m_pStat->IsAborted())
                strHash = "Aborted";
            else
            {
                hash.ReportHash(szBuffer, CSHA1::REPORT_HEX);
                strHash = szBuffer;
            }
        }
        break;
    }
    fclose(pInput);

	String strPath(szPath);

	//store result
    tFileHash _hash;
    _hash.strFile = PathName::GetBaseName(strPath);
    _hash.strHash = strHash;
    m_lstHashResults.push_back(_hash);
}


