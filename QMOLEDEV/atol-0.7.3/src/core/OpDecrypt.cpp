////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: <TOFIX>
////////////////////////////////////////////////////////////////////////////

#include "../support.h"
#include "OpDecrypt.h"
#include "PathName.h"
#include "_crypt/blowfish.h"
#include "_crypt/md5.h"

#ifdef _WIN32
 #include <io.h> //access
#else
 #include<unistd.h>
#endif

bool DecryptFile(const char *szFile, const char *szPwd, OpState *pProgress);
int MsgBox_ThrSafe(String strTitle, int nButtons = GTK_BUTTONS_OK, bool bYNC = false);

OpDecrypt::OpDecrypt()
{
	m_bDeleteOriginal = false;
	m_nOpType = OP_DECRYPT;
}

OpDecrypt::~OpDecrypt()
{
}

bool OpDecrypt::OpExecute()
{
    m_pStat->m_nTotBytesMax = m_objSrcItems.GetTotalSize();

	//decrpyt all selected files (skip dirs)
    int nRootCount = m_objSrcItems.m_lstRootItems.size();
    for(int i=0; i<nRootCount; i++)
    {
        //check for abort, ...
        if(m_pStat->IsAborted())
            break;

        if(!m_objSrcItems.m_lstRootItems[i].IsDir())
        {
            String strPath = m_pVfsSrc->GetDir();
			PathName::EnsureTerminated(strPath);
			strPath += m_objSrcItems.m_lstRootItems[i].GetName();

            if(DecryptFile(strPath, m_strPassword.c_str(), m_pStat))
            {
				//error handling for delete
				//TOFIX tofix support for deleting readonly files (see VfsLocal::Delete)
                if(m_bDeleteOriginal)
                    remove(strPath);  //TOFIX wipe file
            }
        }
    }

    return true;
}

bool DecryptFile(const char *szFile, const char *szPwd, OpState *pProgress)
{
	//assert(NULL != szPwd);

	unsigned char buf_in[256];
	unsigned char buf_out[512];

	CBlowFish fish;
	fish.Initialize((unsigned char *)szPwd, strlen(szPwd));

	FILE *pInFile = fopen(szFile, "rb");
	if(NULL != pInFile)
	{
		//check for header string
		char szHeader[15];
		fread(szHeader, strlen("Fh_enc:BF10"), 1, pInFile);	//TOFIX define
		if(0 != strncmp(szHeader, "Fh_enc:BF10", strlen("Fh_enc:BF10")))
		{
			//TOFIX notify this error to client
			fclose(pInFile);
			//TOFIX beter error handling (msg box outside of this module)
			MsgBox_ThrSafe(_("Invalid file header (not encrypted with Atol)!"));
			return false;	//this is not compressed with my code
		}

		//read hash value from file (md5 algorithm)
		unsigned char szHash[16];
		fread(szHash, 16, 1, pInFile);

		//calculate this-password hash value
		CMD5 md5;
		md5.MD5Init();
		md5.MD5Update((unsigned char *)szPwd, strlen(szPwd));
		md5.MD5Final();

		//compare hash values to quickly validate given password
		if(0 != memcmp(szHash, md5.GetDigestBinary(), 16))
		{
			//TOFIX beter error handling (msg box outside of this module)
			fclose(pInFile);
			//TOFIX delete output
			MsgBox_ThrSafe(_("Invalid password!"));
			return false;	//TOFIX MessageBox?
		}

		int nSize = 0;
		fread(&nSize, sizeof(int), 1, pInFile);

		//TOFIX make sure that file name has ".enc" extension
		//new name has stripped extension -> ".enc"
		String strOut(szFile);
		PathName::StripExtension(strOut);

		//report initial size
		if(NULL != pProgress){
			//NOTE do this before initsingle
			pProgress->InitCurrentFiles(szFile, strOut);
			pProgress->InitCurrentProgress(0, nSize);	//TOFIX use right size (read from within file - see below)????
			if(pProgress->IsAborted()){
				fclose(pInFile);
				//TOFIX delete output
				MsgBox_ThrSafe(_("File decryption aborted!"));
				return false;
			}
		}

		//check for file overwrite
		if(0 == access(strOut, 0)){
			//TOFIX thead safe
			String strMsg;
			strMsg.Printf(_("File %s already exists. Overwrite?"), strOut.c_str());
			if(GTK_RESPONSE_YES != MsgBox_ThrSafe(strMsg, GTK_BUTTONS_YES_NO)){
				fclose(pInFile);
				return false;
			}
		}

		//TOFIX check if the name exists
		FILE *pOutFile = fopen(strOut, "wb");
		if(NULL != pOutFile)
		{
			int nJobDone = 0;

			int nRead;
			while(0 != (nRead = fread(buf_in, 1, 256, pInFile)))
			{
				fish.Decode(buf_in, buf_out, nRead);

				nJobDone += nRead;

				//strip
				if(nJobDone >= nSize)
					nRead -= nJobDone - nSize;

				//TOFIX error handling on all read/write
				fwrite(buf_out, nRead, 1, pOutFile);

				//report progress
				if(NULL != pProgress)
				{
					pProgress->SetPos(nJobDone);

					if(pProgress->IsAborted()){
						fclose(pOutFile);
						fclose(pInFile);
						//TOFIX delete output
						MsgBox_ThrSafe(_("File decryption aborted!"));
						return false;
					}
				}
			}

			fclose(pOutFile);
		}
		else{
			MsgBox_ThrSafe(_("Failed to open output file!"));
			return false;
		}

		fclose(pInFile);
	}
	else{
		MsgBox_ThrSafe(_("Failed to open input file!"));
		return false;
	}

	return true;
}
