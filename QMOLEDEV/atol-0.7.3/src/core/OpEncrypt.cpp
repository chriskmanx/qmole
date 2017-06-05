////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: <TOFIX>
////////////////////////////////////////////////////////////////////////////

#include "../support.h"
#include "OpEncrypt.h"
#include "_crypt/blowfish.h"
#include "_crypt/md5.h"
#include "PathName.h"

#ifdef _WIN32
 #include <io.h> //access
#else
 #include<unistd.h>
#endif

bool EncryptFile(const char *szFile, const char *szPwd, OpState *pProgress);
int MsgBox_ThrSafe(String strTitle, int nButtons = GTK_BUTTONS_OK, bool bYNC = false);

OpEncrypt::OpEncrypt()
{
	m_bDeleteOriginal = false;
	m_nOpType = OP_ENCRYPT;
}

OpEncrypt::~OpEncrypt()
{
}

bool OpEncrypt::OpExecute()
{
    m_pStat->m_nTotBytesMax = m_objSrcItems.GetTotalSize();

    int nRootCount = m_objSrcItems.m_lstRootItems.size();
    for(int i=0; i<nRootCount; i++)
    {
        //check for abort, ...
        if(m_pStat->IsAborted())
            break;

        if(!m_objSrcItems.m_lstRootItems[i].IsDir())
        {
            String  strPath  = m_pVfsSrc->GetDir();
			PathName::EnsureTerminated(strPath, '/');
			strPath += m_objSrcItems.m_lstRootItems[i].GetName();

            if(EncryptFile(strPath, m_strPassword.c_str(), m_pStat))
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


//TOFIX separate module (file/class)
//TOFIX use CFile64
//TOFIX aditional parameter for output path
bool EncryptFile(const char *szFile, const char *szPwd, OpState *pProgress)
{
	//TOFIX check for NULL password here ?
	//assert(NULL != szPwd);

	//calculate hash-value from given password
	CMD5 md5;
	md5.MD5Init();
	md5.MD5Update((unsigned char *)szPwd, strlen(szPwd));
	md5.MD5Final();

    //NOTE: important => bufer must be multiple of 8 bytes
    unsigned char buf_in[256];
    unsigned char buf_out[512];

    CBlowFish fish;
    fish.Initialize((unsigned char *)szPwd, strlen(szPwd));

    FILE *pInFile = fopen(szFile, "rb");
	if(NULL != pInFile)
    {
		//get file size
		fseek(pInFile, 0, SEEK_END);
		int nSize = ftell(pInFile);
		fseek(pInFile, 0, SEEK_SET);

		//TOFIX aditional parameter for output path
        String strOut(szFile);
        strOut += ".enc";
        
		//report initial size
        if(NULL != pProgress){
			//NOTE do this before initsingle
			pProgress->InitCurrentFiles(szFile, strOut);
            pProgress->InitCurrentProgress(0, nSize);
			if(pProgress->IsAborted()){
				fclose(pInFile);
				MsgBox_ThrSafe(_("File encryption aborted!"));
				return false;
			}
		}

		//check for file overwrite
		if(0 == access(strOut, 0)){
			String strMsg;
			strMsg.Printf(_("File %s already exists. Overwrite?"), strOut.c_str());
			if(GTK_RESPONSE_YES != MsgBox_ThrSafe(strMsg, GTK_BUTTONS_YES_NO)){
				fclose(pInFile);
				return false;
			}
		}

		FILE *pOutFile = fopen(strOut, "wb");
		if(NULL != pOutFile)
        {
			//write header string + original file size
			fwrite("Fh_enc:BF10", strlen("Fh_enc:BF10"), 1, pOutFile); //TOFIX define?
			fwrite(md5.GetDigestBinary(), 16, 1, pOutFile);		//write password hash value (md5 algorithm)
			fwrite(&nSize, sizeof(int), 1, pOutFile);			//write original file size

			int nJobDone = ftell(pInFile);

            int nRead, nEncodedSize;
            while(0 != (nRead = fread(buf_in, 1, 256, pInFile)))
            {
                nEncodedSize = fish.Encode(buf_in, buf_out, nRead);
                nJobDone += nRead;

				fwrite(buf_out, 1, nEncodedSize, pOutFile);

                //report progress
                if(NULL != pProgress)
                {
                    pProgress->SetPos(nJobDone);

					if(pProgress->IsAborted())
					{
						fclose(pInFile);
						fclose(pOutFile);
						//TOFIX delete output
						MsgBox_ThrSafe(_("File encryption aborted!"));
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

