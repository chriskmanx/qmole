////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Merge operation implementation
////////////////////////////////////////////////////////////////////////////

#include <gtk/gtk.h> //TOFIX remove this later
#include "OpMerge.h"
#include "opcodes.h"
#include "PathName.h"
#include "../support.h"

int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);

OpMerge::OpMerge()
{
	m_nOpType = OP_MERGE;
}

OpMerge::~OpMerge()
{
}

bool OpMerge::OpExecute()
{
    if(m_pVfsSrc)
		MergeFiles();
    return 1;
}

void OpMerge::MergeFiles()
{
    //check for no files to merge
    if(m_objSrcItems.GetRootCount() < 1) {
        gtkMessageBox(_("No Files to Merge...."));//TOFIX threadsafe
        return;
    }
    //check for all files    
    if(m_objSrcItems.m_lstRootItems[0].IsDir()) {
        gtkMessageBox(_("Please select Files...."));//TOFIX threadsafe
        return;
    }

	if(m_pVfsSrc->GetType() == Vfs::LOCAL || m_pVfsSrc->GetType() == Vfs::NET)
    {
        if(m_pVfsDst->GetType() == Vfs::LOCAL || m_pVfsDst->GetType() == Vfs::NET)
        {
			m_pStat->InitTotalProgress(m_objSrcItems.GetTotalSize());
            
			//Get Destination dir and file
            String strDstPath;
            strDstPath = m_pVfsDst->GetDir();
            PathName::EnsureTerminated(strDstPath);
            strDstPath += m_strOutFile;
			m_pVfsDst->FixPath(strDstPath);
			
            FILE *pDestFile = fopen(strDstPath.c_str(), "wb");
            if(NULL == pDestFile)
				return; //TOFIX error report
			
            //write from all the source files into dest file
            int fileNum = m_objSrcItems.GetTotalCount();
			int i;
            for (i = 0; i < fileNum; i++)
            {
				String strSrcPath;
				strSrcPath = m_pVfsSrc->GetDir();
				PathName::EnsureTerminated(strSrcPath);
				strSrcPath += m_objSrcItems.m_lstRootItems[i].GetName();
				m_pVfsSrc->FixPath(strSrcPath);

                FILE *pSrcFile = fopen(strSrcPath.c_str(), "rb");
				if(NULL == pSrcFile){
                    gtkMessageBox(_("File open error!")); //TOFIX threadsafe
                    return; 
                }

				//get file size
				fseek(pSrcFile, 0, SEEK_END);
				int nSize = ftell(pSrcFile);
				fseek(pSrcFile, 0, SEEK_SET);

				m_pStat->InitCurrentFiles(strSrcPath, strDstPath);
				m_pStat->InitCurrentProgress(0, nSize);
				
			    //read file contents into buffer
				int nRead = 0;
			    char buffer[2000];
			    
				//copy current file into the destiantion
				while(0 != (nRead = fread(buffer, 1, sizeof(buffer), pSrcFile)))
				{
					//check for abort request
					if(m_pStat->IsAborted()){
						//TOFIX delete destination file
						fclose(pSrcFile);
						fclose(pDestFile);
						return;
					}

					//write data into destination file
					int nWritten = fwrite(buffer, 1, nRead, pDestFile);
					if (nWritten != nRead) 
					{
						//TOFIX delete destination file
						fclose(pSrcFile);
						fclose(pDestFile);
						gtkMessageBox(_("Error writing to File...."));//TOFIX threadsafe
						return;
					}
				}
				fclose(pSrcFile);
            }
            fclose(pDestFile);
         }
    }
}

