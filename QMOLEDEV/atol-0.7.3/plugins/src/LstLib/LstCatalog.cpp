////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements working with .lst "archives" (stores just file lists)
//		 This archive is compatible to TotalCommander .lst archive plugin
////////////////////////////////////////////////////////////////////////////

#include "LstCatalog.h"
#include <time.h>

int strcommon(const char *szStr1, const char *szStr2);
unsigned int strcommon1(const char *szStr1, const char *szStr2);

CLstCatalog::CLstCatalog()
{
	m_pArchive = NULL;
}

CLstCatalog::~CLstCatalog()
{
	Close();
}

bool CLstCatalog::Open(const char *szFile, bool bRead)
{
	Close(); // just in case
	m_pArchive = fopen(szFile, (bRead)?"r":"w");
	return IsOpen();
}

void CLstCatalog::Close()
{
	if(m_pArchive)
		fclose(m_pArchive);
	m_pArchive = NULL;
}

void CLstCatalog::ReadList()
{
	char szBuffer[300];

	if(!m_pArchive)
		return;

	m_lstFiles.Clear();		

	std::string strParentDir; //last parent dir

	//read file line by line
	int nLine = 0;
	while(NULL != fgets(szBuffer, sizeof(szBuffer), m_pArchive))
	{
		//kill \n at the end
		char *pszEnd = strchr(szBuffer, '\n');
		if(pszEnd)
			*pszEnd = '\0';

		if(0 == nLine)
		{
			m_lstFiles.m_strRoot = szBuffer;	//remember root dir
		}
		else
		{
			//parse file
			tItem item;

			char *szToken = strtok(szBuffer, "\t");
			if(szToken)
			{
				std::string strLine = szToken;

				//calculate basename
				std::string strName = szToken;
				if("\\" == strName.substr(strName.size()-1, 1))
					strName = strName.substr(0, strName.size()-1);
				int nPos = strName.find_last_of('\\');
				if(nPos >= 0)
					strName = strName.substr(nPos+1, strName.size());
				nPos = strName.find_last_of('/');	// Linux delimiter support
				if(nPos >= 0)
					strName = strName.substr(nPos+1, strName.size());

				item.strFile = strName;

				//test if directory
				if("\\" == strLine.substr(strLine.size()-1, 1))
				{
					item.strFullPath   = strLine; //relative to root
					item.strFile	   = strName;

					item.bDir = true;
					strParentDir = item.strFullPath;
				}
				else{
					item.strFullPath  = strParentDir; //relative to root
					item.strFullPath += szToken;
					item.strFile	  = strName;
					item.bDir		  = false;
				}

				//read file size
				szToken = strtok(NULL, "\t");
				if(szToken)
					item.nSize = atoi(szToken);

				//read file date and file time
				struct tm tms; 
				memset(&tms, 0, sizeof(tms));

				szToken = strtok(NULL, "\t");
				if(szToken)
					sscanf(szToken, "%d.%d.%d", &tms.tm_year, &tms.tm_mon, &tms.tm_mday);
				szToken = strtok(NULL, "\t");
				if(szToken)
					sscanf(szToken, "%d:%d.%d\n", &tms.tm_hour, &tms.tm_min, &tms.tm_sec);

				tms.tm_year -= 1900;
				tms.tm_mon  -= 1;

				//convert to time_t
				item.tmModified = mktime(&tms);
				if(-1 == item.tmModified)
					item.tmModified = 0;

				CEntry *pEntry = m_lstFiles.Insert(item.strFullPath.c_str());
				if(pEntry)
					*pEntry = item;
			}
		}

		nLine ++;
	}
}

void CLstCatalog::WriteList()
{
	if(!m_pArchive)
		return;

	int nCount = m_lstFiles.GetCount();
	if(nCount < 1)
		return;

	m_lstFiles.Reorder();

	//write items to file as separate lines
	std::string strRoot(m_lstFiles.m_strRoot);
#ifndef _WIN32
	if(strRoot.empty())
		strRoot = "\\";
#endif
	unsigned int nRootSize = 0;

	for(int i=0; i<nCount; i++)
	{
		CEntry *pEntry = m_lstFiles.GetEntryRecursive(i);
		if(pEntry)
		{
			std::string strName = pEntry->strFile;
			if(pEntry->bDir){
				pEntry->nSize = 0;
				strName += "\\";
			}

			if(0 == i)
			{
				//on first of the root entries try to grow root dir
				//(merge subdirs into the root name until there is single entry inside them)
				while(pEntry->m_lstEntries.size()==1)
				{
					strRoot += strName;

					CEntry *pNextEntry = &(pEntry->m_lstEntries[0]);

					//refresh name
					strName = pNextEntry->strFile;
					if(pEntry->bDir){
						pEntry->nSize = 0;
						strName += "\\";
					}

					// go forward
					i++;
					pEntry = pNextEntry; 

					if(pNextEntry->m_lstEntries.size()!=1)
						break;
				}

				nRootSize = strRoot.size();

				//write root line
				fprintf(m_pArchive, "%s\n", strRoot.c_str());
			}

			if(pEntry->bDir)
			{
				if(pEntry->strFullPath.size() > 0)
				{
					strName = pEntry->strFullPath;	//dirs are written as full path relative to root dir
					if(strName.size() > nRootSize)
						if(strcommon1(strName.c_str(), strRoot.c_str()) >= nRootSize)
							strName = strName.substr(nRootSize);
					if(strName.size() > 0 && strName.at(strName.size()-1) != '\\')
						strName += "\\";
				}
			}

			struct tm *pTM = localtime(&(pEntry->tmModified));
			if(NULL != pTM)
			{
				fprintf(m_pArchive, "%s\t%d\t%d.%d.%d\t%d:%d.%d\n",
					strName.c_str(),
					pEntry->nSize,
					pTM->tm_year + 1900,
					pTM->tm_mon + 1,
					pTM->tm_mday,
					pTM->tm_hour,
					pTM->tm_min,
					pTM->tm_sec);
			}
			else
			{
				//TOFIX why would date not be valid?
				fprintf(m_pArchive, "%s\t%d\n",
					strName.c_str(),
					pEntry->nSize);
			}
		}
	}
}

bool CLstCatalog::Reorganize(const char *szRoot)
{
	//can the new item with this root be added to this collection ?
	//TOFIX

	//now try to find if they share any common path with the new one
	/*
	int nMin = 10000;
	for(int i=0; i<m_lstFiles.size(); i++){
		int nLen = strcommon(m_lstFiles[i].strFile.c_str(), szRoot);
		if(nLen < nMin)
			nMin = nLen;
	}
	if(nMin > strlen(szRoot))	//fix for 10000
		nMin = strlen(szRoot);

	if(nMin > 0)	//common shared path exists
	{
		m_strRoot = szRoot;
		m_strRoot = m_strRoot.substr(0, nMin);

		//convert all paths to showrt name using new root
		for(int i=0; i<m_lstFiles.size(); i++)
			m_lstFiles[i].strFile = m_lstFiles[i].strFile.substr(nMin, 1000);

		return true;
	}
	*/

	return false;
}

//TOFIX copy to some memory lib
//how many character are being shared between two strings (at the start)
int strcommon(const char *szStr1, const char *szStr2)
{
	int nCommon = 0;

	if(szStr1 && szStr2)
	{
		while(*szStr1 && *szStr2)
		{
			if(*szStr1 != *szStr2)
				break;
			nCommon ++; 
			szStr1++; 
			szStr2++;
		}
	}
	return nCommon;
}

unsigned int strcommon1(const char *szStr1, const char *szStr2)
{
	unsigned int nCommon = 0;

	if(szStr1 && szStr2)
	{
		while(*szStr1 && *szStr2)
		{
			if( *szStr1 == *szStr2 || 
			   (*szStr1 == '\\' && *szStr2 == '/') ||
			   (*szStr1 == '/'  && *szStr2 == '\\'))
			{
				nCommon ++; 
				szStr1++; 
				szStr2++;
			}
			else
				break;
		}
	}
	return nCommon;
}

