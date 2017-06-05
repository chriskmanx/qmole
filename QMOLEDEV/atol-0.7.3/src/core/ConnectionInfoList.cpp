////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "ConnectionInfoList.h"
#include "PathName.h"
#include <algorithm>
#ifdef _WIN32
  #include <windows.h>
#endif

CNodeInfo::CNodeInfo()
{
	m_nID	= -1;
	m_nPID	= -1;
	m_bIsFolder	= false;
	m_bExpanded	= false;
	m_hItem		= 0;
}

void CNodeInfo::operator =(const CNodeInfo &a)
{
	CFtpInfo::operator = (a);

	m_nID		= a.m_nID;
	m_nPID		= a.m_nPID;
	m_bIsFolder	= a.m_bIsFolder;
	m_bExpanded	= a.m_bExpanded;
	m_hItem		= a.m_hItem;
}

bool CNodeInfo::operator <(const CNodeInfo &a)
{
	//if we are parent we go before child
	//if(m_nPID == a.m_nID)
	//	return true;

	//if we are child we go after parent
	//if(m_nID == a.m_nPID)
	//	return false;

	//if we have smaller ID we are before other item
	if(m_nID < a.m_nID)
		return true;

	return false;
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CConnectionInfoList::CConnectionInfoList()
{
}

CConnectionInfoList::~CConnectionInfoList()
{
}

//search ftp links by name
int CConnectionInfoList::Find(const char *szName)
{
	for(unsigned int i=0; i<size(); i++)
		if(	operator[](i).m_strTitle == szName )
			return i;

	return -1;	//not found
}

int CConnectionInfoList::Find(int nID)
{
	for(unsigned int  i=0; i<size(); i++)
		if(	operator[](i).m_nID == nID )
			return i;

	return -1;	//not found
}

int CConnectionInfoList::Find(long hItem)
{
	for(unsigned int  i=0; i<size(); i++)
		if(	operator[](i).m_hItem == hItem )
			return i;

	return -1;	//not found
}

void CConnectionInfoList::Sort()
{
	//TOFIX not used
	//std::sort(begin(), end());
}

void CConnectionInfoList::Remove(int nIdx)
{
	//ASSERT(0 <= nIdx && nIdx < size());
	erase(begin()+nIdx);
}

int	 CConnectionInfoList::GetMaxID()
{
	int nMax = 0;
	for(unsigned int i=0; i<size(); i++)
	{
		if(operator[](i).m_nID > nMax)
			nMax = operator[](i).m_nID;
	}

	return nMax;
}

void CConnectionInfoList::Dump()
{
	/*
	TRACE("CConnectionInfoList::Dump\n");
	for(int i=0; i<size(); i++)
	{
		TRACE("Item: %s, ID=%d, PID=%d\n", operator[](i).m_strTitle, operator[](i).m_nID,operator[](i).m_nPID);
	}
	TRACE("\n");
	*/
}

bool CConnectionInfoList::SaveToIni(const char *szFileName)
{
	//require encrypted format
	if(m_strEncPassword.size()<1)
		return false;

	//ensure file is empty and exists
	FILE *pOut = fopen(szFileName,"w");
	if(pOut)
		fclose(pOut);

	IniFile ini;
	if(m_strEncPassword.size()>0)
		ini.SetEncrypted(m_strEncPassword.c_str());
	ini.SetPath(szFileName);

	SaveToIni(ini);

	return ini.Save();
}

bool CConnectionInfoList::LoadFromIni(const char *szFileName)
{
	//clear previous list content
	clear();

	//require encrypted format
	if(m_strEncPassword.size()<1)
		return false;

	IniFile ini;
	if(m_strEncPassword.size()>0)
		ini.SetEncrypted(m_strEncPassword.c_str());
	if(!ini.Load(szFileName))
		return false;

	return LoadFromIni(ini);
}

bool CConnectionInfoList::LoadFromIni(IniFile &ini)
{
	//clear previous list content
	clear();

	//NOTE: could serialize to CMemFile but this is much cleaner
	String strValueName;

	int nTotalCount;
	ini.GetValue("Sites", "count", nTotalCount, 0);
	for(int i=0; i<nTotalCount; i++)
	{
		CNodeInfo info;

		strValueName.Printf("%d_Title",i);
		ini.GetValue("Sites", strValueName, info.m_strTitle, "");

		strValueName.Printf("%d_Host",i);
		ini.GetValue("Sites", strValueName, info.m_strHost, "");

		strValueName.Printf("%d_User",i);
		ini.GetValue("Sites", strValueName, info.m_strUser, "");

		strValueName.Printf("%d_Password",i); 
		ini.GetValue("Sites", strValueName, info.m_strPassword, "");

		strValueName.Printf("%d_Account",i); 
		ini.GetValue("Sites", strValueName, info.m_strAccount, "");

		strValueName.Printf("%d_RemoteDir",i); 
		ini.GetValue("Sites", strValueName, info.m_strRemoteDir, "");

		strValueName.Printf("%d_LocalDir",i); 
		ini.GetValue("Sites", strValueName, info.m_strLocalDir, "");

		strValueName.Printf("%d_Port",i); 
		ini.GetValue("Sites", strValueName, info.m_uPort, 0);

		strValueName.Printf("%d_Anonymous",i); 
		ini.GetValue("Sites", strValueName, info.m_bAnonymous, 0);

		strValueName.Printf("%d_Passive",i); 
		ini.GetValue("Sites", strValueName, info.m_bPassiveMode, 0);

		strValueName.Printf("%d_UseFirewall",i); 
		ini.GetValue("Sites", strValueName, info.m_bUseFirewall, 0);

		strValueName.Printf("%d_Protocol",i); 
		ini.GetValue("Sites", strValueName, info.m_dwProtocol, 0);

		strValueName.Printf("%d_Description",i); 
		ini.GetValue("Sites", strValueName, info.m_strDesc, "");
		
		strValueName.Printf("%d_EncryptType",i);
		ini.GetValue("Sites", strValueName, (int&)info.m_nEncryptType, 0);

		bool bValue;
		strValueName.Printf("%d_EncryptData",i);
		ini.GetValue("Sites", strValueName, bValue, 0);
		info.m_nEncryptData = bValue;

		strValueName.Printf("%d_ListLongPrintf",i);
		ini.GetValue("Sites", strValueName, bValue, 0);
		info.m_nListLongFormat = bValue;

		strValueName.Printf("%d_ListResolveLinks",i);
		ini.GetValue("Sites", strValueName, bValue, 0);
		info.m_nListResolveLinks = bValue;
		
		strValueName.Printf("%d_ListShowHidden",i);
		ini.GetValue("Sites", strValueName, bValue, 0);
		info.m_nListShowHidden = bValue;
		
		strValueName.Printf("%d_ListCompleteTime",i);
		ini.GetValue("Sites", strValueName, bValue, 0);
		info.m_nListCompleteTime = bValue;

		//extended data -> position within the tree
		strValueName.Printf("%d_ID",i); 
		ini.GetValue("Sites", strValueName, info.m_nID, -1);
		
		strValueName.Printf("%d_IDP",i); 
		ini.GetValue("Sites", strValueName, info.m_nPID, -1);
		
		strValueName.Printf("%d_Expand",i); 
		ini.GetValue("Sites", strValueName, info.m_bExpanded, 0);
		
		strValueName.Printf("%d_IsFolder",i); 
		ini.GetValue("Sites", strValueName, info.m_bIsFolder, 0);

		if(!info.m_strTitle.IsEmpty())
		{
			push_back(info);
		}
		//TOFIX else trace?
	}

	return true;
}

bool CConnectionInfoList::SaveToIni(IniFile &ini)
{
	String strValueName;
	
	int nTotalCount = size();
	char szBuffer[10] = "";
	sprintf(szBuffer, "%d", nTotalCount);
	ini.SetValue("Sites", "count", szBuffer);

	CNodeInfo info;

	for(int i=0; i<nTotalCount; i++)
	{
		info = operator[](i);
		
		strValueName.Printf("%d_Title",i);
		ini.SetValue("Sites", strValueName, info.m_strTitle);

		if(!info.m_bIsFolder)
		{
			strValueName.Printf("%d_Host",i); 
			ini.SetValue("Sites", strValueName, info.m_strHost);
			
			strValueName.Printf("%d_User",i); 
			ini.SetValue("Sites", strValueName, info.m_strUser);
			
			strValueName.Printf("%d_Password",i); 
			ini.SetValue("Sites", strValueName, info.m_strPassword);
			
			strValueName.Printf("%d_Account",i); 
			ini.SetValue("Sites", strValueName, info.m_strAccount);
			
			strValueName.Printf("%d_RemoteDir",i); 
			ini.SetValue("Sites", strValueName, info.m_strRemoteDir);
			
			strValueName.Printf("%d_LocalDir",i); 
			ini.SetValue("Sites", strValueName, info.m_strLocalDir);
			
			strValueName.Printf("%d_Port",i);
			sprintf(szBuffer, "%d", info.m_uPort);
			ini.SetValue("Sites", strValueName, szBuffer);
			
			strValueName.Printf("%d_Anonymous",i);
			sprintf(szBuffer, "%d", info.m_bAnonymous);
			ini.SetValue("Sites", strValueName, szBuffer);
			
			strValueName.Printf("%d_Passive",i);
			sprintf(szBuffer, "%d", info.m_bPassiveMode);
			ini.SetValue("Sites", strValueName, szBuffer);
			
			strValueName.Printf("%d_UseFirewall",i);
			sprintf(szBuffer, "%d", info.m_bUseFirewall);
			ini.SetValue("Sites", strValueName, szBuffer);
			
			strValueName.Printf("%d_Protocol",i);
			sprintf(szBuffer, "%d", info.m_dwProtocol);
			ini.SetValue("Sites", strValueName, szBuffer);
			
			strValueName.Printf("%d_Description",i);
			ini.SetValue("Sites", strValueName, info.m_strLocalDir);
			
			strValueName.Printf("%d_EncryptType",i);
			sprintf(szBuffer, "%d", info.m_nEncryptType);
			ini.SetValue("Sites", strValueName, szBuffer);
			
			strValueName.Printf("%d_EncryptData",i);
			sprintf(szBuffer, "%d", info.m_nEncryptData);
			ini.SetValue("Sites", strValueName, szBuffer);
 
			strValueName.Printf("%d_ListLongPrintf",i);
			sprintf(szBuffer, "%d", info.m_nListLongFormat);
			ini.SetValue("Sites", strValueName, szBuffer);
			
			strValueName.Printf("%d_ListResolveLinks",i);
			sprintf(szBuffer, "%d", info.m_nListResolveLinks);
			ini.SetValue("Sites", strValueName, szBuffer);
			
			strValueName.Printf("%d_ListShowHidden",i);
			sprintf(szBuffer, "%d", info.m_nListShowHidden);
			ini.SetValue("Sites", strValueName, szBuffer);

			strValueName.Printf("%d_ListCompleteTime",i);
			sprintf(szBuffer, "%d", info.m_nListCompleteTime);
			ini.SetValue("Sites", strValueName, szBuffer);
		}

		//extended data -> positions within the tree
/*
		strValueName.Printf("%d_ID",i);
		sprintf(szBuffer, "%d", info.m_nID);
		ini.SetValue("Sites", strValueName, szBuffer);
		
		strValueName.Printf("%d_IDP",i);
		sprintf(szBuffer, "%d", info.m_nPID);
		ini.SetValue("Sites", strValueName, szBuffer);
		
		strValueName.Printf("%d_Expand",i);
		sprintf(szBuffer, "%d", info.m_bExpanded);
		ini.SetValue("Sites", strValueName, szBuffer);

		strValueName.Printf("%d_IsFolder",i);
		sprintf(szBuffer, "%d", info.m_bIsFolder);
		ini.SetValue("Sites", strValueName, szBuffer);
*/
	}

	return true;
}

bool CConnectionInfoList::LoadDefault()
{
	String strPath;
	CalcIniFile(strPath);
	return LoadFromIni(strPath.c_str());
}

void CConnectionInfoList::SaveDefault()
{
	String strPath;
	CalcIniFile(strPath);
	SaveToIni(strPath.c_str());
}

void CConnectionInfoList::Swap(int i, int j)
{
	CNodeInfo info;
	info = operator[](i);
	operator[](i) = operator[](j);
	operator[](j) = info;
}

void CConnectionInfoList::CalcIniFile(String &strPath)
{
	strPath = PathName::GetIniDirectory();
	strPath += "/sites.enc.ini";
}
