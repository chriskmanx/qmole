////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implements archive description object and the list of such objects
////////////////////////////////////////////////////////////////////////////

#include "ArchiveInfo.h"

CArchiveInfo::CArchiveInfo()
{
	m_bUnpacking	= false;
}

CArchiveInfo::~CArchiveInfo()
{
}

COpenArchivesList::COpenArchivesList()
{

}
		
COpenArchivesList::~COpenArchivesList()
{
	Clear();
}

void COpenArchivesList::Clear()
{
	//delete alocated archive descriptions
	tOpenArchiveMap::iterator It = tOpenArchiveMap::begin();
	
	while(It != tOpenArchiveMap::end())
	{
		delete (*It).second;
		tOpenArchiveMap::erase(It);
		It = tOpenArchiveMap::begin();
	}
}

int  COpenArchivesList::Add(CArchiveInfo *pInfo)
{
	//check arguments
	if(NULL == pInfo)
		return 0;	//failure
	
	//STEP 1: check if the pointer exists in the map
	//		  (return existing ID if so)
	tOpenArchiveMap::iterator It = tOpenArchiveMap::begin();
	
	while(It != tOpenArchiveMap::end())
	{
		if(pInfo == (*It).second)
			return (*It).first;

		It++;
	}

	//STEP 2: find new unique (free) ID
	int  dwID = tOpenArchiveMap::size();
	if(dwID < 1)
		dwID = 1;
	
	while (IdExists(dwID))
	{
		dwID ++;
		if(dwID>1000)
			return 0;	//search for unique ID failed
	}

	//STEP 3: insert new ID, pInfo pair int the map
	tOpenArchiveMap::insert(
		tOpenArchiveMap::value_type(dwID, pInfo));
	
	return dwID;
}

//does give ID (Key) exists in the map
bool COpenArchivesList::IdExists(int  dwID)
{
	tOpenArchiveMap::iterator It = tOpenArchiveMap::find(dwID);
	return (It != tOpenArchiveMap::end());
}

CArchiveInfo *COpenArchivesList::Find(int  dwID)
{
	tOpenArchiveMap::iterator It = tOpenArchiveMap::find(dwID);
	
	if(It != tOpenArchiveMap::end())
		return (*It).second;

	return NULL;
}

bool COpenArchivesList::Remove(int  dwID)
{
	tOpenArchiveMap::iterator It = tOpenArchiveMap::find(dwID);
	
	if(It != tOpenArchiveMap::end())
	{
		delete (*It).second;
		tOpenArchiveMap::erase(It);
		
		return true;
	}

	return false;	//not found
}

void COpenArchivesList::RemoveAll()
{
	tOpenArchiveMap::iterator It = tOpenArchiveMap::begin();
	
	while(It != tOpenArchiveMap::end())
	{
		delete (*It).second;
		tOpenArchiveMap::erase(It);
		
		It ++;
	}
}
