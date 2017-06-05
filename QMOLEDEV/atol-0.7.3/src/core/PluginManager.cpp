////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Handles working with archiver plugin .DLL/.so objects
////////////////////////////////////////////////////////////////////////////

#include "PluginManager.h"
#include "PathName.h"
#include "System.h"
#include "util.h"
#include "debug.h"

//NOTE: unique plugin file extension ".atp"
#define PLUGIN_EXT ".atp"
static bool OnEnumFile(const std::string &path, void *data);

PluginManager::PluginManager()
{
}

PluginManager::~PluginManager()
{
	FreePlugins();    //just in case
}

void PluginManager::LoadPlugins(const char *szDir)
{
	TRACE("PluginManager::LoadPlugins - Scan directory: %s\n", szDir);

	//scan directory for files (to load all plugins found)
	std::vector<String> lstFiles;
	System::EnumDirectory(szDir, OnEnumFile, &lstFiles, ENUM_LST_FILES);

	String strDir(szDir);
	
	for(unsigned int i=0; i<lstFiles.size(); i++)
	{
		//TRACE("Plugin candidate: %s\n", szFile);
		String strPath = strDir;
		strPath += '/';
		strPath += lstFiles[i];

		ArchiverPlugin lib;
		if(lib.Load(strPath))
		{
			//TRACE("Plugin %s loaded\n", szFile);
			push_back(lib);    //TOFIX this makes object copy (maybe keep pointers)
			//lib.Detach();
		}
	}

	MapExtensions();
}

//unload plugins and empty plugin list
void PluginManager::FreePlugins()
{
	for(unsigned int i=0; i<size(); i++)
		operator [](i).Unload();
	clear();
}

void PluginManager::MapExtensions()
{
	m_mapExt.clear();

	std::vector<String> lstTokens;
	for(unsigned int i=0; i<size(); i++)
	{
		Tokenize(operator[](i).m_strExtensions, lstTokens, ';');

		for(unsigned int j=0; j<lstTokens.size(); j++)
		{
			//TOFIX find first?; protect from multiple plugins for same format
			m_mapExt[lstTokens[j]] = i;
		}
	}
}

ArchiverPlugin *PluginManager::FindArchiver(const char *szFileExt)
{
	//check arguments
	if(NULL == szFileExt || 0 == strlen(szFileExt))
		return NULL;

	String strExt(szFileExt);

#ifdef _WIN32
	strExt.Lower();	// case insensitive comparison on Win32
#endif

	std::map<String, int>::iterator It = m_mapExt.find(strExt);
	if(It != m_mapExt.end())
	{
		std::vector<ArchiverPlugin>::iterator ItPlg = begin() + (*It).second;
		return &(*ItPlg);
	}

	return NULL;    //not found
}

int PluginManager::CountPackCapableFormats(bool bMultipleFiles)
{
	int nCount = 0;
	std::vector<String> lstTokenized;
	String strExtensions;

	int nMax = GetCount();
	for(int i=0; i<nMax; i++)
	{
		strExtensions = operator[](i).m_strExtensions;
		Tokenize(strExtensions, lstTokenized, ';');
		for(unsigned int j=0; j<lstTokenized.size(); j++)
		{
			int nCaps = operator[](i).m_pfnGetArchiverCaps(lstTokenized[j]);

			//see if plugin capabilities support creating new archive of this type
			//if multiple files selected for compression check caps for storing multiple files in single archive
			if(PK_CAPS_NEW & nCaps)    
				if(!bMultipleFiles || (bMultipleFiles && (PK_CAPS_MULTIPLE & nCaps)))
					nCount ++;
		}
	}

	return nCount;
}


bool OnEnumFile(const std::string &path, void *data)
{
	std::vector<String> *pLstFiles = (std::vector<String> *)data;
	if(pLstFiles)
	{
	        //match plugin extension (case sensitive)
	        if(PathName::GetExt(path.c_str()) == PLUGIN_EXT)
			pLstFiles->push_back(PathName::GetBaseName(path.c_str()));
	}
	return 1;	//keep enumerating
}
