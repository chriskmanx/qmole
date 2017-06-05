////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Handles working with archiver plugin .DLL/.so objects
////////////////////////////////////////////////////////////////////////////

#ifndef PLUGINMANAGER_H_
#define PLUGINMANAGER_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#if _MSC_VER > 1000
  #pragma warning(disable:4786)
#endif

#include "ArchiverPlugin.h"
#include <map>
#include <vector>

class PluginManager : public std::vector<ArchiverPlugin> 
{
public:
    PluginManager();
    virtual ~PluginManager();

    void LoadPlugins(const char *szDir);
    void FreePlugins();     //empty plugin list
    void MapExtensions();

    int GetCount(){ return size(); }
    int CountPackCapableFormats(bool bMultipleFiles = true);

    ArchiverPlugin *FindArchiver(const char *szFileExt);

protected:
    std::map<String, int> m_mapExt;
};

#endif // PLUGINMANAGER_H_

