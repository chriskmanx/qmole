////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Bookmark list implementation
////////////////////////////////////////////////////////////////////////////

#include "BrowseBookmarkList.h"
#include "IniFile.h"
#include "System.h"

static char g_szSection[] = "Bookmarks";

BrowseBookmarkList::BrowseBookmarkList()
{
}

BrowseBookmarkList::~BrowseBookmarkList()
{
}

void BrowseBookmarkList::Clear()
{
    m_lstBookmarks.clear();
}

String BrowseBookmarkList::CalcFileName()
{
    String strFile = System::GetHomeDir().c_str();
    strFile += "/.atol/bookmarks.ini";
    return strFile;
}

bool BrowseBookmarkList::Load()
{
    String strFile = CalcFileName();

    IniFile ini;
    if(!ini.Load(strFile.c_str()))
        return false;

    int nTotalCount = 0;
    ini.GetValue("Bookmarks", "Count", nTotalCount, 0);

    
    String strKey;

    for(int i=0; i<nTotalCount; i++)
    {
        BrowseBookmark item;

        strKey.Printf("%d_Title",i); 
        ini.GetValue(g_szSection, strKey.c_str(), item.m_strTitle, "");
        strKey.Printf("%d_Path",i); 
        ini.GetValue(g_szSection, strKey.c_str(), item.m_strPath, "");

        if(item.IsValid())
            m_lstBookmarks.push_back(item);
    }

    return true;
}

bool BrowseBookmarkList::Save()
{
    String strFile = CalcFileName();

    IniFile ini;
    ini.SetPath(strFile);

    int nTotalCount = m_lstBookmarks.size();
    ini.SetValue(g_szSection, "Count", nTotalCount);
    
    String strKey;

    for(int i=0; i<nTotalCount; i++)
    {
        strKey.Printf("%d_Title",i); 
        ini.SetValue(g_szSection, strKey.c_str(), m_lstBookmarks[i].m_strTitle);
        strKey.Printf("%d_Path",i); 
        ini.SetValue(g_szSection, strKey.c_str(), m_lstBookmarks[i].m_strPath);
    }

    if(!ini.Save())
        return false;

    return true;
}

int BrowseBookmarkList::FindBookByTitle(const char *szTitle)
{
    for(unsigned int i=0; i<m_lstBookmarks.size(); i++)
        if(    m_lstBookmarks[i].m_strTitle == szTitle )
            return i;

    return -1;    //not found
}

int BrowseBookmarkList::FindBookByPath(const char *szPath)
{
    for(unsigned int i=0; i<m_lstBookmarks.size(); i++)
        if( m_lstBookmarks[i].m_strPath == szPath )
            return i;

    return -1;    //not found
}

void BrowseBookmarkList::Remove(int nIdx)
{
    //ASSERT(0 <= nIdx && nIdx < size());
    m_lstBookmarks.erase(m_lstBookmarks.begin()+nIdx);
}

bool BrowseBookmarkList::Insert(const char *szTitle, const char *szPath)
{
    if(FindBookByTitle(szTitle) >= 0)
        return false;    //can not overwrite existing bookmark

    BrowseBookmark item;
    item.m_strTitle = szTitle;
    item.m_strPath = szPath;
    m_lstBookmarks.push_back(item);
    return true;
}


