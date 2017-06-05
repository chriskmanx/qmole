////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This is a list of file system bookmarks (collection of the paths you 
//       like to have quick access to).
//         Bookmarks are being stored inside ini file storage.
////////////////////////////////////////////////////////////////////////////

#ifndef BROWSEBOOKMARKLIST_H__
#define BROWSEBOOKMARKLIST_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <vector>
#include "String.h"

class BrowseBookmarkList  
{
    class BrowseBookmark
    {
    public:
        BrowseBookmark(){};
        BrowseBookmark(const BrowseBookmark &that){ operator =(that); }
        void operator = (const BrowseBookmark &that)
        {
            if(this != &that){   // don't copy yourself
                m_strTitle    = that.m_strTitle;
                m_strPath    = that.m_strPath;

            }
        }

        bool IsValid(){ return (!m_strTitle.IsEmpty() && !m_strPath.IsEmpty()); }

        //needed for searching key list
        bool operator == (const BrowseBookmark &that) { return m_strTitle == that.m_strTitle; };

        String m_strTitle;
        String m_strPath;
    };

public:
    BrowseBookmarkList();
    virtual ~BrowseBookmarkList();

    bool Load();
    bool Save();

    void Clear();
    bool Insert(const char *szTitle, const char *szPath);

    int    GetCount()             const { return m_lstBookmarks.size(); }
    String GetBookPath (int nIdx) const { return m_lstBookmarks[nIdx].m_strPath; }
    String GetBookTitle(int nIdx) const { return m_lstBookmarks[nIdx].m_strTitle; }

    void SetBookPath (int nIdx, String strTxt) { m_lstBookmarks[nIdx].m_strPath = strTxt; }
    void SetBookTitle(int nIdx, String strTxt) { m_lstBookmarks[nIdx].m_strTitle = strTxt; }

    int  FindBookByTitle(const char *szTitle);
    int  FindBookByPath(const char *szPath);
    void Remove(int nIdx);

protected:
    String CalcFileName();

    std::vector<BrowseBookmark>    m_lstBookmarks;
};

#endif // BROWSEBOOKMARKLIST_H__

