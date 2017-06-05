////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: PIDL class handles working with Windows OS PIDL's
//         (these are memory objects for encoding paths of either real or virtual files/directories) 
////////////////////////////////////////////////////////////////////////////

#ifndef __PIDL_H
#define __PIDL_H

#include <shlobj.h>
#include <shlwapi.h>
#include <exdisp.h>
#ifndef __GNUWIN32__                    // there is not comdef
#include <comdef.h>
#endif
#include "../String.h"

class PIDL
{
public:

#ifdef __GNUWIN32__
    static inline LPSHELLFOLDER GetDesktopFolder(void){ return m_sfDesktop; }
    static inline LPMALLOC          GetMalloc(){ return m_piMalloc; };
#else
    static inline IShellFolderPtr    GetDesktopFolder(){ return m_sfDesktop; };
    static inline IMallocPtr        GetMalloc(){ return m_piMalloc; };
#endif

    static UINT GetItemCount(LPITEMIDLIST pidl);
        // count items in pidl list

    static UINT GetPIDLSize(LPITEMIDLIST pidl);
        // returns the total number of bytes in an ITEMIDLIST

    static UINT GetSize1(LPITEMIDLIST pidl);
        // return size in bytes ocupied by pidl (without counting terminator ID)
    
    static bool CmpEqual(LPITEMIDLIST pidl1, LPITEMIDLIST pidl2);
        // check if the two lists are equal

    static inline LPSHITEMID GetFirstID(LPITEMIDLIST pidl){ return (LPSHITEMID)pidl; }
        // returns a pointer to the first item in the list

    static inline void GetNextID(LPSHITEMID& pid){ (LPBYTE &)pid += pid->cb; }
        // points to the next item in the list

    static LPITEMIDLIST GetNextItem(LPITEMIDLIST pidl);
        //TOFIX slicno gornjem

    static LPSHITEMID GetAt(LPITEMIDLIST pidl, int nIdx);
        // get item ID at given list position

    static LPITEMIDLIST MergeIDLists(LPITEMIDLIST pidl1, LPITEMIDLIST pidl2);
        // merge two PIDLs

    static LPITEMIDLIST    Merge(LPITEMIDLIST pidl, ... );
        // merge multiple PIDLs

    static LPITEMIDLIST    CreatePIDL(int cbSize);
        // allocate memory for the pidl using the system allocator
    
    static void Free(void *pv);
        // free memory allocated for pidl

    static LPITEMIDLIST CopyPIDL(LPITEMIDLIST pidl);
        // copy exisiting pidl

    static LPSHELLFOLDER GetFolderPtr(LPITEMIDLIST pidl);
        // takes absolute pidl for a folder and returns IShellFolder pointer for that pidl

    static BOOL GetDisplayName(LPSHELLFOLDER piFolder, LPITEMIDLIST pidl, 
                         DWORD dwFlags, LPTSTR pszName, UINT cchMax);

    static BOOL GetDisplayName(LPSHELLFOLDER piFolder, LPITEMIDLIST pidl, String &sName, DWORD dwFlags);

    static void GetDisplayName(LPITEMIDLIST lpifq, String &sDisplayName);
        //

    static int GetItemIcon(LPITEMIDLIST lpi, UINT uFlags);

    static void GetTypeName(LPITEMIDLIST lpi, String &sTypeName);
        
    static bool PathFromPIDL(LPCITEMIDLIST pidl, String &strResult);

    static bool GetAttributes(LPCITEMIDLIST pidl, DWORD &dwAttributes);

    static bool GetAttributes(LPCSTR szPath, DWORD &dwAttributes);

    static bool GetAttributes(LPSHELLFOLDER piFolder, LPCITEMIDLIST pidl, DWORD &dwAttributes);

    static LPITEMIDLIST GetFromPath(LPCSTR pszFile);
        // returns absolute pidl from given file path

    static LPITEMIDLIST GetFromPath(LPSHELLFOLDER lpsf, LPCTSTR pszPath);

    static LPITEMIDLIST GetFromSpecialFolder(int nSpecialFolder);

    static LPITEMIDLIST GetAbsPidl(LPSHELLFOLDER piParentFolder, LPITEMIDLIST pidl);
        // return absolute pidl from relative pidl and its parent folder

    static LPITEMIDLIST GetFromParentFolder(LPSHELLFOLDER pParentFolder, LPCSTR pszFile);
        // get relative PIDL from path name relative to given folder

    static LPITEMIDLIST GetParent(LPITEMIDLIST pidl);
        // get parent pidl (absolute) for given absolute pidl

    static BOOL GetSpecialFolderPath(int nShellFolderID, String &strResult);

    static HICON GetSpecFolderIcon(int nSpecialFolder);

    static BOOL SHPidlToPathEx(LPCITEMIDLIST pidl, String &sPath, LPSHELLFOLDER pFolder, DWORD dwFlags);
    
    static void BufferFromSTRRET(LPCITEMIDLIST lpidl, LPSTRRET lpStrt, char* buffer, int nLen);

protected:
    static LPSHELLFOLDER    m_sfDesktop;    // desktop object
    static LPMALLOC         m_piMalloc;        // system allocator

    // initializer (used for automatic initialization)
    static struct pidl_initializer {
        pidl_initializer();
        ~pidl_initializer();
    } m_initializer;
    friend struct pidl_initializer;
};

#endif  // __PIDL_H
