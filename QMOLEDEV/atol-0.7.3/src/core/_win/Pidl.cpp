////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: PIDL class implementation
////////////////////////////////////////////////////////////////////////////

#ifndef __GNUWIN32__                    // there is not atlbase
 #include <atlbase.h>
#endif
#include "PIDL.h"

#define CB_SIZE  (sizeof(USHORT))  // termination item size

LPSHELLFOLDER PIDL::m_sfDesktop  = NULL; // cached destkop folder
LPMALLOC      PIDL::m_piMalloc     = NULL; // cached system allocator

PIDL::pidl_initializer PIDL::m_initializer; // automatic init. obj

////////////////////////////////////////////////////////////////////
// Initialization object
////////////////////////////////////////////////////////////////////

// pidl_initializer is used to initialize the static data.  The 
// constructor and destructor are automatically called for us when
// the program starts/ends.

PIDL::pidl_initializer::pidl_initializer()
{
    SHGetDesktopFolder(&m_sfDesktop); // cache d'top folder obj ptr 
    SHGetMalloc(&m_piMalloc);         // cache sys allocator obj ptr
}

PIDL::pidl_initializer::~pidl_initializer()
{
    if(NULL != m_sfDesktop)
        m_sfDesktop->Release();
    if(NULL != m_piMalloc)
        m_piMalloc->Release();
}

////////////////////////////////////////////////////////////////////
// PIDL Implementation
////////////////////////////////////////////////////////////////////

LPITEMIDLIST PIDL::GetNextItem(LPITEMIDLIST pidl)
{   
    if(!pidl || 0 == pidl->mkid.cb)
        return NULL;
    return (LPITEMIDLIST)((LPBYTE)pidl + pidl->mkid.cb);
}

// =====================================================================
// Function:     CreatePIDL()
// Synopsis:    Uses the shell allocator to allocate a block of memory
//                of a given size, then returns a pointer to this memory
// Parameters:  unsigned short size -- bytes of memory to allocate
// =====================================================================
LPITEMIDLIST PIDL::CreatePIDL(int cbSize)
{
    LPITEMIDLIST pidl = NULL;
    if(m_piMalloc){
        pidl = (LPITEMIDLIST)m_piMalloc->Alloc(cbSize);
        if (pidl)
            ZeroMemory(pidl, cbSize);
    }
    return (pidl);
}

// =====================================================================
// Function:     CopyPIDL()
// Synopsis:    Deep copies a PIDL, then returns a pointer to the copy
// Parameters:  LPITEMIDLIST pidl -- PIDL to copy
// =====================================================================
LPITEMIDLIST PIDL::CopyPIDL(LPITEMIDLIST pidl)
{
    LPITEMIDLIST pidlCopy = NULL;
    
    if (pidl){
        UINT uSize = GetPIDLSize(pidl);
        pidlCopy = CreatePIDL(uSize);
        if(pidlCopy)
            CopyMemory((LPVOID)pidlCopy, (LPVOID)pidl, uSize);
    }
    return pidlCopy;
}

void PIDL::Free(void *pv)
{
    if (m_piMalloc && pv)
        m_piMalloc->Free(pv);
}

// =====================================================================
// Function:     GetPIDLSize()
// Synopsis:    Returns the number of bytes occupied by a PIDL
// Parameters:  LPITEMIDLIST pidl -- PIDL to query size of
// =====================================================================
UINT PIDL::GetPIDLSize(LPITEMIDLIST pidl)
{
    //ASSERT(NULL != pidl);

    UINT cbTotal        = 0;
    LPSHITEMID  piid    = GetFirstID(pidl);
    
    if (piid) {
        do {
            cbTotal += piid->cb;
            GetNextID(piid);
        } while (piid->cb);
        cbTotal += CB_SIZE; // count the terminator
    }

    return cbTotal;
}

UINT PIDL::GetSize1(LPITEMIDLIST pidl)
{
    //ASSERT(NULL != pidl);

    UINT cbTotal        = 0;
    LPSHITEMID  piid    = GetFirstID(pidl);
    
    if (piid) {
        do {
            cbTotal += piid->cb;
            GetNextID(piid);
        } while (piid->cb);
    }
    
    return cbTotal;
}

// =====================================================================
// Function:     GetItemCount()
// Synopsis:    Returns the number of item identifiers in a PIDL
// Parameters:  LPITEMIDLIST lpidl -- PIDL to enumerate
// =====================================================================
UINT PIDL::GetItemCount(LPITEMIDLIST pidl)
{
    //ASSERT(NULL != pidl);

    UINT nTotal            = 0;
    LPSHITEMID  piid    = GetFirstID(pidl);
    
    while (piid && piid->cb){
        nTotal ++;
        GetNextID(piid);
    }
    
    return nTotal;
}

LPSHITEMID PIDL::GetAt(LPITEMIDLIST pidl, int nIdx)
{
    //ASSERT(NULL != pidl);

    int nCount = 0;
    LPSHITEMID  piid = GetFirstID(pidl);

    if (piid) {
        while (piid->cb && nCount < nIdx){
            nCount ++;
            GetNextID(piid);
        }
    }
    
    return (nCount == nIdx) ? piid : NULL;
}

bool PIDL::CmpEqual(LPITEMIDLIST pidl1, LPITEMIDLIST pidl2)
{
    if( pidl1 == pidl2 ) return true;
    if( !pidl1 || !pidl2 ) return false; 
    if( GetPIDLSize(pidl1) != GetPIDLSize(pidl2) ) return false;
    return (0 == memcmp(pidl1, pidl2, GetPIDLSize(pidl1)));
}

// =====================================================================
// Function:     MergeIDLists()
// Synopsis:    Creates a new PIDL that contains a concatenation of
//                two pidl, then returns a pointer to this new PIDL
// Parameters:  LPITEMIDLIST pidl1, pidl2 -- PIDLs to merge
// =====================================================================
LPITEMIDLIST PIDL::MergeIDLists(LPITEMIDLIST pidl1, LPITEMIDLIST pidl2)
{
    UINT cb1, cb2;
    cb1 = cb2 = 0;

    //
    // Pidl1 can possibly be NULL if it points to the desktop.  Since we only
    // need a single NULL terminator, we remove the extra 2 bytes from the
    // size of the first ITEMIDLIST.
    //
    if (pidl1)    cb1 = GetPIDLSize(pidl1);
    if (cb1)    cb1 = cb1 - CB_SIZE;
    if (pidl1)    cb2 = GetPIDLSize(pidl2);
    if (cb2)    cb2 = cb2 - CB_SIZE;

    int total_size = cb1 + cb2 + CB_SIZE;

    //
    // Create a new ITEMIDLIST that is the size of both pidl1 and pidl2, then
    // copy pidl1 and pidl2 to the new list.
    //
    LPITEMIDLIST pidlNew = CreatePIDL(cb1 + cb2 + CB_SIZE);
    if (pidlNew)
    {
        CopyMemory(pidlNew, pidl1, cb1);
        CopyMemory(((LPBYTE)pidlNew) + cb1, pidl2, cb2);
    }

    return (pidlNew);
}

LPITEMIDLIST PIDL::Merge(LPITEMIDLIST pidl,...) 
{
    va_list marker;
    int nSize = GetSize1(pidl) + CB_SIZE;
    LPITEMIDLIST p;

    // count total pidl size
    va_start(marker,pidl);
    while( p = va_arg(marker, LPITEMIDLIST) )
        nSize += GetSize1(p);
    va_end(marker);

    // allocate and merge pidls
    LPITEMIDLIST pidlNew = CreatePIDL(nSize); 
    if(pidlNew == NULL) 
        return NULL; 

    va_start(marker,pidl);
    CopyMemory(((LPBYTE)pidlNew), pidl, nSize = GetSize1(pidl)); 
    while( p = va_arg(marker, LPITEMIDLIST) ) {
           CopyMemory(((LPBYTE)pidlNew) + nSize, p, GetSize1(p)); 
           nSize += p->mkid.cb;
    }
    va_end(marker);
    *((USHORT *)(((LPBYTE) pidlNew) + nSize)) = 0;
    return pidlNew; 
}

LPSHELLFOLDER PIDL::GetFolderPtr(LPITEMIDLIST pidl)
{
    if (pidl == NULL || pidl->mkid.cb == 0)
        return m_sfDesktop;
    LPSHELLFOLDER pFolder=NULL;
    if (FAILED(m_sfDesktop->BindToObject(pidl, 0, IID_IShellFolder,(LPVOID*)&pFolder)))
        return NULL;
    return pFolder;
}

//////////////////////////////////////////////////////////////////////////////
//
//   FUNCTION: GetIcon(LPITEMIDLIST lpi, UINT uFlags)
//
//   PURPOSE:  Gets the index for the current icon.  Index is index into system
//             image list.
//
// PARAMETERS:
//   lpi    - Fully qualified item id list for current item.
//   uFlags - Flags for SHGetFileInfo()
//
// RETURN VALUE:
//   Icon index for current item.
///////////////////////////////////////////////////////////////////////////////
int PIDL::GetItemIcon(LPITEMIDLIST lpi, UINT uFlags)
{
    if(!lpi)
        return 0;

    SHFILEINFO    sfi;
    ZeroMemory(&sfi,sizeof(sfi));
    if (uFlags == 0)
        uFlags |= (SHGFI_SYSICONINDEX | SHGFI_SMALLICON);
    uFlags |= SHGFI_PIDL;
    SHGetFileInfo((LPCTSTR)lpi, 0, &sfi, sizeof(SHFILEINFO), uFlags);
    return sfi.iIcon;
}

void PIDL::GetTypeName(LPITEMIDLIST lpi, String &sTypeName)
{
    if(!lpi)
        return;

   SHFILEINFO    sfi;
   ZeroMemory(&sfi,sizeof(sfi));
   UINT uFlags = SHGFI_PIDL | SHGFI_TYPENAME;
   SHGetFileInfo((LPCTSTR)lpi, 0, &sfi, sizeof(SHFILEINFO), uFlags);
   sTypeName = sfi.szTypeName;
}

void PIDL::GetDisplayName(LPITEMIDLIST lpifq, String &sDisplayName)
{
   SHFILEINFO    sfi;
   ZeroMemory(&sfi,sizeof(sfi));
   UINT uFlags = SHGFI_PIDL | SHGFI_DISPLAYNAME;
   SHGetFileInfo((LPCTSTR)lpifq, 0, &sfi, sizeof(SHFILEINFO), uFlags);
   sDisplayName = sfi.szDisplayName;
}

//  FUNCTION:   Pidl_GetDisplayName 
//
//  PURPOSE:    Returns the display name for the item pointed to by pidl.  The
//                function assumes the pidl is relative to piFolder.  If piFolder
//                is NULL, the function assumes the item is fully qualified.
//
//  PARAMETERS:
//      piFolder - Pointer to the IShellFolder for the folder containing the 
//                   item.
//        pidl     - Pointer to an ITEMIDLIST relative to piFolder that we want
//                   the display name for.
//        dwFlags  - Flags to pass to ISF::GetDisplayNameOf().
//        pszName  - Pointer to the string where the display name is returned.
//        cchMax   - Maximum number of characters in pszName.
//
//  RETURN VALUE:
//      Returns TRUE if successful, FALSE otherwise.
//
BOOL PIDL::GetDisplayName(LPSHELLFOLDER piFolder, LPITEMIDLIST pidl, 
                         DWORD dwFlags, LPTSTR pszName, UINT cchMax)
{
    BOOL fSuccess = TRUE;
    BOOL fDesktop = FALSE;

    //
    // Check to see if a parent folder was specified.  If not, get a pointer
    // to the desktop folder.
    //
    if (NULL == piFolder)    
    {
        piFolder = GetDesktopFolder();
        if (!piFolder)
            return (FALSE);
    }

    //
    // Get the display name from the folder.  Then do any conversions necessary
    // depending on the type of string returned.
    //
    STRRET str;
    str.uType = STRRET_WSTR;

    if (NOERROR == piFolder->GetDisplayNameOf(pidl, dwFlags, &str))
    {
        switch (str.uType)
        {
            case STRRET_WSTR:
                WideCharToMultiByte(CP_ACP, 0, str.pOleStr, -1, pszName, 
                                    cchMax, NULL, NULL);
                m_piMalloc->Free(str.pOleStr);
                break;

            case STRRET_OFFSET:
                lstrcpyn(pszName, (LPTSTR) pidl + str.uOffset, cchMax);
                break;

            case STRRET_CSTR:
                lstrcpyn(pszName, (LPTSTR) str.cStr, cchMax);
                break;

            default:
                fSuccess = FALSE;
                break;
        }
    }
    else
        fSuccess = FALSE;

    return (fSuccess);
}

BOOL PIDL::GetDisplayName(LPSHELLFOLDER piFolder, LPITEMIDLIST pidl, String &sName, DWORD dwFlags)
{
    STRRET str;
    str.uType = STRRET_WSTR;
    if (NOERROR != piFolder->GetDisplayNameOf(pidl, dwFlags, &str))
        return FALSE;

    switch (str.uType)
    {
    case STRRET_WSTR:
		{
			char szTemp[MAX_PATH]="";
			WideCharToMultiByte (CP_ACP, 0, str.pOleStr, -1,  szTemp, sizeof (szTemp), NULL, NULL);
			sName = szTemp;
			PIDL::GetMalloc()->Free(str.pOleStr);
		}
        break;
    case STRRET_CSTR:
        sName = str.cStr;
        break;
    case STRRET_OFFSET:
        sName = (char*)((LPBYTE)pidl+str.uOffset);
    }

    return TRUE;
}

LPITEMIDLIST PIDL::GetFromPath(LPCSTR pszPath)
{
    if(NULL == m_sfDesktop)
        return NULL;

    //
    // Now convert the path to a Unicode string
    // as required by ParseDisplayName
    //
    OLECHAR    olePath[_MAX_PATH] = L"";

#ifndef _UNICODE
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, pszPath, -1, 
        olePath, MAX_PATH);
#else
    lstrcpy(olePath,pszPath);
#endif

    
    LPITEMIDLIST  pidl = NULL;
    ULONG  chEaten;

    HRESULT hr = m_sfDesktop->ParseDisplayName(
                    NULL,NULL,olePath,&chEaten,&pidl,NULL);
    if( FAILED(hr) ) 
        return NULL;

    return pidl;
}

LPITEMIDLIST PIDL::GetFromSpecialFolder(int nSpecialFolder)
{
    LPITEMIDLIST pidl = NULL; 
    ::SHGetSpecialFolderLocation(NULL, nSpecialFolder, &pidl);
    return pidl;
}

LPITEMIDLIST PIDL::GetAbsPidl(LPSHELLFOLDER piParentFolder, LPITEMIDLIST pidl)
{
    //
    // Start by getting a pointer to the IShellFolder for the desktop since
    // all fully qualified pidls are paths from the desktop to the itemid.
    //
    if(NULL == GetDesktopFolder())
        return NULL;

    //
    // Get the name of the item for parsing.  We use this to get the fully
    // qualified pidl from the desktop folder.
    //
    String sPath;
    if(!SHPidlToPathEx(pidl, sPath, piParentFolder, SHGDN_FORPARSING))
        return NULL;

    //
    // Convert the path to a Unicode string which is required for the call
    // to IShellFolder::ParseDisplayName().
    //
    OLECHAR szOleChar[MAX_PATH];
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, sPath /*szBuffer*/, -1, szOleChar,
                        sizeof(szOleChar));

    //
    // Finally ask the desktop folder to give us a fully qualified pidl for
    // the item.
    //
    ULONG ulEaten, ulAttribs;
    LPITEMIDLIST pidlFQ;
    HRESULT hr = GetDesktopFolder()->ParseDisplayName(NULL, NULL, szOleChar, &ulEaten,
                                           &pidlFQ, &ulAttribs);
    
    if (FAILED(hr))    
        return (NULL);

    return (pidlFQ);
}    

//
//  FUNCTION:   Pidl_GetFromParentFolder 
//
//  PURPOSE:    This routine takes a Shell folder for the parent and the FileName in the folder
//                and converts that to a relative ITEMIDLIST.
//
//  PARAMETERS:
//      pParentFolder - Pointer to the IShellFolder for the folder containing the 
//                   fileName.
//      pszFile  - file name in the folder.
//
//  RETURN VALUE:
//      Returns a relative ITEMIDLIST, or NULL if an error occurs.
//
LPITEMIDLIST PIDL::GetFromParentFolder(LPSHELLFOLDER pParentFolder, LPCSTR pszFile)
{
    //ASSERT(NULL != pszFile && '\0' != pszFile[0]);

     OLECHAR olePath[MAX_PATH];
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, pszFile, -1, olePath,
                        sizeof(olePath));

    // 
    // Convert the path to an ITEMIDLIST
    //
    LPITEMIDLIST pidl;
    ULONG chEaten, dwAttributes;
    if(FAILED(pParentFolder->ParseDisplayName(NULL, NULL, olePath, &chEaten,
                                           &pidl, &dwAttributes)))
        return (NULL);
    else
        return (pidl);

}

HICON PIDL::GetSpecFolderIcon(int nSpecialFolder)
{
    LPITEMIDLIST pidl = GetFromSpecialFolder(nSpecialFolder);
    if(pidl)
    {
        SHFILEINFO sfi;
        ZeroMemory(&sfi,sizeof(sfi));
        UINT uFlags =  SHGFI_ICON|SHGFI_SMALLICON|SHGFI_PIDL;
        SHGetFileInfo((LPCTSTR)pidl, 0, &sfi, sizeof(SHFILEINFO), uFlags);
        PIDL::Free(pidl);

        return sfi.hIcon;
    }

    return NULL;
}

BOOL PIDL::SHPidlToPathEx(LPCITEMIDLIST pidl, String &sPath, LPSHELLFOLDER pFolder, DWORD dwFlags)
{
    if (pFolder == NULL)
        return FALSE;

    STRRET str;
    str.uType = STRRET_CSTR;
    if (SUCCEEDED(pFolder->GetDisplayNameOf(pidl, dwFlags, &str)))
    {
        char buffer[MAX_PATH];
        BufferFromSTRRET(pidl, &str, buffer, MAX_PATH);
        sPath = buffer;
        return TRUE;
    }

    return FALSE;
}

bool PIDL::GetAttributes(LPCITEMIDLIST pidl, DWORD &dwAttributes)
{
    //TOFIX return GetAttributes(m_sfDesktop, pidl, dwAttributes);
    if(SUCCEEDED(m_sfDesktop->GetAttributesOf(1, &pidl, &dwAttributes)))
        return true;
    return false;
}

bool PIDL::GetAttributes(LPSHELLFOLDER piFolder, LPCITEMIDLIST pidl, DWORD &dwAttributes)
{
    if(piFolder)
        if(SUCCEEDED(piFolder->GetAttributesOf(1, &pidl, &dwAttributes)))
            return true;
    return false;
}

//TOFIX izgleda da ovo ne radi najbolje
bool PIDL::GetAttributes(LPCSTR szPath, DWORD &dwAttributes)
{
    bool bRes = false;

    LPITEMIDLIST pidl = PIDL::GetFromPath(szPath);
    //ASSERT(pidl);
    if(pidl){
        bRes = GetAttributes(pidl, dwAttributes);
        Free(pidl);
    }

    return bRes;
}

BOOL PIDL::GetSpecialFolderPath(int nShellFolderID, String &strResult)
{
    // These systems support the new Chichago shell, get location from registry
    BOOL         result = FALSE;
    LPITEMIDLIST pidl   = NULL;

    // Get a PIDL to the special shell folder
    if(SUCCEEDED(SHGetSpecialFolderLocation(NULL,nShellFolderID,&pidl)))
    {
        // Convert the PIDL into a path
        result = PIDL::PathFromPIDL(pidl, strResult);
        PIDL::Free(pidl);
    }

    return result;
}

// =====================================================================
// Function:     PathFromPIDL()
// Synopsis:    Returns an string path of a PIDL
// Parameters:  LPITEMIDLIST pidl -- PIDL to return path for
// =====================================================================
bool PIDL::PathFromPIDL(LPCITEMIDLIST pidl, String &strResult)
{
    TCHAR szPath[MAX_PATH];
    if(pidl){
        if(SHGetPathFromIDList(pidl, szPath)){
            strResult = szPath;
            return true;
        }
    }
    return false;
}

//TOFIX more error handling (return NULL, Free() on fail)
LPITEMIDLIST PIDL::GetParent(LPITEMIDLIST pidl)
{
    //cut last chain from the absolute PIDL to get its parent PIDL
    LPITEMIDLIST pidlParent = PIDL::CopyPIDL(pidl);
    if(pidlParent){
        int nSize = PIDL::GetItemCount(pidlParent);
        if(nSize>1){
            LPSHITEMID pid = PIDL::GetAt(pidlParent, nSize-1);
            if(pid) pid->cb = 0;
        }
    }
    return pidlParent;
}

// =====================================================================
// Function:     PIDLFromPath()
// Synopsis:    Returns a PIDL from a string path
// Parameters:  LPSHELLFOLDER lpsf -- IShellFolder pointer for pidl
//              string path -- path to return PIDL for 
// =====================================================================
LPITEMIDLIST PIDL::GetFromPath(LPSHELLFOLDER lpsf, LPCTSTR pszPath)
{
    if(!pszPath || !lpsf)
        return NULL;
       //TOFIX? if (Path[Path.Length()] != '\\') Path = Path + "\\";

    LPITEMIDLIST pidl = NULL;

    OLECHAR    olePath[_MAX_PATH] = L"";
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED,
                            pszPath, -1, olePath, sizeof(olePath));

       ULONG  chEaten;
    if (SUCCEEDED(lpsf->ParseDisplayName(NULL, NULL, olePath,
                                         &chEaten, &pidl, NULL)))
    {
        return pidl;
    }
    return NULL;
}

// =====================================================================
// Function:     BufferFromSTRRET()
// Synopsis:    Fills a char* string from information in a STRRET
//              structure.  Called from DisplayNameFromPIDL().
// Parameters:  LPITEMIDLIST lpidl -- PIDL to which lpStrt corresponds
//              LPSTRRET lpStrt -- STRRET struct containing data
//              char* buffer -- filled with info from lpStrt
// =====================================================================
void PIDL::BufferFromSTRRET(LPCITEMIDLIST lpidl, LPSTRRET lpStrt, char* buffer, int nLen)
{
    //ASSERT(NULL != lpidl);
    //ASSERT(NULL != lpStrt);
    //ASSERT(NULL != buffer);

    memset(buffer, 0, nLen);
    switch (lpStrt->uType)
    {
        case STRRET_CSTR:
            lstrcpyn(buffer, lpStrt->cStr, nLen);
            break;
        case STRRET_OFFSET:
            lstrcpyn(buffer, (char*)(lpidl) + lpStrt->uOffset, nLen);
            break;
        case STRRET_WSTR:
#ifdef __GNUWIN32__                     // compiler warning
            WideCharToMultiByte(CP_ACP, (long unsigned int) NULL, lpStrt->pOleStr,
                                -1, buffer, nLen,
                                (const CHAR*) NULL, (BOOL*) NULL);
#else
            WideCharToMultiByte(CP_ACP, NULL, lpStrt->pOleStr,
                                -1, buffer, nLen,
                                NULL, NULL);
#endif
            break;
    }
}

