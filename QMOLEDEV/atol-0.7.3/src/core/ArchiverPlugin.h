////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: class for working with archiver plugins
////////////////////////////////////////////////////////////////////////////

#ifndef ARCHIVERPLUGIN_H_
#define ARCHIVERPLUGIN_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "dll.h"
#include "plugin_defs.h" //shared file
#include "String.h"

//TOFIX separate file with interface description?
typedef const char*     (*tGetExtensions)();
typedef unsigned int    (*tGetArchiverCaps)(const char *szExt);
typedef int             (*tOpenArchive)(const char *);
typedef bool            (*tCloseArchive)(int dwArchID);
typedef void            (*tInitEntryEnum)(int dwArchID);
typedef int             (*tGetNextEntry)(int dwArchID, tArchiveEntry *pInfo);
typedef void            (*tProcessMultiple)(int dwArchID, int nOperation);
typedef void            (*tEndProcessMulti)(int dwArchID);
typedef bool            (*tPackFile)(int dwArchID, const char *szFile, const char *SubPath, const char*szDestName);
typedef bool            (*tUnpackFile)(int dwArchID, const char *szEntry, const char *szDest);
typedef bool            (*tDeleteEntry)(int dwArchID, const char *szEntry);
typedef void            (*tConfigurationDlg)(long hWndParent, long hDllInstance);
typedef void            (*tSetChangeVolProc)(int dwArchID, tChangeVolProc pChangeVolProc1);
typedef void            (*tSetProcessDataProc)(int dwArchID, tProcessDataProc pProcessDataProc, unsigned int dwUser);
typedef void            (*tSetPasswordProc)(tPasswordProc pPwdProc, unsigned int dwUser);
typedef bool            (*tMakeDir)(int dwArchID, const char *szDir);

class ArchiverPlugin : public CDll
{
public:
    ArchiverPlugin();
    ~ArchiverPlugin();

    ArchiverPlugin(const ArchiverPlugin &that){ operator =(that); };
    void operator =(const ArchiverPlugin &that);

    bool Load(const char *szFile);
    void Unload();

//TOFIX inline wrapper functs?
//TOFIX protected: ?
    tGetExtensions      m_pfnGetExtensions;
    tGetArchiverCaps    m_pfnGetArchiverCaps;
    tOpenArchive        m_pfnOpenArchive;
    tCloseArchive       m_pfnCloseArchive;
    tInitEntryEnum      m_pfnInitEntryEnum;
    tGetNextEntry       m_pfnGetNextEntry;
    tProcessMultiple    m_pfnProcessMultiple;
    tEndProcessMulti    m_pfnEndProcessMulti;
    tPackFile           m_pfnPackFile;
    tUnpackFile         m_pfnUnpackFile;
    tDeleteEntry        m_pfnDeleteEntry;
    tConfigurationDlg   m_pfnConfigurationDlg;
    tSetChangeVolProc   m_pfnSetChangeVolProc;
    tSetProcessDataProc m_pfnSetProcessDataProc;
    tSetPasswordProc    m_pfnSetPasswordProc;
    tMakeDir            m_pfnMakeDir;

    String m_strExtensions;    //supported extensions list
};

#endif // ARCHIVERPLUGIN_H_

