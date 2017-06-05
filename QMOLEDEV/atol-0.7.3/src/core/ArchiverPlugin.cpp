////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: class for working with archiver plugins
////////////////////////////////////////////////////////////////////////////

#include "ArchiverPlugin.h"
#include "debug.h"
int PasswordProc(char *FileName, int Size, int dwUser);

ArchiverPlugin::ArchiverPlugin()
{
    m_pfnGetExtensions       = NULL;
    m_pfnGetArchiverCaps     = NULL;
    m_pfnOpenArchive         = NULL;
    m_pfnCloseArchive        = NULL;
    m_pfnInitEntryEnum       = NULL;
    m_pfnGetNextEntry        = NULL;
    m_pfnProcessMultiple     = NULL;
    m_pfnEndProcessMulti     = NULL;
    m_pfnPackFile            = NULL;
    m_pfnUnpackFile          = NULL;
    m_pfnDeleteEntry         = NULL;
    m_pfnConfigurationDlg    = NULL;
    m_pfnSetChangeVolProc    = NULL;
    m_pfnSetProcessDataProc  = NULL;
    m_pfnSetPasswordProc     = NULL;
    m_pfnMakeDir             = NULL;
}

ArchiverPlugin::~ArchiverPlugin()
{
    //Unload();
}

void ArchiverPlugin::operator =(const ArchiverPlugin &that)
{
    CDll::operator =(that);

    m_pfnGetExtensions       = that.m_pfnGetExtensions;
    m_pfnGetArchiverCaps     = that.m_pfnGetArchiverCaps;
    m_pfnOpenArchive         = that.m_pfnOpenArchive;
    m_pfnCloseArchive        = that.m_pfnCloseArchive;
    m_pfnInitEntryEnum       = that.m_pfnInitEntryEnum;
    m_pfnGetNextEntry        = that.m_pfnGetNextEntry;
    m_pfnProcessMultiple     = that.m_pfnProcessMultiple;
    m_pfnEndProcessMulti     = that.m_pfnEndProcessMulti;
    m_pfnPackFile            = that.m_pfnPackFile;
    m_pfnUnpackFile          = that.m_pfnUnpackFile;
    m_pfnDeleteEntry         = that.m_pfnDeleteEntry;
    m_pfnConfigurationDlg    = that.m_pfnConfigurationDlg;
    m_pfnSetChangeVolProc    = that.m_pfnSetChangeVolProc;
    m_pfnSetProcessDataProc  = that.m_pfnSetProcessDataProc;
    m_pfnSetPasswordProc     = that.m_pfnSetPasswordProc;
    m_pfnMakeDir             = that.m_pfnMakeDir;

    m_strExtensions = that.m_strExtensions;
}

bool ArchiverPlugin::Load(const char *szFile)
{
    if(CDll::Load(szFile))
    {
         //get function pointers
        m_pfnGetExtensions      = (tGetExtensions)      GetFunction("GetExtensions");
        m_pfnGetArchiverCaps    = (tGetArchiverCaps)    GetFunction("GetArchiverCaps");
        m_pfnOpenArchive        = (tOpenArchive)        GetFunction("OpenArchive");
        m_pfnCloseArchive       = (tCloseArchive)       GetFunction("CloseArchive");
        m_pfnInitEntryEnum      = (tInitEntryEnum)      GetFunction("InitEntryEnum");
        m_pfnGetNextEntry       = (tGetNextEntry)       GetFunction("GetNextEntry");
        m_pfnProcessMultiple    = (tProcessMultiple)    GetFunction("ProcessMultiple");
        m_pfnEndProcessMulti    = (tEndProcessMulti)    GetFunction("EndProcessMulti");
        m_pfnPackFile           = (tPackFile)           GetFunction("PackFile");
        m_pfnUnpackFile         = (tUnpackFile)         GetFunction("UnpackFile");
        m_pfnDeleteEntry        = (tDeleteEntry)        GetFunction("DeleteEntry");
        m_pfnConfigurationDlg   = (tConfigurationDlg)   GetFunction("ConfigurationDlg");
        m_pfnSetChangeVolProc   = (tSetChangeVolProc)   GetFunction("SetChangeVolProc");
        m_pfnSetProcessDataProc = (tSetProcessDataProc) GetFunction("SetProcessDataProc");
        m_pfnSetPasswordProc    = (tSetPasswordProc)    GetFunction("SetPasswordProc");
        m_pfnMakeDir            = (tMakeDir)            GetFunction("MakeDir");

        //check minimal requirements for read-only archive plugin
        if( NULL == m_pfnGetExtensions      ||
            NULL == m_pfnOpenArchive        ||
            NULL == m_pfnCloseArchive       ||
            NULL == m_pfnGetArchiverCaps    ||
            NULL == m_pfnInitEntryEnum      ||
            NULL == m_pfnGetNextEntry       ||
            NULL == m_pfnUnpackFile )
        {
		    TRACE("ArchiverPlugin::Load - missing exported methods in plugin: %s\n", szFile);

            Unload();
            return false;
        }

        //cache archive extensions list
        m_strExtensions = m_pfnGetExtensions();

		TRACE("ArchiverPlugin::Load - Loaded: %s, Supported extensions: %s\n", szFile, m_strExtensions.c_str());

        if(NULL != m_pfnSetPasswordProc)
            m_pfnSetPasswordProc(PasswordProc, 0);

        return true;    //OK!
    }

	TRACE("ArchiverPlugin::Load - failed to load plugin: %s\n", szFile);
    return false;
}

void ArchiverPlugin::Unload()
{
    CDll::Free();
}

int PasswordProc(char *FileName, int Size, int dwUser)
{
    //ASSERT(NULL != FileName);
    //ASSERT(Size > 0);

    //start password dialog and return typed password
/*TOFIX
    CPasswordDlg dlg;
    dlg.m_bSingleMode = true;

    if(IDOK == dlg.DoModal())
    {
        strncpy(FileName, dlg.m_strPwd, Size);
        FileName[Size-1] = '\0';
    }
*/
    return 1;
}

