////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: wxutil implementation
////////////////////////////////////////////////////////////////////////////

#include "util.h"
#include <sys/stat.h>
#include "PathName.h"
#include "System.h"
#include "debug.h"
#include "../support.h"
#include <errno.h>

#ifdef _WIN32
  #include <windows.h>
  #include <io.h>
  #define access _access
#else
  #include <unistd.h>
  #include <ctype.h>
#endif

extern String g_strTerminal;
int gtkMessageBox(const char *szText, int nButtons = GTK_BUTTONS_OK, int nIcon = GTK_MESSAGE_INFO);

void OpenCommandPrompt(const String &strDir, void *nData)
{
#ifdef _WIN32

    SHELLEXECUTEINFO si;
    si.cbSize       = sizeof(si);
    si.fMask        = 0;
    si.hwnd         = (HWND)nData;  //m_hWnd
    si.lpVerb       = "open";
    //si.lpFile     = (SHELL::IsWinNT())? "cmd.exe" : "command.com";  //TOFIX
    si.lpFile       = "cmd.exe";
    si.lpParameters = NULL;
    si.lpDirectory  = (const char *)strDir;
    si.nShow        = SW_SHOWNORMAL;

    ShellExecuteEx(&si);

    if(si.hProcess){
        //TOFIX set your title to this new window -> get main window from process
        //EnumThreadWindows((DWORD)(si.hProcess), EnumCmdPrompt, 0);
        //EnumWindows(EnumCmdPrompt, (DWORD)(si.hProcess));
        //CloseHandle(si.hProcess);
    }

#else
	//TOFIX
	chdir(strDir);
	if(0 == access(g_strTerminal.c_str(), 00))
		System::RunCommand(g_strTerminal);
	else
		gtkMessageBox(_("Failed to find terminal program!"));
#endif
}

// Format size in bytes as string 
// (dynamicaly choose unit in bytes/kB/MB/GB based on amount)
String FormatSizeUnits(INT64 nBytes)
{
    String strResult;

    if(nBytes < 1024L)
    {
        //less than 1 kB
        strResult.Printf(_("%u bytes"), (unsigned int)(int)nBytes);
    }
    else
    {
        //1 kB or larger
        double dValue = (int)(nBytes/1024);	// convert to kB

        if(dValue < 1024)
        {
            //less than 1MB, stay with kB (one decimal place for kB)
            strResult.Printf("%.1f kB", dValue);
        }
        else
        {
            //1 MB or larger
            dValue /= 1024; // convert to MB

            if(dValue < 1024)
            {
                //less than 1GB, stay with MB (two decimal places for MB)
                strResult.Printf("%.2f MB", dValue);
            }
            else
            {
                dValue /= 1024; // convert to GB

                //if GB use two decimal places
                strResult.Printf("%.2f GB", dValue);
            }
        }
    }

    return strResult;
}

String FormatTime(UINT64 uSeconds)
{
    String strResult;

    //TOFIX multilang support
    //TOFIX double

    if(uSeconds < 60)
    {
        strResult.Printf(_("%u sec"), (unsigned int)uSeconds);
    }
    else
    {
        uSeconds /= 60; // convert to minutes

        if(uSeconds < 60)
        {
            strResult.Printf(_("%u min"), (unsigned int)uSeconds);
        }
        else
        {
            uSeconds /= 60; // convert to hours

            if(uSeconds < 24)
            {
                strResult.Printf(_("%u hours"), (unsigned int)uSeconds);
            }
            else
            {
                strResult.Printf(_("%u days"), (unsigned int)uSeconds/24);
            }
        }
    }

    return strResult;
}

// does file name matches wildcard pattern (*.txt)
// supports wildcards * and ?
bool fnmatch(const char* pattern, const char *string, bool caseSensitive, bool bDOS)
{
    //DOS version assumes '.' always exists
    String strPtrn(pattern);
    String strName(string);

    const char *pos = pattern;
    if(bDOS)
    {
        pos = strrchr(pos, '.');
        if(pos)
        {
            String strExtPtrn(pos+1);

            //extract extension
            String strExt(string);
            int nPos = strExt.Find(".", true);
            if(nPos >= 0){
                strName = strExt.Left(nPos);    //before dot
                strExt  = strExt.Right(strExt.Length()-nPos-1); //after dot
            }

            if(!WildMatch(strExtPtrn.c_str(), strExt.c_str(), caseSensitive))
                return false;

            strPtrn = strPtrn.Left(pos-pattern);
        }
    }

    //match rest of the name
    return WildMatch(strPtrn.c_str(), strName.c_str(), caseSensitive);
}

/**
    \internal
    \author Jack Handy

    Borrowed from http://www.codeproject.com/string/wildcmp.asp.
    Modified by Joshua Jensen.
**/
bool WildMatch( const char* pattern, const char *string, bool caseSensitive )
{
    // Handle all the letters of the pattern and the string.
    while ( *string != 0  &&  *pattern != '*' )
    {
        if ( *pattern != '?' )
        {
            if ( caseSensitive )
            {
                if ( *pattern != *string )
                    return false;
            }
            else
            {
                if ( toupper( *pattern ) != toupper( *string ) )
                    return false;
            }
        }

        pattern++;
        string++;
    }

    const char* mp = NULL;
    const char* cp = NULL;
    while ( *string != 0 )
    {
        if (*pattern == '*')
        {
            // It's a match if the wildcard is at the end.
            if ( *++pattern == 0 )
            {
                return true;
            }

            mp = pattern;
            cp = string + 1;
        }
        else
        {
            if ( caseSensitive )
            {
                if ( *pattern == *string  ||  *pattern == '?' )
                {
                    pattern++;
                    string++;
                }
                else 
                {
                    pattern = mp;
                    string = cp++;
                }
            }
            else
            {
                if ( toupper( *pattern ) == toupper( *string )  ||  *pattern == '?' )
                {
                    pattern++;
                    string++;
                }
                else
                {
                    pattern = mp;
                    string = cp++;
                }
            }
        }
    }

    // Collapse remaining wildcards.
    while ( *pattern == '*' )
        pattern++;

    return !*pattern;
}

void Tokenize(String strData, std::vector<String> &lstTokenized, char szSeparator)
{
    //clear old list contents
    lstTokenized.clear();

    if(strData.IsEmpty())
        return;

    //tokenize string
    String strToken;
    int nStart = 0;

    while(true)
    {
        int nPos = strData.find(szSeparator, nStart);
        if(nPos > 0) //add new token into the list
            strToken = strData.Mid(nStart, nPos-nStart);
        else         //no match, add rest of the string into the list
            strToken = strData.Mid(nStart, strData.Length()-nStart);

        if(!strToken.IsEmpty())
            lstTokenized.push_back(strToken);

        if(nPos > 0)
            nStart = nPos + 1;    //search next token
        else
            break;                //no more tokens
    }
}

int StrCommonPrefixLen(const char *szPath1, const char *szPath2)
{
	int nLen = 0;

	if(szPath1 && szPath2)
		while(*szPath1 && *szPath1 == *szPath2){
			szPath1++; szPath2++; nLen++;
		}

	return nLen;
}

//exactly like access except it doesn't follow the links
// returns 0 if mode is available
int laccess(const char *szFile, int nMode)
{
#ifdef _WIN32
	return access(szFile, nMode);
#else
	struct stat64 st;
	if(0 != lstat64(szFile, &st)){
		TRACE("laccess: Could not stat file %s (errno:%d)!\n", szFile, errno);
		return -1;
	}

	uid_t uid = getuid();
	gid_t gid = getgid();

	if(uid == st.st_uid)
	{
		TRACE("laccess %s: Testing by user rights!\n", szFile);

		//same user for our app and file, match by user rights
		if((nMode & R_OK) && !(st.st_mode & S_IRUSR))	return 1;
		if((nMode & W_OK) && !(st.st_mode & S_IWUSR))	return 1;
		if((nMode & X_OK) && !(st.st_mode & S_IXUSR))	return 1;
	}
	else if(gid == st.st_gid)
	{
		TRACE("laccess %s: Testing by group rights!\n", szFile);

		//same group for our app and file, match by group rights
		if((nMode & R_OK) && !(st.st_mode & S_IRGRP))	return 1;
		if((nMode & W_OK) && !(st.st_mode & S_IWGRP))	return 1;
		if((nMode & X_OK) && !(st.st_mode & S_IXGRP))	return 1;
	}
	else
	{
		TRACE("laccess %s: Testing by other rights!\n", szFile);

		// match by "other" rights
		if((nMode & R_OK) && !(st.st_mode & S_IROTH))	return 1;
		if((nMode & W_OK) && !(st.st_mode & S_IWOTH))	return 1;
		if((nMode & X_OK) && !(st.st_mode & S_IXOTH))	return 1;
	}

	return 0; // OK
#endif
}
