////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: archive plugin interface and flags
////////////////////////////////////////////////////////////////////////////

#ifndef _PLUGIN_DEFS_H
#define _PLUGIN_DEFS_H

#include <time.h>

//this DLL is supposed to be used only dynamically loaded
 #ifdef __GNUWIN32__
  #define ARCHIVE_API extern "C" __declspec(dllexport)
 #else
  #ifdef _WIN32
   #define ARCHIVE_API extern "C" __declspec(dllexport)
  #else
   #define ARCHIVE_API extern "C" 
  #endif
 #endif

//portable file attribute flags
#include "file_attrib.h"

// returned by GetArchiverCaps
#define PK_CAPS_NEW        1    // archiver can create new archives
#define PK_CAPS_MODIFY     2    // can modify exisiting archives
#define PK_CAPS_MULTIPLE   4    // archive can contain multiple files
#define PK_CAPS_DELETE     8    // archiver can delete files
#define PK_CAPS_OPTIONS   16    // archiver has options dialog
#define PK_CAPS_REAL	  32    // we can destroy original file after compressing it (not an virtual archive)

//  archiver independent archive entry description

//#pragma pack(push, 1)
typedef struct {
    char    szPath[260];
    int     nPackSize;
    int     nUnpSize;
    bool    bDir;
    int	    dwAttribs;
    int	    dwFileCRC;
    time_t  tmModified;
} tArchiveEntry;
//#pragma pack(pop)

// Definition of callback functions called by the DLL

typedef int (*tChangeVolProc)(char *ArcName, int Mode);
    // Ask to swap disk for multi-volume archive

typedef int (*tProcessDataProc)(const char *FileName, int Size, int dwUser);
    // Notify that data is processed - used for progress dialog

typedef int (*tPasswordProc)(char *szPwdBuf, int Size, int dwUser);
    // ask password callback

#endif //_PLUGIN_DEFS_H


