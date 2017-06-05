////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: 
////////////////////////////////////////////////////////////////////////////

/*
===============================================================================================
Name    : LibTar
===============================================================================================
Subject : Handling of "tar" files
===============================================================================================
Author  : Stefan Heymann
          Eschenweg 3
          72076 Tbingen
          GERMANY

E-Mail:   stefan@destructor.de
Web:      www.destructor.de

===============================================================================================
CTarArchive Usage
-----------------
- Choose a constructor
- Make an instance of CTarArchive                  TA := CTarArchive.Create (Filename);
- Scan through the archive                         TA.Reset;
                                                   WHILE TA.FindNext (DirRec) DO BEGIN
- Evaluate the DirRec for each file                  ListBox.Items.Add (DirRec.Name);
- Read out the current file                          TA.ReadFile (DestFilename);
  (You can ommit this if you want to
  read in the directory only)                        END;
- You're done                                      TA.Free;


CTarWriter Usage
----------------
- Choose a constructor
- Make an instance of CTarWriter                   TW := CTarWriter.Create ('my.tar');
- Add a file to the tar archive                    TW.AddFile ('foobar.txt');
- Add a string as a file                           TW.AddString (SL.Text, 'joe.txt', Now);
- Destroy TarWriter instance                       TW.Free;
- Now your tar file is ready.


Source, Legals ("Licence")
--------------------------
The official site to get this code is http://www.destructor.de/

Usage and Distribution of this Source Code is ruled by the
"Destructor.de Source code Licence" (DSL) which comes with this file or
can be downloaded at http://www.destructor.de/

IN SHORT: Usage and distribution of this source code is free.
          You use it completely on your own risk.

Postcardware
------------
If you like this code, please send a postcard of your city to my above address.
===============================================================================================
!!!  All parts of this code which are not finished or known to be buggy
     are marked with three exclamation marks
===============================================================================================
Date        Author Changes
-----------------------------------------------------------------------------------------------
2001-04-26  HeySt  0.0.1 Start
2001-04-28  HeySt  1.0.0 First Release
2001-06-19  HeySt  2.0.0 Finished CTarWriter
2001-09-06  HeySt  2.0.1 Bugfix in CTarArchive.FindNext: FBytesToGo must sometimes be 0
2001-10-25  HeySt  2.0.2 Introduced the ClearDirRec procedure
2001-11-13  HeySt  2.0.3 Bugfix: Take out ClearDirRec call from WriteTarHeader
                         Bug Reported by Tony BenBrahim
2001-12-25  HeySt  2.0.4 WriteTarHeader: Fill Rec with zero bytes before filling it
*/

#ifdef _WIN32
 #include <windows.h>
#endif
#include <string>

#ifdef _WIN32
	#define INT64 __int64
#else
	#define INT64 long long
#endif

// --- File Access Permissions
typedef enum {
	tpAll				= 0x0111,
	tpReadByOwner		= 0x0100,
	tpWriteByOwner		= 0x0080,
	tpExecuteByOwner	= 0x0040,
	tpReadByGroup		= 0x0020,
	tpWriteByGroup		= 0x0010,
	tpExecuteByGroup	= 0x0008,
	tpReadByOther		= 0x0004,
	tpWriteByOther		= 0x0002,
	tpExecuteByOther	= 0x0001
} CTarPermission;

typedef unsigned long CTarPermissions; // SET OF CTarPermission;

// --- Type of File
typedef enum {
	ftNormal		= 0x0001,   // Regular file
	ftLink			= 0x0002,   // Link to another, previously archived, file (LinkName)
	ftSymbolicLink	= 0x0004,   // Symbolic link to another file              (LinkName)
	ftCharacter		= 0x0008,   // Character special files
	ftBlock			= 0x0010,   // Block special files
	ftDirectory		= 0x0020,   // Directory entry. Size is zero (unlimited) or max. number of bytes
	ftFifo			= 0x0040,   // FIFO special file. No data stored in the archive.
	ftContiguous	= 0x0080,   // Contiguous file, if supported by OS
	ftDumpDir		= 0x0100,   // List of files
	ftMultiVolume	= 0x0200,   // Multi-volume file part
	ftVolumeHeader	= 0x0400	// Volume header. Can appear only as first record in the archive
}TFileType;

// --- Mode
typedef enum {tmSetUid, tmSetGid, tmSaveText} CTarMode;
typedef unsigned long CTarModes; //SET OF CTarMode;

// --- Record for a Directory Entry
//     Adjust the ClearDirRec procedure when this record changes!
typedef struct{
	std::string		Name;	         // File path and name
	INT64		Size;            // File size in Bytes
	time_t		DateTime;        // Last modification date and time
	CTarPermissions Permissions; // Access permissions
	TFileType	FileType;        // Type of file
	std::string		LinkName;        // Name of linked file (for ftLink, ftSymbolicLink)
	int			UID;			 // User ID
	int			GID;             // Group ID
	std::string		UserName;        // User name
	std::string		GroupName;       // Group name
	bool		ChecksumOK;      // Checksum was OK
	CTarModes	Mode;			 // Mode
	std::string		Magic;           // Contents of the "Magic" field
	int			MajorDevNo;      // Major Device No. for ftCharacter and ftBlock
	int			MinorDevNo;      // Minor Device No. for ftCharacter and ftBlock
	INT64		FilePos;         // Position in TAR file
}CTarRec;

  // --- The TAR Archive CLASS
class CTarArchive
{
public:
	CTarArchive();
	~CTarArchive();

	bool Open(const char *szFile, unsigned short wMode = 1); //TOFIX
	void Close();

	void Reset();					// Reset File Pointer
	bool FindNext (CTarRec &Rec);	// Reads next Directory Info Record. FALSE if EOF reached

	int ReadFile (char *Buffer, int nLen);   // Reads file data for last Directory Record

	void GetFilePos (INT64 &Current);  // Current File Position
	void SetFilePos (INT64 NewPos);    // Set new Current File Position

public:
	bool WriteFile(char *Buffer, int nLen);
	void AddFile (const char *TarFilename, time_t FileDateGmt, int nFileSize);
	void AddString (const char *Contents, const char *TarFilename, time_t FileDateGmt);
	void AddDir          (const char *Dirname, time_t DateGmt, INT64 MaxDirSize = 0);
	void AddSymbolicLink (const char *Filename, const char *Linkname, time_t DateGmt);
	void AddLink         (const char *Filename, const char *Linkname, time_t DateGmt);
	void AddVolumeHeader (const char *VolumeId, time_t DateGmt);
	void Finalize();
	

	CTarPermissions GetPermissions(){ return m_Permissions; }
	void			SetPermissions(CTarPermissions value){m_Permissions = value; }
	int		GetUID(){ return m_nUID; }
	void	SetUID(int value){ m_nUID = value; }
	int		GetGID(){ return m_nGID; }
	void	SetGID(int value){ m_nGID = value; }
	std::string	GetUserName(){ return m_UserName; }
	void	SetUserName(std::string value){ m_UserName = value; }
	std::string	GetGroupName(){ return m_GroupName; }
	void	SetGroupName(std::string value){ m_GroupName = value; }
	CTarModes GetMode(){ return m_Mode; }
	void      SetMode(CTarModes value){ m_Mode = value; }
	std::string	GetMagic(){ return m_Magic; }
	void	SetMagic(std::string value){ m_Magic = value; }

	//added from CTarArchiver class
protected:
	bool	m_bFinalized;
				   // --- Used at the next "Add" method call: ---
	CTarPermissions m_Permissions;   // Access permissions
	int	m_nUID;           // User ID
	int	m_nGID;           // Group ID
	std::string m_UserName;   // User name
	std::string m_GroupName;  // Group name
	CTarModes m_Mode;    // Mode
	std::string m_Magic;      // Contents of the "Magic" field

	FILE   *m_Stream;		// Internal Stream
	INT64 m_nBytesToGo;   // Bytes until the next Header Record
};
