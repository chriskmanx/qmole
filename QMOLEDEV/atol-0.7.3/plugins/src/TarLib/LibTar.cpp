////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File:
////////////////////////////////////////////////////////////////////////////

#include "LibTar.h"
#ifdef _WIN32
 #include <time.h>
 #define stat _stat
#else
 #include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>

#ifndef min
 #define min(a,b) ((a)<(b))?(a):(b)
#endif

static const int RECORDSIZE = 512;
static const int NAMSIZ		= 100;
static const int TUNMLEN    =  32;
static const int TGNMLEN    =  32;
static const char *CHKBLANKS  = "        ";	//8 spaces

#pragma pack(push, 1)

typedef struct {
	char Name[NAMSIZ];
	char Mode[8];
	char UID[8];
	char GID[8];
	char Size[12];
	char MTime[12];
	char ChkSum[8];
	char LinkFlag;
	char LinkName[NAMSIZ];
	char Magic[8];
	char UName[TUNMLEN];
	char GName[TGNMLEN];
	char DevMajor[8];
	char DevMinor[8];
} CTarHeader;

#pragma pack(pop)

std::string PermissionString (CTarPermissions Permissions);
time_t FileTimeGMT    (const char *FileName);
void ClearDirRec        (CTarRec &Rec);

//===============================================================================================
//  IMPLEMENTATION
//===============================================================================================

time_t EncodeDate(int year, int month, int day)
{
	struct tm tms;
	memset(&tms, 0,  sizeof(tms));
	tms.tm_year = year;
	tms.tm_mon  = month;
	tms.tm_mday = day;

	return mktime(&tms);
}

int StreamSize(FILE *pFile)
{
	int nPos = ftell(pFile);		//store initial pos
	fseek(pFile, 0, SEEK_END);
	int nSize = ftell(pFile);
	fseek(pFile, nPos, SEEK_SET);	//restore initial pos
	return nSize;
}

std::string PermissionString (CTarPermissions Permissions)
{
	std::string Result;

	if( tpReadByOwner	 & Permissions ) Result = Result + 'r';	else Result = Result + '-';
	if( tpWriteByOwner   & Permissions ) Result = Result + 'w'; else Result = Result + '-';
	if( tpExecuteByOwner & Permissions ) Result = Result + 'x'; else Result = Result + '-';
	if( tpReadByGroup    & Permissions ) Result = Result + 'r'; else Result = Result + '-';
	if( tpWriteByGroup   & Permissions ) Result = Result + 'w'; else Result = Result + '-';
	if( tpExecuteByGroup & Permissions ) Result = Result + 'x'; else Result = Result + '-';
	if( tpReadByOther    & Permissions ) Result = Result + 'r'; else Result = Result + '-';
	if( tpWriteByOther   & Permissions ) Result = Result + 'w'; else Result = Result + '-';
	if( tpExecuteByOther & Permissions ) Result = Result + 'x'; else Result = Result + '-';
	
	return Result;
}

const char *Trim(const char *strText)
{
	while(*strText == ' ')
		strText ++;
	return strText;
}

// Returns the Date and Time of the last modification of the given File
// The Result is zero if the file could not be found
// The Result is given in UTC (GMT) time zone
time_t FileTimeGMT (const char *FileName)
{
	struct stat stbuff;
	if(0 == stat(FileName, &stbuff))
		return stbuff.st_mtime;

	return (time_t)0;
}

// This is included because a FillChar (DirRec, SizeOf (DirRec), 0)
// will destroy the long string pointers, leading to strange bugs
void ClearDirRec (CTarRec &Rec)
{
    Rec.Name        = "";
    Rec.Size        = 0;
    Rec.DateTime    = 0;
    Rec.Permissions = 0;
    Rec.FileType    = TFileType (0);
    Rec.LinkName    = "";
    Rec.UID         = 0;
    Rec.GID         = 0;
    Rec.UserName    = "";
    Rec.GroupName   = "";
    Rec.ChecksumOK  = false;
    Rec.Mode        = 0;
    Rec.Magic       = "";
    Rec.MajorDevNo  = 0;
    Rec.MinorDevNo  = 0;
    Rec.FilePos     = 0;
}

//===============================================================================================
// TAR format
//===============================================================================================

int ExtractNumber (char *P, int MaxLen)
{
	char S0[256];
	int Result;
	
	//lstrcpyn (S0, P, MaxLen);
	strncpy(S0, P, MaxLen);
	S0[MaxLen-1] = '\0';

	P = S0;
	while ((*P == ' ') & (*P != '\0'))
		P++;

	Result = 0;
	while ((*P != ' ') & (*P != '\0')) 
	{
		Result = (*P - '0') | (Result << 3);
		P++;
	}

	return Result;
}

INT64 ExtractNumber64 (char *P, int MaxLen)
{
	char S0[256];
	std::string Strg;
	INT64 Result;

	//lstrcpyn (S0, P, MaxLen);
	strncpy(S0, P, MaxLen);
	S0[MaxLen-1] = '\0';

	
	P = S0;
	while ((*P == ' ') & (*P != '\0'))
		P++;
	
	Result = 0;
	while ((*P != ' ') & (*P != '\0'))
	{
		Result = (*P - '0') | (Result << 3);
		P++;
	}

	return Result;
}

INT64 Records (INT64 Bytes)
{
	INT64 Result = Bytes / RECORDSIZE;
	if( Bytes % RECORDSIZE > 0 ) 
		Result ++;
	
	return Result;
}

// Makes a string of octal digits
// The string will always be "Len" characters long
void Octal (INT64 N, char *P, int Len)
{
	int i;
	
	for(i = Len-2; i>=0; i--)
	{
		*(P+i) = '0' + (char)(N & 0x07);
		N = N >> 3;
	}
	for(i = 0; i<=Len-3; i++)
	{
		if( *(P+i) == '0')  
			*(P+i) = ' ';
		else
			break;
	}
	*(P+Len-1) = ' ';
}

// Makes a string of octal digits
// The string will always be "Len" characters long
void Octal64 (INT64 N, char *P, int Len)
{
	int i;
	
	for(i = Len-2; i>= 0; i--)
	{
		*(P+i) = '0' + (char)(N & 0x07);
		N = N >> 3;
	}
	
	for(i = 0; i<Len-3; i++)
	{
		if( *(P+i) == '0')  
			*(P+i) = ' ';
		else break;
	}
	
	*(P+Len-1) = ' ';
}

void OctalN (int N, char *P, int Len)
{
	Octal (N, P, Len-1);
	*(P+Len-1) = '\0';
}

void WriteTarHeader (FILE *pFile, CTarRec &DirRec)
{
	//header consist of data + padding (total size = 512 bytes)
	char Rec[RECORDSIZE];
	CTarHeader *pTH = (CTarHeader *)&Rec[0];

	int Mode;
	time_t NullDate = 0;
	long CheckSum;
	unsigned int I;
	
	memset(pTH, 0, RECORDSIZE);
	//lstrcpyn(pTH->Name, DirRec.Name.c_str(), NAMSIZ);
	strncpy(pTH->Name, DirRec.Name.c_str(), NAMSIZ);
	pTH->Name[NAMSIZ-1] = '\0';


	Mode = 0;
	if( tmSaveText & DirRec.Mode )  Mode = Mode | 0x0200;
	if( tmSetGid   & DirRec.Mode )  Mode = Mode | 0x0400;
	if( tmSetUid   & DirRec.Mode )  Mode = Mode | 0x0800;
	if( tpReadByOwner    & DirRec.Permissions )  Mode = Mode | 0x0100;
	if( tpWriteByOwner   & DirRec.Permissions )  Mode = Mode | 0x0080;
	if( tpExecuteByOwner & DirRec.Permissions )  Mode = Mode | 0x0040;
	if( tpReadByGroup    & DirRec.Permissions )  Mode = Mode | 0x0020;
	if( tpWriteByGroup   & DirRec.Permissions )  Mode = Mode | 0x0010;
	if( tpExecuteByGroup & DirRec.Permissions )  Mode = Mode | 0x0008;
	if( tpReadByOther    & DirRec.Permissions )  Mode = Mode | 0x0004;
	if( tpWriteByOther   & DirRec.Permissions )  Mode = Mode | 0x0002;
	if( tpExecuteByOther & DirRec.Permissions )  Mode = Mode | 0x0001;
	OctalN (Mode, pTH->Mode, 8);
	OctalN (DirRec.UID, pTH->UID, 8);
	OctalN (DirRec.GID, pTH->GID, 8);
	Octal64 (DirRec.Size, pTH->Size, 12);
	NullDate = EncodeDate (1970, 1, 1);
	
	INT64 nDate1, nDate2;
	memcpy(&nDate1, &DirRec.DateTime, sizeof(INT64));
	memcpy(&nDate2, &NullDate, sizeof(INT64));

	if(DirRec.DateTime >= NullDate)
		Octal ((nDate1 - nDate2) * 86400, &pTH->MTime[0], 12);
	else
		Octal (nDate2 * 86400, &pTH->MTime[0], 12);
	
	switch (DirRec.FileType){
		case ftNormal       : pTH->LinkFlag = '0';	break;
		case ftLink         : pTH->LinkFlag = '1';	break;
		case ftSymbolicLink : pTH->LinkFlag = '2';	break;
		case ftCharacter    : pTH->LinkFlag = '3';	break;
		case ftBlock        : pTH->LinkFlag = '4';	break;
		case ftDirectory    : pTH->LinkFlag = '5';	break;
		case ftFifo         : pTH->LinkFlag = '6';	break;
		case ftContiguous   : pTH->LinkFlag = '7';	break;
		case ftDumpDir      : pTH->LinkFlag = 'D';	break;
		case ftMultiVolume  : pTH->LinkFlag = 'M';	break;
		case ftVolumeHeader : pTH->LinkFlag = 'V';	break;
	}

	//lstrcpyn (pTH->LinkName, DirRec.LinkName.c_str(), NAMSIZ);
	strncpy(pTH->LinkName, DirRec.LinkName.c_str(), NAMSIZ);
	pTH->LinkName[NAMSIZ-1] = '\0';

	//lstrcpyn (pTH->Magic, DirRec.Magic.c_str(), 8);
	strncpy(pTH->Magic, DirRec.Magic.c_str(), 8);
	pTH->Magic[7] = '\0';

	//lstrcpyn (pTH->UName, DirRec.UserName.c_str(), TUNMLEN);
	strncpy(pTH->UName, DirRec.UserName.c_str(), TUNMLEN);
	pTH->UName[TUNMLEN-1] = '\0';

	//lstrcpyn (pTH->GName, DirRec.GroupName.c_str(), TGNMLEN);
	strncpy(pTH->GName, DirRec.GroupName.c_str(), TGNMLEN);
	pTH->GName[TUNMLEN-1] = '\0';

	OctalN (DirRec.MajorDevNo, pTH->DevMajor, 8);
	OctalN (DirRec.MinorDevNo, pTH->DevMinor, 8);
	//lstrcpyn(pTH->ChkSum, CHKBLANKS, 8);
	strncpy(pTH->ChkSum, CHKBLANKS, 8);
	pTH->ChkSum[8-1] = '\0';
	
	CheckSum = 0;
	for(I = 0; I<sizeof(CTarHeader)-1; I++)
		CheckSum += Rec [I];
	OctalN (CheckSum, pTH->ChkSum, 8);
	
	fwrite (pTH, RECORDSIZE, 1, pFile);
}

//===============================================================================================
//CTarArchive
//===============================================================================================

CTarArchive::CTarArchive()
{
	m_Stream = NULL;

	//writer variables?
	m_bFinalized	= false;
	m_Permissions	= tpAll;
	m_nUID		    = 0;
	m_nGID			= 0;
	m_UserName		= "";
	m_GroupName		= "";
	m_Mode			= 0;
	m_Magic			= "ustar";
}

CTarArchive::~CTarArchive()
{
	//TOFIX this block only when in write mode
	if( !m_bFinalized )
	{
		//Finalize();
		m_bFinalized = true;
	}

	Close();
}

// Reset File Pointer
void CTarArchive::Reset()
{
	fseek(m_Stream, 0, SEEK_SET);
	m_nBytesToGo       = 0;
}

// Reads next Directory Info Record
// The Stream pointer must point to the first byte of the tar header
bool CTarArchive::FindNext (CTarRec &DirRec)
{
	//header consist of data + padding (total size = 512 bytes)
	char Rec[RECORDSIZE];
	CTarHeader *pTH = (CTarHeader *)&Rec[0];

	int CurFilePos;
	unsigned int I;
	unsigned short HeaderChkSum;
	long CheckSum;
	
	// --- Scan until next pointer
	if( m_nBytesToGo > 0 ) 
		fseek (m_Stream, (long)Records (m_nBytesToGo) * RECORDSIZE, SEEK_CUR);
	
	// --- EOF reached?
	CurFilePos = ftell(m_Stream);
	if( (CurFilePos + RECORDSIZE > StreamSize(m_Stream)) )  
		return false;

	fread(pTH, RECORDSIZE, 1, m_Stream);
	if( pTH->Name[0] == 0 )
		return false;
	
	ClearDirRec (DirRec);
	
	time_t ft = EncodeDate (1970, 1, 1);
	INT64 nDate=0;
	memcpy(&nDate, &ft, sizeof(ft));
	nDate += (ExtractNumber (pTH->MTime, 12) / 86400);
	memcpy(&ft, &nDate, sizeof(ft));

	DirRec.FilePos = CurFilePos;
	DirRec.Name = pTH->Name;
	DirRec.Size = ExtractNumber64 (pTH->Size, 12);
	DirRec.DateTime = ft;
	I = ExtractNumber (pTH->Mode, sizeof(pTH->Mode));
	DirRec.Permissions = 0;
	DirRec.Mode        = 0;
	if( (I & 0x0100) != 0 )  DirRec.Permissions |= tpReadByOwner;
	if( (I & 0x0080) != 0 )  DirRec.Permissions |= tpWriteByOwner;
	if( (I & 0x0040) != 0 )  DirRec.Permissions |= tpExecuteByOwner;
	if( (I & 0x0020) != 0 )  DirRec.Permissions |= tpReadByGroup;
	if( (I & 0x0010) != 0 )  DirRec.Permissions |= tpWriteByGroup;
	if( (I & 0x0008) != 0 )  DirRec.Permissions |= tpExecuteByGroup;
	if( (I & 0x0004) != 0 )  DirRec.Permissions |= tpReadByOther;
	if( (I & 0x0002) != 0 )  DirRec.Permissions |= tpWriteByOther;
	if( (I & 0x0001) != 0 )  DirRec.Permissions |= tpExecuteByOther;
	if( (I & 0x0200) != 0 )  DirRec.Mode  |= tmSaveText;
	if( (I & 0x0400) != 0 )  DirRec.Mode  |= tmSetGid;
	if( (I & 0x0800) != 0 )  DirRec.Mode  |= tmSetUid;
	
	switch (pTH->LinkFlag) {
	case '0':	DirRec.FileType = ftNormal;			break;
	case '1':	DirRec.FileType = ftLink;			break;
	case '2':	DirRec.FileType = ftSymbolicLink;	break;
	case '3':	DirRec.FileType = ftCharacter;		break;
	case '4':	DirRec.FileType = ftBlock;			break;
	case '5':	DirRec.FileType = ftDirectory;		break;
	case '6':	DirRec.FileType = ftFifo;			break;
	case '7':	DirRec.FileType = ftContiguous;		break;
	case 'D':	DirRec.FileType = ftDumpDir;		break;
	case 'M':	DirRec.FileType = ftMultiVolume;	break;
	case 'V':	DirRec.FileType = ftVolumeHeader;	break;
	default:	DirRec.FileType = ftNormal;			break;
	}
	
	DirRec.LinkName   = pTH->LinkName;
	DirRec.UID        = ExtractNumber (pTH->UID, sizeof(pTH->UID));
	DirRec.GID        = ExtractNumber (pTH->GID, sizeof(pTH->GID));
	DirRec.UserName   = pTH->UName;
	DirRec.GroupName  = pTH->GName;
	DirRec.Magic      = Trim (pTH->Magic);
	DirRec.MajorDevNo = ExtractNumber (pTH->DevMajor, sizeof(pTH->DevMajor));
	DirRec.MinorDevNo = ExtractNumber (pTH->DevMinor, sizeof(pTH->DevMinor));
	
	HeaderChkSum = ExtractNumber (pTH->ChkSum, sizeof(pTH->ChkSum));   // Calc Checksum
	CheckSum = 0;
	memcpy(pTH->ChkSum, CHKBLANKS, 8);
	for(I = 0; I<sizeof(CTarHeader)-1; I++)
		CheckSum += ((char *)pTH)[I];
	DirRec.ChecksumOK = ((unsigned short)(CheckSum) == (unsigned short)(HeaderChkSum));
	
	if( DirRec.FileType & (ftLink|ftSymbolicLink|ftDirectory|ftFifo|ftVolumeHeader))
		m_nBytesToGo = 0;
	else 
		m_nBytesToGo = DirRec.Size;
	
	return true;
}

// Reads file data for the last Directory Record
int CTarArchive::ReadFile (char *Buffer, int nLen)
{
	if(0 == m_nBytesToGo)  
		return 0;
	int nToRead = min((int)m_nBytesToGo, nLen);
	int nRead = fread(Buffer, 1, nToRead, m_Stream);
	if(nRead > 0)
		m_nBytesToGo -= nRead;

	return nRead;
}

// Returns the Current Position in the TAR stream
void CTarArchive::GetFilePos (INT64 &Current)
{
	Current = ftell(m_Stream);
}

// Set new Current File Position
void CTarArchive::SetFilePos (INT64 NewPos)
{
	if( NewPos < ftell(m_Stream) ) 
		fseek(m_Stream, (long)NewPos, SEEK_SET);
}

//creates file header, use WriteFile after this call to add file content
void CTarArchive::AddFile (const char *TarFilename, time_t FileDateGmt, int nFileSize)
{
	CTarRec DirRec;

	ClearDirRec (DirRec);
	DirRec.Name        = TarFilename;
	DirRec.Size        = nFileSize;
	DirRec.DateTime    = FileDateGmt;
	DirRec.Permissions = m_Permissions;
	DirRec.FileType    = ftNormal;
	DirRec.LinkName    = "";
	DirRec.UID         = m_nUID;
	DirRec.GID         = m_nGID;
	DirRec.UserName    = m_UserName;
	DirRec.GroupName   = m_GroupName;
	DirRec.ChecksumOK  = true;
	DirRec.Mode        = m_Mode;
	DirRec.Magic       = m_Magic;
	DirRec.MajorDevNo  = 0;
	DirRec.MinorDevNo  = 0;
	
	WriteTarHeader (m_Stream, DirRec);
}

bool CTarArchive::WriteFile(char *Buffer, int nLen)
{
	fwrite (Buffer, 1, nLen, m_Stream);
	return true;
}

void CTarArchive::AddDir (const char *Dirname, time_t DateGmt, INT64 MaxDirSize)
{
	CTarRec DirRec;
	
	ClearDirRec (DirRec);
	DirRec.Name        = Dirname;
	DirRec.Size        = MaxDirSize;
	DirRec.DateTime    = DateGmt;
	DirRec.Permissions = m_Permissions;
	DirRec.FileType    = ftDirectory;
	DirRec.LinkName    = "";
	DirRec.UID         = m_nUID;
	DirRec.GID         = m_nGID;
	DirRec.UserName    = m_UserName;
	DirRec.GroupName   = m_GroupName;
	DirRec.ChecksumOK  = true;
	DirRec.Mode        = m_Mode;
	DirRec.Magic       = m_Magic;
	DirRec.MajorDevNo  = 0;
	DirRec.MinorDevNo  = 0;
	
	WriteTarHeader (m_Stream, DirRec);
}

void CTarArchive::AddSymbolicLink (const char *Filename, const char *Linkname, time_t DateGmt)
{
	CTarRec DirRec;
	
	ClearDirRec (DirRec);
	DirRec.Name        = Filename;
	DirRec.Size        = 0;
	DirRec.DateTime    = DateGmt;
	DirRec.Permissions = m_Permissions;
	DirRec.FileType    = ftSymbolicLink;
	DirRec.LinkName    = Linkname;
	DirRec.UID         = m_nUID;
	DirRec.GID         = m_nGID;
	DirRec.UserName    = m_UserName;
	DirRec.GroupName   = m_GroupName;
	DirRec.ChecksumOK  = true;
	DirRec.Mode        = m_Mode;
	DirRec.Magic       = m_Magic;
	DirRec.MajorDevNo  = 0;
	DirRec.MinorDevNo  = 0;
	
	WriteTarHeader (m_Stream, DirRec);
}

void CTarArchive::AddLink (const char *Filename, const char *Linkname, time_t DateGmt)
{
	CTarRec DirRec;
	
	ClearDirRec (DirRec);
	DirRec.Name        = Filename;
	DirRec.Size        = 0;
	DirRec.DateTime    = DateGmt;
	DirRec.Permissions = m_Permissions;
	DirRec.FileType    = ftLink;
	DirRec.LinkName    = Linkname;
	DirRec.UID         = m_nUID;
	DirRec.GID         = m_nGID;
	DirRec.UserName    = m_UserName;
	DirRec.GroupName   = m_GroupName;
	DirRec.ChecksumOK  = true;
	DirRec.Mode        = m_Mode;
	DirRec.Magic       = m_Magic;
	DirRec.MajorDevNo  = 0;
	DirRec.MinorDevNo  = 0;
	
	WriteTarHeader (m_Stream, DirRec);
}

void CTarArchive::AddVolumeHeader (const char *VolumeId, time_t DateGmt)
{
	CTarRec DirRec;
	
	ClearDirRec (DirRec);
	DirRec.Name        = VolumeId;
	DirRec.Size        = 0;
	DirRec.DateTime    = DateGmt;
	DirRec.Permissions = m_Permissions;
	DirRec.FileType    = ftVolumeHeader;
	DirRec.LinkName    = "";
	DirRec.UID         = m_nUID;
	DirRec.GID         = m_nGID;
	DirRec.UserName    = m_UserName;
	DirRec.GroupName   = m_GroupName;
	DirRec.ChecksumOK  = true;
	DirRec.Mode        = m_Mode;
	DirRec.Magic       = m_Magic;
	DirRec.MajorDevNo  = 0;
	DirRec.MinorDevNo  = 0;
	
	WriteTarHeader (m_Stream, DirRec);
}

// Writes the End-Of-File Tag
// Data after this tag will be ignored
// The destructor calls this automatically if you didn't do it before
void CTarArchive::Finalize()
{
	if(m_Stream){
		//write empty file header to terminate the archive
		char Rec[RECORDSIZE];
		memset(Rec, 0, sizeof(Rec));
		fwrite(Rec, 1, RECORDSIZE, m_Stream);
		m_bFinalized = true;
	}
}

/// TOFIX my clean API

bool CTarArchive::Open(const char *szFile, unsigned short wMode)
{
	m_Stream = fopen(szFile, "r+b"); //read + write binary access
	return (NULL != m_Stream);
}

void CTarArchive::Close()
{
	if(m_Stream)
		fclose(m_Stream);
	m_Stream = NULL;
}
