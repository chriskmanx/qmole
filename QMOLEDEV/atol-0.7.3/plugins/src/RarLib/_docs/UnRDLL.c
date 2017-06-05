#define STRICT
#include <windows.h>
#include <stdio.h>
#include <ctype.h>
#include "unrar.h"

enum { EXTRACT, TEST, PRINT };

void ExtractArchive(char *ArcName,int Mode);
void ListArchive(char *ArcName);
void ShowComment(char *CmtBuf);
void OutHelp(void);
void OutOpenArchiveError(int Error,char *ArcName);
void ShowArcInfo(unsigned int Flags,char *ArcName);
void OutProcessFileError(int Error);
int CALLBACK CallbackProc(UINT msg,LONG UserData,LONG P1,LONG P2);

main(int Argc, char *Argv[])
{
  if (Argc!=3)
  {
    OutHelp();
    return(0);
  }

  switch(toupper(Argv[1][0]))
  {
    case 'X':
      ExtractArchive(Argv[2],EXTRACT);
      break;
    case 'T':
      ExtractArchive(Argv[2],TEST);
      break;
    case 'P':
      ExtractArchive(Argv[2],PRINT);
      break;
    case 'L':
      ListArchive(Argv[2]);
      break;
    default:
      OutHelp();
      return(0);
  }

  return(0);
}


void ExtractArchive(char *ArcName,int Mode)
{
  HANDLE hArcData;
  int RHCode,PFCode;
  char CmtBuf[16384];
  struct RARHeaderData HeaderData;
  struct RAROpenArchiveDataEx OpenArchiveData;

  memset(&OpenArchiveData,0,sizeof(OpenArchiveData));
  OpenArchiveData.ArcName=ArcName;
  OpenArchiveData.CmtBuf=CmtBuf;
  OpenArchiveData.CmtBufSize=sizeof(CmtBuf);
  OpenArchiveData.OpenMode=RAR_OM_EXTRACT;
  hArcData=RAROpenArchiveEx(&OpenArchiveData);

  if (OpenArchiveData.OpenResult!=0)
  {
    OutOpenArchiveError(OpenArchiveData.OpenResult,ArcName);
    return;
  }

  ShowArcInfo(OpenArchiveData.Flags,ArcName);

  if (OpenArchiveData.CmtState==1)
    ShowComment(CmtBuf);

  RARSetCallback(hArcData,CallbackProc,(LONG)&Mode);

  HeaderData.CmtBuf=NULL;

  while ((RHCode=RARReadHeader(hArcData,&HeaderData))==0)
  {
    switch(Mode)
    {
      case EXTRACT:
        printf("\nExtracting %-45s",HeaderData.FileName);
        break;
      case TEST:
        printf("\nTesting %-45s",HeaderData.FileName);
        break;
      case PRINT:
        printf("\nPrinting %-45s\n",HeaderData.FileName);
        break;
    }
    PFCode=RARProcessFile(hArcData,(Mode==EXTRACT) ? RAR_EXTRACT:RAR_TEST,
                          NULL,NULL);
    if (PFCode==0)
      printf(" Ok");
    else
    {
      OutProcessFileError(PFCode);
      break;
    }
  }

  if (RHCode==ERAR_BAD_DATA)
    printf("\nFile header broken");

  RARCloseArchive(hArcData);
}


void ListArchive(char *ArcName)
{
  HANDLE hArcData;
  int RHCode,PFCode;
  char CmtBuf[16384];
  struct RARHeaderDataEx HeaderData;
  struct RAROpenArchiveDataEx OpenArchiveData;

  memset(&OpenArchiveData,0,sizeof(OpenArchiveData));
  OpenArchiveData.ArcName=ArcName;
  OpenArchiveData.CmtBuf=CmtBuf;
  OpenArchiveData.CmtBufSize=sizeof(CmtBuf);
  OpenArchiveData.OpenMode=RAR_OM_LIST;
  hArcData=RAROpenArchiveEx(&OpenArchiveData);

  if (OpenArchiveData.OpenResult!=0)
  {
    OutOpenArchiveError(OpenArchiveData.OpenResult,ArcName);
    return;
  }

  ShowArcInfo(OpenArchiveData.Flags,ArcName);

  if (OpenArchiveData.CmtState==1)
    ShowComment(CmtBuf);

  RARSetCallback(hArcData,CallbackProc,0);

  HeaderData.CmtBuf=CmtBuf;
  HeaderData.CmtBufSize=sizeof(CmtBuf);

  printf("\nFile                       Size");
  printf("\n-------------------------------");
  while ((RHCode=RARReadHeaderEx(hArcData,&HeaderData))==0)
  {
    __int64 UnpSize=HeaderData.UnpSize+(((__int64)HeaderData.UnpSizeHigh)<<32);
    printf("\n%-20s %10Ld ",HeaderData.FileName,UnpSize);
    if (HeaderData.CmtState==1)
      ShowComment(CmtBuf);
    if ((PFCode=RARProcessFile(hArcData,RAR_SKIP,NULL,NULL))!=0)
    {
      OutProcessFileError(PFCode);
      break;
    }
  }

  if (RHCode==ERAR_BAD_DATA)
    printf("\nFile header broken");

  RARCloseArchive(hArcData);
}


void ShowComment(char *CmtBuf)
{
  printf("\nComment:\n%s\n",CmtBuf);
}


void OutHelp(void)
{
  printf("\nUNRDLL.   This is a simple example of UNRAR.DLL usage\n");
  printf("\nSyntax:\n");
  printf("\nUNRDLL X <Archive>     extract archive contents");
  printf("\nUNRDLL T <Archive>     test archive contents");
  printf("\nUNRDLL P <Archive>     print archive contents to stdout");
  printf("\nUNRDLL L <Archive>     view archive contents\n");
}


void OutOpenArchiveError(int Error,char *ArcName)
{
  switch(Error)
  {
    case ERAR_NO_MEMORY:
      printf("\nNot enough memory");
      break;
    case ERAR_EOPEN:
      printf("\nCannot open %s",ArcName);
      break;
    case ERAR_BAD_ARCHIVE:
      printf("\n%s is not RAR archive",ArcName);
      break;
    case ERAR_BAD_DATA:
      printf("\n%s: archive header broken",ArcName);
      break;
    case ERAR_UNKNOWN:
      printf("Unknown error");
      break;
  }
}


void ShowArcInfo(unsigned int Flags,char *ArcName)
{
  printf("\nArchive %s\n",ArcName);
  printf("\nVolume:\t\t%s",(Flags & 1) ? "yes":"no");
  printf("\nComment:\t%s",(Flags & 2) ? "yes":"no");
  printf("\nLocked:\t\t%s",(Flags & 4) ? "yes":"no");
  printf("\nSolid:\t\t%s",(Flags & 8) ? "yes":"no");
  printf("\nNew naming:\t%s",(Flags & 16) ? "yes":"no");
  printf("\nAuthenticity:\t%s",(Flags & 32) ? "yes":"no");
  printf("\nRecovery:\t%s",(Flags & 64) ? "yes":"no");
  printf("\nEncr.headers:\t%s",(Flags & 128) ? "yes":"no");
  printf("\nFirst volume:\t%s",(Flags & 256) ? "yes":"no or older than 3.0");
  printf("\n---------------------------\n");
}


void OutProcessFileError(int Error)
{
  switch(Error)
  {
    case ERAR_UNKNOWN_FORMAT:
      printf("Unknown archive format");
      break;
    case ERAR_BAD_ARCHIVE:
      printf("Bad volume");
      break;
    case ERAR_ECREATE:
      printf("File create error");
      break;
    case ERAR_EOPEN:
      printf("Volume open error");
      break;
    case ERAR_ECLOSE:
      printf("File close error");
      break;
    case ERAR_EREAD:
      printf("Read error");
      break;
    case ERAR_EWRITE:
      printf("Write error");
      break;
    case ERAR_BAD_DATA:
      printf("CRC error");
      break;
    case ERAR_UNKNOWN:
      printf("Unknown error");
      break;
  }
}


int CALLBACK CallbackProc(UINT msg,LONG UserData,LONG P1,LONG P2)
{
  switch(msg)
  {
    case UCM_CHANGEVOLUME:
      if (P2==RAR_VOL_ASK)
      {
        printf("\nInsert disk with %s and press 'Enter' or enter 'Q' to exit ",(char *)P1);
        return(toupper(getchar())=='Q' ? -1:0);
      }
      return(0);
    case UCM_PROCESSDATA:
      if (UserData!=0 && *(int *)UserData==PRINT)
      {
        fflush(stdout);
        fwrite((char *)P1,1,P2,stdout);
        fflush(stdout);
      }
      return(0);
    case UCM_NEEDPASSWORD:
      printf("\nPassword required: ");
      gets((char *)P1);
      return(0);
  }
  return(0);
}
