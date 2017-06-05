///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSMessageLog.H>
#include <MSTypes/MSMutex.H>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>

#if defined(MS_WINDOWS)
#include <windows.h>
#endif

MSMessageLog::Destination MSMessageLog::_destination=StdErr;
MSMessageLog::Priority MSMessageLog::_threshold=Warning;
MSMessageLog::LogFunction MSMessageLog::_mstkLogFunc =0;
MSBoolean MSMessageLog::_quietMode=MSFalse;

static int fileOut=0;
static char fileName[FILENAME_MAX]="\0";

#if defined(MS_MULTI_THREAD)
static MSMutex logMutex;
#endif

void MSMessageLog::outputMessage(Priority pri_,const char *buf)
{
  switch(_destination)
   {
   case StdOut:
     fprintf(stdout,buf);
     break;
   case DebugOut:
#if defined(MS_WINDOWS)
     OutputDebugString(buf);
     break;
#endif
//else fall through     
   case StdErr:
     fprintf(stderr,buf);
     break;
   case Function:
     if(_mstkLogFunc) (*_mstkLogFunc)(pri_,buf);
     break;
   case File:
     if(fileOut!=0)
      {
        write(fileOut,buf,strlen(buf));
      }
     break;
   default:
     break;
   }
}

void MSMessageLog::logDestination(Destination dest_)
{
  MSGUARD(logMutex);
  if(_destination!=dest_)
   {
     _destination=dest_;
     if(_destination!=File)
      {
        if(fileOut!=0)
         {
           close(fileOut);
           fileOut=0;
         }
      }
     else
      {
        if(fileOut==0 && fileName[0])
         {
           fileOut = open(fileName,O_CREAT|O_RDWR|O_APPEND,0666);
           if(errno==21)
            {
              close(fileOut);
              fileOut=0;
            }
           if(fileOut==0)
            {
              fprintf(stderr,"MSMessageLog::logFileName: Error: Unable to open file `%s'\n",fileName);
            }
         }
      }
   }
}

const char* MSMessageLog::logFileName(void)
{
  return fileName;
}

int MSMessageLog::logFileName(const char *name_)
{
  MSGUARD(logMutex);
  if(fileOut!=0)
   {
     close(fileOut);
     fileOut=0;
   }

  if(name_==0 || *name_==0) fileName[0]='\0';
  else
   {
     strcpy(fileName,name_);
     if(_destination==File)
      {
        fileOut = open(fileName,O_CREAT|O_RDWR|O_APPEND,0666);
        if(errno==21)
         {
           close(fileOut);
           fileOut=0;
         }
        if(fileOut==0)
         {
           fprintf(stderr,"MSMessageLog::logFileName: Error: Unable to open file `%s'\n",fileName);
         }
      }
   }
  return (fileOut==0)?0:1;
}

  
/*
 * This is a body for all *message functions
 * The code is almost identical, but we can't just call a common
 * function because of necessary vararg setup/cleanup steps in each function.
 */

#define OUTPUT_MESSAGE(pri,fmt,buf) \
  if(_quietMode==MSFalse && pri<=_threshold) \
  {\
   MSGUARD(logMutex);\
   va_list ap;\
   va_start(ap,fmt);\
   if(fmt) vsprintf(buf,fmt,ap);\
   else buf[0]=0;\
   va_end(ap); \
   outputMessage(pri,buf);\
  }

static char msgBuf[1024];

void MSMessageLog::message(Priority pri_, const char *fmt,...)
{ OUTPUT_MESSAGE(pri_,fmt,msgBuf) }

void MSMessageLog::criticalMessage(const char *fmt,...)
{ OUTPUT_MESSAGE(Critical,fmt,msgBuf) }

void MSMessageLog::errorMessage(const char *fmt,...)
{ OUTPUT_MESSAGE(Error,fmt,msgBuf) }

void MSMessageLog::warningMessage(const char *fmt,...)
{ OUTPUT_MESSAGE(Warning,fmt,msgBuf) }

void MSMessageLog::infoMessage(const char *fmt,...)
{ OUTPUT_MESSAGE(Info,fmt,msgBuf) }

void MSMessageLog::debugMessage(const char *fmt,...)
{ OUTPUT_MESSAGE(Debug,fmt,msgBuf) }

