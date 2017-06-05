#ifndef __AplusFormatter_H
#define __AplusFormatter_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSTime.H>
#include <a/fncdcls.h>
#include <a/x.h>
#include <time.h>
#include <AplusGUI/AplusHashTable.H>

class AplusFormatter
{
public:
  enum OutputFormat 
   {BadFormat=0,NoFormat=1,Float=2,Fixed=3,Fraction=4,
    Price32=10,Price64=11,Price128=12,Price320=13,Price328=14,
    hh=20,hh24=21,mm=22,ss=23,hhmm=24,hhmm24=25,mmss=26,hhmmss=27,hhmmss24=28,
    D=30,DDD=31,DD=32,MM=33,MO=34,Y2=35,Y4=36,
    DMDDYYYY=37,DDMMYY=38,DDMMYYYY=39,MMDDYY=40,MMDDYYYY=41,
    D_u=60,DDD_u=61,DD_u=62,MM_u=63,MO_u=64,Y2_u=65,Y4_u=66,
    DMDDYYYY_u=67,DDMMYY_u=68,DDMMYYYY_u=69,MMDDYY_u=70,MMDDYYYY_u=71};
   // XGraph.C is dependent on the integer values assigned to the above enumerations
protected:
  static AplusHashTable *_enumHashTable;   
  static AplusHashTable *_stringEnumHashTable;   
  static int        _count;
  static struct tm  _aTimeStruct;
  
  int count(void) { return _count; }
  AplusHashTable *enumHashTable(void) { return _enumHashTable; }
  AplusHashTable *stringEnumHashTable(void) { return _stringEnumHashTable; }
      
  void initHashTable(void);
  void initStringHashTable(void);
  struct tm *julianDay(double);
  struct tm *parseDate(double);
  
public:
  AplusFormatter(void);
  ~AplusFormatter(void);
 
  OutputFormat format(const char *);
  OutputFormat format(A);
  A formats(void);
  char *formatEnumString(int);
  const char *formatOutput(OutputFormat,double,int,MSBoolean);
  A formatOutput(A,A);
  A fmt(A,A);
  A sfmt(A,A);
  A thorn(A,A);
  int defaultPrecision(void);
};


#endif






