#ifndef MSLabelFormatHEADER
#define MSLabelFormatHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <time.h>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

class MSLabelOut;

class MSGUIExport MSLabelFormat
{
public:
  MSLabelFormat(void);
  ~MSLabelFormat(void);
 
  unsigned long nextWeekday(unsigned long);
  const double **findProtocol(const MSLabelOut&);
  double snapTime(double,double);
  double snapNumber(double,const MSLabelOut&);
  double snipNumber(double,const MSLabelOut&);
  double minimumNumber(const MSLabelOut&);
  time_t timegm(struct tm *tm_)
   {
#if defined(MS_HAS_MKTIME) || defined(HAVE_MKTIME)
     return mktime(tm_);
#else
     return ::timegm(tm_);
#endif
  }
};

#endif





