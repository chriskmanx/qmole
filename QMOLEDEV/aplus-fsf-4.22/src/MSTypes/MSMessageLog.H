#ifndef MSMessageLogHEADER
#define MSMessageLogHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#include <MSTypes/MSEnum.H>

class MSTypesExport MSMessageLog
{
public:
  enum Priority { Critical,Error,Warning,Info,Debug};
  enum Destination { StdErr,StdOut,Function,File,DebugOut };

  typedef void (*LogFunction)(Priority,const char *);

  static void quietMode(MSBoolean mode_) { _quietMode=mode_; }
  static MSBoolean quietMode(void)       { return _quietMode; }
  
  //Default threshold is Warning
  static void logThreshold(Priority threshold_) { _threshold=threshold_; }
  static Priority logThreshold(void)            { return _threshold;     }

  //Default destination is stdErr
  static void logDestination(Destination dest_);
  static Destination logDestination(void)       { return _destination; }

  //Default function is 0
  static void logFunction(LogFunction func_)        { _mstkLogFunc=func_;  }
  static LogFunction logFunction(void)              { return _mstkLogFunc; }

  static int logFileName(const char *fileName_);
  static const char *logFileName(void);

  //--------------------------------------------------
  //Below is interface for reporting messages.
  //It is for MSTK internal usage.
  static void message(Priority,const char *,...);

  //convinience functions.
  static void criticalMessage(const char *,...);
  static void errorMessage(const char *,...);
  static void warningMessage(const char *,...);
  static void infoMessage(const char *,...);
  static void debugMessage(const char *,...);
  

private:
  static void outputMessage(Priority,const char*);
  
  static Destination  _destination;
  static Priority     _threshold;
  static LogFunction  _mstkLogFunc;
  static MSBoolean    _quietMode;
};

#endif
