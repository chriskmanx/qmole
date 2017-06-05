#ifndef MSMappedFileAccessHEADER
#define MSMappedFileAccessHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSCharVector.H>
#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSLongVector.H>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSFloatMatrix.H>
#include <MSTypes/MSCharMatrix.H>
#include <MSTypes/MSIntMatrix.H>
#include <MSTypes/MSLongMatrix.H>

class MSTypesExport MSMappedFileAccess
{
public:

  static MSBoolean beamIn(MSCharVector &,const char *fileName_);
  static MSBoolean beamIn(MSIntVector &,const char *fileName_);
  static MSBoolean beamIn(MSLongVector &,const char *fileName_);
  static MSBoolean beamIn(MSFloatVector &,const char *fileName_);
  
  static MSBoolean beamIn(MSCharMatrix &,const char *fileName_);
  static MSBoolean beamIn(MSIntMatrix &,const char *fileName_);
  static MSBoolean beamIn(MSLongMatrix &,const char *fileName_);
  static MSBoolean beamIn(MSFloatMatrix &,const char *fileName_);

  static void beamOut(const char *fileName_,const MSCharVector &vector_);
  static void beamOut(const char *fileName_,const MSIntVector &vector_);
  static void beamOut(const char *fileName_,const MSLongVector &vector_);
  static void beamOut(const char *fileName_,const MSFloatVector &vector_);

  static void beamOut(const char *fileName_,const MSCharMatrix &matrix_);
  static void beamOut(const char *fileName_,const MSIntMatrix &matrix_);
  static void beamOut(const char *fileName_,const MSLongMatrix &matrix_);
  static void beamOut(const char *fileName_,const MSFloatMatrix &matrix_);

protected:
  
};

#endif
