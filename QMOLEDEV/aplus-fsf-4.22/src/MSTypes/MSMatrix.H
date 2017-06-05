#ifndef MSMatrixHEADER
#define MSMatrixHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <assert.h>
#include <MSTypes/MSEnum.H>
#include <MSTypes/MSIndexedModel.H>
#include <MSTypes/MSString.H>

typedef void (*MSMatrixErrorHandler)(const char*);

#if HAVE_IOSTREAM
#include <iostream>
// #include <fstream>
using namespace std;
#else
class ostream;
#endif


const unsigned MSMatrixDefaultMinimumAllocationSize=8;

class MSTypesExport MSMatrix : public MSIndexedModel 
{
protected:
  unsigned _count;  
  unsigned _rows;
  unsigned _columns;

  static void indexError(unsigned,unsigned);  

  static MSMatrixErrorHandler _matrixErrorHandler;
  static void matrixErrorHandler(const char*);
  static MSMatrixErrorHandler setMatrixErrorHandler(MSMatrixErrorHandler);
  
public:
  MSMatrix();
  MSMatrix(unsigned rows_,unsigned columns_);
  virtual ~MSMatrix();
  
  unsigned count(void) const;
  unsigned rows(void) const;
  unsigned columns(void) const;
  unsigned index(unsigned row_,unsigned column_) const;
};

inline unsigned MSMatrix::count(void) const
{ return _count; }
inline unsigned MSMatrix::rows(void) const
{ return _rows; } 
inline unsigned MSMatrix::columns(void) const
{ return _columns; } 
inline unsigned MSMatrix::index(unsigned  row_,unsigned column_) const
{ return (row_*columns())+column_; }

#endif
