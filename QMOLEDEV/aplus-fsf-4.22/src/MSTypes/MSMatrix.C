///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSMatrix.H>
#include <MSTypes/MSMessageLog.H>

MSMatrix::MSMatrix() : _rows(0), _columns(0), _count(0)
{
//  _rows=_columns=_count=0;
}


MSMatrix::MSMatrix (unsigned rows_,unsigned columns_) : _rows(rows_),_columns(columns_),_count(rows_*columns_)
{
//  _rows=rows_,_columns=columns_,_count=rows_*columns_;
}


MSMatrix::~MSMatrix()
{
}


void MSMatrix::indexError(unsigned i_,unsigned l_)
{ MSMessageLog::errorMessage("MSMatrix Index Error: index %d\tcount %d\n",i_,l_); }

void MSMatrix::matrixErrorHandler(const char* msg_)
{ MSMessageLog::errorMessage("MSMatrix Error: %s\n",msg_); }

MSMatrixErrorHandler MSMatrix::_matrixErrorHandler=matrixErrorHandler;

MSMatrixErrorHandler MSMatrix::setMatrixErrorHandler(MSMatrixErrorHandler aFunction_)
{
  MSMatrixErrorHandler old=_matrixErrorHandler;
  _matrixErrorHandler=aFunction_;
  return old;
}
