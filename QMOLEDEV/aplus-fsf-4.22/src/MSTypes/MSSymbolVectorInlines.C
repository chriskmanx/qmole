#ifndef MSSymbolVectorINLINES
#define MSSymbolVectorINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////



INLINELINKAGE const MSSymbol & MSTypeVector<MSSymbol>::firstElement() const
{
  return elementAt (0);
}


INLINELINKAGE const MSSymbol & MSTypeVector<MSSymbol>::lastElement() const
{
  return elementAt (_pImpl->length() -1);
}


INLINELINKAGE const MSSymbol & MSTypeVector<MSSymbol>::operator() (unsigned int index_) const
{
  return elementAt (index_);
}


INLINELINKAGE const MSSymbol & MSTypeVector<MSSymbol>::operator[] (unsigned int index_) const
{
  return elementAt (index_);
}


INLINELINKAGE const MSSymbol & MSTypeVector<MSSymbol>::elementAt (unsigned int index_) const
{
#if !defined MS_NO_INDEX_ERROR
  if (index_ >= _pImpl->length())
    {
      _pImpl->vectorIndexError (index_);
      return *(const MSSymbol *)ops().badData();
    }
#endif  // MS_NO_INDEX_ERROR
  return data()[index_];
}


INLINELINKAGE MSTypeVector<MSSymbol>::SPick MSTypeVector<MSSymbol>::operator[] (unsigned int index_)
{
  return SPick (*this, index_);
}


INLINELINKAGE MSTypeVector<MSSymbol> MSTypeVector<MSSymbol>::operator[] (const MSIndexVector & iVect_) const
{
  return select (*this, iVect_);
}


INLINELINKAGE MSTypeVector<MSSymbol> MSTypeVector<MSSymbol>::operator[] (const MSBinaryVector & bVect_) const
{
  return compress (*this, bVect_);
}


INLINELINKAGE MSTypeVector<MSSymbol>::SPick::SPick (MSTypeVector<MSSymbol> & aVector_, unsigned int index_)
  : _pVector(&aVector_), _index(index_)
{ 
}


INLINELINKAGE MSTypeVector<MSSymbol>::SPick::SPick (const MSTypeVector<MSSymbol>::SPick & aPick_)
  : _pVector(aPick_._pVector), _index(aPick_._index)
{
}


INLINELINKAGE MSTypeVector<MSSymbol>::SPick::operator MSSymbol() const
{
  return (*_pVector)(_index);
}


#endif  //MSSymbolVectorINLINES
