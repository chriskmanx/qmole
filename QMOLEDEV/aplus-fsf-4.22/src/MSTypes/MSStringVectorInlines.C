#ifndef MSStringVectorINLINES
#define MSStringVectorINLINES

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#ifndef MS_NO_INLINES
#define INLINELINKAGE inline
#else
#define INLINELINKAGE
#endif  // MS_NO_INLINES


INLINELINKAGE MSString MSTypeVector<MSString>::asString (const char separator_) const
{
  return _pImpl->asString (separator_);
}


INLINELINKAGE MSError::ErrorStatus MSTypeVector<MSString>::set (unsigned int index_, const MSString & value_)
{
  return ObjectVectorString::set (index_, value_);
}


INLINELINKAGE MSTypeVector<MSString> & MSTypeVector<MSString>::appendSingle (const MSString & string_)
{
  ObjectVectorString::append (string_);
  return *this;
}


INLINELINKAGE MSTypeVector<MSString> & MSTypeVector<MSString>::append (const MSString & string_, const char delimiter_)
{
  return append ((const char *)string_, delimiter_);
}


INLINELINKAGE MSTypeVector<MSString> & MSTypeVector<MSString>::append (const MSTypeVector<MSString> & vect_)
{
  ObjectVectorString::append (vect_);
  return *this;
}


INLINELINKAGE MSTypeVector<MSString> & MSTypeVector<MSString>::operator<<= (const MSTypeVector<MSString> & vect_)
{
  return append (vect_);
}


INLINELINKAGE MSTypeVector<MSString> & MSTypeVector<MSString>::operator<<= (const MSString & value_)
{
  return append ((const char *)value_);
}


INLINELINKAGE MSTypeVector<MSString> & MSTypeVector<MSString>::operator<<= (const char *pString_)
{
  return append (pString_);
}


INLINELINKAGE MSTypeVector<MSString> & MSTypeVector<MSString>::operator<< (const MSTypeVector<MSString> & vect_)
{
  return append (vect_);
}


INLINELINKAGE MSTypeVector<MSString> & MSTypeVector<MSString>::operator<< (const MSString & value_)
{
  return append ((const char *)value_);
}


INLINELINKAGE MSTypeVector<MSString> & MSTypeVector<MSString>::operator<< (const char *pString_)
{
  return append (pString_);
}

#endif  // MSStringVectorINLINES
