///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSIntVector.H>
#include <MSTypes/MSFloatVector.H>
#include <MSTypes/MSIndexVector.H>
#include <MSTypes/MSBinaryVector.H>
#include <MSTypes/MSConvert.H>

#ifdef MS_NO_INLINES
#include <MSTypes/MSConvertInlines.C>
#endif // MS_NO_INLINES

MSError::ErrorStatus msConvert(const MSIntVector& iv_, MSFloatVector& fv_)
{
  unsigned int n=iv_.length();

  fv_.reshape(n);
  //
  // fv_'s reference count should now be 1, so it's safe to write directly into elements
  //
  const int *ivData=iv_.data();
  double *fvData=fv_.data();

  while (n--)
    {
      *fvData++ = (double)*ivData++;
    }

  return MSError::MSSuccess;
}


MSError::ErrorStatus msConvert(const MSBinaryVector& bv_, MSIndexVector& iv_)
{
  unsigned int n=bv_.length();

  iv_.reshape((unsigned int)(bv_.sum()));	// the number of elements in iv_ is the number of 1's in bv_
  //
  // iv_'s reference count should now be 1, so it's safe to write directly into elements
  //
  const unsigned char *bvData=bv_.data();
  unsigned int *ivData=iv_.data();

  for (unsigned int i=0; i<n; ++i)
    {
      if (bvData[i]==1)
	{
	  *ivData++ = i;
	}
    }

  return MSError::MSSuccess;
}



