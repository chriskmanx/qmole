///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <float.h>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#if HAVE_SSTREAM
#include <sstream>
#else
#include <strstream.h>
#endif

#include <MSTypes/MSDefines.H>

#if defined(MS_REQUIRE_IEEEFP)
#include <ieeefp.h>
#endif

#include <MSTypes/MSString.H>
#include <MSTypes/MSTypes_MSF.H>

#include <MSTypes/MSBuiltinVectorImpl.H>
#include <MSTypes/MSBuiltinVectorImplInlines.C>

MSBuiltinVectorImpl::MSBuiltinVectorImpl (MSVectorImplOps *pOps_, MSBuiltinVectorImplOps *pBuiltInOps_,
					  unsigned int length_)
  : MSVectorImpl(pOps_,length_), _pBuiltInOps(pBuiltInOps_)
{
}


MSBuiltinVectorImpl::MSBuiltinVectorImpl (MSVectorImplOps *pOps_, MSBuiltinVectorImplOps *pBuiltInOps_,
					  unsigned int length_, void *pFiller_)
  : MSVectorImpl(pOps_,length_,pFiller_), _pBuiltInOps(pBuiltInOps_)
{
}


MSBuiltinVectorImpl::MSBuiltinVectorImpl (const MSBuiltinVectorImpl & vImpl_)
  : MSVectorImpl(vImpl_), _pBuiltInOps(vImpl_._pBuiltInOps)
{
}


MSBuiltinVectorImpl::MSBuiltinVectorImpl (MSVectorImplOps *pOps_, MSBuiltinVectorImplOps *pBuiltInOps_,
					  void *pData_, unsigned int len_)
  : MSVectorImpl(pOps_, pData_, len_), _pBuiltInOps(pBuiltInOps_)
{
}

MSBuiltinVectorImpl::~MSBuiltinVectorImpl()
{
}


MSVectorImpl *MSBuiltinVectorImpl::create (unsigned int len_, unsigned int size_) const
{
  if (size_)
    return new MSBuiltinVectorImpl (_pOperations, _pBuiltInOps, _pOperations->allocateWithSize (size_), len_);
  else
    return new MSBuiltinVectorImpl (_pOperations, _pBuiltInOps, len_);
}


MSVectorImpl *MSBuiltinVectorImpl::clone() const
{
  return new MSBuiltinVectorImpl (*this);
}


MSString MSBuiltinVectorImpl::asMSF()
{
  MSString result;
  static char buf[64];
#if HAVE_SSTREAM
  static ostringstream oss (buf, ios::out);
#else
  static ostrstream oss (buf, 64, ios::out);
#endif
  oss.precision (8);

  result << MSMSF_US << MSString(_len);

  for (unsigned int i=0; i<_len; i++)
    {
      oss.seekp (ios::beg);
      oss << MSMSF_US;
      _pBuiltInOps->writeToStream (_pElements, i, oss);
      oss << ends;
      
      result << buf;
    }
  
  result.encodeMSF();
  return result;
}


MSError::ErrorStatus MSBuiltinVectorImpl::setFromString (const char *pString_, const char)
{
  MSError::ErrorStatus rc = MSError::MSSuccess;

  _pOperations->deallocate (_pElements);
  
  if (pString_)
    {
#if defined(MS_NO_ISTRSTREAM_CONSTCHAR_CONSTRUCTOR)
      istrstream ist ((char *)(void *)pString_, strlen(pString_));
#else
#if HAVE_SSTREAM
      istringstream ist (pString_, istringstream::in);
#else
      istrstream ist (pString_, strlen(pString_));
#endif
#endif	
      _len = _pBuiltInOps->stringLen (pString_);
      _pElements = _pOperations->allocate (_len);
	  
      if (_len)
	{
	  _pBuiltInOps->whitespace (ist);
	  
	  for (unsigned int i=0; i<_len; i++)
	    _pBuiltInOps->readFromStream (_pElements, i, ist);

	  if (!ist)
	    rc = MSError::MSFailure;
	}
      else
	rc = MSError::MSFailure;
    }
  else  // if pString is NULL
    {
      _len = 0;
      _pElements = _pOperations->allocate (0);
    }
  
  return rc;
}


MSError::ErrorStatus MSBuiltinVectorImpl::setFromMSF (const char *pString_)
{
  MSError::ErrorStatus rc;
  unsigned int startpos;
  unsigned int value;
  char *pstring;
  const char *pcurrent; 

  if (pString_ && (*pString_ == MSMSF_US) && (strlen (pString_) > sizeof(MSMSF_US)))
    {
      rc = MSError::MSSuccess;
      _pOperations->deallocate (_pElements);

      MSString decode (pString_);
      decode.decodeMSF();
      
      unsigned int slen = decode.length();
      startpos = sizeof(MSMSF_US);
      pcurrent = (const char *)decode.string() + startpos; 
      
      // first get the size of the array....it the first element in the string
      // then point to the beginning of the data
      //
      value = 0;
      if (isdigit (*pcurrent))
	{
	  value = strtoul (pcurrent, &pstring, 10);
	  if (*pstring != '\0')
	    startpos = decode.indexOf (MSMSF_US, startpos);
	  else
	    {
	      value = 0;
	      rc = MSError::BadMSFString;
	    }
	}
      
      _len = value;
      _pElements = _pOperations->allocate (_len);
      
      if (_len)
	{
	  if (_pElements)
	    {
	      for (unsigned int i=0; i<_len; i++)
		{
		  if (startpos < slen) 
		    {
		      startpos += sizeof(MSMSF_US);
		      rc = _pOperations->setFromString (_pElements, i, (const char *)decode.string()+startpos);
		      if (rc == MSError::MSSuccess)
			startpos = decode.indexOf (MSMSF_US, startpos);
		      else
			{
			  rc = MSError::BadMSFString;
			  break;
			}
		    }
		  else 
		    {
		      rc = MSError::BadMSFString;
		      break;
		    }
		}
	    }
	  else
	    rc = MSError::MSFailure;
	}
      else
	rc = MSError::BadMSFString;
    }
  else
    rc = MSError::BadMSFString;
  
  if (rc != MSError::MSSuccess)
    removeAll();
  
  return rc;
}


MSIndexVector MSBuiltinVectorImpl::gradeUp() const
{
#if defined(MS_64BIT)
  return MSVectorImpl::gradeUp();
#else
  
  struct HH
  {
    struct HH *h;
    unsigned i;
  };
  
  unsigned int size = _pOperations->size (_pElements);

  MSIndexVector::Data *d = MSIndexVector::Data::allocateWithSize (size);
  
  if (_len > 0)
    {
      unsigned int *pIndices = d->elements();
      double x, u, y;
      unsigned int i, c=_len*20;
      unsigned int *pGradient = new unsigned [3*_len];
      HH *h = (HH *)(pGradient + _len);
      HH *j, *k;
     
      u = _pBuiltInOps->getAsNumber (_pElements, 0);      
      y = u;
      
      for (i=0; i<_len; i++) // get upper,lower bound, and initialize pGradient
	{
	  pGradient[i] = 0;
	  x = _pBuiltInOps->getAsNumber (_pElements, i);
	  if (isfinite(x)==0)	// if the element is Inf or NaN
	    {
	      // only "normal" numbers are allowed in this sort algorithm
	      //
	      mergeSortUp(pGradient,pIndices);
	      delete [] pGradient;
	      return MSIndexVector(d,_len);
	    }

	  if (x < u)
	    u = x;
	  else if (x > y)
	    y = x;
	}
      
      y -= u;

      if (y>=DBL_MAX || isfinite(y)==0)
	{ 
	  // range too large--bail out into merge sort
	  mergeSortUp(pGradient,pIndices);
	  delete [] pGradient;
	  return MSIndexVector(d,_len);
	}
      
      if (y)
	y = _len / (y*(1+1E-13));
      
      for (i=_len; i--;)
	{
	  x = _pBuiltInOps->getAsNumber (_pElements, i);
	  k = (HH *)(pGradient + (int)((x-u)*y));
	  
	  for (;(j=k->h) && x>_pBuiltInOps->getAsNumber(_pElements,j->i); k=j)
	    {
	      if (!--c) 
		{ 
		  // hash not going well--bail out into merge sort
		  mergeSortUp(pGradient,pIndices);
		  delete [] pGradient;
		  return MSIndexVector(d,_len);
		}
	    }
	  
	  h->h = j;
	  h->i = i;
	  k->h = h++;
	}
      
      for (i=0; i<_len; i++)
	for (h=(HH *)pGradient[i]; h!=0; h=h->h)
	  *pIndices++ = h->i;
      
      delete [] pGradient;
    }
  
  return MSIndexVector (d, _len);
#endif  //MS_64BIT
}


MSIndexVector MSBuiltinVectorImpl::gradeDown() const
{
#if defined(MS_64BIT)
  return MSVectorImpl::gradeDown();
#else
  struct HH
  {
    struct HH *h;
    unsigned i;
  };
  
  unsigned int size = _pOperations->size (_pElements);

  MSIndexVector::Data *d = MSIndexVector::Data::allocateWithSize (size);
  
  if (_len > 0)
    {
      unsigned int *pIndices = d->elements();
      double x, u, y;
      unsigned int i, c=_len*20;
      unsigned int *pGradient = new unsigned [3*_len];
      HH *h = (HH *)(pGradient + _len);
      HH *j, *k;
     
      u = - _pBuiltInOps->getAsNumber (_pElements, 0);
      y = u;
      
      for (i=0; i<_len; i++) // get upper,lower bound, and initialize pGradient
	{
	  pGradient[i] = 0;
	  x = - _pBuiltInOps->getAsNumber (_pElements, i);
	  if (isfinite(x)==0)	// if the element is Inf or NaN
	    {
	      // only "normal" numbers are allowed in this sort algorithm, no NaN or Inf
	      //
	      mergeSortDown(pGradient,pIndices);
	      delete [] pGradient;
	      return MSIndexVector(d,_len);
	    }
	  
	  if (x < u)
	    u = x;
	  else if (x > y)
	    y = x;
	}
      
      y -= u;

      if (y>=DBL_MAX || isfinite(y)==0) 
	{ 
	  // range too large--bail out into merge sort
	  mergeSortDown(pGradient,pIndices);
	  delete [] pGradient;
	  return MSIndexVector(d,_len);
	}
      
      if (y)
	y = _len / (y*(1+1E-13));
      
      for (i=_len; i--;)
	{
	  x = - _pBuiltInOps->getAsNumber (_pElements, i);
	  k = (HH *)(pGradient + (int)((x-u)*y));
	  
	  for (;(j=k->h) && x>-_pBuiltInOps->getAsNumber(_pElements,j->i); k=j)
	    {
	      if (!--c) 
		{ 
		  // hash not going well--bail out into merge sort
		  mergeSortDown(pGradient,pIndices);
		  delete [] pGradient;
		  return MSIndexVector(d,_len);
		}
	    }
	  
	  h->h = j;
	  h->i = i;
	  k->h = h++;
	}
      
      for (i=0; i<_len; i++)
	for (h=(HH *)pGradient[i]; h!=0; h=h->h)
	  *pIndices++ = h->i;
      
      delete [] pGradient;
    }
  
  return MSIndexVector (d, _len);
#endif  //MS_64BIT
}


double MSBuiltinVectorImpl::median() const
{
  if (!_len)
    return 0.0;
  
  if (_len % 2 == 0)  // is it even? if so, be pedantic about the median - 
    {                // some books say don't bother for n>100
      const MSIndexVector i=gradeUp();
      unsigned n = _len / 2;
      return (_pBuiltInOps->getAsNumber(_pElements,i(n-1))+_pBuiltInOps->getAsNumber(_pElements,i(n))) / 2;
    }
  else
    return (_pBuiltInOps->getAsNumber(_pElements, gradeUp()(_len/2)));
}


double MSBuiltinVectorImpl::variance(double mean_, MSEstimateType estType_) const
{
  if (_len > 1)
    {
      double t = 0.0, r = 0.0;
      //
      // The variance is a measure of how spread out a distribution is.
      // It is computed as the average squared deviation of each number from its mean.
      // The formula for the variance in a *population* is:
      //
      //            sigma^2 = SUMMATION(i=1,n,(X(i)-mu)^2) / n
      //
      // where n is the size of the population, mu is its mean, and X(i) is the value of i-th data element.
      //
      // When the variance is computed in a *sample*, the following statistic can be used:
      //
      //            S^2 = SUMMATION(i=1,N,(X(i)-M)^2) / N
      //
      // where N is the size of the sample, and M is its mean.  S is a *biased* estimate of sigma, however.
      // An *unbiased* estimate of sigma^2 can be found by the following formula:
      //
      //            S^2 = SUMMATION(i=1,N,(X(i)-M)^2) / (N-1)
      //
      // In this method, the estimate type (biased or unbiased) is controlled by the estType_ parameter.
      //
      double denom = (estType_==MSBiased) ? _len : _len-1;

      for (unsigned int i=0; i<_len; i++)
	{
	  t = _pBuiltInOps->getAsNumber (_pElements, i) - mean_;
	  r += t * t;
	}
      
      return (r/denom);
    }
  
  return 0.0;
}


MSBuiltinVectorImpl *MSBuiltinVectorImpl::movingAverage (unsigned int width_)
{
  if (_len < width_)
    return (MSBuiltinVectorImpl *)create();
  
  unsigned int newLen = _len - width_ +1;   // because of the above test, newLen >= 1

  MSBuiltinVectorImpl *mvAvg = (MSBuiltinVectorImpl *) create (newLen);
  double sum = 0.0;
  unsigned int i, j;

  for (i=0; i<width_; i++)
    sum += _pBuiltInOps->getAsNumber (_pElements, i);
  
  for (i=0, j=width_; ; i++, j++)
    {
      mvAvg->_pBuiltInOps->setToNumber (mvAvg->_pElements, i, sum/width_);
      if (j == _len)      
	break;
      
      sum += _pBuiltInOps->getAsNumber (_pElements, j) - _pBuiltInOps->getAsNumber (_pElements, i);
    }
  
  return mvAvg;
}


MSBuiltinVectorImplOps::MSBuiltinVectorImplOps()
{
}


MSBuiltinVectorImplOps::~MSBuiltinVectorImplOps()
{
}
