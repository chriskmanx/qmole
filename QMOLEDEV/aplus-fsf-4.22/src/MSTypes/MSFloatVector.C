///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSDefines.H>

#if defined(MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION)
#define MS_MSFloatVector_INSTANTIATE
#define MSTK_MANUAL_INSTANTIATION
#endif


#include <MSTypes/MSFloatVector.H>

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

#ifdef MS_NO_INLINES
#include <MSTypes/MSFloatVectorInlines.C>
#endif // MS_NO_INLINES

#if defined(MSTK_MANUAL_INSTANTIATION)
#include <MSTypes/MSBuiltinTypeVector.C>

#if defined(MS_XLC_TEMPLATE_INSTANTIATION)
#pragma define (MSTypeVector<double>)
#endif

#if defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma instantiate MSBaseVector<double, MSAllocator<double> >
#pragma instantiate MSBuiltinVector<double>

#if !defined(MS_TEMPLATE_MANUAL_FRIEND_BUG)
// The version of EDG used by SGI's dcc compiler has a bug related to access from friends
#pragma instantiate ostream & operator<<(ostream &, const MSBaseVector<double, MSAllocator<double> >&)
#endif  //MS_TEMPLATE_MANUAL_FRIEND_BUG

#endif  //MS_EDG_TEMPLATE_INSTANTIATION_

#if defined(MS_VC_TEMPLATE_INSTANTIATION)
template MSBaseVector<double, MSAllocator<double> >;
template MSBaseVectorOps<double, MSAllocator<double> >;
template class MSBuiltinVector<double>;
template class MSBuiltinVectorOps<double>;


static int __instantiateFriends__()     // instantiate non-inline friend template functions
{
  if (0)
    {
      MSBuiltinVector<double> dummy;
      operator<<(cout, dummy);
    }
  return 0;
}
static int __dummyInt__=__instantiateFriends__();
#endif  // MS_VC_TEMPLATE_INSTANTIATION

#if defined(MS_STD_TEMPLATE_INSTANTIATION)
template class MSBaseVector<double, MSAllocator<double> >;
template class MSBaseVectorOps<double, MSAllocator<double> >;
template class MSBuiltinVector<double>;
template class MSBuiltinVectorOps<double>;
template ostream& operator<<(ostream&,const MSBaseVector<double, MSAllocator<double> >&);
#endif

#endif // MSTK_MANUAL_INSTANTIATION

#include <MSTypes/MSFloatSPick.C>

MSTypeVector<double>::MSTypeVector() : BuiltinVectorDouble()
{
}


MSTypeVector<double>::MSTypeVector (unsigned int length_) : BuiltinVectorDouble (length_)
{
}


MSTypeVector<double>::MSTypeVector (unsigned int length_, const double & filler_)
  : BuiltinVectorDouble (length_, filler_)
{
}


MSTypeVector<double>::MSTypeVector (const MSTypeVector<double> & vect_) : BuiltinVectorDouble (vect_)
{
}


MSTypeVector<double>::MSTypeVector (const BuiltinVectorDouble & vect_) : BuiltinVectorDouble (vect_)
{
}


MSTypeVector<double>::MSTypeVector (const BaseVectorDouble & vect_)
  : BuiltinVectorDouble ((BuiltinVectorDouble &)vect_)
{
}


MSTypeVector<double>::MSTypeVector (const char *pString_) : BuiltinVectorDouble (pString_)
{
}

  
MSTypeVector<double>::MSTypeVector (MSTypeData<double, MSAllocator<double> > *pData_, unsigned int len_)
  : BuiltinVectorDouble (pData_, len_)
{
}


MSTypeVector<double>::MSTypeVector (const double *pElements_, unsigned int len_)
  : BuiltinVectorDouble (pElements_, len_)
{
}


MSTypeVector<double>::~MSTypeVector()
{
}


MSTypeVector<double> & MSTypeVector<double>::operator= (const MSTypeVector<double> & vect_)
{
  return (MSTypeVector<double> &) BuiltinVectorDouble::operator= (vect_);
}


MSTypeVector<double> & MSTypeVector<double>::operator= (const BuiltinVectorDouble & vect_)
{
  return (*this = (MSTypeVector<double> &)vect_);
}


MSTypeVector<double> & MSTypeVector<double>::operator= (const BaseVectorDouble & vect_)
{
  return (*this = (MSTypeVector<double> &)vect_);
}


MSTypeVector<double> & MSTypeVector<double>::operator= (const double & value_)
{
  return (MSTypeVector<double> &) BuiltinVectorDouble::operator= (value_);
}


MSTypeVector<double> & MSTypeVector<double>::operator= (const char *pString_)
{
  return (*this = MSTypeVector<double>(pString_));
//  return (MSTypeVector<double> &) BuiltinVectorDouble::operator= (pString_);
}


MSString MSTypeVector<double>::className() const
{
  return MSString ("MSTypeVector<double>");
}


const MSSymbol & MSTypeVector<double>::type() const
{
  return symbol();
}


MSModel * MSTypeVector<double>::clone() const
{
  return new MSTypeVector<double> (*this);
}


MSModel * MSTypeVector<double>::create() const
{
  return new MSTypeVector<double>;
}


const MSSymbol & MSTypeVector<double>::symbol()
{
  static MSSymbol sym ("MSTypeVector<double>");
  return sym;
}


MSTypeVector<double> & MSTypeVector<double>::allElementsDo (MathFunction aFunction_)
{
  unsigned int len = length();  
  double *dp = data();

  // What we do below is a little bit of a hack; since we have a reference counted vector data, we have to check
  // the reference count to see if we need to allocate our own copy of the data.  However, the data itself is stored
  // in BuiltInVectorImpl, and we don't have direct access to it.  Therefore, we here reallocate the whole
  // implementation class, which shouldn't be much slower since the bulk of it is the vector data anyway.
  //
  if (ops().refCount (_pImpl->data()) > 1)
    {
      MSBuiltinVectorImpl *pNewImpl = (MSBuiltinVectorImpl *)_pImpl->create (len, size());
      double *pNewData = ((MSTypeData<double, MSAllocator<double> > *)pNewImpl->data())->elements();      

      while (len--)
	*pNewData++ = aFunction_ (*dp++);
      
      delete _pImpl;
      _pImpl = pNewImpl;
    }
  else 	// refCount == 1
    {
      while (len--)
	{
	  *dp = aFunction_ (*dp);
	  dp++;
	}
    }

  changed();

  return *this;
}


MSTypeVector<double> & MSTypeVector<double>::allElementsDo (ElementWiseFunction aFunction_,
							    void *pClientData_)
{
  unsigned int len = length();  
  double *dp = data();

  // What we do below is a little bit of a hack; since we have a reference counted vector data, we have to check
  // the reference count to see if we need to allocate our own copy of the data.  However, the data itself is stored
  // in BuiltInVectorImpl, and we don't have direct access to it.  Therefore, we here reallocate the whole
  // implementation class, which shouldn't be much slower since the bulk of it is the vector data anyway.
  //
  if (ops().refCount (_pImpl->data()) > 1)
    {
      MSBuiltinVectorImpl *pNewImpl = (MSBuiltinVectorImpl *)_pImpl->create (len, size());
      double *pNewData = ((MSTypeData<double, MSAllocator<double> > *)pNewImpl->data())->elements();      

      while (len--)
	*pNewData++ = aFunction_ (*dp++, pClientData_);

      delete _pImpl;
      _pImpl = pNewImpl;
    }
  else 	// refCount == 1
    {
      while (len--)
	{
	  *dp = aFunction_ (*dp, pClientData_);
	  dp++;
	}
    }

  changed();
  return *this;
}


MSTypeVector<double>
MSTypeVector<double>::allElementsDo (const MSTypeVector<double> & vect_, MathFunction func_)
{
  unsigned int len = vect_.length();
  
  MSTypeData<double, MSAllocator<double> > *pNewData = MSTypeData<double, MSAllocator<double> >::allocateWithSize (vect_.size());

  const double *pElements = vect_.data();
  double *pNewElements = pNewData->elements();
  
  while (len--)
    *pNewElements++ = func_ (*pElements++);
  
  return MSTypeVector<double> (pNewData, vect_.length());
}


MSTypeVector<double>
MSTypeVector<double>::allElementsDo (const MSTypeVector<double>& vect_,
                                     ElementWiseFunction aFunction_, void *pClientData_)
{
  unsigned int len = vect_.length();
  
  MSTypeData<double, MSAllocator<double> > *pNewData = MSTypeData<double, MSAllocator<double> >::allocateWithSize (vect_.size());

  const double *pElements = vect_.data();
  double *pNewElements = pNewData->elements();
  
  while (len--)
    *pNewElements++ = aFunction_ (*pElements++, pClientData_);
  
  return MSTypeVector<double> (pNewData, vect_.length());
}

MSTypeVector<double> & MSTypeVector<double>::sin()
{
  return allElementsDo (::sin);
}

MSTypeVector<double> & MSTypeVector<double>::cos()
{
  return allElementsDo (::cos);
}

MSTypeVector<double> & MSTypeVector<double>::tan()
{
  return allElementsDo (::tan);
}
  
MSTypeVector<double> & MSTypeVector<double>::asin()
{
  return allElementsDo (::asin);
}

MSTypeVector<double> & MSTypeVector<double>::acos()
{
  return allElementsDo (::acos);
}

MSTypeVector<double> & MSTypeVector<double>::atan()
{
  return allElementsDo (::atan);
}

MSTypeVector<double> & MSTypeVector<double>::sinh()
{
  return allElementsDo (::sinh);
}

MSTypeVector<double> & MSTypeVector<double>::cosh()
{
  return allElementsDo (::cosh);
}

MSTypeVector<double> & MSTypeVector<double>::tanh()
{
  return allElementsDo (::tanh);
}

MSTypeVector<double> & MSTypeVector<double>::exp()
{
  return allElementsDo (::exp);
}

MSTypeVector<double> & MSTypeVector<double>::log()
{
  return allElementsDo (::log);
}

MSTypeVector<double> & MSTypeVector<double>::sqrt()
{
  return allElementsDo (::sqrt);
}

MSTypeVector<double> & MSTypeVector<double>::ceil()
{
  return allElementsDo (::ceil);
}

MSTypeVector<double> & MSTypeVector<double>::floor()
{
  return allElementsDo (::floor);
}

MSTypeVector<double> & MSTypeVector<double>::abs()
{
  return allElementsDo (::fabs);
}

MSTypeVector<double> & MSTypeVector<double>::pow (double exponent_)
{
  return allElementsDo (&MSTypeVector<double>::powFunc, (void *)&exponent_);
}

MSTypeVector<double> sin (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::sin);
}

MSTypeVector<double> cos (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::cos);
}

MSTypeVector<double> tan (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::tan);
}

MSTypeVector<double> asin (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::asin);
}

MSTypeVector<double> acos (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::acos);
}

MSTypeVector<double> atan (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::atan);
}

MSTypeVector<double> sinh (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::sinh);
}

MSTypeVector<double> cosh (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::cosh);
}

MSTypeVector<double> tanh (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::tanh);
}
     
MSTypeVector<double> exp (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::exp);
}

MSTypeVector<double> log (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::log);
}

MSTypeVector<double> sqrt (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::sqrt);
}

MSTypeVector<double> ceil (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::ceil);
}

MSTypeVector<double> floor (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::floor);
}

MSTypeVector<double> abs (const MSTypeVector<double> & vect_)
{
  return MSTypeVector<double>::allElementsDo (vect_, ::fabs);
}

MSTypeVector<double> pow (const MSTypeVector<double> & vect_, double exponent_)
{
  return MSTypeVector<double>::allElementsDo (vect_, &MSTypeVector<double>::powFunc, (void *)&exponent_);
}

// ElementWiseFunction that returns x**y
double MSTypeVector<double>::powFunc (double x_, void *pY_)
{
  return ::pow (x_,*(double *)pY_);
}

