///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusConvert.H>

extern long dbg_tmstk;

MSA AplusConvert::_convert;

MSFloat AplusConvert::asMSFloat(A var_)
{
  ic(var_);
  _convert.aStructPtr((MSA::a *) var_);
  MSFloat result = _convert.asMSFloat();
  _convert.aStructPtr((MSA::a *)0);
  return result;
}

MSString AplusConvert::asMSString(A var_)
{
  MSString result;
  if (QA(var_) && var_->t==Ct)
    {
      if (var_->r==0)
	{
	  P p; p.i=var_->p;
	  result = MSString(p.c);
	}
      else if (var_->r==1 && var_->n>0)
	{
	  P p; p.i=var_->p;
	  result = p.c;
	}
    }
  else if (!QS(var_)&&(var_->t==Et&&var_->n>0&&QS(*var_->p)))
   {
     result = (char *) XS(var_->p[0])->n;
   }
  return result;
}

//
// Vector Conversions
//

MSStringVector AplusConvert::asMSStringVector(A var_)
{
  MSStringVector result;
  
  if (isNull(var_)==MSTrue) goto out;
  
  if (var_->t == Ct)
   {
     if (var_->r<=1)
      {
	result << MSString((char *)var_->p, (unsigned) var_->n);
      }
     else if (var_->r==2)
      {
	unsigned dim = var_->d[0];
	unsigned len = var_->d[1];
	int offset=0;
	for (int i = 0; i < dim; i++, offset+=len)
	 {
	   result << MSString(((char *)var_->p + offset), len);
	 }
      }
   }
  else if (var_->t ==Et)
   {
     if (QS(*var_->p))
      {
	goto out;
      }

     for (int i = 0 ; i < var_->n; i++)
      {
	A a = (A) var_->p[i];
	if (a && a->t == Ct)
	 {
	   MSStringVector t = AplusConvert::asMSStringVector(a);
	   result << t;
	 }
      }
   }
  else
    {
      if (dbg_tmstk) cerr << "Non charType in AplusConvert::asMSStringVector" << endl;
    }
  
 out:
  if (result.maxLength()==0) result.removeAll();
  
  return result;
}

MSFloatVector AplusConvert::asMSFloatVector(A var_)
{
  MSFloatVector result;
  if (var_->t==Ft)
   {
     ic(var_);
     _convert.aStructPtr((MSA::a *) var_);
     result = _convert.asMSFloatVector();
     _convert.aStructPtr((MSA::a *)0);
   }
  else if (var_->t==It)
   {
     ic(var_);
     _convert.aStructPtr((MSA::a *) var_);
     MSUnsignedLongVector uv = _convert.asMSUnsignedLongVector();
     _convert.aStructPtr((MSA::a *)0);
     for (unsigned i =0; i < uv.length();i++)
       result << (double)uv(i);
   }
  return result;
}

MSIntVector AplusConvert::asMSIntVector(A var_)
{
  ic(var_);
  _convert.aStructPtr((MSA::a *) var_);
  MSIntVector result = _convert.asMSIntVector();
  _convert.aStructPtr((MSA::a *)0);
  return result;
}

MSUnsignedLongVector AplusConvert::asMSUnsignedLongVector(A var_)
{
  ic(var_);
  _convert.aStructPtr((MSA::a *) var_);
  MSUnsignedLongVector result = _convert.asMSUnsignedLongVector();
  _convert.aStructPtr((MSA::a *)0);
  return result;
}

MSIndexVector AplusConvert::asMSIndexVector(A var_)
{
  MSUnsignedLongVector uv = asMSUnsignedLongVector(var_);
  MSIndexVector iv;
  for(unsigned i =0; i < uv.length(); i++) iv << uv(i);
  return iv;
}

MSUnsignedVector AplusConvert::asMSUnsignedVector(A var_)
{
  MSUnsignedLongVector uv = asMSUnsignedLongVector(var_);
  MSUnsignedVector iv;
  for(unsigned i =0; i < uv.length(); i++) iv << (unsigned int)uv(i);
  return iv;
}




// Matrix Conversions

MSFloatMatrix  AplusConvert::asMSFloatMatrix(A var_)
{
  ic(var_);
  _convert.aStructPtr((MSA::a *) var_);
  MSFloatMatrix result = _convert.asMSFloatMatrix();
  _convert.aStructPtr((MSA::a *)0);
  return result;
}
MSIntMatrix    AplusConvert::asMSIntMatrix(A var_)
{
  ic(var_);
  _convert.aStructPtr((MSA::a *) var_);
  MSIntMatrix result = _convert.asMSIntMatrix();
  _convert.aStructPtr((MSA::a *)0);
  return result;
}


// A Conversions


A AplusConvert::asA(const MSString &s_)
{
  A a = aplus_nl;
  long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  if (s_.length()>0)
   {
     d[0] = s_.length();
     a = gc(Ct, 1, s_.length(), d, (long *) (const char *) s_);
   }
  return a;
}

A AplusConvert::asA(const MSFloat &f_)
{
  A a = aplus_nl;
  a = gf((float)f_);
  return a;
}

A AplusConvert::asA(const MSStringVector &sv_)
{ 
  A ap = aplus_nl;
  long d[MAXR]={ 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  long xrho=sv_.length();
  d[0]=xrho;
  if (xrho>0)
   {
     ap = ga(Et, 1, xrho, d);
     for (long i=0; i<xrho; i++)
      {
	d[0]=sv_(i).length();
	ap->p[i]=(long) gc(Ct, 1, sv_(i).length(), d, (long *)sv_(i).string());
      }
   }
  return ap;
}

A AplusConvert::asA(const MSFloatVector &fv_)
{
  A ap = aplus_nl;
  long xrho=fv_.length();
  if (xrho>0)
   {
     ap = gv(Ft, xrho);
     P p; p.i = ap->p;
     for (long i=0; i<xrho; i++) p.f[i] = (double) fv_(i);
   }
  return ap;
}

A AplusConvert::asA(const MSUnsignedVector &uv_)
{
  A ap = aplus_nl;
  long xrho=uv_.length();
  if (xrho>0)
   {
     ap = gv(It, xrho);
     P p; p.i = ap->p;
     for (long i=0; i<xrho; i++) p.i[i] = uv_(i);
   }
  return ap;
}


A AplusConvert::asA(const MSIndexVector &iv_)
{
  A ap = aplus_nl;
  long xrho=iv_.length();
  if (xrho>0)
   {
     ap = gv(It, xrho);
     P p; p.i = ap->p;
     for (long i=0; i<xrho; i++) p.i[i] = iv_(i);
   }
  return ap;
}
