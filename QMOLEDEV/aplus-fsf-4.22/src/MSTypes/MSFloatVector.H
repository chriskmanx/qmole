#ifndef MSFloatVectorHEADER
#define MSFloatVectorHEADER

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

#if defined(MS_NEED_IMPLEMENTATION_PRAGMA)
#pragma implementation("MSFloatVector.C")
#endif

#include <MSTypes/MSString.H>
#include <MSTypes/MSBuiltinTypeVector.H>
#include <MSTypes/MSFloatSPick.H>


#if defined(MS_TEMPLATE_BEFORE_SPECIALIZATION)
#include <MSTypes/MSTypeVector.H>
#endif

template <class Type> class MSTypeVector;

typedef MSBaseVector<double, MSAllocator<double> >    BaseVectorDouble;
typedef MSBuiltinVector<double> BuiltinVectorDouble;

#if defined(MS_VC_NEED_TEMPLATE_EXPORT)
class MSTypesExport MSBaseVector<double, MSAllocator<double> >;
class MSTypesExport MSBaseVectorOps<double, MSAllocator<double> >;
class MSTypesExport MSBuiltinVector<double>;
class MSTypesExport MSBuiltinVectorOps<double>;
#endif


#ifndef MS_MSFloatVector_INSTANTIATE
#ifdef MS_DECLARE_TEMPLATE_SPECIALIZATION
template <> class MSBaseVector<double, MSAllocator<double> >;
template <> class MSBaseVectorOps<double, MSAllocator<double> >;
template <> class MSBuiltinVector<double>;
template <> class MSBuiltinVectorOps<double>;
template <> ostream& operator<<(ostream&,const MSBaseVector<double, MSAllocator<double> >&);
template <> MSTypeVector<double> sin   (const MSTypeVector<double> &);
template <> MSTypeVector<double> cos   (const MSTypeVector<double> &);
template <> MSTypeVector<double> tan   (const MSTypeVector<double> &);
template <> MSTypeVector<double> asin  (const MSTypeVector<double> &);
template <> MSTypeVector<double> acos  (const MSTypeVector<double> &);
template <> MSTypeVector<double> atan  (const MSTypeVector<double> &);
template <> MSTypeVector<double> sinh  (const MSTypeVector<double> &);
template <> MSTypeVector<double> cosh  (const MSTypeVector<double> &);
template <> MSTypeVector<double> tanh  (const MSTypeVector<double> &);
template <> MSTypeVector<double> exp   (const MSTypeVector<double> &);
template <> MSTypeVector<double> log   (const MSTypeVector<double> &);
template <> MSTypeVector<double> sqrt  (const MSTypeVector<double> &);
template <> MSTypeVector<double> ceil  (const MSTypeVector<double> &);
template <> MSTypeVector<double> floor (const MSTypeVector<double> &);
template <> MSTypeVector<double> abs   (const MSTypeVector<double> &);
template <> MSTypeVector<double> pow   (const MSTypeVector<double> &, double);
#endif
#endif


#if defined(MS_C_LINKAGE_INCOMPATIBILITY)
extern "C" 
{
  typedef double (*MathFunction)(double);
}
#else
typedef double (*MathFunction)(double);
#endif
typedef double (*ElementWiseFunction)(double, void *);

#if !defined(MS_MSFloatVector_INSTANTIATE) && defined(MS_STD_TEMPLATE_SPECIALIZATION)
template <> class MSTypeVector<double> : public BuiltinVectorDouble
#else
template <> class MSTypesExport MSTypeVector<double> : public BuiltinVectorDouble
#endif
{
protected:
  // ElementWiseFunction that returns x**y
  INLINELINKAGE static double powFunc (double x_,void *y_);
  
public:
  MSTypeVector ();
  MSTypeVector (unsigned int);
  MSTypeVector (unsigned int, const double &);
  MSTypeVector (const MSTypeVector<double> &);
  MSTypeVector (const BuiltinVectorDouble &);
  MSTypeVector (const BaseVectorDouble &);
  MSTypeVector (const char *);
  MSTypeVector (MSTypeData<double, MSAllocator<double> > *, unsigned int);
  MSTypeVector (const double *, unsigned int);

  virtual ~MSTypeVector();

  MSTypeVector<double> & operator= (const MSTypeVector<double> &);
  MSTypeVector<double> & operator= (const BuiltinVectorDouble &);  
  MSTypeVector<double> & operator= (const BaseVectorDouble &);
  MSTypeVector<double> & operator= (const double &);
  MSTypeVector<double> & operator= (const char *);

  virtual const MSSymbol & type() const;
  virtual MSString className() const;
  virtual MSModel *clone() const;
  virtual MSModel *create() const;

  static const MSSymbol & symbol();


  MSTypeVector<double> & allElementsDo (MathFunction);
  MSTypeVector<double> & allElementsDo (ElementWiseFunction, void * =0);

  static MSTypeVector<double> allElementsDo (const MSTypeVector<double> &, MathFunction);
  static MSTypeVector<double> allElementsDo (const MSTypeVector<double> &, ElementWiseFunction, void * =0);
  
  MSTypeVector<double> & sin();
  MSTypeVector<double> & cos();
  MSTypeVector<double> & tan();
  MSTypeVector<double> & asin();
  MSTypeVector<double> & acos();
  MSTypeVector<double> & atan();
  MSTypeVector<double> & sinh();
  MSTypeVector<double> & cosh();
  MSTypeVector<double> & tanh();
  MSTypeVector<double> & exp();
  MSTypeVector<double> & log();
  MSTypeVector<double> & sqrt();
  MSTypeVector<double> & ceil();
  MSTypeVector<double> & floor();
  MSTypeVector<double> & abs();
  MSTypeVector<double> & pow (double);
  
  friend MSTypesExport MSTypeVector<double> sin   (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> cos   (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> tan   (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> asin  (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> acos  (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> atan  (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> sinh  (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> cosh  (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> tanh  (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> exp   (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> log   (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> sqrt  (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> ceil  (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> floor (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> abs   (const MSTypeVector<double> &);
  friend MSTypesExport MSTypeVector<double> pow   (const MSTypeVector<double> &, double);
};

typedef MSTypeVector<double> MSFloatVector;

#ifndef MS_NO_INLINES
#include <MSTypes/MSFloatVectorInlines.C>
#endif  // MS_NO_INLINES

// We provide provide explicit "do_not_instantiate" pragmas here to avoid instantiation complications in
// MStk applications.
//
#if !defined(__MSTYPESLIBRARY_BUILD__) && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)
#pragma do_not_instantiate MSAllocator<double>
#pragma do_not_instantiate MSTypeData<double, MSAllocator<double> >
#pragma do_not_instantiate MSBaseVector<double, MSAllocator<double> >
#pragma do_not_instantiate MSBaseVectorOps<double, MSAllocator<double> >
#pragma do_not_instantiate MSBuiltinVector<double>
#pragma do_not_instantiate MSBuiltinVectorOps<double>
#pragma do_not_instantiate ostream& operator<<(ostream&, const MSBaseVector<double, MSAllocator<double> >&)
#pragma do_not_instantiate unsigned int msMergeSortUp(unsigned int, double *, unsigned int *,unsigned int, unsigned int)
#pragma do_not_instantiate unsigned int msMergeSortDown(unsigned int, double *, unsigned int *,unsigned int, unsigned int)
#endif // !__MSTYPESLIBRARY_BUILD__ && defined(MS_DO_NOT_INSTANTIATE) && defined(MS_EDG_TEMPLATE_INSTANTIATION)

#endif // MSFloatVectorHEADER
