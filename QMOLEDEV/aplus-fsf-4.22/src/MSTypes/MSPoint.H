#ifndef MSPointHEADER
#define MSPointHEADER

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
#pragma implementation("MSPoint.C")
#endif

#if defined(MS_PREDECLARE_TEMPLATE_FRIENDS)
template <class X, class Y> class MSPoint;
template <class X, class Y> ostream& operator<<(ostream&, const MSPoint<X,Y>&);
#endif  //MS_PREDECLARE_TEMPLATE_FRIENDS

template <class X, class Y>
class MSPoint
{
public:
  inline MSPoint(const X&,const Y&);

  inline X& x(void);
  inline Y& y(void);
  inline const X& x(void) const;
  inline const Y& y(void) const;

  friend MSTypesExport ostream& operator<<(ostream& aStream_,const MSPoint<X,Y>& aPoint_);

protected:
  X _x;
  Y _y;
};

//--------------------
// MSPoint inlines
//--------------------
template<class X,class Y> 
inline MSPoint<X,Y>::MSPoint(const X& x_,const Y& y_)
{ _x=x_,_y=y_; }

template<class X,class Y> 
inline const X& MSPoint<X,Y>::x(void) const
{ return _x; }

template<class X,class Y> 
inline const Y& MSPoint<X,Y>::y(void) const
{ return _y; }

template<class X,class Y> 
inline X& MSPoint<X,Y>::x(void)
{ return _x; }

template<class X,class Y> 
inline Y& MSPoint<X,Y>::y(void)
{ return _y; }

#endif
