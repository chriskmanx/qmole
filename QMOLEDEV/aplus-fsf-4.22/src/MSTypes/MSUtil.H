#ifndef MSUtilHEADER
#define MSUtilHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSEnum.H>

class MSTypesExport MSUtil
{
public:
  static MSBoolean hasAlpha(const char *s_);
  static void comma(const char *from_,char *to_,int size_);

  static void remove(char *s_, int c_);
  static MSBoolean isSpace(const char *s_);
  static MSBoolean isNumeric(const char *s_);

  static int	  abs(int x_)                  { return x_>=0?x_:-x_; }
  static short    abs(short x_)                { return x_>=0?x_:-x_; }
  static long     abs(long x_)                 { return x_>=0?x_:-x_; }
  static double	  abs(double x_)               { return x_>=0?x_:-x_; }
  static int	  max(int a_,int b_)           { return a_>=b_?a_:b_; }
  static unsigned max(unsigned a_,unsigned b_) { return a_>=b_?a_:b_; }
  static double	  max(double a_,double b_)     { return a_>=b_?a_:b_; }
  static int	  min(int a_,int b_)           { return a_<=b_?a_:b_; }
  static unsigned min(unsigned a_,unsigned b_) { return a_<=b_?a_:b_; }
  static double	  min(double a_,double b_)     { return a_<=b_?a_:b_; }

  inline static MSBoolean isEqualTo(double a_, double b_, double tolerance_ =0.00000000000001);
  inline static MSBoolean isNotEqualTo(double a_, double b_, double tolerance_ =0.00000000000001);
  inline static MSBoolean isGreaterThan(double a_, double b_, double tolerance_ =0.00000000000001);
  inline static MSBoolean isGreaterThanOrEqualTo(double a_, double b_, double tolerance_ =0.00000000000001);
  inline static MSBoolean isLessThan(double a_, double b_, double tolerance_ =0.00000000000001);
  inline static MSBoolean isLessThanOrEqualTo(double a_, double b_, double tolerance_ =0.00000000000001);
};


inline MSBoolean MSUtil::isEqualTo(double a_, double b_, double tolerance_)
{
  a_-=b_;
  //
  // If tolerance is negative (which is meaningless), it will always
  // return MSFalse.
  //
  // Otherwise, it will check if both a_ and -a_ (after having done
  // a_-=b) are <= tolerance_:
  //
  // - if a_ is negative (i.e., a_<b_), then it will always be less
  //   than tolerance_ (since tolerance_ is non-negative) and then
  //   -a_ will be compared to tolerance_ to decide if the
  //   difference between the original a_ and b_ is within tolerance_
  //
  // - if a_ is non-negative (i.e., a_>=b_), then -a_ will always be
  //   less than tolerance_ (since tolerance_ is non-negative) and
  //   then a_ will be compared to tolerance_ to decide if the
  //   difference between the original a_ and b_ is within tolerance_.
  //
  // This algorithm is faster than taking the absolute value of a_ and
  // comparing if it is <= tolerance_.
  //
  // Please note that the tolerance interval is *closed*.
  //
  return MSBoolean(a_<=tolerance_ && -a_<=tolerance_);
}


inline MSBoolean MSUtil::isNotEqualTo(double a_, double b_, double tolerance_)
{
  a_-=b_;
  return MSBoolean(a_>=tolerance_ || -a_>=tolerance_);
}


inline MSBoolean MSUtil::isGreaterThan(double a_, double b_, double tolerance_)
{
  a_-=b_;
  return MSBoolean(a_>0 && a_>tolerance_);
}


inline MSBoolean MSUtil::isGreaterThanOrEqualTo(double a_, double b_, double tolerance_)
{
  a_-=b_;
  return MSBoolean(a_>=0 || -a_<=tolerance_);
}


inline MSBoolean MSUtil::isLessThan(double a_, double b_, double tolerance_)
{
  a_-=b_;
  return MSBoolean(a_<0 && -a_>tolerance_);
}


inline MSBoolean MSUtil::isLessThanOrEqualTo(double a_, double b_, double tolerance_)
{
  a_-=b_;
  return MSBoolean(a_<=0 || a_<=tolerance_);
}

#endif //MSUtilHEADER
