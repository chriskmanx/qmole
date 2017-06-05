#ifndef MSMergeSortIMPLEMENTATION
#define MSMergeSortIMPLEMENTATION

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <limits.h>

template<class Type>
inline unsigned int msIndexCompareUp(Type *p_,unsigned int i_,unsigned int j_)
{ return (p_[i_]!=p_[j_])?((p_[i_])<(p_[j_])):(i_<j_); }

template<class Type>
inline unsigned int msIndexCompareDown(Type *p_,unsigned int i_,unsigned int j_)
{ return (p_[i_]!=p_[j_])?((p_[j_])<(p_[i_])):(i_<j_); }

template<class Type>
unsigned int msMergeSortUp(unsigned int n_,Type *sp_,unsigned int *p_,unsigned int low_,unsigned int high_)
{
  unsigned int t,m=(low_+high_+1)>>1;
  if (high_==m) {p_[low_]=UINT_MAX;return low_;}
  high_=msMergeSortUp(n_,sp_,p_,m,high_);
  low_=msMergeSortUp(n_,sp_,p_,low_,m);
  if (msIndexCompareUp(sp_,high_,low_)) {m=low_;low_=high_;high_=m;}
  for (t=low_;;low_=p_[low_]=high_,high_=m)
   {
L:m=p_[low_];
    if (UINT_MAX==m) {p_[low_]=high_;return t;}
    if (msIndexCompareUp(sp_,m,high_)) {low_=m;goto L;}
   }
}

template<class Type>
unsigned int msMergeSortDown(unsigned int n_,Type *sp_,unsigned int *p_,unsigned int low_,unsigned int high_)
{
  unsigned int t,m=(low_+high_+1)>>1;
  if (high_==m) {p_[low_]=UINT_MAX;return low_;}
  high_=msMergeSortDown(n_,sp_,p_,m,high_);
  low_=msMergeSortDown(n_,sp_,p_,low_,m);
  if (msIndexCompareDown(sp_,high_,low_)) {m=low_;low_=high_;high_=m;}
  for (t=low_;;low_=p_[low_]=high_,high_=m)
   {
L:m=p_[low_];
    if (UINT_MAX==m) {p_[low_]=high_;return t;}
    if (msIndexCompareDown(sp_,m,high_)) {low_=m;goto L;}
   }
}

#endif
