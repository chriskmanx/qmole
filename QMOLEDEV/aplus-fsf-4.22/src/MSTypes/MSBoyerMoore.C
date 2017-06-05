///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSBoyerMoore.H>

const unsigned MSBoyerMoore::DeltaTableSize=256;

MSBoyerMoore::MSBoyerMoore(void)
{ _deltaTable=0; }
MSBoyerMoore::MSBoyerMoore(const MSString& sourceString_):_sourceString(sourceString_) 
{ _deltaTable=0; }
MSBoyerMoore::~MSBoyerMoore(void)
{ if (_deltaTable!=0) delete [] _deltaTable; }

void MSBoyerMoore::sourceString(const MSString& sourceString_)
{ _sourceString=sourceString_; }

void MSBoyerMoore::searchPattern(const MSString& searchPattern_)
{
  if (_deltaTable==0) _deltaTable=new unsigned[deltaTableSize()];
  unsigned i;
  if (searchPattern_.length()>0)
   {
     if (searchPattern_!=searchPattern())
      {
	_searchPattern=searchPattern_;
	for(i=0;i<deltaTableSize();i++)  _deltaTable[i]=searchPattern().length();
	for(i=1;i<searchPattern().length();i++) _deltaTable[searchPattern_(i-1)]=searchPattern().length()-i;
	_deltaTable[searchPattern_[searchPattern().length()-1]]=1;
      }
   }  
  else 
   {
     _searchPattern="";     
     for(i=0;i<deltaTableSize();i++) _deltaTable[i]=searchPattern().length();
   }
}

unsigned MSBoyerMoore::indexOf(unsigned startPos_,const char *searchPattern_)
{
  if (startPos_<sourceString().length())
   {
     if (searchPattern_!=0) searchPattern(searchPattern_);
     if (searchPattern().length()>0)
      {
	const char *pat=searchPattern();
	const char *str=sourceString();
	unsigned len=searchPattern().length();    
	unsigned slen=sourceString().length(); 
	unsigned i=startPos_+len;              // index into target
	unsigned j;                            // index into pattern
	
	while (i<=slen)
	 {
	   j=len;
	   while (pat[j-1]==str[i-1])
	    {
	      if (j>1) { --j,--i; }   // move left for next comparison
	      else return i-1;        // we've reached the beginning of pat
	    }
	   i+=delta(str[i-1]);        // move target index by delta of mismatched character
	 }
      }
   }
  return sourceString().length();
}











