#ifndef MSPHEADER
#define MSPHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


class MSP
{
public:
  MSP(void) {}
  ~MSP(void) {}
  
  enum                  {buflen=128};
  enum DisclaimerStyle  {Text=41,Rule=43,Toprule=44,AppendBox=45,Append=46,Box=64,NoDisclaimer=1};
  enum PageSize         {Letter=1,Legal=2,A4=3,B5=4,A=5,B=6};
  enum PageOrientation  {Portrait=7,Landscape=8,UpsideDown=14,Seascape=15,Default=1};
  enum PrintMode        {Mono=10,Color=11,Colorfg=12,Reverse=13};
  enum OutputMode       {PS=27,EPS=28,PPM=29,ASCII=30};
  enum OutputStyle      {ColMajor=2,RowMajor=4,Aligned=8,NoHeadings=16,Stacked=32,Quoted=64};
  enum BreakProcessMode {Total=2,Minimum=4,Maximum=8,Average=16};
  enum Occurrence       {EveryPage=1,OddPage=2,EvenPage=4,FirstPage=8,LastPage=16,AnyButFirstAndLast=32,
                         IfNextPage=64};
  enum BannerStyle      {Diagonal=128,Repetitive=256};
  enum Style            {Underline=128,DUnderline=256,Superscript=512,Subscript=1024,Outline=2048,
                         Smallcap=4096,Strikethru=8192,BoxL=16384,BoxR=32768,BoxT=65536,BoxB=131072,
                         Cell=262144,Stipple=524288,CharOnly=1048576,PosAbove=2097152,PosBelow=4194304};
};

#endif
