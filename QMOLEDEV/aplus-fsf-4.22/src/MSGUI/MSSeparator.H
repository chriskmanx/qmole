#ifndef MSSeparatorHEADER
#define MSSeparatorHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSWidgetCommon.H>

class MSGUIExport MSSeparator : public MSWidgetCommon
{
public:
   int marginWidth(void) const;
   void marginWidth(int);
   int marginHeight(void) const;
   void marginHeight(int);
   int thickness(void) const;
   void thickness(int);
   virtual void naturalSize(void);

   virtual void set(MSAttrValueList&);
   virtual MSAttrValueList& get(MSAttrValueList&);
   
protected:
   MSSeparator(MSWidget *owner_);
   int  _marginWidth;
   int  _marginHeight;
   int  _thickness;

   void init(void);
   virtual void firstMapNotify(void);
   virtual void updateBackground(unsigned long);
};

inline int MSSeparator::marginWidth(void) const
{ return _marginWidth; }
inline int MSSeparator::marginHeight(void) const
{ return _marginHeight; }
inline int MSSeparator::thickness(void) const
{ return _thickness; }
inline void MSSeparator::firstMapNotify(void)
{ computeSize(); }

#endif
