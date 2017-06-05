#ifndef MSModelCheckBoxHEADER
#define MSModelCheckBoxHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSGUI/MSCheckBox.H>

class MSGUIExport MSModelCheckBox : public MSCheckBox
{
friend class MSCheckButton;
public:  
  MSModelCheckBox(MSWidget *,const char *title_=0);
  MSModelCheckBox(MSWidget *,const MSStringVector &title_);
  ~MSModelCheckBox(void);

protected:
   virtual void arm(MSCheckButton *);
   virtual void disarm(MSCheckButton *);
   virtual void update(const MSIndexVector &);
   virtual void updateData(void);
   virtual void firstMapNotify(void);

   virtual void drawCurrentState(void)=0;
   virtual void setModel(void)=0;
};

#endif












