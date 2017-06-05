///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSTitleMenuItem.H>
#include <MSGUI/MSFontObject.H>

MSTitleMenuItem::MSTitleMenuItem(MSMenu *owner_) : 
MSMenuItem(owner_)
{ init(); }

MSTitleMenuItem::MSTitleMenuItem(MSMenu *owner_, const char *title_) : 
MSMenuItem(owner_, title_)
{ init(); }

MSTitleMenuItem::MSTitleMenuItem(MSMenu *owner_, const MSString& title_) : 
MSMenuItem(owner_,title_)
{ init(); }

MSTitleMenuItem::~MSTitleMenuItem(void) {} 

void MSTitleMenuItem::init(void) 
{
  _alignment=MSCenter;
  _mnemonic=0;
  _sensitive=MSFalse;
}

void MSTitleMenuItem::updateSensitivity(void)
{ _sensitive=MSFalse; }

void MSTitleMenuItem::alignment(MSAlignment alignment_)
{
  if(alignment_!= alignment())
   {
     if(alignment_==MSLeft || alignment_==MSCenter)
      {
        _alignment=alignment_;
        redraw();
      }
   }
}

int MSTitleMenuItem::computeXCoord(void)
{
  if(alignment()==MSLeft) return MSMenuItem::computeXCoord();
  else
   {
     int offset = highlightThickness()+shadowThickness()+marginWidth();
     int dw= width()-2*offset;
     const char *pString=label();
     int tw=fontObject()->textWidth(pString, label().length());
     int space =0;
     if(dw <tw) space = indent();
     else space = (dw-tw)/2;
     return offset+x_origin() +space;
   }
}


MSAttrValueList& MSTitleMenuItem::get(MSAttrValueList& avList_)
{
  avList_<<MSAttrValue("alignment",alignment()==MSLeft?"MSLeft":"MSCenter",
                       MSStringVector("MSLeft\nMSCenter"));
  return MSMenuItem::get(avList_);
}


void MSTitleMenuItem::set(MSAttrValueList& avList_)
{
  MSMenuItem::set(avList_);
  MSIndexVector index;
  for(unsigned i=0;i<avList_.length();i++)
   {
     if(avList_[i].attribute()=="alignment")
      {
        if(avList_[i].value()=="MSLeft") alignment(MSLeft);
        else if(avList_[i].value()=="MSCenter") alignment(MSCenter);
        index<<i;
      }
   }
  avList_.remove(index);
}
