///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSGUI/MSStringPopupMenu.H>
#include <MSTypes/MSIndexedEvent.H>

MSStringPopupMenu::MSStringPopupMenu(MSDisplayServer *server_) :
MSPopupMenu(server_)
{}

MSStringPopupMenu::MSStringPopupMenu(MSDisplayServer *server_,MSStringVector& aModel_) :
MSPopupMenu(server_)
{ model(aModel_); }

void MSStringPopupMenu::model(const MSStringVector& aModel_)
{ constCouple(&aModel_); }

void MSStringPopupMenu::updateData(void)
{
  if (MSView::model()!=0)
   {
     freeze();
     const MSStringVector& aStringVector=stringVector();
     unsigned currentCount(itemCount());
     unsigned i;
     MSWidgetVector itemVector(children());
     MSMenuItem *pMenuItem;
     for (i=0;i<aStringVector.length();i++)
      {
	if (i<itemVector.length())
	 {
           pMenuItem=(MSMenuItem *)itemVector(i);
	   pMenuItem->label(aStringVector(i));
	 }
	else pMenuItem=new MSMenuItem(this,aStringVector(i),0,i);
	setItem(pMenuItem,i);
      }
     for (i=aStringVector.length();i<itemVector.length();i++)
      {
	pMenuItem=(MSMenuItem *)itemVector(i);
	delete pMenuItem;
      }
     unfreeze();
     computeSize();
   }
  else removeAllItems();
}

void MSStringPopupMenu::receiveEvent(MSEvent& aEvent_)
{
  if (aEvent_.type()==MSIndexedEvent::symbol())
   {
     MSIndexedEvent& aIndexedEvent=(MSIndexedEvent&)aEvent_;
     const MSIndexVector& aIndexVector=aIndexedEvent.index();
     if (aIndexVector.length()==0) updateData();
     else 
      {
	const MSStringVector& aStringVector=stringVector();
	unsigned currentCount(itemCount());
	if (aStringVector.length()!=currentCount) updateData();
	else
	 {
	   MSMenuItem *pMenuItem;
	   for (unsigned i=0;i<aIndexVector.length();i++)
	    {
	      pMenuItem=menuItem(aIndexVector(i));
	      if (pMenuItem!=0) pMenuItem->label(aStringVector(aIndexVector(i)));
	    }
	 }
      }
   }
  else updateData();
}


