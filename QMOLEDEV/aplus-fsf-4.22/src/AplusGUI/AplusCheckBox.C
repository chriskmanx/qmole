///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusCheckBox.H>

extern MSBoolean isSlotFiller(A);

AplusCheckButton::AplusCheckButton(MSWidget *owner_) : MSCheckButton(owner_, "")
{
 dynamic(MSTrue);
 // AplusModel *am = new AplusModel(0);
 // INTERNAL_COUPLE(am);
}

AplusCheckButton::~AplusCheckButton(void)
{}

void AplusCheckButton::checkBoxArm(void)
{
  AplusCheckBox *cbox=(AplusCheckBox *)owner();
  if (cbox!=0) cbox->arm(this);

}

void AplusCheckButton::checkBoxDisarm(void)
{
  AplusCheckBox *cbox=(AplusCheckBox *)owner();
  if (cbox!=0) cbox->disarm(this);
}

void AplusCheckButton::up(void) { if (checkBox()) checkBox()->up(); }
void AplusCheckButton::down(void){ if (checkBox()) checkBox()->down(); }
void AplusCheckButton::left(void){ if (checkBox()) checkBox()->left(); }
void AplusCheckButton::right(void){ if (checkBox()) checkBox()->right(); }
void AplusCheckButton::focusIn(void)
{
  if (checkBox())
    checkBox()->selectedItem(checkBox()->buttons().indexOf((unsigned long)(MSWidget *)this));
  MSCheckButton::focusIn();
}

MSBoolean AplusCheckButton::isProtected(void) const
{
  if (checkBox()->readOnly(checkBox()->buttons().indexOf((unsigned long)(MSWidget *)this))==MSFalse &&
      sensitive()==MSTrue)
    {
      return MSFalse;
    }
  else	// readOnly()==MSTrue || sensitive()==MSFalse
    {
      return MSTrue;
    }
}

AplusCheckBox::AplusCheckBox(MSWidget *owner_) : AplusButtonBox(owner_)
{}
AplusCheckBox::~AplusCheckBox(void)
{}

void AplusCheckBox::arm(MSCheckButton *checkButton_)    
{
  valueChange(checkButton_, MSTrue);
}
void AplusCheckBox::disarm(MSCheckButton *checkButton_) 
{
  valueChange(checkButton_, MSFalse);
}

const MSSymbol& AplusCheckBox::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusCheckBox::symbol(void)
{
  static MSSymbol sym("AplusCheckBox");
  return sym;
}


MSBoolean AplusCheckBox::verifyData(V,A a_)
{ 
  if (isSlotFiller(a_)==MSTrue)
   {
     P p; p.i=a_->p;
     A val=p.a[1];
     P pv; pv.i=val->p;
     for (int row=0;row<(int)val->n;row++)
      {
	if (QS(pv.a[row])) return MSFalse;
	else if (pv.a[row]->t!=It||pv.a[row]->r!=0) return MSFalse;
	else if ((int)*pv.a[row]->p<0||(int)*pv.a[row]->p>1) return MSFalse;
      }
     return MSTrue;
   }
  return MSFalse;
}

void AplusCheckBox::updateValue(int row_)
{
  V v=(model()==0)?0:((AplusModel*)model())->aplusVar();

  if (v!=0)
   {
     P p; p.i=((AplusModel*)model())->data();
     A val=(A)p.a[1];
     P pv; pv.i=val->p;

     MSCheckButton *btn=(MSCheckButton *)(MSWidget *)buttons()(row_);
     if (btn!=0)
       (*pv.a[row_]->p==0)?btn->state(MSFalse):btn->state(MSTrue);
   }
}

MSActionButton *AplusCheckBox::newButton(void)
{
  return new AplusCheckButton((MSWidget *)this);
}

void AplusCheckBox::setButtonState(MSActionButton *btn_, unsigned long val_)
{
  AplusCheckButton *abtn = (AplusCheckButton *) btn_;
  if (abtn) (val_==0)?abtn->setDisarmState():abtn->setArmState();
}

void AplusCheckBox::updateForeground(unsigned long)
{
  for (int i = 0; i < buttons().length(); i++)
   {
     AplusCheckButton *btn = (AplusCheckButton *) (MSWidget *)buttons()(i);
     if(btn) 
       btn->selectColor(color(i));
   }
}
