///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <AplusGUI/AplusRadioBox.H>

extern MSBoolean isSlotFiller(A);

AplusRadioButton::AplusRadioButton(MSWidget *owner_) : MSRadioButton((MSRadioBox *)owner_, "")
{
  dynamic(MSTrue);
  //  AplusModel *am = new AplusModel(0);
  //  INTERNAL_COUPLE(am);
}

AplusRadioButton::~AplusRadioButton(void)
{}

void AplusRadioButton::radioBoxArm(void)
{
  AplusRadioBox *rbox=(AplusRadioBox *)owner();
  if (rbox!=0) rbox->arm(this);
}

void AplusRadioButton::radioBoxDisarm(void)
{
  AplusRadioBox *rbox=(AplusRadioBox *)owner();
  if (rbox!=0) rbox->disarm();
}

void AplusRadioButton::radioBoxActivate(void)
{}

void AplusRadioButton::up(void) { if (radioBox()) radioBox()->up(); }
void AplusRadioButton::down(void){ if (radioBox()) radioBox()->down(); }
void AplusRadioButton::left(void){ if (radioBox()) radioBox()->left(); }
void AplusRadioButton::right(void){ if (radioBox()) radioBox()->right(); }
void AplusRadioButton::focusIn(void)
{
  if (radioBox())
    radioBox()->selectedItem(radioBox()->buttons().indexOf((unsigned long)(MSWidget *)this));
  MSRadioButton::focusIn();
}

MSBoolean AplusRadioButton::isProtected(void) const
{
  if (radioBox()->readOnly(radioBox()->buttons().indexOf((unsigned long)(MSWidget *)this))==MSFalse &&
      sensitive()==MSTrue)
    {
      return MSFalse;
    }
  else	// readOnly()==MSTrue || sensitive()==MSFalse
    {
      return MSTrue;
    }
}


AplusRadioBox::AplusRadioBox(MSWidget *owner_) : AplusButtonBox(owner_)
{ _armedButton = 0;}
AplusRadioBox::~AplusRadioBox(void)
{}

void AplusRadioBox::arm(MSRadioButton *radioButton_)
{
  V v = ((AplusModel *) model())->aplusVar();

  if (v)
   {
     disarm();
     _activeButton=radioButton_;
     if (activeButton()!=0) activeButton()->state(MSTrue);
     
     if (armedButton()!=0)
      {
	unsigned buttonIndex = buttons().indexOf((unsigned long)armedButton());
	*un((A *)(un((A *)(un((A *)(&v->a))->p+1))->p+buttonIndex))->p=0;  
	armedButton()->state(MSFalse);
	_armedButton=0;
      }
     radioButton_->state(MSTrue);
     valueChange(radioButton_,MSTrue);
   }
}

void AplusRadioBox::disarm(void)
{
  if (activeButton()!=0) activeButton()->state(MSFalse);
  _activeButton=0;
}

void AplusRadioBox::firstMapNotify(void)
{
  MSNodeItem     *hp=childListHead(); 
  MSNodeItem     *np=hp;
  MSLayoutEntry  *entry;
  MSRadioButton  *radioButton;
  unsigned        count=0;
  while ((np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();	
     radioButton=(MSRadioButton *)entry->widget();
     if (radioButton->state()==MSTrue) 
      {
        if (count==0) _activeButton=radioButton;
        count++;
      }
     if (count>1) radioButton->state(MSFalse);
   }
  if (count==0&&(np=np->next())!=hp)
   {
     entry=(MSLayoutEntry *)np->data();	
     radioButton=(MSRadioButton *)entry->widget();
     radioButton->state(MSTrue);
     _activeButton=radioButton;
   }
  MSActionBox::firstMapNotify();
}

MSActionButton *AplusRadioBox::newButton(void)
{
  return new AplusRadioButton((MSWidget *) this);
}

MSBoolean AplusRadioBox::verifyData(V,A a_)
{ 
  if (isSlotFiller(a_)==MSTrue)
   {
     P p; p.i=a_->p;
     A val=p.a[1];
     A attr=p.a[0];
     P pv; pv.i=val->p;
     int trueCount=0;
     if (val->t==Et&&val->n==0&&attr->t==Et&&attr->n==0) return MSTrue;
     for (int row=0;row<(int)val->n;row++)
      {
	if (QS(pv.a[row])) return MSFalse;
	else if (pv.a[row]->t!=It||pv.a[row]->r!=0) return MSFalse;
	else 
         {
           if ((int)*pv.a[row]->p==1) trueCount++;
	   else if ((int)*pv.a[row]->p!=0) return MSFalse;
	 }
      }
     return (trueCount==0||trueCount>1)?MSFalse:MSTrue;
   }
  return MSFalse;
}

void AplusRadioBox::updateValue(int row_)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
  if (v!=0)
   {
     AplusRadioButton    *btn;
     
     P p; p.i=((AplusModel*)model())->data();
     A val=(A)p.a[1];
     P pv; pv.i=val->p;
     
     btn = (AplusRadioButton *)buttons()(row_);
     if (btn)
      {
	if (*pv.a[row_]->p==0) btn->disarm();
	else 
	 {
           if (armedButton()!=0&&armedButton()!=btn)
	    {
	      unsigned buttonIndex = buttons().indexOf((unsigned long)armedButton());
	      *un((A *)(un((A *)(un((A *)(&v->a))->p+1))->p+buttonIndex))->p=0;
	      /* Watch out!!! p, val and pv are not what they seem */
   	      /* *pv.a[row(_armedBtn)]->p=0; */
 	      armedButton()->state(MSFalse);
	    }
	   _armedButton=btn;
	   btn->state(MSTrue);
	 }
      }
   }
}

void AplusRadioBox::updateData(void)
{
  if (model() && ((AplusModel*) model())->aplusVar())
   {
     int               nr=numRows();
     AplusRadioButton  *btn;
     A                 str;
     MSBoolean         doMap;
     MSBoolean         change=(childCount()==nr)?MSFalse:MSTrue;
     MSBoolean         one = MSFalse;
     V                 v=(model()!=0)?((AplusModel*)model())->aplusVar():0;

     freeze();

     // Set Managed WidgetVector size
     for (;buttons().length() < nr; buttons()<<(unsigned long)0);
     
     // remove extra buttons 
     if (childCount()-nr>0)
      {
	int extra = childCount()-nr;
	for(int i = nr; i < extra+nr; i++)
	{
	  ((MSWidget *)buttons()(i))->destroy();
	  buttons()[i] = 0;
	}
      }

     P p; p.i=((AplusModel*)model())->data();
     A val=(A)p.a[1];
     P pv; pv.i=val->p;

     for (int r=0;r<nr;r++)
      {
	doMap=MSFalse;
	if (r<buttons().length() && buttons()(r)!=0)
	   btn=(AplusRadioButton *) (MSWidget *)buttons()(r);
	else
	 {
	   btn = new AplusRadioButton((MSWidget *)this);
	   buttons()[r]=(unsigned long)(MSWidget *)btn;
	   doMap=MSTrue;
	 }

	btn->selectColor(color(r));
	btn->font(titleFont(r));
	btn->background(background());
	btn->foreground(titleColor(r));
	str = itemLabel(r);
	if (isNull(str)==MSFalse)
         {
           btn->label((char *)str->p);
           dc(str);
         }

	setButtonState(btn,*pv.a[r]->p);
	btn->integerTag(r);
	if (*pv.a[r]->p==1&&one==MSFalse)
	 {
	   _armedButton=btn;
	   btn->state(MSTrue);
	   one=MSTrue;
	 }
	else if (*pv.a[r]->p==1)
	 {
	   *un((A *)(un((A *)(un((A *)(&v->a))->p+1))->p+r))->p=0;
	   p.i=((AplusModel*)model())->data();
	   val=(A)p.a[1];
	   pv.i=val->p;
	   btn->state(MSFalse);
	 }
	else btn->state(MSFalse);

	if (doMap==MSTrue && btn->mapped()==MSFalse) btn->map();
      }
     if (change==MSTrue)  naturalSize();
     unfreeze();
   }
}


void AplusRadioBox::setButtonState(MSActionButton *btn_, unsigned long val_)
{
  AplusRadioButton *abtn = (AplusRadioButton *) btn_;
  if (abtn) (val_==0)?abtn->setDisarmState():abtn->setArmState();
}

void AplusRadioBox::updateForeground(unsigned long)
{
  for (int i = 0; i < buttons().length(); i++)
   {
     AplusRadioButton *btn = (AplusRadioButton *) (MSWidget *)buttons()(i);
     if (btn)
       btn->selectColor(color(i));
   }
}


const MSSymbol& AplusRadioBox::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusRadioBox::symbol(void)
{
  static MSSymbol sym("AplusRadioBox");
  return sym;
}
