///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSEventBlocker.H>
#include <AplusGUI/AplusChoice.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/Macros.H>

extern long dbg_tmstk;

AplusChoice::AplusChoice(MSWidget *widget_) : MSOptionMenu(widget_)
{
  _itemVal = aplus_nl;
  dynamic(MSTrue);
  AplusModel *am = new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusChoice::~AplusChoice(void)
{
  dc(_itemVal);
}

MSBoolean AplusChoice::verifyData(V, A a_) 
{ 
  MSBoolean r=MSFalse;
  if (0!=a_&&QA(a_)&&a_->t==Et&&a_->n==2&&a_->r==1)
   {
     P p; p.i=a_->p;
     A val=p.a[1];
     A attr=p.a[0];
     int row;
     if (val->t==Et&&val->n==0&&attr->t==Et&&attr->n==0) r=MSTrue;
     else if (val->n==attr->n&&val->t==Et&&attr->t==Et&&val->r<=1)
      {
	r=MSTrue;
	for (row=0;row<(int)attr->n&&r==MSTrue;row++)
	 {
	   if (!QS(attr->p[row])) r=MSFalse;
	 }
	if (r==MSTrue)
	 {
	   P pv; pv.i=val->p;
           int trueCount=0;
	   for (row=0;row<(int)val->n&&r==MSTrue;row++)
	    {
              if (QS(pv.a[row])) r=MSFalse;
              else if (pv.a[row]->t!=It||pv.a[row]->r!=0) r=MSFalse;
              else
	       {
		 if ((int)*pv.a[row]->p==1) trueCount++;
		 else if ((int)*pv.a[row]->p!=0) r=MSFalse;
	       }
	    }
           if (trueCount==0||trueCount>1) r=MSFalse;
	 }
	else if (val->t==Et&&val->n==0&&attr->t==Et&&attr->n==0) r=MSTrue;
        else r=MSFalse;
      }
   }
  return r;
}

void AplusChoice::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel*)m_));
  setWidth();
  updateTitle();
  updateData();
}


void AplusChoice::receiveEvent(MSEvent &event_)
{
  const MSSymbol& eventType=event_.type();

  if (eventType==AplusEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateEvent in AplusChoice"  << endl;
      AplusEvent *ave = (AplusEvent *) &event_;
      V v     = model()->aplusVar();
      A index = ave->index();
      A pick  = ave->pick();
      I ravel = ave->ravel();;
      update(v,index, pick, ravel);
    }
  else if (eventType==AplusVerifyEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received VerifyEvent in AplusChoice"  << endl;
      
      AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
      ave->result(verifyData(ave->aplusVar(), ave->a()));
    }
  else if (eventType==AplusUpdateDataEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateDataEvent in AplusChoice"  << endl;
      setWidth();
    }
  else if (eventType==AplusUpdateTitleEvent::symbol())
    {
      if (dbg_tmstk) cout << "Received UpdateTitleEvent in AplusChoice"  << endl;
      updateTitle();
    }
}

void AplusChoice::updateData(void)
{
  if (model()!=0)
   {
     V v = model()->aplusVar();

     if (v)
      {
	P p; p.i =model()->data();
	updateModelVector(p.a[0]);
      }
     else
      {
	MSEventBlocker block(&_modelBuffer);
	modelBuffer().removeAll();
      }
     optionsModel(modelBuffer());
     updateOptions();
   }
}

void AplusChoice::updateModelVector(A attr_)
{
  if (attr_!=0)
    {
      int itemCount = ((int)attr_->n);
      A sym;
  
      MSEventBlocker block(&_modelBuffer);
      _modelBuffer.removeAll();
      _modelBuffer.reshape(itemCount);

      for (unsigned int i=0; i<itemCount; i++)
	{
	  sym=(A)attr_->p[i];
	  _modelBuffer.replaceAt(i,((char *)((S)XS(sym))->n));
	}
    }
}

extern "C" A gpix(A,A);

void AplusChoice::update(V v_,A,A pick_,I)
{ 
  V v = (model()!=0)?model()->aplusVar():0;

  if (pick_==0) updateData();
  else
   {
     if (QA(pick_))
      {
        A pick=(A)gpix(pick_,(A)v_->a);
        if (pick!=0&&QA(pick)&&pick->t==It&&pick->r<=1)
         {
           if (pick->r==1)
            {
     	 if (pick->n>0&&selectedItem()>=0) 
     	  {
     	    P p; p.i=model()->data();
     	    P pv; pv.i=p.a[1]->p;
     	    int r;
     	    for (int i=0;i<pick->n;i+=2)
     	     {
     	       r=(int)pick->p[i+1];
     	       if (r!=selectedItem()&&*pv.a[r]->p==1)
     		{
     		  *un((A *)(un((A *)(un((A *)(&v->a))->p+1))->p+selectedItem()))->p=0;
     		}
     	     }
     	  }
            }
           else
            {
     	 if (pick->p[0]==1) 
     	  {
     	    P p; p.i=model()->data();
     	    P pv; pv.i=p.a[1]->p;
	    int ni=numberOfItems();
     	    MSBoolean one=MSFalse;
     	    for (int i=0;i<ni;i++)
     	     {
     	       if (*pv.a[i]->p==1)
     		{
     		  if (one==MSFalse) one=MSTrue;
     		  else *un((A *)(un((A *)(un((A *)(&v->a))->p+1))->p+i))->p=0;
     		}
     	     }
     	  }
     	 else updateData();
            }
         }
        else if (pick==0) cerr<<"choice: pick assignment error in update."<<endl;
        else cerr<<"choice: pick assignment error in update."<<endl;
        if (pick!=0) dc(pick);
      }
     setChoice();
   }
}

void AplusChoice::updateFont(Font oldfid_)
{
  MSComposite::updateFont(oldfid_);
  _fieldValue->font(font());
  if (dynamic()==MSTrue) computeSize();
  else redraw();
}

			   

void AplusChoice::updateTitle(void)
{
  V v = (model()!=0)?model()->aplusVar():0;
  
  // UpdateTitle - OptionMenu uses label rather than title
  if (v!=0)
   {
     AVariableData *varData = pAVarDataFromV(v);
     A str=varData->title();
     if (isNull(str)==MSFalse && str->t==Ct)
      {
	label((char*)str->p);
      }
    
     // UpdateTitleFont - OptionMenu uses label rather than title

     Font fid = varData->titleFont();
     if (fid && fid != labelFont())
      {
	labelFont(fid);
      }

     // UpdateTitleForeground - OptionMenu uses label rather than title
     
     unsigned long fg = titleForeground();
     labelForeground(fg);
   }
  updateOptions();
}

void AplusChoice::updateOptions(void)
{
  V v = (model()!=0)?model()->aplusVar():0;
  A a = (model()!=0)?model()->a():0;
  int n = model()->numElmts();

  if (v!=0 && n==2 && optionMenu()!=0)
   {
     AOutFunction *titleFunc=AplusModel::getTitleFunc(v);
     AFontFunction *titleFontFunc=AplusModel::getTitleFontFunc(v);
     AColorFunction *titleColorFunc=AplusModel::getTitleColorFunc(v);
     unsigned long fg;
     MSMenuItem *mi;
     A outStr;
     P p; p.i=model()->data();
     A as=p.a[0];
     A av=p.a[1];
     A attr,val;
     Font fid=(titleFontFunc!=0)?titleFontFunc->invoke(v,a):dataFont();
     for (int i=0;i<numberOfItems();i++)
      {
	attr=(A)as->p[i];  // used as the pick
	val=(A)av->p[i];
        mi=optionMenu()->taggedMenuItem(i);
	outStr=(titleFunc!=0)?titleFunc->invoke(v,(A)val,(A)attr):aplus_nl;   
	fg=(titleColorFunc!=0)?
	  titleColorFunc->invoke(v,(A)val,(A)attr):foreground();
        mi->foreground(fg);
	if (outStr->t==Ct) mi->label((char *)outStr->p);
	if (isNull(outStr)==MSFalse) dc(outStr);
      }
     if (optionMenu()->font()!=fid) optionMenu()->font(fid);
     else optionMenu()->naturalSize();
     setChoice();
   }
}

Font AplusChoice::dataFont(void)
{
  V v = (model()!=0)?model()->aplusVar():0;
  A a = (model()!=0)?model()->a():0;
  if (v)
   {
     AFontFunction *fontFunc=AplusModel::getFontFunc(v);
     if (fontFunc!=0) return (Font) fontFunc->invoke(v,a);   
   }
  return font();
}


void AplusChoice::setChoice(void)
{
  V v = model()->aplusVar();
  
  if (v!=0)
   {
     P p; p.i=model()->data();
     A val=p.a[1];
     P pv; pv.i=val->p;
     int i;
     for(i=0;i<val->n;i++)
      {
	if ((int)*pv.a[i]->p==1) 
         {
           selectedItem(i);
           break;
         }
      }
   }
}

void AplusChoice::activate(int choice_)
{
  extern int safeAset(V,A,A,A);
  V v = (model()!=0)?model()->aplusVar():0;

  if (v&&choice_>=0&&choice_<numberOfItems()&&selectedItem()!=choice_)
   {
     busyOn();
     if (selectedItem()>=0&&selectedItem()<numberOfItems())
      { *un((A *)(un((A *)(un((A *)(&v->a))->p+1))->p+selectedItem()))->p=0; }
     P p; p.i=model()->data();
     A as=p.a[0];
     A attr=(A)as->p[choice_];  // used as the pick
     A pick=(A)gs(Et);
     *pick->p=(I)attr;
     A data=(A)gi(1);
     selectedItem(-1);
     freeze();
//     MSDeleteQueue::allowDelete(MSFalse);
     data=(A)ic(data);
     if (safeAset(v,data,0,pick)==0) showError(qs);
     else
     {
       model()->doneCB(v, data, 0, pick);
     }
     dc(data);
     dc(pick);
     unfreeze();
     redraw();
//     MSDeleteQueue::allowDelete(MSTrue);
     busyOff();
   }
}


A AplusChoice::itemValue(int row_)
{
  V v = (model()!=0)?model()->aplusVar():0;
  int n = (model()!=0)?model()->numElmts():0;

  if (v!=0&&n==2)
   {
     AOutFunction *titleFunc=AplusModel::getTitleFunc(v);
     if (titleFunc!=0)
      {
	P p; p.i=model()->data();
	A as=p.a[0];
	A av=p.a[1];
        if (row_>=0&&row_<(int)av->n)
	 {
	   _itemVal=(A)titleFunc->invoke(v,(A)av->p[row_],(A)as->p[row_]);   
	   _itemVal=(_itemVal->t==Ct)?_itemVal:aplus_nl;
	 }
      }
   }
  return _itemVal;
}


unsigned long AplusChoice::itemForeground(unsigned row_)
{
  unsigned long fg = foreground();
  V v = (model()!=0)?model()->aplusVar():0;
  int n = (model()!=0)?model()->numElmts():0;
  if (v!=0 && n ==2)
   {
     AColorFunction *fgFunc=AplusModel::getFgFunc(v);
     if (fgFunc!=0)
      {
	P p; p.i=model()->data();
	A as=p.a[0];
	A av=p.a[1];
        if (row_>=0&&row_<(int)av->n)
	  fg = fgFunc->invoke(v,(A)av->p[row_], (A) as->p[row_]);
      }
   }
  return fg;
}


const char *AplusChoice::formatOutput(MSString& str_)
{
  static const char blank[]={" "};
  A outStr = itemValue(selectedItem());
  str_ = (Ct==outStr->t) ? (char *)outStr->p : blank;
  return str_;
}

const char *AplusChoice::itemLabel(unsigned row_)
{
  A outStr = itemValue(row_);
  return (char *) outStr->p;
}


void AplusChoice::updateForeground(unsigned long oldfg_)
{
  MSComposite::updateForeground(oldfg_);
  _fieldValue->foreground(foreground());
  redraw();
}


MSBoolean AplusChoice::isProtected(void) const
{
  V v=(model()==0)?0:model()->aplusVar();
  int n=(model()==0)?0:model()->numElmts();
  AVariableData *varData = pAVarDataFromV(v);  
  AReadOnlyFunction *roFunc=AplusModel::getReadOnlyFunc(v);
  MSBoolean ro=(varData!=0)?varData->readOnly():MSFalse;

  if (roFunc!=0&&n==2)
   {
     P p; p.i=model()->data();
     A av=p.a[1];
     //A as=p.a[0];
     //	A attr=(A)as->p;  // used as the pick
     A val =(A)av->p;
     ro=(MSBoolean) roFunc->invoke(v,val,aplus_nl);
   }

  return ro;
}


void AplusChoice::setWidth(void)
{
  AplusModel *pModel = (AplusModel *)model();

  if (pModel!=0 && pModel->aplusVar()!=0)
    {
      int width = pModel->pAVarData()->colWidth();
      if (width<0)	// width has not been set
	{
	  // Set value width to 0, which will set it
	  // to the length of the longest option string
	  //
	  valueWidth(0);
	}
      else	// width has already been set
	{
	  // Set the value width to the value of the
	  // persistent "space" attribute
	  //
	  valueWidth((unsigned int)width);
	}
    }
}  


MSBoolean AplusChoice::hasModel(void) const
{
  // Override hasModel() method to fool MSOptionMenu to believe that it has no model.
  // An MSString model support was added to MSOptionMenu in MStk 2.7 to be able to
  // have an automatically synchronized current selection.
  //
  return MSFalse;
}


const MSSymbol& AplusChoice::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusChoice::symbol(void)
{
  static MSSymbol sym("AplusChoice");
  return sym;
}
