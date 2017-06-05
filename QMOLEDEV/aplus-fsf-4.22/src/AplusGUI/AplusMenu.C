///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <MSTypes/MSTime.H>
#include <a/ik.h>
#include <AplusGUI/AplusMenu.H>

extern long dbg_tmstk;

///////////////////////////////////////////////////////////////////////////////

static const int AplusMenuItemDefaultMarginHeight = 0;

AplusMenu::AplusMenu(MSWidget *widget_, AplusMenu::Orientation orientation_) : MSMenuBar(widget_)
{
  orientation(orientation_);
  font(widget_->font());
  AplusModel *am = new AplusModel(0);
  INTERNAL_COUPLE(am);
}

AplusMenu::~AplusMenu(void)
{}


MSBoolean AplusMenu::verifyData(V,A a_)
{
  return isSlotFiller(a_);
}

void AplusMenu::addSenderNotify(MSEventSender *m_)
{
  INTERNAL_COUPLE(((AplusModel*)m_));
  updateData();
}

void AplusMenu::receiveEvent(MSEvent &event_)
{
  if (event_.type() == AplusEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received UpdateEvent in AplusMenu"  << endl;
     AplusEvent *ave = (AplusEvent *) &event_;
     V v     = ((AplusModel *)model())->aplusVar();
     A index = ave->index();
     A pick  = ave->pick();
     I ravel = ave->ravel();;
     update(v,index, pick, ravel);
   }
  if (event_.type() == AplusVerifyEvent::symbol())
   {
     if (dbg_tmstk) cout << "Received VerifyEvent in AplusMenu"  << endl;

     AplusVerifyEvent *ave = (AplusVerifyEvent *) &event_;
     ave->result(verifyData(ave->aplusVar(), ave->a()));
   }
}

void AplusMenu::updateData(void)
{
  V v = (model()!=0)?((AplusModel*)model())->aplusVar():0;
 
  if (v!=0)
   {
     P p; p.i=((AplusModel*)model())->data();
     A attr=p.a[0];
     A val=p.a[1];
     removeAllItems();
     freeze();
     buildMenu(attr,val);
     unfreeze();
     computeSize();
     redraw();
   }
} 

void AplusMenu::buildMenu(A attr_, A val_)
{
  if (attr_!=0&&val_!=0)
   {
     MSMenuBarItem     *mi;
     MSBoolean          cascade;
     A value;
     A sym;
     S s;
     S *syms=new S[1];
     
     int itemCount = (int)attr_->n;
     for (int i=0;i<itemCount;i++)
      {
        sym=(A)attr_->p[i];  // used as the pick
        value=(A)val_->p[i];
        s=XS(sym);
        cascade=isSlotFiller(value);
        mi=newMenuBarItem((char*)s->n);
        if (cascade==MSTrue)
         {
           syms[0]=s;
           buildPullDown(mi, value,syms,1);
	 }
      }
     delete [] syms;
   }
}

MSMenuBarItem *AplusMenu::newMenuBarItem(const char *title_)
{
  MSMenuBarItem *pItem = new MSMenuBarItem(this,title_);
  pItem->font(font());
  pItem->marginHeight(AplusMenuItemDefaultMarginHeight);
  return pItem;
}

void AplusMenu::buildPullDown(MSMenuBarItem *menuHead_, A data_, S *syms_,
			      int numSyms_)
{
  if (isSlotFiller(data_)==MSTrue)
   {
     MSPulldownMenu *pd = new MSPulldownMenu(menuHead_);
     pd->font(menuHead_->font());
     buildCascades(pd, data_, syms_, numSyms_);
   }
}

void AplusMenu::buildCascadePullDown(MSCascadeMenuItem *menuHead_,A data_,
				     S *syms_,int numSyms_)
{
  if (isSlotFiller(data_)==MSTrue)
   {
     MSCascadeMenu *pd = new MSCascadeMenu(menuHead_);
     pd->font(menuHead_->font());
     buildCascades(pd, data_, syms_, numSyms_);
   }
}

void AplusMenu::buildCascades(MSMenu *menuHead_,A data_,S *syms_,int numSyms_)
{
  if (isSlotFiller(data_)==MSTrue)
   {
     MSCascadeMenuItem *mci;
     MSMenuItem *mi;
     MSBoolean cascade;
     S s;
     P p; p.i=data_->p;
     A attr=p.a[0];
     A val=p.a[1];
     A sym;
     A value;
     S *syms=new S[numSyms_+1];
     int i;

     for(i=0;i<numSyms_;i++)
     {
       syms[i]=syms_[i];              // set up symbol list for next cascade;
     }
     int itemCount = (int)attr->n;
     for (i=0;i<itemCount;i++)
     {
       sym=(A)attr->p[i];  // used as the pick;
       value=(A)val->p[i];
       s=XS(sym);
       cascade=isSlotFiller(value);
       if (cascade==MSTrue)
       {
	 mi=mci=new MSCascadeMenuItem(menuHead_, (char *)s->n);
	 syms[numSyms_]=s;
	 buildCascadePullDown(mci, value, syms, numSyms_+1);
       }
       else 
	 mi=new MSMenuItem(menuHead_, (char *)s->n);
       mi->marginHeight(AplusMenuItemDefaultMarginHeight);
     }
     delete [] syms;
   }
}

void AplusMenu::update(V,A,A,I)
{
  removeAllItems();
  updateData();
}


void AplusMenu::formSymbolVector(MSStringVector &sv_)
{
  MSMenu *m = selectedMenu();
  
  while(m!=this)
    {
      MSMenuItem *sel = m->activeMenuItem();
      if (sel!=0)
	{
	  sv_.appendSingle(sel->label());
	}

      m = sel->menu()->parentMenuItem()->menu();
    }

  sv_.appendSingle(activeMenuItem()->label());
  sv_.reverse();
}

extern "C" A ep_gp(A,A);
extern I dbg_tscb;

void AplusMenu::activate(void)
{
  extern int safeAset(V,A,A,A);
  extern void setBusyState(MSBoolean);
  releaseGrab();
  V v = (model()!=0)?((AplusModel *)model())->aplusVar():0;
  
  if(v)
  {
    MSStringVector sv;
    formSymbolVector(sv);
    if (sv.length()>0)
    {
      A pick = gv(Et, sv.length());
      P ppick; ppick.i=pick->p;
      // Form Pick;
      for(int k=0; k < sv.length(); k++)
      {
	const char *str = sv(k);
	ppick.a[k] = (A) MS(si((char *)str));
      }
      
      // Form New Value;
      A val=ep_gp(pick,(A)v->a);
      if(0==val) val=aplus_nl;

      setBusyState(MSTrue);

      if(Sf&&v->f)
      {
	A res;
	if(dbg_tscb)cbtrc(v,0);
	v->z=2;

	if(AScbTraceHook::function()) 
	  {
	    AScbTraceHook::run((A)v->f,v->c,(I)val,0,(I)pick,v);
	  }

	res=af4((A)v->f,v->c,(I)val,0,(I)pick,v);
	v->z=1;
        if(0==res)showError(qs); 
	dc(res);
      }

      ((AplusModel*)model())->doneCB(v, val, 0, pick);
      dc(val);dc(pick);

      setBusyState(MSFalse);
    }
  }

  MSMenuBar::activate();
}

void AplusMenu::mnemonics(A a_)
{
  if (verifyMnemonic(this, a_) == MSTrue)
   {
     setMnemonic(this, a_);
   }
}

A AplusMenu::mnemonics(void)
{
  return getMnemonic(this);
}

A AplusMenu::getMnemonic(MSMenu *m_)
{
  int childcount = m_->children().length();
  
  A st=gv(Et,2);
  A attr=(childcount>0)?gv(Et,childcount):aplus_nl;
  A val=(childcount>0)?gv(Et,childcount):aplus_nl;
  char str[2];
  str[1]='\0';
  
  MSMenuItem *item;
  for(int i = 0; i < childcount; i++)
   {
     item = (MSMenuItem *) m_->children()(i);

     str[0] = item->mnemonic();
     if (str[0]!=0)
       attr->p[i] = MS(si(str));
     else     
       attr->p[i] = (unsigned long) aplus_nl;

     if (item->cascade())
      {
	val->p[i]= (unsigned long) getMnemonic((MSMenu *)item->children()(0));
      }
     else
       val->p[i]=(unsigned long) aplus_nl;
   }
  st->p[0] = (unsigned long) attr;
  st->p[1] = (unsigned long) val;
  return st;
}


void AplusMenu::setMnemonic(MSMenu *m_, A a_)
{
  int childcount = m_->children().length();
  
  if (isNull(a_)==MSTrue)
    {
      for (int i=0; i<childcount; ++i)
	{
	  MSMenuItem *item = (MSMenuItem *)m_->children()(i);
	  item->mnemonic('\0');
	  if (item->cascade())
	    {
	      setMnemonic((MSMenu *)item->children()(0), aplus_nl);
	    }
	}
    }
  else
    {
      P p; p.i=a_->p;
      A attr=p.a[0];
      A val=p.a[1];

      MSMenuItem *item;
      for(int i = 0; i < childcount; i++)
	{
	  item = (MSMenuItem *) m_->children()(i);
      
	  if (isNull(attr)==MSTrue)
	    {
	      item->mnemonic('\0');
	    }
	  else if (!QS(attr)&&(attr->t==Et&&attr->n>0&&QS(*attr->p)))
	    {
	      item->mnemonic(((char *) XS(attr->p[i])->n)[0]);
	    }
	  else if (attr->t == Ct)
	    {
	      char *c = (char *) attr->p[0];
	      item->mnemonic(c[0]);
	    }

	  if (item->cascade())
	    {
	      setMnemonic((MSMenu *)item->children()(0), (A)val->p[i]);
	    }
	}
    }

  redraw();
}


MSBoolean AplusMenu::verifyMnemonic(MSMenu *m_, A a_)
{
  if (isNull(a_)==MSTrue)	// null is a valid value for mnemonics
    {
      return MSTrue;
    }

  int childcount = m_->children().length();
  
  if (a_->n==2 && a_->t==Et)
   {
     P p; p.i=a_->p;
     A attr=p.a[0];
     A val=p.a[1];

     if (attr->n!=childcount)
      {
	showError("Mnemonic error");
	return MSFalse;
      }
     MSMenuItem *item;
     for(int i = 0; i < childcount; i++)
      {
	item = (MSMenuItem *) m_->children()(i);
	if (item->cascade())
	 {
	   assert(item->children().length()==1);
	   return verifyMnemonic((MSMenu *)item->children()(0), (A)val->p[i]);
	 }
      }
     return MSTrue;
   }
  else showError("Mnemonic type error");
  return MSFalse;
}
  

const MSSymbol& AplusMenu::widgetType(void) const
{
  return symbol();
}


const MSSymbol& AplusMenu::symbol(void)
{
  static MSSymbol sym("AplusMenu");
  return sym;
}
