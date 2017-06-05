///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <stdlib.h>
#include <string.h>
#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSGUI/MSTableColumn.H>
#include <MSGUI/MSWidgetCommon.H>
#include <AplusGUI/AplusTableColumn.H>
#include <AplusGUI/AplusTrace.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusUpdateQueue.H>

extern void setBusyState(MSBoolean);
extern A grc(A,int,int);

unsigned long AVariableData::_defaultHlColor = ULONG_MAX;
unsigned long AVariableData::_defaultRowColor = ULONG_MAX;
unsigned long AVariableData::_defaultMatrixColor = ULONG_MAX;
A AScbTraceHook::_function=0;

A AOutFunction::callFunc(V v_,A a_,int row_,int col_,A p_) 
{ A i=(row_==-1&&col_==-1)?0:(A)grc((A)v_->a,row_,col_);
  A r=(_func!=0)?(A)(*_func)(_arg,a_,i,p_,v_):(A)gsv(0,"");
  if (i!=0) dc((A)i);
  if (a_!=0) dc((A)a_);
  return r;
}

A ACharStrFunction::callFunc(V v_,A a_,int row_,int col_,A p_) 
{ 
  A r=AOutFunction::callFunc(v_, a_, row_, col_, p_);
  if(!r)return aplus_nl;
  if(isNull(r)==MSFalse && Ct!=r->t)
    {
      showError("non-character result from out function");
      dc(r);
      r=aplus_nl;
    }
  return r;
}

MSBoolean processAVariables(void)
{
  AVariableData *varData;
  MSBoolean done=MSFalse;
  MSBoolean didWork=MSFalse;
  int i;
  V v;  

  // the update queue may have some updates waiting to be processed
  //
  processUpdateQueue();

  while(done==MSFalse)
    {
      done=MSTrue;
      for (i=0;i<AplusModel::aplusVarList().length();i++)
	{
	  v=(V)AplusModel::aplusVarList()(i);
	  if (v->z==0)
	    {
	      varData = ::pAVarDataFromV(v); 
	      if (varData!=0&&varData->pWidgetView()!=0)
		{
		  if (varData->pWidgetView()->visible()==MSTrue)
		    {
		      didWork=MSTrue;
		      
		      setBusyState(MSTrue);
		      done=MSFalse;
		      ::gt(v);
		      // 
		      // the update queue should be empty by this time, but
		      // let's process it just in case; if the queue is empty,
		      // this will be a no-op
		      //
		      processUpdateQueue();
		      setBusyState(MSFalse);
		    }
		}
	    }
	}
    }
  
  return didWork;
}

unsigned long AColorFunction::callFunc(V v_,A a_,int row_,int col_,A p_) 
{ 
  AVariableData *vd = ::pAVarDataFromV(v_);
  unsigned long pixel=vd->pWidgetView()->foreground();

  A r=0;
  A i=(row_==-1&&col_==-1)?0:grc((A)v_->a,row_,col_);

  if (func()!=0) 
   {
     r=(A)(*func())(arg(),a_,i,p_,v_);
     if (r!=0)
      {
        if (QS(r))
         pixel=(unsigned long)vd->pWidgetView()->server()->pixel((char *)XS(r)->n);
	else if (r->t==It)
	 pixel=(unsigned long)r->p[0];
	else if (r->t==Ct&&vd->pWidgetView()!=0)
 	 pixel=(unsigned long)vd->pWidgetView()->server()->pixel((char *)r->p);
        else if (r->t==Et&&r->n>0&&QS(*r->p)&&vd->pWidgetView()!=0)
         pixel=(unsigned long)vd->pWidgetView()->server()->pixel((char *)XS(*r->p)->n);
      }
     if (!QS(r)) dc((A)r); 
   }
  if (i!=0) dc((A)i);
  if (a_!=0) dc((A)a_);
  return pixel;
}

Font AFontFunction::callFunc(V v_,A a_,int row_,int col_,A p_) 
{ 
  AVariableData *vd = ::pAVarDataFromV(v_);
  Font fid=vd->pWidgetView()->font();

  A r=0;
  A i=(row_==-1&&col_==-1)?0:grc((A)v_->a,row_,col_);
  if (func()!=0) 
   {
     r=(A) (*func())(arg(),a_,i,p_,v_);
     if (r!=0)
      {
        if (QS(r))
	 fid=(Font)vd->pWidgetView()->server()->fontID((char *)XS(r)->n);
	else if (r->t==It)
	 fid=(Font)r->p[0];
	else if (r->t==Ct&&vd->pWidgetView()!=0)
 	 fid=(Font)vd->pWidgetView()->server()->fontID((char *)r->p);
        else if (r->t==Et&&r->n>0&&QS(*r->p)&&vd->pWidgetView()!=0)
         fid=(Font)vd->pWidgetView()->server()->fontID((char *)XS(*r->p)->n);
      }
     if (!QS(r)) dc((A)r); 
   }
  if (i!=0) dc((A)i);
  if (a_!=0) dc((A)a_);
  return fid;
}

MSBoolean AReadOnlyFunction::callFunc(V v_,A a_,int row_,int col_,A p_) 
{ 
  AVariableData *vd = ::pAVarDataFromV(v_);
  MSBoolean ro=MSFalse;
  A r=0;
  A i=0;

  if (row_==-1&&col_==-1) i=0;
  else i=grc((A)v_->a,row_,col_);

  if (func()!=0) 
   {
     r=(A) (*func())(arg(),a_,i,p_,v_);
     if (r!=0&&r->t==It) ro=(MSBoolean)r->p[0];
     dc((A)r); 
   }
  if (i!=0) dc((A)i);
  if (a_!=0) dc((A)a_);
  return ro;
}

// utility functions for dealing with vars/symbols and context
// get a v from a symbol
V av(A a_)
{
  V v;
  if (a_->n==1) v=getVFromSym(Cx,(S)XS(*a_->p));
  else 
   { 
     int n=(int) a_->n-1;
     S s;
     v=vi(XS(a_->p[n]),n?(s=XS(*a_->p),cxi(*s->n?s:si(""))):Cx);
   }
  return v;
}

V getV(A a_)
{
  if (isNull(a_)==MSTrue || (!QS(a_) && a_->t!=Et))
    {
      return 0;
    }

  S s=(S)XS(*a_->p);
  return (a_->n<=1) ? getVFromSym(Cx,s) :
    vi(XS(a_->p[a_->n-1]),a_->n-1?cxi(*s->n?s:si("")):Cx);
}

// get the symbol for a v as a scalar: `symbol
A getSym(V v_)
{
  A ar;
  if (v_==0) ar=aplus_nl;
  else
   {
     ar=(A)gs(Et);   
     ar->p[0]=(I)MS(v_->s); // symbol
   }
  return (A) ar;
}

// get the symbol for a v as an enclosed type with the context: `context `symbol
A getSymWithContext(V v_)
{
  A ar;
  if (v_==0 ) ar=aplus_nl; 
  else
   {
     ar=(A)gv(Et,2);   
				// context
     if(v_->cx==0) ar->p[0]=(I)MS(si(""));
     else ar->p[0]=(I)MS(v_->cx==cx("")?si(""):v_->cx->s); 
				// symbol
     if(v_->s==0) ar->p[1]=(I)MS(si(""));
     else ar->p[1]=(I)MS(v_->s);
   }
  return (A) ar;
}

extern "C" V sv(CX context_, S sym_);
V getVFromSym(CX context_,S sym_)
{
  return sv(context_, sym_);
}

AVariableData::AVariableData(void) 
{ 
  _pWidgetView=0;
  _colWidth=-1;
  _editWidth=-1;
  _class=0;
  _stars=MSTrue;
  _bg=aplus_nl;
  _fg=aplus_nl;
  _font=aplus_nl;
  _titleFg=aplus_nl;
  _titleFont=aplus_nl;
  _title=aplus_nl;
  _readOnly=MSFalse;
}

AVariableData::~AVariableData(void) 
{
  if (QA(titleA()) && isNl(titleA())==MSFalse) dc(titleA());
  if (QA(titleAFg()) && isNl(titleAFg())==MSFalse) dc(titleAFg());
  if (QA(titleAFont())&& isNl(titleAFont())==MSFalse) dc(titleAFont());
  if (QA(bgA()) && isNl(bgA())==MSFalse) dc(bgA());
  if (QA(fgA()) && isNl(fgA())==MSFalse) dc(fgA());
  if (QA(fontA()) && isNl(fontA())==MSFalse) dc(fontA());
}

void AVariableData::colWidth(int cw_, MSBoolean sendEvent_) 
{
  _colWidth=cw_; 
  if (_pWidgetView && _pWidgetView->model()!=0 && sendEvent_==MSTrue)
    {
      AplusUpdateDataEvent aude;
      ((MSEventReceiver *)_pWidgetView)->receiveEvent(aude);
    }
}

void asTitleStringVector(MSStringVector &t_, A title_)
{
  if (isNull(title_)==MSTrue) goto out;
  
  if (title_->t == Ct)
   {
     if (title_->r<=1)
      {
	t_ << MSString((char *)title_->p, (unsigned) title_->n);
      }
     else if (title_->r==2)
      {
	unsigned dim = title_->d[0];
	unsigned len = title_->d[1];
	int offset=0;
	for (int i = 0; i < dim; i++, offset+=len)
	 {
	   t_ << MSString(((char *)title_->p + offset), len);
	 }
      }
   }
  else if (title_->t ==Et)
   {
     if (QS(*title_->p))
      {
	goto out;
      }

     for (int i = 0 ; i < title_->n; i++)
      {
	A a = (A) title_->p[i];
	if (a && a->t == Ct)
	 {
	   MSStringVector t;
	   asTitleStringVector (t, a);
	   t_ << t;
	 }
      }
   }
  else
   {
     cout << "Non charType in asTitleStringVector" << endl;
   }
  
 out:
  if (t_.maxLength()==0) t_.removeAll();
  
  return;
}



void AVariableData::title(A title_) 
{ 
  if (!QS(title_))
  {
    if (QA(titleA()) && isNull(titleA())==MSFalse) dc(titleA());     
    _title=QA(title_)?(A)ic(title_):title_;
    if (_pWidgetView!=0)
    {
      MSStringVector t;
      asTitleStringVector(t, title_);
      const MSSymbol& sym = _pWidgetView->widgetType();
      if (sym==AplusTableColumn::symbol())
      {
	((AplusTableColumn *)_pWidgetView)->heading(t);
      }
      else if (sym==AplusTraceSet::symbol())
      {
	((AplusTraceSet *)_pWidgetView)->textBuffer(t);
      }
      else	// a subclass of MSWidgetCommon
      {
	((MSWidgetCommon *)_pWidgetView)->title(t);
      }
    }
  }
}


void AVariableData::setReadOnly(int i_)
{
  roFunc((AFunc)0,0);
  _readOnly=(0!=i_)?MSTrue:MSFalse;
  if (_pWidgetView!=0)
    {
      AplusProtectEvent event;
      ((MSEventReceiver *)pWidgetView())->receiveEvent(event);
    }
}


void AVariableData::stars(MSBoolean b_)
{
  if (b_!=stars())
    {
      _stars=b_;
      if (pWidgetView()!=0)
	{
	  AplusUpdateDataEvent event;
	  ((MSEventReceiver *)pWidgetView())->receiveEvent(event);
	}
    }
}


unsigned long AVariableData::titleFg(void) 
{
  if (isNull(titleAFg())==MSTrue)
  {
    if (pWidgetView()!=0) return pWidgetView()->server()->defaultForeground();
    else return MSDisplayServer::defaultDisplayServer()->defaultForeground();
  }
  else return (unsigned long) titleAFg()->p[0];
}

Font AVariableData::titleFont(void) 
{
  if (isNull(titleAFont())==MSTrue)
  {
    if (pWidgetView()!=0) return pWidgetView()->server()->defaultFont();
    else return MSDisplayServer::defaultDisplayServer()->defaultFont();
  }
  else return (Font) titleAFont()->p[0];
}

void AVariableData::titleFg(A fg_) 
{ 
  A fg=aplus_nl;
  if (_pWidgetView!=0)
  {
    if (QS(fg_))
    {
      fg=gi((I)_pWidgetView->server()->pixel((char *)XS(fg_)->n));
    }
    else if (fg_->t==It)
    {
      fg=gi((I)fg_->p[0]);
    }
    else if (fg_->t==Ct)
    {
      fg=gi((I)_pWidgetView->server()->pixel((char *)fg_->p));
    }
    else if (fg_->t==Et&&fg_->n>0&&QS(*fg_->p))
    {
      fg=gi((I)_pWidgetView->server()->pixel((char *)XS(*fg_->p)->n));
    }
  }
  else if (QA(fg_)&&fg_->t==It)
  {
    fg=gi((I)fg_->p[0]);
  }
  
  if (isNull(titleAFg())==MSFalse)
  {
    dc(titleAFg());
  }
  
  _titleFg=fg;
  
  if (_pWidgetView!=0)
  {
    const MSSymbol& sym = _pWidgetView->widgetType();
    if (sym==AplusTableColumn::symbol())
    {
      ((AplusTableColumn *)_pWidgetView)->headingForeground((unsigned long)titleFg());
    }
    else if (sym==AplusTraceSet::symbol())
    {
      ((AplusTraceSet *)_pWidgetView)->textForeground((unsigned long)titleFg());
    }
    else	// a subclass of MSWidgetCommon
    {
      ((MSWidgetCommon *)_pWidgetView)->titleForeground((unsigned long)titleFg());
    }
  }
}

void AVariableData::titleFont(A fid_) 
{ 
  if (isNull(fid_)==MSTrue)
    {
      return;
    }

  A fid=aplus_nl;
  if (_pWidgetView!=0)
  {
    if (QS(fid_))
    {
      fid=gi((I)_pWidgetView->server()->fontID((char *)XS(fid_)->n));
    }
    else if (fid_->t==It)
    {
      fid=gi((I)fid_->p[0]);
    }
    else if (fid_->t==Ct)
    {
      fid=gi((I)_pWidgetView->server()->fontID((char *)fid_->p));
    }
    else if (fid_->t==Et&&fid_->n>0&&QS(*fid_->p))
    {
      fid=gi((I)_pWidgetView->server()->fontID((char *)XS(*fid_->p)->n));
    }
  }
  else if (QA(fid_)&&fid_->t==It)
  {
    fid=gi((I)fid_->p[0]);
  }
  
  if (isNull(titleAFont())==MSFalse)
  {
    dc(titleAFont());
  }
  
  _titleFont=fid;
  
  if (_pWidgetView!=0)
  {
    const MSSymbol& sym = _pWidgetView->widgetType();
    if (sym==AplusTableColumn::symbol())
    {
      ((AplusTableColumn *)_pWidgetView)->headingFont((Font)fid->p[0]);
    }
    else if (sym==AplusTraceSet::symbol())
    {
      ((AplusTraceSet *)_pWidgetView)->textFont((Font)fid->p[0]);
    }
    else	// a subclass of MSWidgetCommon
    {
      ((MSWidgetCommon *)_pWidgetView)->titleFont((Font)fid->p[0]);
    }
  }
}

void AVariableData::pWidgetView(MSWidgetView *pWidgetView_)  
{ 
  if (_pWidgetView!=pWidgetView_) 
  {
    _pWidgetView = pWidgetView_; 
    
    if (_pWidgetView!=0)
    {
      _pWidgetView->background(background()); 
      _pWidgetView->foreground(foreground()); 
      _pWidgetView->font(font()); 
      
      MSStringVector tit;
      asTitleStringVector(tit, title());
      const MSSymbol& sym = _pWidgetView->widgetType();
      if (sym==AplusTableColumn::symbol())
      {
	AplusTableColumn *pColumn = (AplusTableColumn *)_pWidgetView;
	if (isNull(titleA())==MSFalse)
	{
	  pColumn->heading(tit);
	}
	if (isNull(titleAFont())==MSFalse)
	{
	  pColumn->headingFont(titleFont());
	}
	if (isNull(titleAFg())==MSFalse)
	{
	  pColumn->headingForeground(titleFg());
	}
      }
      else if (sym!=AplusTraceSet::symbol()) // a subclass of MSWidgetCommon
      {
	MSWidgetCommon *pWidget = (MSWidgetCommon *)_pWidgetView;
	if (isNull(titleA())==MSFalse)
	{
	  pWidget->title(tit);
	}
	if (isNull(titleAFont())==MSFalse)
	{
	  pWidget->titleFont(titleFont());
	}
	if (isNull(titleAFg())==MSFalse)
	{
	  pWidget->titleForeground(titleFg());
	}
      }
    }
  }
}

void AVariableData::foreground(A fg_) 
{ 
  A fg=aplus_nl;
  if (pWidgetView()!=0)
  {
    if (QS(fg_)) fg=gi((I)pWidgetView()->server()->pixel((char *)XS(fg_)->n));
    else if (fg_->t==It) fg=gi((I)fg_->p[0]);
    else if (fg_->t==Ct) 
      fg=gi((I)pWidgetView()->server()->pixel((char *)fg_->p));
    else if (fg_->t==Et&&fg_->n>0&&QS(*fg_->p)) 
      fg=gi((I)pWidgetView()->server()->pixel((char *)XS(*fg_->p)->n));
  }
  else if (QA(fg_)&&fg_->t==It) fg=gi((I)fg_->p[0]);
  if (isNull(fgA())==MSFalse) dc(fgA());
  _fg=fg;
  if (pWidgetView()!=0)
    pWidgetView()->foreground((isNull(fg)==MSTrue) ?
      MSDisplayServer::defaultDisplayServer()->defaultForeground() :
        foreground());
  fgFunc((AFunc)0,0);
}

void AVariableData::background(A bg_) 
{ 
  A bg=aplus_nl;
  if (pWidgetView()!=0)
  {
    if (QS(bg_)) bg=gi((I)pWidgetView()->server()->pixel((char *)XS(bg_)->n));
    else if (bg_->t==It) bg=gi((I)bg_->p[0]);
    else if (bg_->t==Ct) 
      bg=gi((I)pWidgetView()->server()->pixel((char *)bg_->p));
    else if (bg_->t==Et&&bg_->n>0&&QS(*bg_->p)) 
      bg=gi((I)pWidgetView()->server()->pixel((char *)XS(*bg_->p)->n));
  }
  else if (QA(bg_)&&bg_->t==It) bg=gi((I)bg_->p[0]);
  if (isNull(bgA())==MSFalse) dc(bgA());
  _bg=bg;
  if (pWidgetView()!=0) 
    pWidgetView()->background(
      (isNull(bg)==MSTrue) ? 
	MSDisplayServer::defaultDisplayServer()->defaultBackground() :
	  background()); 
  bgFunc((AFunc)0,0);
}

void AVariableData::font(A fa_)
{
  A afid=aplus_nl;
  if (pWidgetView()!=0)
  {
    if(QS(fa_))afid=gi((I)pWidgetView()->server()->fontID((char *)XS(fa_)->n));
    else if (It==fa_->t) afid=gi((I)fa_->p[0]);
    else if (Ct==fa_->t) 
      afid=gi((I)pWidgetView()->server()->fontID((char *)fa_->p));
  }
  else if (QA(fa_)&&It==fa_->t) afid=gi((I)fa_->p[0]);
  if (isNull(fontA())==MSFalse) dc(fontA());
  _font=afid;
  if (0!=pWidgetView()) 
    pWidgetView()->font((isNull(afid)==MSTrue) ?
		      MSDisplayServer::defaultDisplayServer()->defaultFont() :
			font());
  fontFunc((AFunc)0,0);
}

ACharStrFunction *AVariableData::outFunc(void) { return &_outFunc; }
void AVariableData::outFunc(AFunc func_,AClientData *arg_)
{
  outFunc()->set(func_,arg_); 
  if (pWidgetView()!=0) pWidgetView()->redraw();
}

AInFunction *AVariableData::inFunc(void) { return &_inFunc; }
void AVariableData::inFunc(AFunc func_,AClientData *arg_)
{ inFunc()->set(func_,arg_); }

AColorFunction *AVariableData::fgFunc(void) { return &_fgFunc; }
void AVariableData::fgFunc(AFunc func_,AClientData *arg_)
{
  fgFunc()->set(func_,arg_); 
  if (pWidgetView()!=0)
  {
    pWidgetView()->updateForeground(pWidgetView()->foreground());
  }
}

AColorFunction *AVariableData::bgFunc(void) { return &_bgFunc; }
void AVariableData::bgFunc(AFunc func_,AClientData *arg_)
{
  bgFunc()->set(func_,arg_); 
  if (pWidgetView()!=0)
  {
    pWidgetView()->updateForeground(pWidgetView()->foreground());
  }
}

AFontFunction *AVariableData::fontFunc(void) { return &_fontFunc; }
void AVariableData::fontFunc(AFunc func_,AClientData *arg_)
{
  fontFunc()->set(func_,arg_); 
  if (pWidgetView()!=0)
  {
    pWidgetView()->updateFont(pWidgetView()->font());
  }
}

AFontFunction *AVariableData::titleFontFunc(void) { return &_titleFontFunc; }
void AVariableData::titleFontFunc(AFunc func_,AClientData *arg_)
{
  titleFontFunc()->set(func_,arg_);
  if (_pWidgetView!=0)
    {
      AplusUpdateTitleEvent event;
      ((MSEventReceiver *)_pWidgetView)->receiveEvent(event);
    }
}

AColorFunction *AVariableData::titleColorFunc(void) {return &_titleColorFunc; }
void AVariableData::titleColorFunc(AFunc func_,AClientData *arg_)
{
  titleColorFunc()->set(func_,arg_); 
  if (_pWidgetView!=0)
    {
      AplusUpdateTitleEvent event;
      ((MSEventReceiver *)_pWidgetView)->receiveEvent(event);
    }
}

AOutFunction *AVariableData::titleFunc(void)
{ return &_titleFunc; }

//#include <AplusGUI/AplusCheckBox.H>

void AVariableData::titleFunc(AFunc func_,AClientData *arg_)
{
  titleFunc()->set(func_,arg_); 
  if (_pWidgetView!=0)
    {
      AplusUpdateTitleEvent event;
      ((MSEventReceiver *)_pWidgetView)->receiveEvent(event);
    }
}

AReadOnlyFunction *AVariableData::roFunc(void) { return &_roFunc; }
void AVariableData::roFunc(AFunc func_,AClientData *arg_)
{ _readOnly=MSFalse; roFunc()->set(func_,arg_); }

AGeometryFunction *AVariableData::geoFunc(void) { return &_geoFunc; }
void AVariableData::geoFunc(AFunc func_,AClientData *arg_)
{ geoFunc()->set(func_,arg_);
  if (pWidgetView()!=0) pWidgetView()->naturalSize();
}

AFunction *AVariableData::doneFunc(void) { return &_doneFunc; }
void AVariableData::doneFunc(AFunc func_,AClientData *arg_)
{ doneFunc()->set(func_,arg_); }

ACycleFunction *AVariableData::cycleFunc(void) { return &_cycleFunc; }
void AVariableData::cycleFunc(AFunc func_,AClientData *arg_)
{ cycleFunc()->set(func_,arg_); }

unsigned long AVariableData::defaultHlColor(void)
{
  if (_defaultHlColor==ULONG_MAX)
  {
    _defaultHlColor = MSDisplayServer::defaultDisplayServer()->pixel("yellow");
  }
  return _defaultHlColor;
}
 
unsigned long AVariableData::defaultRowColor(void)
{
  if (_defaultRowColor==ULONG_MAX)
   {
     _defaultRowColor = 
       MSDisplayServer::defaultDisplayServer()->pixel("lightsteelblue3");
   }
  return _defaultRowColor;
}

unsigned long AVariableData::defaultMatrixColor(void)
{
  if (_defaultMatrixColor==ULONG_MAX)
   {
     _defaultMatrixColor = 
       MSDisplayServer::defaultDisplayServer()->pixel("mediumaquamarine");
   }
  return _defaultMatrixColor;
}

extern "C" int ep_issf(A);

MSBoolean isSlotFiller(A a_)
{
  return (ep_issf(a_)==1)?MSTrue:MSFalse;
}


MSBoolean isAplusWidget(const MSWidget *pWidget_)
{
  // This is a somewhat kludgy way of determining if a widget is really an A+
  // widget or an MStk widget.  All A+ widget classes have to implement the
  // widgetType() method.
  //
  MSString aplusPrefix("Aplus");
  if (aplusPrefix.isAbbreviationFor(pWidget_->widgetType().symbolName())==MSTrue)
    {
      return MSTrue;
    }
  else
    {
      return MSFalse;
    }
}


void showError(const char *msg_, int flag_)
{
  if (msg_!=0)
    {
      cout << "ã";

      switch (flag_)
	{
	case 0:
	  cout << " A+ error:  " << msg_ << endl;
	  break;
	case 1:
	  cout << " A+ warning:  " << msg_ << endl;
	  break;
	default:
	  cout << " " << msg_ << endl;
	  break;
	}
    }
	
}


// utility function for converting a symbol or vector of symbols to
// a pixel value or vector of pixel values
//
A convertToPixels(const MSWidgetCommon *xwin_,A a_)
{
  // cast away the const-ness of server() in order to be able to call
  // pixel() method on it, which is non-const
  //
  MSDisplayServer *pServer=(MSDisplayServer *)xwin_->server();
  A rr;

  if (QS(a_))
   {
     rr=gs(It);
     rr->p[0]=(I)pServer->pixel((char *)XS(a_)->n); 
     return rr;   
   }
  else if (a_->t==It&&a_->r<=1) return a_;
  else if (a_->t==Et&&a_->n>0)
   { 
     int i;
     for (i=0;i<a_->n;i++) if (!QS(a_->p[i])) return aplus_nl;
     rr=gv(It,a_->n);
     for (i=0;i<a_->n;i++)
      {
	 rr->p[i]=(I)pServer->pixel((char *)XS(a_->p[i])->n); 
      }

     dc(a_);
     return rr;
   }
  return a_;
}
