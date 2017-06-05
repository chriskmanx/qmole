///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <X11/Xlib.h>
#include <X11/Xatom.h>

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#if HAVE_NEW
#include <new>
#else
#include <new.h>
#endif
#include <math.h>

#include <MSGUI/MSApplication.H>
#include <MSGUI/MSLayoutManager.H>
#include <MSGUI/MSBusy.H>
#include <MSGUI/MSGC.H>
#include <MSGUI/MSMenuItem.H>
#include <MSIPC/MSChannel.H>
#include <MSTypes/MSTime.H>

#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusCallback.H>
#include <AplusGUI/AplusLabel.H>
#include <AplusGUI/AplusReference.H>
#include <AplusGUI/AGIF.H>
#include <AplusGUI/AplusShell.H>
#include <AplusGUI/AplusPopup.H>
#include <AplusGUI/AplusTable.H>
#include <AplusGUI/AplusTrace.H>
#include <AplusGUI/AplusArray.H>
#include <AplusGUI/AplusTable.H>
#include <AplusGUI/AplusTableColumn.H>
#include <AplusGUI/AplusView.H>
#include <AplusGUI/AplusMatrix.H>
#include <AplusGUI/AplusUpdateQueue.H>
#include <AplusGUI/AplusFormatter.H>
#include <AplusGUI/AplusSlot.H>
#include <AplusGUI/AplusEnumConverter.H>

extern long dbg_tmstk;
extern MSString applicationVersionString(void);

int AplusEvaluationDepth=0;

typedef void *XCallArg;
typedef void *XClientArg;

//////////////////////////////////////////////////////////////////////////////

int DefaultPixelValue=0;
static MSChannel *stdinChannel_g;
static AplusFormatter OutputObject;

void serverExitFunction(AplusDisplayServer *server_)
{
  if (server_!=0)
   {
//     MSDeleteQueue::allowDelete(MSFalse);
     E e;
     e=(E)ma(3);
     e->n=1;
     e->f=(I)server_->exitFunc()->function();
     e->a[0]=(I)server_->exitFunc()->data();
     dc((A)ez(ME(e)));
     mf((I *)e);
//     MSDeleteQueue::allowDelete(MSTrue);
   }
}

static char *g_mstkVersion(void)
{
  return (char *)applicationVersionString().string();
}

static void s_scbTraceHook(A func)
{
  AScbTraceHook::function(func);
  return;
}

static void s_XSynchronize(AplusDisplayServer *server_, I mode_)
{
  if(server_)
    XSynchronize(server_->display(),mode_?True:False);
  return;
}

static void s_exitFunc(AplusDisplayServer *server_,A fc_)
{
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AExitFunc *exitFunc=new AExitFunc((A)f,(A)c);       
     server_->exitFunc(exitFunc);
   }
}

static A g_exitFunc(AplusDisplayServer *server_)
{
  A r=aplus_nl;
  if (server_!=0)
   {
     r=gv(Et,2);
     r->p[0]=ic(server_->exitFunc()->function());
     r->p[1]=ic(server_->exitFunc()->data());
   }
  return (A)r;
}

// Setting Server defaults
static A g_defaultPixelValue() { return gi(DefaultPixelValue); }

static A g_defaultFontValue()
{ return gi((I)MSDisplayServer::defaultDisplayServer()->defaultFont()); }
static A g_colorMapDefaultBg(MSDisplayServer *server_)
{
  A z=aplus_nl;
  if (server_!=0) z=gi(server_->defaultBackground());
  return z;
}
static void s_colorMapDefaultBg(MSDisplayServer *server_, unsigned long color_)
{ server_->defaultBackground(server_->colorName(color_)); }
static A g_colorMapDefaultFg(MSDisplayServer *server_)
{
  A z=aplus_nl;
  if (server_!=0) z=gi(server_->defaultForeground());
  return z;
}
static void s_colorMapDefaultFg(MSDisplayServer * server_, unsigned long color_)
{ server_->defaultForeground(server_->colorName(color_)); } 
static A g_colorMapDefaultHlColor(MSDisplayServer *)
{ return (A) gi(AVariableData::defaultHlColor()); }
static void s_colorMapDefaultHlColor(MSDisplayServer *,unsigned long color_)
{ AVariableData::defaultHlColor(color_); } 
// Index Color is not supported, call RowColor instead
static A g_colorMapDefaultIdxColor(MSDisplayServer *)
{ return (A) gi(AVariableData::defaultRowColor()); }
static void s_colorMapDefaultIdxColor(MSDisplayServer *,unsigned long)
{ return; } 
static A g_colorMapDefaultRowColor(MSDisplayServer *)
{ return (A) gi(AVariableData::defaultRowColor()); }
static void s_colorMapDefaultRowColor(MSDisplayServer *,unsigned long color_)
{ AVariableData::defaultRowColor(color_); } 
static A g_colorMapDefaultMatrixColor(MSDisplayServer *)
{ return (A) gi(AVariableData::defaultMatrixColor()); }
static void s_colorMapDefaultMatrixColor(MSDisplayServer *,unsigned long color_)
{ AVariableData::defaultMatrixColor(color_); } 
static A g_serverDefaultFont(MSDisplayServer *server_)
{
  A z=aplus_nl;
  if (server_!=0) z=gi(server_->defaultFont());
  return z;
}
static void s_serverDefaultFont(MSDisplayServer *server_, Font f_)
{ server_->defaultFont(server_->fontName(f_)); } 

static A q_outputFormats(void) { return (A)OutputObject.formats(); }
static A sfmt(A fmt_,A data_) { return (A)OutputObject.sfmt(fmt_,data_); }

A cdipv(AClientData *ac_,A data_,A index_,A pick_,V v_)
{
//   ++AplusEvaluationDepth;   fix in mstk

  if(AScbTraceHook::function()) 
    {
      AScbTraceHook::run(ac_->function(),(I)ac_->data(),(I)data_,(I)index_,(I)pick_,v_);
    }

  A r=af4(ac_->function(),(I)ac_->data(),(I)data_,(I)index_,(I)pick_,v_);
//   --AplusEvaluationDepth;   fix in mstk
  if (r==0) showError(qs);
  return r;
}

A cdipvFmt(AClientData *ac_,A data_,A,A,V)
{
  A r=OutputObject.fmt(ac_->data(),data_);  
  if (r==0) showError(qs);
  return r;
}

static A cdipvThorn(AClientData *ac_,A data_,A,A,V)
{
  A r=(data_->t<=Ft)?OutputObject.thorn(ac_->data(),data_):aplus_nl;  
  if (r==0) showError(qs);
  return r;
}

static A cdipvSymbol(AClientData *ac_,A data_,A,A,V)
{
  A r=OutputObject.formatOutput(ac_->data(),data_);
  if (r==0) showError(qs);
  return r;
}

AVariableData *getVarData(A a_)
{
  V v=getV(a_);
  if (v==0)
    {
      return 0;
    }

  AVariableData *varData;
  if (v->attr!=0)
    {
      varData = (AVariableData *)v->attr;
    }
  else
   {
     varData = new AVariableData;
     v->attr = (void *)varData;
   }

  return varData;
}

static void s_varOutFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (QS(fc_))
   {
     A sym=gs(Et);
     sym->p[0]=(I)fc_;
     A fmt=gv(It,2);
     int f=(int)OutputObject.format(fc_);
     if (f==0)
      {
        V v=getV(a_);
        cerr<<"ã ! ";
        if (v!=0) cerr<<(char*)v->cx->s->n<<"."<<(char*)v->s->n;
        cerr<<": invalid symbol"<<endl;
      }
     fmt->p[0]=(I)f;
     fmt->p[1]=(I)OutputObject.defaultPrecision();
     AClientData *ac=new AClientData((A)aplus_nl,(A)fmt,(A)sym);       
     varData->outFunc((AFunc)cdipvSymbol,ac);     
   }
  else if (QA(fc_))
   {
     if (fc_->t==Ct)
      {
	AClientData *ac=new AClientData((A)aplus_nl,(A)fc_,(A)fc_);       
	varData->outFunc((AFunc)cdipvFmt,ac);     
      }
     else if (fc_->t==It||fc_->t==Ft)
      {
        P p; p.i=fc_->p;
        double dfmt=(fc_->t==Ft)?(double)p.f[0]:(double)p.i[0];
        int ifmt=(int)(10.0*dfmt);
        A fmt=(A)gv(It,2);
        int width=(int)floor(dfmt);
	int precision=(ifmt-(width*10));
        fmt->p[0]=(I)width;
	fmt->p[1]=(I)precision;
	AClientData *ac=new AClientData((A)aplus_nl,(A)fmt,(A)fc_);       
	varData->outFunc((AFunc)cdipvThorn,ac);     
      }
     else if (fc_->t==Et&&fc_->n>0)
      {
        P p; p.i=fc_->p;
        if (QS(p.a[0]))
	 {
	   A fmt=gv(It,2);
	   int f=(int)OutputObject.format(p.a[0]);
	   if (f==0)
	    {
	      V v=getV(a_);
	      cerr<<"ã ! ";
	      if (v!=0) cerr<<(char*)v->cx->s->n<<"."<<(char*)v->s->n;
	      cerr<<": invalid symbol"<<endl;
	    }
	   fmt->p[0]=(I)f;
	   fmt->p[1]=(I)(fc_->n==2)?(int)p.a[1]->p[0]:
	     OutputObject.defaultPrecision();
	   AClientData *ac=new AClientData((A)aplus_nl,(A)fmt,(A)fc_);       
	   varData->outFunc((AFunc)cdipvSymbol,ac);     
	 }
        else if (fc_->t==Et&&fc_->n==2)
         {
           AClientData *ac=new AClientData((A)p.a[0],(A)p.a[1],(A)fc_);       
           varData->outFunc((AFunc)cdipv,ac);     
         }
	else showError("Invalid 'out' Function Specification");
      }
     else if (fc_->t==Et&&fc_->n==0) varData->outFunc((AFunc)0,0);     
     else showError("Invalid 'out' Function Specification");
   }
  else showError("Invalid 'out' Function Specification");
}

static A getVarFunc(AClientData *ac_)
{ 
  A r=aplus_nl;
  if (ac_!=0)
  { 
    if (isNull(ac_->actualData())==MSFalse) r=(A)ic(ac_->actualData());
    else
    {
      r=gv(Et,2);
      r->p[0]=ic(ac_->function());
      r->p[1]=ic(ac_->data());
    }
  }
  return (A)r;
}

void setBusyState(MSBoolean);

// Safe Object Assignment function 
int safeAset(V v_,A d_,A i_,A p_)
{
  setBusyState(MSTrue);
//  MSDeleteQueue::allowDelete(MSFalse);
  int r=(int)aset((I)v_,(I)d_,(I)i_,(I)p_);
//  MSDeleteQueue::allowDelete(MSTrue);
  setBusyState(MSFalse);
  return r;
}

// Safe Object Destruction function 
extern "C" void aplusDestroy(MSWidgetView *pWidgetView_);
void aplusdestroy(MSWidgetView *pWidgetView_)
{
  AVariableData *vd = 0;
  // Need to clear pWidgetView_ right away, can't wait for deleteQueue to process
  if (pWidgetView_)
    {
      AplusModel *pModel = (AplusModel *)pWidgetView_->model();
      if (pModel)
	{
	  vd = pModel->aplusVar() ? pModel->pAVarData() : 0;
	}
    }

  pWidgetView_->safeDestroy(pWidgetView_);
  if (vd) vd->pWidgetView(0);
}

// Variable Destruction function
extern "C" void destroyVar(V v_);
void destroyVar(V v_)
{ 
  if (v_!=0)
   {
     AVariableData *varData = pAVarDataFromV(v_);
     if (varData!=0) delete varData; 
     v_->attr=0;
   }
}

//   CDE Window Manager Functions

static A StringVectorToNestedArray(const MSStringVector &svec_)
{
  unsigned const len=svec_.length();
  A z=len?gv(Et,len):aplus_nl;
  for(int i=0;i<len;++i) z->p[i]=(I)gsv(0,(char *)svec_(i).string());
  return z;
}

static MSStringVector NestedArrayToStringVector(A aobj_)
{
  MSStringVector svec;
  A aelement;
  for (int i=0;i<aobj_->n;++i)
  {
    aelement=(A)aobj_->p[i];
    svec.append(MSString((Ct==aelement->t)?(char *)aelement->p:""));
  }
  return svec;
}

static I g_serverIsCDERunning(AplusDisplayServer *server_)
{ return server_->isCDERunning()?1:0; }
static char *g_serverCurrentWorkspace(AplusDisplayServer *server_)
{ return (char *) server_->currentWorkspaceName().string(); }
static A g_serverWorkspaceNames(AplusDisplayServer *server_)
{ return StringVectorToNestedArray(server_->workspaceNames()); }
static I s_serverCurrentWorkspace(AplusDisplayServer *server_,char *name_)
{ return server_->changeWorkspaceTo(MSString(name_))?1:0; }


//   misc functions
static Font getFontID(MSDisplayServer *server_,const char *fontString_)
{ return server_->fontID(fontString_); }
static char *getFontString(MSDisplayServer *server_,Font fontID_)
{ return (char *) server_->fontName(fontID_); }
static unsigned long getPixelValue(MSDisplayServer *server_,const char *colorString_)
{ return server_->pixel(colorString_); }
static char *getColorString(MSDisplayServer *server_,unsigned long p_)
{ return (char *) server_->colorName(p_); }

// misc window functions
static void mapW(MSWidgetView *pWidgetView_)           // NEED RTTI
{
  MSModel *pModel = pWidgetView_->model();
  if (pModel!=0 && pModel->type()==AplusModel::symbol())
    {
      AplusModel *aplusModel=(AplusModel *)pModel;
      aplusModel->evaluate();
    }

  pWidgetView_->show();
}
static void unmapW(MSWidgetView *pWidgetView_) { pWidgetView_->unmap(); }
static void wRaise(MSWidgetView *pWidgetView_) { pWidgetView_->raise(); }
static void wLower(MSWidgetView *pWidgetView_) { pWidgetView_->lower(); }
static void naturalSize(MSWidgetView *pWidgetView_) { pWidgetView_->naturalSize(); }
static I mapped(MSWidgetView *pWidgetView_) 
{ return MSTrue==pWidgetView_->mapped() ? 1 : 0; }
static I beenMapped(MSWidgetView *pWidgetView_) 
{  return MSTrue==pWidgetView_->firstMap()? 1 : 0 ; }
static MSWidgetView *parent(MSWidgetView *pWidgetView_) { return (MSWidgetView *)pWidgetView_->owner(); }
static MSDisplayServer *server(MSWidgetView *pWidgetView_) { return pWidgetView_->server(); }
static void resize(MSWidgetView *pWidgetView_,int w_,int h_) { pWidgetView_->resize(w_,h_); }
static void moveTo(MSWidgetView *pWidgetView_,int x_,int y_) { pWidgetView_->moveTo(x_,y_); }

static I warpPointer(MSWidgetView *pWidgetView_)
{
  if (pWidgetView_!=0 && pWidgetView_->mapped()==MSTrue)
    {
      XWarpPointer(pWidgetView_->display(),None,pWidgetView_->window(),0,0,0,0,0,0);
      return 0;
    }
  else
    {
      return -1;
    }
}

static void positionChild(MSWidgetView *pWidgetView_,int r_,int c_,int vspan_,
			  int hspan_,int opts_) 
{
  enum ResizeOptions { Center=0,Left=4,Right=8,Top=16,Bottom=32,
		       SmallWidth=64,SmallHeight=128,LikeWidth=256,LikeHeight=512};
  unsigned long constraint = At::Default;

  if (opts_ & Left       ) constraint |= At::Left;
  if (opts_ & Right      ) constraint |= At::Right;
  if (opts_ & Top        ) constraint |= At::Top;
  if (opts_ & Bottom     ) constraint |= At::Bottom;
  if (opts_ & SmallWidth ) constraint |= At::MinimizeWidth;
  if (opts_ & SmallHeight) constraint |= At::MinimizeHeight;
  if (opts_ & LikeWidth  ) constraint |= At::MaintainWidth;
  if (opts_ & LikeHeight ) constraint |= At::MaintainHeight;

  pWidgetView_->at(At(r_, c_, vspan_, hspan_, constraint));
}

static void wRefresh(MSWidgetView *pWidgetView_) { pWidgetView_->redraw(); }

static void wDefaultBgColorChanged(MSWidgetView *w_, unsigned long new_)
{
  if (w_->background()==MSDisplayServer::defaultDisplayServer()->defaultBackground())
    w_->background(new_);
}
static void wDefaultFgColorChanged(MSWidgetView *w_, unsigned long new_)
{
  if (w_->foreground()==MSDisplayServer::defaultDisplayServer()->defaultForeground())
    w_->foreground(new_);
}
static void wDefaultHlColorChanged(MSWidgetView *w_, unsigned long new_)
{
  if (w_->widgetType()==AplusSlot::symbol())
    {
      AplusSlot *sl = (AplusSlot *)w_;
      if (sl->highlightColor()==AVariableData::defaultHlColor())
	sl->highlightColor(new_);
    }
  else
    {
      if (w_)
	{
	  if (w_->highlightColor()==AVariableData::defaultHlColor())
	    w_->highlightColor(new_);
	}
    }
}

// Index color is no longer supported.  Only row color is honored.
static void wDefaultIdxColorChanged(MSWidgetView *, unsigned long)
{ return; }

static void wDefaultRowColorChanged(MSWidgetView *w_, unsigned long new_)
{
  if (w_->widgetType()==AplusArray::symbol() ||
      w_->widgetType()==AplusTable::symbol() ||
      w_->widgetType()==AplusView::symbol() ||
      w_->widgetType()==AplusMatrix::symbol())
   {
     MSArrayView *av = (MSArrayView *)w_;
     if (av->selectedRowBackground()==AVariableData::defaultRowColor())
       av->selectedRowBackground(new_);
   }
}

static void wDefaultMatrixColorChanged(MSWidgetView *w_, unsigned long new_)
{
  if (w_->widgetType()==AplusMatrix::symbol())
   {
     AplusMatrix *am = (AplusMatrix *)w_;
     if (am->rowIndexBg()==AVariableData::defaultMatrixColor() &&
	 am->colIndexBg()==AVariableData::defaultMatrixColor() &&
	 am->cornerIndexBg()==AVariableData::defaultMatrixColor())
      {
	am->rowIndexBg(new_);
	am->colIndexBg(new_);
	am->cornerIndexBg(new_);
      }
   }
}

static void wDefaultFontChanged(MSWidgetView *pWidgetView_, Font new_)
{
  if (pWidgetView_->font()==MSDisplayServer::defaultDisplayServer()->defaultFont())
    pWidgetView_->font(new_);

  const MSSymbol& sym = pWidgetView_->widgetType();
  if (sym==AplusTableColumn::symbol())
    {
      AplusTableColumn *pColumn = (AplusTableColumn *)pWidgetView_;
      if (pColumn->headingFont()==MSDisplayServer::defaultDisplayServer()->defaultFont())
	{
	  pColumn->headingFont(new_);
	}
    }
  else if (sym!=AplusTraceSet::symbol()) 	// a regular widget, derived from MSWidgetCommon
    {
      MSWidgetCommon *pWidget = (MSWidgetCommon *)pWidgetView_;
      if (pWidget->titleFont()==MSDisplayServer::defaultDisplayServer()->defaultFont())
	{
	  pWidget->titleFont(new_);
	}
    }
}


void ACallback(XCallArg,XClientArg client_)
{
//  MSDeleteQueue::allowDelete(MSFalse);
  AClientData *ac=(AClientData *)client_;

  if(AScbTraceHook::function()) 
    {
      AScbTraceHook::run(ac->function(),(I)ac->data(),0,0,0,ac->aplusVar());
    }

  A r=af4(ac->function(),(I)ac->data(),0,0,0,ac->aplusVar());
  if (r==0) showError(qs);
  else dc(r);
//  MSDeleteQueue::allowDelete(MSTrue);
}

extern "C" void stdinDisable(void);
extern "C" void stdinEnable(void);

static MSBoolean stdinOn=MSTrue;
void stdinDisable(void )
{ stdinOn=MSFalse; stdinChannel_g->disable(); }
void stdinEnable(void )
{ stdinOn=MSTrue; stdinChannel_g->enable(); }
static void AStdinCB(XCallArg)
{ tf(); if (stdinOn==MSTrue) stdinChannel_g->enable(); }

// generate row col index for cdip functions
A grc(A av_,int r,int c)
{
   A a=av_;
   I n=a->r-(a->t==Ct&&a->r);
   a=gv(Et,n);
   if (1<=n)
     {
       a->p[0]=(r==-1)?(I)aplus_nl:(I)gi(r);
       if(2<=n)
	 {
	   a->p[1]=(c==-1)?(I)aplus_nl:(I)gi(c);
	 }
     }

   return a;
}

static A getCB(MSWidgetView *pWidgetView_,A cbname_)
{
  A r=aplus_nl;
  char *cbnamep;

  if (cbname_->t==Ct) cbnamep=(char *)cbname_->p;
  else if (cbname_->t==Et&&QS(*cbname_->p)) cbnamep=XS(*cbname_->p)->n;

  AplusCallback *acb=0;
  
  if ((acb=(AplusCallback *)pWidgetView_->callback(cbnamep))!=0)
   {
     AClientData *ac=acb->ac();
     if (ac!=0)
      {
	r=gv(Et,2);
	r->p[0]=ic(ac->function());
	r->p[1]=ic(ac->data());
      }
   }
  return (A)r;
}

// use AXCommon::addAClientData&&AXCommon::removeAClientData to 
// insure that fc_ gets dc when the window is destroyed - the 
// PViewdow cb mechanism will not delete the client data - so we do
// it separately here.

static void addCB(MSWidgetView *pWidgetView_,A cbname_,A fc_)   // NEED RTTI
{
  char *cbnamep;

  if (cbname_->t==Ct) cbnamep=(char *)cbname_->p;
  else if (cbname_->t==Et&&QS(*cbname_->p)) cbnamep=XS(*cbname_->p)->n;

  AClientData *ac=0;
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     if (pWidgetView_->model())
       ac=new AClientData(((AplusModel *)pWidgetView_->model())->aplusVar(),(A)f,(A)c);       
     else
       ac=new AClientData((V) 0,(A)f,(A)c);
     
     pWidgetView_->callback(cbnamep,new AplusCallback(ac));
   }
  else
    {
      pWidgetView_->callback(cbnamep, (MSCallback *)0);  // Clear the callback
    }
}

/*
 *   'A' variable attribute functions for descendents of AXCommon
 */
static void s_varInFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);

  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->inFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) varData->inFunc((AFunc)0,0);     
  else showError("Invalid 'in' Function Specification");
}

static void s_varGeoFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->geoFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) varData->geoFunc((AFunc)0,0);     
  else showError("Invalid 'geometry' Function Specification");
}

static void s_varDoneFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->doneFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) varData->doneFunc((AFunc)0,0);     
  else showError("Invalid 'done' Function Specification");
}

/*
static void s_varOutFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->outFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) varData->outFunc((AFunc)0,0);     
  else showError("Invalid 'out' Function Specification");
}
*/

static void s_varTitleFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->titleFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) varData->titleFunc((AFunc)0,0);     
  else showError("Invalid 'title' Function Specification");
}

static void s_varFgFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et && fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->fgFunc((AFunc)cdipv,ac);     
   }
  else if (It==fc_->t && 1==fc_->n || isNull(fc_)==MSTrue) varData->foreground(fc_);
  else showError("Invalid 'color' Function Specification");
}

static void s_varBgFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et && fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->bgFunc((AFunc)cdipv,ac);     
   }
  else if (It==fc_->t && 1==fc_->n || isNull(fc_)==MSTrue) varData->background(fc_);
  else showError("Invalid 'color' Function Specification");
}

static void s_varTitleColorFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->titleColorFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) varData->titleColorFunc((AFunc)0,0);     
  else showError("Invalid 'titleColor' Function Specification");
}

static void s_varRoFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->roFunc((AFunc)cdipv,ac);     
   }
  else if (It==fc_->t&&1==fc_->n) varData->setReadOnly((int)fc_->p[0]);
  else if (isNull(fc_)==MSTrue) varData->setReadOnly(0);
  else showError("Invalid 'protect' Function Specification");
}

static void s_varFontFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->fontFunc((AFunc)cdipv,ac);     
   }
  else if (It==fc_->t && 1==fc_->n || isNull(fc_)==MSTrue) varData->font(fc_);
  else showError("Invalid 'font' Function Specification");
}

static void s_varTitleFontFunc(A a_,A fc_)
{ 
  AVariableData *varData=getVarData(a_);
  if (fc_->t==Et&&fc_->n==2)
   {
     I f=fc_->p[0];
     I c=fc_->p[1];
     AClientData *ac=new AClientData((A)f,(A)c);       
     varData->titleFontFunc((AFunc)cdipv,ac);     
   }
  else if (isNull(fc_)==MSTrue) varData->titleFontFunc((AFunc)0,0);     
  else showError("Invalid 'titleFont' Function Specification");
}

static A g_varInFunc(A a_)
{return (A)getVarFunc((AClientData *)getVarData(a_)->inFunc()->arg()); }
static A g_varGeoFunc(A a_)
{return (A)getVarFunc((AClientData *)getVarData(a_)->geoFunc()->arg()); }
static A g_varOutFunc(A a_)
{return (A)getVarFunc((AClientData *)getVarData(a_)->outFunc()->arg()); }
static A g_varTitleFunc(A a_)
{return (A)getVarFunc((AClientData *)getVarData(a_)->titleFunc()->arg()); }

static A g_varFgFunc(A a_) // really returns foreground
{
 AVariableData *varData=getVarData(a_);
 A z=(A)getVarFunc((AClientData *)varData->fgFunc()->arg());
 if (isNull(z)==MSTrue) return (A)ic(varData->fgA());
 return z;
}

static A g_varBgFunc(A a_) // really returns background
{
 AVariableData *varData=getVarData(a_);
 A z=(A)getVarFunc((AClientData *)varData->bgFunc()->arg());
 if (isNull(z)==MSTrue) return (A)ic(varData->bgA());
 return z;
}

static A g_varFontFunc(A a_)
{
  AVariableData *varData=getVarData(a_);
  A z=(A)getVarFunc((AClientData *)varData->fontFunc()->arg());
  if (isNull(z)==MSTrue) return (A)ic(varData->fontA());
  return z;
}

// static A g_varFontFunc(A a_)
// {return (A)getVarFunc((AClientData *)getVarData(a_)->fontFunc()->arg()); }

static A g_varTitleColorFunc(A a_)
{return (A)getVarFunc((AClientData *)getVarData(a_)->titleColorFunc()->arg());}
static A g_varRoFunc(A a_)
{
  AVariableData *varData=getVarData(a_);
  A z=(A)getVarFunc((AClientData *)varData->roFunc()->arg());
  if (isNull(z)==MSTrue) return (A) gi(varData->readOnly());
  return z;
}
static A g_varTitleFontFunc(A a_)
{return (A)getVarFunc((AClientData *)getVarData(a_)->titleFontFunc()->arg());}
static A g_varDoneFunc(A a_)
{return (A)getVarFunc((AClientData *)getVarData(a_)->doneFunc()->arg()); }

/* Variable set Functions */
static void s_varClass(A a_,A class_)
{ (isNull(class_)==MSTrue||Et!=class_->t||1>class_->n) ?
  getVarData(a_)->varClass((S)0):getVarData(a_)->varClass(XS(*class_->p)); }
static void s_varBg(A a_,A bg_) { getVarData(a_)->background(bg_); }
static void s_varStars(A a_,MSBoolean b_) { getVarData(a_)->stars(b_); }
static void s_varCw(A a_,int w_) { getVarData(a_)->colWidth(w_); }
static void s_varEw(A a_,int w_) { getVarData(a_)->editWidth(w_); }
static void s_varWid(A a_,MSWidgetView *pWidgetView_) { getVarData(a_)->pWidgetView(pWidgetView_); }
static void s_varTitle(A a_,A str_) { getVarData(a_)->title(str_); }
static void s_varTitleColor(A a_,A fg_) { getVarData(a_)->titleFg(fg_); }
static void s_varTitleFont(A a_,A fid_) { getVarData(a_)->titleFont(fid_); }

/* Variable get Functions */
static A g_varClass(A a_)
{
  AVariableData *pVarData=getVarData(a_);
  if (pVarData==0)
    {
      return aplus_nl;
    }

  S sym=pVarData->varClass();
  return (sym!=0)?gsym(sym->n):aplus_nl;
}
static A g_varBg(A a_)
{ A ap=getVarData(a_)->bgA(); 
  return (QA(ap)&&isNull(ap)==MSFalse)?(A)ic(ap):ap;
}
static I g_varStars(A a_) 
{ return MSTrue==getVarData(a_)->stars() ? 1 :0; }
static I g_varCw(A a_) { return (I) getVarData(a_)->colWidth(); }
static I g_varEw(A a_) { return (I) getVarData(a_)->editWidth(); }
static MSWidgetView *g_varWid(A a_) { return getVarData(a_)->pWidgetView(); }
static A g_varTitle(A a_)
{ A ap=getVarData(a_)->title(); 
  return (QA(ap) && isNull(ap)==MSFalse) ? (A)ic(ap) : ap;
}
static A g_varTitleColor(A a_)
{ A ap=getVarData(a_)->titleAFg(); 
  return (QA(ap) && isNull(ap)==MSFalse) ? (A)ic(ap) : ap;
}
static A g_varTitleFont(A a_)
{ A ap=getVarData(a_)->titleAFont(); 
  return (QA(ap)&&isNull(ap)==MSFalse)?(A)ic(ap):ap;
}

extern "C" void update(V,A,A,A,I,I);

void update(V v_,A data_,A index_,A pick_,I ravel_,I immediateUpdate_)
{
  // This function must take care to prevent re-entrance into the GUI code
  // during dependency re-evaluation.  There are 2 flags, which are used
  // to detect if we are currently in the middle of some GUI update and
  // delay update processing, placing the event onto the update queue instead.
  //
  // 1) immediateUpdate_ argument indicates whether the update should be processed
  //    immediately or not.  It's set to 0 in liba during nested dependency
  //    evaluation.
  //
  // 2) AplusEvaluationDepth is a global flag incremented in AplusModel::evaluate()
  //    before gt() is called and decremented after gt() is done.  If a non-zero value
  //    indicates that this update came from a dependency re-evaluation in AplusGUI.
  //
  if (immediateUpdate_==0 || AplusEvaluationDepth>0)
    {
      addToUpdateQueue(v_,data_,index_,pick_,ravel_);
    }
  else	// if the call is not recursive
   {
     setBusyState(MSTrue);
     AplusUpdate upd(v_,data_,index_,pick_,ravel_);
     upd.send();
     processUpdateQueue();
     setBusyState(MSFalse);
   }
}

static A g_data(MSWidgetView *pWidgetView_)
{
  V arg;

  if (pWidgetView_->model()==0)
    {
      if (dbg_tmstk) cout << "Widget with no model" << endl;
      arg = 0;
    }
  else
    {
      arg = ((AplusModel *) pWidgetView_->model())->aplusVar();
    }

  return (A) getSymWithContext(arg);
}


void s_data(MSWidgetView *pWidgetView_,A aobject_)
{
  V aplusVar = getVfromA(aobject_);

  // Special Hack job for Shell and Popups.  
  if (pWidgetView_->widgetType() == AplusShell::symbol() ||
      pWidgetView_->widgetType() == AplusPopup::symbol())
    {
      ((AplusModel*)pWidgetView_->model())->aplusVar(aplusVar);
      return;
    }

  if (aplusVar!=0 && gt(aplusVar))  // ok to call gt() directly as aplusVar is not bound to widget yet
    {
      AplusModel *pModel = new AplusModel(aplusVar);
      pModel->coupleWidgetView(pWidgetView_);
    }
}

I v_data(MSWidgetView *pWidgetView_,A a_)
{
  V v=getV(a_);
  if (v==0)
    {
      return 0;
    }

  AplusVerifyEvent ave(v, (A)gt(v));

  if (pWidgetView_!=0 && pWidgetView_->model()!=0)
   {
      // we can call receiveEvent() on the widget-view directly instead of
      // calling sendEvent() on the model, but for that we need to
      // cast the widget-view to MSEventReceiver, since MSView::receiveEvent()
      // is protected
     ((MSEventReceiver *)pWidgetView_)->receiveEvent(ave);
     return  MSTrue==ave.result() ? 1 : 0;
   }
  else
   {
     if (dbg_tmstk) cout << "No model defined in v_data" << endl;
     return 1;
   }

} 

extern "C" int verify(V v_,A a_);
int verify(V v_,A a_)
{
  MSBoolean r=MSFalse;
  if (v_->attr!=0)
    {
      AVariableData *varData=pAVarDataFromV(v_);
      AplusVerifyEvent ave(v_, a_);
      MSWidgetView *pWidgetView = varData->pWidgetView();

      if (pWidgetView && pWidgetView->model())
	{
	  // see the comment in v_data()
	  ((MSEventReceiver *)pWidgetView)->receiveEvent(ave);
	  r = ave.result();
	}
      else
	{
	  if (dbg_tmstk) cout << "Warning:  No model defined in verify" << endl;
	  r=MSFalse;
	}
    }
  
  return (int)  MSTrue==r ? 1 : 0;
}

extern "C" void flush(void);
void flush(void) { MSApplication::application()->flush(); }

static void bFlush(void) { MSApplication::application()->flush(); }
static void bBeep(MSDisplayServer *server_) { server_->bell(); }

static I bVirtualScreen(AplusDisplayServer *server_) {return (I) server_->virtualScreen();}

static A bVirtualGeometry(AplusDisplayServer *server_)
{
  int r,c;
  server_->virtualGeometry(r,c);
  A av=gv(It,2);
  av->p[0]=(I)r;
  av->p[1]=(I)c;
  return av;
}

static void bRequestPrimary(MSWidgetView *pWidgetView_)
{ pWidgetView_->convertSelection(); }

static A g_primary(MSWidgetView *pWidgetView_)
{ return (A)(pWidgetView_->server()->pasteBuffer().length()>0)?
            gsv(0,(char *)pWidgetView_->server()->pasteBuffer().string()):gsv(0,"");
}
static void s_primary(MSWidgetView *pWidgetView_, A data_)
{
  if (QA(data_)&&data_->t==Ct)
   {
     if (data_->r==2)
      {
        int slen=(int)(data_->n+data_->d[0]);
        char *str=new char[slen+1];
        char *cp=(char *)data_->p;
        int c=0,col=0;
	for (int i=0;i<data_->n;i++)
	 {
           if (col==data_->d[1]) {str[c++]='\n';col=0;}
	   str[c++]=cp[i];
           col++;
	 }
        str[c++]='\n';
        str[slen]='\0';
        (void)pWidgetView_->copyPrimary(str,slen);
        delete [] str;
      }
     else (void)pWidgetView_->copyPrimary((char *)data_->p,(int)data_->n);
   }
}

static I bIsTopLevel(MSTopLevel *);
static I bIsPopup(MSTopLevel *);


// Shell 'A' interface functions 
static void s_virtualScreen(MSTopLevel *top_, A screen_)
{
  if (bIsTopLevel(top_)==1) // if this is a shell
    {
      ((AplusShell *)top_)->virtualScreen(screen_);
    }
  else if (bIsPopup(top_)==1) // if this is a popup
    {
      ((AplusPopup *)top_)->virtualScreen(screen_);
    }
}

static A g_virtualScreen(MSTopLevel *top_)
{
  if (bIsTopLevel(top_)==1) // if this is a shell
    {
      return ((AplusShell *)top_)->virtualScreen();
    }
  else if (bIsPopup(top_)==1) // if this is a popup
    {
      return ((AplusPopup *)top_)->virtualScreen();
    }
  else
    {
      return gi(-1);
    }
}

// Left and Right Footers won't be supported anymore

//static void s_shellLeftFooter(MSShell *sh_,const char *s_) { sh_->leftFooter(s_); }
//static void s_shellRightFooter(MSShell *sh_,const char *s_){ sh_->rightFooter(s_); }
//static char *g_shellLeftFooter(MSShell *sh_) {return(char *)sh_->leftFooter();}
//static char *g_shellRightFooter(MSShell *sh_){return(char *)sh_->rightFooter();}     

static const char *DefaultFooter = "";
static const char *DefaultHeader = "";

static void s_shellLeftFooter(MSTopLevel *,const char *) { return; }
static void s_shellRightFooter(MSTopLevel *,const char *){ return; }
static char *g_shellLeftFooter(MSTopLevel *) {return (char *) DefaultFooter;}
static char *g_shellRightFooter(MSTopLevel *) {return(char *) DefaultHeader;}
static void s_shellFooter(MSTopLevel *sh_, MSBoolean b_) { sh_->footer(b_); }
static void s_shellHeader(MSTopLevel *sh_, MSBoolean b_) { sh_->header(b_); }
static I g_shellFooter(MSShell *sh_) 
{ return MSTrue==sh_->footer() ? 1 : 0; }
static I g_shellHeader(MSShell *sh_) 
{ return MSTrue==sh_->header() ? 1 : 0; }

static A g_shellWorkspacePresence(MSTopLevel *sh_) 
{ return StringVectorToNestedArray(sh_->workspacePresence()); }

static void s_shellWorkspacePresence(MSTopLevel *sh_,A names_)
{
  if (Ct==names_->t) sh_->workspacePresence(MSString((char *)names_->p));
  else if (Et==names_->t)
  {
    MSStringVector svec=NestedArrayToStringVector(names_);
    sh_->workspacePresence(svec);
  }
}

static void s_shellResizeable(MSTopLevel *sh_,MSBoolean b_) {sh_->resizeable((MSBoolean)b_); }

static I s_windowGroup(MSShell *tl_,MSShell *gr_)
{ 
  return MSTrue==tl_->windowGroup(gr_) ? 1 : 0;
}
static MSShell *g_windowGroup(MSShell *tl_) {return tl_->windowGroup();}

static A g_followers(MSShell *tl_)
{
  const MSWidgetVector &wv = tl_->followerList();
  A r = aplus_nl;
  if (wv.length()>0) r = gv(It, wv.length());
  for (unsigned i = 0; i < wv.length(); i++)
   {
     if (wv(i)!=(MSWidgetView *) tl_)  // The widget itself is not a follower in A+
       r->p[i] = (unsigned long) wv(i);
   }
  return r;
}
   
static void s_pushPinState(MSPopup *pw_,MSBoolean state_) { (state_==MSTrue)?pw_->pinIn():pw_->pinOut(); }
static I g_pushPinState(MSPopup *pw_) 
{ return (pw_->pushPinState()==MSTrue) ? 1 : 0; }

static MSShell *g_topLevel(MSWidgetView *pWidgetView_)
{
  while (pWidgetView_->owner()!=0)
    {
      pWidgetView_ = (MSWidgetView *)pWidgetView_->owner();
    }

  return (MSShell *)pWidgetView_;
}

static I bIsTopLevel(MSTopLevel *top_)  /* Should be bIsShell */
{
  if (top_->widgetType() == AplusShell::symbol())
    return 1;
  else
    return 0;
}

static I bIsPopup(MSTopLevel *top_)
{
  if (top_->widgetType() == AplusPopup::symbol())
    return 1;
  else
    return 0;
}


// We still need a BusyCount, despite ref counting of MSApplicationBusy;
// Otherwise we don't know when to clear the pointer bp;

static MSApplicationBusy *bp=0;
static MSBoolean BusyEnable = MSTrue;
static int BusyCount=0;

void busyEnable(MSBoolean state_)
{BusyEnable=state_;}
MSBoolean busyEnable(void)
{return BusyEnable;}
  
static void s_busyState(I state_)
{
  if (state_)
  {
    ++BusyCount;
    if (bp==0) bp=new MSApplicationBusy;
  }
  else if (BusyCount>0)
  {
    BusyCount--;
    if(0==BusyCount){if (bp!=0) delete bp;bp=0;}
  }
}

void setBusyState(MSBoolean state_)
{
  s_busyState((state_==MSTrue && BusyEnable==MSTrue)?1:0);
}

static I g_busyCount(void) {return BusyCount;}

// Busy Title no longer exists
static void s_busyTitleState(MSBoolean) {}
static I g_busyTitleState(void)
{ return 0; }

static void s_busyClockState(I state_)
{
  BusyEnable=state_ ? MSTrue : MSFalse;
  if (BusyEnable == MSFalse)
   {
     delete bp;
   }
}

static I g_busyClockState(void)
{ return MSTrue==BusyEnable ? 1 : 0; }

static void s_shellIconData(MSShell *sh_,A a_)
{
  if (QA(a_)&&a_->t==Et&&a_->n==3)
   {
     P p; p.i=a_->p;
     if (p.a[0]->t==It&&p.a[0]->r==0&&
         p.a[1]->t==It&&p.a[1]->r==0&&
         p.a[2]->t==It&&p.a[2]->r==1)
      {
        int width=(int)p.a[0]->p[0];
        int height=(int)p.a[1]->p[0];	
        char *bits=new char[p.a[2]->n];
        for (int i=0;i<p.a[2]->n;i++) bits[i]=(char)p.a[2]->p[i];

        Pixmap bitmap=XCreateBitmapFromData(sh_->display(),sh_->window(),bits,width,height);
	sh_->iconPixmap(MSPixmap(sh_->server(), "iconPixelMap", bits, width, height,
				 sh_->foreground(),sh_->background()));
      }
   }
}


static void traverseFocus(MSWidgetView *pWidgetView_) 
{ (void)g_topLevel(pWidgetView_)->traverseFocus(pWidgetView_); }

static A g_tabList(MSShell *sh_)
{
  A r = aplus_nl;
  const MSWidgetVector &wv = sh_->traversalList();
  r = gv(It, wv.length());
  for (int i = 0; i < wv.length(); i++)
   {
     r->p[i] = (unsigned long) wv(i);
   }
  return r;
}

static void s_tabList(MSShell *sh_, A a_)
{
  if (a_==0)
    {
      return;
    }

  if (isNull(a_) || a_->t==It)
    {
      MSWidgetVector wv;
      P p; p.i=a_->p;
      for (int ind=0; ind<a_->n; ++ind)
	{
	  wv << (MSWidgetView *)p.i[ind];
	}

      sh_->traversalList(wv);
    }
}

static MSWidgetView *g_metaTab(MSWidgetView *pWidgetView_)
{
  MSShell *top = g_topLevel(pWidgetView_);
  MSWidget *pFocusWidget=0;

  if (top->widgetType()==AplusShell::symbol())
    {
      pFocusWidget = ((AplusShell *)top)->getNextFocusAfter(pWidgetView_);
    }
  else if (top->widgetType() == AplusPopup::symbol())
    {
      pFocusWidget = ((AplusPopup *)top)->getNextFocusAfter(pWidgetView_);
    }

  // Make sure that we have an A+ widget here, not an MStk widget.  This
  // especially applies to composite A+ widgets, such as AplusSlot, since
  // in MStk every field is also a widget.  If pFocusWidget is an MStk
  // widget (e.g., a slot field), then go up the widget hierarchy until
  // we find an A+ widget.
  //
  while (pFocusWidget!=0 && isAplusWidget(pFocusWidget)==MSFalse)
    {
      pFocusWidget = pFocusWidget->owner();
    }

  // Either the A+ widget that has the focus or 0 will be returned to s;
  // s should handle the null pointer case properly.
  //
  return (MSWidgetView *)pFocusWidget;
}

static void s_metaTab(MSWidgetView *from_, MSWidgetView *to_)
{
  MSShell *top = g_topLevel(from_);
  if (top->widgetType() == AplusShell::symbol())
   {
     ((AplusShell *)top)->insertFocusAfter(from_, to_);
   }
  else if (top->widgetType() == AplusPopup::symbol())
   {
     ((AplusPopup *)top)->insertFocusAfter(from_, to_);
   }
}

static void bShowAndWaitForMap(MSWidgetView *pWidgetView_)
{
  MSShell *top = g_topLevel(pWidgetView_);
  if (top->widgetType() == AplusShell::symbol())
    ((AplusShell *)top)->showAndWaitForMap();
  else if (top->widgetType() == AplusPopup::symbol())
    ((AplusPopup *)top)->showAndWaitForMap();
}

static void bAddTab(MSShell *,MSWidgetView *pWidgetView_){pWidgetView_->addToFocusList();}
static void bRemoveTab(MSShell *,MSWidgetView *pWidgetView_) { pWidgetView_->removeFromFocusList(); }

static MSDisplayServer *DefaultXServer(void)
{
  if (MSDisplayServer::defaultDisplayServer()==0)
   {
     cerr<<"No Default Server Established: exit in DefaultXServer()"<<endl;
     exit(255);   
   }
  return MSDisplayServer::defaultDisplayServer();
}

MSBoolean serverNoDelay(MSDisplayServer *server_,MSBoolean) // Bogus Function
{
  if (server_==0) server_=MSDisplayServer::defaultDisplayServer();
  return MSFalse;
}

static I serverConnection(MSDisplayServer *server_)
{
  if (server_==0) server_=MSDisplayServer::defaultDisplayServer();
  return (I) server_->connection();
}  

A xwindowHashStat(MSDisplayServer *server_)
{
  if (server_==0) server_=MSDisplayServer::defaultDisplayServer();
  int size=server_->widgetHashTable()->size();
  A r=gv(It,size);
  for (int i=0;i<size;i++) r->p[i]=(I)server_->widgetHashTable()->chainLength(i);
  return r;
}

A shadowHashStat(MSDisplayServer *server_)
{
  if (server_==0) server_=MSDisplayServer::defaultDisplayServer();
  int size=server_->shadowHashTable()->size();
  A r=gv(It,size);
  for (int i=0;i<size;i++) r->p[i]=(I)server_->shadowHashTable()->chainLength(i);
  return r;
}

A shadowReferenceStat(MSDisplayServer *)
{
/*
if (server_==0) server_=MSDisplayServer::defaultDisplayServer();
  MSHashTable *ht=server_->shadowHashTable();
  int c=0;
  int i;
  for (i=0;i<ht->size();i++) c+=ht->chainLength(i);
  A r=gv(It,c);
  c=0;
  for (i=0;i<ht->size();i++) 
   {
     MSHashEntry *entry=ht->bucket(i);
     while (entry!=0)
      {
        MSShadowColors *data=(MSShadowColors *)entry->value();
	r->p[c++]=(I)data->count();
        entry=entry->next();
      }
   }
  return r;
  */
  return aplus_nl;
}


static I bServerConnection(MSDisplayServer *server_)
{ return (I) serverConnection(server_); }
static I bServerNoDelay(MSDisplayServer *server_,MSBoolean b_)
{ return MSTrue==serverNoDelay(server_,b_) ? 1 : 0; }
static A bXWindowHashStat(MSDisplayServer *server_)
{ return xwindowHashStat(server_); }
static A bShadowHashStat(MSDisplayServer *server_)
{ return shadowHashStat(server_); }
static A bShadowReferenceStat(MSDisplayServer *server_)
{ return shadowReferenceStat(server_); }

static A bPixmapHashStat(void)
{
/*
  int size=MSPixmap::table()->size();
  A r=gv(It,size);
  for (int i=0;i<size;i++) r->p[i]=(I)MSPixmap::table()->chainLength(i);
  return r;
  */
  return aplus_nl;
}

static A bPixmapReferenceStat(void)
{
/*
  MSHashTable *ht=MSPixmap::table();
  int c=0;
  int i;
  for (i=0;i<ht->size();i++) c+=ht->chainLength(i);
  A r=gv(It,c);
  c=0;
  for (i=0;i<ht->size();i++) 
   {
     MSHashEntry *entry=ht->bucket(i);
     while (entry!=0)
      {
        MSPixmapData *data=(MSPixmapData *)entry->value();
	r->p[c++]=(I)data->count();
        entry=entry->next();
      }
   }
  return r;
  */
  return aplus_nl;
}

static A bXGCReferenceStat(void)
{
  A r=gv(It,MSGC::xgcList().count());
  for (int i=0;i<MSGC::xgcList().count();i++) r->p[i]=(I)MSGC::xgcList().data(i)->count();
  return r;
}

static void bPrintLayoutInfo(MSLayoutManager *)
{
  if (dbg_tmstk) cout << "PrintLayoutInfo is currently unsupported." << endl;
  return;
}

static void s_doubleClickInterval(unsigned long t_)
{ MSApplication::application()->doubleClickInterval(t_); }

static unsigned long g_doubleClickInterval(void)
{ return MSApplication::application()->doubleClickInterval(); }

static void s_menuDefaultMnemonic(MSBoolean b_)
{ MSMenuItem::defaultMnemonic(b_); }

static I g_menuDefaultMnemonic(void)
{ return MSTrue==MSMenuItem::defaultMnemonic() ? 1 : 0; }

   

extern "C" A mj(A);
// return a float representing width.precision
static A w_p(A a_)
{ return (A)mj(a_); }

static A bOut(A a_,A data_)
{
  AVariableData *varData=getVarData(a_);
  AOutFunction *f=varData->outFunc();
  return (f->func()!=0)?(*f->func())(f->arg(),data_,aplus_nl,aplus_nl,(V)0):(A)gsv(0,"");
}


extern "C" I g_backingStoreOption(void);
extern "C" void s_backingStoreOption(int o_);

void s_backingStoreOption(int o_)
{
 MSApplication::backingStoreOption(0==o_?MSFalse:MSTrue);
}

I g_backingStoreOption(void) 
{ 
  return MSTrue==MSApplication::backingStoreOption() ? 1 : 0;
}

A q_shadowStyle(void)
{
  AplusShadowStyleConverter converter;
  return converter.stringDomain();
}

///////////////////////////////////////////////////////////////////////////////

void AGIFInstall(void)
{
  CX context=Cx;
  Cx=cx("s");

  install((PFI)g_mstkVersion,"g_mstkVersion",CP,0,0,0,0,0,0,0,0,0);
  install((PFI)s_scbTraceHook,"s_scbTraceHook",V_,1,A_,0,0,0,0,0,0,0);
  install((PFI)s_varClass,"s_varClass",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varBg,"s_varBg",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varStars,"s_varStars",V_,2,A_,IV,0,0,0,0,0,0);
  install((PFI)s_varCw,"s_varCw",V_,2,A_,IV,0,0,0,0,0,0);
  install((PFI)s_varEw,"s_varEw",V_,2,A_,IV,0,0,0,0,0,0);
  install((PFI)s_varWid,"s_varWid",V_,2,A_,IV,0,0,0,0,0,0);
  install((PFI)s_varOutFunc,"s_varOutFunc",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varTitleFunc,"s_varTitleFunc",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varGeoFunc,"s_varGeoFunc",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varDoneFunc,"s_varDoneFunc",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varInFunc,"s_varInFunc",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varFgFunc,"s_varFgFunc",V_,2,A_,0,A_,0,0,0,0,0);
  install((PFI)s_varBgFunc,"s_varBgFunc",V_,2,A_,0,A_,0,0,0,0,0);
  install((PFI)s_varTitleColorFunc,"s_varTitleColorFunc",V_,2,A_,0,A_,0,0,0,0,0);
  install((PFI)s_varRoFunc,"s_varRoFunc",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varFontFunc,"s_varFontFunc",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varTitleFontFunc,"s_varTitleFontFunc",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varTitle,"s_varTitle",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varTitleColor,"s_varTitleColor",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)s_varTitleFont,"s_varTitleFont",V_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)g_varWid,"g_varWid",IV,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varBg,"g_varBg",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varStars,"g_varStars",IV,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varCw,"g_varCw",IV,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varEw,"g_varEw",IV,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varClass,"g_varClass",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varGeoFunc,"g_varGeoFunc",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varDoneFunc,"g_varDoneFunc",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varOutFunc,"g_varOutFunc",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varTitleFunc,"g_varTitleFunc",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varInFunc,"g_varInFunc",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varFgFunc,"g_varFgFunc",A_,1,0,0,A_,0,0,0,0,0);
  install((PFI)g_varBgFunc,"g_varBgFunc",A_,1,0,0,A_,0,0,0,0,0);
  install((PFI)g_varTitleColorFunc,"g_varTitleColorFunc",A_,1,0,0,A_,0,0,0,0,0);
  install((PFI)g_varRoFunc,"g_varRoFunc",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varFontFunc,"g_varFontFunc",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varTitleFontFunc,"g_varTitleFontFunc",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varTitle,"g_varTitle",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varTitleColor,"g_varTitleColor",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)g_varTitleFont,"g_varTitleFont",A_,1,A_,0,0,0,0,0,0,0);

  install((PFI)s_XSynchronize,"s_XSynchronize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_exitFunc,"s_exitFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_exitFunc,"g_exitFunc",A_,1,IV,0,0,0,0,0,0,0);

  install((PFI)g_defaultPixelValue,"bDefaultPixelValue",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)g_defaultFontValue,"bDefaultFontValue",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)s_colorMapDefaultBg,"s_defaultBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_colorMapDefaultBg,"g_defaultBg",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_colorMapDefaultFg,"s_defaultFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_colorMapDefaultFg,"g_defaultFg",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_colorMapDefaultHlColor,"s_defaultHlColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_colorMapDefaultHlColor,"g_defaultHlColor",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_colorMapDefaultIdxColor,"s_defaultIdxColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_colorMapDefaultIdxColor,"g_defaultIdxColor",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_colorMapDefaultRowColor,"s_defaultRowColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_colorMapDefaultRowColor,"g_defaultRowColor",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_colorMapDefaultMatrixColor,"s_defaultMatrixColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_colorMapDefaultMatrixColor,"g_defaultMatrixColor",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_serverDefaultFont,"s_defaultFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_serverDefaultFont,"g_defaultFont",A_,1,IV,0,0,0,0,0,0,0);

  install((PFI)s_data,"s_data",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_data,"g_data",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)v_data,"v_data",IV,2,IV,A_,0,0,0,0,0,0);

  install((PFI)s_windowGroup,"s_windowGroup",IV,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_windowGroup,"g_windowGroup",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_followers,"g_followers",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_pushPinState,"s_pushPinState",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_pushPinState,"g_pushPinState",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_shellIconData,"s_shellIconData",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)s_busyState,"s_busyState",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_busyCount,"g_busyCount",IV,0, 0,0,0,0,0,0,0,0);
  install((PFI)s_busyTitleState,"s_busyTitleState",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_busyTitleState,"g_busyTitleState",IV,0,0,0,0,0,0,0,0,0);
  install((PFI)s_busyClockState,"s_busyClockState",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_busyClockState,"g_busyClockState",IV,0,0,0,0,0,0,0,0,0);

  install((PFI)g_topLevel,"g_topLevel",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_tabList,"g_tabList",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tabList,"s_tabList",V_,2,IV,A_,0,0,0,0,0,0);

  install((PFI)s_virtualScreen,"s_virtualScreen",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_virtualScreen,"g_virtualScreen",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_shellLeftFooter,"s_shellLeftFooter",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)s_shellRightFooter,"s_shellRightFooter",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)s_shellFooter,"s_shellFooter",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_shellHeader,"s_shellHeader",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)s_shellResizeable,"s_shellResizeable",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_shellFooter,"g_shellFooter",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_shellHeader,"g_shellHeader",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_shellLeftFooter,"g_shellLeftFooter",CP,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_shellRightFooter,"g_shellRightFooter",CP,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_shellWorkspacePresence,"g_shellWorkspacePresence",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_shellWorkspacePresence,"s_shellWorkspacePresence",V_,2,IV,A_,0,0,0,0,0,0);

  install((PFI)bShowAndWaitForMap,"bShowAndWaitForMap",V_,1,IV,0,0,0,0,0,0,0);

  install((PFI)s_metaTab,"s_metaTab",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_metaTab,"g_metaTab",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)traverseFocus,"bTraverseFocus",V_,1,IV,0,0,0,0,0,0,0);

  install((PFI)bRequestPrimary,"bRequestPrimary",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_primary,"s_primary",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_primary,"g_primary",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)bIsTopLevel,"bIsTopLevel",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)bIsPopup,"bIsPopup",IV,1,IV,0,0,0,0,0,0,0);

  install((PFI)bAddTab,"bAddTab",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)bRemoveTab,"bRemoveTab",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)bOut,"bOut",A_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)sfmt,"sfmt",A_,2,A_,A_,0,0,0,0,0,0);
  install((PFI)q_outputFormats,"q_outputFormats",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)bBeep,"bBeep",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)aplusdestroy,"bDestroy",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)mapW,"bMap",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)unmapW,"bunMap",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)wRaise,"bRaise",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)wLower,"bLower",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)naturalSize,"bNaturalSize",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)parent,"bParent",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)mapped,"bMapped",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)beenMapped,"bBeenMapped",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)server,"bServer",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)bFlush,"bFlush",V_,0,0,0,0,0,0,0,0,0);
  install((PFI)resize,"bResize",V_,3,IV,IV,IV,0,0,0,0,0);
  install((PFI)moveTo,"bMoveTo",V_,3,IV,IV,IV,0,0,0,0,0);
  install((PFI)warpPointer,"bWarpPointer",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)addCB,"bAddCB",V_,3,IV,A_,A_,0,0,0,0,0);
  install((PFI)getCB,"bGetCB",A_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)positionChild,"bPositionChild",V_,6,IV,IV,IV,IV,IV,IV,0,0);
  install((PFI)wRefresh,"bRefresh",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)wDefaultBgColorChanged,"bDefaultBgColorChanged",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)wDefaultFgColorChanged,"bDefaultFgColorChanged",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)wDefaultHlColorChanged,"bDefaultHlColorChanged",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)wDefaultIdxColorChanged,"bDefaultIdxColorChanged",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)wDefaultRowColorChanged,"bDefaultRowColorChanged",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)wDefaultMatrixColorChanged,"bDefaultMatrixColorChanged",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)wDefaultFontChanged,"bDefaultFontChanged",V_,2,IV,IV,0,0,0,0,0,0);

  install((PFI)g_serverIsCDERunning,"g_serverIsCDERunning",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_serverCurrentWorkspace,"g_serverCurrentWorkspace",CP,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_serverWorkspaceNames,"g_serverWorkspaceNames",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_serverCurrentWorkspace,"s_serverCurrentWorkspace",IV,2,IV,CP,0,0,0,0,0,0);

  install((PFI)getFontID,"bgetFontID",IV,2,IV,CP,0,0,0,0,0,0);
  install((PFI)getFontString,"bgetFontString",CP,2,IV,IV,0,0,0,0,0,0);
  install((PFI)getPixelValue,"bgetPixelValue",IV,2,IV,CP,0,0,0,0,0,0);
  install((PFI)getColorString,"bgetColorString",CP,2,IV,IV,0,0,0,0,0,0);
  install((PFI)w_p,"w_p",A_,1,A_,0,0,0,0,0,0,0);
  install((PFI)bVirtualScreen,"bVirtualScreen",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)bVirtualGeometry,"bVirtualGeometry",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)bServerConnection,"bServerConnection",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)bServerNoDelay,"bServerNoDelay",IV,2,IV,IV,0,0,0,0,0,0);
  install((PFI)bXWindowHashStat,"bXWindowHashStat",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)bShadowHashStat,"bShadowHashStat",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)bShadowReferenceStat,"bShadowReferenceStat",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)bPixmapHashStat,"bPixmapHashStat",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)bPixmapReferenceStat,"bPixmapReferenceStat",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)bXGCReferenceStat,"bXGCReferenceStat",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)bPrintLayoutInfo,"bPrintLayoutInfo",V_,1,IV,0,0,0,0,0,0,0);

  install((PFI)s_doubleClickInterval, "bsetDoubleClickInterval",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_doubleClickInterval, "bgetDoubleClickInterval",IV,0,0,0,0,0,0,0,0,0);
  install((PFI)g_menuDefaultMnemonic, "bgetMenuDefaultMnemonic",IV,0,0,0,0,0,0,0,0,0);
  install((PFI)s_menuDefaultMnemonic, "bsetMenuDefaultMnemonic",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_backingStoreOption, "bgetBackingStoreOption",IV,0,0,0,0,0,0,0,0,0);
  install((PFI)s_backingStoreOption, "bsetBackingStoreOption",V_,1,IV,0,0,0,0,0,0,0);
  
  install((PFI)q_shadowStyle, "q_shadowStyle", A_,1,0,0,0,0,0,0,0,0);

  Cx=context;
}

extern void AGIFmstkInstall(void);
extern void AGIFGraphInstall(void);
extern void AGIFPrintInstall(void);

typedef void (*PFV)(void*);

extern "C" void XaInstall(void);
void XaInstall(void)
{
  AGIFInstall();
  AGIFmstkInstall();
  AGIFGraphInstall();
  AGIFPrintInstall();

  stdinChannel_g=new MSChannel("stdin",0,MSChannel::Default,MSChannel::Read,new AplusStdinCallback((PFV)AStdinCB));
  stdinChannel_g->enable();
}

extern "C" void CppInstall(void);

A ep_gfmtsym(void)
{ return (A)OutputObject.formats(); }
A ep_sfmt(A fmt_,A data_)
{ return (A)OutputObject.sfmt(fmt_,data_); }

void CppInstall(void)
{
  CX context=Cx;
  Cx=Rx;
  install((PFI)ep_gfmtsym,"_gfmtsym",A_,0,0,0,0,0,0,0,0,0);
  install((PFI)ep_sfmt,"_sfmt",A_,2,A_,A_,0,0,0,0,0,0);
  Cx=context;
}
