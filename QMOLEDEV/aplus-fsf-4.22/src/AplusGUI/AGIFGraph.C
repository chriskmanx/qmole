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
#include <AplusGUI/AGIFGraph.H>
#include <AplusGUI/AplusConvert.H>
#include <AplusGUI/EnumTables.H>
#include <AplusGUI/AplusReportEnum.H>

extern A cdipv(AClientData *,A,A,A,V);
extern A cdipvFmt(AClientData *,A,A,A,V);
extern A getVarFunc(AClientData *);
extern const int MSPointsPerInch;

/*
 *  XTrace access functions
 */
static void s_traceLineStyleFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->lineStyleFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->lineStyleFunc((AFunc)0,0);     
  else showError("Invalid 'lineStyle' Function Specification");
}
static A g_traceLineStyleFunc(AplusTraceSet *tp_)
{  return (A) getVarFunc((AClientData *) tp_->lineStyleFunc()->arg()); }

static void s_traceLineWidthFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->lineWidthFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->lineWidthFunc((AFunc)0,0);     
  else showError("Invalid 'lineWidth' Function Specification");
}
static A g_traceLineWidthFunc(AplusTraceSet *tp_)
{  return (A) getVarFunc((AClientData *) tp_->lineWidthFunc()->arg()); }

/***  pieOffset attribute should not be functional
static void s_tracePieOffsetFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->pieOffsetFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->pieOffsetFunc((AFunc)0,0);     
  else showError("Invalid 'pieOffset' Function Specification");
}
static A g_tracePieOffsetFunc(AplusTraceSet *tp_)
{  return (A) getVarFunc((AClientData *) tp_->pieOffsetFunc()->arg()); }
***/

static void s_traceLineColorFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->lineColorFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->lineColorFunc((AFunc)0,0);     
  else showError("Invalid 'lineColor' Function Specification");
}
static A g_traceLineColorFunc(AplusTraceSet *tp_)
{
  AColorFunction *f = (AColorFunction *) tp_->lineColorFunc();  // Need to cast away constness
  return (A) getVarFunc((AClientData *) f->arg());
}

static void s_traceFillColorFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->fillColorFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->fillColorFunc((AFunc)0,0);     
  else showError("Invalid 'fillColor' Function Specification");
}
static A g_traceFillColorFunc(AplusTraceSet *tp_)
{
  AColorFunction *f = (AColorFunction *) tp_->fillColorFunc();  // Need to cast away constness
  return (A) getVarFunc((AClientData *) f->arg());
}

static void s_traceLegendFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->legendFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->legendFunc((AFunc)0,0);     
  else showError("Invalid 'legend' Function Specification");
}
static A g_traceLegendFunc(AplusTraceSet *tp_)
{ return (A) getVarFunc((AClientData *) tp_->legendFunc()->arg()); }

static void s_traceStyleFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->traceStyleFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->traceStyleFunc((AFunc)0,0);     
  else showError("Invalid 'traceStyle' Function Specification");
}
static A g_traceStyleFunc(AplusTraceSet *tp_)
{  return (A) getVarFunc((AClientData *) tp_->traceStyleFunc()->arg()); }

static void s_traceSymbolFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->traceSymbolFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->traceSymbolFunc((AFunc)0,0);     
  else showError("Invalid 'traceSymbol' Function Specification");
}
static A g_traceSymbolFunc(AplusTraceSet *tp_)
{  return (A) getVarFunc((AClientData *) tp_->traceSymbolFunc()->arg()); }

static void s_traceSymbolSizeFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->traceSymbolSizeFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->traceSymbolSizeFunc((AFunc)0,0);     
  else showError("Invalid 'traceSymbolSize' Function Specification");
}
static A g_traceSymbolSizeFunc(AplusTraceSet *tp_)
{  return (A) getVarFunc((AClientData *) tp_->traceSymbolSizeFunc()->arg()); }

static void s_traceYaxisFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->altYaxisFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->altYaxisFunc((AFunc)0,0);     
  else showError("Invalid 'altYaxis' Function Specification");
}
static A g_traceYaxisFunc(AplusTraceSet *tp_)
{  return (A) getVarFunc((AClientData *) tp_->altYaxisFunc()->arg()); }

static void s_traceXaxisFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->altXaxisFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->altXaxisFunc((AFunc)0,0);     
  else showError("Invalid 'altXaxis' Function Specification");
}
static A g_traceXaxisFunc(AplusTraceSet *tp_)
{  return (A) getVarFunc((AClientData *) tp_->altXaxisFunc()->arg()); }

static void s_traceGradientFunc(AplusTraceSet *tp_,A fc_)
{ 
  if (fc_->t==Et&&fc_->n==2) tp_->gradientFunc((AFunc)cdipv,fc_);     
  else if (isNull(fc_)==MSTrue) tp_->gradientFunc((AFunc)0,0);     
  else showError("Invalid 'gradient' Function Specification");
}
static A g_traceGradientFunc(AplusTraceSet *tp_)
{  return (A) getVarFunc((AClientData *) tp_->gradientFunc()->arg()); }

static void s_traceCoordinate(AplusTraceSet *,A)  
{}

// static void s_traceCoordinate(AplusTraceSet *tp_,A value_) 
// { if (!QS(value_)&&(value_->t==It || value_->t==Ft)&&value_->n==2)
//    { P p; p.i=value_->p; tp_->x_offset(value_->t==Ft?p.f[0]:(double)p.i[0]);
//      tp_->y_offset(value_->t==Ft?p.f[1]:(double)p.i[1]); } }

static A g_traceCoordinate(AplusTraceSet *tp_) { return tp_->coordinate(); }

static void s_tracePieAngle(AplusTraceSet *tp_,A value_) {
  if (!QS(value_)&&(value_->t==It || value_->t==Ft)) {
    P p; 
    p.i=value_->p;
    tp_->pieAngle(value_->t==Ft?(int)p.f[0]:p.i[0]);
  }
}

static A g_tracePieAngle(AplusTraceSet *tp_) { A r=gf(tp_->pieAngle()); return r; }

static void s_tracePieOffsets(AplusTraceSet *pTraceSet_, A value_)
{
  if (!QS(value_) && value_->r<2 && (value_->t==It || value_->t==Ft))  // if it's an int or float scalar or vector
    {
      if (value_->r==0)	// if it's a scalar (as opposed to a one-element vector)
	{
	  double offset = (value_->t==Ft) ? *(double*)value_->p : *(I*)value_->p;
	  pTraceSet_->pieOffsets(MSFloatVector(1,offset));
	}
      else
	{
	  MSFloatVector offsets(AplusConvert::asMSFloatVector(value_));
	  if (offsets.length() > 0)
	    {
	      pTraceSet_->pieOffsets(offsets);
	    }
	}
    }
}

static A g_tracePieOffsets(AplusTraceSet *pTraceSet_)
{
  MSFloatVector *offsets = pTraceSet_->pieOffsets();
  return (offsets==0) ? (A)gf(pTraceSet_->pieOffset()) : AplusConvert::asA(*offsets);
}

static void s_tracePieAspectRatio(AplusTraceSet *tp_,A value_)
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
   { P p; 
     p.i=value_->p; tp_->pieAspectRatio(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_tracePieAspectRatio(AplusTraceSet *tp_) { A r=gf(tp_->pieAspectRatio()); return r; }

static void s_tracePieDepthFactor(AplusTraceSet *tp_,A value_)
{
  if (!QS(value_)&&(value_->t==It || value_->t==Ft))
    {
      P p; 
      p.i=value_->p; tp_->pieDepthFactor(value_->t==Ft?p.f[0]:(double)p.i[0]);
    }
}
static A g_tracePieDepthFactor(AplusTraceSet *tp_)  { A r=gf(tp_->pieDepthFactor()); return r; }

static void s_tracePieProfiles(AplusTraceSet *pTraceSet_, A value_)
{
  if (!QS(value_) && value_->r<2 && (value_->t==It || value_->t==Ft))  // if it's an int or float scalar or vector
    {
      if (value_->r==0)	// if it's a scalar (as opposed to a one-element vector)
	{
	  double profile = (value_->t==Ft) ? *(double*)value_->p : *(I*)value_->p;
	  pTraceSet_->pieProfiles(MSFloatVector(1,profile));
	}
      else
	{
	  MSFloatVector profiles(AplusConvert::asMSFloatVector(value_));
	  if (profiles.length() > 0)
	    {
	      pTraceSet_->pieProfiles(profiles);
	    }
	}
    }
}

static A g_tracePieProfiles(AplusTraceSet *pTraceSet_)
{
  MSFloatVector *profiles = pTraceSet_->pieProfiles();
  return (profiles==0) ? (A)gf(1.0) : AplusConvert::asA(*profiles);
}

static void s_tracePieLegendAlign(AplusTraceSet *pTraceSet_, A align_)
{
  pTraceSet_->pieLegendAlignment(GUIEnum.alignFormat(align_));
}

static A g_tracePieLegendAlign(AplusTraceSet *pTraceSet_)
{ 
  return GUIEnum.alignFormat(pTraceSet_->pieLegendAlignment());
}

static void s_tracePieValueAlign(AplusTraceSet *pTraceSet_, A align_)
{
  pTraceSet_->pieValueAlignment(GUIEnum.alignFormat(align_));
}

static A g_tracePieValueAlign(AplusTraceSet *pTraceSet_)
{ 
  return GUIEnum.alignFormat(pTraceSet_->pieValueAlignment());
}

static void s_tracePiePercentAlign(AplusTraceSet *pTraceSet_, A align_)
{
  pTraceSet_->piePercentAlignment(GUIEnum.alignFormat(align_));
}

static A g_tracePiePercentAlign(AplusTraceSet *pTraceSet_)
{ 
  return GUIEnum.alignFormat(pTraceSet_->piePercentAlignment());
}

static void s_tracePrimarySlice(AplusTraceSet *pTraceSet_, int slice_) { pTraceSet_->primarySlice(slice_); }
static I g_tracePrimarySlice(AplusTraceSet *pTraceSet_) { return (I) pTraceSet_->primarySlice(); }

static void s_tracePrimarySliceAlign(AplusTraceSet *pTraceSet_, A align_)
{
  pTraceSet_->primarySliceAlignment((MSAlignment)GUIEnum.alignFormat(align_));
}

static A g_tracePrimarySliceAlign(AplusTraceSet *pTraceSet_)
{
  return GUIEnum.alignFormat(pTraceSet_->primarySliceAlignment());
}

static void s_traceConstraint(AplusTraceSet *tp_,A value_){tp_->constraint(value_);}
static A g_traceConstraint(AplusTraceSet *tp_) { return tp_->constraintSym(); }

static void s_traceSelectable(AplusTraceSet *tp_,MSBoolean value_) 
{ tp_->selectable(value_); }
static I g_traceSelectable(AplusTraceSet *tp_) 
{ return MSTrue==tp_->selectable() ? 1 : 0; }

static char *g_traceText(AplusTraceSet *tp_) { return (char *) tp_->graph()->editorString(); }
static void s_traceSelected(AplusTraceSet *tp_,A value_) { tp_->selected(value_); }
static A g_traceSelected(AplusTraceSet *tp_) { return (A)tp_->selected(); }

static void s_traceTextFont(AplusTraceSet *ts_, Font font_) { ts_->textFont(font_); }
static Font g_traceTextFont(AplusTraceSet *ts_) { return ts_->textFont(); }

static void s_traceTextFg(AplusTraceSet *ts_, unsigned long fg_) { ts_->textForeground(fg_); }
static unsigned long g_traceTextFg(AplusTraceSet *ts_) { return ts_->textForeground(); }


//
//  XGraph access functions
//

// Label Format Functions

void graphYlabelFormatFunc(AplusGraph *gr_,A fc_,int axis_)
{ 
  if (QA(fc_) && fc_->t==Ct)
    {
      AClientData *ac=new AClientData((A)aplus_nl,(A)fc_,(A)fc_);       
      gr_->y_labelFormatFunc((AFunc)cdipvFmt,ac);     
    }
  else if (!QS(fc_)&&(fc_->t==Et&&fc_->n>0&&QS(*fc_->p)))
   {
     gr_->y_labelFormat(fc_,axis_);
   }
  else if (fc_->t==Et&&fc_->n==2)
   {
     P p; p.i = fc_->p;
     if (QA(p.a[0])&&(p.a[0]->t==Et&&p.a[0]->n>0&&QS(*p.a[0]->p)))
      {
	P pp; pp.i = p.a[1]->p;
	if (pp.i!=0)
          gr_->y_labelFormat(p.a[0], axis_, (int)pp.i[0]);
	else
	  gr_->y_labelFormat(p.a[0],axis_);
      }
     else
       {
	 AClientData *ac=new AClientData(p.a[0],p.a[1]); 
	 gr_->y_labelFormatFunc((AFunc)cdipv,ac,axis_);
       }
   }
  else if (isNull(fc_)==MSTrue) gr_->y_labelFormatFunc((AFunc)0,0,axis_);
  else showError("Invalid 'y format label' Function Specification");
}
static void s_graphYlabelFormatFunc(AplusGraph *gr_,A fc_) { graphYlabelFormatFunc(gr_,fc_,0); }
static A g_graphYlabelFormatFunc(AplusGraph *gr_)
{
  A ac=(A)getVarFunc((AClientData *)gr_->y_labelFormatFunc(0)->arg());
  return (isNull(ac)==MSTrue) ? gr_->y_labelFormatSym(0) : ac;
}
static void s_graphYYlabelFormatFunc(AplusGraph *gr_,A fc_) { graphYlabelFormatFunc(gr_,fc_,1); }
static A g_graphYYlabelFormatFunc(AplusGraph *gr_)
{
  A ac=(A)getVarFunc((AClientData *)gr_->y_labelFormatFunc(1)->arg());
  return (isNull(ac)==MSTrue) ? gr_->y_labelFormatSym(1) : ac;
}

void graphXlabelFormatFunc(AplusGraph *gr_,A fc_,int axis_)
{
  if (QA(fc_) && fc_->t==Ct)
    {
      AClientData *ac=new AClientData((A)aplus_nl,(A)fc_,(A)fc_);       
      gr_->x_labelFormatFunc((AFunc)cdipvFmt,ac);     
    }
  else if (!QS(fc_)&&(fc_->t==Et&&fc_->n>0&&QS(*fc_->p)))
   {
     gr_->x_labelFormat(fc_,axis_);
   }
  else if (fc_->t==Et&&fc_->n==2)
   {
     P p; p.i = fc_->p;
     if (QA(p.a[0])&&(p.a[0]->t==Et&&p.a[0]->n>0&&QS(*p.a[0]->p)))
      {
	P pp; pp.i = p.a[1]->p;
	if (pp.i!=0)
          gr_->x_labelFormat(p.a[0], axis_, (int)pp.i[0]);
	else
	  gr_->x_labelFormat(p.a[0], axis_);
      }
     else
       {
	 AClientData *ac=new AClientData(p.a[0],p.a[1]); 
	 gr_->x_labelFormatFunc((AFunc)cdipv,ac,axis_);
       }
   }
  else if (isNull(fc_)==MSTrue) gr_->x_labelFormatFunc((AFunc)0,0,axis_);
  else showError("Invalid 'x format label' Function Specification");
}
static void s_graphXlabelFormatFunc(AplusGraph *gr_,A fc_) { graphXlabelFormatFunc(gr_,fc_,0); }
static A g_graphXlabelFormatFunc(AplusGraph *gr_)
{
  A ac=(A)getVarFunc((AClientData *)gr_->x_labelFormatFunc(0)->arg());
  return (isNull(ac)==MSTrue) ? gr_->x_labelFormatSym(0) : ac;
}
static void s_graphXXlabelFormatFunc(AplusGraph *gr_,A fc_) { graphXlabelFormatFunc(gr_,fc_,1); }
static A g_graphXXlabelFormatFunc(AplusGraph *gr_)
{
  A ac=(A)getVarFunc((AClientData *)gr_->x_labelFormatFunc(1)->arg());
 return (isNull(ac)==MSTrue) ? gr_->x_labelFormatSym(0) : ac;
}

void graphXsubLabelFormatFunc(AplusGraph *gr_,A fc_,int axis_)
{ 
  if (QA(fc_) && fc_->t==Ct)
    {
      AClientData *ac=new AClientData((A)aplus_nl,(A)fc_,(A)fc_);       
      gr_->x_subLabelFormatFunc((AFunc)cdipvFmt,ac);     
    }
  else if (!QS(fc_)&&(fc_->t==Et&&fc_->n>0&&QS(*fc_->p)))
   {
     gr_->x_subLabelFormat(fc_,axis_);
   }
  else if (fc_->t==Et&&fc_->n==2)
   {
     P p; p.i = fc_->p;
     if (QA(p.a[0])&&(p.a[0]->t==Et&&p.a[0]->n>0&&QS(*p.a[0]->p)))
      {
	P pp; pp.i = p.a[1]->p;
	if (pp.i!=0)
          gr_->x_subLabelFormat(p.a[0], axis_, (int)pp.i[0]);
	else
	  gr_->x_subLabelFormat(p.a[0],axis_);
      }
     else
       {
	 AClientData *ac=new AClientData(p.a[0],p.a[1]); 
	 gr_->x_subLabelFormatFunc((AFunc)cdipv,ac,axis_);
       }
   }
  else if (isNull(fc_)==MSTrue) gr_->x_labelFormatFunc((AFunc)0,0,axis_);
  else showError("Invalid 'x format label' Function Specification");
}
static void s_graphXsubLabelFormatFunc(AplusGraph *gr_,A fc_) { graphXsubLabelFormatFunc(gr_,fc_,0); }
static A g_graphXsubLabelFormatFunc(AplusGraph *gr_)
{
  A ac=(A)getVarFunc((AClientData *)gr_->x_subLabelFormatFunc(0)->arg());
  return (isNull(ac)==MSTrue) ? gr_->x_subLabelFormatSym(0) : ac;
}
static void s_graphXXsubLabelFormatFunc(AplusGraph *gr_,A fc_) { graphXsubLabelFormatFunc(gr_,fc_,1); }
static A g_graphXXsubLabelFormatFunc(AplusGraph *gr_)
{
  A ac=(A)getVarFunc((AClientData *)gr_->x_subLabelFormatFunc(1)->arg());
  return (isNull(ac)==MSTrue) ? gr_->x_subLabelFormatSym(1) : ac;
}

// Label Functions

void graphYlabelFunc(AplusGraph *gr_,A fc_,int axis_)
{ 
  if (fc_->t==Et&&fc_->n==2)
    {
      P p; p.i=fc_->p;
      AClientData *ac=new AClientData(p.a[0],p.a[1]); 
      gr_->y_labelFunc((AFunc)cdipv,ac,axis_);     
    }
  else if (isNull(fc_)==MSTrue) gr_->y_labelFunc((AFunc)0,0,axis_);     
  else showError("Invalid 'y label' Function Specification");
}
static void s_graphYlabelFunc(AplusGraph *gr_,A fc_) {graphYlabelFunc(gr_,fc_,0);}
static A g_graphYlabelFunc(AplusGraph *gr_) { return (A) getVarFunc((AClientData *) gr_->y_labelFunc(0)->arg()); }
static void s_graphYYlabelFunc(AplusGraph *gr_,A fc_) {graphYlabelFunc(gr_,fc_,1);}
static A g_graphYYlabelFunc(AplusGraph *gr_) {  return (A) getVarFunc((AClientData *) gr_->y_labelFunc(1)->arg()); }

void graphXsubLabelFunc(AplusGraph *gr_,A fc_,int axis_)
{ 
  if (fc_->t==Et&&fc_->n==2)
    {
      P p; p.i=fc_->p;
      AClientData *ac=new AClientData(p.a[0],p.a[1]); 
      gr_->x_subLabelFunc((AFunc)cdipv,ac,axis_);     
    }
  else if (isNull(fc_)==MSTrue) gr_->x_subLabelFunc((AFunc)0,0,axis_);     
  else showError("Invalid 'x label' Function Specification");
}
static void s_graphXsubLabelFunc(AplusGraph *gr_,A fc_) { graphXsubLabelFunc(gr_,fc_,0); }
static A g_graphXsubLabelFunc(AplusGraph *gr_) { return (A) getVarFunc((AClientData *) gr_->x_subLabelFunc()->arg()); }
static void s_graphXXsubLabelFunc(AplusGraph *gr_,A fc_) { graphXsubLabelFunc(gr_,fc_,1); }
static A g_graphXXsubLabelFunc(AplusGraph *gr_) {  return (A) getVarFunc((AClientData *) gr_->x_subLabelFunc(1)->arg()); }

void graphXlabelFunc(AplusGraph *gr_,A fc_,int axis_)
{ 
  if (fc_->t==Et&&fc_->n==2)
    {
      P p; p.i=fc_->p;
      AClientData *ac=new AClientData(p.a[0],p.a[1]); 
      gr_->x_labelFunc((AFunc)cdipv,ac,axis_);
    }
  else if (isNull(fc_)==MSTrue) gr_->x_labelFunc((AFunc)0,0,axis_);     
  else showError("Invalid 'x label' Function Specification");
}
static void s_graphXlabelFunc(AplusGraph *gr_,A fc_) {graphXlabelFunc(gr_,fc_,0);}
static A g_graphXlabelFunc(AplusGraph *gr_) { return (A) getVarFunc((AClientData *) gr_->x_labelFunc()->arg()); }
static void s_graphXXlabelFunc(AplusGraph *gr_,A fc_) {graphXlabelFunc(gr_,fc_,1);}
static A g_graphXXlabelFunc(AplusGraph *gr_) { return (A) getVarFunc((AClientData *) gr_->x_labelFunc(1)->arg()); }


A q_traceStyle(AplusTraceSet *tp_)      { return tp_->enumSymbols("traceStyle"); }
A q_traceLineStyle(AplusTraceSet *tp_)  { return tp_->enumSymbols("traceLineStyle");}
A q_traceSymbol(AplusTraceSet *tp_)     { return tp_->enumSymbols("traceSymbol"); }
A q_traceConstraint(AplusTraceSet *tp_) { return tp_->enumSymbols("traceConstraint");}
A q_graphMode(AplusGraph *gr_)      { return gr_->enumSymbols("graphMode"); }
A q_graphUIMode(AplusGraph *gr_)    { return gr_->enumSymbols("graphUIMode"); }
A q_graphStyle(AplusGraph *gr_)     { return gr_->enumSymbols("graphStyle"); }
A q_graphAxis(AplusGraph *gr_)      { return gr_->enumSymbols("axis"); }
A q_graphYmode(AplusGraph *gr_)     { return gr_->enumSymbols("ymode"); }
A q_graphRule(AplusGraph *gr_) 	    { return gr_->enumSymbols("rule"); }
A q_graphGrid(AplusGraph *gr_) 	    { return gr_->enumSymbols("grid"); }
A q_graphZero(AplusGraph *gr_) 	    { return gr_->enumSymbols("grid"); }
A q_graphGridStyle(AplusGraph *gr_) { return gr_->enumSymbols("gridStyle"); }
A q_graphZeroStyle(AplusGraph *gr_) { return gr_->enumSymbols("gridStyle"); }
A q_graphTickStyle(AplusGraph *gr_) { return gr_->enumSymbols("tickStyle"); }

A q_graphLegendPosition(void)
{
  AplusGraph::LegendAlignConverter converter;
  return converter.stringDomain();
}
A q_graphLegendStyle(AplusGraph *gr_) 	 { return gr_->enumSymbols("legendStyle"); }
A q_graphYtitleStyle(AplusGraph *gr_) 	 { return gr_->enumSymbols("ylabelStyle"); }

static A g_graphXextents(AplusGraph *gr_)  	      { return gr_->xExtents(0); }
static A g_graphXXextents(AplusGraph *gr_) 	      { return gr_->xExtents(1); }
static A g_graphYextents(AplusGraph *gr_)  	      { return gr_->yExtents(0); }
static A g_graphYYextents(AplusGraph *gr_) 	      { return gr_->yExtents(1); }
static A g_graphXlabelWidth(AplusGraph *gr_,A value_) { return gr_->xLabelWidth(value_,0); }
static A g_graphXXlabelWidth(AplusGraph *gr_,A value_){ return gr_->xLabelWidth(value_,1); }
static A g_graphYlabelWidth(AplusGraph *gr_,A value_) { return gr_->yLabelWidth(value_,0); }
static A g_graphYYlabelWidth(AplusGraph *gr_,A value_){ return gr_->yLabelWidth(value_,1); }
static A g_graphXlabelHeight(AplusGraph *gr_)  	      { return gr_->xLabelHeight(0); }
static A g_graphXXlabelHeight(AplusGraph *gr_) 	      { return gr_->xLabelHeight(1); }
static A g_graphYlabelHeight(AplusGraph *gr_)  	      { return gr_->yLabelHeight(0); }
static A g_graphYYlabelHeight(AplusGraph *gr_) 	      { return gr_->yLabelHeight(1); }

void bGraphDebug(MSBoolean flag_) { MSGraph::debug(flag_==MSTrue?MSFalse:MSTrue); }

static I g_graphTraceCount(AplusGraph *gr_) { return (I) gr_->traceCount(); }

static void s_graphXYcoordinate(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft)&&value_->n==2)
    { P p; 
      p.i=value_->p; gr_->xCursorValue(MSBottom, value_->t==Ft?p.f[0]:(double)p.i[0]);
      gr_->yCursorValue(MSLeft, value_->t==Ft?p.f[1]:(double)p.i[1]); } }

static A g_graphXYcoordinate(AplusGraph *gr_) { return gr_->coordinate(0);}

static void s_graphXXYYcoordinate(AplusGraph *gr_, A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft)&&value_->n==2)
    { P p; 
      p.i=value_->p; gr_->xCursorValue(MSTop, value_->t==Ft?p.f[0]:(double)p.i[0]);
      gr_->yCursorValue(MSRight, value_->t==Ft?p.f[1]:(double)p.i[1]); } }

static A g_graphXXYYcoordinate(AplusGraph *gr_) { return gr_->coordinate(1);}

static void s_graphSessionPeriod(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
   { P p; 
     p.i=value_->p; gr_->sessionPeriod(value_->t==Ft?p.f[0]:(double)p.i[0]);} }
static A g_graphSessionPeriod(AplusGraph *gr_) 
{ A r=gf(gr_->sessionPeriod()); return r; }

static void s_graphSessionOffset(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
   { P p; 
     p.i=value_->p; gr_->sessionOffset(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_graphSessionOffset(AplusGraph *gr_) { A r=gf(gr_->sessionOffset()); return r; }

static void s_graphTpoPriceInc(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
   { P p; 
     p.i=value_->p; gr_->tpoPriceInc(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_graphTpoPriceInc(AplusGraph *gr_) { A r=gf(gr_->tpoPriceInc()); return r; }

static void s_graphTpoPeriod(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
   { P p; 
     p.i=value_->p; gr_->tpoPeriod(value_->t==Ft?p.f[0]:(double)p.i[0]);} }
static A g_graphTpoPeriod(AplusGraph *gr_) { A r=gf(gr_->tpoPeriod()); return r; }

static void s_graphTpoOpen(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
   { P p; p.i=value_->p; gr_->tpoOpen(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_graphTpoOpen(AplusGraph *gr_) { A r=gf(gr_->tpoOpen()); return r; }

static void s_graphTpoClose(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
   { P p; p.i=value_->p; gr_->tpoClose(value_->t==Ft?p.f[0]:(double)p.i[0]);} }
static A g_graphTpoClose(AplusGraph *gr_) { A r=gf(gr_->tpoClose()); return r; }


static void s_graphXminimum(AplusGraph *gr_,A value_) 
{
  if (isNull(value_)==MSTrue)
    {
      gr_->axisMinimum(MSGraph::Unset, MSBottom);
      return;
    }

  if (!QS(value_)&&(value_->t==It || value_->t==Ft))
   {
     P p;
     p.i=value_->p;
     gr_->axisMinimum(value_->t==Ft?p.f[0]:(double)p.i[0], MSBottom);
   }
}

static A g_graphXminimum(AplusGraph *gr_) { A r=gf(gr_->axisMinimum(MSBottom)); return r; }

static void s_graphXXminimum(AplusGraph *gr_,A value_) 
{ if (isNull(value_)==MSTrue) { gr_->axisMinimum(MSGraph::Unset, MSTop); return; }
  if (!QS(value_)&&(value_->t==It || value_->t==Ft))
   { P p; 
     p.i=value_->p; gr_->axisMinimum(value_->t==Ft?p.f[0]:(double)p.i[0],MSTop); } }
static A g_graphXXminimum(AplusGraph *gr_) { A r=gf(gr_->axisMinimum(MSTop)); return r; }

static void s_graphXmaximum(AplusGraph *gr_,A value_) 
{ if (isNull(value_)==MSTrue) { gr_->axisMaximum(MSGraph::Unset, MSBottom); return; }
  if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->axisMaximum(value_->t==Ft?p.f[0]:(double)p.i[0], MSBottom);} }
static A g_graphXmaximum(AplusGraph *gr_) { A r=gf(gr_->axisMaximum(MSBottom));  return r; }

static void s_graphXXmaximum(AplusGraph *gr_,A value_) 
{ if (isNull(value_)==MSTrue) { gr_->axisMaximum(MSGraph::Unset, MSTop); return; }
  if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p;
    p.i=value_->p; gr_->axisMaximum(value_->t==Ft?p.f[0]:(double)p.i[0],MSTop);} }
static A g_graphXXmaximum(AplusGraph *gr_) {A r=gf(gr_->axisMaximum(MSTop));  return r;}

static void s_graphYminimum(AplusGraph *gr_,A value_) 
{ if (isNull(value_)==MSTrue) { gr_->axisMinimum(MSGraph::Unset, MSLeft); return; }
  if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->axisMinimum(value_->t==Ft?p.f[0]:(double)p.i[0], MSLeft);} }
static A g_graphYminimum(AplusGraph *gr_) { A r=gf(gr_->axisMinimum(MSLeft));  return r; }

static void s_graphYYminimum(AplusGraph *gr_,A value_) 
{ if (isNull(value_)==MSTrue) { gr_->axisMinimum(MSGraph::Unset, MSRight); return; }
  if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->axisMinimum(value_->t==Ft?p.f[0]:(double)p.i[0],MSRight); } }
static A g_graphYYminimum(AplusGraph *gr_) { A r=gf(gr_->axisMinimum(MSRight)); return r;}

static void s_graphYmaximum(AplusGraph *gr_,A value_) 
{ if (isNull(value_)==MSTrue) { gr_->axisMaximum(MSGraph::Unset, MSLeft); return; }
  if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->axisMaximum(value_->t==Ft?p.f[0]:(double)p.i[0], MSLeft);} }
static A g_graphYmaximum(AplusGraph *gr_) { A r=gf(gr_->axisMaximum(MSLeft)); return r; }

static void s_graphYYmaximum(AplusGraph *gr_,A value_) 
{ if (isNull(value_)==MSTrue) { gr_->axisMaximum(MSGraph::Unset, MSRight); return; }
  if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->axisMaximum(value_->t==Ft?p.f[0]:(double)p.i[0], MSRight); } }
static A g_graphYYmaximum(AplusGraph *gr_) {A r=gf(gr_->axisMaximum(MSRight)); return r;}



static void s_graphXincrement(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->axisLabelIncrement(value_->t==Ft?p.f[0]:(double)p.i[0], MSBottom); } }
static A g_graphXincrement(AplusGraph *gr_){A r=gf(gr_->axisLabelIncrement(MSBottom)); return r;}

static void s_graphXXincrement(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->axisLabelIncrement(value_->t==Ft?p.f[0]:(double)p.i[0],MSTop); } }
static A g_graphXXincrement(AplusGraph *gr_) 
{ A r=gf(gr_->axisLabelIncrement(MSTop)); return r; }

static void s_graphYincrement(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->axisLabelIncrement(value_->t==Ft?p.f[0]:(double)p.i[0], MSLeft); } }
static A g_graphYincrement(AplusGraph *gr_) 
{ A r=gf(gr_->axisLabelIncrement(MSLeft));  return r; }

static void s_graphYYincrement(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->axisLabelIncrement(value_->t==Ft?p.f[0]:(double)p.i[0],MSRight); } }
static A g_graphYYincrement(AplusGraph *gr_) 
{ A r=gf(gr_->axisLabelIncrement(MSRight));  return r; }




static void s_graphMode(AplusGraph *gr_,A mode_) { gr_->graphAMode(mode_); }
static A g_graphMode(AplusGraph *gr_) { return (A) gr_->graphAMode(); }

static void s_graphUIMode(AplusGraph *gr_,A mode_) { gr_->graphAUIMode(mode_); }
static A g_graphUIMode(AplusGraph *gr_) { return (A) gr_->graphAUIMode(); }

static void s_graphStyle(MSWidgetView *pWidget_,A value_)
{
  AplusReportStyleConverter converter;
  unsigned long style = converter(value_);
  if (style!=converter.enumNotFound())
    {
      ((AplusGraph *)pWidget_)->style(style);
    }
}

static A g_graphStyle(MSWidgetView *pWidget_)
{
  AplusReportStyleConverter converter;
  return converter(((AplusGraph *)pWidget_)->style());
}

static void s_graphAxis(AplusGraph *gr_,A style_) { gr_->axisAMode(style_); }
static A g_graphAxis(AplusGraph *gr_) { return (A) gr_->axisAMode(); }

static void s_graphYmode(AplusGraph *gr_,A style_) { gr_->yAMode(style_,MSLeft); }
static A g_graphYmode(AplusGraph *gr_) { return (A) gr_->yAMode(MSLeft); }

static void s_graphYYmode(AplusGraph *gr_,A style_) { gr_->yAMode(style_,MSRight); }
static A g_graphYYmode(AplusGraph *gr_) { return (A) gr_->yAMode(MSRight); }

static void s_graphRule(AplusGraph *gr_,A style_) { gr_->axisARule(style_); }
static A g_graphRule(AplusGraph *gr_) { return (A) gr_->axisARule(); }

static void s_graphGrid(AplusGraph *gr_,A style_) { gr_->gridA(style_); }
static A g_graphGrid(AplusGraph *gr_) { return (A) gr_->gridA(); }

static void s_graphGridStyle(AplusGraph *gr_,A style_) { gr_->gridAStyle(style_); }
static A g_graphGridStyle(AplusGraph *gr_) { return (A) gr_->gridAStyle(); }

static void s_graphZero(AplusGraph *gr_,A style_) { gr_->zeroA(style_); }
static A g_graphZero(AplusGraph *gr_) { return (A) gr_->zeroA(); }

static void s_graphZeroStyle(AplusGraph *gr_,A style_) { gr_->zeroAStyle(style_); }
static A g_graphZeroStyle(AplusGraph *gr_) { return (A) gr_->zeroAStyle(); }



static void s_graphScreenLeftMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->margin(value_->t==Ft?p.f[0]:(double)p.i[0],MSLeft); } }
static A g_graphScreenLeftMargin(const AplusGraph *gr_) { A r=gf(gr_->margin(MSLeft));  return r; }

static void s_graphScreenRightMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->margin(value_->t==Ft?p.f[0]:(double)p.i[0],MSRight); } }
static A g_graphScreenRightMargin(const AplusGraph *gr_) { A r=gf(gr_->margin(MSRight));  return r; }

static void s_graphScreenTopMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->margin(value_->t==Ft?p.f[0]:(double)p.i[0],MSTop); } }
static A g_graphScreenTopMargin(const AplusGraph *gr_) { A r=gf(gr_->margin(MSTop));  return r; }

static void s_graphScreenBottomMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->margin(value_->t==Ft?p.f[0]:(double)p.i[0],MSBottom); } }
static A g_graphScreenBottomMargin(const AplusGraph *gr_) {A r=gf(gr_->margin(MSBottom));  return r;}

static void s_graphXleftMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->bottomAxisDataMargin(value_->t==Ft?p.f[0]:(double)p.i[0], MSLeft); } }
static A g_graphXleftMargin(AplusGraph *gr_) {A r=gf(gr_->bottomAxisDataMargin(MSLeft));return r;}

static void s_graphXrightMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->bottomAxisDataMargin(value_->t==Ft?p.f[0]:(double)p.i[0], MSRight); } }
static A g_graphXrightMargin(AplusGraph *gr_){A r=gf(gr_->bottomAxisDataMargin(MSRight));return r;}

static void s_graphXXleftMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->topAxisDataMargin(value_->t==Ft?p.f[0]:(double)p.i[0], MSLeft); } }
static A g_graphXXleftMargin(AplusGraph *gr_){A r=gf(gr_->topAxisDataMargin(MSLeft));return r;}

static void s_graphXXrightMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->topAxisDataMargin(value_->t==Ft?p.f[0]:(double)p.i[0], MSRight); } }
static A g_graphXXrightMargin(AplusGraph *gr_) 
{ A r=gf(gr_->topAxisDataMargin(MSRight)); return r; }

static void s_graphYtopMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->leftAxisDataMargin(value_->t==Ft?p.f[0]:(double)p.i[0],MSTop); } }
static A g_graphYtopMargin(AplusGraph *gr_){A r=gf(gr_->leftAxisDataMargin(MSTop)); return r;}

static void s_graphYbottomMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->leftAxisDataMargin(value_->t==Ft?p.f[0]:(double)p.i[0],MSBottom); } }
static A g_graphYbottomMargin(AplusGraph *gr_)
{A r=gf(gr_->leftAxisDataMargin(MSBottom));return r; }

static void s_graphYYtopMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->rightAxisDataMargin(value_->t==Ft?p.f[0]:(double)p.i[0],MSTop); } }
static A g_graphYYtopMargin(AplusGraph *gr_) {A r=gf(gr_->rightAxisDataMargin(MSTop));return r;}

static void s_graphYYbottomMargin(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; 
    p.i=value_->p; gr_->rightAxisDataMargin(value_->t==Ft?p.f[0]:(double)p.i[0],MSBottom); } }
static A g_graphYYbottomMargin(AplusGraph *gr_) 
{ A r=gf(gr_->rightAxisDataMargin(MSBottom));  return r; }



static void s_graphLegendHlThickness(AplusGraph *gr_,int width_) 
{ gr_->legendHighlightThickness(width_); }
static I g_graphLegendHlThickness(AplusGraph *gr_) 
{ return (I) gr_->legendHighlightThickness(); }

static void s_graphLegendShadowThickness(AplusGraph *gr_,int width_) 
{ gr_->legendShadowThickness(width_); }
static I g_graphLegendShadowThickness(AplusGraph *gr_) 
{ return (I) gr_->legendShadowThickness(); }

static void s_graphLegendBg(AplusGraph *gr_,unsigned long color_) 
{ gr_->legendBackground(color_); }
static unsigned long g_graphLegendBg(AplusGraph *gr_) 
{ return (unsigned long) gr_->legendBackground(); }

static void s_graphLegendFg(AplusGraph *gr_,unsigned long color_) 
{ gr_->legendForeground(color_); }
static unsigned long g_graphLegendFg(AplusGraph *gr_) 
{ return (unsigned long) gr_->legendForeground(); }

static void s_graphLegendPosition(MSWidgetView *pWidget_, A value_) 
{
  AplusGraph::LegendAlignConverter converter;
  unsigned long align = converter(value_);
  if (align!=converter.enumNotFound())
    {
      AplusGraph *pGraph = (AplusGraph *)pWidget_;
      pGraph->xLegendPosition(0); // flush the precise position settings first
      pGraph->yLegendPosition(0);
      ((AplusGraph *)pWidget_)->legendAlignment(align);
    }
}

static A g_graphLegendPosition(MSWidgetView *pWidget_) 
{
  AplusGraph::LegendAlignConverter converter;
  return converter(((AplusGraph *)pWidget_)->legendAlignment());
}  

static void s_graphLegendStyle(AplusGraph *gr_,A style_){gr_->legendAStyle(style_);}
static A g_graphLegendStyle(AplusGraph *gr_) { return (A) gr_->legendAStyle(); }

static void s_graphLegendFont(AplusGraph *gr_,Font font_) 
{ gr_->legendFont(font_); }
static Font g_graphLegendFont(AplusGraph *gr_) 
{ return (Font) gr_->legendFont(); }


static void s_graphYtitleStyle(AplusGraph *gr_,A style_) { gr_->yTitleAStyle(style_,MSLeft); }
static A g_graphYtitleStyle(AplusGraph *gr_) {return (A) gr_->yTitleAStyle(MSLeft);}

static void s_graphYYtitleStyle(AplusGraph *gr_,A style_) { gr_->yTitleAStyle(style_,MSRight); }
static A g_graphYYtitleStyle(AplusGraph *gr_){return (A) gr_->yTitleAStyle(MSRight);}



static void s_graphXlegend(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->xLegendPosition(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_graphXlegend(AplusGraph *gr_) { A r=gf(gr_->xLegendPosition()); return r; }

static void s_graphYlegend(AplusGraph *gr_,A value_) 
{ if (!QS(value_)&&(value_->t==It || value_->t==Ft))
  { P p; p.i=value_->p; gr_->yLegendPosition(value_->t==Ft?p.f[0]:(double)p.i[0]); } }
static A g_graphYlegend(AplusGraph *gr_) { A r=gf(gr_->yLegendPosition()); return r; }



static void s_graphTitleAlign(AplusGraph *gr_,A just_) { gr_->titleAlignment(GUIEnum.alignFormat(just_)); }
static A g_graphTitleAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->titleAlignment()); }

static void s_graphSubtitle(AplusGraph *gr_,A title_) { gr_->subTitleA(title_); }
static A g_graphSubtitle(AplusGraph *gr_) { A ap=gr_->subTitleA(); return(QA(ap)&&isNull(ap)==MSFalse)?(A)ic(ap):ap; }

static void s_graphSubtitleColor(AplusGraph *gr_,unsigned long color_) { gr_->subtitleForeground(color_); }
static unsigned long g_graphSubtitleColor(AplusGraph *gr_) { return (unsigned long) gr_->subtitleForeground(); }

static void s_graphSubtitleFont(AplusGraph *gr_,Font id_) {gr_->subtitleFont(id_);}
static Font g_graphSubtitleFont(AplusGraph *gr_){return(Font)gr_->subtitleFont(); }

static void s_graphSubtitleAlign(AplusGraph *gr_, A just_){ gr_->subtitleAlignment((MSAlignment)GUIEnum.alignFormat(just_));}
static A g_graphSubtitleAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->subtitleAlignment()); }

static void s_graphFootnote(AplusGraph *gr_,A title_) { gr_->footnoteA(title_); }
static A g_graphFootnote(AplusGraph *gr_) { A ap=gr_->footnoteA(); return(QA(ap)&&isNull(ap)==MSFalse)?(A)ic(ap):ap; }

static void s_graphFootnoteColor(AplusGraph *gr_,unsigned long color_) { gr_->footnoteForeground(color_); }
static unsigned long g_graphFootnoteColor(AplusGraph *gr_) { return (unsigned long) gr_->footnoteForeground(); }

static void s_graphFootnoteFont(AplusGraph *gr_,Font id_) {gr_->footnoteFont(id_);}
static Font g_graphFootnoteFont(AplusGraph *gr_) {return(Font)gr_->footnoteFont();}

static void s_graphFootnoteAlign(AplusGraph *gr_, A just_) { gr_->footnoteAlignment((MSAlignment)GUIEnum.alignFormat(just_));}
static A g_graphFootnoteAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->footnoteAlignment());}

static void s_graphXsubLabelAlign(AplusGraph *gr_, A just_) { gr_->axisSubLabelAlignment((MSAlignment)GUIEnum.alignFormat(just_), MSBottom);}
static A g_graphXsubLabelAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisSubLabelAlignment(MSBottom)); }

static void s_graphXXsubLabelAlign(AplusGraph *gr_, A just_) { gr_->axisSubLabelAlignment((MSAlignment)GUIEnum.alignFormat(just_), MSTop);}
static A g_graphXXsubLabelAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisSubLabelAlignment(MSTop)); }

static void s_graphXlabelAlign(AplusGraph *gr_, A just_) { gr_->axisLabelAlignment((MSAlignment)GUIEnum.alignFormat(just_), MSBottom);}
static A g_graphXlabelAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisLabelAlignment(MSBottom)); }

static void s_graphXXlabelAlign(AplusGraph *gr_, A just_) { gr_->axisLabelAlignment((MSAlignment)GUIEnum.alignFormat(just_), MSTop);}
static A g_graphXXlabelAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisLabelAlignment(MSTop)); }

static void s_graphYlabelAlign(AplusGraph *gr_, A just_) { gr_->axisLabelAlignment((MSAlignment)GUIEnum.alignFormat(just_), MSLeft);}
static A g_graphYlabelAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisLabelAlignment(MSLeft)); }

static void s_graphYYlabelAlign(AplusGraph *gr_, A just_) { gr_->axisLabelAlignment((MSAlignment)GUIEnum.alignFormat(just_), MSRight);}
static A g_graphYYlabelAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisLabelAlignment(MSRight)); }

static void s_graphXtitle(AplusGraph *gr_,char *title_) { gr_->axisTitle(title_, MSBottom); }
static const char *g_graphXtitle(AplusGraph *gr_) { return gr_->axisTitle(MSBottom).asString(); }

static void s_graphXXtitle(AplusGraph *gr_,char *title_) {gr_->axisTitle(title_,MSTop); }
static const char *g_graphXXtitle(AplusGraph *gr_) { return gr_->axisTitle(MSTop).asString(); }

static void s_graphYtitle(AplusGraph *gr_,char *title_) { gr_->axisTitle(title_, MSLeft); }
static const char *g_graphYtitle(AplusGraph *gr_) { return gr_->axisTitle(MSLeft).asString(); }

static void s_graphYYtitle(AplusGraph *gr_,char *title_) { gr_->axisTitle(title_,MSRight); }
static const char *g_graphYYtitle(AplusGraph *gr_) { return gr_->axisTitle(MSRight).asString(); }

static void s_graphXtitleColor(AplusGraph *gr_,unsigned long color_) { gr_->axisTitleForeground(color_, MSBottom); }
static unsigned long g_graphXtitleColor(AplusGraph *gr_) { return (unsigned long) gr_->axisTitleForeground(MSBottom); }

static void s_graphXXtitleColor(AplusGraph *gr_,unsigned long color_) { gr_->axisTitleForeground(color_,MSTop); }
static unsigned long g_graphXXtitleColor(AplusGraph *gr_) { return (unsigned long) gr_->axisTitleForeground(MSTop); }

static void s_graphYtitleColor(AplusGraph *gr_,unsigned long color_) { gr_->axisTitleForeground(color_, MSLeft); }
static unsigned long g_graphYtitleColor(AplusGraph *gr_) { return (unsigned long) gr_->axisTitleForeground(MSLeft); }

static void s_graphYYtitleColor(AplusGraph *gr_,unsigned long color_) { gr_->axisTitleForeground(color_,MSRight); }
static unsigned long g_graphYYtitleColor(AplusGraph *gr_) { return (unsigned long) gr_->axisTitleForeground(MSRight); }

static void s_graphXtitleFont(AplusGraph *gr_,Font id_) { gr_->axisTitleFont(id_, MSBottom); }
static Font g_graphXtitleFont(AplusGraph *gr_) {return (Font) gr_->axisTitleFont(MSBottom); }

static void s_graphXXtitleFont(AplusGraph *gr_,Font id_) {gr_->axisTitleFont(id_,MSTop);}
static Font g_graphXXtitleFont(AplusGraph *gr_){return(Font)gr_->axisTitleFont(MSTop); }

static void s_graphYtitleFont(AplusGraph *gr_,Font id_) { gr_->axisTitleFont(id_, MSLeft); }
static Font g_graphYtitleFont(AplusGraph *gr_) {return(Font)gr_->axisTitleFont(MSLeft); }

static void s_graphYYtitleFont(AplusGraph *gr_,Font id_) { gr_->axisTitleFont(id_,MSRight);}
static Font g_graphYYtitleFont(AplusGraph *gr_) {return(Font)gr_->axisTitleFont(MSRight);}

static void s_graphXtitleAlign(AplusGraph *gr_, A just_) { gr_->axisTitleAlignment((MSAlignment)GUIEnum.alignFormat(just_),MSBottom); }
static A g_graphXtitleAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisTitleAlignment(MSBottom)); }

static void s_graphXXtitleAlign(AplusGraph *gr_, A just_) { gr_->axisTitleAlignment((MSAlignment)GUIEnum.alignFormat(just_),MSTop); }
static A g_graphXXtitleAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisTitleAlignment(MSTop)); }

static void s_graphYtitleAlign(AplusGraph *gr_, A just_)
{
  unsigned long oldAlign = gr_->axisTitleAlignment(MSLeft);
  unsigned long newAlign = GUIEnum.alignFormat(just_);
  if (MSG::Vertical & oldAlign) newAlign|=MSG::Vertical;  // Need to preserver the style bits
  else newAlign|=MSG::Horizontal;
  gr_->axisTitleAlignment(newAlign,MSLeft);
}
static A g_graphYtitleAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisTitleAlignment(MSLeft)); }

static void s_graphYYtitleAlign(AplusGraph *gr_, A just_)
{
  unsigned long oldAlign = gr_->axisTitleAlignment(MSRight);
  unsigned long newAlign = GUIEnum.alignFormat(just_);
  if (MSG::Vertical & oldAlign) newAlign|=MSG::Vertical;  // Need to preserver the style bits
  else newAlign|=MSG::Horizontal;
  gr_->axisTitleAlignment(newAlign,MSRight);
}

static A g_graphYYtitleAlign(AplusGraph *gr_) { return GUIEnum.alignFormat(gr_->axisTitleAlignment(MSRight)); }



static void s_graphXcolor(AplusGraph *gr_,unsigned long color_) { gr_->axisForeground(color_, MSBottom); }
static unsigned long g_graphXcolor(AplusGraph *gr_) { return (unsigned long) gr_->axisForeground(MSBottom); }

static void s_graphXXcolor(AplusGraph *gr_,unsigned long color_) {gr_->axisForeground(color_,MSTop); }
static unsigned long g_graphXXcolor(AplusGraph *gr_) { return (unsigned long) gr_->axisForeground(MSTop); }

static void s_graphYcolor(AplusGraph *gr_,unsigned long color_) { gr_->axisForeground(color_, MSLeft); }
static unsigned long g_graphYcolor(AplusGraph *gr_) { return (unsigned long) gr_->axisForeground(MSLeft); }

static void s_graphYYcolor(AplusGraph *gr_,unsigned long color_) { gr_->axisForeground(color_,MSRight); }
static unsigned long g_graphYYcolor(AplusGraph *gr_) { return (unsigned long) gr_->axisForeground(MSRight); }

static void s_graphXlabelFont(AplusGraph *gr_,Font id_) { gr_->axisLabelFont(id_, MSBottom); }
static Font g_graphXlabelFont(AplusGraph *gr_) {return (Font) gr_->axisLabelFont(MSBottom); }

static void s_graphXXlabelFont(AplusGraph *gr_,Font id_) {gr_->axisLabelFont(id_,MSTop);}
static Font g_graphXXlabelFont(AplusGraph *gr_) {return(Font)gr_->axisLabelFont(MSTop);}

static void s_graphYlabelFont(AplusGraph *gr_,Font id_) { gr_->axisLabelFont(id_, MSLeft); }
static Font g_graphYlabelFont(AplusGraph *gr_) {return (Font) gr_->axisLabelFont(MSLeft); }

static void s_graphYYlabelFont(AplusGraph *gr_,Font id_) {gr_->axisLabelFont(id_,MSRight);}
static Font g_graphYYlabelFont(AplusGraph *gr_) {return(Font)gr_->axisLabelFont(MSRight); }


static void s_graphRuleWidth(AplusGraph *gr_,int value_) {gr_->axisRuleWidth(value_); }
static I g_graphRuleWidth(AplusGraph *gr_) { return (I) gr_->axisRuleWidth(); }

static void s_graphGridWidth(AplusGraph *gr_,int value_) {gr_->gridWidth(value_); }
static I g_graphGridWidth(AplusGraph *gr_) { return (I) gr_->gridWidth(); }

static void s_graphGridColor(AplusGraph *gr_,unsigned long color_) { gr_->gridForeground(color_); }
static unsigned long g_graphGridColor(AplusGraph *gr_) { return (unsigned long) gr_->gridForeground(); }

static void s_graphZeroWidth(AplusGraph *gr_,int value_) {gr_->zeroAxisWidth(value_);}
static I g_graphZeroWidth(AplusGraph *gr_) { return (I) gr_->zeroAxisWidth(); }

static void s_graphZeroColor(AplusGraph *gr_,unsigned long color_) { gr_->zeroAxisForeground(color_); }
static unsigned long g_graphZeroColor(AplusGraph *gr_) { return (unsigned long) gr_->zeroAxisForeground(); }


static void s_graphXtickStyle(AplusGraph *gr_,A position_) { gr_->tickStyleA(position_, MSBottom); }
static A g_graphXtickStyle(AplusGraph *gr_) { return (A) gr_->tickStyleA(MSBottom); }

static void s_graphXXtickStyle(AplusGraph *gr_,A position_) { gr_->tickStyleA(position_,MSTop); }
static A g_graphXXtickStyle(AplusGraph *gr_) { return (A) gr_->tickStyleA(MSTop); }

static void s_graphYtickStyle(AplusGraph *gr_,A position_) { gr_->tickStyleA(position_, MSLeft); }
static A g_graphYtickStyle(AplusGraph *gr_) { return (A) gr_->tickStyleA(MSLeft); }

static void s_graphYYtickStyle(AplusGraph *gr_,A position_) { gr_->tickStyleA(position_,MSRight); }
static A g_graphYYtickStyle(AplusGraph *gr_) { return (A) gr_->tickStyleA(MSRight); }




static void s_graphXminorTicks(AplusGraph *gr_,int value_) { gr_->minorTicks(value_,MSBottom); }
static I g_graphXminorTicks(AplusGraph *gr_) { return (I) gr_->minorTicks(MSBottom);}

static void s_graphXXminorTicks(AplusGraph *gr_,int value_) { gr_->minorTicks(value_,MSTop); }
static I g_graphXXminorTicks(AplusGraph *gr_) { return(I)gr_->minorTicks(MSTop);}

static void s_graphYminorTicks(AplusGraph *gr_,int value_) { gr_->minorTicks(value_,MSLeft); }
static I g_graphYminorTicks(AplusGraph *gr_) { return (I) gr_->minorTicks(MSLeft);}

static void s_graphYYminorTicks(AplusGraph *gr_,int value_) { gr_->minorTicks(value_,MSRight); }
static I g_graphYYminorTicks(AplusGraph *gr_) { return(I)gr_->minorTicks(MSRight);}

static void s_graphXmajorTickSize(AplusGraph *gr_,int val_) { gr_->majorTickSize(val_,MSBottom); }
static I g_graphXmajorTickSize(AplusGraph *gr_) { return(I)gr_->majorTickSize(MSBottom); }

static void s_graphXXmajorTickSize(AplusGraph *gr_,int val_) { gr_->majorTickSize(val_,MSTop); }
static I g_graphXXmajorTickSize(AplusGraph *gr_) { return (I) gr_->majorTickSize(MSTop); }

static void s_graphYmajorTickSize(AplusGraph *gr_,int val_) { gr_->majorTickSize(val_,MSLeft); }
static I g_graphYmajorTickSize(AplusGraph *gr_) { return (I) gr_->majorTickSize(MSLeft); }

static void s_graphYYmajorTickSize(AplusGraph *gr_,int val_) { gr_->majorTickSize(val_,MSRight); }
static I g_graphYYmajorTickSize(AplusGraph *gr_) { return (I) gr_->majorTickSize(MSRight); }

static void s_graphXminorTickSize(AplusGraph *gr_,int val_) { gr_->minorTickSize(val_,MSBottom); }
static I g_graphXminorTickSize(AplusGraph *gr_) { return (I) gr_->minorTickSize(MSBottom); }

static void s_graphXXminorTickSize(AplusGraph *gr_,int val_) { gr_->minorTickSize(val_,MSTop); }
static I g_graphXXminorTickSize(AplusGraph *gr_) { return (I) gr_->minorTickSize(MSTop); }

static void s_graphYminorTickSize(AplusGraph *gr_,int val_) { gr_->minorTickSize(val_,MSLeft); }
static I g_graphYminorTickSize(AplusGraph *gr_) { return (I) gr_->minorTickSize(MSLeft); }

static void s_graphYYminorTickSize(AplusGraph *gr_,int val_) { gr_->minorTickSize(val_,MSRight); }
static I g_graphYYminorTickSize(AplusGraph *gr_) { return (I) gr_->minorTickSize(MSRight); }

static void s_graphBarWidth(AplusGraph *gr_,int value_) {gr_->maxBarWidth(value_);}
static I g_graphBarWidth(AplusGraph *gr_) { return (I) gr_->maxBarWidth(); }

static void s_graphDepth(AplusGraph *gr_,int value_) { gr_->graphDepth(value_); }
static I g_graphDepth(AplusGraph *gr_) { return (I) gr_->graphDepth(); }

static void s_longPressTime(AplusGraph *gr_,int value_) { gr_->longPressTime(value_); }
static long g_longPressTime(AplusGraph *gr_) { return gr_->longPressTime(); }

static void s_graphSelectDistance(AplusGraph *gr_,unsigned long value_) { gr_->selectDistance(value_); }
static unsigned long g_graphSelectDistance(AplusGraph *gr_) { return (unsigned long) gr_->selectDistance(); }

MSWidgetCommon *g_graphSelected(AplusGraph *gr_) 
{
  if (gr_->selectTrace()!=0)
   {
     AplusTrace *atp = (AplusTrace *) gr_->selectTrace();
     AVariableData *varData = pAVarDataFromV(atp->aplusVar());
     return (MSWidgetCommon *)varData->pWidgetView();
   }
  else return 0;
}

static void s_graphPieOffsetMargin(AplusGraph *pGraph_, A value_)
{
  if (QS(value_)==0)
    {
      if (value_->t==Ft)
	{
	  pGraph_->pieOffsetMargin(*(double*)value_->p);
	}
      else if (value_->t==It)
	{
	  pGraph_->pieOffsetMargin((double)*(I*)value_->p);
	}
    }
}

static A g_graphPieOffsetMargin(AplusGraph *pGraph_) { return gf(pGraph_->pieOffsetMargin()); }

// Reporting attributes

static void s_graphLeftMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusGraph *pGraph = (AplusGraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pGraph->leftMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pGraph->leftMargin((double)*(I*)value_->p);
	}
    }
}

static A g_graphLeftMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((const AplusGraph *)pWidget_)->leftMargin();
  return gf((margin<0)?-1:margin);
}

static void s_graphRightMargin(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusGraph *pGraph = (AplusGraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pGraph->rightMargin(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pGraph->rightMargin((double)*(I*)value_->p);
	}
    }
}

static A g_graphRightMargin(MSWidgetView *pWidget_)
{
  // A fix for a small MStk problem - to be fixed in MStk for release 2.6:
  // margins in inches are calculated based on the corresponding pixel offset values;
  // the default pixel offset is -1 (meaning that it's inherited from the report's pixel offset);
  // thus, the default margin is -0.0139, which is confusing to the user - it should also be -1.
  double margin = ((const AplusGraph *)pWidget_)->rightMargin();
  return gf((margin<0)?-1:margin);
}

static void s_graphTopOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusGraph *pGraph = (AplusGraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pGraph->topOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pGraph->topOffset((double)*(I*)value_->p);
	}
    }
}

static A g_graphTopOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusGraph *)pWidget_)->topPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_graphBottomOffset(MSWidgetView *pWidget_, A value_)
{
  if (!QS(value_))
    {
      AplusGraph *pGraph = (AplusGraph *)pWidget_;
      if (value_->t==Ft)	// if it's a floating point value
	{
	  pGraph->bottomOffset(*(double*)value_->p);
	}
      else if (value_->t==It)  // it's an integer
	{
	  pGraph->bottomOffset((double)*(I*)value_->p);
	}
    }
}

static A g_graphBottomOffset(MSWidgetView *pWidget_)
{
  // A fix for an MStk bug to be fixed in MStk 2.6:
  // MSPrintItem::topOffset() and MSPrintItem::bottomOffset() methods do the cast to double incorrectly
  // so the decimal part gets truncated
  double offset = ((AplusGraph *)pWidget_)->bottomPixel();
  return gf(offset/MSPointsPerInch);
}

static void s_graphPrintRow(MSWidgetView *pWidget_, int value_) { ((AplusGraph *)pWidget_)->printRow(value_); }
static I  g_graphPrintRow(MSWidgetView *pWidget_) { return (I) ((AplusGraph *)pWidget_)->printRow(); }

static void s_graphPrintColumn(MSWidgetView *pWidget_, int value_) { ((AplusGraph *)pWidget_)->printColumn(value_); }
static I  g_graphPrintColumn(MSWidgetView *pWidget_) { return (I) ((AplusGraph *)pWidget_)->printColumn(); }

static void s_graphJustify(MSWidgetView *pWidget_, A value_)
{
  AplusAlignmentConverter converter;
  unsigned long justify = converter(value_);
  if (justify!=converter.enumNotFound())
    {
      ((AplusGraph *)pWidget_)->justification(justify);
    }
}

static A g_graphJustify(MSWidgetView *pWidget_)
{
  AplusAlignmentConverter converter;
  return converter(((AplusGraph *)pWidget_)->justification());
}  

static void s_graphPageAlign(MSWidgetView *pWidget_, A value_)
{
  AplusAlignmentConverter converter;
  unsigned long align = converter(value_);
  if (align!=converter.enumNotFound())
    {
      ((AplusGraph *)pWidget_)->pageAlignment(align);
    }
}  

static A g_graphPageAlign(MSWidgetView *pWidget_)
{
  AplusAlignmentConverter converter;
  return converter(((AplusGraph *)pWidget_)->pageAlignment());
}


extern MSWidgetView *validateParent(MSWidgetView *parent_);

MSWidgetView *c_AXGraph(MSWidgetView *parent_)
{ return (MSWidgetView *) new AplusGraph(validateParent(parent_)); }
MSWidgetView *c_AXTrace(MSWidgetView *parent_)  // This Function shouldn't get called
{ return (MSWidgetView *) new AplusTraceSet((MSGraph *)validateParent(parent_)); }



void AGIFGraphInstall(void)
{
  CX context=Cx;
  Cx=cx("s");

  install((PFI)c_AXGraph,"c_AXGraph",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)c_AXTrace,"c_AXTrace",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceLineStyleFunc,"s_traceLineStyleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceLineStyleFunc,"g_traceLineStyleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceLineWidthFunc,"s_traceLineWidthFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceLineWidthFunc,"g_traceLineWidthFunc",A_,1,IV,0,0,0,0,0,0,0);
  //  install((PFI)s_tracePieOffsetFunc,"s_tracePieOffsetFunc",V_,2,IV,A_,0,0,0,0,0,0);
  //  install((PFI)g_tracePieOffsetFunc,"g_tracePieOffsetFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceLineColorFunc,"s_traceLineColorFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceLineColorFunc,"g_traceLineColorFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceLegendFunc,"s_traceLegendFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceLegendFunc,"g_traceLegendFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceStyleFunc,"s_traceStyleFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceStyleFunc,"g_traceStyleFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceSymbolFunc,"s_traceSymbolFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceSymbolFunc,"g_traceSymbolFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceSymbolSizeFunc,"s_traceSymbolSizeFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceSymbolSizeFunc,"g_traceSymbolSizeFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceFillColorFunc,"s_traceFillColorFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceFillColorFunc,"g_traceFillColorFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceYaxisFunc,"s_traceYYaxisFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceYaxisFunc,"g_traceYYaxisFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceXaxisFunc,"s_traceXaxisFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceXaxisFunc,"g_traceXaxisFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceGradientFunc,"s_traceGradientFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceGradientFunc,"g_traceGradientFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_traceText,"g_traceText",CP,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceSelected,"s_traceSelected",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceSelected,"g_traceSelected",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceCoordinate,"s_traceCoordinate",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceCoordinate,"g_traceCoordinate",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceConstraint,"s_traceConstraint",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_traceConstraint,"g_traceConstraint",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceSelectable,"s_traceSelectable",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_traceSelectable,"g_traceSelectable",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePieAngle,"s_tracePieAngle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tracePieAngle,"g_tracePieAngle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePieOffsets,"s_tracePieOffsets",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tracePieOffsets,"g_tracePieOffsets",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePieAspectRatio,"s_tracePieAspectRatio",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tracePieAspectRatio,"g_tracePieAspectRatio",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePieDepthFactor,"s_tracePieDepthFactor",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tracePieDepthFactor,"g_tracePieDepthFactor",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePieProfiles,"s_tracePieProfiles",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tracePieProfiles,"g_tracePieProfiles",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePieLegendAlign,"s_tracePieLegendAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tracePieLegendAlign,"g_tracePieLegendAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePieValueAlign,"s_tracePieValueAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tracePieValueAlign,"g_tracePieValueAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePiePercentAlign,"s_tracePiePercentAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tracePiePercentAlign,"g_tracePiePercentAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePrimarySlice,"s_tracePrimarySlice",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_tracePrimarySlice,"g_tracePrimarySlice",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_tracePrimarySliceAlign,"s_tracePrimarySliceAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_tracePrimarySliceAlign,"g_tracePrimarySliceAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceTextFont,"s_traceTextFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_traceTextFont,"g_traceTextFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_traceTextFg,"s_traceTextFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_traceTextFg,"g_traceTextFg",IV,1,IV,0,0,0,0,0,0,0);

  install((PFI)q_traceStyle,"q_traceStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_traceLineStyle,"q_traceLineStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_traceSymbol,"q_traceSymbol",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_traceConstraint,"q_traceConstraint",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYlabelFormatFunc,"s_graphYlabelFormatFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYlabelFormatFunc,"g_graphYlabelFormatFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYlabelFormatFunc,"s_graphYYlabelFormatFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYlabelFormatFunc,"g_graphYYlabelFormatFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXsubLabelFormatFunc,"s_graphXsubLabelFormatFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXsubLabelFormatFunc,"g_graphXsubLabelFormatFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXsubLabelFormatFunc,"s_graphXXsubLabelFormatFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXsubLabelFormatFunc,"g_graphXXsubLabelFormatFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXlabelFormatFunc,"s_graphXlabelFormatFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXlabelFormatFunc,"g_graphXlabelFormatFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXlabelFormatFunc,"s_graphXXlabelFormatFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXlabelFormatFunc,"g_graphXXlabelFormatFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYlabelFunc,"s_graphYlabelFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYlabelFunc,"g_graphYlabelFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYlabelFunc,"s_graphYYlabelFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYlabelFunc,"g_graphYYlabelFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXlabelFunc,"s_graphXlabelFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXlabelFunc,"g_graphXlabelFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXlabelFunc,"s_graphXXlabelFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXlabelFunc,"g_graphXXlabelFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXsubLabelFunc,"s_graphXsubLabelFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXsubLabelFunc,"g_graphXsubLabelFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXsubLabelFunc,"s_graphXXsubLabelFunc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXsubLabelFunc,"g_graphXXsubLabelFunc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_graphXlabelWidth,"g_graphXlabelWidth",A_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXlabelWidth,"g_graphXXlabelWidth",A_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYlabelWidth,"g_graphYlabelWidth",A_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYlabelWidth,"g_graphYYlabelWidth",A_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXlabelHeight,"g_graphXlabelHeight",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_graphXXlabelHeight,"g_graphXXlabelHeight",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_graphYlabelHeight,"g_graphYlabelHeight",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_graphYYlabelHeight,"g_graphYYlabelHeight",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_graphSelected,"g_graphSelected",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphMode,"q_graphMode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphStyle,"q_graphStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphAxis,"q_graphAxis",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphYmode,"q_graphYmode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphRule,"q_graphRule",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphGrid,"q_graphGrid",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphGridStyle,"q_graphGridStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphZero,"q_graphZero",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphZeroStyle,"q_graphZeroStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphTickStyle,"q_graphTickStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphLegendPosition,"q_graphLegendPosition",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphLegendStyle,"q_graphLegendStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)q_graphYtitleStyle,"q_graphYtitleStyle",A_,1,IV,0,0,0,0,0,0,0);

  install((PFI)g_graphXextents,"g_graphXextents",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_graphXXextents,"g_graphXXextents",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_graphYextents,"g_graphYextents",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_graphYYextents,"g_graphYYextents",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)bGraphDebug,"bGraphDebug",V_,1,IV,0,0,0,0,0,0,0);
  install((PFI)g_graphTraceCount,"g_graphTraceCount",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXYcoordinate,"s_graphXYcoordinate",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXYcoordinate,"g_graphXYcoordinate",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXYcoordinate,"s_graphCoordinate",V_,2,IV,A_,0,0,0,0,0,0); // temp
  install((PFI)g_graphXYcoordinate,"g_graphCoordinate",A_,1,IV,0,0,0,0,0,0,0);// temp
  install((PFI)s_graphXXYYcoordinate,"s_graphXXYYcoordinate",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXYYcoordinate,"g_graphXXYYcoordinate",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphSessionPeriod,"s_graphSessionPeriod",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphSessionPeriod,"g_graphSessionPeriod",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphSessionOffset,"s_graphSessionOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphSessionOffset,"g_graphSessionOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphTpoPriceInc,"s_graphTpoPriceInc",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphTpoPriceInc,"g_graphTpoPriceInc",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphTpoPeriod,"s_graphTpoPeriod",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphTpoPeriod,"g_graphTpoPeriod",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphTpoOpen,"s_graphTpoOpen",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphTpoOpen,"g_graphTpoOpen",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphTpoClose,"s_graphTpoClose",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphTpoClose,"g_graphTpoClose",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXminimum,"s_graphXminimum",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXminimum,"g_graphXminimum",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXminimum,"s_graphXXminimum",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXminimum,"g_graphXXminimum",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXmaximum,"s_graphXmaximum",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXmaximum,"g_graphXmaximum",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXmaximum,"s_graphXXmaximum",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXmaximum,"g_graphXXmaximum",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYminimum,"s_graphYminimum",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYminimum,"g_graphYminimum",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYminimum,"s_graphYYminimum",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYminimum,"g_graphYYminimum",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYmaximum,"s_graphYmaximum",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYmaximum,"g_graphYmaximum",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYmaximum,"s_graphYYmaximum",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYmaximum,"g_graphYYmaximum",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXincrement,"s_graphXincrement",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXincrement,"g_graphXincrement",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXincrement,"s_graphXXincrement",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXincrement,"g_graphXXincrement",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYincrement,"s_graphYincrement",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYincrement,"g_graphYincrement",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYincrement,"s_graphYYincrement",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYincrement,"g_graphYYincrement",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphRuleWidth,"s_graphRuleWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphRuleWidth,"g_graphRuleWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphMode,"s_graphMode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphMode,"g_graphMode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphUIMode,"s_graphUIMode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphUIMode,"g_graphUIMode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphStyle,"s_graphStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphStyle,"g_graphStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphAxis,"s_graphAxis",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphAxis,"g_graphAxis",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYmode,"s_graphYmode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYmode,"g_graphYmode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYmode,"s_graphYYmode",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYmode,"g_graphYYmode",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphRule,"s_graphRule",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphRule,"g_graphRule",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphGrid,"s_graphGrid",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphGrid,"g_graphGrid",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphGridWidth,"s_graphGridWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphGridWidth,"g_graphGridWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphGridStyle,"s_graphGridStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphGridStyle,"g_graphGridStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphZero,"s_graphZero",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphZero,"g_graphZero",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphZeroWidth,"s_graphZeroWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphZeroWidth,"g_graphZeroWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphZeroStyle,"s_graphZeroStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphZeroStyle,"g_graphZeroStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphScreenTopMargin,"s_graphScreenTopMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphScreenTopMargin,"g_graphScreenTopMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphScreenBottomMargin,"s_graphScreenBottomMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphScreenBottomMargin,"g_graphScreenBottomMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphScreenLeftMargin,"s_graphScreenLeftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphScreenLeftMargin,"g_graphScreenLeftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphScreenRightMargin,"s_graphScreenRightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphScreenRightMargin,"g_graphScreenRightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXleftMargin,"s_graphXleftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXleftMargin,"g_graphXleftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXrightMargin,"s_graphXrightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXrightMargin,"g_graphXrightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXleftMargin,"s_graphXXleftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXleftMargin,"g_graphXXleftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXrightMargin,"s_graphXXrightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXrightMargin,"g_graphXXrightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYtopMargin,"s_graphYtopMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYtopMargin,"g_graphYtopMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYbottomMargin,"s_graphYbottomMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYbottomMargin,"g_graphYbottomMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYtopMargin,"s_graphYYtopMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYtopMargin,"g_graphYYtopMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYbottomMargin,"s_graphYYbottomMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYbottomMargin,"g_graphYYbottomMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphLegendHlThickness,"s_graphLegendHlThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphLegendHlThickness,"g_graphLegendHlThickness",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphLegendShadowThickness,"s_graphLegendShadowThickness",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphLegendShadowThickness,"g_graphLegendShadowThickness",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphLegendBg,"s_graphLegendBg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphLegendBg,"g_graphLegendBg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphLegendFg,"s_graphLegendFg",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphLegendFg,"g_graphLegendFg",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphLegendPosition,"s_graphLegendPosition",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphLegendPosition,"g_graphLegendPosition",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphLegendStyle,"s_graphLegendStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphLegendStyle,"g_graphLegendStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphLegendFont,"s_graphLegendFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphLegendFont,"g_graphLegendFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYtitleStyle,"s_graphYtitleStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYtitleStyle,"g_graphYtitleStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYtitleStyle,"s_graphYYtitleStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYtitleStyle,"g_graphYYtitleStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXlegend,"s_graphXlegend",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXlegend,"g_graphXlegend",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYlegend,"s_graphYlegend",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYlegend,"g_graphYlegend",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphTitleAlign,"s_graphTitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphTitleAlign,"g_graphTitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphSubtitle,"s_graphSubtitle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphSubtitle,"g_graphSubtitle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphSubtitleColor,"s_graphSubtitleColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphSubtitleColor,"g_graphSubtitleColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphSubtitleFont,"s_graphSubtitleFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphSubtitleFont,"g_graphSubtitleFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphSubtitleAlign,"s_graphSubtitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphSubtitleAlign,"g_graphSubtitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphFootnote,"s_graphFootnote",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphFootnote,"g_graphFootnote",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphFootnoteColor,"s_graphFootnoteColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphFootnoteColor,"g_graphFootnoteColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphFootnoteFont,"s_graphFootnoteFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphFootnoteFont,"g_graphFootnoteFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphFootnoteAlign,"s_graphFootnoteAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphFootnoteAlign,"g_graphFootnoteAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXsubLabelAlign,"s_graphXsubLabelAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXsubLabelAlign,"g_graphXsubLabelAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXsubLabelAlign,"s_graphXXsubLabelAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXsubLabelAlign,"g_graphXXsubLabelAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXlabelAlign,"s_graphXlabelAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXlabelAlign,"g_graphXlabelAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXlabelAlign,"s_graphXXlabelAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXlabelAlign,"g_graphXXlabelAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYlabelAlign,"s_graphYlabelAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYlabelAlign,"g_graphYlabelAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYlabelAlign,"s_graphYYlabelAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYlabelAlign,"g_graphYYlabelAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXtitle,"s_graphXtitle",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)g_graphXtitle,"g_graphXtitle",CP,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXtitle,"s_graphXXtitle",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)g_graphXXtitle,"g_graphXXtitle",CP,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYtitle,"s_graphYtitle",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)g_graphYtitle,"g_graphYtitle",CP,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYtitle,"s_graphYYtitle",V_,2,IV,CP,0,0,0,0,0,0);
  install((PFI)g_graphYYtitle,"g_graphYYtitle",CP,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXtitleColor,"s_graphXtitleColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXtitleColor,"g_graphXtitleColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXtitleColor,"s_graphXXtitleColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXXtitleColor,"g_graphXXtitleColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYtitleColor,"s_graphYtitleColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYtitleColor,"g_graphYtitleColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYtitleColor,"s_graphYYtitleColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYYtitleColor,"g_graphYYtitleColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXtitleFont,"s_graphXtitleFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXtitleFont,"g_graphXtitleFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXtitleFont,"s_graphXXtitleFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXXtitleFont,"g_graphXXtitleFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYtitleFont,"s_graphYtitleFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYtitleFont,"g_graphYtitleFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYtitleFont,"s_graphYYtitleFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYYtitleFont,"g_graphYYtitleFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXtitleAlign,"s_graphXtitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXtitleAlign,"g_graphXtitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXtitleAlign,"s_graphXXtitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXtitleAlign,"g_graphXXtitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYtitleAlign,"s_graphYtitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYtitleAlign,"g_graphYtitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYtitleAlign,"s_graphYYtitleAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYtitleAlign,"g_graphYYtitleAlign",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXcolor,"s_graphXcolor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXcolor,"g_graphXcolor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXcolor,"s_graphXXcolor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXXcolor,"g_graphXXcolor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYcolor,"s_graphYcolor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYcolor,"g_graphYcolor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYcolor,"s_graphYYcolor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYYcolor,"g_graphYYcolor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXlabelFont,"s_graphXlabelFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXlabelFont,"g_graphXlabelFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXlabelFont,"s_graphXXlabelFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXXlabelFont,"g_graphXXlabelFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYlabelFont,"s_graphYlabelFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYlabelFont,"g_graphYlabelFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYlabelFont,"s_graphYYlabelFont",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYYlabelFont,"g_graphYYlabelFont",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphGridColor,"s_graphGridColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphGridColor,"g_graphGridColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphZeroColor,"s_graphZeroColor",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphZeroColor,"g_graphZeroColor",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXtickStyle,"s_graphXtickStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXtickStyle,"g_graphXtickStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXtickStyle,"s_graphXXtickStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphXXtickStyle,"g_graphXXtickStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYtickStyle,"s_graphYtickStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYtickStyle,"g_graphYtickStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYtickStyle,"s_graphYYtickStyle",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphYYtickStyle,"g_graphYYtickStyle",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXminorTicks,"s_graphXminorTicks",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXminorTicks,"g_graphXminorTicks",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXminorTicks,"s_graphXXminorTicks",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXXminorTicks,"g_graphXXminorTicks",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYminorTicks,"s_graphYminorTicks",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYminorTicks,"g_graphYminorTicks",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYminorTicks,"s_graphYYminorTicks",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYYminorTicks,"g_graphYYminorTicks",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXminorTickSize,"s_graphXminorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXminorTickSize,"g_graphXminorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXminorTickSize,"s_graphXXminorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXXminorTickSize,"g_graphXXminorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYminorTickSize,"s_graphYminorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYminorTickSize,"g_graphYminorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYminorTickSize,"s_graphYYminorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYYminorTickSize,"g_graphYYminorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXmajorTickSize,"s_graphXmajorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXmajorTickSize,"g_graphXmajorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphXXmajorTickSize,"s_graphXXmajorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphXXmajorTickSize,"g_graphXXmajorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYmajorTickSize,"s_graphYmajorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYmajorTickSize,"g_graphYmajorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphYYmajorTickSize,"s_graphYYmajorTickSize",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphYYmajorTickSize,"g_graphYYmajorTickSize",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphBarWidth,"s_graphBarWidth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphBarWidth,"g_graphBarWidth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphDepth,"s_graphDepth",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphDepth,"g_graphDepth",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_longPressTime,"s_longPressTime",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_longPressTime,"g_longPressTime",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphSelectDistance,"s_graphSelectDistance",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphSelectDistance,"g_graphSelectDistance",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphPieOffsetMargin,"s_graphPieOffsetMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphPieOffsetMargin,"g_graphPieOffsetMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphLeftMargin,"s_graphLeftMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphLeftMargin,"g_graphLeftMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphRightMargin,"s_graphRightMargin",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphRightMargin,"g_graphRightMargin",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphTopOffset,"s_graphTopOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphTopOffset,"g_graphTopOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphBottomOffset,"s_graphBottomOffset",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphBottomOffset,"g_graphBottomOffset",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphPrintRow,"s_graphPrintRow",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphPrintRow,"g_graphPrintRow",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphPrintColumn,"s_graphPrintColumn",V_,2,IV,IV,0,0,0,0,0,0);
  install((PFI)g_graphPrintColumn,"g_graphPrintColumn",IV,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphJustify,"s_graphJustify",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphJustify,"g_graphJustify",A_,1,IV,0,0,0,0,0,0,0);
  install((PFI)s_graphPageAlign,"s_graphPageAlign",V_,2,IV,A_,0,0,0,0,0,0);
  install((PFI)g_graphPageAlign,"g_graphPageAlign",A_,1,IV,0,0,0,0,0,0,0);


  Cx=context;
}
