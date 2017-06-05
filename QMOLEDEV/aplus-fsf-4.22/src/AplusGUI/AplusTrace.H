#ifndef AplusTraceHEADER
#define AplusTraceHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSFloatMatrix.H>
#include <MSGUI/MSTrace.H>
#include <MSGUI/MSTraceSet.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusEvent.H>

class AObject
{
protected:
  A _a;

public:
  AObject(A a_ = 0) { _a = 0; if (a_ != 0 && !QS(a_)) _a = (A)ic(a_); }
  ~AObject( void ) { if (_a != 0) dc(_a); }

  void a( A a_ ) { if (_a != 0 ) dc(_a); 
                   _a = 0; 
                   if (a_ != 0 && !QS(a_)) _a = (A)ic(a_); 
                 }

  int rank( void )  { return (_a!=0) ? (int)_a->r : 0; }
  int type( void )  { return (_a!=0) ? (int)_a->t : 0; }
  int n( void )     { return (_a!=0) ? (int)_a->n : 0; }
  I *data( void )   { return (_a!=0) ? (I *)_a->p : 0; }
  int width( void ) { return (isMatrix()) ? numCols() : 1; }
  int offset( int row_, int col_ = 0 ) { return row_*width() + col_; }

  MSBoolean isNl( void )       { return (_a==aplus_nl) ? MSTrue : MSFalse; }
  MSBoolean isNull( void )     { return (isEnclosed()&&n()==0) ? MSTrue : MSFalse; }
  MSBoolean isEnclosed( void ) { return (type()==Et) ? MSTrue : MSFalse; }
  MSBoolean isVector( void )   { return (rank()==1) ? MSTrue : MSFalse; }
  MSBoolean isMatrix( void )   { return (rank()==2) ? MSTrue : MSFalse; }
  MSBoolean isScalar( void )   
   { return ((rank()==0&&type()<Ct) || (type()==Ct&&rank()==1)) ? MSTrue : MSFalse; }

  int numRows( void ) 
   { if ( _a == 0 ) return 0;
     else if (isEnclosed()) return n();
     else if (type() == Ct && rank() == 1) return 1; // scalar char vector
     else return (rank()==0)?1:(int)_a->d[0];
   }
  int numCols( void ) 
   { if ( isNull() == MSTrue || _a == 0 ) return 0;
     else if (type() == Ct || rank()== 0) return 1;
     else if (isEnclosed()) return 1;
     else return (rank()==1)?rank():(int)_a->d[1];
   }
};



class ATraceFunction:public AFunction
{
public:
  ATraceFunction(void) {_func=0; _arg=0;}
  ATraceFunction(AFunc func_,AClientData *arg_=0):AFunction(func_,arg_) {}
  ~ATraceFunction(void) {}

  A callFunc(V v_,A value_,int row_,int col_,A pick_=aplus_nl) 
   {A r=aplus_nl;
     A index=0;
     if (row_==-1&&col_==-1) index=0;
     else index=grc((A)v_->a,row_,col_);  // generate r,c index
     if (_func!=0) r=(A)(*_func)(arg(),value_,index,pick_,v_);
     if (index!=0) dc((A)index);
     if (value_!=0) dc((A)value_);
     return r;}

  A invoke(V v_,A value_,int row_,int col_,A pick_=aplus_nl)  
   {A val=0;
     if (value_!=0&&QS(value_)) {val=(A)gs(Et); *val->p=(I)value_;}
     else if (value_!=0) {val=(A)ic(value_);}
     return callFunc(v_,val,row_,col_,pick_);} 

  long enumInvoke(V v_,A value_,int row_,int col_,A pick_=aplus_nl)
   {
     long r=0;
     A ar=invoke(v_,value_,row_,col_,pick_);
     AObject ao(ar);
     if (ao.type()==It &&ao.isScalar())
      {	P p; p.i=ao.data();
	r=p.i[0];}
     dc((A)r);
     return r;}

  long intInvoke(V v_,A value_,int row_,int col_,A pick_=aplus_nl)
  {
    long r=1;
    A ar=invoke(v_,value_,row_,col_,pick_);

    if (isNull(ar)==MSTrue)
      {
	return -1;
      }

    if (!QS(ar)) 
     {
       P p; p.i=ar->p; 
       if (ar->t==Ft) r=(int)p.f[0]; 
       else r=(long)p.i[0]; 
    }
    if (!QS(ar)&&ar!=0) dc((A)ar);
    return r;
 }
}; 


class AplusGraph;
class AplusTrace;
class AplusTraceSet : public MSTraceSet
{
friend class AplusGraph;

  static MSHashTable 	_enumHashTable;
  static MSHashTable	_constraintEnumHashTable;
  static MSHashTable	_styleEnumHashTable;
  static MSHashTable	_symbolEnumHashTable;

 public:
  
  static MSBoolean    	_initialized;   // for enumTables

/*
  typedef enum {Xsym=20,Cross=21,Star=22,Square=23,SquareCross=24,SquareCircle=25,Circle=26,
		CircleCross=27,CircleDiamond=28,CircleTriangle=29,Diamond=30,DiamondCross=31,
		Triangle=32,Char=33,SquareFilled=34,CircleFilled=35,DiamondFilled=36,
		TriangleFilled=37} TraceSymbol;

  Xsym           = MSG::X                      = 2
  Cross          = MSG::Cross                  = 1
  Star           = MSG::Cross | MSG::X         = 3
  Square         = MSG::Square                 = 4
  SquareCross    = MSG::Square | MSG::Cross    = 7
  SquareCircle   = MSG::Square | MSG::Circle   = 12
  Circle         = MSG::Circle                 = 8
  CircleCross    = MSG::Cross | MSG::Circle    = 11
  CircleDiamond  = MSG::Diamond | MSG::Circle  = 24
  CircleTriangle = MSG::Triangle | MSG::Circle = 40
  Diamond        = MSG::Diamond                = 16 
  DiamondCross   = MSG::Diamond | MSG::Cross   = 17
  Triangle       = MSG::Triangle               = 32

  Char           = MSG::Text                   = 512
  SquareFilled   = MSG::Square | MSG::Fill     = 132 
  CircleFilled   = MSG::Circle | MSG::Fill     = 136
  DiamondFilled  = MSG::Diamond | MSG::Fill    = 144
  TriangleFilled = MSG::Triangle | MSG::Fill   = 160
  
  typedef enum {NoConstraint=1,HoldX=41,HoldY=42} Constraint;

  NoConstraint = MSNone      = 0
  HoldX        = MSG::HoldX  = 1
  HoldY        = MSG::HoldY  = 2
  
  typedef enum {Line=50,Outline=51,Scatter=52,LineScatter=53,Area=54,Bar=55,Close=56,
		MarketProfile=57,ColorProfile=58,HL=59,HLOC=60,HLC=61,Candle=62,Stack=63,
		Step=64,StepScatter=65,Osc=66,Fill=67,Segment=68,Text=69,Null=1} TraceStyle;

  Line         	= MSG::Line                = 1
  Outline      	= MSG::Outline             = 2
  Scatter      	= MSG::Scatter             = 64
  LineScatter  	= MSG::Line | MSG::Scatter = 65
  Area         	= MSG::Area                = 4
  Bar          	= MSG::Bar                 = 8
  Close        	= MSG::Close               = 2048
  MarketProfile	= MSG::MarketProfile       = 65536
  ColorProfile 	= MSG::ColorProfile        = 131072
  HL  	       	= MSG::HL                  = 8192
  HLOC	       	= MSG::HLOC                = 16384
  HLC 	       	= MSG::HLC                 = 32768
  Candle       	= MSG::Candle              = 4096
  Stack        	= MSG::Stack               = 16
  Step         	= MSG::Step                = 32
  StepScatter  	= MSG::Step | MSG::Scatter = 96
  Osc          	= MSG::Osc                 = 1024
  Fill         	= MSG::Fill                = 128
  Segment      	= MSG::Segment             = 256
  Text         	= MSG::Text                = 512
  Null         	= MSNone                   = 0

  typedef enum {_TraceStyle=202,_LineStyle=203,_Symbol=204,_Constraint=205} TraceEnum;

  _TraceStyle =202
  _LineStyle  =203
  _Symbol     =204
  _Constraint =205

  */

  typedef enum {_TraceStyle=202,_LineStyle=203,_Symbol=204,_Constraint=205} TraceEnum;

  AColorFunction       _lineColorFunc;
  AColorFunction       _fillColorFunc;
  AOutFunction         _legendFunc;
  ATraceFunction       _traceSymbolFunc;
  ATraceFunction       _traceSymbolSizeFunc;
  ATraceFunction       _traceStyleFunc;
  ATraceFunction       _lineStyleFunc;
  ATraceFunction       _lineWidthFunc;
  ATraceFunction       _pieOffsetFunc;
  ATraceFunction       _gradientFunc;

  ATraceFunction       _altXaxisFunc;
  ATraceFunction       _altYaxisFunc;
  
public:
  AplusTraceSet(MSGraph *);
  ~AplusTraceSet(void);
  AplusTrace *trace(int i_)                 const { return (AplusTrace *) MSTraceSet::trace(i_); }
  
  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

  void initEnumHashTable(void);
  void initStringEnumHashTable(void);

  AplusGraph *graph(void)                      { return (AplusGraph *) owner(); }

  MSHashTable *enumHashTable(void)             { return &_enumHashTable; }
  MSHashTable *constraintEnumHashTable(void)   { return &_constraintEnumHashTable; }
  MSHashTable *styleEnumHashTable(void)        { return &_styleEnumHashTable; }
  MSHashTable *symbolEnumHashTable(void)       { return &_symbolEnumHashTable; }

  A enumSymbols(const char *);

  // Functional attributes

  ATraceFunction *lineStyleFunc(void)       	  { return &_lineStyleFunc;}
  ATraceFunction *lineWidthFunc(void)       	  { return &_lineWidthFunc;}
  ATraceFunction *pieOffsetFunc(void)       	  { return &_pieOffsetFunc;}
  ATraceFunction *gradientFunc(void)        	  { return &_gradientFunc;}
  const AColorFunction *lineColorFunc(void) const { return &_lineColorFunc;}
  const AColorFunction *fillColorFunc(void) const { return &_fillColorFunc;}
  AOutFunction *legendFunc(void)            	  { return &_legendFunc;}
  ATraceFunction *traceStyleFunc(void)      	  { return &_traceStyleFunc;}
  ATraceFunction *traceSymbolFunc(void)     	  { return &_traceSymbolFunc;}
  ATraceFunction *traceSymbolSizeFunc(void) 	  { return &_traceSymbolSizeFunc;}
  ATraceFunction *altYaxisFunc(void)        	  { return &_altYaxisFunc;}
  ATraceFunction *altXaxisFunc(void)        	  { return &_altXaxisFunc;}

  void lineStyleFunc(AFunc,A);
  void lineWidthFunc(AFunc,A);
  void pieOffsetFunc(AFunc,A);
  void gradientFunc(AFunc,A);
  void lineColorFunc(AFunc,A);
  void fillColorFunc(AFunc,A);
  void legendFunc(AFunc,A);
  void traceStyleFunc(AFunc,A);
  void traceSymbolFunc(AFunc,A);
  void traceSymbolSizeFunc(AFunc,A);
  void altYaxisFunc(AFunc,A);
  void altXaxisFunc(AFunc func_,A fc_);

  void lineStyleFuncInvoke(void);
  void lineWidthFuncInvoke(void);
  void pieOffsetFuncInvoke(void);
  void gradientFuncInvoke(void);
  void lineColorFuncInvoke(void);
  void fillColorFuncInvoke(void);
  void legendFuncInvoke(void);
  void traceStyleFuncInvoke(void);
  void traceSymbolFuncInvoke(void);
  void traceSymbolSizeFuncInvoke(void);
  void altYaxisFuncInvoke(void);
  void altXaxisFuncInvoke(void);

  A coordinate(void);
  void constraint(A);

  A constraintSym(void);
  virtual void constraint(unsigned long x_) { MSTraceSet::constraint(x_); }
  virtual unsigned long constraint(void) const { return MSTraceSet::constraint(); }

  void selected(A);
  A selected(void);
  void selected(int r_, int c_) { MSTraceSet::selected(r_,c_); }

  virtual void selectable(MSBoolean);
  virtual MSBoolean selectable(void) const { return MSTraceSet::selectable(); }

  void textBuffer(const MSStringVector t_);
  MSStringVector &textBuffer(void)         { return _textBuffer; }

  void gradient(const MSUnsignedVector);
  
  // TraceSet methods

  virtual int numColumns(void) const;
  virtual int numRows(void) const;
  virtual int dataCount(void) const;
  virtual double x(int) const;
  virtual double y(int,int) const;
  virtual void validate(int,int,double,double);

  // Standard Widget Methods

  virtual void receiveEvent(MSEvent &);
  virtual void addSenderNotify(MSEventSender *);

  virtual void symbol(const MSUnsignedLongVector& s_) { MSTraceSet::symbol(s_); }
  virtual void symbol(unsigned long s_)               { MSTraceSet::symbol(s_); }
  virtual void symbol(unsigned long x_,unsigned s_)   { MSTraceSet::symbol(x_,s_); }
  virtual void symbol(Symbol s_)                      { MSTraceSet::symbol(s_); }
  virtual unsigned long symbol(unsigned s_) const     { return MSTraceSet::symbol(s_); }

  // 'A' interface routines

  virtual void update(V,int,int,UpdateType);
  virtual void update(V,A,A,I);
  virtual void update(const MSIndexVector &v_) { MSTraceSet::update(v_); }
  virtual void updateData(void);
  virtual MSBoolean verifyData(V,A);
  virtual void updateTitle(void);

  virtual unsigned long traceLineColor(int,int) const;
  virtual unsigned long traceFillColor(int,int) const;

  virtual const char *formatText(MSString&, unsigned);
  virtual unsigned textLength(void) const;
  virtual void validateText(const MSStringVector);

protected:
  virtual const char *formatOutput(MSString&,unsigned,unsigned);
  virtual void invokeFunction(AOutFunction *, unsigned, unsigned);
  virtual MSBoolean moveTraceValidate(double,double);
  virtual MSFloatMatrix asFloatMatrix(void) const;

  virtual MSBoolean isProtected(void) const;

  ::A computeAdjustedData(double,double) const;

private:
  unsigned long convertTraceStyle(A);
  MSBoolean convertXAxis(A, unsigned long &);
  MSBoolean convertYAxis(A, unsigned long &);
  void convertTraceSymbol(A, unsigned long &, MSString &);

  A	   	 _outStr;
  MSStringVector _textBuffer;

  STANDARD_WIDGET_METHODS_H
};

// This class is created so AplusTraceSet and AplusGraph can access the
// protected members of MSTrace.

class AplusTrace : public MSTrace
{
friend class AplusTraceSet;
friend class AplusGraph;
public:
  AplusTrace(MSTraceSet *,int col_=0,const MSSymbol& tag_=MSSymbol::nullSymbol());
  ~AplusTrace(void);
  V aplusVar(void);
  void gradient(MSBoolean g_) { _gradient = g_; }
  MSBoolean gradient(void)    { return _gradient; }

protected:
  int virtualCol(void)             const { return MSTrace::virtualCol(); }
  unsigned long style(void)        const { return MSTrace::style(); }
  unsigned long lineColor(void)    const { return MSTrace::lineColor(); }
  unsigned long fillColor(void)    const { return MSTrace::fillColor(); }
  void virtualCol(int x_)                { MSTrace::virtualCol(x_); }
  void lineColor(unsigned long c_)       { MSTrace::lineColor(c_); }
  void fillColor(unsigned long c_)       { MSTrace::fillColor(c_); }

private:
  MSBoolean _gradient;
};


#endif  //AplusTraceHEADER
