#ifndef AplusGraphHEADER
#define AplusGraphHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#if HAVE_IOSTREAM
#include <iostream>
#else
#include <iostream.h>
#endif
#include <MSGUI/MSGraph.H>
#include <AplusGUI/Macros.H>
#include <AplusGUI/AplusCommon.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusEvent.H>
#include <AplusGUI/AplusFormatter.H>
#include <AplusGUI/AplusLabelOut.H>
#include <AplusGUI/AplusEnumConverter.H>

class EnumError
{
protected:
  MSString       _lamp;
  MSString       _attrmsg;
  MSString       _symmsg;
  
public:
  EnumError(void);
  ~EnumError(void){}
  
  void        attrmsg(const char *msg_) { _attrmsg = msg_;}
  const char *attrmsg(void)             { return _attrmsg.string(); }
  void        symmsg (const char *msg_) { _symmsg = msg_;}
  const char *symmsg (void) 		{ return _symmsg.string(); }
  const char *lamp   (void)   		{ return _lamp.string(); }
  
  void showError(V v_,const char* str_)
   {
     cerr<<lamp()<<" ! ";
     if (v_!=0) cerr<<(char*)v_->cx->s->n<<"."<<(char*)v_->s->n;
     if (str_!=0) cerr<<attrmsg()<<str_<<endl;
     else cerr<<symmsg()<<endl;
   }
};



class AplusTraceSet;
class AplusGraph : public MSGraph
{
friend class AplusTraceSet;

private:

  static EnumError 	_enumError;
  static MSHashTable 	_enumHashTable;
  static MSHashTable	_stringEnumHashTable;
  static MSHashTable	_gridEnumHashTable;
  static MSHashTable	_legendEnumHashTable;
  static MSBoolean   	_initialized;   // for enumTable
  static AplusFormatter _outFormat;

/********************************************************************************

  typedef enum {_GraphMode=204,_GraphStyle=205,_Axis=206,_YAxis=207,_Rule=208,
		_Grid=209,_LineStyle=210,_TickStyle=211,_LegendPosition=212,
		_LegendStyle=214,_DisplayStyle=215} GraphEnum;

  // For Query Function
  _GraphMode 	  = 204
  _GraphStyle	  = 205
  _Axis      	  = 206
  _YAxis     	  = 207
  _Rule      	  = 208
  _Grid      	  = 209
  _LineStyle 	  = 210
  _TickStyle 	  = 211
  _LegendPosition = 212
  _LegendStyle 	  = 214
  _DisplayStyle	  = 215


  typedef enum {Vert=2,Horz=3} DisplayStyle;  // Can be ignored

  Vert = MSG::Vertical   = 64
  Horz = MSG::Horizontal = 128

  typedef enum {VertLegend=2,HorzLegend=3,LastValue=150} LegendStyle;

  VertLegend = MSG::Vertical   = 64
  HorzLegend = MSG::Horizontal = 128
  LastValue  = MSG::LastValue  = 256
  
  typedef enum {Normal=10,AddTrace=11,AddTextTrace=12,RealizeTrace=13,MoveTrace=14,
		CopyTrace=15,CopyTextTrace=16,DeleteTrace=17,RotateTrace=18,
		BendTrace=19,InsertDataPoint=20,DeleteDataPoint=21} GraphMode;

  Normal       	  = MSGraph::Normal       = 0
  AddTrace     	  = MSGraph::AddTrace     = 1
  AddTextTrace 	  = MSGraph::AddTextTrace = 2
  RealizeTrace 	  = Not Implemented       = ?
  MoveTrace    	  = MSGraph::MoveTrace    = 4
  CopyTrace    	  = Not Implemented       = ?
  CopyTextTrace	  = Not Implemented       = ?
  DeleteTrace  	  = Not Implemented       = ?
  RotateTrace  	  = Not Implemented       = ?
  BendTrace    	  = Not Implemented       = ?
  InsertDataPoint = Not Implemented       = ?
  DeleteDataPoint = Not Implemented       = ?

		
  typedef enum {NoGrid=1, xRuled=47,XRuled=48,yRuled=49,YRuled=50,XYRuled=51,
		xYRuled=52,XyRuled=53,xyRuled=116} Grid;

  NoGrid  = MSNone              = 1
  xRuled  = MSBottom            = 32
  XRuled  = MSTop               = 16
  yRuled  = MSLeft              = 4
  YRuled  = MSRight             = 8
  XYRuled = MSTop | MSRight     = 16 | 8 = 24
  xYRuled = MSBottom | MSRight  = 32 | 8 = 40
  XyRuled = MSTop | MSLeft      = 16 | 4 = 20
  xyRuled = MSBottom | MSLeft   = 32 | 4 = 36
  
  typedef enum {Outside=60,Inside=61,InOut=62,NoTick=1} TickStyle;

  Outside = MSG::Outside                = 512
  Inside  = MSG::Inside                 = 1024
  InOut   = MSG::Inside | MSG::Outside  = 1536
  NoTick  = MSNone                      = 1

  typedef enum {NoFormat=1, Func=79,YFunc=80,YYFunc=81,XFunc=82,XXFunc=83,
		XSubFunc=84,XXSubFunc=85} FuncFormat;

  NoFormat = Not Implemented
  Func     = Not Implemented
  YFunc    = Not Implemented
  YYFunc   = Not Implemented
  XFunc    = Not Implemented
  XXFunc   = Not Implemented
  XSubFunc = Not Implemented
  XXSubFunc= Not Implemented

  typedef enum {TopLeft=110,TopCenter=111,TopRight=112,BottomLeft=113,
		BottomCenter=114,BottomRight=115,XY=116,NoLegend=1} LegendPosition;

  TopLeft      = MSTop | MSLeft       = 20
  TopCenter    = MSTop | MSCenter     = 16 | 2 = 18 
  TopRight     = MSTop | MSRight      = 16 | 8 = 24
  BottomLeft   = MSBottom | MSLeft    = 32 | 4 = 36
  BottomCenter = MSBottom | MSCenter  = 32 | 2 = 34
  BottomRight  = MSBottom | MSRight   = 32 | 8 = 40
  XY           = Not Implemented      = 
  NoLegend     = MSNone               = 0

******************************************************************************/

  AOutFunction          _x_subLabelFunc[2];
  AOutFunction          _x_labelFunc[2];
  AOutFunction          _y_labelFunc[2];

  AOutFunction          _x_subLabelFormatFunc[2];
  AOutFunction          _x_labelFormatFunc[2];
  AOutFunction          _y_labelFormatFunc[2];

  int                   _y_labelPrecision[2];
  int                   _x_labelPrecision[2];
  int                   _x_subLabelPrecision[2];

  int                   _y_labelFormat[2];
  int                   _x_labelFormat[2];
  int                   _x_subLabelFormat[2];

  MSString              _axisAMode;
  MSString              _axisARule;
  MSString              _gridAStyle;
  MSString              _zeroAStyle;

public:
  class LegendAlignConverter : public AplusHashEnumConverter
  {
  protected:
    virtual const AplusHashTable& enumTable(void) const;
    virtual const AplusHashTable& stringTable(void) const;
    virtual const char *type(void) const;
  public:
    inline LegendAlignConverter(void);
    inline ~LegendAlignConverter(void);
  };

  typedef enum {_GraphMode=204,_GraphStyle=205,_Axis=206,_YAxis=207,_Rule=208,
		_Grid=209,_LineStyle=210,_TickStyle=211,_LegendPosition=212, _LegendPlacement=213,
		_LegendStyle=214,_DisplayStyle=215} GraphEnum;

  typedef enum {Solid=600,Dash0=601,Dash1=602,Dash2=603,Dash3=604,Dash4=605,
		DotDash0=606,DotDash1=607,DotDash2=608,DotDash3=609,DotDash4=610,
		Dot0=611,Dot1=612,Dot2=613,Dot3=614,Dot4=615,Dot5=616} LineStyle;

  typedef enum {NoFormat=1,Func=79,YFunc=80,YYFunc=81,XFunc=82,XXFunc=83,
		XSubFunc=84,XXSubFunc=85} FuncFormat;

  AplusGraph(MSWidget *);
  ~AplusGraph(void);
  
  void initEnumHashTable(void);       
  void initStringEnumHashTable(void); 
  void initGridEnumHashTable(void);   
  void initLegendEnumHashTable(void); 

  void longPressTime(long val_) { MSGraph::longPressTime(val_); }
  long longPressTime(void) { return MSGraph::longPressTime(); }

  static AplusFormatter *outFmt(void) { return &_outFormat; }

  MSHashTable *enumHashTable(void)         { return &_enumHashTable; }      
  MSHashTable *stringEnumHashTable(void)   { return &_stringEnumHashTable; }
  MSHashTable *gridEnumHashTable(void)	   { return &_gridEnumHashTable; }  
  MSHashTable *legendEnumHashTable(void)   { return &_legendEnumHashTable ; }
  EnumError   *enumError(void)             { return &_enumError; }

  ::A enumSymbols(const char *);

  void x_subLabelFormatFunc(AFunc,AClientData *,int axis_=0);
  void x_labelFormatFunc(AFunc,AClientData *,int axis_=0);
  void y_labelFormatFunc(AFunc,AClientData *,int axis_=0);
  AOutFunction *y_labelFormatFunc(int axis_=0) { return &_y_labelFormatFunc[axis_]; }
  AOutFunction *x_subLabelFormatFunc(int axis_=0) { return &_x_subLabelFormatFunc[axis_]; }
  AOutFunction *x_labelFormatFunc(int axis_=0) { return &_x_labelFormatFunc[axis_]; }

  int y_labelPrecision(int axis_ = 0) { return _y_labelPrecision[axis_]; }
  int x_labelPrecision(int axis_ = 0) { return _x_labelPrecision[axis_]; }
  int x_subLabelPrecision(int axis_ = 0) { return _x_subLabelPrecision[axis_]; }

  // LabelFuncs are not fully implemented
  
  void x_subLabelFunc(AFunc func_,AClientData *,int axis_=0);
  void x_labelFunc(AFunc func_,AClientData *,int axis_=0);
  void y_labelFunc(AFunc func_,AClientData *,int axis_=0);

  AOutFunction *x_subLabelFunc(int axis_=0) { return &_x_subLabelFunc[axis_]; }
  AOutFunction *x_labelFunc(int axis_=0) { return &_x_labelFunc[axis_]; }
  AOutFunction *y_labelFunc(int axis_=0) { return &_y_labelFunc[axis_]; }

  int traceCount(void)   { return traceList().count();}

  int  y_labelFormat(int axis_)             { return _y_labelFormat[axis_];    }
  void y_labelFormat(int fmt_,int axis_)    { _y_labelFormat[axis_]=fmt_;      }
  int  x_labelFormat(int axis_=0)           { return _x_labelFormat[axis_];    }
  void x_labelFormat(int fmt_,int axis_)    { _x_labelFormat[axis_]=fmt_;      }  
  int  x_subLabelFormat(int axis_=0)        { return _x_subLabelFormat[axis_]; }
  void x_subLabelFormat(int fmt_,int axis_) { _x_subLabelFormat[axis_]=fmt_;   }

  void y_labelFormat(::A, int, int precision_ = -1);
  void x_labelFormat(::A, int, int precision_ = -1);
  void x_subLabelFormat(::A, int, int precision_ = -1);

  ::A generateSym(char *, int);
  ::A generateSym(MSHashTable *, int);
  ::A generateGraphModeSym(unsigned long);
  ::A generateGraphUIModeSym(int);
  ::A generateAxisModeSym(AxisMode);
  ::A generateGridSym(unsigned long);
  ::A generateLegendStyleSym(unsigned long);
  ::A generateTickStyleSym(unsigned long);

  ::A generateFuncSym(void);
  ::A y_labelFormatSym(int axis_=0); 
  ::A x_labelFormatSym(int axis_=0); 
  ::A x_subLabelFormatSym(int axis_=0);
  
  ::A xExtents(int);
  ::A yExtents(int);
  ::A xLabelHeight(int);
  ::A yLabelHeight(int);
  ::A xLabelWidth(::A,int);
  ::A yLabelWidth(::A,int);
  ::A labelWidth(::A,XFontStruct *);

  void pixelToValue(int,int,double *,double *,int,int);
  ::A coordinate(int); // Call xCursorValue and yCursorValue
  // if 0 then call x(bottom) and y(left)
  // if 1 then call x(top) and y(right)

  void graphAMode(::A); // Need to look up symbol in hashtable and call graphUIMode()
  ::A graphAMode(void); //  See graphModeSym in XGraph.H
  
  void graphAUIMode(::A); // Need to look up symbol in hashtable and call graphUIMode()
  ::A graphAUIMode(void); //  See graphModeSym in XGraph.H
  
  void axisAMode(::A); // Need to look up symbol in hashtable and call axisMode()
  ::A axisAMode(void); //  See axisSym in XGraph.H

  void yAMode(::A,MSAlignment); // Need to look up symbol in hashtable and call yMode()
  ::A yAMode(MSAlignment);      //  See axisSym in XGraph.H

  void axisARule(::A); // Need to look up symbol in hashtable and call axisRule()
  ::A axisARule(void); //  See axisSym in XGraph.H

  void gridA(::A); // Need to look up symbol in hashtable and call grid()
  ::A gridA(void); //  See axisSym in XGraph.H

  void gridAStyle(::A); // Has to look up enumtable 
  ::A gridAStyle(void); // call gridStyle and gridWeight

  void zeroA(::A); // Need to look up symbol in hashtable and call zero()
  ::A zeroA(void); //  See axisSym in XGraph.H

  void zeroAStyle(::A); // Has to look up enumtable 
  ::A zeroAStyle(void); // call zeroStyle and zeroWeight

  void legendAStyle(::A);  // call legendStyle
  ::A legendAStyle(void);

  void yTitleAStyle(::A, MSAlignment);
  ::A yTitleAStyle(MSAlignment);

  void subTitleA(::A);  // Convert A to MSStringVector then call subTitle
  ::A subTitleA(void);
  
  void footnoteA(::A);  // Convert A to MSStringVector then call footnote
  ::A footnoteA(void);
 
  void tickStyleA(::A, MSAlignment);  // Covert A and call tickStyle
  ::A tickStyleA(MSAlignment);

  void sessionPeriod(double x_) { MSGraph::sessionPeriod(x_); }
  void sessionOffset(double x_) { MSGraph::sessionOffset(x_); }
  void tpoPriceInc(double x_)   { MSGraph::tpoPriceInc(x_);   }
  void tpoPeriod(double x_)     { MSGraph::tpoPeriod(x_);     }
  void tpoOpen(double x_)       { MSGraph::tpoOpen(x_);       }
  void tpoClose(double x_)      { MSGraph::tpoClose(x_);      }

  double sessionPeriod(void) { return MSGraph::sessionPeriod(); }
  double sessionOffset(void) { return MSGraph::sessionOffset(); }
  double tpoPriceInc(void)   { return MSGraph::tpoPriceInc();   }
  double tpoPeriod(void)     { return MSGraph::tpoPeriod();     }
  double tpoOpen(void)       { return MSGraph::tpoOpen();       }
  double tpoClose(void)      { return MSGraph::tpoClose();      }
  
  void graphDepth(int)       {}            //MSGraph::graphDepth(val_); }  These are NOPs according to grl
  int graphDepth(void)       { return 0; } // return graphDepth(); }
  virtual MSBoolean verifyData(V,::A);

  MSBoolean styleConvert(LineStyle, unsigned long &, unsigned long &);

  virtual void printDebugInfo(const char *);

  virtual const MSSymbol& widgetType(void) const;
  static const MSSymbol& symbol(void);

protected:
  virtual void addSenderNotify(MSEventSender *m_);
  virtual void receiveEvent(MSEvent &);
  MSBoolean verifyTraceSet(::A);
  virtual void update(V,::A,::A,I);
  virtual void update(V,int,int,UpdateType) {}
  virtual void update(const MSIndexVector &v_) { MSGraph::update(v_); }
  virtual void update(MSTraceSet *ts_,const MSIndexVector &i_) { MSGraph::update((MSTraceSet *)ts_, i_); }
  virtual void insertChild(MSWidget *);
  virtual void removeChild(MSWidget *);
  virtual void updateData(void);
  
  // Methods for Traceset to call (Forward messages upward)
  void updateLegendStatus(MSBoolean x_) { MSGraph::updateLegendStatus(x_); }
  virtual void redrawImmediately(MSBoolean update_=MSFalse, MSBoolean append_=MSFalse) { MSGraph::redrawImmediately(update_, append_); }
  void drawLegends(void) { MSGraph::drawLegends(); }
  void highlightPoint(AplusTraceSet *t_) { MSGraph::highlightPoint((MSTraceSet *)t_); }
  MSPointerArray<MSTrace>& traceList(void) {return MSGraph::traceList(); }
  
  STANDARD_WIDGET_METHODS_H
};

inline AplusGraph::LegendAlignConverter::LegendAlignConverter(void) : AplusHashEnumConverter() {}
inline AplusGraph::LegendAlignConverter::~LegendAlignConverter(void) {}

#endif  //AplusGraphHEADER
