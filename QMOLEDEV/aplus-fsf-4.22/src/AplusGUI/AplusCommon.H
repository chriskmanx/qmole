#ifndef __AplusCommon_H
#define __AplusCommon_H

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <X11/X.h>	// declaration of Font
#include <MSGUI/MSDisplayServer.H>
#include <MSGUI/MSWidgetView.H>
#include <MSTypes/MSTime.H>
#include <AplusGUI/AplusModel.H>
#include <AplusGUI/AplusEvent.H>

A grc(A,int,int);
void showError(const char *msg_, int flag_=0);
MSBoolean processAVariables(void);
void asTitleStringVector(MSStringVector &, A);

inline MSBoolean isNull (A aobject_){return qz(aobject_)==0? MSFalse : MSTrue;}

inline MSBoolean isNl (A aobject_) { return(aobject_==aplus_nl)?MSTrue : MSFalse; }

MSBoolean isAplusWidget(const MSWidget *);

class AScbTraceHook
{
 public:
  static A _function;

  AScbTraceHook(A function_) {_function=(A)ic(function_);}
  AScbTraceHook(void)        {_function=(A)(0);}
  ~AScbTraceHook(void)       {dc(_function); _function=(A)0;}
  
  static void function(A function_)
  {
    if(_function) dc(_function);

    if(aplus_nl==function_)
      { 
	_function=(A)0;
      }
    else
      {
	_function=(A)ic(function_);
      }
  };

  static A function(void)         { return _function; };

  static void run(A f,I a,I b,I c,I d,V v)
  {
    const int numArgs=6;
    
    // Function cx.name as a symbol
    CX cx=(CX)f->p[f->n+2];      
    if((cx->s->n[0]=='s') &&(cx->s->n[1]=='\0')) return; // Ignore calls to the `s context

    A fn=ge(MS(symjoin(cx->s,XS(*f->d))));  
    
    // variable cx.name as a symbol
    A vn=ge(MS(symjoin(v->cx->s, v->s)));
    
    E e=(E)ma(numArgs+2);
    e->n=numArgs;
    e->f=(I)AScbTraceHook::_function;
    e->a[0]=(I)fn;		// `cx.functionName
    e->a[1]=a?a:(I)aplus_nl;		// s
    e->a[2]=b?b:(I)aplus_nl;		// d
    e->a[3]=c?c:(I)aplus_nl;		// i
    e->a[4]=d?d:(I)aplus_nl;		// p
    e->a[5]=(I)vn;		// `cx.variableNme
    
    dc((A)ez(ME(e))); 
    mf((I *)e);
    dc((A)fn);
    dc((A)vn); 
    return;
  }

};

class AClientData
{
private:
  A _function;
  A _data;
  A _actualData;
  V _aplusVar;

public:
  AClientData(V v_,A function_,A data_,A actualData_=aplus_nl) 
    {_aplusVar=v_;_function=(A)ic(function_);_data=(A)ic(data_);_actualData=(A)ic(actualData_);}
  AClientData(A function_,A data_,A actualData_=aplus_nl) 
    {_aplusVar=0;_function=(A)ic(function_);_data=(A)ic(data_);_actualData=(A)ic(actualData_);}
  ~AClientData(void)
    {dc(_function);dc(_data);dc(_actualData);_aplusVar=0;_aplusVar=0;_actualData=aplus_nl;_data=aplus_nl;}

  V aplusVar(void)   { return _aplusVar; }
  A function(void)   { return _function; }
  A data(void)       { return _data; }
  A actualData(void) { return _actualData; }
};

typedef A (*AFunc)(AClientData *,A,A,A,V);

class AFunction
{
public:
  AFunc        _func;
  AClientData *_arg;

  AFunction(void) { _arg=0; _func=0; }
  AFunction(AFunc func_,AClientData *arg_=0) { _func=func_; _arg=arg_; }
  ~AFunction(void) { if (_arg!=0) delete _arg; }
  
  void set(AFunc func_,AClientData *arg_) 
   { _func=func_; if (_arg!=0) delete _arg; _arg=arg_; }
  void arg(AClientData *arg_) { if (_arg!=0) delete _arg; _arg=arg_; }
  void func(AFunc func_) { _func=func_; }
  void execute(V v_,A a_,A i_,A p_) { (void)(*func())(arg(),a_,i_,p_,v_); }

  AClientData *arg(void) { return _arg; }
  AFunc func(void)       { return _func; }
};

class ACycleFunction : public AFunction
{
public:
  ACycleFunction(void) { _func=0;_arg=0; }
  ACycleFunction(AFunc func_,AClientData *arg_=0) : AFunction(func_,arg_) {}
  ~ACycleFunction(void) {}
  
  A grc(V v_,int r,int c)
   {
     A av=(A)v_->a;
     I n=av->r-(av->t==Ct&&av->r);
     if(av=gv(Et,n),n)if(*av->p=r==-1?(I)aplus_nl:(I)gi(r),n==2)av->p[1]=c==-1?(I)aplus_nl:(I)gi(c);
     return av;
   }
  A callFunc(V v_,A a_,int row_,int col_)
   { 
     A i=(row_==-1&&col_==-1)?0:grc(v_,row_,col_);
     A r=(func()!=0)?(A)(*func())(arg(),a_,i,aplus_nl,v_):aplus_nl;
     if (i!=0) dc((A)i);
     if (a_!=0) dc((A)a_);
     return r;
   }
  A invoke(V v_,A value_,A pick_) 
   { A ap=0;
     A val=0;
     if (pick_!=0&&QS(pick_))   { ap=(A)gs(Et); *ap->p=(I)pick_; }
     else if (pick_!=0)         { ap=(A)ic(pick_); }
     if (value_!=0&&QS(value_)) { val=(A)gs(Et); *val->p=(I)value_; }
     else if (value_!=0)        { val=(A)ic(value_); }
     A r=(func()!=0)?(A)(*func())(arg(),val,0,ap,v_):aplus_nl;
     if (val!=0) dc((A)val);
     if (ap!=0) dc(ap);
     return r;
   }
  A invoke(V v_,A value_)
   { return (A)callFunc(v_,(A)ic(value_),-1,-1); } 
  A invoke(V v_,int value_,int row_,int col_)
   { return (A)callFunc(v_,(A)gi(value_),row_,col_); } 
  A invoke(V v_,double value_,int row_,int col_)
   { return (A)callFunc(v_,(A)gf(value_),row_,col_); } 
  A invoke(V v_,char *value_,int row_,int col_) 
   { return (A)callFunc(v_,(A)gsv(0,value_),row_,col_); } 
  A invoke(V v_,A value_,int row_,int col_)
   { A val=0;
     if (value_!=0&&QS(value_)) { val=(A)gs(Et); *val->p=(I)value_; }
     else if (value_!=0)        { val=(A)ic(value_); }
     return callFunc(v_,val,row_,col_);
   } 
};

class AColorFunction : public AFunction
{
public:
  AColorFunction(void) { _func=0; _arg=0; }
  AColorFunction(AFunc func_,AClientData *arg_=0) : AFunction(func_,arg_) {}
  ~AColorFunction(void) {}
  
  unsigned long callFunc(V v_,A a_,int row_,int col_,A p_=aplus_nl);
  unsigned long invoke(V v_,A value_,A pick_) 
   { A ap=0;
     A val=0;
     if (pick_!=0&&QS(pick_))   { ap=(A)gs(Et); *ap->p=(I)pick_; }
     else if (pick_!=0)         { ap=(A)ic(pick_); }
     if (value_!=0&&QS(value_)) { val=(A)gs(Et); *val->p=(I)value_; }
     else if (value_!=0)        { val=(A)ic(value_); }
     unsigned long r=callFunc(v_,(A)val,-1,-1,ap); 
     if (ap!=0) dc(ap);
     return r;
   }
  unsigned long invoke(V v_,A value_)
   { return callFunc(v_,(A)ic(value_),-1,-1); }
  unsigned long invoke(V v_,A value_,int row_,int col_,A pick_=aplus_nl)  
   { A val=0;
     if (value_!=0&&QS(value_)) { val=(A)gs(Et); *val->p=(I)value_; }
     else if (value_ != 0)    { val=(A)ic(value_); }
     return callFunc(v_,val,row_,col_,pick_);
   } 
  unsigned long invoke(V v_,int value_,int row_,int col_,A pick_=aplus_nl) 
   { A ai=(A)gi(value_);
     return callFunc(v_,ai,row_,col_,pick_);
   } 
  unsigned long invoke(V v_,double value_,int row_,int col_,A pick_=aplus_nl) 
   { A af=(A)gf(value_);
     return callFunc(v_,af,row_,col_,pick_);
   } 
  unsigned long invoke(V v_,char *value_,int row_,int col_,A pick_=aplus_nl) 
   { A str=(A)gsv(0,value_);
     return callFunc(v_,str,row_,col_,pick_);
   } 
  unsigned long invoke(V v_,char *value_,int len_,int row_,int col_,A pick_=aplus_nl)
  { I len=len_;
    A str=gc(Ct,1,len,&len,(I*)value_);
    return callFunc(v_,str,row_,col_,pick_);
  } 
};

class AFontFunction : public AFunction
{
public:
  AFontFunction(void) { _func=0; _arg=0; }
  AFontFunction(AFunc func_,AClientData *arg_=0) : AFunction(func_,arg_) {}
  ~AFontFunction(void) {}
  
  virtual Font callFunc(V v_,A a_,int row_,int col_,A p_=aplus_nl);
  Font invoke(V v_,A value_,A pick_) 
   { A ap=0;
     A val=0;
     if (pick_!=0&&QS(pick_))     { ap=(A)gs(Et); *ap->p=(I)pick_; }
     else if (pick_!=0)           { ap=(A)ic(pick_); }
     if (value_!=0&&QS(value_))   { val=(A)gs(Et); *val->p=(I)value_; }
     else if (value_!=0)          { val=(A)ic(value_); }
     Font r=callFunc(v_,(A)val,-1,-1,ap); 
     if (ap!=0) dc(ap);
     return r;
   }
  Font invoke(V v_,A value_)
   { return callFunc(v_,(A)ic(value_),-1,-1); }
  Font invoke(V v_,A value_,int row_,int col_,A pick_=aplus_nl)
   { A val=0;
     if (value_!=0&&QS(value_))  { val=(A)gs(Et); *val->p=(I)value_; }
     else if (value_!=0)         { val=(A)ic(value_); }
     return callFunc(v_,val,row_,col_,pick_);
   } 
  Font invoke(V v_,int value_,int row_,int col_,A pick_=aplus_nl)
   { A ai=(A)gi(value_);
     return callFunc(v_,ai,row_,col_,pick_);
   } 
  Font invoke(V v_,double value_,int row_,int col_,A pick_=aplus_nl)
   { A af=(A)gf(value_);
     return callFunc(v_,af,row_,col_,pick_);
   } 
  Font invoke(V v_,char *value_,int row_,int col_,A pick_=aplus_nl)
   { A str=(A)gsv(0,value_);
     return callFunc(v_,str,row_,col_,pick_);
   } 
  Font invoke(V v_,char *value_,int len_,int row_,int col_,A pick_=aplus_nl) 
  { I len=len_;
    A str=gc(Ct,1,len,&len,(I*)value_);
    return callFunc(v_,str,row_,col_,pick_);
  } 
};

class AReadOnlyFunction : public AFunction
{
public:
  AReadOnlyFunction(void) { _func=0; _arg=0; }
  AReadOnlyFunction(AFunc func_,AClientData *arg_=0) : AFunction(func_,arg_) {}
  ~AReadOnlyFunction(void) {}
  
  MSBoolean callFunc(V v_,A a_,int row_,int col_,A p_=aplus_nl);
  MSBoolean invoke(V v_,A value_,A pick_) 
     { A ap=0;
       A val=0;
       if (pick_!=0&&QS(pick_))   { ap=(A)gs(Et); *ap->p=(I)pick_; }
       else if (pick_!=0)         { ap=(A)ic(pick_); }
       if (value_!=0&&QS(value_)) { val=(A)gs(Et); *val->p=(I)value_; }
       else if (value_!=0)        { val=(A)ic(value_); }
       MSBoolean r=callFunc(v_,(A)val,-1,-1,ap); 
       if (ap!=0) dc(ap);
       return r;
     }
  MSBoolean invoke(V v_,A value_,int row_,int col_,A pick_=aplus_nl)
     { A val=0;
       if (value_!=0&&QS(value_)) { val=(A)gs(Et); *val->p=(I)value_; }
       else if (value_!=0)        { val=(A)ic(value_); }
       return callFunc(v_,val,row_,col_,pick_);
     } 
  MSBoolean invoke(V v_,A value_)
     { return callFunc(v_,(A)ic(value_),-1,-1); }
  MSBoolean invoke(V v_,int value_,int row_,int col_,A pick_=aplus_nl)
     { A ai=(A)gi(value_);
       return callFunc(v_,ai,row_,col_,pick_);
     }
  MSBoolean invoke(V v_,double value_,int row_,int col_,A pick_=aplus_nl)
     { A af=(A)gf(value_);
       return callFunc(v_,af,row_,col_,pick_);
     } 
  MSBoolean invoke(V v_,char *value_,int row_,int col_,A pick_=aplus_nl)
     { A str=(A)gsv(0,value_);
       return callFunc(v_,str,row_,col_,pick_);
     } 
  MSBoolean invoke(V v_,char *value_,int len_,int row_,int col_,A pick_=aplus_nl) 
  { I len=len_;
    A str=gc(Ct,1,len,&len,(I*)value_);
    return callFunc(v_,str,row_,col_,pick_);
  } 
};

class AOutFunction : public AFunction
{
public:
  AOutFunction(void) { _func=0; _arg=0; }
  AOutFunction(AFunc func_,AClientData *arg_=0) : AFunction(func_,arg_) {}
  ~AOutFunction(void) {}

  virtual A callFunc(V v_,A a_,int row_,int col_,A p_=aplus_nl);
  A invoke(V v_,A value_,A pick_) 
     { A ap=0;
       A val=0;
       if (pick_!=0&&QS(pick_))   { ap=(A)gs(Et); *ap->p=(I)pick_; }
       else if (pick_!=0)         { ap=(A)ic(pick_); }
       if (value_!=0&&QS(value_)) { val=(A)gs(Et); *val->p=(I)value_; }
       else if (value_!=0)        { val=(A)ic(value_); }
       A r=callFunc(v_,(A)val,-1,-1,ap); 
       if (ap!=0) dc(ap);
       return r;
     }
  A invoke(V v_,A value_)
     { return callFunc(v_,(A)ic(value_),-1,-1); }
  A invoke(V v_,A value_,int row_,int col_,A pick_=aplus_nl)
     { A val=0;
       if (value_!=0&&QS(value_)) { val=(A)gs(Et); *val->p=(I)value_; }
       else if (value_!=0)        { val=(A)ic(value_); }
       return callFunc(v_,val,row_,col_,pick_);
     } 
  A invoke(V v_,int value_,int row_,int col_,A pick_=aplus_nl)
     { A ai=(A)gi(value_);
       return (A) callFunc(v_,ai,row_,col_,pick_);
     } 
  A invoke(V v_,double value_,int row_,int col_,A pick_=aplus_nl)
     { A af=(A)gf(value_);
       return (A) callFunc(v_,af,row_,col_,pick_);
     } 
  A invoke(V v_,char *value_,int row_,int col_,A pick_=aplus_nl) 
     { A str=(A)gsv(0,value_);
       return (A) callFunc(v_,str,row_,col_,pick_);
     } 
  A invoke(V v_,char *value_,int len_,int row_,int col_,A pick_=aplus_nl) 
  { I len=len_;
    A str=gc(Ct,1,len,&len,(I*)value_);
    return (A) callFunc(v_,str,row_,col_,pick_);
  } 
};

// ACharStrFunction is just like AOutFunction, but requires character
// string output. MCA

class ACharStrFunction : public AOutFunction
{
public:
  ACharStrFunction(void) { _func=0; _arg=0; }
  ACharStrFunction(AFunc func_,AClientData *arg_=0) : AOutFunction(func_,arg_) {}
  ~ACharStrFunction(void) {}

  virtual A callFunc(V v_,A a_,int row_,int col_,A p_=aplus_nl);
};

class AGeometryFunction : public AFunction
{
public:
  AGeometryFunction(void) { _func=0; _arg=0; }
  AGeometryFunction(AFunc func_,AClientData *arg_=0) : AFunction(func_,arg_) {}
  ~AGeometryFunction(void) {}

  A invoke(V v_,A a_) 
     { A r=(_func!=0&&v_->a!=0)?(A)(*_func)(_arg,(A)ic(a_),aplus_nl,aplus_nl,v_):aplus_nl;
       dc((A)a_);
       return r;
     }
};

class AInFunction : public AFunction
{
public:
  AInFunction(void) { _func=0; _arg=0; }
  AInFunction(AFunc func_,AClientData *arg_=0) : AFunction(func_,arg_) {}
  ~AInFunction(void) {}

  A invoke(V v_,char *value_,int row_,int col_,A p_=aplus_nl)
     { A i=(row_==-1&&col_==-1)?0:(A)grc((A)v_->a,row_,col_);
       A a=(A)gsv(0,value_);       
       A r=(_func!=0)?(A)(*_func)(_arg,a,i,p_,v_):aplus_nl;
       if (i!=0) dc((A)i);
       dc((A)a);
       return r;
     } 
  A invoke(V v_,char *value_,int len_,int row_,int col_,A p_=aplus_nl)
     { I len=len_;
       A i=(row_==-1&&col_==-1)?0:(A)grc((A)v_->a,row_,col_);
       A a=gc(Ct,1,len,&len,(I*)value_);
       A r=(_func!=0)?(A)(*_func)(_arg,a,i,p_,v_):aplus_nl;
       if (i!=0) dc((A)i);
       dc((A)a);
       return r;
     } 
  A invoke(V v_,char *value_,A pick_) 
     { A ap=0;
       if (pick_!=0&&QS(pick_)) { ap=(A)gs(Et); *ap->p=(I)pick_; }
       else if (pick_!=0)       { ap=(A)ic(pick_); }
       A r=invoke(v_,value_,-1,-1,ap); 
       if (ap!=0) dc(ap);
       return r;
     }
  A invoke(V v_,char *value_,int len_,A pick_) 
     { A ap=0;
       if (pick_!=0&&QS(pick_)) { ap=(A)gs(Et); *ap->p=(I)pick_; }
       else if (pick_!=0)       { ap=(A)ic(pick_); }
       A r=invoke(v_,value_,len_,-1,-1,ap); 
       if (ap!=0) dc(ap);
       return r;
     }
};

typedef enum
{
  ShapeUpdate =0,  // the shape of the object changed - i.e. rank change
  SizeUpdate  =1,  // same shape,but change in the size - i.e. more rows or cols
  ValueUpdate =2,  // same shape and size,but values have changed
  AppendUpdate=3    // same shape and size,but additional rows appended to data
                      // row=-1 : update all rows of specified col
                      // col=-1 : update all cols of specified row
                      // col=-1 && row=-1 : update all values
                      // row >= 0 && col >=0  : update specified row,col only
} UpdateType;

class AVariableData
{
protected:
  MSWidgetView        	*_pWidgetView;  // window association--1:1 relationship
  S                  	 _class;        // class of this variable - for Steve
  int                	 _colWidth;     // col width in chars for this variable
  int                	 _editWidth;    // edit width in chars for this var
  A                  	 _bg;           // background pixel value
  A                  	 _fg;           // foreground pixel value
  A                  	 _font;         // data font value
  MSBoolean          	 _readOnly;     // true if entire var is protected
  MSBoolean          	 _stars;        // should output be clipped or '*' used
  ACharStrFunction   	 _outFunc;      // formatting function
  AOutFunction       	 _titleFunc;    // formatting function for title of var
  AInFunction        	 _inFunc;       // validate and send back the new value
  AColorFunction     	 _fgFunc;       // color of a cell's foreground
  AColorFunction     	 _bgFunc;       // color of a cell's background
  AColorFunction     	 _titleColorFunc;  // color for the variable title
  AFontFunction      	 _fontFunc;     // get the font for a value
  AFontFunction      	 _titleFontFunc;// get the title font
  AReadOnlyFunction  	 _roFunc;       // is the cell protected
  AGeometryFunction  	 _geoFunc;      // get the geometry for its layout
  ACycleFunction     	 _cycleFunc;    // colors to cycle thru on data change
  AFunction          	 _doneFunc;     // notify app that input cycle is done
  A                  	 _title;
  A                  	 _titleFg;
  A                  	 _titleFont;
  static unsigned long   _defaultHlColor;
  static unsigned long   _defaultRowColor;
  static unsigned long   _defaultMatrixColor;

public:

  AVariableData(void); 
  ~AVariableData(void);

  MSWidgetView *pWidgetView(void)      {return _pWidgetView;}
  S varClass(void)                     {return _class;}
  A bgA(void)                          {return _bg;}
  A fgA(void)                          {return _fg;}
  A fontA(void)                        {return _font;}
  A titleAFg(void)                     {return _titleFg;}
  A titleAFont(void)                   {return _titleFont;}
  A titleA(void)                       {return _title;}
  A title(void)                        {return _title;}
  MSBoolean readOnly(void)               {return _readOnly;}
  MSBoolean stars(void)                  {return _stars;}
  inline unsigned long background(void)
  { return (isNull(bgA())==MSTrue) ? MSDisplayServer::defaultDisplayServer()->defaultBackground() : bgA()->p[0]; }
  inline unsigned long foreground(void)
  { return (isNull(fgA())==MSTrue) ? MSDisplayServer::defaultDisplayServer()->defaultForeground() : fgA()->p[0];}
  inline Font font(void)
  { return (isNull(fontA())==MSTrue) ? MSDisplayServer::defaultDisplayServer()->defaultFont() : fontA()->p[0];}

  unsigned long titleFg(void);
  Font titleFont(void);

  void title(A title_);
  void titleFg(A fg_);
  void titleFont(A fid_);
  void pWidgetView(MSWidgetView *pWidgetView_);
  
  void varClass(S sym_) { _class=sym_; }
  void background(A bg_);
  void foreground(A fg_);
  void font(A fa_);
  void setReadOnly(int i_);
  void stars(MSBoolean);

  ACharStrFunction *outFunc(void);
  void outFunc(AFunc func_,AClientData *arg_);

  AInFunction *inFunc(void);
  void inFunc(AFunc func_,AClientData *arg_);

  AColorFunction *fgFunc(void);
  void fgFunc(AFunc func_,AClientData *arg_);

  AColorFunction *bgFunc(void);
  void bgFunc(AFunc func_,AClientData *arg_);

  AFontFunction *fontFunc(void);
  void fontFunc(AFunc func_,AClientData *arg_);

  AFontFunction *titleFontFunc(void);
  void titleFontFunc(AFunc func_,AClientData *arg_);

  AColorFunction *titleColorFunc(void);
  void titleColorFunc(AFunc func_,AClientData *arg_);

  AOutFunction *titleFunc(void);
  void titleFunc(AFunc func_,AClientData *arg_);

  AReadOnlyFunction *roFunc(void);
  void roFunc(AFunc func_,AClientData *arg_);

  AGeometryFunction *geoFunc(void);
  void geoFunc(AFunc func_,AClientData *arg_);

  AFunction *doneFunc(void);
  void doneFunc(AFunc func_,AClientData *arg_);

  ACycleFunction *cycleFunc(void);
  void cycleFunc(AFunc func_,AClientData *arg_);

  // always called after setting outFunction -- potential problem ???
  int colWidth(void) { return _colWidth; }
  void colWidth(int, MSBoolean =MSTrue);

  int editWidth(void) { return _editWidth; }
  void editWidth(int ew_) { if (ew_!=editWidth()) _editWidth=ew_;  }

  static unsigned long defaultHlColor(void);
  static unsigned long defaultRowColor(void);
  static unsigned long defaultMatrixColor(void);

  static void defaultHlColor(unsigned long c_) { _defaultHlColor = c_; }
  static void defaultRowColor(unsigned long c_) { _defaultRowColor = c_; }
  static void defaultMatrixColor(unsigned long c_) { _defaultMatrixColor = c_; }

};

extern V getV(A a_);
extern V getVFromSym(CX context_,S sym_);
extern A getSym(V v_);
extern A getSymWithContext(V v_);
extern MSBoolean isSlotFiller(A a_);

inline V getVfromA(A a_)  { return (isNull(a_)==MSFalse && (QS(a_)||a_->t==Et)) ? getV(a_) : 0; }

inline AVariableData *pAVarDataFromV(V v_)  { return (AVariableData *)(v_==0 ? 0 : v_->attr); }

#endif
