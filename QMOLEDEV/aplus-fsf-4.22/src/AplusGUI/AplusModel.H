#ifndef AplusModelHEADER
#define AplusModelHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved.
// See .../src/LICENSE for terms of distribution.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <MSTypes/MSScalarModel.H>
#include <MSTypes/MSUnsignedLongVector.H>
#include <MSTypes/MSTime.H>
#include <a/fncdcls.h>
#include <a/x.h>

class MSWidgetView;

class AInFunction;
class ACharStrFunction;
class AOutFunction;
class AColorFunction;
class AFontFunction;
class AGeometryFunction;
class AReadOnlyFunction;
class ACycleFunction;
class AFunction;

class AVariableData;

extern int AplusEvaluationDepth;

class AplusModel : public MSScalarModel
{
private:
  // private default constructor to make sure that AplusModel is always constructed
  // with an explicit argument
  AplusModel();

protected:
  V _aplusVar;    // Aplus variable
  A _sflags;      // flags for S layer

  AplusModel& operator=(const AplusModel&);

public:
  AplusModel(V);
  ~AplusModel(void);

  inline void aplusVar(V);
  inline V    aplusVar(void) const;

  inline void sflags(A pFlags_);
  inline A    sflags(void) const;

  inline MSBoolean isValid(void) const;

  int numRows(void) const;
  int numCols(void) const;
  int charLength(void) const;

  inline MSBoolean evaluate(void) const;

  inline A a(void) const;

  inline int rank(void) const; 
  inline int a_type(void) const;
  inline int numElmts(void) const;
  inline int d0(void) const;
  inline int d1(void) const;
  inline I  *data(void) const;
  inline CX  context(void) const;
  inline AVariableData *pAVarData(void) const;

  virtual MSString asString(void) const;
  virtual MSString asDebugInfo(void) const;
  virtual MSString asMSF(void) const;
  virtual MSString className(void) const;
  virtual const MSSymbol& type(void) const;
  virtual MSError::ErrorStatus set(const char *);
  virtual MSError::ErrorStatus setFromMSF(const char *);
  virtual MSModel *clone(void) const;
  virtual MSModel *create(void) const;
  virtual void assign(const MSModel&); 
  virtual long compare(const MSModel&) const;

  static const MSSymbol& symbol(void);

  MSBoolean coupleWidgetView (MSWidgetView *);

  void bindWidgetView  (MSWidgetView *);
  void unbindWidgetView(void);

  static MSUnsignedLongVector & aplusVarList(void);

  static void doneCB(V,A,A,A);

  static AInFunction       *getInFunc(V);
  static ACharStrFunction  *getOutFunc(V);
  static AOutFunction      *getTitleFunc(V);
  static AColorFunction    *getFgFunc(V);
  static AColorFunction    *getBgFunc(V);
  static AColorFunction    *getTitleColorFunc(V);
  static AFontFunction     *getFontFunc(V);
  static AFontFunction     *getTitleFontFunc(V);
  static AReadOnlyFunction *getReadOnlyFunc(V);
  static AGeometryFunction *getGeometryFunc(V);
  static ACycleFunction    *getCycleFunc(V);
  static AFunction         *getDoneFunc(V);
};

inline void AplusModel::aplusVar(V var_)     { _aplusVar=var_; }
inline V    AplusModel::aplusVar(void) const { return _aplusVar; }

inline A AplusModel::a(void) const
{
  if (_aplusVar==0)
    {
      return 0;
    }
  else
    {
      if (_aplusVar->z==0)
	{
	  evaluate();
	}

      return (A)_aplusVar->a;
    }
}

inline void AplusModel::sflags(A pFlags_)   { dc(_sflags); _sflags=(A)ic(pFlags_); }
inline A    AplusModel::sflags(void) const  { return (A) ic(_sflags); }

inline MSBoolean AplusModel::isValid(void) const  { return (aplusVar()->z==1)?MSTrue:MSFalse; }

inline MSBoolean AplusModel::evaluate(void) const
{ ++AplusEvaluationDepth; gt(_aplusVar); --AplusEvaluationDepth; return (q==0)?MSTrue:MSFalse; }

inline int AplusModel::rank(void) const     { return (_aplusVar)?(int)a()->r:0; }
inline int AplusModel::a_type(void) const   { return (_aplusVar)?(int)a()->t:0; } 
inline int AplusModel::numElmts(void) const { return (_aplusVar)?(int)a()->n:0; }
inline int AplusModel::d0(void) const       { return (_aplusVar)?(int)a()->d[0]:0; }
inline int AplusModel::d1(void) const       { return (_aplusVar)?(int)a()->d[1]:0; }
inline I  *AplusModel::data(void) const     { return (_aplusVar)?(I *)a()->p:0; }
inline CX  AplusModel::context(void) const  { return (_aplusVar)?(CX)_aplusVar->cx:0; }

inline AVariableData *AplusModel::pAVarData(void) const   { return (AVariableData *)_aplusVar->attr; }

#endif  //AplusModelHEADER

