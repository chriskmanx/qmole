#ifndef MSStringTestHEADER
#define MSStringTestHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

/*******************************************************************************
* DESCRIPTION:                                                                 *
*   This file contains the declaration(s) of the class(es):                    *
*     MSStringTest-Abstract "string test" class.                               *
*******************************************************************************/

#include <MSTypes/MSEnum.H>

extern "C"
{
  typedef int MSCStringTestFn(int);
}

#if defined(MS_C_LINKAGE_INCOMPATIBILITY)
typedef int (*MSCPPStringTestFn)(int);
#endif

class MSTypesExport MSStringTest
{
/*******************************************************************************
  The MSStringTest class defines the basic protocol for test objects that can
  be passed to MSStrings to assist in performing various
  testing and searching functions.  It also provides concrete implementation
  for the common case of using a plain C function for such testing.

  A derived template class,MSStringTestMemberFn,is provided to permit the
  use of member functions of any class on the MSString functions that support
  MSStringTest.

  Derived classes should re-implement the test virtual function to test
  characters passed by the MSString and return the appropriate result.

  A constructor for this class accepts a pointer to a C function that accepts
  an integer as an argument and returns a MSBoolean.  Such functions can be
  used anywhere an MSStringTest can be used.  Note that this is the type of
  the standard C library is.... functions that check the type of C characters.

  Example:
    // Strip leading alphanumerics from aString...
    MSString
      aString;
    ...
    aString.stripLeading(isalnum); // Uses C library isalnum function.
*******************************************************************************/
public:
/*------------------------------ Function Types --------------------------------
  CFunction  -Pointer to the C function that accepts an integer argument
                and returns MSBoolean.
------------------------------------------------------------------------------*/
  typedef MSCStringTestFn CFunction;

/*------------------------------- Constructor ---------------------------------
  An object of this class can be constructed with a pointer to the C
  function to be used to implement the test member function.
------------------------------------------------------------------------------*/
  MSStringTest(CFunction *cFunc);
  virtual ~MSStringTest(void);


#if defined(MS_C_LINKAGE_INCOMPATIBILITY)
  typedef MSCPPStringTestFn CPPFunction;
  MSStringTest(CPPFunction cFunc);
#endif

/*--------------------------------- Testing ------------------------------------
  test-This function tests the argument integer(character) and returns
       MSMSTrue or MSMSFalse as returned by the C function provided at construction.
       Derived classes should override this function to implement their own
       testing function.
------------------------------------------------------------------------------*/
  virtual MSBoolean test(int) const;

protected:
/*------------------------------ Implementation --------------------------------
  FnType-Enumeration describing the various flavors of functions
           supported.
             user  - User-defined.
             c     - C.
             cpp   - C++ static or non-member function.
             memFn - C++ member function.
             cMemFn- Const C++ member function.

  data  - Data member union,varying by FnType:
             cFn  -Pointer to a C function.
             user -Pointer to an arbitrary derived-class data(if FnType is
                     neither c nor cpp).
------------------------------------------------------------------------------*/
  enum FnType {user,c,cpp,memFn,cMemFn};
  FnType type;
#if defined(MS_C_LINKAGE_INCOMPATIBILITY)
  union {CFunction *cFn; CPPFunction cppFn; void *user;} data;
#else
  union {CFunction *cFn;void *user;} data;
#endif

/*--------------------- Protected Constructor ----------------------------------
  This protected constructor can be used by derived classes to
  reuse the space for the C/C++ function pointer.
------------------------------------------------------------------------------*/
  MSStringTest(FnType type,void *userData);

}; // class MSStringTest

#if defined(MS_C_LINKAGE_INCOMPATIBILITY)
inline MSStringTest::MSStringTest(CPPFunction cppfunc):type(cpp)
{ data.cppFn=cppfunc; }
#endif

inline MSStringTest::MSStringTest(FnType typ,void *p):type(typ)
{ data.user=p; }

#endif // MSStringTestHEADER
