#ifndef MSStringTestMemberFnHEADER
#define MSStringTestMemberFnHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////


#include <MSTypes/MSStringTest.H>

template<class T >
class MSStringTestMemberFn : public MSStringTest
{
/*******************************************************************************
  The template class MSStringTestMemberFn is used to provide an
  MSStringTest-type wrapper for particular C++ member functions.  This permits
  such member functions to be used in conjunction with the MSString
  functions that accept an MSStringTest object as an argument.

  The template class argument is the class name T of the class whose member
  function is to be wrappered.

  The constructor for the object requires two things:
    - An instance of class T.
    - A pointer to a member function of the class T.  This member function
      will be applied to the object specified on the constructor to test
      each character passed to the test function of this class.  The member
      function must accept a single integer argument and return MSBoolean.

  Both const and non-const member functions are supported.  The latter
  require that a non-const member be specified as the first argument.

  Example:
    // Any arbitrary class with appropriate function...
    struct StringTester {
    MSBoolean
      charTest(int c) const;
    };
    // StringTester object...
    StringTester
      tester;
    // Test aString using tester object...
    MSString
      aString;
    ...
    aString
      .includes(MSStringTestMemberFn<StringTester>(tester,
                                                    StringTester::charTest));

  Customization(Template Arguments)
  Class MSStringTestMemberFn is a template class instantiated with the
  template arguments:

    class MSTypesExport T
*******************************************************************************/
public:
/*------------------------------ Related Types ---------------------------------
  ConstFn   -Const member function of the appropriate type.
  NonconstFn-Non-const member function of the appropriate type.
------------------------------------------------------------------------------*/
  typedef MSBoolean(T::*ConstFn)(int) const;
  typedef MSBoolean(T::*NonconstFn)(int);

/*------------------------------- Constructors ---------------------------------
  There are two constructors,one supporting const member functions,the
  other non-const member functions.  The constructors also require an object
  of the class T(non-const object for non-const member functions).
------------------------------------------------------------------------------*/
  MSStringTestMemberFn(const T &object,ConstFn constFn)
   :MSStringTest(user,(void*)&object),cMF(constFn) {}
  MSStringTestMemberFn(T &object,NonconstFn nonconstFn)
   :MSStringTest(user,(void*)&object),ncMF(nonconstFn) {}

/*-------------------------------- Overrides -----------------------------------
  test-Overridden to dispatch a member function against an object.
------------------------------------------------------------------------------*/
  virtual MSBoolean test(int aChar_) const
   {
     if (type==cMemFn) return MSBoolean(((const T*)data.user->*cMF)(aChar_)>0);
     else return MSBoolean(((T*)data.user->*ncMF)(aChar_)>0);
   }
private: /*------------------------ PRIVATE ----------------------------------*/
  union {ConstFn cMF;NonconstFn ncMF;};
};

#endif // MSStringTestMemberFnHEADER
