#ifndef MSMutexHEADER
#define MSMutexHEADER

///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
// See .../src/LICENSE for terms of distribution
//
//
///////////////////////////////////////////////////////////////////////////////

#ifndef MSDefinesHEADER
#include <MSTypes/MSDefines.H>
#endif

//
// Simple Mutex Lock. Relevant only in mt situation
//

#if defined(MS_MULTI_THREAD)

#if defined (MS_POSIX_THREADS)
#undef MIN
#undef MAX
#include <pthread.h>
typedef pthread_mutex_t MSMutexType;
#elif defined (MS_SOLARIS_THREADS)
#include <synch.h>
typedef mutex_t MSMutexType;
#elif defined (MS_WINDOWS_THREADS)
#include <windows.h>
typedef CRITICAL_SECTION MSMutexType;
#else
#error Class MSMutex is not yet supported in this environment
#endif



//
// MSMutex 
//

class MSTypesExport MSMutex
{
public:
  enum StaticCtorType { StaticCtor };
  
  MSMutex ();                   // Construct the mutex.
  MSMutex (StaticCtorType);     // Constructor for static use
  ~MSMutex ();                  // Destroy the mutex.
  void acquire ();              // Acquire the mutex.
  void release ();              // Release the mutex.

private:
  void init ();
  MSMutex (const MSMutex&);
  MSMutex& operator= (const MSMutex&);

  MSMutexType mutex;
#if !defined (MS_NO_MUTEX_INIT)
  int initFlag;
#endif
};

//
// MSGuard, acquire associated mutex upon construction, release when destructed.
//

class MSTypesExport MSGuard
{
public:
    MSGuard  (MSMutex& m);
    ~MSGuard ();          
private:
    MSMutex& _msmutex;
};

void inline MSMutex::init ()
{ 
#if defined (MS_POSIX_THREADS)
#  if defined(MS_POSIX_D10_THREADS)
#   if defined(MS_MUTEXATTR_DEFAULT)
    pthread_mutex_init(&mutex, &pthread_mutexattr_default);
#   else
    pthread_mutex_init(&mutex, 0);
#   endif
#  else
    pthread_mutex_init(&mutex, pthread_mutexattr_default);
#  endif
#elif defined (MS_SOLARIS_THREADS)
    mutex_init(&mutex, USYNC_THREAD, NULL); 
#elif defined (MS_WINDOWS_THREADS)
    InitializeCriticalSection(&mutex);
#endif

#if !defined (MS_NO_MUTEX_INIT)
    initFlag = 1;
#endif
}  


inline MSMutex::MSMutex () 
{ 
  init(); 
} 

//
// Inline definitions of MSMutex class
//

inline MSMutex::MSMutex (MSMutex::StaticCtorType)
{
    // Empty, because acquire() may already have been used.
}

inline MSMutex::~MSMutex () 
{ 
#if !defined (MS_NO_MUTEX_INIT)
  if (initFlag) 
   {
     initFlag=0;
#endif     
#if defined (MS_POSIX_THREADS)
    pthread_mutex_destroy(&mutex); 
#elif defined (MS_SOLARIS_THREADS)
    mutex_destroy(&mutex); 
#elif defined (MS_WINDOWS_THREADS)
    DeleteCriticalSection(&mutex);
#endif
#if !defined (MS_NO_MUTEX_INIT)
   }
#endif 
}

inline void MSMutex::acquire ()
{ 
#if !defined (MS_NO_MUTEX_INIT)
  if(initFlag==0) init();
#endif

#if defined (MS_POSIX_THREADS)
    pthread_mutex_lock(&mutex);    
#elif defined (MS_SOLARIS_THREADS)
    mutex_lock(&mutex);    
#elif defined (MS_WINDOWS_THREADS)
    EnterCriticalSection(&mutex);
#endif
}

inline void MSMutex::release ()
{ 
#if defined (MS_POSIX_THREADS)
    pthread_mutex_unlock(&mutex);  
#elif defined (MS_SOLARIS_THREADS)
    mutex_unlock(&mutex);  
#elif defined (MS_WINDOWS_THREADS)
    LeaveCriticalSection(&mutex);
#endif
}

//
// Inline definitions of MSGuard class
//


inline MSGuard::MSGuard (MSMutex& msmutex_) : _msmutex(msmutex_)
{
    _msmutex.acquire();
}

inline MSGuard::~MSGuard () { _msmutex.release(); }

//useful macro for protecting a piece of code with a guard on a stack.
#define MSGUARD(mutex)    MSGuard mutex##Guard (mutex)

#else // MS_MULTI_THREAD

#define MSGUARD(mutex)  

#endif // MS_MULTI_THREAD

#endif // MSMutexHEADER
