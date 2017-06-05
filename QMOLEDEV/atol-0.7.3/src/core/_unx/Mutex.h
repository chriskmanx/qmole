////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for Mutex object on Linux
////////////////////////////////////////////////////////////////////////////

#ifndef _MUTEX_POSIX_
#define _MUTEX_POSIX_

#include <pthread.h>

class Mutex
{
public:
//private:
	pthread_mutex_t M;

public:
#ifdef _DEBUG
	bool locked;
#endif

  Mutex()
  {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr,PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&M,&attr);
    pthread_mutexattr_destroy(&attr);
 #ifdef _DEBUG
    locked = false;
 #endif
  }

  virtual ~Mutex()
  { pthread_mutex_destroy(&M); }

  bool Lock() 
  { 
#ifdef _DEBUG
	int r = pthread_mutex_lock(&M);
	if(r == 0) locked = true;
	return r == 0;
#else
	return pthread_mutex_lock(&M) == 0;
#endif
  }

  int Lock_Try() 
  { return pthread_mutex_trylock(&M); }

  bool Unlock() 
  {
#ifdef _DEBUG
	int r = pthread_mutex_unlock(&M);
	if(r == 0) locked = false;
	return r == 0;
#else
	return pthread_mutex_unlock(&M) == 0;
#endif
  }
};

#endif

