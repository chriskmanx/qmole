////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for Event object on Linux
////////////////////////////////////////////////////////////////////////////

#ifndef _EVENT_H__
#define _EVENT_H__

#include "Mutex.h"

class Event
{ 
private: 
	pthread_mutex_t m_mutex;
	//Mutex  m_mutex;
	pthread_cond_t m_cond;
	int m_signaled;
	
public: 
	Event() { 
		pthread_mutex_init(&m_mutex, 0); 
		pthread_cond_init(&m_cond, NULL); 
		m_signaled = false;
	}
 
	~Event() { 
		pthread_cond_destroy(&m_cond); 
		pthread_mutex_destroy(&m_mutex); 
	}

	void Set()		
	{ 
 		//m_mutex.Lock();
		pthread_mutex_lock(&m_mutex);
		pthread_cond_broadcast(&m_cond);
		m_signaled = true;
		//m_mutex.Unlock();
		pthread_mutex_unlock(&m_mutex);
	}

	void Reset() { m_signaled = false; }
	void Wait()		
	{
		//m_mutex.Lock();
		pthread_mutex_lock(&m_mutex);

		if(!IsSignaled()){
			pthread_cond_wait(&m_cond, &m_mutex);
			//pthread_cond_wait(&m_cond, &(m_mutex.M));  
		}

		pthread_mutex_unlock( &m_mutex );
		//m_mutex.Unlock();
	}

	bool IsSignaled(){ return m_signaled; }
};

#endif //_EVENT_H__
