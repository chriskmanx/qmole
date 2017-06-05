////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: Implementation for Semaphore object on Linux
////////////////////////////////////////////////////////////////////////////

#ifndef _SEMAPHORE_H__
#define _SEMAPHORE_H__

class Semaphore 
{ 
private: 
	sema_t semaphore; 

public: 
	Semaphore() { sema_init(&semaphore, 0, USYNC_PROCESS, (void*)0); }
	Semaphore(int available) { sema_init(&semaphore, available, USYNC_PROCESS, (void*)0); }
	~Semaphore() { sema_destroy(&semaphore); } 
	
	void Post() { sema_post(&semaphore); }
	void Post(int how_many) { while (how_many-- > 0) sema_post(&semaphore); }
	void Wait() { sema_wait(&semaphore); } 

	bool IsAvailable(){ return (0 == sema_trywait(&semaphore, 0)); }
};

#endif
