/* Copyright (C) 2008 AbiSource Corporation B.V.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#ifndef __ASYNC_WORKER__
#define __ASYNC_WORKER__

#include <asio.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/noncopyable.hpp>
#include <boost/enable_shared_from_this.hpp>
#include "ut_debugmsg.h"
#include <sync/xp/Synchronizer.h>

template <class T>
class AsyncWorker : private boost::noncopyable, public boost::enable_shared_from_this<AsyncWorker<T> >
{
public:
	AsyncWorker(boost::function<T ()> async_func, boost::function<void (T)> async_callback)
	: m_async_func(async_func),
	m_async_callback(async_callback),
	m_synchronizer() // can't initialize the synchronizer here yet, because you can't call shared_from_this() from a constructor
	{
	}

	virtual ~AsyncWorker()
	{
		UT_DEBUGMSG(("~AsyncWorker()\n"));
		if (m_thread_ptr)
			m_thread_ptr->join();
	}

	virtual void start()
	{
		m_synchronizer.reset(new Synchronizer(boost::bind(&AsyncWorker<T>::_signal, 
												boost::enable_shared_from_this<AsyncWorker<T> >::shared_from_this())));
		m_thread_ptr.reset(
				new asio::thread(
					boost::bind(&AsyncWorker::_thread_func,
								AsyncWorker<T>::shared_from_this())
				)
			);
	}

private:
	void _signal()
	{
		UT_DEBUGMSG(("Calling async callback function from the main loop\n"));
		m_async_callback(m_func_result);
		m_synchronizer.reset();
	}

	void _thread_func()
	{
		UT_DEBUGMSG(("Starting async function...\n"));
		m_func_result = m_async_func();
		UT_DEBUGMSG(("Async function completed...\n"));
		m_synchronizer->signal();
	}

	boost::function<T ()>					m_async_func;
	boost::function<void (T)>				m_async_callback;
	boost::shared_ptr<Synchronizer>			m_synchronizer;
	boost::shared_ptr<asio::thread>			m_thread_ptr;
	T										m_func_result;
};

#endif /* __ASYNC_WORKER__ */
