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

#ifndef __INTERRUPTABLE_ASYNC_WORKER__
#define __INTERRUPTABLE_ASYNC_WORKER__

#include <stdint.h>
#include <boost/bind.hpp>
#include "xap_App.h"
#include "xap_DialogFactory.h"
#include "AsyncWorker.h"
#include "ap_Dialog_GenericProgress.h"
#include "ServiceAccountHandler.h"

class InterruptedException {};
class InternalErrorException {};

template <class T>
class InterruptableAsyncWorker : public boost::enable_shared_from_this< InterruptableAsyncWorker<T> >
{
public:
	InterruptableAsyncWorker(boost::function<T ()> async_func)
		: m_async_func(async_func),
		m_worker_ptr(),
		m_pProgressDlg(NULL),
		m_progress(0),
		m_cancelled(false),
		m_finished(false),
		m_progressSynchronizerPtr(),
		m_result()
	{}
	
	T run()
	{
		UT_DEBUGMSG(("InterruptableAsyncWorker::run()\n"));

		m_worker_ptr.reset(new AsyncWorker<T>(m_async_func, boost::bind(&InterruptableAsyncWorker<T>::invoke_cb, InterruptableAsyncWorker<T>::shared_from_this(), _1)));
		m_progressSynchronizerPtr.reset(new Synchronizer(boost::bind(&InterruptableAsyncWorker<T>::_updateDialog, InterruptableAsyncWorker<T>::shared_from_this())));

		// get the progress dialog
		XAP_Frame* pFrame = XAP_App::getApp()->getLastFocussedFrame();
		if (!pFrame)
			throw InternalErrorException();

		XAP_DialogFactory* pFactory = static_cast<XAP_DialogFactory *>(XAP_App::getApp()->getDialogFactory());
		if (!pFactory)
			throw InternalErrorException();

		m_pProgressDlg = static_cast<AP_Dialog_GenericProgress*>(
					pFactory->requestDialog(ServiceAccountHandler::getDialogGenericProgressId())
				);		
		m_pProgressDlg->setTitle("Retrieving Document");
		m_pProgressDlg->setInformation("Please wait while retrieving document...");

		// start the asynchronous process
		m_worker_ptr->start();
		
		// run the dialog
		m_pProgressDlg->runModal(pFrame);
		UT_DEBUGMSG(("Progress dialog destroyed...\n"));
		m_cancelled = m_pProgressDlg->getAnswer() == AP_Dialog_GenericProgress::a_CANCEL;
		pFactory->releaseDialog(m_pProgressDlg);
		m_pProgressDlg = NULL;
		if (m_cancelled)
			throw InterruptedException();
		return m_result;
	}
	
	bool cancelled()
	{
		return m_cancelled;
	}

	void progress(uint32_t progress)
	{
		UT_DEBUGMSG(("InterruptableAsyncWorker::_progress_cb() - %d\n", progress));
		UT_return_if_fail(m_progressSynchronizerPtr);		
		
		if (progress > 100)
			progress = 100;
		
		m_progress = progress;
		m_progressSynchronizerPtr->signal();
	}

	// the result of run() is undefined when using this function
	void forceFinished()
	{
		m_finished = true;
		m_progressSynchronizerPtr->signal();
	}
	
private:
	void invoke_cb(T result)
	{
		UT_DEBUGMSG(("InterruptableAsyncWorker::invoke_cb()\n"));
		m_result = result;
		
		// signal the mainloop that we are done
		m_finished = true;
		m_progressSynchronizerPtr->signal();
	}
	
	void _updateDialog()
	{
		UT_DEBUGMSG(("InterruptableAsyncWorker::_updateDialog()\n"));
		if (m_finished)
		{
			UT_DEBUGMSG(("We are finished, closing dialog...\n"));
			if (m_pProgressDlg)
			{
				m_pProgressDlg->close();
			}
			else
			{
				UT_DEBUGMSG(("Skipping dialog closure; it is destructed already\n"));
			}
		}
		else
		{
			UT_DEBUGMSG(("Setting progress value to %d%%\n", m_progress));
			if (m_pProgressDlg) // it could be that we have no dialog when we have been cancelled
			{
				m_pProgressDlg->setProgress(m_progress);
			}
			else
			{
				UT_DEBUGMSG(("Skipping dialog progress update; it is destructed already\n"));
			}
		}
	}
	
	boost::function<T ()>				m_async_func;
	boost::shared_ptr< AsyncWorker<T> > m_worker_ptr;
	
	AP_Dialog_GenericProgress*			m_pProgressDlg;
	uint32_t							m_progress;
	bool								m_cancelled;	
	bool								m_finished;
	boost::shared_ptr<Synchronizer>		m_progressSynchronizerPtr;
	
	T									m_result;
};

#endif /* __INTERRUPTABLE_ASYNC_WORKER__ */
