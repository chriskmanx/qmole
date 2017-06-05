////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#include "OpConnect.h"

OpConnect::OpConnect()
{
//    m_pDlg = NULL;
	m_nOpType = OP_CONNECT;
}

OpConnect::~OpConnect()
{
}

void OpConnect::OpInit()
{
	/*
    m_pDlg = new CBlockingDlg;
    m_Stat.m_pWndProgress = m_pDlg;

    if(m_pDlg){
        m_pDlg->Create(NULL, 1, _("Operation in progress ..."), wxDefaultPosition, wxDefaultSize, wxCAPTION);
        m_pDlg->m_pOp = this;
        m_pDlg->ShowModal();
    }
	*/
}

void OpConnect::OpCleanup()
{
	/*
    if(m_pDlg){
        m_pDlg->Show(false);
        m_pDlg->Destroy();    //will delete the object
        m_pDlg = NULL;
    }
	*/
}

bool OpConnect::OpExecute()
{
	if(m_pVfsSrc)
		return m_pVfsSrc->Open();

	return false;
}

