////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: TOFIX
////////////////////////////////////////////////////////////////////////////

#ifndef FTP_H__
#define FTP_H__

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "ConnectionInfo.h"

class CFtpInfo : public CConnectionInfo
{
public:
    
    typedef enum {
	    ENCRYPT_NONE = 0,
	    ENCRYPT_SSLv2,
	    ENCRYPT_SSLv3,
	    ENCRYPT_TLSv1 } FtpEncryptionType;
	    
    CFtpInfo(){ 
		m_uPort = 21; 
		m_bAnonymous = false;
		m_bPassiveMode = false;
		m_nListLongFormat	= 0;
		m_nListShowHidden	= 0;
		m_nListResolveLinks	= 0;
		m_nListCompleteTime = 0;
		m_nEncryptType = ENCRYPT_NONE;
		m_nEncryptData = 0;
	};

    CFtpInfo(const CFtpInfo &a){ operator=(a); };

    void operator =(const CFtpInfo &a)
    {
        if(this == &a)
            return;

		CConnectionInfo::operator =(a);
		m_strAccount	= a.m_strAccount;	//account name
		m_bAnonymous    = a.m_bAnonymous;
		m_bPassiveMode  = a.m_bPassiveMode;
		m_nEncryptType		= a.m_nEncryptType;
		m_nEncryptData		= a.m_nEncryptData;
		m_nListLongFormat	= a.m_nListLongFormat;
		m_nListShowHidden	= a.m_nListShowHidden;
		m_nListResolveLinks	= a.m_nListResolveLinks;
		m_nListCompleteTime = a.m_nListCompleteTime;
    }

//	void Serialize( CArchive& ar );
	bool IsValid(){
		if(m_strHost.IsEmpty())
			return false;
		if(!m_bAnonymous && (m_strUser.IsEmpty() || m_strPassword.IsEmpty()))
			return false;
		return true;
	}

public:
	String     m_strAccount;		//account name	//TOFIX start using
	bool        m_bAnonymous;       //anonymous login
	bool        m_bPassiveMode;     //client creates connections

	FtpEncryptionType	m_nEncryptType;
	unsigned char		m_nListLongFormat:1;
	unsigned char		m_nListShowHidden:1;
	unsigned char		m_nListResolveLinks:1;
	unsigned char		m_nListCompleteTime:1;
	unsigned char		m_nEncryptData:1;
//	unsigned char		m_nListRecursive:1;
};

#endif // ifndef FTP_H__
