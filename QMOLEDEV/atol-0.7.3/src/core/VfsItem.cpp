////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: VfsItem implementation
////////////////////////////////////////////////////////////////////////////

#if _MSC_VER > 1000
 #pragma warning (disable: 4786)
#endif

#include "VfsItem.h"
#include "String.h"
#include "types.h"

String FormatSize(INT64 nValue, char cDelimiter = ',');

#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>

VfsItem::VfsItem(const VfsItem& a)
{
	Clear();
	operator = (a);
}
    
VfsItem::VfsItem()
{
	Clear();
}

void VfsItem::operator =(const VfsItem& a)
{
	m_strPath        = a.m_strPath;
	m_strName        = a.m_strName;
	m_nLastModDate   = a.m_nLastModDate;
	m_nSize          = a.m_nSize;
	m_nAttrib        = a.m_nAttrib;
	m_nExtPos        = a.m_nExtPos;
	m_nIconIdx       = a.m_nIconIdx;
}

void VfsItem::Clear()
{
	m_strPath       = "";
	m_strName       = "";
	m_nSize         = -1; 
	m_nExtPos       = -1;
	m_nLastModDate  = 0;
	m_nAttrib       = 0;
	m_nIconIdx      = -1;
}

bool VfsItem::IsDir() const 
{ 
	return (0 != (m_nAttrib & ATTR_DIR)); 
}

bool VfsItem::IsLink() const 
{ 
	return (0 != (m_nAttrib & ATTR_LINK)); 
}

bool VfsItem::IsDots() const
{
    if(IsDir())
    {
        //TOFIX speedup using static const strings "."/".."
        if( m_strName == String(".") || 
            m_strName == String("..") )
        {
            return true;
        }
    }
    return false;
}
    
String VfsItem::GetName() const 
{
    return m_strName;
}

String VfsItem::GetTitle() const 
{
    if(m_nExtPos > 0)
        return m_strName.Left(m_nExtPos);
    return m_strName;
}

String VfsItem::GetExt() const 
{
    if(m_nExtPos > 0)
        return m_strName.Mid(m_nExtPos);      //ext with dot
    return String("");
}

String VfsItem::GetExtTitle() const 
{
    if(m_nExtPos > 0)
        return m_strName.Mid(m_nExtPos+1);    //ext without dot
    return String("");
}

void VfsItem::CalcExt()
{
    //calculate start of extension within item name
    if(IsDir())
        m_nExtPos = -1;    //dirs don't have extensions
    else
    {
        //find last '.' character in the name
        m_nExtPos = m_strName.Find(".", false);

        //when item starts with single . it is not counted as extension
        //(since then item would have no name)
        if(m_nExtPos == 0)    
            m_nExtPos = -1;
    }
}

String VfsItem::GetSize() const 
{
	return (IsDir() && m_nSize < 0) ? String("<DIR>") : FormatSize(m_nSize);
}

String VfsItem::GetDate() const 
{
	String strDate;
    struct tm *pTM = localtime( &m_nLastModDate );
	if(NULL != pTM)
		strDate.Printf("%04d.%02d.%02d %02d:%02d", 1900+pTM->tm_year, 1+pTM->tm_mon, pTM->tm_mday, pTM->tm_hour, pTM->tm_min);
	return strDate;
}

String VfsItem::GetAttr() const 
{
    String strAttr;

    if(m_nAttrib & ATTR_UNIX)
    {
        //calculate first letter
        if(m_nAttrib & ATTR_DIR)
            strAttr.Append("d");
        else if(m_nAttrib & ATTR_LINK)
            strAttr.Append("l");
        else 
            strAttr.Append("-");

        //calculate other letters
        if(m_nAttrib & ATTR_R_USR)    strAttr.Append("r"); else  strAttr.Append("-");
        if(m_nAttrib & ATTR_W_USR)    strAttr.Append("w"); else  strAttr.Append("-");
        if(m_nAttrib & ATTR_X_USR)    strAttr.Append("x"); else  strAttr.Append("-");    

        if(m_nAttrib & ATTR_R_GRP)    strAttr.Append("r"); else  strAttr.Append("-");
        if(m_nAttrib & ATTR_W_GRP)    strAttr.Append("w"); else  strAttr.Append("-");
        if(m_nAttrib & ATTR_X_GRP)    strAttr.Append("x"); else  strAttr.Append("-");    

        if(m_nAttrib & ATTR_R_OTH)    strAttr.Append("r"); else  strAttr.Append("-");
        if(m_nAttrib & ATTR_W_OTH)    strAttr.Append("w"); else  strAttr.Append("-");
        if(m_nAttrib & ATTR_X_OTH)    strAttr.Append("x"); else  strAttr.Append("-");    
    }
    else
    {
        if(m_nAttrib & ATTR_RONLY)    strAttr.Append("r"); else  strAttr.Append("-");
        if(m_nAttrib & ATTR_ARCH)     strAttr.Append("a"); else  strAttr.Append("-");
        if(m_nAttrib & ATTR_HIDDEN)   strAttr.Append("h"); else  strAttr.Append("-");    
        if(m_nAttrib & ATTR_SYSTEM)   strAttr.Append("s"); else  strAttr.Append("-");
    }
    
    return strAttr;
}

void VfsItem::SetName(const char *szName)
{
	m_strName = szName;
	CalcExt();
}

// Format number as string separating every three digits
// with given separator for better readibility
String FormatSize(INT64 nValue, char cDelimiter)
{
    //use portable 64bit inside printf format "%I64d", ...
    //static const String strFormat = String::Printf("%%%sd", wxLongLongFmtSpec);

    String strResult;
  #ifdef _WIN32
    strResult.Printf("%I64d", nValue);
  #else  
    strResult.Printf("%lld", nValue);
  #endif
    
    //add delimiter to format size number 
    //NOTE: delimiter is added from end of string forward to the start
    //        because it simplifies the calculation (next insertion
    //        point does not change index as we insert delimiters)
    int nLen = strResult.Length();
    int nCommas = (nLen-1)/3;
    for(int i=0; i<nCommas; i++)
        strResult.insert(nLen-3*(i+1), 1, cDelimiter);
 
    return strResult;
}

