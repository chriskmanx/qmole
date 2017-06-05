////////////////////////////////////////////////////////////////////////////
// Atol file manager project <http://atol.sf.net>
//
// This code is licensed under BSD license.See "license.txt" for more details.
//
// File: This file collects some useful functions
////////////////////////////////////////////////////////////////////////////

#ifndef WXUTIL_H__
#define WXUTIL_H__

#include <gtk/gtk.h>
#include <vector>
#include <string>
#include <algorithm>
#include "String.h"
#include "types.h"
 
// Strategy for freeing objects of type T.
template <typename T>
struct FreeStrategy
{
	static void Free(T* ptr)
	{
		delete ptr;
	}
};

// Smart pointer to object or array of objects of type T.
template < typename T, typename FS = FreeStrategy<T> >
class ScopedPtr
{
public:
	ScopedPtr() : m_ptr(0) {}
	ScopedPtr(T* ptr) : m_ptr(ptr) {}
	~ScopedPtr()
	{
		Reset();
	}

	void Reset(T* ptr = 0)
	{
		if (m_ptr != ptr)
		{
			if (m_ptr != 0)
				FS::Free(m_ptr);
			m_ptr = ptr;
		}
	}

	operator bool() const
	{
		return m_ptr != 0;
	}

	// Get stored pointer.
	T* Get() const
	{
		return m_ptr;
	}

	// Get address of stored pointer.
	T** GetAddress()
	{
		return &m_ptr;
	}

private:
	// forbid copying
	ScopedPtr(const ScopedPtr& );
	const ScopedPtr& operator=(const ScopedPtr& );

	T* m_ptr;
};

namespace Glib
{

// Free strategy that uses g_free.
template <typename T>
struct GFreeStrategy
{
	static void Free(T* ptr)
	{
		g_free(ptr);
	}
};

typedef ScopedPtr< char, GFreeStrategy<char> > ScopedCharPtr;

template<typename T>
struct ListTraits
{
	typedef T CType;
	typedef T CppType;

	static CType ToCType(const CppType& value) { return value; }
	static CppType ToCppType(const CType& value) { return value; }
	static void ReleaseCType(const CType& ) {}
};

template<>
struct ListTraits<std::string>
{
	typedef char* CType;
	typedef std::string CppType;

	static CType ToCType(const CppType& value)
	{
		return static_cast<CType>(g_memdup(value.c_str(), value.size() + 1));
	}
	static CppType ToCppType(const CType& value)
	{
		return value;
	}
	static void ReleaseCType(const CType& value)
	{
		g_free(value);
	}
};

// GList wrapper.
// TODO: Consider using Glibmm instead of reimplementing wrappers.
template< typename T, typename Traits = ListTraits<T> >
class List
{
public:
	List() : m_handle(0) {}
	// transfers ownership
	List(const List& other) : m_handle(g_list_copy(other.m_handle))
	{
		other.m_handle = 0;
	}
	virtual ~List()
	{
		for (GList* node = m_handle; node != 0; node = node->next)
			Traits::ReleaseCType(static_cast<typename Traits::CType>(node->data)); 
		g_list_free(m_handle);
	}

	const List& operator=(const List& other)
	{
		List temp(other);
		Swap(temp);
	}

	void Swap(List& other)
	{
		std::swap(m_handle, other.m_handle);
	}

	GList* GetHandle() const
	{
		return m_handle;
	}

	void Append(const T& t)
	{
		m_handle = g_list_append(m_handle, Traits::ToCType(t));
	}

private:
	GList* m_handle;
};

} // namespace Glib
 

void OpenCommandPrompt(const String &strDir, void *nData);

bool fnmatch(const char* pattern, const char *string, bool caseSensitive, bool bDOS);
bool WildMatch(const char* pattern, const char *string, bool caseSensitive);
int StrCommonPrefixLen(const char *szPath1, const char *szPath2);

String FormatSizeUnits(INT64 nBytes);
String FormatTime(UINT64 uSeconds);
String FormatSize(INT64 nValue, char cDelimiter = ',');

void Tokenize(String strData, std::vector<String> &lstTokenized, char szSeparator = ';');

int laccess(const char *szFile, int nMode);

#endif



