/*
 * AbiCollab - Code to enable the modification of remote documents.
 * Copyright (C) 2007 by Marc Maurer <uwog@uwog.net>
 * Copyright (C) 2007 One Laptop Per Child
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
 
#ifndef ABICOLLAB_SERIALIZATION_H
#define ABICOLLAB_SERIALIZATION_H

#include <map>
#include <vector>
#include <string>
#include <string.h>
#include <ut_types.h>
#include <ut_string_class.h>

//
// Quick n' compact serialization implementation
// 			by Marc 'Foddex' Oude Kotte (marc@solcon.nl)
//

// POD type traits
#define DEFINE_POD_OPERATOR_ONE(T)					\
	Archive& operator<<( T& Val )					\
	{												\
		Serialize( &Val, sizeof(T) );				\
		return *this;								\
	}
#define DEFINE_POD_OPERATOR_VECTOR(T)				\
	Archive& operator<<( std::vector<T>& Val )		\
	{												\
		unsigned int count;							\
		if (isLoading()) {							\
			*this << COMPACT_INT(count);			\
			Val.resize( count );					\
		} else {									\
			count = Val.size();						\
			*this << COMPACT_INT(count);			\
		}											\
		Serialize( &Val[0], count*sizeof(T) );		\
		return *this;								\
	}

#define DEFINE_POD_OPERATOR(T)						\
	DEFINE_POD_OPERATOR_ONE(T)						\
	DEFINE_POD_OPERATOR_VECTOR(T)

class Archive;

/** Compact serializer, can safely be used to compact int's and unsigned int's.
 *	Basically any 4-bytes value. Don't use it on size_t's or longs, 64-bits
 *	machines will crash.
 *	Use the COMPACT_INT define to easily serialize integer values:
 *		e.g.
 *			ar << COMPACT_INT(m_iRev); 
 */
#define COMPACT_INT(v) (*(CompactInt*)&v)
struct  CompactInt {
public:	
	friend Archive& operator<<( Archive& ar, CompactInt& c );
protected:
	int Val;
};

/** Base archive */
class Archive 
{
public:
	virtual ~Archive() {}
	bool isLoading() const { return m_bLoading; }
	bool isSaving() const { return !m_bLoading; }
	virtual void Serialize( void* Buffer, unsigned int Count ) = 0;
	virtual unsigned int Size() const = 0;
	virtual void Skip( unsigned int Count ) = 0;
	virtual bool EndOfFile() const = 0;
protected:
	Archive( bool bLoading )
	: m_bLoading( bLoading )
	{}
private:
	bool	m_bLoading;
	
public:
	// pod types
	DEFINE_POD_OPERATOR(unsigned char);
	DEFINE_POD_OPERATOR(unsigned short);
	DEFINE_POD_OPERATOR(unsigned int);
	DEFINE_POD_OPERATOR(UT_uint64);
	DEFINE_POD_OPERATOR(char);
	DEFINE_POD_OPERATOR(short);
	DEFINE_POD_OPERATOR(int);
	DEFINE_POD_OPERATOR(float);
	DEFINE_POD_OPERATOR(double);
	DEFINE_POD_OPERATOR_ONE(bool);
	
	// basic string
	Archive& operator<<( std::string& Val )
	{
		unsigned int s;	// NOTE: not size_t's !!
		if (isLoading()) {
			// get size, resize, and then get characters
			*this << COMPACT_INT(s);
			Val.resize( s );
		} else {
			// save size & then characters
			s = Val.size();
			*this << COMPACT_INT(s);
		}
		Serialize( &Val[0], s );
		return *this;
	}
#if !defined(SERIALIZATION_TEST)
	// UT_UTF8String 
	Archive& operator<<( UT_UTF8String& Val )
	{
		// easy, but not really the most efficient implementation :-)
		if (isLoading()) {
			std::string s;
			*this << s;
			Val = UT_UTF8String( s.c_str() );
		} else {
			std::string s = Val.utf8_str();
			*this << s;
		}
		return *this;
	}
#endif
	// Default, expects it's an object and will attempt to call serialize on it.
	// If compilation fails here, write a custom operator for the type you're trying
	// to serialize, or add a serialize member to your class.
	template<typename _T>
	Archive& operator<<( _T& Val )
	{
		Val.serialize( *this );
		return *this;
	}
#if !defined(SERIALIZATION_TEST)
	// Vector serialization (note: there are optimized version of this
	// for the various POD types)
	template<typename _K>
	Archive& operator<<( std::vector<_K>& Val ) 
	{
		unsigned int count;
		if (isLoading()) {
			*this << count;
			Val.resize( count );
		} else {
			count = Val.size();
			*this << count;
		}
		for (unsigned int i=0; i<count; ++i) {
			*this << Val[i];
		}
		return *this;
	}
	// Map serialization
	template<typename _K, typename _V>
	Archive& operator<<( std::map<_K,_V>& Val ) 
	{
		unsigned int count;
		if (isLoading()) 
		{
			Val.clear();
			*this << count;
			for (unsigned int i=0; i<count; ++i) 
			{
				_K k;
				_V v;
				*this << k << v;
				Val.insert( typename std::map<_K,_V>::value_type( k, v ) );
			}
		} 
		else 
		{
			count = Val.size();
			*this << count;
			for (typename std::map<_K,_V>::iterator it=Val.begin(); it!=Val.end(); ++it) {
				*this << (_K&)(*it).first << (_V&)(*it).second;
			}
		}
		return *this;
	}
#endif
};

/** String based archive */
class StrArchive : public Archive
{
public:
	unsigned int Size() const { return m_sSource.size(); }
	const std::string& getData() const { return m_sSource; }
protected:
	StrArchive()
	: Archive( false )
	{}
	StrArchive( const std::string& sSource )
	: Archive( true )
	, m_sSource( sSource )
	{}
	std::string			m_sSource;
};

/** Input (loading) archive */
class IStrArchive : public StrArchive 
{
public:
	IStrArchive( const std::string& sSource )
	: StrArchive( sSource )
	, m_uPosition( 0 )
	{}
	virtual void Serialize( void* Buffer, unsigned int Count )
	{
#if !defined(SERIALIZATION_TEST)
		UT_ASSERT( m_uPosition + Count <= m_sSource.size() );	// check if we're overshooting!
#endif
		memcpy( Buffer, &m_sSource[m_uPosition], Count );
		m_uPosition += Count;
	}
	virtual void Skip( unsigned int Count )
	{
#if !defined(SERIALIZATION_TEST)
		UT_ASSERT( m_uPosition + Count <= m_sSource.size() );	// check if we're overshooting!
#endif
		m_uPosition += Count;
	}
	virtual bool EndOfFile() const
	{
		return m_uPosition >= m_sSource.size();
	}
protected:
	unsigned int				m_uPosition;
};

/** Output (saving) archive */
class OStrArchive : public StrArchive
{
public:
	OStrArchive()
	: StrArchive()
	{}
	virtual void Serialize( void* Buffer, unsigned int Count )
	{
		unsigned int pos = m_sSource.size();
		m_sSource.resize( pos + Count );
		memcpy( &m_sSource[pos], Buffer, Count );
	}
	virtual void Skip( unsigned int /*Count*/ )
	{
#if !defined(SERIALIZATION_TEST)
		UT_ASSERT(UT_SHOULD_NOT_HAPPEN);
#endif
	}
	virtual bool EndOfFile() const
	{
		return true;	// writing file is always at EOF ;)
	}
};

#endif /* ABICOLLAB_SERIALIZATION_H */
