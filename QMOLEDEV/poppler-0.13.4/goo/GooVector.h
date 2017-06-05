//========================================================================
//
// GooVector.h
//
// This file is licensed under the GPLv2 or later
//
// Copyright 2010 David Benjamin <davidben@mit.edu>
// Copyright 2010 Albert Astals Cid <aacid@kde.org>
//
//========================================================================



#ifndef GOO_GOOVECTOR_H
#define GOO_GOOVECTOR_H

#ifdef USE_GCC_PRAGMAS
#pragma interface
#endif

#include <new> // vector implementations need placement-new

#include <assert.h>
#include <stdlib.h>

/* Mostly STL-compatible vector class. Should correctly call constructors and
 * destructors, but does not carefully handle alignment requirements. */

template<class T> class GooVector {
public:
  /* various STL-compatible typedefs */
  typedef T value_type;
  typedef T* pointer;
  typedef T& reference;
  typedef const T& const_reference;
  typedef size_t size_type;
  typedef int difference_type;
  typedef T* iterator;
  typedef const T* const_iterator;
  // TODO: reverse_iterator, if we feel like it
  
  GooVector() : m_data(NULL), m_capacity(0), m_size(0) {}
  explicit GooVector(size_type n) : m_data(NULL), m_capacity(0), m_size(0) {
    resize(n);
  }
  explicit GooVector(size_type n, const T& t) : m_data(NULL), m_capacity(0), m_size(0) {
    resize(n, t);
  }
  explicit GooVector(const GooVector& gv) : m_data(NULL), m_capacity(0), m_size(0) {
    reserve(gv.size());
    for (size_type i = 0; i < m_size; i++) {
      push_back(gv[i]);
    }
  }

  ~GooVector() {
    clear();
  }

  iterator begin() { return m_data; }
  const_iterator begin() const { return m_data; }
  iterator end() { return m_data + m_size; }
  const_iterator end() const { return m_data + m_size; }

  size_type size() const { return m_size; }
  size_type capacity() const { return m_capacity; }

  bool empty() const { return m_size == 0; }

  reference operator[] (size_type n) { return m_data[n]; }
  const_reference operator[] (size_type n) const { return m_data[n]; }

  reference at(size_type n) {
    assert(n < m_size);
    return m_data[n];
  }
  const_reference at(size_type n) const {
    assert(n < m_size);
    return m_data[n];
  }

  reference front() { assert(!empty()); return m_data[0]; }
  const_reference front() const { assert(!empty()); return m_data[0]; }

  reference back() { assert(!empty()); return m_data[m_size-1]; }
  const_reference back() const { assert(!empty()); return m_data[m_size-1]; }

  void push_back(const T& v) {
    reserve(m_size + 1);
    place_new(m_data + m_size, v);
    m_size++;
  }
  void pop_back() {
    assert(!empty());
    m_size--;
    destruct(m_data + m_size);
  }

  void clear() {
    for (size_t i = 0; i < m_size; i++) {
      destruct(m_data + i);
    }
    m_size = 0;
    free(m_data);
    m_data = NULL;
    m_capacity = 0;
  }

  void reserve(size_type cap) {
    if (m_capacity >= cap) return;
    // make sure we always at least double
    if (m_capacity*2 > cap)
      cap = m_capacity*2;
    resize_internal(cap);
  }

  void resize(size_type n) { resize(n, T()); }
  void resize(size_type n, const T& t) {
    reserve(n);
    while (m_size < n)
      push_back(t);
    while (m_size > n)
      pop_back();
  }

private:
  T *m_data;
  size_type m_capacity;
  size_type m_size;

  inline void destruct(T *obj) {
    obj->~T();
  }
  inline void place_new(T *loc, const T& v) {
    new (loc) T(v);
  }

  inline void resize_internal(size_type new_cap) {
    assert(new_cap >= m_capacity);
    // To be correct with ctors and dtors, we do not use realloc and friends.
    // A more efficient implementation would specialize for POD types and just
    // realloc() or something. Meh, if we care, we ought to use just STL's
    T *new_data = (T*) malloc(sizeof(T) * new_cap);
    assert(new_data);
    // Move over old data
    if (m_data) {
      for (size_type i = 0; i < m_size; i++) {
	place_new(new_data + i, m_data[i]);
	destruct(m_data + i);
      }
      free(m_data);
    }
    // And set the new values
    m_data = new_data;
    m_capacity = new_cap;
  }
};

#endif
