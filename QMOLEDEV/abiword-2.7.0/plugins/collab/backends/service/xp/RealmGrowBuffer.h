#ifndef __REALM_GROW_BUFFER__
#define __REALM_GROW_BUFFER__

namespace realm {

class GrowBuffer {
public:
	GrowBuffer(size_t capacity) 
		: m_capacity(capacity),
		m_buf(capacity, '\0'),
		m_index(0)
	{}
	
	GrowBuffer()
		: m_capacity(0),
		m_buf(),
		m_index(0)
	{}

	void clear() {
		if (m_buf.size() > m_capacity)
			m_buf.resize(m_capacity);
		m_index = 0;
	}

	char* data() {
		return &m_buf[0];
	}
	
	size_t size() {
		return m_index;
	}
	
	size_t free() {
		return m_buf.size() - m_index;
	}
	
	char* prepare(size_t capacity) {
		if (free() < capacity)
			m_buf.resize(m_buf.size() + (capacity - free()));
		return &m_buf[m_index];
	}

	void commit(size_t capacity) {
		if (m_index + capacity > m_buf.size())
			return; // TODO: throw exception
		m_index += capacity;
	}
	
private:
	size_t			m_capacity;
	std::string		m_buf;
	size_t			m_index;
};

}

#endif /* __REALM_GROW_BUFFER__ */
