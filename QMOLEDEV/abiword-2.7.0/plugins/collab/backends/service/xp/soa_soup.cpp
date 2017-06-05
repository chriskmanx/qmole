/* Copyright (c) 2008-2009, AbiSource Corporation B.V.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of AbiSource Corporation B.V. nor the
 *       names of other contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY ABISOURCE CORPORATION B.V. AND OTHER 
 * CONTRIBUTORS ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, 
 * BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL ABISOURCE 
 * CORPORATION B.V OR OTHER CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, 
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <string>
#include <glib.h>
#include <libsoup/soup.h>
#include <boost/lexical_cast.hpp>
#include <soa_soup.h>
#include "ut_assert.h"

namespace soup_soa {

	/* public types */
	
	struct SoaSoupSession
	{
		SoaSoupSession(SoupMessage* msg, const std::string& ssl_ca_file)
			: m_session(NULL),
			m_msg(msg),
			progress_cb_ptr(),
			received_content_length(0)
		{
			_set_session(ssl_ca_file);
		}
	
		SoaSoupSession(SoupMessage* msg, const std::string& ssl_ca_file, boost::function<void (SoupSession*, SoupMessage*, uint32_t)> progress_cb_)
			: m_session(NULL),
			m_msg(msg),
			progress_cb_ptr(new boost::function<void (SoupSession*, SoupMessage*, uint32_t)>(progress_cb_)),
			received_content_length(0)
		{
			_set_session(ssl_ca_file);
		}
		
		~SoaSoupSession() {
			if (m_session)
				g_object_unref(m_session);
			if (m_msg)
				g_object_unref(m_msg);
		}
		
		void callback(uint32_t progress) {
			if (!progress_cb_ptr)
				return;
			(*progress_cb_ptr)(m_session, m_msg, progress);
		}
		
		SoupSession* m_session;
		SoupMessage* m_msg;
		boost::shared_ptr<boost::function<void (SoupSession*, SoupMessage*, uint32_t)> > progress_cb_ptr;
		uint32_t received_content_length;
		
		private:
			void _set_session(const std::string& ssl_ca_file) {
				m_session = 
					ssl_ca_file.size() == 0
						? soup_session_sync_new()
						: soup_session_sync_new_with_options(				
											SOUP_SESSION_SSL_CA_FILE, ssl_ca_file.c_str(),
											/* TODO: add user agent */
											NULL
									);			
			}

	};

	/* private function prototypes */

#ifdef SOUP24	
	static void _got_chunk_cb(SoupMessage* msg, SoupBuffer *chunk, SoaSoupSession* progress_info);
#else
	static void _got_chunk_cb(SoupMessage *msg, SoaSoupSession* user_data);
#endif
	static soa::GenericPtr _invoke(const std::string& url, const soa::method_invocation& mi, SoaSoupSession& sess);
	
	/* public functions */
	
	soa::GenericPtr invoke(const std::string& url, const soa::method_invocation& mi, const std::string& ssl_ca_file) {
		std::string soap_msg = mi.str();
		SoupMessage* msg = soup_message_new ("POST", url.c_str());
#ifdef SOUP24
		soup_message_set_request(msg, "text/xml", SOUP_MEMORY_STATIC, &soap_msg[0], soap_msg.size());
#else
		soup_message_set_request(msg, "text/xml", SOUP_BUFFER_USER_OWNED, &soap_msg[0], soap_msg.size());
#endif
		SoaSoupSession sess(msg, ssl_ca_file);
		return _invoke(url, mi, sess);
	}
	
	soa::GenericPtr invoke(const std::string& url, const soa::method_invocation& mi, const std::string& ssl_ca_file,
						   boost::function<void (SoupSession*, SoupMessage*, uint32_t)> progress_cb) {
		std::string soap_msg = mi.str();
		SoupMessage* msg = soup_message_new ("POST", url.c_str());
		SoaSoupSession sess(msg, ssl_ca_file, progress_cb);
		g_signal_connect(G_OBJECT (msg), "got-chunk", G_CALLBACK(_got_chunk_cb), (gpointer)(&sess));
#ifdef SOUP24
		soup_message_set_request(msg, "text/xml", SOUP_MEMORY_STATIC, &soap_msg[0], soap_msg.size());
#else
		soup_message_set_request(msg, "text/xml", SOUP_BUFFER_USER_OWNED, &soap_msg[0], soap_msg.size());
#endif
		return _invoke(url, mi, sess);
	}	
	
	/* private functions */
	
	static soa::GenericPtr _invoke(const std::string& /*url*/, const soa::method_invocation& mi, SoaSoupSession& sess) {
		if (!sess.m_session || !sess.m_msg )
			return soa::GenericPtr();

		guint status = soup_session_send_message (sess.m_session, sess.m_msg);
		if (!(SOUP_STATUS_IS_SUCCESSFUL (status) ||
			status == SOUP_STATUS_INTERNAL_SERVER_ERROR /* used for SOAP Faults */))
		{
			return soa::GenericPtr();
		}
		
		// store the SOAP result in a string
		// FIXME: ineffecient copy
		std::string result;
#ifdef SOUP24
		if (!sess.m_msg->response_body || !sess.m_msg->response_body->data)
			return soa::GenericPtr();
		result.resize(sess.m_msg->response_body->length);
		std::copy(sess.m_msg->response_body->data, sess.m_msg->response_body->data+sess.m_msg->response_body->length, result.begin());
#else		
		result.resize(sess.m_msg->response.length);
		std::copy(sess.m_msg->response.body, sess.m_msg->response.body+sess.m_msg->response.length, result.begin());
#endif	
		return soa::parse_response(result, mi.function().response());
	}

#ifdef SOUP24
	static void _got_chunk_cb(SoupMessage* msg, SoupBuffer * /*chunk*/, SoaSoupSession* progress_info)
#else
	static void _got_chunk_cb(SoupMessage* msg, SoaSoupSession* progress_info)
#endif
	{
		if (!msg || !msg->response_headers || !progress_info)
			return;
		
		uint32_t content_length = 0;
#ifdef SOUP24
		content_length = (uint32_t)soup_message_headers_get_content_length(msg->response_headers);
#else
		const char* content_length_str = soup_message_get_header(msg->response_headers, "Content-Length");
		if (!content_length_str)
			return;

		try
		{
			content_length = boost::lexical_cast<uint32_t>(content_length_str);
		}
		catch (boost::bad_lexical_cast &)
		{
       			UT_ASSERT_HARMLESS(UT_SHOULD_NOT_HAPPEN); // unless the server is really broken
			return;
		}
#endif

		if (content_length == 0)
			return;
		
#ifdef SOUP24
		if (!msg->response_body)
			return;
		progress_info->received_content_length = msg->response_body->length;
#else
		progress_info->received_content_length += msg->response.length;
#endif
		uint32_t progress = (uint32_t)(((float)progress_info->received_content_length / content_length)*100);
		if (progress > 100)
			progress = 100;
		progress_info->callback(progress);
	}

}

