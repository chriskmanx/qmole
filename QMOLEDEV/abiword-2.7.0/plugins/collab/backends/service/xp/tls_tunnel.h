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

#ifndef __TLS_TUNNEL_H__
#define __TLS_TUNNEL_H__

#include <boost/lexical_cast.hpp>
#include <boost/function.hpp>
#include <gnutls/gnutls.h>
#include <asio.hpp>
#include <boost/bind.hpp>
#include <string>
#include <vector>
#include <gcrypt.h>
#include <gnutls/gnutls.h>
#include <gnutls/x509.h>

namespace tls_tunnel {

typedef boost::shared_ptr<asio::ip::tcp::socket> socket_ptr_t;
typedef boost::shared_ptr<gnutls_session_t> session_ptr_t;
typedef boost::shared_ptr< std::vector<char> > buffer_ptr_t;

class Exception {
public:
	Exception(const std::string& message);
	const std::string& message() const;
private:
	std::string message_;
};

class Transport {
public:
	asio::io_service& io_service();
	void run();
	void stop();

protected:
	Transport();
	
private:
	asio::io_service io_service_;
	asio::io_service::work work_;
};

class ClientTransport : public Transport {
public:
	ClientTransport(const std::string& host, unsigned short port, boost::function<void (socket_ptr_t)> on_connect);
	void connect();
private:
	std::string host_;
	unsigned short port_;
	boost::function<void (socket_ptr_t)> on_connect_;
};

class ServerTransport : public Transport {
public:
	ServerTransport(const std::string& ip, unsigned short port, boost::function<void (socket_ptr_t)> on_connect);
	void accept();
private:
	void on_accept(const asio::error_code& error, socket_ptr_t socket_ptr);

	asio::ip::tcp::acceptor acceptor_;
	boost::function<void (socket_ptr_t)> on_connect_;
};

class Proxy {
public:
	virtual ~Proxy();
	static bool tls_tunnel_init();
	static void tls_tunnel_deinit();
	void run();
	void stop();
	virtual Transport& transport() = 0;
	
protected:
	Proxy(const std::string& ca_file);

	void on_local_read(const asio::error_code& error, std::size_t bytes_transferred,
			session_ptr_t session_ptr, socket_ptr_t local_socket_ptr, buffer_ptr_t local_buffer_ptr, socket_ptr_t remote_socket_ptr);
	void tunnel(session_ptr_t session_ptr, socket_ptr_t local_socket_ptr, socket_ptr_t remote_socket_ptr);
	void disconnect_(session_ptr_t session_ptr, socket_ptr_t local_socket_ptr, socket_ptr_t remote_socket_ptr);

	gnutls_certificate_credentials_t x509cred;	

private:
	void tunnel_(session_ptr_t session_ptr, socket_ptr_t local_socket_ptr, buffer_ptr_t local_buffer_ptr, socket_ptr_t remote_socket_ptr);
};

// FIXME: this clientproxy can only handle 1 SSL connection at the same time
class ClientProxy : public Proxy {
public:
	ClientProxy(const std::string& connect_address, unsigned short connect_port, 
			const std::string& ca_file, bool check_hostname);

	const std::string& local_address() const;
	unsigned short local_port() const;

private:
	Transport& transport();
	void on_transport_connect(socket_ptr_t remote_socket_ptr);
	void on_client_connect(const asio::error_code& error, 
			session_ptr_t session_ptr, socket_ptr_t local_socket_ptr, socket_ptr_t remote_socket_ptr);
	session_ptr_t setup_tls_session(socket_ptr_t remote_socket_ptr);

	ClientTransport transport_;
	std::string local_address_;
	unsigned short local_port_;
	std::string connect_address_;
	boost::shared_ptr<asio::ip::tcp::acceptor> acceptor_ptr;
	bool check_hostname_;
};

class ServerProxy : public Proxy {
public:
	ServerProxy(const std::string& bind_ip, unsigned short bind_port, unsigned short local_port,
			const std::string& ca_file, const std::string& cert_file, const std::string& key_file);
	
private:
	Transport& transport();
	void on_transport_connect(socket_ptr_t remote_socket_ptr);
	session_ptr_t setup_tls_session(socket_ptr_t remote_socket_ptr);
	
	ServerTransport transport_;
	unsigned short local_port_;
	gnutls_dh_params_t dh_params;
};

} /* namespace tls_tunnel */

#endif /* __TLS_TUNNEL_H__ */
