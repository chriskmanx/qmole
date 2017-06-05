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


#ifndef __SOA__
#define __SOA__

#include <stdint.h>
#include <boost/lexical_cast.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include <vector>
#include "soa_types.h"
#include "soa_result.h"

namespace soa {

class function_arg {
public:
	function_arg(const std::string& name, Type type)
		: name_(name),
		type_(type)
	{}
	
	virtual ~function_arg() {}

	const std::string& name() const {
		return name_;
	}

	Type type() const {
		return type_;
	}

	// FIXME: returning an std::string is inefficient for large blocks of 
	// data; we should make it a boost::shared_ptr<std::string>
	virtual std::string str() const = 0;

private:
	std::string name_;
	Type type_;
};

class function_arg_string : public function_arg {
public:
	function_arg_string(const std::string& name, const std::string& value)
		: function_arg(name, STRING_TYPE),
		value_(value)
	{}

	virtual std::string str() const {
		return value_;
	}
	
private:
	std::string value_;
};

class function_arg_int : public function_arg {
public:
	function_arg_int(const std::string& name, int64_t value)
		: function_arg(name, INT_TYPE),
		value_(value)
	{}

	virtual std::string str() const {
		try {
			return boost::lexical_cast<std::string>(value_);
		} catch (boost::bad_lexical_cast &) {
			return "0";
		}
	}
	
private:
	int64_t value_;
};

class function_arg_bool : public function_arg {
public:
	function_arg_bool(const std::string& name, bool value)
		: function_arg(name, BOOL_TYPE),
		value_(value)
	{}

	virtual std::string str() const {
		return value_ ? "true" : "false";
	}
	
private:
	bool value_;
};

class function_arg_base64bin : public function_arg {
public:
	function_arg_base64bin(Base64Bin value)
		: function_arg(value.name(), BASE64BIN_TYPE),
		value_(value)
	{}

	virtual std::string str() const {
		return value_.value();
	}
	
private:
	Base64Bin value_;
};

class function_call {
public:
	function_call() {}

	function_call(const std::string& request, const std::string& response)
		: request_(request),
		response_(response)
	{}

	function_call& operator()(std::string name, const char* value) {
		args.push_back(boost::shared_ptr<function_arg>(new function_arg_string(name, value)));
		return *this;
	}

	function_call& operator()(std::string name, std::string value) {
		args.push_back(boost::shared_ptr<function_arg>(new function_arg_string(name, value)));
		return *this;
	}

	function_call& operator()(std::string name, int64_t value) {
		args.push_back(boost::shared_ptr<function_arg>(new function_arg_int(name, value)));
		return *this;
	}

	function_call& operator()(std::string name, bool value) {
		args.push_back(boost::shared_ptr<function_arg>(new function_arg_bool(name, value)));
		return *this;
	}

	function_call& operator()(Base64Bin value) {
		args.push_back(boost::shared_ptr<function_arg>(new function_arg_base64bin(value)));
		return *this;
	}

	const std::string& request() const {
		return request_;
	}

	const std::string& response() const {
		return response_;
	}

	void add_arg(boost::shared_ptr<function_arg> arg) {
		args.push_back(arg);
	}

	std::string str() const {
		std::string ret;
		// TODO: XML escape args/values
		for (std::vector< boost::shared_ptr<function_arg> >::const_iterator cit = args.begin(); cit != args.end(); cit++) {
			const function_arg& arg = **cit;
			ret += "<" + arg.name() + " " + soap_type(arg.type()) + ">" + arg.str() + "</" + arg.name() + ">\n";
		}
		return ret;
	}

private:
	std::string request_;
	std::string response_;
	std::vector< boost::shared_ptr<function_arg> > args;
};

class header {
public:
	std::string str() const {
		return "";
	}
};

class body {
public:
	body(const std::string& ns_ref_)
		: ns_ref(ns_ref_)
	{}

	body(const std::string& ns_ref_, const function_call& fc)
		: ns_ref(ns_ref_),
		fc_(fc)
	{}

	const function_call& function() const {
		return fc_;
	}

	void set_function(function_call fc) {
		fc_ = fc;
	}

	std::string str() const {
		return "<SOAP-ENV:Body>\n" \
			"<" + ns_ref + ":" + fc_.request() + ">\n" +
			fc_.str() + 
			"</" + ns_ref + ":" + fc_.request() + ">\n" \
			"</SOAP-ENV:Body>\n";
	}

private:
	std::string ns_ref;
	function_call fc_;	
};

class method_invocation {
public:
	method_invocation(const std::string& custom_ns)
		: custom_ns_(custom_ns),
		custom_ns_ref_("nsref"),
		body_(custom_ns_ref_)
	{}

	method_invocation(const std::string& custom_ns, function_call fc)
		: custom_ns_(custom_ns),
		custom_ns_ref_("nsref"),
		body_(custom_ns_ref_, fc)
	{}

	const function_call& function() const {
		return body_.function();
	}

	std::string str() const {
		return "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" \
			"<SOAP-ENV:Envelope " + 
			default_namespaces() +
			" xmlns:" + custom_ns_ref_ + "=\"" + custom_ns_ + "\"" +
			" " + encoding_style() + ">\n" + 
			header_.str() +
			body_.str() +
			"</SOAP-ENV:Envelope>";
	}

private:
	std::string default_namespaces() const {
		return "xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"" \
			" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"" \
			" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"" \
			" xmlns:SOAP-ENC=\"http://schemas.xmlsoap.org/soap/encoding/\"";
	}

	std::string encoding_style() const {
		return "SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"";
	}

	std::string input_name_;
	std::string output_name_;
	std::string custom_ns_;
	std::string custom_ns_ref_;
	header header_;
	body body_;
};
}

#endif /* __SOA__ */
