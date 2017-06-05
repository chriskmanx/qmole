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

#include <strings.h>
#include <boost/shared_ptr.hpp>
#include <boost/lexical_cast.hpp>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include "soa_result.h"

namespace soa {

	static Type element_type(char* type) {
		if (!type)
			return COLLECTION_TYPE; // FIXME: well, just say it's a generic type for now

		const std::string& type_(type);
		if (type_ == "xsd:string")
			return STRING_TYPE;
		if (type_ == "xsd:int")
			return INT_TYPE;
		if (type_ == "xsd:boolean")
			return BOOL_TYPE;
		if (type_ == "xsd:base64Binary")
			return BASE64BIN_TYPE;
		if (type_ == "xsd:QName")
			return QNAME_TYPE;		
		if (type_ == "SOAP-ENC:Array")
			return ARRAY_TYPE;

		return COLLECTION_TYPE; // FIXME: well, just say it's a generic type for now
	}

	std::string soap_type(Type type) {
		switch (type) {
			case STRING_TYPE:
				return "xsi:type=\"xsd:string\"";
			case INT_TYPE:
				return "xsi:type=\"xsd:int\"";
			case BOOL_TYPE:
				return "xsi:type=\"xsd:boolean\"";
			case BASE64BIN_TYPE:
				return "xsi:type=\"xsd:base64Binary\"";
			case QNAME_TYPE:
				return "xsi:type=\"xsd:QName\"";				
			default:
				// FIXME: should throw exception
				return "";
		}
	}

	static bool arg_value(const std::string& val, bool& b) {
		if (val == "true" || val == "1") {
			b = true;
			return true;
		}
		if (val == "false" || val == "0") {
			b = false;
			return true;
		}
		return false;
	}

	static bool arg_value(const std::string& val, int64_t& i) {
		try {
			i = boost::lexical_cast<int64_t>(val);
		} catch (boost::bad_lexical_cast &) {
			return false;
		}
		return true;
	}

	class XmlDocDeleter {
	public:
		    void operator()(xmlDocPtr* doc) {
		           // printf("Destructing xml doc ptr!\n");
		            if (!doc || !*doc)
		                    return;
		            xmlFreeDoc(*doc);
		    }
	};

	static GenericPtr parse_elements(xmlNode* element, GenericPtr parent) {
		if (!element || element->type != XML_ELEMENT_NODE)
			return parent;

		if (parent && !parent->complex()) {
			//printf("ERROR: ADDING CHILD TO NON-COMPLEX TYPE!\n");
			return GenericPtr();
		}

		// parse this node

		char* type_str = reinterpret_cast<char *>(xmlGetProp(element, reinterpret_cast<const xmlChar*>("type")));
		Type elem_type = element_type(type_str);
		free(type_str);

		GenericPtr t;
		switch (elem_type) {
			case ARRAY_TYPE:
				t.reset(new Array<GenericPtr>(reinterpret_cast<const char*>(element->name)));
				break;
			case COLLECTION_TYPE:
				t.reset(new Collection(reinterpret_cast<const char*>(element->name)));
				break;
			case STRING_TYPE:
				{
					xmlChar* value = xmlNodeGetContent(element);
					t.reset(new String(reinterpret_cast<const char*>(element->name), reinterpret_cast<const char*>(value)));
					xmlFree(value);
				}
				break;
			case INT_TYPE:
				{
					xmlChar* value = xmlNodeGetContent(element);
					int64_t i = 0;
					if (arg_value(reinterpret_cast<const char*>(value), i))
						t.reset(new Int(reinterpret_cast<const char*>(element->name), i));
					xmlFree(value);
				}
				break;
			case BOOL_TYPE:
				{
					xmlChar* value = xmlNodeGetContent(element);
					bool b = false;
					if (arg_value(reinterpret_cast<const char*>(value), b))
						t.reset(new Bool(reinterpret_cast<const char*>(element->name), b));
					xmlFree(value);
				}
				break;
			case BASE64BIN_TYPE:
				{
					xmlChar* value = xmlNodeGetContent(element);
					t.reset(new Base64Bin(
										reinterpret_cast<const char*>(element->name), 
										boost::shared_ptr<std::string>(new std::string(reinterpret_cast<const char*>(value)))
									));
					xmlFree(value);
				}
				break;
			case QNAME_TYPE:
				{
					xmlChar* value = xmlNodeGetContent(element);
					t.reset(new QName(reinterpret_cast<const char*>(element->name), reinterpret_cast<const char*>(value)));
					xmlFree(value);
				}
				break;			
		}
		if (!t) {
			return GenericPtr();
		}

		// parse the child nodes
		if (t->complex()) {
			for (xmlNode* child = element->children; child; child = child->next) {
				parse_elements(child, t);
			}
		}

		// we have no parent; return what we found
		if (!parent) {
			return t;
		}
		
		// attach the new element to the parent ellement
		// FIXME: generalize
		switch (parent->type()) {
			case ARRAY_TYPE:
				{
					ArrayPtr a = parent->as< Array<GenericPtr> >();
					a->add(t);
				}
				break;
			case COLLECTION_TYPE:
				{
					CollectionPtr c = parent->as<Collection>();
					c->add(t);
				}
				break;
			default:
				// should not happen;
				break;			
		}

		return parent;
	}

	GenericPtr parse_response(const std::string& response, const std::string& method_name) {

		// process the soap request
		xmlDocPtr doc = xmlReadMemory(&response[0], response.size(), "noname.xml", NULL, 0);
		if (!doc) {
			return GenericPtr();
		}

		boost::shared_ptr<xmlDocPtr> reader(&doc, XmlDocDeleter());
		xmlNode* rootNode = xmlDocGetRootElement(*reader);
		if (!rootNode) {
			return GenericPtr();
		}

		if (strcasecmp(reinterpret_cast<const char*>(rootNode->name), "Envelope") != 0) {
			return GenericPtr();
		}

		for (xmlNode* soapNode = rootNode->children; soapNode; soapNode = soapNode->next) {
			if (soapNode->type != XML_ELEMENT_NODE)
				continue;

			if (strcasecmp(reinterpret_cast<const char*>(soapNode->name), "Body") == 0) {

				for (xmlNode* methodNode = soapNode->children; methodNode; methodNode = methodNode->next) {
					if (methodNode->type != XML_ELEMENT_NODE)
						continue;

					if (strcasecmp(reinterpret_cast<const char*>(methodNode->name), "Fault") == 0) {
						GenericPtr soap_tree = parse_elements(methodNode, GenericPtr());
						if (!soap_tree)
							throw SoapFault();
						
						soa::CollectionPtr fault = boost::dynamic_pointer_cast<soa::Collection>(soap_tree);
						if (!fault)
							throw SoapFault();
						
						throw SoapFault(fault->get<soa::QName>("faultcode"), 
										fault->get<soa::String>("faultstring"),
										fault->get<soa::String>("detail"));
					}
					
					if (method_name != reinterpret_cast<const char*>(methodNode->name))
						continue;

					for (xmlNode* child = methodNode->children; child; child = child->next) {
						GenericPtr soap_tree = parse_elements(child, GenericPtr());
						if (soap_tree) // FIXME: are we right in assuming there is only 1 response element?
							return soap_tree;
					}

					return GenericPtr();
				}
			} else if (strcasecmp(reinterpret_cast<const char*>(soapNode->name), "Header") == 0) {
				return GenericPtr();
			}
			else {
				return GenericPtr();
			}
		}
		return GenericPtr();
	}

}

