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

#ifndef __SERVICE_ERRORCODES__
#define __SERVICE_ERRORCODES__

#include "soa.h"

namespace abicollab {
namespace service {

enum SOAP_ERROR
{
	// generic error (reserved)
	SOAP_ERROR_OK = 0x00,
	SOAP_ERROR_GENERIC = 0x01,
	
	// exception soap errors
	SOAP_ERROR_GENERIC_EXCEPTION = 0x101,
	SOAP_ERROR_DATABASE_EXCEPTION = 0x102,
	
	// abicollab soap errors
	SOAP_ERROR_INVALID_PASSWORD = 0x201,
	SOAP_ERROR_INVALID_EMAIL = 0x202
	// TODO: add more errors
	
	// saveDocument soap errors
	// TODO: add more errors
		
	// openDocument soap errors
	// TODO: add more errors
};

SOAP_ERROR error(const soa::SoapFault& fault);

}
}

#endif /* __SERVICE_ERRORCODES__ */
