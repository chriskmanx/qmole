/*
 * Copyright (C) 2000, 2001, 2002, 2003, 2004, 2006, 2008, 2009, 2010,
 * 2011 Free Software Foundation, Inc.
 *
 * This file is part of LIBTASN1.
 *
 * The LIBTASN1 library is free software; you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA
 */

#ifndef _ELEMENT_H
#define _ELEMENT_H


asn1_retCode _asn1_append_sequence_set (ASN1_TYPE node);

asn1_retCode _asn1_convert_integer (const char *value,
				    unsigned char *value_out,
				    int value_out_size, int *len);

void _asn1_hierarchical_name (ASN1_TYPE node, char *name, int name_size);

#endif
