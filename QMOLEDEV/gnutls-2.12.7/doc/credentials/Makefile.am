## Process this file with automake to produce Makefile.in
# Copyright (C) 2007, 2010 Free Software Foundation, Inc.
#
# Author: Simon Josefsson
#
# This file is part of GnuTLS.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

SUBDIRS = openpgp srp x509

EXTRA_DIST = gnutls-http-serv 

EXTRA_DIST += ca.tmpl client.tmpl proxy.tmpl server.tmpl

EXTRA_DIST += params.pem

EXTRA_DIST += srp-passwd.txt  srp-tpasswd.conf

EXTRA_DIST += psk-passwd.txt

EXTRA_DIST += openpgp-server.bin openpgp-server-key.bin	\
	openpgp-server-key.txt openpgp-server.txt

EXTRA_DIST += x509-client.p12

EXTRA_DIST += x509-ca.pem x509-client.pem x509-proxy-key.pem	\
	x509-server-dsa.pem x509-server-key.pem x509-trust.pem	\
	x509-ca-key.pem x509-client-key.pem x509-other-ca.pem	\
	x509-proxy.pem x509-server-key-dsa.pem x509-server.pem

EXTRA_DIST += x509-ca.crt x509-client.crt x509-proxy.crt	\
	x509-server.crt x509-server-dsa.crt
