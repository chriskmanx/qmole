
SUBDIRS = unix xp

noinst_LTLIBRARIES = libservice.la

libservice_la_LIBADD = unix/libunix.la xp/libxp.la

libservice_la_SOURCES = 

certdir=$(ABIWORD_DATADIR)/certs
cert_DATA = \
        cacert.pem

EXTRA_DIST = \
        $(cert_DATA)
