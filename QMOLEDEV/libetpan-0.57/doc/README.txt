libEtPan!

Viet Hoa DINH

   Copyright © 2003 DINH Viet Hoa
     __________________________________________________________________

   Table of Contents
   1. Introduction

        Description
        Author

              Main author
              Contributors

   2. Installation

        Dependencies

              Dependencies for users
              Dependencies for developers

        Existing packages

              FreeBSD
              Debian
              Mandrake Linux

        Compilation

              FreeBSD
              Mac OS X
              Linux
              configure
              Compile and install

   3. Use of libEtPan!

        How to open an Hotmail mailbox
     __________________________________________________________________

Chapter 1. Introduction

Description

   The purpose of this mail library is to provide a portable, efficient
   middleware for different kinds of mail access (IMAPrev4, POP3, NNTP,
   mbox, MH, Maildir).

   You have two kinds of mailbox access, either using low-level functions
   with a different interface for each kind of access or using
   higher-level functions, using a driver to wrap the higher-level API.
   The API will be the same for each kind of mail access using the
   higher-level API.
     __________________________________________________________________

Author

Main author

   DINH Viet Hoa <hoa@users.sourceforge.net>
     __________________________________________________________________

Contributors

     * Wim Delvaux
     * Melvin Hadasht
     * David Woodhouse
     * Juergen Graf
     * Zsolt VARGA
     * Gael Roualland
     * Toni Willberg
     * Rajko Albrecht
     * Nikita V. Youshchenko
     * Frederic Devernay
     * Michael Leupold
     * Colin Leroy
     __________________________________________________________________

Chapter 2. Installation

Dependencies

Dependencies for users

     * OpenSSL (optional but recommended)
     * Berkeley DB (optional but recommended)
     * POSIX Thread (required)
     __________________________________________________________________

Dependencies for developers

     * autoconf 2.13
     * automake 1.4
     * libtool 1.4.3
     * jade and some SGML tools will be required
     __________________________________________________________________

Existing packages

   Before you try to compile it, you have to know that packages exist for
   some systems.
     __________________________________________________________________

FreeBSD

   you can find it in ports/mail/libetpan.
     __________________________________________________________________

Debian

   This is in the package collection.
     __________________________________________________________________

Mandrake Linux

   This is in the package collection.
     __________________________________________________________________

Compilation

   Generic installation instructions are in the INSTALL file You can pass
   the following extra options to configure :
     __________________________________________________________________

FreeBSD

     * make sure libiconv is installed from the ports collection (see
       pkg_info).
     * issue configure with the following parameter:
$ ./configure --with-libiconv-prefix=/usr/local
     __________________________________________________________________

Mac OS X

     * in tests/option-parser.c, change the inclusion of getopt.h to
       gnugetopt/getopt.h
     * in tests/Makefile, add -I/sw/include for the CFLAGS and -L/sw/lib
       -lgnugetopt for the LDFLAGS.
     __________________________________________________________________

Linux

     *

                                      Warning
   Since libEtPan! is making high usage of mmap() even for writing, when
   your mailboxes are on NFS filesystem with a Linux server, it is advised
   to use option no_subtree_check in /etc/exports. This should avoid
   corruption of data.
   The problem exist in Linux 2.4.22 and earlier versions.
     * On RedHat systems, you have to configure using the following
       command line : ./configure --with-openssl=/usr/kerberos
     * On Debian systems, if the ./autogen script fails on missing
       AM_ICONV, you have to install gettext package.
     __________________________________________________________________

configure

   You can use the following options :

     * --enable-debug Compiles with debugging turned on
     * --enable-optim Turns on some optimizations flags for gcc
     * --without-openssl Disables OpenSSL (do not look for it)
     __________________________________________________________________

Compile and install

   Download the package and do the following :
$ tar xzvf libetpan-XX.XX.tar.gz      # to decompress the package

$ cd libetpan-XX.XX

$ ./configure --help                 # to get options of configure

$ ./configure                        # you can specify your own options

$ make                               # to compile the package

$ su

# make install

# logout
     __________________________________________________________________

Chapter 3. Use of libEtPan!

How to open an Hotmail mailbox

   If you wish to access hotmail using libEtPan!, you can, by using
   hotwayd. Then, create a POP3 storage with the given parameters :
   command as clear text for connection type (CONNECTION_TYPE_COMMAND),
   "/usr/bin/hotwayd" as command, plain text authentication
   (ePOP3_AUTH_TYPE_PLAIN), full hotmail address as login
   (foobar@hotmail.com or foobar@hotmail.com/mailbox_name if you want to
   access a specific mailbox) and give your password.
