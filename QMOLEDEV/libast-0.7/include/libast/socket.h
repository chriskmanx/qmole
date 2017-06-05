/*
 * Copyright (C) 1997-2004, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _LIBAST_SOCKET_H_
#define _LIBAST_SOCKET_H_

/* Cast an arbitrary object pointer to a socket. */
#define SPIF_SOCKET(o)                      (SPIF_CAST(socket) (o))
#define SPIF_SOCKET_SOCKFD(o)               (SPIF_SOCKET(o)->fd)
#define SPIF_SOCKET_SOCKADDR(o)             (SPIF_SOCKET(o)->addr)
#define SPIF_SOCKET_SOCKADDR_IP(o)          (SPIF_CAST(ipsockaddr) (SPIF_SOCKET(o)->addr))

/* Check to see if a pointer references an socket. */
#define SPIF_OBJ_IS_SOCKET(o)               (SPIF_OBJ_IS_TYPE(o, socket))

/* Used for testing the NULL-ness of sockets. */
#define SPIF_SOCKET_ISNULL(o)               (SPIF_SOCKET(o) == SPIF_NULL_TYPE(socket))

/* Calls to the basic functions. */
#define SPIF_SOCKET_NEW()                   SPIF_CAST(socket) (SPIF_CLASS(SPIF_CLASS_VAR(socket)))->(noo)()
#define SPIF_SOCKET_INIT(o)                 SPIF_OBJ_INIT(o)
#define SPIF_SOCKET_DONE(o)                 SPIF_OBJ_DONE(o)
#define SPIF_SOCKET_DEL(o)                  SPIF_OBJ_DEL(o)
#define SPIF_SOCKET_SHOW(o, b, i)           SPIF_OBJ_SHOW(o, b, i)
#define SPIF_SOCKET_COMP(o1, o2)            SPIF_OBJ_COMP(o)
#define SPIF_SOCKET_DUP(o)                  SPIF_OBJ_DUP(o)
#define SPIF_SOCKET_TYPE(o)                 SPIF_OBJ_TYPE(o)

/* Socket flags */
#define SPIF_SOCKET_FLAGS_FAMILY_INET       (1UL << 0)
#define SPIF_SOCKET_FLAGS_FAMILY_UNIX       (1UL << 1)
#define SPIF_SOCKET_FLAGS_FAMILY            (0x0f << 0)
#define SPIF_SOCKET_FLAGS_TYPE_STREAM       (1UL << 4)
#define SPIF_SOCKET_FLAGS_TYPE_DGRAM        (1UL << 5)
#define SPIF_SOCKET_FLAGS_TYPE_RAW          (1UL << 6)
#define SPIF_SOCKET_FLAGS_TYPE              (0x0f << 4)
#define SPIF_SOCKET_FLAGS_LISTEN            (1UL << 8)
#define SPIF_SOCKET_FLAGS_OPEN              (1UL << 9)
#define SPIF_SOCKET_FLAGS_CONNECTED         (1UL << 10)
#define SPIF_SOCKET_FLAGS_HAVE_INPUT        (1UL << 11)
#define SPIF_SOCKET_FLAGS_CAN_OUTPUT        (1UL << 12)
#define SPIF_SOCKET_FLAGS_NBIO              (1UL << 13)
#define SPIF_SOCKET_FLAGS_IOSTATE           (0xff << 8)
#define SPIF_SOCKET_FLAGS(s)                (SPIF_SOCKET(s)->flags)
#define SPIF_SOCKET_FLAGS_SET(s, b)         do {SPIF_SOCKET_FLAGS(s) |= (b);} while (0)
#define SPIF_SOCKET_FLAGS_CLEAR(s, b)       do {SPIF_SOCKET_FLAGS(s) &= ~(b);} while (0)
#define SPIF_SOCKET_FLAGS_IS_SET(s, b)      (SPIF_SOCKET_FLAGS(s) & (b))

#include <libast/url.h>

SPIF_DECL_OBJ(socket) {
    SPIF_DECL_PARENT_TYPE(obj);
    spif_sockfd_t fd;
    spif_sockfamily_t fam;
    spif_socktype_t type;
    spif_sockproto_t proto;
    spif_sockaddr_t addr;
    spif_sockaddr_len_t len;
    spif_uint32_t flags;
    spif_url_t local_url, remote_url;
};

extern spif_class_t SPIF_CLASS_VAR(socket);
extern spif_socket_t spif_socket_new(void);
extern spif_socket_t spif_socket_new_from_urls(spif_url_t, spif_url_t);
extern spif_bool_t spif_socket_del(spif_socket_t);
extern spif_bool_t spif_socket_init(spif_socket_t);
extern spif_bool_t spif_socket_init_from_urls(spif_socket_t, spif_url_t, spif_url_t);
extern spif_bool_t spif_socket_done(spif_socket_t);
extern spif_str_t spif_socket_show(spif_socket_t, spif_charptr_t, spif_str_t, size_t);
extern spif_cmp_t spif_socket_comp(spif_socket_t, spif_socket_t);
extern spif_socket_t spif_socket_dup(spif_socket_t);
extern spif_classname_t spif_socket_type(spif_socket_t);
extern spif_bool_t spif_socket_open(spif_socket_t);
extern spif_bool_t spif_socket_close(spif_socket_t);
extern spif_bool_t spif_socket_check_io(spif_socket_t);
extern spif_socket_t spif_socket_accept(spif_socket_t);
extern spif_bool_t spif_socket_send(spif_socket_t, spif_str_t);
extern spif_str_t spif_socket_recv(spif_socket_t);
extern spif_bool_t spif_socket_set_nbio(spif_socket_t);
extern spif_bool_t spif_socket_clear_nbio(spif_socket_t);

#endif /* _LIBAST_SOCKET_H_ */
