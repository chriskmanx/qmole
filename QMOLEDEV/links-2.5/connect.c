/* connect.c
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#include "links.h"

/*
#define LOG_TRANSFER	"/tmp/log"
*/

#ifdef LOG_TRANSFER
static void log_data(unsigned char *data, int len)
{
	static int hlaseno = 0;
	int fd;
	if (!hlaseno) {
		printf("\n\033[1mWARNING -- LOGGING NETWORK TRANSFERS !!!\033[0m%c\n", 7);
		fflush(stdout);
		sleep(1);
		hlaseno = 1;
	}
	if ((fd = open(LOG_TRANSFER, O_WRONLY | O_APPEND | O_CREAT, 0600)) != -1) {
		set_bin(fd);
		write(fd, data, len);
		close(fd);
	}
}

#else
#define log_data(x, y)
#endif

static void connected(struct connection *);
static void dns_found(struct connection *, int);
static void handle_socks_reply(struct connection *);

static void exception(struct connection *c)
{
	setcstate(c, S_EXCEPT);
	retry_connection(c);
}

void close_socket(int *s)
{
	if (*s == -1) return;
	close(*s);
	set_handlers(*s, NULL, NULL, NULL, NULL);
	*s = -1;
}

struct conn_info {
	void (*func)(struct connection *);
	ip__address addr;
	int port;
	int *sock;
	int real_port;
	int socks_byte_count;
	unsigned char socks_reply[8];
	unsigned char dns_append[1];
};

void make_connection(struct connection *c, int port, int *sock, void (*func)(struct connection *))
{
	int real_port = -1;
	int as;
	unsigned char *dns_append = "";
	unsigned char *host;
	struct conn_info *b;
	if (*c->socks_proxy) {
		unsigned char *p = strchr(c->socks_proxy, '@');
		if (p) p++;
		else p = c->socks_proxy;
		host = stracpy(p);
		real_port = port;
		port = 1080;
		if ((p = strchr(host, ':'))) {
			*p++ = 0;
			if (!*p) goto badu;
			port = strtoul(p, (char **)(void *)&p, 10);
			if (*p) {
				badu:
				mem_free(host);
				setcstate(c, S_BAD_URL);
				abort_connection(c);
				return;
			}
		}
		dns_append = proxies.dns_append;
	} else if (!(host = get_host_name(c->url))) {
		setcstate(c, S_INTERNAL);
		abort_connection(c);
		return;
	}
	if (c->newconn)
		internal("already making a connection");
	b = mem_alloc(sizeof(struct conn_info) + strlen(dns_append));
	b->func = func;
	b->sock = sock;
	b->port = port;
	b->real_port = real_port;
	b->socks_byte_count = 0;
	strcpy(b->dns_append, dns_append);
	c->newconn = b;
	log_data("\nCONNECTION: ", 13);
	log_data(host, strlen(host));
	log_data("\n", 1);
	if (c->no_cache >= NC_RELOAD) as = find_host_no_cache(host, &b->addr, &c->dnsquery, (void(*)(void *, int))dns_found, c);
	else as = find_host(host, &b->addr, &c->dnsquery, (void(*)(void *, int))dns_found, c);
	mem_free(host);
	if (as) setcstate(c, S_DNS);
}

int get_pasv_socket(struct connection *c, int cc, int *sock, unsigned char *port)
{
	int s;
	struct sockaddr_in sa;
	struct sockaddr_in sb;
	socklen_t len = sizeof(sa);
	memset(&sa, 0, sizeof sa);
	memset(&sb, 0, sizeof sb);
	if (getsockname(cc, (struct sockaddr *)(void *)&sa, &len)) {
		e:
		setcstate(c, get_error_from_errno(errno));
		retry_connection(c);
		return -1;
	}
	if ((s = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1) goto e;
	*sock = s;
	fcntl(s, F_SETFL, O_NONBLOCK);
	memcpy(&sb, &sa, sizeof(struct sockaddr_in));
	sb.sin_port = 0;
	if (bind(s, (struct sockaddr *)(void *)&sb, sizeof sb)) goto e;
	len = sizeof(sa);
	if (getsockname(s, (struct sockaddr *)(void *)&sa, &len)) goto e;
	if (listen(s, 1)) goto e;
	memcpy(port, &sa.sin_addr.s_addr, 4);
	memcpy(port + 4, &sa.sin_port, 2);
	return 0;
}

#ifdef HAVE_SSL
static void ssl_want_read(struct connection *c)
{
	struct conn_info *b = c->newconn;

	set_timeout(c);

#ifndef HAVE_NSS
	if (c->no_tsl) c->ssl->options |= SSL_OP_NO_TLSv1;
#endif
	switch (SSL_get_error(c->ssl, SSL_connect(c->ssl))) {
		case SSL_ERROR_NONE:
			c->newconn = NULL;
			b->func(c);
			mem_free(b);
			break;
		case SSL_ERROR_WANT_READ:
			set_handlers(*b->sock, (void(*)(void *))ssl_want_read, NULL, (void(*)(void *))exception, c);
			break;
		case SSL_ERROR_WANT_WRITE:
			set_handlers(*b->sock, NULL, (void(*)(void *))ssl_want_read, (void(*)(void *))exception, c);
			break;
		default:
			c->no_tsl++;
			setcstate(c, S_SSL_ERROR);
			retry_connection(c);
			break;
	}
}
#endif

static void handle_socks(struct connection *c)
{
	struct conn_info *b = c->newconn;
	unsigned char *command = init_str();
	int len = 0;
	unsigned char *host;
	int wr;
	setcstate(c, S_SOCKS_NEG);
	set_timeout(c);
	add_bytes_to_str(&command, &len, "\004\001", 2);
	add_chr_to_str(&command, &len, b->real_port >> 8);
	add_chr_to_str(&command, &len, b->real_port);
	add_bytes_to_str(&command, &len, "\000\000\000\001", 4);
	if (strchr(c->socks_proxy, '@'))
		add_bytes_to_str(&command, &len, c->socks_proxy, strcspn(c->socks_proxy, "@"));
	add_chr_to_str(&command, &len, 0);
	if (!(host = get_host_name(c->url))) {
		mem_free(command);
		setcstate(c, S_INTERNAL);
		abort_connection(c);
		return;
	}
	add_to_str(&command, &len, host);
	add_to_str(&command, &len, b->dns_append);
	add_chr_to_str(&command, &len, 0);
	mem_free(host);
	if (b->socks_byte_count >= len) {
		mem_free(command);
		setcstate(c, S_MODIFIED);
		retry_connection(c);
		return;
	}
	wr = write(*b->sock, command + b->socks_byte_count, len - b->socks_byte_count);
	mem_free(command);
	if (wr <= 0) {
		setcstate(c, wr ? get_error_from_errno(errno) : S_CANT_WRITE);
		retry_connection(c);
		return;
	}
	b->socks_byte_count += wr;
	if (b->socks_byte_count < len) {
		set_handlers(*b->sock, NULL, (void(*)(void *))handle_socks, (void(*)(void *))exception, c);
		return;
	} else {
		b->socks_byte_count = 0;
		set_handlers(*b->sock, (void(*)(void *))handle_socks_reply, NULL, (void(*)(void *))exception, c);
		return;
	}
}

static void handle_socks_reply(struct connection *c)
{
	struct conn_info *b = c->newconn;
	int rd;
	set_timeout(c);
	rd = read(*b->sock, b->socks_reply + b->socks_byte_count, sizeof b->socks_reply - b->socks_byte_count);
	if (rd <= 0) {
		setcstate(c, rd ? get_error_from_errno(errno) : S_CANT_READ);
		retry_connection(c);
		return;
	}
	b->socks_byte_count += rd;
	if (b->socks_byte_count < (int)sizeof b->socks_reply) return;
	/* debug("%x %x %x %x %x %x %x %x", b->socks_reply[0], b->socks_reply[1], b->socks_reply[2], b->socks_reply[3], b->socks_reply[4], b->socks_reply[5], b->socks_reply[6], b->socks_reply[7]); */
	if (b->socks_reply[0]) {
		setcstate(c, S_BAD_SOCKS_VERSION);
		abort_connection(c);
		return;
	}
	switch (b->socks_reply[1]) {
		case 91:
			setcstate(c, S_SOCKS_REJECTED);
			retry_connection(c);
			return;
		case 92:
			setcstate(c, S_SOCKS_NO_IDENTD);
			abort_connection(c);
			return;
		case 93:
			setcstate(c, S_SOCKS_BAD_USERID);
			abort_connection(c);
			return;
		default:
			setcstate(c, S_SOCKS_UNKNOWN_ERROR);
			retry_connection(c);
			return;
		case 90:
			break;
	}
	b->real_port = -1;
	connected(c);
}

static void dns_found(struct connection *c, int state)
{
	int s;
	struct conn_info *b = c->newconn;
	struct sockaddr_in sa;
	if (state) {
		setcstate(c, S_NO_DNS);
		abort_connection(c);
		return;
	}
	if ((s = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) == -1) {
		setcstate(c, get_error_from_errno(errno));
		retry_connection(c);
		return;
	}
	*b->sock = s;
	fcntl(s, F_SETFL, O_NONBLOCK);
	memset(&sa, 0, sizeof(struct sockaddr_in));
	sa.sin_family = AF_INET;
	sa.sin_addr.s_addr = b->addr;
	sa.sin_port = htons(b->port);
	if (connect(s, (struct sockaddr *)(void *)&sa, sizeof sa)) {
		if (errno != EALREADY && errno != EINPROGRESS) {
#ifdef BEOS
			if (errno == EWOULDBLOCK) errno = ETIMEDOUT;
#endif
			setcstate(c, get_error_from_errno(errno));
			retry_connection(c);
			return;
		}
		set_handlers(s, NULL, (void(*)(void *))connected, (void(*)(void *))exception, c);
		setcstate(c, S_CONN);
	} else {
		connected(c);
	}
}

static void connected(struct connection *c)
{
	struct conn_info *b = c->newconn;
	int err = 0;
	socklen_t len = sizeof(int);
	if (getsockopt(*b->sock, SOL_SOCKET, SO_ERROR, (void *)&err, &len))
		if (!(err = errno)) {
			err = -(S_STATE);
			goto bla;
		}
	if (err >= 10000) err -= 10000;	/* Why does EMX return so large values? */
	if (err > 0) {
		bla:
		setcstate(c, get_error_from_errno(err));
		retry_connection(c);
		return;
	}
	set_timeout(c);
	if (b->real_port != -1) {
		handle_socks(c);
		return;
	}
#ifdef HAVE_SSL
	if (c->ssl) {
		c->ssl = getSSL();
		if (!c->ssl) {
			goto ssl_error;
		}
		SSL_set_fd(c->ssl, *b->sock);
#ifndef HAVE_NSS
		if (c->no_tsl) c->ssl->options |= SSL_OP_NO_TLSv1;
#endif
		switch (SSL_get_error(c->ssl, SSL_connect(c->ssl))) {
			case SSL_ERROR_WANT_READ:
				setcstate(c, S_SSL_NEG);
				set_handlers(*b->sock, (void(*)(void *))ssl_want_read, NULL, (void(*)(void *))exception, c);
				return;
			case SSL_ERROR_WANT_WRITE:
				setcstate(c, S_SSL_NEG);
				set_handlers(*b->sock, NULL, (void(*)(void *))ssl_want_read, (void(*)(void *))exception, c);
				return;
			case SSL_ERROR_NONE:
				break;
			default:
			ssl_error:
				c->no_tsl++;
				setcstate(c, S_SSL_ERROR);
				retry_connection(c);
				return;
		}
	}
#endif
	c->newconn = NULL;
	b->func(c);
	mem_free(b);
}

struct write_buffer {
	int sock;
	int len;
	int pos;
	void (*done)(struct connection *);
	unsigned char data[1];
};

static void write_select(struct connection *c)
{
	struct write_buffer *wb;
	int wr;
	if (!(wb = c->buffer)) {
		internal("write socket has no buffer");
		setcstate(c, S_INTERNAL);
		abort_connection(c);
		return;
	}
	set_timeout(c);
	/*printf("ws: %d\n",wb->len-wb->pos);
	for (wr = wb->pos; wr < wb->len; wr++) printf("%c", wb->data[wr]);
	printf("-\n");*/

#ifdef HAVE_SSL
	if(c->ssl) {
		if ((wr = SSL_write(c->ssl, wb->data + wb->pos, wb->len - wb->pos)) <= 0) {
			int err;
			if ((err = SSL_get_error(c->ssl, wr)) != SSL_ERROR_WANT_WRITE) {
				setcstate(c, wr ? (err == SSL_ERROR_SYSCALL ? get_error_from_errno(errno) : S_SSL_ERROR) : S_CANT_WRITE);
				if (!wr || err == SSL_ERROR_SYSCALL) retry_connection(c);
				else abort_connection(c);
				return;
			}
			else return;
		}
	} else
#endif
		if ((wr = write(wb->sock, wb->data + wb->pos, wb->len - wb->pos)) <= 0) {
#ifdef ATHEOS
	/* Workaround for a bug in Syllable */
			if (wr && errno == EAGAIN) {
				return;
			}
#endif
			setcstate(c, wr ? get_error_from_errno(errno) : S_CANT_WRITE);
			retry_connection(c);
			return;
		}

	/*printf("wr: %d\n", wr);*/
	if ((wb->pos += wr) == wb->len) {
		void (*f)(struct connection *) = wb->done;
		c->buffer = NULL;
		set_handlers(wb->sock, NULL, NULL, NULL, NULL);
		mem_free(wb);
		f(c);
	}
}

void write_to_socket(struct connection *c, int s, unsigned char *data, int len, void (*write_func)(struct connection *))
{
	struct write_buffer *wb;
	log_data(data, len);
	if ((unsigned)len > MAXINT - sizeof(struct write_buffer)) overalloc();
	wb = mem_alloc(sizeof(struct write_buffer) + len);
	wb->sock = s;
	wb->len = len;
	wb->pos = 0;
	wb->done = write_func;
	memcpy(wb->data, data, len);
	if (c->buffer) mem_free(c->buffer);
	c->buffer = wb;
	set_handlers(s, NULL, (void (*)(void *))write_select, (void (*)(void *))exception, c);
}

#define READ_SIZE	64240

static void read_select(struct connection *c)
{
	struct read_buffer *rb;
	int rd;
	if (!(rb = c->buffer)) {
		internal("read socket has no buffer");
		setcstate(c, S_INTERNAL);
		abort_connection(c);
		return;
	}
	set_handlers(rb->sock, NULL, NULL, NULL, NULL);
	if ((unsigned)rb->len > MAXINT - sizeof(struct read_buffer) - READ_SIZE) overalloc();
	rb = mem_realloc(rb, sizeof(struct read_buffer) + rb->len + READ_SIZE);
	c->buffer = rb;

#ifdef HAVE_SSL
	if(c->ssl) {
		if ((rd = SSL_read(c->ssl, rb->data + rb->len, READ_SIZE)) <= 0) {
			int err;
			if ((err = SSL_get_error(c->ssl, rd)) == SSL_ERROR_WANT_READ) {
				read_from_socket(c, rb->sock, rb, rb->done);
				return;
			}
			if (rb->close && !rd) {
				rb->close = 2;
				rb->done(c, rb);
				return;
			}
			setcstate(c, rd ? (err == SSL_ERROR_SYSCALL ? get_error_from_errno(errno) : S_SSL_ERROR) : S_CANT_READ);
			/*mem_free(rb);*/
			if (!rd || err == SSL_ERROR_SYSCALL) retry_connection(c);
			else abort_connection(c);
			return;
		}
	} else
#endif
		if ((rd = read(rb->sock, rb->data + rb->len, READ_SIZE)) <= 0) {
			if (rb->close && !rd) {
				rb->close = 2;
				rb->done(c, rb);
				return;
			}
			if (!rd) {
/* Many servers supporting compression have a bug
   --- they send the size of uncompressed data.
   Turn off compression support once before the final retry.
*/
				unsigned char *prot, *h;
				if (is_last_try(c) && (prot = get_protocol_name(c->url))) {
					if (!strcasecmp(prot, "http")) {
						if ((h = get_host_name(c->url))) {
							add_blacklist_entry(h, BL_NO_COMPRESSION);
							mem_free(h);
						}
					}
					mem_free(prot);
				}
			}
			setcstate(c, rd ? get_error_from_errno(errno) : S_CANT_READ);
			/*mem_free(rb);*/
			retry_connection(c);
			return;
		}
	log_data(rb->data + rb->len, rd);
	rb->len += rd;
	rb->done(c, rb);
}

struct read_buffer *alloc_read_buffer(struct connection *c)
{
	struct read_buffer *rb;
	rb = mem_alloc(sizeof(struct read_buffer) + READ_SIZE);
	memset(rb, 0, sizeof(struct read_buffer));
	return rb;
}

void read_from_socket(struct connection *c, int s, struct read_buffer *buf, void (*read_func)(struct connection *, struct read_buffer *))
{
	buf->done = read_func;
	buf->sock = s;
	if (c->buffer && buf != c->buffer) mem_free(c->buffer);
	c->buffer = buf;
	set_handlers(s, (void (*)(void *))read_select, NULL, (void (*)(void *))exception, c);
}

void kill_buffer_data(struct read_buffer *rb, int n)
{
	if (n > rb->len || n < 0) {
		internal("called kill_buffer_data with bad value");
		rb->len = 0;
		return;
	}
	memmove(rb->data, rb->data + n, rb->len - n);
	rb->len -= n;
}
