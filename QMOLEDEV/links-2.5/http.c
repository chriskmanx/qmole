/* http.c
 * HTTP protocol client implementation
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#include "links.h"

struct http_connection_info {
	int bl_flags;
	int http10;
	int close;
	off_t length;
	int version;
	int chunk_remaining;
};

/* prototypes */
static void http_send_header(struct connection *);
static void http_get_header(struct connection *);
static void add_user_agent(unsigned char **hdr, int *l);
static void add_referer(unsigned char **hdr, int *l, unsigned char *url, unsigned char *prev_url);
static void add_accept(unsigned char **hdr, int *l);
static void add_accept_encoding(unsigned char **hdr, int *l, unsigned char *url, struct http_connection_info *info);
static void add_accept_charset(unsigned char **hdr, int *l, struct http_connection_info *info);
static void add_accept_language(unsigned char **hdr, int *l, struct http_connection_info *info);
static void add_connection(unsigned char **hdr, int *l, int http10, int proxy, int post);
static void add_if_modified(unsigned char **hdr, int *l, struct connection *c);
static void add_range(unsigned char **hdr, int *l, struct connection *c);
static void add_pragma_no_cache(unsigned char **hdr, int *l, int no_cache);
static void add_auth_string(unsigned char **hdr, int *l, unsigned char *url);
static void add_post_header(unsigned char **hdr, int *l, unsigned char **post);
static void add_extra_options(unsigned char **hdr, int *l);


/* Returns a string pointer with value of the item.
 * The string must be destroyed after usage with mem_free.
 */
unsigned char *parse_http_header(unsigned char *head, unsigned char *item, unsigned char **ptr)
{
	unsigned char *i, *f, *g, *h;
	if (!head) return NULL;
	for (f = head; *f; f++) {
		if (*f != 10) continue;
		f++;
		for (i = item; *i && *f; i++, f++)
			if (upcase(*i) != upcase(*f)) goto cont;
		if (!*f) break;
		if (f[0] == ':') {
			while (f[1] == ' ') f++;
			for (g = ++f; *g >= ' '; g++)
				;
			while (g > f && g[-1] == ' ') g--;
			h = mem_alloc(g - f + 1);
			memcpy(h, f, g - f);
			h[g - f] = 0;
			if (ptr) {
				*ptr = f;
			}
			return h;
		}
		cont:;
		f--;
	}
	return NULL;
}

unsigned char *parse_header_param(unsigned char *x, unsigned char *e, int all)
{
	unsigned char u;
	size_t le = strlen(e);
	int lp;
	unsigned char *y = x;
	if (!all) {
		a:
		if (!(y = strchr(y, ';'))) return NULL;
	}
	while (*y && (*y == ';' || *y <= ' ')) y++;
	if (strlen(y) < le) return NULL;
	if (casecmp(y, e, le)) goto a;
	y += le;
	while (*y && (*y <= ' ' || *y == '=')) y++;
	u = ';';
	if (*y == '\'' || *y == '"') u = *y++;
	lp = 0;
	while (y[lp] >= ' ' && y[lp] != u) {
		lp++;
		if (lp == MAXINT) overalloc();
	}
	return memacpy(y, lp);
}

int get_http_code(unsigned char *head, int *code, int *version)
{
	if (!head) return -1;
	while (head[0] == ' ') head++;
	if (upcase(head[0]) != 'H' || upcase(head[1]) != 'T' || upcase(head[2]) != 'T' ||
	    upcase(head[3]) != 'P') return -1;
	if (head[4] == '/' && head[5] >= '0' && head[5] <= '9'
	 && head[6] == '.' && head[7] >= '0' && head[7] <= '9' && head[8] <= ' ') {
		if (version) *version = (head[5] - '0') * 10 + head[7] - '0';
	} else if (version) *version = 0;
	for (head += 4; *head > ' '; head++)
		;
	if (*head++ != ' ') return -1;
	if (head[0] < '1' || head [0] > '9' || head[1] < '0' || head[1] > '9' ||
	    head[2] < '0' || head [2] > '9') {
		if (code) *code = 200;
		return 0;
	}
	if (code) *code = (head[0]-'0')*100 + (head[1]-'0')*10 + head[2]-'0';
	return 0;
}

static struct {
	unsigned char *name;
	int bugs;
} buggy_servers[] = {
	{ "mod_czech/3.1.0", BL_HTTP10 },
	{ "Purveyor", BL_HTTP10 },
	{ "Netscape-Enterprise", BL_HTTP10 | BL_NO_ACCEPT_LANGUAGE },
	{ "Apache Coyote", BL_HTTP10 },
	{ "lighttpd", BL_HTTP10 },
	{ "FORPSI", BL_NO_RANGE },
	{ "Sausalito", BL_HTTP10 },
	{ NULL, 0 }
};

static int check_http_server_bugs(unsigned char *url, struct http_connection_info *info, unsigned char *head)
{
	unsigned char *server;
	int i, bugs;
	if (!http_options.allow_blacklist || info->http10) return 0;
	if (!(server = parse_http_header(head, "Server", NULL))) return 0;
	bugs = 0;
	for (i = 0; buggy_servers[i].name; i++) if (strstr(server, buggy_servers[i].name)) bugs |= buggy_servers[i].bugs;
	mem_free(server);
	if (bugs && (server = get_host_name(url))) {
		add_blacklist_entry(server, bugs);
		mem_free(server);
		return bugs & ~BL_NO_RANGE;
	}
	return 0;	
}

static void http_end_request(struct connection *c, int notrunc)
{
	if (c->state == S_OK) {
		if (c->cache) {
			if (!notrunc) truncate_entry(c->cache, c->from, 1);
			c->cache->incomplete = 0;
		}
	}
	if (c->info && !((struct http_connection_info *)c->info)->close 
#ifdef HAVE_SSL
	&& (!c->ssl) /* We won't keep alive ssl connections */
#endif
	&& (!http_options.bug_post_no_keepalive || !strchr(c->url, POST_CHAR))) {
		add_keepalive_socket(c, HTTP_KEEPALIVE_TIMEOUT);
	} else abort_connection(c);
}

void http_func(struct connection *c)
{
	/*setcstate(c, S_CONN);*/
	/*set_timeout(c);*/
	if (get_keepalive_socket(c)) {
		int p;
		if ((p = get_port(c->url)) == -1) {
			setcstate(c, S_INTERNAL);
			abort_connection(c);
			return;
		}
		make_connection(c, p, &c->sock1, http_send_header);
	} else http_send_header(c);
}

void proxy_func(struct connection *c)
{
	http_func(c);
}

static void add_url_to_str(unsigned char **str, int *l, unsigned char *url)
{
	unsigned char *sp;
	for (sp = url; *sp && *sp != POST_CHAR; sp++) {
		if (*sp <= ' ') {
			unsigned char esc[4];
			sprintf(esc, "%%%02X", (int)*sp);
			add_to_str(str, l, esc);
		} else {
			add_chr_to_str(str, l, *sp);
		}
	}
}

static void http_send_header(struct connection *c)
{
	struct http_connection_info *info;
	int http10 = http_options.http10;
	unsigned char *hdr;
	unsigned char *h, *u;
	unsigned char *u2;
	int l = 0;
	unsigned char *post;
	unsigned char *host;

	if (!find_in_cache(c->url, &c->cache))
		c->cache->refcount--;

	host = upcase(c->url[0]) != 'P' ? c->url : get_url_data(c->url);
	set_timeout(c);
	info = mem_calloc(sizeof(struct http_connection_info));
	c->info = info;
	if ((h = get_host_name(host))) {
		info->bl_flags = get_blacklist_flags(h);
		mem_free(h);
	}
	if (info->bl_flags & BL_HTTP10) http10 = 1;
	info->http10 = http10;
	post = strchr(host, POST_CHAR);
	if (post) post++;
	hdr = init_str();
	if (!post) add_to_str(&hdr, &l, "GET ");
	else {
		add_to_str(&hdr, &l, "POST ");
		c->unrestartable = 2;
	}
	if (upcase(c->url[0]) != 'P') add_to_str(&hdr, &l, "/");
	if (!(u = get_url_data(c->url))) {
		http_bad_url:
		mem_free(hdr);
		setcstate(c, S_BAD_URL);
		http_end_request(c, 0);
		return;
	}
	if (post && post < u) {
		goto http_bad_url;
	}
	u2 = u;
	if (upcase(c->url[0]) == 'P' && !*c->socks_proxy && *proxies.dns_append) {
		unsigned char *u_host;
		int u_host_len;
		int u2_len = 0;
		if (parse_url(u, NULL, NULL, NULL, NULL, NULL, &u_host, &u_host_len, NULL, NULL, NULL, NULL, NULL)) goto http_bad_url;
		u2 = init_str();
		add_bytes_to_str(&u2, &u2_len, u, u_host + u_host_len - u);
		add_to_str(&u2, &u2_len, proxies.dns_append);
		add_to_str(&u2, &u2_len, u_host + u_host_len);
	}
	add_url_to_str(&hdr, &l, u2);
	if (u2 != u) mem_free(u2);
	if (!http10) add_to_str(&hdr, &l, " HTTP/1.1\r\n");
	else add_to_str(&hdr, &l, " HTTP/1.0\r\n");
	if ((h = get_host_name(host))) {
		add_to_str(&hdr, &l, "Host: ");
		add_to_str(&hdr, &l, h);
		mem_free(h);
		if ((h = get_port_str(host))) {
			add_to_str(&hdr, &l, ":");
			add_to_str(&hdr, &l, h);
			mem_free(h);
		}
		add_to_str(&hdr, &l, "\r\n");
	}
	add_user_agent(&hdr, &l);
	add_referer(&hdr, &l, host, c->prev_url);
	add_accept(&hdr, &l);
	add_accept_encoding(&hdr, &l, host, info);
	add_accept_charset(&hdr, &l, info);
	add_accept_language(&hdr, &l, info);
	add_connection(&hdr, &l, http10, upcase(c->url[0]) == 'P', !!post);
	add_if_modified(&hdr, &l, c);
	add_range(&hdr, &l, c);
	add_pragma_no_cache(&hdr, &l, c->no_cache);
	add_auth_string(&hdr, &l, c->url);
	add_post_header(&hdr, &l, &post);
	add_cookies(&hdr, &l, host);
	add_extra_options(&hdr, &l);
	add_to_str(&hdr, &l, "\r\n");
	if (post) {
		while (post[0] && post[1]) {
			int h1, h2;
			h1 = post[0] <= '9' ? (unsigned)post[0] - '0' : post[0] >= 'A' ? upcase(post[0]) - 'A' + 10 : 0;
			if (h1 < 0 || h1 >= 16) h1 = 0;
			h2 = post[1] <= '9' ? (unsigned)post[1] - '0' : post[1] >= 'A' ? upcase(post[1]) - 'A' + 10 : 0;
			if (h2 < 0 || h2 >= 16) h2 = 0;
			add_chr_to_str(&hdr, &l, h1 * 16 + h2);
			post += 2;
		}
	}
	write_to_socket(c, c->sock1, hdr, l, http_get_header);
	mem_free(hdr);
	setcstate(c, S_SENT);
}

static void add_user_agent(unsigned char **hdr, int *l)
{
	add_to_str(hdr, l, "User-Agent: ");
	if (!(*http_options.header.fake_useragent)) {
		add_to_str(hdr, l, "Links (" VERSION_STRING "; ");
		add_to_str(hdr, l, system_name);
		add_to_str(hdr, l, "; ");
		add_to_str(hdr, l, compiler_name);
		add_to_str(hdr, l, "; ");
		if (!F && !list_empty(terminals)) {
			add_to_str(hdr, l, "text");
		}
#ifdef G
		else if (F && drv) {
			add_to_str(hdr, l, drv->name);
		}
#endif
		else {
			add_to_str(hdr, l, "dump");
		}
		add_to_str(hdr, l, ")\r\n");
	} else {
		add_to_str(hdr, l, http_options.header.fake_useragent);
		add_to_str(hdr, l, "\r\n");
	}
}

static void add_referer(unsigned char **hdr, int *l, unsigned char *url, unsigned char *prev_url)
{
	switch (http_options.header.referer)
	{
		case REFERER_FAKE:
		add_to_str(hdr, l, "Referer: ");
		add_to_str(hdr, l, http_options.header.fake_referer);
		add_to_str(hdr, l, "\r\n");
		break;
		
		case REFERER_SAME_URL:
		add_to_str(hdr, l, "Referer: ");
		add_url_to_str(hdr, l, url);
		add_to_str(hdr, l, "\r\n");
		break;

		case REFERER_REAL_SAME_SERVER:
		{
			unsigned char *h, *j;
			int brk = 1;
			if ((h = get_host_name(url))) {
				if ((j = get_host_name(prev_url))) {
					if (!strcasecmp(h, j)) brk = 0;
					mem_free(j);
				}
				mem_free(h);
			}
			if (brk) break;
			/* fall through */
		}
		case REFERER_REAL:
		{
			unsigned char *ref;
			unsigned char *user, *ins;
			int ulen;
			if (!prev_url) break;   /* no referrer */

			ref = stracpy(prev_url);
			if (!parse_url(ref, NULL, &user, &ulen, NULL, NULL, &ins, NULL, NULL, NULL, NULL, NULL, NULL) && ulen && ins) {
				memmove(user, ins, strlen(ins) + 1);
			}
			add_to_str(hdr, l, "Referer: ");
			add_url_to_str(hdr, l, ref);
			add_to_str(hdr, l, "\r\n");
			mem_free(ref);
		}
		break;
	}
}

static void add_accept(unsigned char **hdr, int *l)
{
	add_to_str(hdr, l, "Accept: */*\r\n");
}

static void add_accept_encoding(unsigned char **hdr, int *l, unsigned char *url, struct http_connection_info *info)
{
#if defined(HAVE_ZLIB) || defined(HAVE_BZIP2) || defined(HAVE_LZMA)
	if (!http_options.no_compression && !(info->bl_flags & BL_NO_COMPRESSION)) {
		int orig_l = *l;
		int l1;
		unsigned char *extd = strrchr(url, '.');
		if (extd && get_compress_by_extension(extd + 1, strchr(extd + 1, 0)))
			goto skip_compress;
		add_to_str(hdr, l, "Accept-Encoding: ");
		l1 = *l;
#if defined(HAVE_ZLIB)
		if (*l != l1) add_chr_to_str(hdr, l, ',');
		add_to_str(hdr, l, "gzip,deflate");
#endif
#if defined(HAVE_BZIP2)
		if (!(info->bl_flags & BL_NO_BZIP2)) {
			if (*l != l1) add_chr_to_str(hdr, l, ',');
			add_to_str(hdr, l, "bzip2");
		}
#endif
#if defined(HAVE_LZMA)
		if (!(info->bl_flags & BL_NO_BZIP2)) {
			if (*l != l1) add_chr_to_str(hdr, l, ',');
			add_to_str(hdr, l, "lzma,lzma2");
		}
#endif
		if (*l != l1) add_to_str(hdr, l, "\r\n");
		else *l = orig_l;
		skip_compress:;
	}
#endif
}

static void add_accept_charset(unsigned char **hdr, int *l, struct http_connection_info *info)
{
	static unsigned char *accept_charset = NULL;
	if (!accept_charset) {
		int i;
		unsigned char *cs, *ac;
		int aclen = 0;
		ac = init_str();
		for (i = 0; (cs = get_cp_mime_name(i)); i++) {
			if (aclen) add_to_str(&ac, &aclen, ",");
			else add_to_str(&ac, &aclen, "Accept-Charset: ");
			add_to_str(&ac, &aclen, cs);
		}
		if (aclen) add_to_str(&ac, &aclen, "\r\n");
		retry:
		if (!(accept_charset = malloc(strlen(ac) + 1))) {
			if (out_of_memory(NULL, 0))
				goto retry;
			mem_free(ac);
			return;
		}
		strcpy(accept_charset, ac);
		mem_free(ac);
	}
	if (!(info->bl_flags & BL_NO_CHARSET) && !http_options.no_accept_charset) add_to_str(hdr, l, accept_charset);
}

static void add_accept_language(unsigned char **hdr, int *l, struct http_connection_info *info)
{
	if (!(info->bl_flags & BL_NO_ACCEPT_LANGUAGE)) {
		int la;
		add_to_str(hdr, l, "Accept-Language: ");
		la = *l;
		add_to_str(hdr, l, _(TEXT_(T__ACCEPT_LANGUAGE), NULL));
		add_to_str(hdr, l, ",");
		if (!strstr(*hdr + la, "en,") && !strstr(*hdr + la, "en;")) add_to_str(hdr, l, "en;q=0.2,");
		add_to_str(hdr, l, "*;q=0.1\r\n");
	}
}

static void add_connection(unsigned char **hdr, int *l, int http10, int proxy, int post)
{
	if (!http10) {
		if (!proxy) add_to_str(hdr, l, "Connection: ");
		else add_to_str(hdr, l, "Proxy-Connection: ");
		if (!post || !http_options.bug_post_no_keepalive) add_to_str(hdr, l, "keep-alive\r\n");
		else add_to_str(hdr, l, "close\r\n");
	}
}

static void add_if_modified(unsigned char **hdr, int *l, struct connection *c)
{
	struct cache_entry *e;
	if ((e = c->cache)) {
		int code = 0, vers;	/* against warning */
		if (get_http_code(e->head, &code, &vers) || code >= 400) goto skip_ifmod;
		if (!e->incomplete && e->head && c->no_cache <= NC_IF_MOD) {
			unsigned char *m;
			if (e->last_modified) m = stracpy(e->last_modified);
			else if ((m = parse_http_header(e->head, "Date", NULL)))
				;
			else if ((m = parse_http_header(e->head, "Expires", NULL)))
				;
			else goto skip_ifmod;
			add_to_str(hdr, l, "If-Modified-Since: ");
			add_to_str(hdr, l, m);
			add_to_str(hdr, l, "\r\n");
			mem_free(m);
		}
		skip_ifmod:;
	}
}

static void add_range(unsigned char **hdr, int *l, struct connection *c)
{
	struct cache_entry *e;
	struct http_connection_info *info = c->info;
	if ((e = c->cache)) {
		int code = 0, vers;	/* against warning */
		if (get_http_code(e->head, &code, &vers) || code >= 400) goto skip_range;
	}
	if (c->from && (c->est_length == -1 || c->from < c->est_length) && c->no_cache < NC_IF_MOD && !(info->bl_flags & BL_NO_RANGE)) {
/* If the cached entity is compressed and we turned off compression,
   request the whole file */
		if ((info->bl_flags & BL_NO_COMPRESSION || http_options.no_compression) && e) {
			unsigned char *d;
			if ((d = parse_http_header(e->head, "Transfer-Encoding", NULL))) {
				mem_free(d);
				goto skip_range;
			}
		}
		add_to_str(hdr, l, "Range: bytes=");
		add_num_to_str(hdr, l, c->from);
		add_to_str(hdr, l, "-\r\n");
		skip_range:;
	}
}

static void add_pragma_no_cache(unsigned char **hdr, int *l, int no_cache)
{
	if (no_cache >= NC_PR_NO_CACHE) add_to_str(hdr, l, "Pragma: no-cache\r\nCache-Control: no-cache\r\n");
}

static void add_auth_string(unsigned char **hdr, int *l, unsigned char *url)
{
	unsigned char *h;
	if ((h = get_auth_string(url))) {
		add_to_str(hdr, l, h);
		mem_free(h);
	}
}

static void add_post_header(unsigned char **hdr, int *l, unsigned char **post)
{
	if (*post) {
		unsigned char *pd = strchr(*post, '\n');
		if (pd) {
			add_to_str(hdr, l, "Content-Type: ");
			add_bytes_to_str(hdr, l, *post, pd - *post);
			add_to_str(hdr, l, "\r\n");
			*post = pd + 1;
		}
		add_to_str(hdr, l, "Content-Length: ");
		add_num_to_str(hdr, l, strlen(*post) / 2);
		add_to_str(hdr, l, "\r\n");
	}
}

static void add_extra_options(unsigned char **hdr, int *l)
{
	unsigned char *p = http_options.header.extra_header;
	while (1) {
		unsigned char *q = p + strcspn(p, "\\");
		if (p != q) {
			unsigned char *c;
			unsigned char *s = memacpy(p, q - p);
			c = strchr(s, ':');
			if (c && casecmp(s, "Cookie:", 7)) {
				unsigned char *v = NULL; /* against warning */
				unsigned char *cc = memacpy(s, c - s);
				unsigned char *x = parse_http_header(*hdr, cc, &v);
				mem_free(cc);
				if (x) {
					unsigned char *new_hdr;
					int new_l;
					mem_free(x);
					new_hdr = init_str();
					new_l = 0;
					add_bytes_to_str(&new_hdr, &new_l, *hdr, v - *hdr);
					while (*++c == ' ')
						;
					add_to_str(&new_hdr, &new_l, c);
					add_to_str(&new_hdr, &new_l, v + strcspn(v, "\r\n"));
					mem_free(*hdr);
					*hdr = new_hdr;
					*l = new_l;
					goto already_added;
				}
			}
			add_to_str(hdr, l, s);
			add_to_str(hdr, l, "\r\n");
			already_added:
			mem_free(s);
		}
		if (!*q) break;
		p = q + 1;
	}
}

static int is_line_in_buffer(struct read_buffer *rb)
{
	int l;
	for (l = 0; l < rb->len; l++) {
		if (rb->data[l] == 10) return l + 1;
		if (l < rb->len - 1 && rb->data[l] == 13 && rb->data[l + 1] == 10) return l + 2;
		if (l == rb->len - 1 && rb->data[l] == 13) return 0;
		if (rb->data[l] < ' ') return -1;
	}
	return 0;
}

static void read_http_data(struct connection *c, struct read_buffer *rb)
{
	struct http_connection_info *info = c->info;
	set_timeout(c);
	if (rb->close == 2) {
		setcstate(c, S_OK);
		http_end_request(c, 0);
		return;
	}
	if (info->length != -2) {
		int l = rb->len;
		if (info->length >= 0 && info->length < l) l = info->length;
		if ((off_t)(0UL + c->from + l) < 0) {
			setcstate(c, S_LARGE_FILE);
			abort_connection(c);
			return;
		}
		c->received += l;
		if (add_fragment(c->cache, c->from, rb->data, l) == 1) c->tries = 0;
		if (info->length >= 0) info->length -= l;
		c->from += l;
		kill_buffer_data(rb, l);
		if (!info->length && !rb->close) {
			setcstate(c, S_OK);
			http_end_request(c, 0);
			return;
		}
	} else {
		next_chunk:
		if (info->chunk_remaining == -2) {
			int l;
			if ((l = is_line_in_buffer(rb))) {
				if (l == -1) {
					setcstate(c, S_HTTP_ERROR);
					abort_connection(c);
					return;
				}
				kill_buffer_data(rb, l);
				if (l <= 2) {
					setcstate(c, S_OK);
					http_end_request(c, 0);
					return;
				}
				goto next_chunk;
			}
		} else if (info->chunk_remaining == -1) {
			int l;
			if ((l = is_line_in_buffer(rb))) {
				unsigned char *de;
				long n = 0;	/* warning, go away */
				if (l != -1) n = strtol(rb->data, (char **)(void *)&de, 16);
				if (l == -1 || n < 0 || n >= MAXINT || de == rb->data) {
					setcstate(c, S_HTTP_ERROR);
					abort_connection(c);
					return;
				}
				kill_buffer_data(rb, l);
				if (!(info->chunk_remaining = n)) info->chunk_remaining = -2;
				goto next_chunk;
			}
		} else {
			int l = info->chunk_remaining;
			if (l > rb->len) l = rb->len;
			if ((off_t)(0UL + c->from + l) < 0) {
				setcstate(c, S_LARGE_FILE);
				abort_connection(c);
				return;
			}
			c->received += l;
			if (add_fragment(c->cache, c->from, rb->data, l) == 1) c->tries = 0;
			info->chunk_remaining -= l;
			c->from += l;
			kill_buffer_data(rb, l);
			if (!info->chunk_remaining && rb->len >= 1) {
				if (rb->data[0] == 10) kill_buffer_data(rb, 1);
				else {
					if (rb->data[0] != 13 || (rb->len >= 2 && rb->data[1] != 10)) {
						setcstate(c, S_HTTP_ERROR);
						abort_connection(c);
						return;
					}
					if (rb->len < 2) goto read_more;
					kill_buffer_data(rb, 2);
				}
				info->chunk_remaining = -1;
				goto next_chunk;
			}
		}
				
	}
	read_more:
	read_from_socket(c, c->sock1, rb, read_http_data);
	setcstate(c, S_TRANS);
}

static int get_header(struct read_buffer *rb)
{
	int i;
	if (rb->len <= 0) return 0;
	if (rb->data[0] != 'H') return -2;
	if (rb->len <= 1) return 0;
	if (rb->data[1] != 'T') return -2;
	if (rb->len <= 2) return 0;
	if (rb->data[2] != 'T') return -2;
	if (rb->len <= 3) return 0;
	if (rb->data[3] != 'P') return -2;
	for (i = 0; i < rb->len; i++) {
		unsigned char a = rb->data[i];
		if (/*a < ' ' && a != 10 && a != 13*/!a) return -1;
		if (i < rb->len - 1 && a == 10 && rb->data[i + 1] == 10) return i + 2;
		if (i < rb->len - 3 && a == 13) {
			if (rb->data[i + 1] != 10) return -1;
			if (rb->data[i + 2] == 13) {
				if (rb->data[i + 3] != 10) return -1;
				return i + 4;
			}
		}
	}
	return 0;
}

static void http_got_header(struct connection *c, struct read_buffer *rb)
{
	off_t cf;
	int state = c->state != S_PROC ? S_GETH : S_PROC;
	unsigned char *head;
	unsigned char *cookie, *ch;
	int a, h = 0, version = 0;	/* against warning */
	unsigned char *d;
	struct cache_entry *e;
	struct http_connection_info *info;
	unsigned char *host = upcase(c->url[0]) != 'P' ? c->url : get_url_data(c->url);
	set_timeout(c);
	info = c->info;
	if (rb->close == 2) {
		unsigned char *h;
		if (!c->tries && (h = get_host_name(host))) {
			if (info->bl_flags & BL_NO_CHARSET) {
				del_blacklist_entry(h, BL_NO_CHARSET);
			} else {
				add_blacklist_entry(h, BL_NO_CHARSET);
				c->tries = -1;
			}
			mem_free(h);
		}
		setcstate(c, S_CANT_READ);
		retry_connection(c);
		return;
	}
	rb->close = 0;
	again:
	if ((a = get_header(rb)) == -1) {
		setcstate(c, S_HTTP_ERROR);
		abort_connection(c);
		return;
	}
	if (!a) {
		read_from_socket(c, c->sock1, rb, http_got_header);
		setcstate(c, state);
		return;
	}
	if (a != -2) {
		head = mem_alloc(a + 1);
		memcpy(head, rb->data, a); head[a] = 0;
		kill_buffer_data(rb, a);
	} else {
		head = stracpy("HTTP/0.9 200 OK\r\nContent-Type: text/html\r\n\r\n");
	}
	if (get_http_code(head, &h, &version) || h == 101) {
		mem_free(head);
		setcstate(c, S_HTTP_ERROR);
		abort_connection(c);
		return;
	}
	if (check_http_server_bugs(host, c->info, head) && is_connection_restartable(c)) {
		mem_free(head);
		setcstate(c, S_RESTART);
		retry_connection(c);
		return;
	}
	ch = head;
	while ((cookie = parse_http_header(ch, "Set-Cookie", &ch))) {
		unsigned char *host = upcase(c->url[0]) != 'P' ? c->url : get_url_data(c->url);
		set_cookie(NULL, host, cookie);
		mem_free(cookie);
	}
	if (h == 100) {
		mem_free(head);
		state = S_PROC;
		goto again;
	}
	if (h < 200) {
		mem_free(head);
		setcstate(c, S_HTTP_ERROR);
		abort_connection(c);
		return;
	}
	if (h == 204) {
		mem_free(head);
		setcstate(c, S_HTTP_204);
		http_end_request(c, 0);
		return;
	}
	if (h == 304) {
		mem_free(head);
		setcstate(c, S_OK);
		http_end_request(c, 1);
		return;
	}
	if ((h == 500 || h == 502 || h == 503 || h == 504) && http_options.retry_internal_errors && is_connection_restartable(c)) {
			/* !!! FIXME: wait some time ... */
		if (is_last_try(c)) {
			unsigned char *h;
			if ((h = get_host_name(host))) {
				add_blacklist_entry(h, BL_NO_BZIP2);
				mem_free(h);
			}
		}
		mem_free(head);
		setcstate(c, S_RESTART);
		retry_connection(c);
		return;
	}
	if (!c->cache) {
		if (get_cache_entry(c->url, &c->cache)) {
			mem_free(head);
			setcstate(c, S_OUT_OF_MEM);
			abort_connection(c);
			return;
		}
		c->cache->refcount--;
	}
	e = c->cache;
	e->http_code = h;
	if (e->head) mem_free(e->head);
	e->head = head;
	if ((d = parse_http_header(head, "Expires", NULL))) {
		time_t t = parse_http_date(d);
		if (t && e->expire_time != 1) e->expire_time = t;
		mem_free(d);
	}
	if ((d = parse_http_header(head, "Pragma", NULL))) {
		if (!casecmp(d, "no-cache", 8)) e->expire_time = 1;
		mem_free(d);
	}
	if ((d = parse_http_header(head, "Cache-Control", NULL))) {
		unsigned char *f = d;
		while (1) {
			while (*f && (*f == ' ' || *f == ',')) f++;
			if (!*f) break;
			if (!casecmp(f, "no-cache", 8) || !casecmp(f, "must-revalidate", 15)) {
				e->expire_time = 1;
			}
			if (!casecmp(f, "max-age=", 8)) {
				if (e->expire_time != 1) e->expire_time = time(NULL) + atoi(f + 8);
			}
			while (*f && *f != ',') f++;
		}
		mem_free(d);
	}
#ifdef HAVE_SSL
	if (c->ssl) {
		int l = 0;
		if (e->ssl_info) mem_free(e->ssl_info);
		e->ssl_info = init_str();
		add_num_to_str(&e->ssl_info, &l, SSL_get_cipher_bits(c->ssl, NULL));
		add_to_str(&e->ssl_info, &l, "-bit ");
		add_to_str(&e->ssl_info, &l, SSL_get_cipher_version(c->ssl));
		add_to_str(&e->ssl_info, &l, " ");
		add_to_str(&e->ssl_info, &l, (unsigned char *)SSL_get_cipher_name(c->ssl));
	}
#endif
	if (e->redirect) mem_free(e->redirect), e->redirect = NULL;
	if (h == 301 || h == 302 || h == 303 || h == 307) {
		if ((h == 302 || h == 303 || h == 307) && !e->expire_time) e->expire_time = 1;
		if ((d = parse_http_header(e->head, "Location", NULL))) {
			unsigned char *user, *ins;
			unsigned char *newuser, *newpassword;
			if (!parse_url(d, NULL, &user, NULL, NULL, NULL, &ins, NULL, NULL, NULL, NULL, NULL, NULL) && !user && ins && (newuser = get_user_name(host))) {
				if (*newuser) {
					int ins_off = ins - d;
					newpassword = get_pass(host);
					if (!newpassword) newpassword = stracpy("");
					add_to_strn(&newuser, ":");
					add_to_strn(&newuser, newpassword);
					add_to_strn(&newuser, "@");
					extend_str(&d, strlen(newuser));
					ins = d + ins_off;
					memmove(ins + strlen(newuser), ins, strlen(ins) + 1);
					memcpy(ins, newuser, strlen(newuser));
					mem_free(newpassword);
				}
				mem_free(newuser);
			}
			if (e->redirect) mem_free(e->redirect);
			e->redirect = d;
			e->redirect_get = h == 303;
		}
	}
	if (!e->expire_time && strchr(c->url, POST_CHAR)) e->expire_time = 1;
	info->close = 0;
	info->length = -1;
	info->version = version;
	if ((d = parse_http_header(e->head, "Connection", NULL)) || (d = parse_http_header(e->head, "Proxy-Connection", NULL))) {
		if (!strcasecmp(d, "close")) info->close = 1;
		mem_free(d);
	} else if (version < 11) info->close = 1;
	cf = c->from;
	c->from = 0;
	if ((d = parse_http_header(e->head, "Content-Range", NULL))) {
		if (strlen(d) > 6) {
			d[5] = 0;
			if (!(strcasecmp(d, "bytes")) && d[6] >= '0' && d[6] <= '9') {
#if defined(HAVE_STRTOLL)
				long long f = strtoll(d + 6, NULL, 10);
#elif defined(HAVE_STRTOQ)
				longlong f = strtoq(d + 6, NULL, 10);
#else
				long f = strtol(d + 6, NULL, 10);
				if (f == MAXLONG) f = -1;
#endif
				if (f >= 0 && (off_t)f >= 0 && (off_t)f == f) c->from = f;
			}
		}
		mem_free(d);
	} else if (h == 206) {
/* Hmm ... some servers send 206 partial but don't sent Content-Range */
		c->from = cf;
	}
	if (cf && !c->from && !c->unrestartable) c->unrestartable = 1;
	if (c->from > cf || c->from < 0) {
		setcstate(c, S_HTTP_ERROR);
		abort_connection(c);
		return;
	}
	if ((d = parse_http_header(e->head, "Content-Length", NULL))) {
		unsigned char *ep;
#if defined(HAVE_STRTOLL)
		long long l = strtoll(d, (char **)(void *)&ep, 10);
#elif defined(HAVE_STRTOQ)
		longlong l = strtoq(d, (char **)(void *)&ep, 10);
#else
		long l = strtol(d, (char **)(void *)&ep, 10);
		if (l == MAXLONG) l = -1;
#endif
		if (!*ep && l >= 0 && (off_t)l >= 0 && (off_t)l == l) {
			if (!info->close || version >= 11) info->length = l;
			if (c->from + l >= 0) c->est_length = c->from + l;
		}
		mem_free(d);
	}
	if ((d = parse_http_header(e->head, "Accept-Ranges", NULL))) {
		if (!strcasecmp(d, "none") && !c->unrestartable) c->unrestartable = 1;
		mem_free(d);
	} else {
		if (!c->unrestartable && !c->from) c->unrestartable = 1;
	}
	if (info->bl_flags & BL_NO_RANGE && !c->unrestartable) c->unrestartable = 1;
	if ((d = parse_http_header(e->head, "Transfer-Encoding", NULL))) {
		if (!strcasecmp(d, "chunked")) {
			info->length = -2;
			info->chunk_remaining = -1;
		}
		mem_free(d);
	}
	if (!info->close && info->length == -1) info->close = 1;
	if ((d = parse_http_header(e->head, "Last-Modified", NULL))) {
		if (e->last_modified && strcasecmp(e->last_modified, d)) {
			delete_entry_content(e);
			if (c->from) {
				c->from = 0;
				mem_free(d);
				setcstate(c, S_MODIFIED);
				retry_connection(c);
				return;
			}
		}
		if (!e->last_modified) e->last_modified = d;
		else mem_free(d);
	}
	if (!e->last_modified && (d = parse_http_header(e->head, "Date", NULL)))
		e->last_modified = d;
	if (info->length == -1 || (version < 11 && info->close)) rb->close = 1;
	read_http_data(c, rb);
}

static void http_get_header(struct connection *c)
{
	struct read_buffer *rb;
	set_timeout(c);
	if (!(rb = alloc_read_buffer(c))) return;
	rb->close = 1;
	read_from_socket(c, c->sock1, rb, http_got_header);
}
